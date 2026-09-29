/**
 * @fileoverview Lambda Function Bundler - esbuild integration for UNRDF
 *
 * @description
 * Bundles UNRDF applications into optimized Lambda deployment packages using esbuild.
 * Handles dependency resolution, minification, and tree-shaking for minimal cold starts.
 *
 * @module serverless/deploy/lambda-bundler
 * @version 1.0.0
 * @license MIT
 */

import { build } from 'esbuild';
import { createWriteStream, promises as fs } from 'node:fs';
import { join } from 'node:path';
import { createGzip } from 'node:zlib';
import { pipeline } from 'node:stream/promises';
import { z } from 'zod';

const BundlerConfigSchema = z.object({
  entryPoint: z.string().min(1),
  outDir: z.string().min(1),
  minify: z.boolean().default(true),
  sourcemap: z.boolean().default(false),
  external: z.array(z.string()).default(['@aws-sdk/*']),
  define: z.record(z.string()).default({}),
  platform: z.enum(['node', 'browser']).default('node'),
  target: z.string().default('node20'),
});

/**
 * Bundles a single UNRDF entry point into a Lambda deployment package with esbuild.
 */
export class LambdaBundler {
  #config;

  /**
   * Creates a bundler after validating and defaulting the configuration.
   *
   * @param {Object} config - Bundler configuration.
   * @param {string} config.entryPoint - Path of the source entry file.
   * @param {string} config.outDir - Directory to write the bundle into.
   * @param {boolean} [config.minify=true] - Whether to minify output.
   * @param {boolean} [config.sourcemap=false] - Whether to emit a sourcemap.
   * @param {string[]} [config.external] - Module patterns left unbundled (default `@aws-sdk/*`).
   * @param {Record<string,string>} [config.define] - Compile-time constant replacements.
   * @param {'node'|'browser'} [config.platform='node'] - Target platform.
   * @param {string} [config.target='node20'] - esbuild target.
   * @throws {import('zod').ZodError} If the configuration is invalid.
   */
  constructor(config) {
    this.#config = BundlerConfigSchema.parse(config);
  }

  /**
   * Runs esbuild, writes `index.js` plus a gzip copy to the output directory, and reports sizes.
   *
   * @returns {Promise<{outputPath: string, sizeBytes: number, gzipSizeBytes: number, dependencies: string[], buildTimeMs: number}>} Bundle path, raw and gzipped sizes, sorted node_modules dependency names, and build duration.
   * @throws {Error} `Bundle failed: ...` wrapping any underlying error.
   */
  async bundle() {
    const startTime = Date.now();

    try {
      await fs.mkdir(this.#config.outDir, { recursive: true });
      const outputPath = join(this.#config.outDir, 'index.js');

      const result = await build({
        entryPoints: [this.#config.entryPoint],
        bundle: true,
        platform: this.#config.platform,
        target: this.#config.target,
        format: 'esm',
        outfile: outputPath,
        minify: this.#config.minify,
        sourcemap: this.#config.sourcemap,
        external: this.#config.external,
        define: this.#config.define,
        treeShaking: true,
        metafile: true,
        logLevel: 'info',
      });

      const stats = await fs.stat(outputPath);
      const gzipPath = `${outputPath}.gz`;
      await this.#gzipFile(outputPath, gzipPath);
      const gzipStats = await fs.stat(gzipPath);
      const dependencies = this.#extractDependencies(result.metafile);

      return {
        outputPath,
        sizeBytes: stats.size,
        gzipSizeBytes: gzipStats.size,
        dependencies,
        buildTimeMs: Date.now() - startTime,
      };
    } catch (error) {
      throw new Error(`Bundle failed: ${error.message}`, { cause: error });
    }
  }

  /**
   * Builds several bundles concurrently.
   *
   * @param {Object[]} configs - Bundler configurations, one per bundle.
   * @returns {Promise<Object[]>} Bundle results in the same order as `configs`.
   */
  static async bundleAll(configs) {
    const bundlers = configs.map(config => new LambdaBundler(config));
    return Promise.all(bundlers.map(bundler => bundler.bundle()));
  }

  async #gzipFile(inputPath, outputPath) {
    const input = (await import('node:fs')).createReadStream(inputPath);
    const output = createWriteStream(outputPath);
    const gzip = createGzip({ level: 9 });
    await pipeline(input, gzip, output);
  }

  #extractDependencies(metafile) {
    const deps = new Set();
    for (const input of Object.keys(metafile.inputs || {})) {
      if (input.includes('node_modules')) {
        const match = input.match(/node_modules\/(@[^/]+\/[^/]+|[^/]+)/);
        if (match) deps.add(match[1]);
      }
    }
    return Array.from(deps).sort();
  }

  /**
   * Summarizes bundle size per module from an esbuild metafile JSON file.
   *
   * @param {string} metafilePath - Path to an esbuild metafile JSON file.
   * @returns {Promise<{totalSizeBytes: number, largestDeps: Array<{name: string, bytes: number, percentage: string}>, moduleCount: number}>} Total input bytes, the ten largest modules with percentage share, and the number of distinct modules.
   */
  static async analyzeBundleSize(metafilePath) {
    const content = await fs.readFile(metafilePath, 'utf-8');
    const metafile = JSON.parse(content);
    const sizeByModule = {};

    for (const [path, data] of Object.entries(metafile.inputs || {})) {
      const bytes = data.bytes || 0;
      const moduleName = path.includes('node_modules')
        ? path.match(/node_modules\/(@[^/]+\/[^/]+|[^/]+)/)?.[1] || 'unknown'
        : 'application';
      sizeByModule[moduleName] = (sizeByModule[moduleName] || 0) + bytes;
    }

    const totalSize = Object.values(sizeByModule).reduce((sum, size) => sum + size, 0);
    const sorted = Object.entries(sizeByModule)
      .sort(([, a], [, b]) => b - a)
      .slice(0, 10);

    return {
      totalSizeBytes: totalSize,
      largestDeps: sorted.map(([name, bytes]) => ({
        name,
        bytes,
        percentage: totalSize === 0 ? '0.00' : ((bytes / totalSize) * 100).toFixed(2),
      })),
      moduleCount: Object.keys(sizeByModule).length,
    };
  }
}

/**
 * Builds the default bundler configuration for a named Lambda function.
 *
 * @param {string} functionName - Function directory name under `src/lambda/`.
 * @param {Object} [options={}] - Configuration overrides merged over the defaults.
 * @returns {Object} Bundler configuration for `LambdaBundler`.
 */
export function createDefaultBundlerConfig(functionName, options = {}) {
  return {
    entryPoint: `./src/lambda/${functionName}/index.mjs`,
    outDir: `./dist/lambda/${functionName}`,
    minify: true,
    sourcemap: false,
    external: ['@aws-sdk/*'],
    define: {
      'process.env.NODE_ENV': '"production"',
      'process.env.FUNCTION_NAME': `"${functionName}"`,
    },
    ...options,
  };
}

/**
 * Bundles the built-in `query` and `ingest` Lambda functions sequentially.
 *
 * @param {Object} [options={}] - Configuration overrides applied to every function.
 * @returns {Promise<Map<string, Object>>} Bundle results keyed by function name.
 */
export async function bundleUNRDFFunctions(options = {}) {
  const functions = ['query', 'ingest'];
  const results = new Map();

  for (const fn of functions) {
    const config = createDefaultBundlerConfig(fn, options);
    const bundler = new LambdaBundler(config);
    results.set(fn, await bundler.bundle());
  }

  return results;
}
