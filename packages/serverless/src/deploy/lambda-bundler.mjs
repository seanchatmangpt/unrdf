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
import { join, dirname } from 'node:path';
import { createGzip } from 'node:zlib';
import { pipeline } from 'node:stream/promises';
import { z } from 'zod';

/**
 * Bundler configuration schema
 * @typedef {Object} BundlerConfig
 * @property {string} entryPoint - Entry point file path
 * @property {string} outDir - Output directory
 * @property {boolean} minify - Enable minification
 * @property {boolean} sourcemap - Generate source maps
 * @property {string[]} external - External dependencies
 * @property {Object} define - Environment variable definitions
 */
const BundlerConfigSchema = z.object({
  entryPoint: z.string(),
  outDir: z.string(),
  minify: z.boolean().default(true),
  sourcemap: z.boolean().default(false),
  external: z.array(z.string()).default(['@aws-sdk/*']),
  define: z.record(z.string()).default({}),
  platform: z.enum(['node', 'browser']).default('node'),
  target: z.string().default('node20'),
});

/**
 * Bundle metadata
 * @typedef {Object} BundleMetadata
 * @property {string} outputPath - Bundled file path
 * @property {number} sizeBytes - Bundle size in bytes
 * @property {number} gzipSizeBytes - Gzipped size in bytes
 * @property {string[]} dependencies - Included dependencies
 * @property {number} buildTimeMs - Build duration in milliseconds
 */

/**
 * Lambda Function Bundler
 *
 * @class LambdaBundler
 *
 * @description
 * High-performance bundler for Lambda functions with:
 * - Tree-shaking for minimal bundle size
 * - Automatic external dependency detection
 * - Source map generation for debugging
 * - Gzip compression for deployment
 *
 * @example
 * ```javascript
 * const bundler = new LambdaBundler({
 *   entryPoint: './src/handler.mjs',
 *   outDir: './dist/lambda',
 *   minify: true
 * });
 *
 * const metadata = await bundler.bundle();
 * console.log(`Bundle size: ${metadata.sizeBytes} bytes`);
 * ```
 */
export class LambdaBundler {
  /**
   * Bundler configuration
   * @type {Object}
   * @private
   */
  #config;

  /**
   * Create Lambda bundler
   *
   * @param {Object} config - Bundler configuration
   */
  constructor(config) {
    this.#config = BundlerConfigSchema.parse(config);
  }

  /**
   * Bundle Lambda function
   *
   * @returns {Promise<BundleMetadata>} Bundle metadata
   *
   * @throws {Error} If bundling fails
   *
   * @example
   * ```javascript
   * const metadata = await bundler.bundle();
   * ```
   */
  async bundle() {
    const startTime = Date.now();

    try {
      // Ensure output directory exists
      await fs.mkdir(this.#config.outDir, { recursive: true });

      // Build with esbuild
      const result = await build({
        entryPoints: [this.#config.entryPoint],
        bundle: true,
        platform: this.#config.platform,
        target: this.#config.target,
        format: 'esm',
        outdir: this.#config.outDir,
        minify: this.#config.minify,
        sourcemap: this.#config.sourcemap,
        external: this.#config.external,
        define: this.#config.define,
        treeShaking: true,
        metafile: true,
        logLevel: 'info',
      });

      const outputPath = join(this.#config.outDir, 'index.js');
      const stats = await fs.stat(outputPath);

      // Create gzipped version
      const gzipPath = `${outputPath}.gz`;
      await this.#gzipFile(outputPath, gzipPath);
      const gzipStats = await fs.stat(gzipPath);

      // Extract dependencies from metafile
      const dependencies = this.#extractDependencies(result.metafile);

      const buildTimeMs = Date.now() - startTime;

      return {
        outputPath,
        sizeBytes: stats.size,
        gzipSizeBytes: gzipStats.size,
        dependencies,
        buildTimeMs,
      };
    } catch (error) {
      throw new Error(`Bundle failed: ${error.message}`, { cause: error });
    }
  }

  /**
   * Bundle multiple Lambda functions
   *
   * @param {Object[]} configs - Array of bundler configurations
   * @returns {Promise<BundleMetadata[]>} Array of bundle metadata
   *
   * @example
   * ```javascript
   * const results = await LambdaBundler.bundleAll([
   *   { entryPoint: './src/query.mjs', outDir: './dist/query' },
   *   { entryPoint: './src/ingest.mjs', outDir: './dist/ingest' }
   * ]);
   * ```
   */
  static async bundleAll(configs) {
    const bundlers = configs.map((config) => new LambdaBundler(config));
    return Promise.all(bundlers.map((bundler) => bundler.bundle()));
  }

  /**
   * Gzip a file
   *
   * @private
   * @param {string} inputPath - Input file path
   * @param {string} outputPath - Output gzip path
   * @returns {Promise<void>}
   */
  async #gzipFile(inputPath, outputPath) {
    const input = (await import('node:fs')).createReadStream(inputPath);
    const output = createWriteStream(outputPath);
    const gzip = createGzip({ level: 9 });

    await pipeline(input, gzip, output);
  }

  /**
   * Extract dependencies from esbuild metafile
   *
   * @private
   * @param {Object} metafile - esbuild metafile
   * @returns {string[]} List of dependencies
   */
  #extractDependencies(metafile) {
    const deps = new Set();

    for (const input of Object.keys(metafile.inputs || {})) {
      if (input.includes('node_modules')) {
        const match = input.match(/node_modules\/(@[^/]+\/[^/]+|[^/]+)/);
        if (match) {
          deps.add(match[1]);
        }
      }
    }

    return Array.from(deps).sort();
  }

  /**
   * Analyze bundle size and composition
   *
   * @param {string} metafilePath - Path to esbuild metafile
   * @returns {Promise<Object>} Bundle analysis
   *
   * @example
   * ```javascript
   * const analysis = await LambdaBundler.analyzeBundleSize('./dist/metafile.json');
   * console.log('Largest dependencies:', analysis.largestDeps);
   * ```
   */
  static async analyzeBundleSize(metafilePath) {
    const content = await fs.readFile(metafilePath, 'utf-8');
    const metafile = JSON.parse(content);

    const inputs = metafile.inputs || {};
    const sizeByModule = {};

    for (const [path, data] of Object.entries(inputs)) {
      const bytes = data.bytes || 0;
      const moduleName = path.includes('node_modules')
        ? path.match(/node_modules\/(@[^/]+\/[^/]+|[^/]+)/)?.[1] || 'unknown'
        : 'application';

      sizeByModule[moduleName] = (sizeByModule[moduleName] || 0) + bytes;
    }

    const sorted = Object.entries(sizeByModule)
      .sort(([, a], [, b]) => b - a)
      .slice(0, 10);

    const totalSize = Object.values(sizeByModule).reduce((sum, size) => sum + size, 0);

    return {
      totalSizeBytes: totalSize,
      largestDeps: sorted.map(([name, bytes]) => ({
        name,
        bytes,
        percentage: ((bytes / totalSize) * 100).toFixed(2),
      })),
      moduleCount: Object.keys(sizeByModule).length,
    };
  }
}

/**
 * Create default UNRDF Lambda bundler configuration
 *
 * @param {string} functionName - Lambda function name (query, ingest, etc.)
 * @param {Object} options - Additional options
 * @returns {Object} Bundler configuration
 *
 * @example
 * ```javascript
 * const config = createDefaultBundlerConfig('query', { minify: true });
 * const bundler = new LambdaBundler(config);
 * ```
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
 * Bundle all UNRDF Lambda functions
 *
 * @param {Object} options - Bundle options
 * @returns {Promise<Map<string, BundleMetadata>>} Map of function name to metadata
 *
 * @example
 * ```javascript
 * const results = await bundleUNRDFFunctions({ minify: true });
 * for (const [name, metadata] of results) {
 *   console.log(`${name}: ${metadata.sizeBytes} bytes`);
 * }
 * ```
 */
export async function bundleUNRDFFunctions(options = {}) {
  const functions = ['query', 'ingest'];
  const results = new Map();

  for (const fn of functions) {
    const config = createDefaultBundlerConfig(fn, options);
    const bundler = new LambdaBundler(config);
    const metadata = await bundler.bundle();
    results.set(fn, metadata);
  }

  return results;
}
