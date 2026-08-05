/**
 * @fileoverview Lambda Function Bundler - esbuild integration for UNRDF
 *
 * @description
 * Bundles UNRDF applications into optimized Lambda deployment packages using esbuild.
 * Handles dependency resolution, minification, and tree-shaking for minimal cold starts.
 *
 * @module serverless/deploy/lambda-bundler
 * @version [VERSION]
 * @license MIT
 */

import { build } from 'esbuild';
import { createWriteStream, promises as fs } from 'node:fs';
import { join } from 'node:path';
import { createGzip } from 'node:zlib';
import { pipeline } from 'node:stream/promises';
import { z } from 'zod';

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

export class LambdaBundler {
  #config;

  constructor(config) {
    this.#config = BundlerConfigSchema.parse(config);
  }

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
