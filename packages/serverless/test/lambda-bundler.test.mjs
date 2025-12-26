/**
 * @fileoverview Tests for Lambda Bundler
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { LambdaBundler, createDefaultBundlerConfig } from '../src/deploy/lambda-bundler.mjs';
import { writeFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';

describe('LambdaBundler', () => {
  const testDir = join(process.cwd(), 'test-output');

  beforeEach(async () => {
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('creates bundler with valid configuration', () => {
    const bundler = new LambdaBundler({
      entryPoint: './src/index.mjs',
      outDir: './dist',
    });

    expect(bundler).toBeDefined();
  });

  it('validates configuration schema', () => {
    expect(() => {
      new LambdaBundler({
        entryPoint: '', // Invalid empty string
        outDir: './dist',
      });
    }).toThrow();
  });

  it('bundles simple module', async () => {
    const entryPoint = join(testDir, 'entry.mjs');
    const outDir = join(testDir, 'dist');

    await writeFile(
      entryPoint,
      `
      export function handler() {
        return { statusCode: 200, body: 'OK' };
      }
    `
    );

    const bundler = new LambdaBundler({
      entryPoint,
      outDir,
      minify: false,
    });

    const metadata = await bundler.bundle();

    expect(metadata.outputPath).toContain('index.js');
    expect(metadata.sizeBytes).toBeGreaterThan(0);
    expect(metadata.gzipSizeBytes).toBeLessThan(metadata.sizeBytes);
    expect(metadata.buildTimeMs).toBeGreaterThan(0);
  });

  it('creates default bundler config', () => {
    const config = createDefaultBundlerConfig('query', { minify: true });

    expect(config.entryPoint).toContain('query');
    expect(config.outDir).toContain('query');
    expect(config.minify).toBe(true);
    expect(config.external).toContain('@aws-sdk/*');
  });

  it('excludes AWS SDK from bundle', async () => {
    const entryPoint = join(testDir, 'entry.mjs');
    const outDir = join(testDir, 'dist');

    await writeFile(
      entryPoint,
      `
      export function handler() {
        return { statusCode: 200 };
      }
    `
    );

    const bundler = new LambdaBundler({
      entryPoint,
      outDir,
      external: ['@aws-sdk/*'],
    });

    const metadata = await bundler.bundle();
    expect(metadata.dependencies).not.toContain('@aws-sdk/client-dynamodb');
  });
});

describe('createDefaultBundlerConfig', () => {
  it('creates query function config', () => {
    const config = createDefaultBundlerConfig('query');

    expect(config.entryPoint).toBe('./src/lambda/query/index.mjs');
    expect(config.outDir).toBe('./dist/lambda/query');
  });

  it('creates ingest function config', () => {
    const config = createDefaultBundlerConfig('ingest');

    expect(config.entryPoint).toBe('./src/lambda/ingest/index.mjs');
    expect(config.outDir).toBe('./dist/lambda/ingest');
  });

  it('applies custom options', () => {
    const config = createDefaultBundlerConfig('query', {
      minify: false,
      sourcemap: true,
    });

    expect(config.minify).toBe(false);
    expect(config.sourcemap).toBe(true);
  });
});
