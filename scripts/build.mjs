#!/usr/bin/env node

/**
 * Build script for UNRDF monorepo
 * Orchestrates esbuild for all 21 packages with ESM + CJS dual format output
 *
 * Handles:
 * - Loading esbuild config with multiple build passes
 * - Generating TypeScript definitions
 * - Creating proper outdir structure for dual format
 * - Error handling and logging
 */

import * as esbuild from 'esbuild';
import config from '../esbuild.config.mjs';

const isDev = process.env.NODE_ENV !== 'production';

async function build() {
  const startTime = process.hrtime.bigint();

  try {
    // Handle both single config and array of configs
    const configs = Array.isArray(config) ? config : [config];

    console.log(`\n🔨 Building ${configs.length} format(s) for UNRDF monorepo...\n`);

    // Build each config sequentially (ESM first, then CJS)
    for (const cfg of configs) {
      const formatName = cfg.format === 'esm' ? 'ESM' : 'CJS';
      console.log(`📦 Building ${formatName} format...`);

      try {
        const result = await esbuild.build(cfg);

        // Log results
        if (result.warnings && result.warnings.length > 0) {
          console.warn(`⚠️  ${formatName} warnings:`, result.warnings);
        }
      } catch (error) {
        console.error(`❌ ${formatName} build failed:`, error.message);
        throw error;
      }
    }

    const endTime = process.hrtime.bigint();
    const duration = Number(endTime - startTime) / 1_000_000;

    console.log(
      `\n✅ Build complete in ${duration.toFixed(2)}ms (${(duration / 1000).toFixed(1)}s)`
    );

    if (duration > 30_000) {
      console.warn(`⚠️  Build took ${(duration / 1000).toFixed(1)}s (target: <30s)`);
    }

    process.exit(0);
  } catch (error) {
    console.error(`\n❌ Build failed: ${error.message}`);
    if (isDev) {
      console.error('Stack:', error.stack);
    }
    process.exit(1);
  }
}

build();
