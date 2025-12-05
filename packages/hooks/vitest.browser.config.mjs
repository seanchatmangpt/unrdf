/**
 * @file Vitest Browser Configuration for UNRDF Hooks
 * @description
 * Enables real browser testing via Playwright for performance benchmarks
 * across Chromium, Firefox, and WebKit JS engines.
 */

import { defineConfig } from 'vitest/config';
import { resolve } from 'path';

export default defineConfig({
  test: {
    name: 'browser',
    include: ['test/benchmarks/browser/**/*.test.mjs'],
    browser: {
      enabled: true,
      provider: 'playwright',
      name: 'chromium',
      headless: true,
      screenshotFailures: false,
    },
    // Performance-focused settings
    testTimeout: 30000,
    hookTimeout: 10000,
    // Isolate benchmarks for accurate timing
    pool: 'forks',
    isolate: true,
    // Reporter for CI
    reporters: ['default', 'json'],
    outputFile: {
      json: 'reports/browser-benchmark-results.json',
    },
  },
  resolve: {
    alias: {
      '@unrdf/hooks': resolve(__dirname, 'src/index.mjs'),
    },
  },
});
