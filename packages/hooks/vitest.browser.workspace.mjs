/**
 * @file Multi-Browser Vitest Workspace
 * @description
 * Workspace configuration for running benchmarks across multiple browsers:
 * - Chromium (V8 engine)
 * - Firefox (SpiderMonkey engine)
 * - WebKit (JavaScriptCore engine)
 * - Node.js (V8 baseline)
 */

import { defineWorkspace } from 'vitest/config';

export default defineWorkspace([
  // Node.js baseline
  {
    test: {
      name: 'node',
      include: ['test/benchmarks/**/*.test.mjs'],
      exclude: ['test/benchmarks/browser/**/*.test.mjs'],
      environment: 'node',
    },
  },
  // Chromium (V8)
  {
    test: {
      name: 'chromium',
      include: ['test/benchmarks/browser/**/*.test.mjs'],
      browser: {
        enabled: true,
        provider: 'playwright',
        name: 'chromium',
        headless: true,
      },
    },
  },
  // Firefox (SpiderMonkey)
  {
    test: {
      name: 'firefox',
      include: ['test/benchmarks/browser/**/*.test.mjs'],
      browser: {
        enabled: true,
        provider: 'playwright',
        name: 'firefox',
        headless: true,
      },
    },
  },
  // WebKit (JavaScriptCore)
  {
    test: {
      name: 'webkit',
      include: ['test/benchmarks/browser/**/*.test.mjs'],
      browser: {
        enabled: true,
        provider: 'playwright',
        name: 'webkit',
        headless: true,
      },
    },
  },
]);
