/**
 * @file Vitest configuration for KGC Runtime
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: ['**/node_modules/**', '**/test/**'],
    },
    // node:test suites (run via `node --test` in the package test script)
    exclude: [
      '**/node_modules/**',
      'test/bounds.test.mjs',
      'test/enhanced-bounds.test.mjs',
      'test/validators.test.mjs',
    ],
    testTimeout: 5000,
  },
});
