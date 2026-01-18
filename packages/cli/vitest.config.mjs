/**
 * @file Vitest Configuration for CLI Package
 * @module @unrdf/cli/vitest
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    exclude: ['**/node_modules/**', '**/dist/**'],
    testTimeout: 10000,
    globals: false,
    passWithNoTests: false,
  },
});
