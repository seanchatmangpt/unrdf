/**
 * @file Vitest Configuration
 * @description Test configuration for @unrdf/tera-engine
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    exclude: [
      'node_modules/**',
      'dist/**',
      'test/fixtures/**',
      'test/utils/**',
    ],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: [
        'src/**/*.test.mjs',
        'src/**/*.spec.mjs',
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 80,
        statements: 80,
      },
    },
    testTimeout: 5000,
    hookTimeout: 5000,
    teardownTimeout: 5000,
  },
});
