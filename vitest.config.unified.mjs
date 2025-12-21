import { defineConfig } from 'vitest/config';

/**
 * Unified Vitest configuration for all UNRDF packages.
 * Single test runner serving 21 packages with coverage reporting and 80% thresholds.
 */

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',

    // Test file patterns across all packages
    include: [
      'packages/*/test/**/*.test.mjs',
      'packages/*/src/**/*.test.mjs',
    ],

    exclude: [
      'node_modules/**',
      'dist/**',
      'build/**',
      '**/node_modules/**',
    ],

    // Coverage configuration
    coverage: {
      provider: 'v8',

      reporter: [
        'text',
        'text-summary',
        'json',
        'html',
        'lcov',
      ],

      reportsDirectory: './coverage',

      // 80% threshold for all metrics
      thresholds: {
        lines: 80,
        branches: 80,
        functions: 80,
        statements: 80,
      },

      // Include all source files
      all: true,

      include: [
        'packages/*/src/**/*.mjs',
      ],

      exclude: [
        'packages/*/test/**',
        'packages/*/src/**/*.test.mjs',
        'packages/*/node_modules/**',
        'packages/*/dist/**',
        'packages/*/build/**',
        '**/*.config.mjs',
        '**/examples/**',
      ],
    },

    // Execution settings
    threads: true,
    isolate: true,
    testTimeout: 60000,
    hookTimeout: 10000,

    // Reporters
    reporters: ['default', 'json'],
    outputFile: {
      json: './coverage/test-results.json',
    },
  },
});
