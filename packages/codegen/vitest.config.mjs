/**
 * @fileoverview Vitest configuration for @unrdf/codegen
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Enable globals
    globals: false,

    // Environment
    environment: 'node',

    // Test file patterns - look in test/ directory
    include: ['test/**/*.test.mjs'],

    // Exclude patterns
    exclude: [
      'node_modules/**',
      'dist/**',
      '**/fixtures/**',
    ],

    // Timeout - 5s SLA (Andon Principle)
    testTimeout: 5000,

    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: [
        '**/*.test.mjs',
        '**/*.spec.mjs',
        '**/index.mjs', // Re-export files
      ],
      // 80% coverage requirement
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 80,
        statements: 80,
      },
    },

    // Reporter
    reporter: ['verbose'],

    // Parallel execution
    pool: 'forks',
    poolOptions: {
      forks: {
        maxForks: 4,
      },
    },
  },
});
