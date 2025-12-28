/**
 * @file Vitest Configuration for kgc-cli
 * @description Test configuration for LaTeX pipeline and CLI tests
 */

import { defineConfig } from 'vitest/config';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export default defineConfig({
  test: {
    // Test environment
    environment: 'node',

    // Test file patterns
    include: ['test/**/*.test.mjs'],
    exclude: [
      '**/node_modules/**',
      '**/dist/**',
      '**/build/**',
      '**/vendor/**',
      '**/*.skip.test.mjs'
    ],

    // Global test APIs
    globals: true,

    // Test timeout (per CLAUDE.md: default 5s, extended for integration)
    testTimeout: 10000, // 10s for LaTeX compilation tests

    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      reportsDirectory: './coverage',

      // Coverage thresholds (80% per CLAUDE.md)
      lines: 80,
      functions: 80,
      branches: 80,
      statements: 80,

      // Files to include
      include: ['src/**/*.mjs'],

      // Files to exclude
      exclude: [
        'test/**',
        '**/*.test.mjs',
        '**/*.spec.mjs',
        '**/node_modules/**',
        '**/dist/**',
        '**/coverage/**',
        '**/vendor/**',
        'src/cli.mjs', // CLI entry point (tested via integration)
        '**/__tests__/**'
      ]
    },

    // Reporter configuration
    reporters: ['default'],

    // Retry failed tests once
    retry: 0,

    // Run tests in parallel
    pool: 'threads',
    poolOptions: {
      threads: {
        singleThread: false
      }
    }
  },

  resolve: {
    alias: {
      '@': join(__dirname, 'src'),
      '@test': join(__dirname, 'test')
    }
  }
});
