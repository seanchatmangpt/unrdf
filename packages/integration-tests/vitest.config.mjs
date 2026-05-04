/**
 * Integration Tests - Vitest Configuration
 * Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)
 *
 * @module @unrdf/integration-tests/vitest.config
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Test environment
    environment: 'node',
    globals: false,

    // Pattern matching
    include: [
      'test/**/*.test.mjs',
      'workflows/**/*.test.mjs',
      'federation/**/*.test.mjs',
      'streaming/**/*.test.mjs',
      'error-recovery/**/*.test.mjs',
      'performance/**/*.test.mjs',
    ],
    exclude: ['**/node_modules/**', '**/dist/**'],

    // Timeouts (strict SLAs)
    testTimeout: 5000,       // 5s default
    hookTimeout: 5000,       // 5s for setup/teardown

    // Parallel execution (Vitest 4 format)
    pool: 'threads',
    minWorkers: 1,
    maxWorkers: 10,          // 10 workers as per spec

    // Retry configuration for flakiness detection
    retry: 0,                // No retries - flaky tests must be fixed

    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      reportsDirectory: './coverage',
      thresholds: {
        lines: 85,           // Phase 5 target: >=85%
        branches: 80,        // Phase 5 target: >=80%
        functions: 85,       // Phase 5 target: >=85%
        statements: 85,      // Phase 5 target: >=85%
      },
      include: [
        '../**/src/**/*.mjs',
      ],
      exclude: [
        '**/node_modules/**',
        '**/dist/**',
        '**/test/**',
        '**/*.test.mjs',
      ],
    },

    // Performance budgets
    slowTestThreshold: 1000, // Warn if test takes >1s

    // Reporter configuration
    reporters: process.env.CI
      ? ['default', 'github-actions']
      : ['default', 'verbose'],

    // Bail on first failure in CI
    bail: process.env.CI ? 1 : 0,

    // Sequence - run tests in deterministic order (no flakiness)
    sequence: {
      shuffle: false,
    },
  },
});
