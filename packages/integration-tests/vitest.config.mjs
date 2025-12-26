import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: false,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      include: [
        'workflows/**/*.test.mjs',
        'federation/**/*.test.mjs',
        'streaming/**/*.test.mjs',
        'error-recovery/**/*.test.mjs',
        'performance/**/*.test.mjs',
      ],
      exclude: ['**/node_modules/**', '**/dist/**'],
      thresholds: {
        lines: 80, // Increased from 70% to 80%
        functions: 80,
        branches: 80,
        statements: 80,
      },
    },
    testTimeout: 30000, // 30s timeout for integration tests
    hookTimeout: 10000,
    // Performance budgets
    slowTestThreshold: 1000, // Warn if integration test takes >1s
    // Custom reporters for CI
    reporters: process.env.CI
      ? ['default', 'github-actions']
      : ['default', 'verbose'],
  },
});
