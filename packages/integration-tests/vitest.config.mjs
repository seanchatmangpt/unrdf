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
        lines: 70,
        functions: 70,
        branches: 70,
        statements: 70,
      },
    },
    testTimeout: 30000, // 30s timeout for integration tests
    hookTimeout: 10000,
  },
});
