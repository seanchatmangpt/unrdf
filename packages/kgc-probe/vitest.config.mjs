/**
 * @file Vitest Configuration for KGC Probe
 * @description Test configuration with coverage thresholds
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    include: [
      'test/**/*.test.mjs',
      'test/**/*.test.js'
    ],
    exclude: [
      '**/node_modules/**',
      '**/dist/**',
      '**/build/**'
    ],
    globals: true,
    testTimeout: 30000,  // 30s for E2E tests with coverage
    hookTimeout: 10000,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      reportsDirectory: './coverage',
      lines: 85,
      functions: 85,
      branches: 75,
      statements: 85,
      include: ['src/**/*.mjs'],
      exclude: [
        'test/**',
        '**/*.test.mjs',
        '**/*.spec.mjs',
        '**/node_modules/**',
        '**/dist/**',
        '**/coverage/**'
      ]
    },
    reporters: ['default'],
    isolate: true,
    pool: 'forks',
    maxForks: 4,
    minForks: 1,
    bail: 0,
    silent: false
  }
});
