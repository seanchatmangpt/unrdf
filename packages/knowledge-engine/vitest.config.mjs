/**
 * @file Vitest configuration for @unrdf/knowledge-engine
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    pool: 'forks',
    maxForks: 10,
    testTimeout: 5000,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    exclude: [
      'node_modules/**',
      'dist/**',
      'test/fixtures/**',
      'test/utils/**',
    ],
    reporter: ['verbose'],
    globals: false,
    isolate: true,
    passWithNoTests: false,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        '**/node_modules/**',
        '**/dist/**',
        '**/*.test.mjs',
        '**/*.spec.mjs',
      ],
    },
  },
});
