/**
 * Vitest configuration for v6-compat package
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    pool: 'threads',
    poolOptions: {
      threads: {
        singleThread: false,
        minThreads: 1,
        maxThreads: 4,
      },
    },
    testTimeout: 10000,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],
    reporter: ['verbose'],
    globals: false,
    isolate: true,
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
