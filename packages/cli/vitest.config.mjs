import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    environment: 'node',
    globals: true,
    // Single-threaded for consistency
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },
    concurrent: false,
    maxConcurrency: 1,
    testTimeout: 30_000,
    hookTimeout: 30_000,
    // Include only CLI package tests
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],
    // Pass with no tests
    passWithNoTests: true,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: ['node_modules/', 'dist/'],
    },
  },
})
