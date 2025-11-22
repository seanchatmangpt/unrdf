import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs', 'test/**/*.spec.mjs'],
    exclude: ['node_modules', 'dist'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'lcov', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['src/**/*.test.mjs', 'src/**/*.spec.mjs']
    },
    testTimeout: 10000,
    hookTimeout: 10000
  }
});
