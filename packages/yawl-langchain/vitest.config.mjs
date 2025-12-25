import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['test/**', 'examples/**', 'node_modules/**'],
    },
    testTimeout: 10000,
    hookTimeout: 10000,
  },
});
