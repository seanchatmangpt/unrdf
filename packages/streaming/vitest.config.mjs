import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    testTimeout: 10000,
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],
    globals: false,
    isolate: true,
  },
});
