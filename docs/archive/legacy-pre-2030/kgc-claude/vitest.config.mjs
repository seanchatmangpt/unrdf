import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['test/**/*.test.mjs'],
    globals: false,
    environment: 'node',
    testTimeout: 10000,
  },
});
