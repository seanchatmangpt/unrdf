import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.mjs'],
    testTimeout: 60000,
    exclude: ['node_modules/**', 'dist/**'],
  },
});
