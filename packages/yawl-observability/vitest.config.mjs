import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules', 'dist'],
    globals: true,
    environment: 'node',
  },
});
