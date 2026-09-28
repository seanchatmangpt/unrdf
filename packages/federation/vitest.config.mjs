import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    exclude: ['node_modules/**', 'test/**/*.node.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'test/**', 'dist/**'],
    },
  },
});
