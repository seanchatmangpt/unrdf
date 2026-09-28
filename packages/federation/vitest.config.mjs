import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    // node:test files run via `node --test` (see package.json test scripts)
    exclude: ['**/node_modules/**', '**/dist/**', 'test/**/*.node.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'test/**', 'dist/**'],
    },
  },
});
