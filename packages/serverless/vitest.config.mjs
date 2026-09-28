import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // *.node.test.mjs use node:test and run via node --test
    exclude: ['node_modules/**', 'test/**/*.node.test.mjs'],
  },
});
