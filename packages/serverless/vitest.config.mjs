import { defineConfig, configDefaults } from 'vitest/config';

export default defineConfig({
  test: {
    // *.node.test.mjs files use node:test and run via `node --test` (see package.json "test")
    exclude: [...configDefaults.exclude, '**/*.node.test.mjs'],
  },
});
