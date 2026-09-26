import { configDefaults, defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    // node:test suites run by scripts/test-interchangeable-parts.mjs (vitest cannot collect them)
    exclude: [...configDefaults.exclude, 'test/substitution-receipt.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        '**/*.test.mjs',
      ],
    },
  },
});
