import { configDefaults, defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['test/**/*.test.mjs'],
    // node:test suites run by scripts/test-interchangeable-parts.mjs (vitest cannot collect them)
    exclude: [...configDefaults.exclude, 'test/artifact/interchangeable-part.test.mjs'],
    environment: 'node',
    testTimeout: 10_000,
  },
});
