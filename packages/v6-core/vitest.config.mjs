import { defineConfig } from 'vitest/config';

// Only the vitest-based suites; the rest use node:test and run via `node --test` (see package.json).
export default defineConfig({
  test: {
    environment: 'node',
    include: [
      'test/receipts/with-receipt.test.mjs',
      'test/implementations.test.mjs',
      'test/docs/latex-generator.test.mjs',
      'test/docs/thesis-builder.test.mjs',
    ],
  },
});
