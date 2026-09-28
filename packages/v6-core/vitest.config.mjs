import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    // Only suites written against vitest; the rest use node:test and run via `pnpm test:node`.
    include: [
      'test/docs/*.test.mjs',
      'test/implementations.test.mjs',
      'test/oxigraph-determinism.test.mjs',
      'test/receipts/with-receipt.test.mjs',
    ],
  },
});
