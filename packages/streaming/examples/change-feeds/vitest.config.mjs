import { defineConfig } from 'vitest/config';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  resolve: {
    alias: {
      '@unrdf/streaming': resolve(__dirname, '../../src/index.mjs'),
      '@unrdf/core': resolve(__dirname, '../../../core/src/index.mjs'),
      '@unrdf/hooks': resolve(__dirname, '../../../hooks/src/index.mjs'),
    },
  },
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      include: ['src/**/*.mjs'],
      lines: 80,
    },
    testTimeout: 10000,
  },
});
