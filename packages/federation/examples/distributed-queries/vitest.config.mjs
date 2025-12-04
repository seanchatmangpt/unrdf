/**
 * @file Vitest configuration for distributed-queries example
 */

import { defineConfig } from 'vitest/config';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
  },
  resolve: {
    alias: {
      '@unrdf/federation': resolve(__dirname, '../../src/index.mjs'),
      '@unrdf/core': resolve(__dirname, '../../../core/src/index.mjs'),
    },
  },
});
