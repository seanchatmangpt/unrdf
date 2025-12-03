/**
 * Vitest Configuration for @unrdf/composables
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom',
    include: ['test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['src/**/*.d.ts', 'examples/**'],
      lines: 80,
      functions: 80,
      branches: 80,
      statements: 80,
    },
  },
});
