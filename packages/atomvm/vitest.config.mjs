/**
 * @file Vitest Configuration for AtomVM Browser Package
 * @description
 * Browser testing configuration using jsdom for unit tests.
 * Service worker and COI features require real browser environment.
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    name: 'atomvm',
    environment: 'jsdom',
    globals: true,
    include: ['test/**/*.test.mjs'],
    exclude: ['test/playwright/**', 'node_modules/**', 'dist/**'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['test/**', 'dist/**', 'public/**'],
      thresholds: {
        lines: 28,
        functions: 30,
        branches: 24,
        statements: 28,
      },
    },
    testTimeout: 10000,
    hookTimeout: 5000,
  },
});

