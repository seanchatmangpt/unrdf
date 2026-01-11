/**
 * @file Vitest configuration for GeoSPARQL package
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],
    globals: false,
    environment: 'node',
    testTimeout: 5000,
    hookTimeout: 5000,
    teardownTimeout: 5000,
    isolate: true,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['src/index.mjs', 'src/**/*.schema.mjs'],
      lines: 80,
      functions: 80,
      branches: 80,
      statements: 80
    }
  }
});
