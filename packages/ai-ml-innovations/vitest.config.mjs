/**
 * @fileoverview Vitest configuration for ai-ml-innovations package
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Test files
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],

    // Timeout - federated learning can take longer
    testTimeout: 30000,

    // Environment
    environment: 'node',

    // Reporter
    reporter: ['verbose'],

    // Globals
    globals: false,
    isolate: true,
  },
});
