import { defineConfig } from 'vitest/config';

export default defineConfig({
  globals: true,
  environment: 'node',
  include: ['**/*.{test,spec}.mjs'],
  testTimeout: 60000,
  exclude: [
    'node_modules/**',
    'dist/**',
  ],
});
