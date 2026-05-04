import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        'examples/',
        '**/*.test.mjs'
      ]
    },
    testTimeout: 5000,
    hookTimeout: 5000
  }
});
