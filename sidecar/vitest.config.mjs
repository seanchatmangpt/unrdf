import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    testTimeout: 60000,
    hookTimeout: 10000,
    include: [
      'test/**/*.test.mjs',
      'test/performance/**/*.test.mjs',
      'test/security/**/*.test.mjs',
      'test/chaos/**/*.test.mjs'
    ],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        '**/*.test.mjs'
      ]
    }
  }
});
