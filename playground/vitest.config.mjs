import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**/*.test.mjs', '**/*.spec.mjs'],
    exclude: ['node_modules/**', 'dist/**'],
    timeout: 30000, // 30 seconds for CLI operations
    testTimeout: 30000,
    hookTimeout: 30000,
    teardownTimeout: 30000,
    reporter: ['verbose', 'json', 'html'],
    outputFile: {
      json: './test-results.json',
      html: './test-results.html'
    },
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/**',
        'dist/**',
        'test/**',
        '**/*.test.mjs',
        '**/*.spec.mjs',
        '**/*.config.mjs'
      ]
    }
  }
});
