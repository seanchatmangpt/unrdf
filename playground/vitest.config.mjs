import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**/*.test.mjs', '**/*.spec.mjs'],
    exclude: ['**/node_modules/**', 'dist/**', '.nitro/**'],
    // API tests need the Nitro server; globalSetup starts (and stops) it
    globalSetup: './test/setup.mjs',
    testTimeout: 30_000, // 30 seconds for CLI operations
    hookTimeout: 30_000,
    teardownTimeout: 30_000,
    reporters: ['verbose'],
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
