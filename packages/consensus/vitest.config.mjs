import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['test/**/*.test.mjs'],
    testTimeout: 10000, // 10 seconds per test
    hookTimeout: 10000, // 10 seconds for hooks
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
    },
  },
});
