import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    testTimeout: 5000, // 5 second timeout per Andon principle
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],
    globals: false,
    isolate: true,
  },
});
