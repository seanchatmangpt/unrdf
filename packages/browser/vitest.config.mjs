import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'jsdom',
    globals: true,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['node_modules/**', 'dist/**', 'test/**'],
    },
  },
});
