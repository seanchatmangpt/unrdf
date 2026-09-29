import { defineConfig } from 'vitest/config';
import { fileURLToPath } from 'node:url';

const nitroImports = fileURLToPath(new URL('./test/mocks/nitro-imports.mjs', import.meta.url));

export default defineConfig({
  resolve: {
    alias: { '#imports': nitroImports }
  },
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
