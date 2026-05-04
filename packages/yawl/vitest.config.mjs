import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    testTimeout: 10000,
    include: ['test/**/*.test.mjs'],
  },
  coverage: {
    provider: 'v8',
    reporter: ['text', 'json', 'html', 'lcov'],
    all: true,
    include: ['src/**/*.mjs'],
    exclude: [
      '**/*.test.mjs',
      '**/*.spec.mjs',
      '**/test/**',
      '**/examples/**'
    ],
    lines: 80,
    functions: 80,
    branches: 80,
    statements: 80
  }
});
