import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    include: ['test/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    exclude: ['node_modules', 'dist', '.idea', '.git', '.cache'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      lines: 80,
      functions: 80,
      branches: 80,
      statements: 80,
      all: true,
      exclude: [
        'node_modules/',
        'test/',
        '**/*.config.js',
        '**/*.config.mjs',
        '**/dist/**',
      ],
    },
    globals: true,
    testTimeout: 5000,
    hookTimeout: 5000,
    reporters: ['default'],
    isolate: true,
    threads: true,
    maxThreads: 4,
    minThreads: 1,
    bail: 0,
    silent: false,
  },
});
