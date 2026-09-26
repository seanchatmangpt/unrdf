import { configDefaults, defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['test/**/*.test.mjs'],
    // node:test suites run by scripts/test-interchangeable-parts.mjs (vitest cannot collect them)
    exclude: [...configDefaults.exclude, 'test/interchangeable-part.test.mjs'],
    testTimeout: 60000, // 60s timeout for long-running tests (e.g., 100K quad performance tests)
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/**',
        'dist/**',
        'test/**',
        '**/*.config.mjs',
      ],
    },
  },
});
