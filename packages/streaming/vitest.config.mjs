import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    testTimeout: 10000,
    include: ['test/**/*.test.mjs'],
    // node:test suites run via `pnpm test:node`
    exclude: ['node_modules/**', 'dist/**', 'test/**/*.node.test.mjs'],
    globals: false,
    isolate: true,
    pool: 'forks',
    execArgv: ['--expose-gc'], // memory-bound tests call global.gc()
  },
});
