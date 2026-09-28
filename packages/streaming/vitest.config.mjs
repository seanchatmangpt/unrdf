import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    testTimeout: 10000,
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**', 'test/checkpointed-pipeline.node.test.mjs', 'test/shacl-core.node.test.mjs'], // node:test suites, run via node --test
    globals: false,
    isolate: true,
    execArgv: ['--expose-gc'],
  },
});
