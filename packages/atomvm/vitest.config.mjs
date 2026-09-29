/**
 * @file Vitest Configuration for @unrdf/atomvm
 * @description
 * Node is the default environment: the runtime, broker and continuum code use
 * node:child_process / node:fs and import.meta.url, none of which work under
 * jsdom. DOM-dependent suites opt in with `// @vitest-environment jsdom`.
 * Suites written against node:test cannot be collected by vitest; they run via
 * `pnpm test:node` (node --test). Real-browser tests run via `pnpm test:playwright`.
 */

import { defineConfig } from 'vitest/config';

/** Suites authored with node:test; kept in one list so vitest and `test:node` agree. */
export const NODE_TEST_SUITES = [
  'test/armstrong-kernel-chicago.test.mjs',
  'test/innovation-checkpoints.test.mjs',
  'test/otp-patterns-chicago.test.mjs',
  'test/otp-patterns-data-chicago.test.mjs',
  'test/otp-patterns-lifecycle-chicago.test.mjs',
  'test/otp-patterns-workers-chicago.test.mjs',
  'test/part-admission-broker.test.mjs',
  'test/part-admission-broker-falsifiers.test.mjs',
  'test/part-admission-broker-adversarial.test.mjs',
  'test/process-broker.test.mjs',
  'test/swarm-cluster.test.mjs',
];

export default defineConfig({
  test: {
    name: 'atomvm',
    environment: 'node',
    globals: true,
    include: ['test/**/*.test.mjs'],
    exclude: [
      'test/playwright/**',
      'node_modules/**',
      'dist/**',
      // node:test suites (vitest cannot collect them) - see `test:node`
      ...NODE_TEST_SUITES,
    ],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['src/**/*.mjs'],
      exclude: ['test/**', 'dist/**', 'public/**'],
      thresholds: {
        lines: 28,
        functions: 30,
        branches: 24,
        statements: 28,
      },
    },
    testTimeout: 10000,
    hookTimeout: 5000,
  },
});

