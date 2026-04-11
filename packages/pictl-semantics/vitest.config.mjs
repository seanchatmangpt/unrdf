/**
 * @fileoverview Vitest configuration for @unrdf/pictl-semantics
 * PICTL Semantics Integration with @unrdf Federation
 */

import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Environment
    environment: 'node',

    // Pool configuration
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },

    // Execution
    concurrent: false,
    maxConcurrency: 1,

    // Timeouts
    testTimeout: 10_000,
    hookTimeout: 10_000,

    // Test discovery
    include: ['test/**/*.test.mjs'],
    exclude: ['node_modules/**', 'dist/**'],

    // Globals
    globals: false,

    // Isolation
    isolate: true,

    // Reporting
    reporter: ['verbose'],
    logLevel: 'info',

    // Coverage
    coverage: {
      provider: 'v8',
      reporter: ['text', 'text-summary'],
      exclude: [
        'node_modules/**',
        'test/**',
        'dist/**',
        '**/*.config.mjs',
        '**/*.test.mjs',
      ],
      include: ['src/**/*.mjs'],
      all: true,
    },

    // Retry logic
    retry: 1,
    bail: 0,

    // Watch mode
    watch: false,

    // Setup
    setupFiles: [],

    // Type checking
    typecheck: {
      enabled: false,
    },

    // Deps
    deps: {
      external: [],
      inline: [],
    },

    // Server
    server: {
      sourcemap: false,
    },

    // Worker
    worker: {
      pool: 'forks',
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
    },

    // Browser
    browser: {
      enabled: false,
    },

    // Pass with no tests (shouldn't happen)
    passWithNoTests: false,
  },

  // Resolve
  resolve: {
    alias: {},
  },

  // Define
  define: {},

  // Build
  build: {
    sourcemap: false,
    minify: false,
    target: 'node18',
  },

  // Cache and cleanup
  cacheDir: 'node_modules/.vite',
  clearScreen: false,

  // Logging
  logLevel: 'info',

  // Environment
  envPrefix: ['VITE_', 'VITEST_'],
  envDir: process.cwd(),

  // Mode
  mode: 'test',
  isProduction: false,
  isTest: true,
});
