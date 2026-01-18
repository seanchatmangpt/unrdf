/**
 * @fileoverview Ultra-fast vitest configuration for core RDF tests (<1s total)
 * 80/20 Pure - Only 5 test files with 12 essential tests
 * All use mocks, pure functions, NO external dependencies
 * Target: <1000ms total execution
 */
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Single fork for minimal overhead (<100ms startup)
    pool: "forks",
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },

    // Test timeout - 2s per test (generous for safety)
    testTimeout: 2000,
    hookTimeout: 2000,

    // Node.js environment (no browser overhead)
    environment: "node",

    // Bail on first failure for fast feedback
    bail: true,

    // No retries - fail fast
    retry: 0,

    // 80/20 ULTRA-FAST: Only 5 core test files (12 tests total, <50ms each)
    include: [
      // Core RDF diff operations (3 tests, <50ms)
      "test/diff.test.mjs",

      // Core project engine operations (3 tests, <50ms)
      "test/project-engine.test.mjs",

      // Knowledge substrate smoke test (2 tests, <50ms)
      "test/dark-matter-80-20.test.mjs",

      // E2E integration smoke test (1 test, <50ms)
      "test/e2e-integration.test.mjs",

      // Guard validation tests (3 tests, <50ms)
      "test/guards.test.mjs",
    ],

    exclude: [
      "node_modules/**",
      "dist/**",
      "coverage/**",
      "test/fixtures/**",
      "test/utils/**",
      "test/setup/**",
      "docs/**",
      "examples/**",
      "AUTONOMIC_INNOVATION/**",
      "ENTERPRISE_MIGRATION/**",
      "playground/**",
      "packages/**",
      "proofs/**",
      "src/**",
    ],

    // Minimal reporters for speed
    reporter: ["verbose"],

    // Isolate tests
    isolate: true,

    // Pass with no tests
    passWithNoTests: false,

    // No watch mode
    watch: false,

    // Type checking disabled (using JSDoc)
    typecheck: {
      enabled: false,
    },

    // Global configuration
    globals: false,
    logLevel: "info",
    silent: false,

    // Coverage disabled for speed
    coverage: {
      enabled: false,
    },

    // Deps configuration
    deps: {
      external: [],
      inline: [],
    },

    // Server configuration
    server: {
      sourcemap: false,
    },

    // Worker configuration
    worker: {
      pool: "forks",
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
    },

    // Browser disabled
    browser: {
      enabled: false,
    },
  },

  // Resolve configuration
  resolve: {
    alias: {},
  },

  // Define global constants
  define: {},

  // Build configuration
  build: {
    sourcemap: false,
    minify: false,
    target: "node18",
  },

  // CSS and JSON (not used for Node.js)
  css: {},
  json: {},

  // Assets
  assetsInclude: [],
  publicDir: false,
  cacheDir: "node_modules/.vite",
  clearScreen: false,
  logLevel: "info",

  // Environment
  envPrefix: ["VITE_", "VITEST_"],
  envDir: process.cwd(),

  // Modes
  mode: "test",
  command: "test",
  isProduction: false,
  isTest: true,
});
