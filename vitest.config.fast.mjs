/**
 * @fileoverview Vitest configuration for fast pre-push testing (<30s)
 * 80/20 principle: Keep only critical tests delivering 80% of value
 * Used for git pre-push hook validation
 */
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Single-threaded execution for deterministic results
    pool: "forks",
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },

    // Disable concurrent execution for consistent timing
    concurrent: false,
    maxConcurrency: 1,

    // Test timeout - reasonable for RDF operations
    testTimeout: 15_000,
    hookTimeout: 15_000,

    // Coverage configuration
    coverage: {
      provider: "v8",
      reporter: ["text"],
      exclude: [
        "node_modules/**",
        "test/**",
        "**/*.test.mjs",
        "**/*.config.mjs",
        "dist/**",
        "coverage/**",
      ],
      include: ["src/**/*.mjs"],
      // Target: 80%+ coverage on core functionality
      thresholds: {
        global: {
          branches: 80,
          functions: 80,
          lines: 80,
          statements: 80,
        },
      },
      all: true,
    },

    // Node.js environment (no browser, no React)
    environment: "node",

    // 80/20 Fast Test Suite: Keep only critical tests that verify core functionality
    // Total: ~900 lines = 5% of full suite but 80% of value
    include: [
      // v4.0: Core diff utilities (427 lines) - CRITICAL for diff engine
      "test/diff.test.mjs",

      // v3.0: Core parsing and querying (21 lines) - CONTRACT TESTS
      "test/knowledge-engine/parse-contract.test.mjs",
      "test/knowledge-engine/query-contract.test.mjs",

      // v3.0: Observability (992 lines) - KEY FEATURE, but only run smoke tests
      // NOTE: Full observability tests included in test:full
      // For now we include it but could reduce if needed

      // v4.0: Project engine core (487 lines) - CRITICAL for domain inference
      "test/project-engine.test.mjs",

      // v3.0: Dark matter 80/20 (362 lines) - CORE OPTIMIZATION FEATURE
      "test/dark-matter-80-20.test.mjs",

      // v3.0: CLI baseline (16 lines) - CLI smoke test
      "test/cli/baseline-cli.test.mjs",

      // v3.1.0: E2E integration (110 lines) - CRITICAL end-to-end validation
      "test/e2e-integration.test.mjs",

      // v3.0: RingBuffer utilities (354 lines) - PERFORMANCE CRITICAL
      "test/knowledge-engine/utils/ring-buffer.test.mjs",

      // v3.0: Circuit breaker (505 lines) - RESILIENCE CRITICAL
      "test/knowledge-engine/utils/circuit-breaker.test.mjs",

      // v3.0: Lockchain verification (168 lines) - SECURITY CRITICAL
      "test/lockchain-merkle-verification.test.mjs",

      // v3.0: Hooks dependency validation (52 lines) - HOOK ENGINE VALIDATION
      "test/hook-executor-deps.test.mjs",
    ],

    exclude: [
      "node_modules/**",
      "dist/**",
      "test/fixtures/**",
      "test/utils/**",

      // Exclude large federation tests (300+ lines each) - nice to have, not critical
      "test/federation/**",

      // Exclude slow sandbox tests (500+ lines, native modules)
      "test/knowledge-engine/sandbox/**",

      // Exclude browser compatibility tests (not relevant for pre-push)
      "test/browser/**",

      // Exclude validation/monitoring tests (not critical for pre-push)
      "test/validation/**",
      "test/knowledge-engine/monitoring/**",

      // Exclude advanced project-engine subdirectory tests (only run main)
      "test/project-engine/**",

      // Exclude streaming tests (advanced feature, not critical)
      "test/streaming/**",

      // Exclude profiling tests
      "test/profiling/**",
    ],

    // Minimal reporters for speed
    reporter: ["verbose"],

    // Setup files
    setupFiles: ["./test/setup/cleanup-hooks.mjs"],

    // Isolate tests
    isolate: true,

    // Pass with no tests
    passWithNoTests: true,

    // Retry failed tests once
    retry: 1,

    // Don't bail - run all tests even if some fail (for better diagnostics)
    bail: 0,

    // No watch mode for CI
    watch: false,

    // Type checking disabled (using JSDoc)
    typecheck: {
      enabled: false,
    },

    // Global configuration
    globals: false,
    logLevel: "info",
    silent: false,

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

  // Optimize dependencies
  optimizeDeps: {
    include: ["n3", "zod", "@comunica/query-sparql"],
    exclude: [],
  },

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
