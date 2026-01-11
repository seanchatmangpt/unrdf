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

    // Test timeout - 2s per test for fast tier (Andon principle)
    testTimeout: 2_000,
    hookTimeout: 5_000,

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

    // 80/20 Fast Test Suite: Essential (20%) + Important (30%) = 75%+ coverage
    // Total: ~45 tests = 8% of full suite but 80% of value
    // Target: <30 seconds execution
    include: [
      // === TIER 1: ESSENTIAL (Core 20%) ===
      // Core RDF operations
      "test/diff.test.mjs",                              // 685 lines - Diff engine CRITICAL
      "test/project-engine.test.mjs",                    // 487 lines - Domain inference

      // Hook system
      "test/hook-executor-deps.test.mjs",                // 52 lines - Dependency validation

      // Knowledge engine contracts
      "test/knowledge-engine/parse-contract.test.mjs",   // 21 lines - Parse contract
      "test/knowledge-engine/query-contract.test.mjs",   // 21 lines - Query contract

      // Security & validation
      "test/lockchain-merkle-verification.test.mjs",     // 168 lines - Cryptographic proofs
      "test/security-error-sanitizer.test.mjs",          // Error sanitization

      // Integration smoke test
      "test/e2e-integration.test.mjs",                   // 112 lines - E2E validation

      // V6 core (if exists)
      "packages/v6-core/test/*.test.mjs",
      "packages/v6-compat/test/*.test.mjs",

      // Hooks core (if exists)
      "packages/hooks/test/hook-registration.test.mjs",
      "packages/hooks/test/hook-execution.test.mjs",

      // === TIER 2: IMPORTANT (Next 30% for 75% total coverage) ===
      // Optimization & performance
      "test/dark-matter-80-20.test.mjs",                 // 362 lines - Core optimization
      "test/query-optimizer-cache.test.mjs",             // Query optimization

      // YAWL workflow engine (if exists)
      "packages/yawl/test/task-activation.test.mjs",
      "packages/yawl/test/workflow-execution.test.mjs",

      // Streaming & real-time (if exists)
      "packages/streaming/test/change-feed.test.mjs",
      "packages/streaming/test/real-time-sync.test.mjs",

      // KGC governance (if exists)
      "packages/kgc-runtime/test/governance.test.mjs",
      "packages/kgc-4d/test/universe-freeze.test.mjs",
      "packages/receipts/test/receipt-chain.test.mjs",

      // Resilience & circuit breaking
      "test/knowledge-engine/utils/circuit-breaker.test.mjs",  // 505 lines - Circuit breaker
      "test/knowledge-engine/utils/ring-buffer.test.mjs",      // 354 lines - Ring buffer

      // Federation basics (if exists)
      "packages/federation/test/distributed-query.test.mjs",
      "packages/consensus/test/raft-basic.test.mjs",

      // CLI core
      "test/cli.test.mjs",
      "packages/cli/test/commands.test.mjs",
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
