/**
 * @fileoverview Vitest configuration for fast pre-push testing (<30s)
 * 80/20 principle: Keep only critical tests delivering 80% of value
 * Used for git pre-push hook validation
 *
 * Core Features:
 * - testTimeout: 5000ms (5s SLA - Andon Principle)
 * - maxForks: 10 (parallel execution)
 * - bail: true (fast failure)
 * - ~30 critical tests covering 80% of functionality
 */
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Parallel execution with maxForks: 10
    pool: "forks",
    poolOptions: {
      forks: {
        maxForks: 10,
      },
    },

    // Allow concurrent execution for speed
    concurrent: true,
    maxConcurrency: 10,

    // Test timeout - 5s SLA (Andon principle)
    testTimeout: 5000,
    hookTimeout: 5000,

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

    // 80/20 Fast Test Suite: ~30 critical tests = 80% of value in <30s
    // Target: <30 seconds execution
    include: [
      // === TIER 1: ESSENTIAL (Core 20%) ===
      // Core RDF operations (CRITICAL)
      "test/diff.test.mjs",                              // Core diff engine
      "test/project-engine.test.mjs",                    // Domain inference

      // Knowledge engine contracts
      "test/knowledge-engine/parse-contract.test.mjs",   // Parse contract
      "test/knowledge-engine/query-contract.test.mjs",   // Query contract

      // Security & validation
      "test/lockchain-merkle-verification.test.mjs",     // Cryptographic proofs
      "test/security-error-sanitizer.test.mjs",          // Error sanitization

      // Integration smoke test
      "test/e2e-integration.test.mjs",                   // E2E validation
      "test/guards.test.mjs",                            // Guard enforcement

      // === TIER 2: IMPORTANT (Next 30% for 75% total coverage) ===
      // Optimization & performance
      "test/dark-matter-80-20.test.mjs",                 // Core optimization

      // Resilience & circuit breaking
      "test/knowledge-engine/utils/circuit-breaker.test.mjs",  // Circuit breaker
      "test/knowledge-engine/utils/ring-buffer.test.mjs",      // Ring buffer

      // Core package tests (critical)
      "packages/core/test/core.test.mjs",
      "packages/core/test/config.test.mjs",

      // Hooks system
      "packages/hooks/test/hooks.test.mjs",

      // KGC essentials
      "packages/kgc-4d/test/freeze.test.mjs",
      "packages/kgc-4d/test/store.test.mjs",
      "packages/kgc-runtime/test/validators.test.mjs",

      // Receipts & verification
      "packages/receipts/test/batch-receipt-generator.test.mjs",
      "packages/receipts/test/merkle-batcher.test.mjs",

      // V6 core
      "packages/v6-core/test/implementations.test.mjs",
      "packages/v6-compat/test/batch-1-validation.test.mjs",

      // YAWL workflow essentials
      "packages/yawl/test/workflow-basics.test.mjs",
      "packages/yawl/test/cancellation.test.mjs",

      // Federation & consensus
      "packages/federation/test/federation.test.mjs",
      "packages/consensus/test/consensus.test.mjs",

      // CLI core
      "test/cli.test.mjs",
      "packages/cli/test/daemon-cli.test.mjs",

      // Proofs (formal verification)
      "proofs/poka-yoke/01-sealed-universe.test.mjs",
      "proofs/poka-yoke/02-receipt-immutability.test.mjs",
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
    ],

    // Minimal reporters for speed
    reporter: ["verbose"],

    // Setup files
    setupFiles: ["./test/setup/cleanup-hooks.mjs"],

    // Isolate tests
    isolate: true,

    // Fail fast on first error
    bail: true,

    // Pass with no tests
    passWithNoTests: false,

    // No retries - fail fast
    retry: 0,

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
