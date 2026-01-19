/**
 * @fileoverview Vitest configuration for ESSENTIAL tier (<10s)
 * Ultra-fast pre-commit validation
 *
 * Core Features:
 * - testTimeout: 5000ms (5s SLA - Andon Principle)
 * - maxForks: 10 (parallel execution)
 * - bail: true (fast failure)
 * - 15 essential test files = 60%+ coverage
 * - Target: <10 seconds execution
 * - Use case: Pre-commit hook, rapid feedback loop
 *
 * @module vitest.config.essential
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Parallel execution with maxForks: 10
    pool: 'forks',
    poolOptions: {
      forks: {
        maxForks: 10,
      },
    },

    // Allow concurrent execution for speed
    concurrent: true,
    maxConcurrency: 10,

    // Test timeout - 5s SLA (Andon principle)
    testTimeout: 5000,       // 5s per test
    hookTimeout: 5000,       // 5s for setup/teardown

    // Node environment only
    environment: 'node',

    // ESSENTIAL TIER: Critical 20% delivering 60%+ coverage
    // Target: <10 seconds execution time
    include: [
      // === CORE RDF OPERATIONS (Foundation) ===
      'test/diff.test.mjs',                              // Diff engine CRITICAL
      'test/project-engine.test.mjs',                    // Domain inference

      // === KNOWLEDGE ENGINE CONTRACTS ===
      'test/knowledge-engine/parse-contract.test.mjs',   // Parse contract
      'test/knowledge-engine/query-contract.test.mjs',   // Query contract

      // === SECURITY & VALIDATION ===
      'test/lockchain-merkle-verification.test.mjs',     // Cryptographic proofs
      'test/security-error-sanitizer.test.mjs',          // Error sanitization

      // === INTEGRATION SMOKE TEST ===
      'test/e2e-integration.test.mjs',                   // E2E validation

      // === CORE PACKAGE TESTS ===
      'packages/core/test/core.test.mjs',
      'packages/hooks/test/hooks.test.mjs',

      // === KGC & RECEIPTS ===
      'packages/kgc-4d/test/freeze.test.mjs',
      'packages/receipts/test/batch-receipt-generator.test.mjs',

      // === V6 CORE ===
      'packages/v6-core/test/implementations.test.mjs',

      // === YAWL WORKFLOW ===
      'packages/yawl/test/workflow-basics.test.mjs',

      // === PROOFS (Formal Verification) ===
      'proofs/poka-yoke/01-sealed-universe.test.mjs',
      'proofs/poka-yoke/02-receipt-immutability.test.mjs',

      // === CONSENSUS ===
      'packages/consensus/test/consensus.test.mjs',

      // === FEDERATION ===
      'packages/federation/test/federation.test.mjs',
    ],

    // Exclude everything else
    exclude: [
      'node_modules/**',
      'dist/**',
      'coverage/**',
      'test/fixtures/**',
      'test/utils/**',
      'test/setup/**',
      'docs/**',
      'examples/**',
      'AUTONOMIC_INNOVATION/**',
      'ENTERPRISE_MIGRATION/**',
      'playground/**',
      'src/**',
      'benchmarks/**',
    ],

    // Minimal coverage for speed
    coverage: {
      enabled: false,  // Disable for essential tier (speed priority)
    },

    // Minimal reporter for speed
    reporter: ['basic'],

    // Setup files
    setupFiles: ['./test/setup/cleanup-hooks.mjs'],

    // Isolate tests
    isolate: true,

    // Fail fast on first error
    bail: true,

    // No retries (fail fast)
    retry: 0,

    // Do not pass with no tests - essential tier must have tests
    passWithNoTests: false,

    // No watch mode
    watch: false,

    // No type checking
    typecheck: {
      enabled: false,
    },

    // Global config
    globals: false,
    logLevel: 'error',  // Minimal output
    silent: false,

    // Deps
    deps: {
      external: [],
      inline: [],
    },

    // Server config
    server: {
      sourcemap: false,
    },

    // Worker config
    worker: {
      pool: 'forks',
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
    },

    // No browser
    browser: {
      enabled: false,
    },
  },

  // Optimize dependencies
  optimizeDeps: {
    include: ['zod', '@unrdf/oxigraph'],
    exclude: [],
  },

  // Build config
  build: {
    sourcemap: false,
    minify: false,
    target: 'node18',
  },

  // Cache config
  cacheDir: 'node_modules/.vite',
  clearScreen: false,
  logLevel: 'error',

  // Environment
  envPrefix: ['VITE_', 'VITEST_'],
  envDir: process.cwd(),

  // Modes
  mode: 'test',
  isTest: true,
});
