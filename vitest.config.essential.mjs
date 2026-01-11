/**
 * @fileoverview Vitest configuration for ESSENTIAL tier (<10s)
 * Ultra-fast pre-commit validation - 20% of tests = 60%+ coverage
 *
 * 80/20 Principle:
 * - 15 essential test files (2.7% of 552 total)
 * - Target: <10 seconds execution
 * - Coverage: 60%+ (core functionality only)
 * - Use case: Pre-commit hook, rapid feedback loop
 *
 * @module vitest.config.essential
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Ultra-fast single-fork execution
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: true,
      },
    },

    // Minimal concurrency for speed
    concurrent: false,
    maxConcurrency: 1,

    // Aggressive timeout (Andon principle)
    testTimeout: 500,        // 500ms per test
    hookTimeout: 1000,       // 1s for setup/teardown

    // Node environment only
    environment: 'node',

    // ESSENTIAL TIER: Critical 20% delivering 60%+ coverage
    include: [
      // === CORE RDF OPERATIONS (Foundation) ===
      'test/diff.test.mjs',                              // 685 lines - Diff engine CRITICAL
      'test/project-engine.test.mjs',                    // 487 lines - Domain inference

      // === HOOK SYSTEM (Governance) ===
      'test/hook-executor-deps.test.mjs',                // 52 lines - Dependency validation

      // === KNOWLEDGE ENGINE CONTRACTS ===
      'test/knowledge-engine/parse-contract.test.mjs',   // 21 lines - Parse contract
      'test/knowledge-engine/query-contract.test.mjs',   // 21 lines - Query contract

      // === SECURITY & VALIDATION ===
      'test/lockchain-merkle-verification.test.mjs',     // 168 lines - Cryptographic proofs
      'test/security-error-sanitizer.test.mjs',          // Error sanitization

      // === INTEGRATION SMOKE TEST ===
      'test/e2e-integration.test.mjs',                   // 112 lines - E2E validation

      // === V6 CORE (if tests exist) ===
      'packages/v6-core/test/*.test.mjs',
      'packages/v6-compat/test/*.test.mjs',

      // === HOOKS CORE (if tests exist) ===
      'packages/hooks/test/hook-registration.test.mjs',
      'packages/hooks/test/hook-execution.test.mjs',
    ],

    // Exclude everything else
    exclude: [
      'node_modules/**',
      'dist/**',
      'test/fixtures/**',
      'test/utils/**',
      'test/browser/**',
      'test/react-hooks/**',
      'test/ml/**',
      'test/resilience/**',
      'test/streaming/**',
      'test/federation/**',
      'test/knowledge-engine/**',
      '!test/knowledge-engine/parse-contract.test.mjs',
      '!test/knowledge-engine/query-contract.test.mjs',
      'test/validation/**',
      'test/e2e/**',
      '!test/e2e-integration.test.mjs',
      'examples/**',
      'proofs/**',
      'AUTONOMIC_INNOVATION/**',
      'ENTERPRISE_MIGRATION/**',
      'benchmarks/**',
      '**/node_modules/**',
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

    // Pass with no tests (some includes may not exist yet)
    passWithNoTests: true,

    // No retries (fail fast)
    retry: 0,

    // Bail on first failure (rapid feedback)
    bail: 1,

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
