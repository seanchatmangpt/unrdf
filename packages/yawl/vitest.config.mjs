/**
 * @fileoverview DX-optimized Vitest configuration for unrdf v6.0.0
 * - Parallel execution with maxForks: 10
 * - 5s SLA (Andon Principle) for all tests
 * - Multiple reporters (verbose + junit for CI)
 * - ~106 remaining tests after 80/20 consolidation
 */
import { defineConfig } from "vitest/config";

const isCI = process.env.CI === "true";

export default defineConfig({
  test: {
    // Parallel execution - 2-4x speedup
    pool: "forks",
    poolOptions: {
      forks: {
        maxForks: 10,
      },
    },

    // Test timeout - 5s SLA (Andon Principle)
    testTimeout: 5000,

    // Environment
    environment: "node",

    // Bail on first failure for fast feedback
    bail: true,

    // File patterns - 106 remaining tests after consolidation
    include: [
      // Core tests (root test/ directory)
      "test/cli.test.mjs",
      "test/dark-matter-80-20.test.mjs",
      "test/diff.test.mjs",
      "test/e2e-integration.test.mjs",
      "test/guards.test.mjs",
      "test/knowledge-engine/parse-contract.test.mjs",
      "test/knowledge-engine/query-contract.test.mjs",
      "test/knowledge-engine/utils/circuit-breaker.test.mjs",
      "test/knowledge-engine/utils/ring-buffer.test.mjs",
      "test/lockchain-merkle-verification.test.mjs",
      "test/project-engine.test.mjs",
      "test/receipts.test.mjs",
      "test/security-error-sanitizer.test.mjs",

      // CLI tests
      "packages/cli/test/cli/decision-fabric.test.mjs",
      "packages/cli/test/cli/rdf-commands.test.mjs",
      "packages/cli/test/daemon-cli.test.mjs",

      // Core package tests
      "packages/consensus/test/consensus.test.mjs",
      "packages/core/test/config.test.mjs",
      "packages/core/test/core.test.mjs",
      "packages/core/test/debug.test.mjs",
      "packages/core/test/docs-alignment.test.mjs",
      "packages/core/test/enhanced-errors.test.mjs",

      // Federation
      "packages/federation/test/federation.test.mjs",

      // Hooks system
      "packages/hooks/test/hooks.test.mjs",
      "packages/hooks/test/knowledge-hook-manager.test.mjs",
      "packages/hooks/test/policy-compiler.test.mjs",

      // KGC 4D temporal engine
      "packages/kgc-4d/test/freeze.test.mjs",
      "packages/kgc-4d/test/store.test.mjs",
      "packages/kgc-4d/test/time.test.mjs",

      // KGC runtime governance
      "packages/kgc-runtime/test/transaction.test.mjs",
      "packages/kgc-runtime/test/validators.test.mjs",
      "packages/kgc-runtime/test/work-item.test.mjs",

      // Oxigraph SPARQL engine
      "packages/oxigraph/test/application-jtbd.test.mjs",
      "packages/oxigraph/test/basic.test.mjs",
      "packages/oxigraph/test/benchmark.test.mjs",

      // Receipts & batch processing
      "packages/receipts/test/batch-receipt-generator.test.mjs",
      "packages/receipts/test/merkle-batcher.test.mjs",
      "packages/receipts/test/pq-receipts.test.mjs",

      // Streaming & real-time
      "packages/streaming/test/subscription.test.mjs",
      "packages/streaming/test/sync-protocol.test.mjs",

      // V6 compatibility & core
      "packages/v6-compat/test/batch-1-validation.test.mjs",
      "packages/v6-compat/test/integration-node.test.mjs",
      "packages/v6-compat/test/integration.test.mjs",
      "packages/v6-core/test/implementations.test.mjs",

      // YAWL workflow engine
      "packages/yawl/test/architecture.test.mjs",
      "packages/yawl/test/cancellation.test.mjs",
      "packages/yawl/test/integration-core.test.mjs",
      "packages/yawl/test/task-activation.test.mjs",
      "packages/yawl/test/workflow-basics.test.mjs",

      // Proofs & formal verification
      "proofs/poka-yoke/01-sealed-universe.test.mjs",
      "proofs/poka-yoke/02-receipt-immutability.test.mjs",
      "proofs/poka-yoke/05-atomic-delta.test.mjs",

      // New implementation tests
      "src/admission/admission-engine.test.mjs",
      "src/compression/compression.test.mjs",
      "src/integration.test.mjs",
      "src/measurement/measurement.test.mjs",
      "src/monitoring/monitoring.test.mjs",
      "src/monorepo-admission/monorepo-admission.test.mjs",
      "src/multi-swarm/__tests__/coordination.test.mjs",
      "src/multi-swarm/__tests__/queen.test.mjs",
      "src/multi-swarm/__tests__/worker-swarm.test.mjs",
      "src/narrative-state-chain/narrative-state-chain.test.mjs",
      "src/orchestration/orchestration.test.mjs",
      "src/orchestration/parallel-orchestration.test.mjs",

      // Benchmarks (validation only)
      "benchmarks/integration.test.mjs",
      "benchmarks/validation.test.mjs",
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

    // Multi-reporter: verbose for dev, junit for CI
    reporter: isCI
      ? ["junit", "default"]
      : ["verbose"],

    // JUnit output for CI/CD pipelines
    outputFile: {
      junit: "./test-results/junit.xml",
    },

    // Standard settings
    globals: false,
    isolate: true,
    passWithNoTests: false,

    // Coverage configuration (when --coverage used)
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      exclude: [
        "**/node_modules/**",
        "**/dist/**",
        "**/*.test.mjs",
        "**/*.spec.mjs",
      ],
    },

    // Watch mode optimization (80/20 DX)
    watchExclude: [
      "**/node_modules/**",
      "**/dist/**",
      "**/coverage/**",
      "**/.git/**",
      "**/test-results/**",
    ],
  },
});
