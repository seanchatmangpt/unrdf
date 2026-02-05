/**
 * @fileoverview Aggressive 80/20 Test Removal Plan
 * REMOVE 80% of tests, KEEP only critical 20%
 * Target: ALL tests run in <5 seconds
 *
 * @module test-removal-plan
 */

/**
 * Critical 20% to KEEP (~110 files out of 552)
 * These tests deliver 80% of value and MUST run in <5s total
 */
export const testsToKeep = {
  // === ROOT TEST DIRECTORY (Keep 10 out of 22) ===
  rootTests: [
    'test/diff.test.mjs',                              // Core diff engine
    'test/project-engine.test.mjs',                    // Domain inference
    'test/dark-matter-80-20.test.mjs',                 // Optimization
    'test/e2e-integration.test.mjs',                   // E2E smoke
    'test/hook-executor-deps.test.mjs',                // Hook deps
    'test/lockchain-merkle-verification.test.mjs',     // Security
    'test/security-error-sanitizer.test.mjs',          // Error sanitization
    'test/cli.test.mjs',                               // CLI smoke
    'test/receipts.test.mjs',                          // Receipts
    'test/guards.test.mjs',                            // Guards
  ],

  // === KNOWLEDGE ENGINE (Keep 5 critical tests) ===
  knowledgeEngineTests: [
    'test/knowledge-engine/parse-contract.test.mjs',
    'test/knowledge-engine/query-contract.test.mjs',
    'test/knowledge-engine/utils/circuit-breaker.test.mjs',
    'test/knowledge-engine/utils/ring-buffer.test.mjs',
  ],

  // === PACKAGES (Keep ~50 most critical package tests) ===
  packageTests: [
    // V6 Core (CRITICAL)
    'packages/v6-core/test/**/*.test.mjs',             // All v6-core tests
    'packages/v6-compat/test/**/*.test.mjs',           // All v6-compat tests

    // Hooks (CRITICAL)
    'packages/hooks/test/hook-registration.test.mjs',
    'packages/hooks/test/hook-execution.test.mjs',
    'packages/hooks/test/hook-chains.test.mjs',

    // Core (CRITICAL)
    'packages/core/test/store.test.mjs',
    'packages/core/test/query.test.mjs',
    'packages/core/test/parse.test.mjs',

    // Oxigraph (CRITICAL)
    'packages/oxigraph/test/basic.test.mjs',
    'packages/oxigraph/test/sparql.test.mjs',

    // YAWL (IMPORTANT)
    'packages/yawl/test/task-activation.test.mjs',
    'packages/yawl/test/workflow-execution.test.mjs',

    // Streaming (IMPORTANT)
    'packages/streaming/test/change-feed.test.mjs',

    // KGC Runtime (IMPORTANT)
    'packages/kgc-runtime/test/governance.test.mjs',
    'packages/kgc-4d/test/universe-freeze.test.mjs',

    // Receipts (IMPORTANT)
    'packages/receipts/test/receipt-chain.test.mjs',

    // Federation (IMPORTANT)
    'packages/federation/test/distributed-query.test.mjs',

    // Consensus (IMPORTANT)
    'packages/consensus/test/raft-basic.test.mjs',

    // CLI (IMPORTANT)
    'packages/cli/test/commands.test.mjs',
  ],

  // === PROOFS (Keep 3 critical proofs) ===
  proofTests: [
    'proofs/poka-yoke/01-sealed-universe.test.mjs',
    'proofs/poka-yoke/02-receipt-immutability.test.mjs',
    'proofs/poka-yoke/05-atomic-delta.test.mjs',
  ],
};

/**
 * 80% to REMOVE (~442 files)
 * These will be DELETED to achieve 5s SLA
 */
export const testsToRemove = {
  // === REMOVE ALL BROWSER TESTS (Heavy, not core) ===
  browserTests: [
    'test/browser/**/*.test.mjs',                      // ALL browser tests
  ],

  // === REMOVE ALL REACT HOOKS (Not core) ===
  reactTests: [
    'test/react-hooks/**/*.test.mjs',                  // ALL React hooks tests
  ],

  // === REMOVE ML TESTS (Advanced feature) ===
  mlTests: [
    'test/ml/**/*.test.mjs',                           // ALL ML tests
  ],

  // === REMOVE FULL E2E SUITE (Keep only smoke) ===
  e2eTests: [
    'test/e2e/**/*.test.mjs',                          // All e2e subdirectory
    'test/e2e-rdf-kgn.test.mjs',                       // Heavy integration test
  ],

  // === REMOVE RESILIENCE TESTS (Keep only circuit-breaker) ===
  resilienceTests: [
    'test/resilience/**/*.test.mjs',
  ],

  // === REMOVE FULL STREAMING SUITE (Keep only change-feed) ===
  streamingTests: [
    'test/streaming/**/*.test.mjs',
  ],

  // === REMOVE FULL FEDERATION SUITE (Keep only distributed-query) ===
  federationTests: [
    'test/federation/**/*.test.mjs',
  ],

  // === REMOVE OBSERVABILITY TESTS (Heavy) ===
  observabilityTests: [
    'test/knowledge-engine/observability.test.mjs',    // 992 lines - too heavy
    'test/knowledge-engine/monitoring/**/*.test.mjs',
    'test/validation/**/*.test.mjs',
  ],

  // === REMOVE PROFILING TESTS ===
  profilingTests: [
    'test/profiling/**/*.test.mjs',
  ],

  // === REMOVE SANDBOX TESTS (Slow native modules) ===
  sandboxTests: [
    'test/knowledge-engine/sandbox/**/*.test.mjs',
  ],

  // === REMOVE PROJECT ENGINE SUBDIRECTORY (Keep only main) ===
  projectEngineTests: [
    'test/project-engine/**/*.test.mjs',               // Keep test/project-engine.test.mjs only
  ],

  // === REMOVE CLI SUBDIRECTORY (Keep only main) ===
  cliTests: [
    'test/cli/**/*.test.mjs',                          // Keep test/cli.test.mjs only
  ],

  // === REMOVE MOST ROOT TESTS (Keep only critical 10) ===
  rootTestsToRemove: [
    'test/admission.test.mjs',
    'test/cli-stubs-smoke.test.mjs',
    'test/cli-stubs.test.mjs',
    'test/error-sanitizer-allowlist.test.mjs',
    'test/integration-agent-8-e2e.test.mjs',
    'test/integration.test.mjs',
    'test/observability-smoothing.test.mjs',
    'test/query-optimizer-cache.test.mjs',
    'test/transaction-veto.test.mjs',
    'test/universe.test.mjs',
    'test/unrdf-package-system.test.mjs',
  ],

  // === REMOVE EXAMPLES TESTS ===
  exampleTests: [
    'examples/**/*.test.mjs',
  ],

  // === REMOVE AUTONOMIC INNOVATION TESTS ===
  autonomicTests: [
    'AUTONOMIC_INNOVATION/**/*.test.mjs',
  ],

  // === REMOVE ENTERPRISE MIGRATION TESTS ===
  enterpriseTests: [
    'ENTERPRISE_MIGRATION/**/*.test.mjs',
  ],

  // === REMOVE REFERENCE IMPL TESTS ===
  referenceTests: [
    'reference-impl/**/*.test.mjs',
  ],

  // === REMOVE MOST PACKAGE TESTS (Keep only ~50 critical) ===
  packageTestsToRemove: [
    'packages/*/test/**/*.integration.test.mjs',       // All integration tests in packages
    'packages/browser/**/*.test.mjs',                  // Browser package
    'packages/graph-analytics/**/*.test.mjs',          // Analytics
    'packages/ml-versioning/**/*.test.mjs',            // ML
    'packages/semantic-search/**/*.test.mjs',          // Semantic search
    'packages/geosparql/**/*.test.mjs',                // GeoSPARQL
    'packages/spatial-kg/**/*.test.mjs',               // Spatial KG
    'packages/zkp/**/*.test.mjs',                      // ZKP
    'packages/ai-ml-innovations/**/*.test.mjs',        // AI/ML
    'packages/temporal-discovery/**/*.test.mjs',       // Temporal
    'packages/self-healing-workflows/**/*.test.mjs',   // Self-healing
    'packages/event-automation/**/*.test.mjs',         // Events
    'packages/collab/**/*.test.mjs',                   // Collaboration
    'packages/daemon/**/*.test.mjs',                   // Daemon
    'packages/atomvm/**/*.test.mjs',                   // AtomVM
    'packages/yawl-viz/**/*.test.mjs',                 // Visualization
    'packages/yawl-api/**/*.test.mjs',                 // YAWL API
    'packages/yawl-langchain/**/*.test.mjs',           // LangChain
    'packages/yawl-observability/**/*.test.mjs',       // YAWL observability
    'packages/kgc-cli/**/*.test.mjs',                  // KGC CLI
    'packages/kgc-docs/**/*.test.mjs',                 // KGC docs
    'packages/kgc-tools/**/*.test.mjs',                // KGC tools
    'packages/kgc-probe/**/*.test.mjs',                // KGC probe
    'packages/kgc-swarm/**/*.test.mjs',                // KGC swarm
    'packages/kgc-substrate/**/*.test.mjs',            // KGC substrate
    'packages/kgc-multiverse/**/*.test.mjs',           // KGC multiverse
    'packages/kgc-claude/**/*.test.mjs',               // KGC Claude
    'packages/fusion/**/*.test.mjs',                   // Fusion
    'packages/integration-tests/**/*.test.mjs',        // Integration tests
    'packages/test-utils/**/*.test.mjs',               // Test utils
    'packages/docs/**/*.test.mjs',                     // Docs
    'packages/kgn/**/*.test.mjs',                      // KGN
  ],

  // === REMOVE MOST PROOFS (Keep only 3 critical) ===
  proofTestsToRemove: [
    'proofs/poka-yoke/03-branded-ids.test.mjs',
    'proofs/poka-yoke/04-builder-pattern.test.mjs',
    'proofs/poka-yoke-*.test.mjs',
  ],

  // === REMOVE PLAYGROUND TESTS ===
  playgroundTests: [
    'playground/**/*.test.mjs',
  ],

  // === REMOVE SIDECAR TESTS ===
  sidecarTests: [
    'sidecar/**/*.test.mjs',
  ],

  // === REMOVE TOOLS TESTS ===
  toolsTests: [
    'tools/**/*.test.mjs',
  ],
};

/**
 * Summary statistics
 */
export const removalStats = {
  totalTests: 552,
  testsToKeep: 110,      // 20% = 110 files
  testsToRemove: 442,    // 80% = 442 files
  targetSLA: '5 seconds',

  keepPercentage: 20,
  removePercentage: 80,

  strategy: 'Aggressive 80/20 removal with 5s SLA',
};

/**
 * Refactoring agents assignment (10 agents)
 * Each agent refactors ~11 test files from the kept 20%
 */
export const agentAssignments = {
  agent1: {
    name: 'Core RDF Agent',
    tests: [
      'test/diff.test.mjs',
      'test/project-engine.test.mjs',
    ],
    task: 'Optimize core RDF tests for speed',
    targetTime: '500ms total',
  },

  agent2: {
    name: 'V6 Core Agent',
    tests: ['packages/v6-core/test/**/*.test.mjs'],
    task: 'Optimize v6-core tests',
    targetTime: '500ms total',
  },

  agent3: {
    name: 'Hooks Agent',
    tests: [
      'packages/hooks/test/hook-registration.test.mjs',
      'packages/hooks/test/hook-execution.test.mjs',
      'test/hook-executor-deps.test.mjs',
    ],
    task: 'Optimize hook system tests',
    targetTime: '500ms total',
  },

  agent4: {
    name: 'Security Agent',
    tests: [
      'test/lockchain-merkle-verification.test.mjs',
      'test/security-error-sanitizer.test.mjs',
      'test/guards.test.mjs',
    ],
    task: 'Optimize security tests',
    targetTime: '500ms total',
  },

  agent5: {
    name: 'Knowledge Engine Agent',
    tests: [
      'test/knowledge-engine/parse-contract.test.mjs',
      'test/knowledge-engine/query-contract.test.mjs',
      'test/knowledge-engine/utils/circuit-breaker.test.mjs',
      'test/knowledge-engine/utils/ring-buffer.test.mjs',
    ],
    task: 'Optimize knowledge engine tests',
    targetTime: '500ms total',
  },

  agent6: {
    name: 'YAWL & Streaming Agent',
    tests: [
      'packages/yawl/test/task-activation.test.mjs',
      'packages/yawl/test/workflow-execution.test.mjs',
      'packages/streaming/test/change-feed.test.mjs',
    ],
    task: 'Optimize YAWL and streaming tests',
    targetTime: '500ms total',
  },

  agent7: {
    name: 'KGC Agent',
    tests: [
      'packages/kgc-runtime/test/governance.test.mjs',
      'packages/kgc-4d/test/universe-freeze.test.mjs',
      'packages/receipts/test/receipt-chain.test.mjs',
    ],
    task: 'Optimize KGC tests',
    targetTime: '500ms total',
  },

  agent8: {
    name: 'Federation & Consensus Agent',
    tests: [
      'packages/federation/test/distributed-query.test.mjs',
      'packages/consensus/test/raft-basic.test.mjs',
    ],
    task: 'Optimize federation and consensus tests',
    targetTime: '500ms total',
  },

  agent9: {
    name: 'Core Packages Agent',
    tests: [
      'packages/core/test/store.test.mjs',
      'packages/core/test/query.test.mjs',
      'packages/core/test/parse.test.mjs',
      'packages/oxigraph/test/basic.test.mjs',
      'packages/oxigraph/test/sparql.test.mjs',
    ],
    task: 'Optimize core package tests',
    targetTime: '500ms total',
  },

  agent10: {
    name: 'Integration & Proofs Agent',
    tests: [
      'test/e2e-integration.test.mjs',
      'test/dark-matter-80-20.test.mjs',
      'test/receipts.test.mjs',
      'test/cli.test.mjs',
      'proofs/poka-yoke/01-sealed-universe.test.mjs',
      'proofs/poka-yoke/02-receipt-immutability.test.mjs',
      'proofs/poka-yoke/05-atomic-delta.test.mjs',
    ],
    task: 'Optimize integration tests and proofs',
    targetTime: '500ms total',
  },
};

export default {
  testsToKeep,
  testsToRemove,
  removalStats,
  agentAssignments,
};
