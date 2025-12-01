/**
 * @fileoverview Vitest workspace configuration for unrdf
 * TRIZ #1 Segmentation - Separate test suites for optimal parallelization
 *
 * Workspaces:
 * - unit: Fast unit tests with thread pool
 * - integration: E2E tests with singleFork for isolation
 * - react-hooks: React hook tests with thread pool
 *
 * Usage:
 *   pnpm test                    # Run all workspaces
 *   pnpm test --project unit     # Run only unit tests
 *   pnpm test --project e2e      # Run only e2e tests
 *   pnpm test --project hooks    # Run only react hooks tests
 */
import { defineWorkspace } from 'vitest/config';

export default defineWorkspace([
  // Project Engine tests workspace - Node.js only, isolated
  {
    extends: './vitest.config.mjs',
    test: {
      name: 'project-engine',
      pool: 'forks',
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
      include: [
        'test/project-engine/**/*.test.mjs',
      ],
      exclude: [
        'node_modules/**',
        // Performance tests are environment-dependent
        'test/performance/**/*.test.mjs',
      ],
      testTimeout: 30_000,
      hookTimeout: 20_000,
      environment: 'node',
      // Force node environment - ignore file-level annotations that might conflict
      env: {
        NODE_ENV: 'test',
      },
    },
  },

  // Unit tests workspace - fast, parallel execution
  {
    extends: './vitest.config.mjs',
    test: {
      name: 'unit',
      pool: 'threads',
      poolOptions: {
        threads: {
          singleThread: false,
          minThreads: 1,
          maxThreads: 4,
        },
      },
      include: [
        'test/knowledge-engine/**/*.test.mjs',
        'test/cli/*.test.mjs',
        'test/sidecar/*.test.mjs',
        'test/dark-matter-80-20.test.mjs',
      ],
      exclude: [
        'node_modules/**',
        'test/knowledge-engine/sandbox/executor-detection.test.mjs',
        'test/knowledge-engine/sandbox/isolated-vm.test.mjs',
        'test/project-engine/**/*.test.mjs', // Exclude project-engine from unit tests
        // Performance tests are environment-dependent and should not run in unit tests
        'test/performance/**/*.test.mjs',
      ],
      testTimeout: 15_000,
      hookTimeout: 15_000,
      environment: 'node',
    },
  },

  // Integration/E2E tests workspace - isolated, sequential
  {
    extends: './vitest.config.mjs',
    test: {
      name: 'e2e',
      pool: 'forks',
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
      include: [
        'test/e2e/**/*.test.mjs',
        'test/validation/**/*.test.mjs',
      ],
      exclude: [
        'node_modules/**',
        'test/project-engine/**/*.test.mjs', // Exclude project-engine from e2e
        // Performance tests are environment-dependent
        'test/performance/**/*.test.mjs',
      ],
      testTimeout: 60_000,
      hookTimeout: 30_000,
      retry: 1,
      environment: 'node',
    },
  },

  // React hooks tests workspace - parallel with jsdom-like mocking
  {
    extends: './vitest.config.mjs',
    test: {
      name: 'hooks',
      pool: 'threads',
      poolOptions: {
        threads: {
          singleThread: false,
          minThreads: 1,
          maxThreads: 4,
        },
      },
      include: [
        'test/react-hooks/**/*.test.mjs',
      ],
      exclude: [
        'node_modules/**',
        'test/project-engine/**/*.test.mjs', // Exclude project-engine from hooks
        // Exclude tests that require jsdom/browser DOM (document not defined in node)
        // These use @testing-library/react renderHook which requires document
        'test/react-hooks/core.test.mjs',                // Root level core tests
        'test/react-hooks/core/**/*.test.mjs',           // useStore, useTriples, useTerms, etc.
        'test/react-hooks/query/**/*.test.mjs',          // useSPARQLQuery, useDeltaQuery, etc.
        'test/react-hooks/storage/**/*.test.mjs',        // useAuditTrail, useQuadStore, etc.
        'test/react-hooks/cache/**/*.test.mjs',          // useQueryCache, useCacheStats, etc.
        'test/react-hooks/context/**/*.test.mjs',        // KnowledgeEngineProvider
        'test/react-hooks/knowledge-hooks/**/*.test.mjs', // useHookManager, useHookRegistry
        'test/react-hooks/integration/**/*.test.mjs',    // multi-hook-interaction, performance-benchmarks
        'test/react-hooks/federation/**/*.test.mjs',     // useConsensusManager, useDistributedQuery, etc.
        'test/react-hooks/streaming/**/*.test.mjs',      // useChangeFeed, useStreamProcessor, etc.
        // Performance tests are environment-dependent
        'test/performance/**/*.test.mjs',
      ],
      testTimeout: 20_000,
      hookTimeout: 15_000,
      environment: 'node', // React hooks can run in node with proper mocking
    },
  },

  // Browser tests workspace - isolated for browser-specific code
  {
    extends: './vitest.config.mjs',
    test: {
      name: 'browser',
      pool: 'forks',
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
      include: [
        'test/browser/**/*.test.mjs',
      ],
      exclude: [
        'node_modules/**',
        'test/browser/browser-compatibility.test.mjs',
        'test/browser/playwright.spec.mjs',
        'test/project-engine/**/*.test.mjs', // Exclude project-engine from browser
        'test/performance/**/*.test.mjs', // Performance tests are environment-dependent
      ],
      testTimeout: 30_000,
      hookTimeout: 20_000,
      environment: 'node', // Browser tests can use node with browser shims
    },
  },

  // Streaming tests workspace - dedicated for streaming features
  {
    extends: './vitest.config.mjs',
    test: {
      name: 'streaming',
      pool: 'threads',
      poolOptions: {
        threads: {
          singleThread: false,
          minThreads: 1,
          maxThreads: 2,
        },
      },
      include: [
        'test/streaming/**/*.test.mjs',
        'test/federation/**/*.test.mjs',
      ],
      exclude: [
        'node_modules/**',
        'test/project-engine/**/*.test.mjs', // Exclude project-engine from streaming
        'test/performance/**/*.test.mjs', // Performance tests are environment-dependent
      ],
      testTimeout: 30_000,
      hookTimeout: 20_000,
      environment: 'node',
    },
  },
]);
