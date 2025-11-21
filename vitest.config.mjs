/**
 * @fileoverview Vitest configuration for unrdf
 * Single-threaded execution for AI agent compatibility
 * 80/20 pruned test suite using glob patterns for core functionality
 */
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Single-threaded execution for AI agent compatibility
    pool: "forks",
    poolOptions: {
      forks: {
        // Single fork to avoid AI agent conflicts
        singleFork: true,
      },
    },

    // Disable concurrent execution
    concurrent: false,

    // Maximum number of concurrent test files
    maxConcurrency: 1,

    // Test timeout - generous for RDF operations
    testTimeout: 30_000,

    // Hook timeout
    hookTimeout: 30_000,

    // Coverage configuration
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      // Exclude test files and config files from coverage
      exclude: [
        "node_modules/**",
        "test/**",
        "**/*.test.mjs",
        "**/*.config.mjs",
        "dist/**",
        "coverage/**",
      ],
      // Include source files
      include: ["src/**/*.mjs"],
      // Coverage thresholds - targeting 100% for Node.js code
      thresholds: {
        global: {
          branches: 95,
          functions: 95,
          lines: 95,
          statements: 95,
        },
      },
      // v3.1.0: Extended excludes for browser-only and test infrastructure
      all: true,
    },

    // Environment configuration
    environment: "node",

    // File patterns - comprehensive test suite including v3.1.0 features
    include: [
      // v3.0 Core tests
      "test/knowledge-engine/parse.test.mjs",
      "test/dark-matter-80-20.test.mjs",
      "test/sidecar/client.test.mjs",
      "test/cli/*.test.mjs",

      // v3.1.0 Security & Sandbox tests (excluding executor-detection/isolated-vm due to native module issues)

      // v3.1.0 Browser compatibility tests
      "test/browser/browser-shims.test.mjs",
      "test/browser/indexeddb-store.test.mjs",

      // v3.1.0 OTEL validation tests
      "test/validation/otel-validation-v3.1.test.mjs",

      // v3.1.0 Performance tests
      "test/performance/performance-regression.test.mjs",

      // v3.1.0 End-to-end tests
      "test/e2e/v3.1-features.test.mjs",

      // v3.2.0 Streaming tests (unrdf 2028)
      "test/streaming/*.test.mjs",

      // v3.2.0 Federation tests (unrdf 2028)
      "test/federation/*.test.mjs",

      // v3.2.0 React Hooks tests - All categories
      "test/react-hooks/dark-matter/*.test.mjs",
      "test/react-hooks/advanced-utility/*.test.mjs",
      "test/react-hooks/policy-security/*.test.mjs",
      "test/react-hooks/error-recovery/*.test.mjs",
      "test/react-hooks/form-ui/*.test.mjs",
      "test/react-hooks/composition/*.test.mjs",
    ],
    exclude: [
      "node_modules/**",
      "dist/**",
      "test/fixtures/**",
      "test/utils/**",
      "test/knowledge-engine/sandbox/executor-detection.test.mjs",
      "test/knowledge-engine/sandbox/isolated-vm.test.mjs",
      "test/browser/browser-compatibility.test.mjs",
      "test/browser/playwright.spec.mjs",
    ],

    // Reporter configuration
    reporter: ["verbose", "json", "html"],
    outputFile: {
      json: "./coverage/test-results.json",
      html: "./coverage/test-results.html",
    },

    // citty-test-utils configuration (CLI testing harness)
    citty: {
      // Path to the CLI entry point
      cliPath: './cli/index.mjs',
      // Optional: working directory
      cwd: process.cwd(),
      // Cleanroom (Docker) disabled by default; can be enabled per-test
      cleanroom: {
        enabled: false,
        image: 'node:20-alpine',
        timeout: 30000,
        env: {
          NODE_ENV: 'test',
        },
      },
    },

    // Global setup
    globalSetup: [],

    // Setup files - automatic cleanup after each test
    setupFiles: ["./test/setup/cleanup-hooks.mjs"],

    // Test file patterns
    globals: false,

    // Isolate test environment
    isolate: true,

    // Pass with no tests
    passWithNoTests: true,

    // Retry failed tests
    retry: 2,

    // Bail on first failure (useful for CI)
    bail: 0,

    // Watch mode configuration
    watch: false,

    // Force Rerun on file change
    forceRerunTriggers: ["**/package.json/**", "**/vitest.config.*/**"],

    // Type checking
    typecheck: {
      enabled: false, // We're using JSDoc, not TypeScript
    },

    // Benchmark configuration
    benchmark: {
      outputFile: "./coverage/benchmark-results.json",
    },

    // UI mode configuration
    ui: false,

    // API mode
    api: false,

    // Inspect mode
    inspect: false,

    // Inspect brk mode
    inspectBrk: false,

    // Log level
    logLevel: "info",

    // Silent mode
    silent: false,

    // Reporter options
    reporterOptions: {
      verbose: {
        showDiff: true,
        showErrorStack: true,
      },
    },

    // Test name pattern
    testNamePattern: undefined,

    // Update snapshots
    update: false,

    // Related files
    related: undefined,

    // Run tests
    run: true,

    // Mode
    mode: "test",

    // Root directory
    root: process.cwd(),

    // Config file
    config: undefined,

    // Dependencies
    deps: {
      // External dependencies that should be treated as external
      external: [],
      // Inline dependencies that should be bundled
      inline: [],
    },

    // Server configuration
    server: {
      // Source map support
      sourcemap: true,
    },

    // Worker configuration
    worker: {
      // Single-threaded worker pool for AI agent compatibility
      pool: "forks",
      poolOptions: {
        forks: {
          singleFork: true,
        },
      },
    },

    // Browser configuration (not used for Node.js tests)
    browser: {
      enabled: false,
    },

    // Experimental features
    experimentalFeatures: {
      // Enable experimental features if needed
    },
  },

  // Resolve configuration
  resolve: {
    alias: {
      // Add any path aliases if needed
    },
  },

  // Define global constants
  define: {
    // Define any global constants
  },

  // Optimize dependencies
  optimizeDeps: {
    // Include dependencies that should be pre-bundled
    include: ["n3", "zod", "@comunica/query-sparql"],
    // Exclude dependencies that should not be pre-bundled
    exclude: [],
  },

  // Build configuration
  build: {
    // Source map support
    sourcemap: true,
    // Minification
    minify: false,
    // Target
    target: "node18",
  },

  // CSS configuration (not used for Node.js tests)
  css: {
    // CSS configuration
  },

  // JSON configuration
  json: {
    // JSON configuration
  },

  // Assets configuration
  assetsInclude: [],

  // Public directory
  publicDir: false,

  // Cache directory
  cacheDir: "node_modules/.vite",

  // Clear screen
  clearScreen: true,

  // Log level
  logLevel: "info",

  // Custom logger
  customLogger: undefined,

  // Environment variables
  envPrefix: ["VITE_", "VITEST_"],

  // Environment variables
  envDir: process.cwd(),

  // Mode
  mode: "test",

  // Command
  command: "test",

  // Is production
  isProduction: false,

  // Is preview
  isPreview: false,

  // Is test
  isTest: true,

  // Is build
  isBuild: false,

  // Is serve
  isServe: false,

  // Is optimize
  isOptimize: false,

  // Is watch
  isWatch: false,

  // Is dev
  isDev: false,
});
