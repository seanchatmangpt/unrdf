/**
 * @fileoverview DX-optimized Vitest configuration for unrdf
 * - Parallel execution with maxConcurrency
 * - Multiple reporters (verbose + junit for CI)
 * - Fast test modes via environment variables
 */
import { defineConfig } from "vitest/config";

const isCI = process.env.CI === "true";
const isFast = process.env.TEST_MODE === "fast";

export default defineConfig({
  test: {
    // Parallel execution - 2-4x speedup
    pool: "forks",
    maxForks: 10,

    // Test timeout - 5s SLA (Andon Principle)
    testTimeout: 5000,

    // Environment
    environment: "node",

    // File patterns - 80/20 core test suite
    include: [
      "test/diff.test.mjs",
      "test/dark-matter-80-20.test.mjs",
      "test/cli/cli-package.test.mjs",
      "test/daemon-cli.test.mjs",
      "test/knowledge-engine/parse-contract.test.mjs",
      "test/knowledge-engine/query-contract.test.mjs",
      "test/knowledge-engine/utils/circuit-breaker.test.mjs",
      "test/browser/browser-shims.test.mjs",
      "test/browser/indexeddb-store.test.mjs",
      "test/validation/otel-validation-v3.1.test.mjs",
      "test/streaming/streaming.test.mjs",
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
      "test/cli/baseline-cli.test.mjs",
      "test/project-engine.test.mjs",
      "test/project-engine/code-complexity-js.test.mjs",
      "test/project-engine/initialize.test.mjs",
      "**/benchmarks/**",
      isFast ? "test/**/*.integration.test.mjs" : null,
    ].filter(Boolean),

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
    passWithNoTests: true,

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
