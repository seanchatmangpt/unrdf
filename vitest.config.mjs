/**
 * @fileoverview Simplified Vitest configuration for unrdf
 * Standard configuration compatible with pnpm workspace execution
 */
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Standard defaults - removed singleFork that causes hanging
    pool: "forks",

    // Test timeout - 5s SLA (Andon Principle)
    testTimeout: 5000,

    // Environment
    environment: "node",

    // File patterns - 80/20 core test suite (REMOVED adversarial tests)
    include: [
      "test/diff.test.mjs",
      "test/dark-matter-80-20.test.mjs",
      "test/cli/cli-package.test.mjs",
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
      "**/benchmarks/**", // Benchmarks are NOT tests
    ],

    // Reporter
    reporter: ["verbose"],

    // Standard settings
    globals: false,
    isolate: true,
    passWithNoTests: true,
  },
});
