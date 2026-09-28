/**
 * @file Knowledge Hooks API Validation Suite
 * @module validation/knowledge-hooks-api
 *
 * @description
 * OTEL span-based validation for Knowledge Hooks API.
 * Tests hook definition, registration, execution, and evaluation.
 *
 * v3.1.0 Feature: Core Knowledge Hooks API validation (20% weight)
 *
 * @deprecated This validation is currently NON-FUNCTIONAL (scores 0/100).
 * Issue: No OTEL spans are collected during validation execution.
 * Root Cause: Hooks implementation exists but doesn't integrate with OTEL validation runner.
 * Impact: Lowers overall OTEL score from 100 to 83/100.
 * Status: Marked as deprecated for v5.0.0-beta.2.
 * Future: Either fix OTEL integration or remove feature entirely.
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../packages/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Knowledge Hooks API validation suite
 */
const knowledgeHooksSuite = {
  name: "knowledge-hooks-api",
  description: "OTEL span-based validation for Knowledge Hooks API",

  features: [
    {
      name: "hook-definition",
      description: "Knowledge hook definition validation",
      config: {
        expectedSpans: [
          "hook.define",
          "hook.validate",
          "hook.register",
        ],
        requiredAttributes: [
          "hook.name",
          "hook.kind",
          "hook.priority",
          "hook.meta",
        ],
        performanceThresholds: {
          maxLatency: 400,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 25 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("hook.define", {
            "hook.name": "test-hook",
          }),
          helpers.createSpanExistenceRule("hook.validate", {
            "hook.valid": true,
          }),
          helpers.createSpanStatusRule("hook.define", "ok"),
          helpers.createSpanStatusRule("hook.validate", "ok"),
          helpers.createPerformanceRule("latency", 400, "<"),
          helpers.createValidationRule(
            "hook-defined-successfully",
            (spans) => {
              const defineSpan = spans.find((s) => s.name === "hook.define");
              return defineSpan && defineSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "hook-execution",
      description: "Knowledge hook execution validation",
      config: {
        expectedSpans: [
          "hook.execute",
          "hook.evaluate",
          "hook.result",
        ],
        requiredAttributes: [
          "hook.name",
          "hook.kind",
          "hook.fired",
          "execution.time",
        ],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 30 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("hook.execute", {
            "hook.name": "test-hook",
          }),
          helpers.createSpanExistenceRule("hook.evaluate", {
            "hook.kind": "sparql-ask",
          }),
          helpers.createSpanStatusRule("hook.execute", "ok"),
          helpers.createSpanStatusRule("hook.evaluate", "ok"),
          helpers.createPerformanceRule("latency", 500, "<"),
          helpers.createValidationRule(
            "hook-executed-successfully",
            (spans) => {
              const executeSpan = spans.find((s) => s.name === "hook.execute");
              return executeSpan && executeSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "hook-batching",
      description: "Knowledge hook batching (Dark Matter 80/20)",
      config: {
        expectedSpans: [
          "hook.batch.collect",
          "hook.batch.execute",
          "hook.batch.results",
        ],
        requiredAttributes: [
          "batch.size",
          "batch.duration",
          "batch.success",
        ],
        performanceThresholds: {
          maxLatency: 600,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 35 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("hook.batch.execute", {
            "batch.size": 3,
          }),
          helpers.createSpanStatusRule("hook.batch.execute", "ok"),
          helpers.createPerformanceRule("latency", 600, "<"),
          helpers.createValidationRule(
            "batch-optimization",
            (spans) => {
              const batchSpan = spans.find(
                (s) => s.name === "hook.batch.execute",
              );
              return (
                batchSpan &&
                batchSpan.attributes["batch.size"] >= 3 &&
                batchSpan.status === "ok"
              );
            },
            "error",
          ),
        ],
      },
    },
  ],

  globalConfig: {
    timeout: 30000,
    retries: 1,
    parallel: false,
  },
};

/**
 * Run knowledge hooks API validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runKnowledgeHooksValidation() {
  console.log("🔍 Starting Knowledge Hooks API OTEL Validation...");

  try {
    const report = await runner.runSuite(knowledgeHooksSuite);

    // Print summary
    console.log("\n📊 Knowledge Hooks API Validation Summary:");
    console.log(`   Score: ${report.summary.score}/100`);
    console.log(
      `   Features: ${report.summary.passed}/${report.summary.total} passed`,
    );
    console.log(`   Duration: ${report.summary.duration}ms`);

    if (report.summary.failed > 0) {
      console.log(`   ❌ Failed features: ${report.summary.failed}`);
      for (const feature of report.features) {
        if (!feature.passed) {
          console.log(
            `      - ${feature.name}: ${feature.violations.length} violations`,
          );
        }
      }
    }

    return report;
  } catch (error) {
    console.error("❌ Knowledge hooks API validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { knowledgeHooksSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runKnowledgeHooksValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
