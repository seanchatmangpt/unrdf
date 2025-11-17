/**
 * @file Policy Packs Validation Suite
 * @module validation/policy-packs
 *
 * @description
 * OTEL span-based validation for policy pack system.
 * Tests policy pack loading, activation, and hook management.
 *
 * v3.1.0 Feature: Policy pack system validation (15% weight)
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Policy packs validation suite
 */
const policyPacksSuite = {
  name: "policy-packs",
  description: "OTEL span-based validation for policy pack system",

  features: [
    {
      name: "policy-load",
      description: "Policy pack loading validation",
      config: {
        expectedSpans: [
          "policy.load",
          "policy.parse",
          "policy.validate",
        ],
        requiredAttributes: [
          "policy.name",
          "policy.version",
          "policy.hooks_count",
          "policy.loaded",
        ],
        performanceThresholds: {
          maxLatency: 700,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 35 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("policy.load", {
            "policy.name": "test-policy",
          }),
          helpers.createSpanExistenceRule("policy.validate", {
            "policy.valid": true,
          }),
          helpers.createSpanStatusRule("policy.load", "ok"),
          helpers.createSpanStatusRule("policy.validate", "ok"),
          helpers.createPerformanceRule("latency", 700, "<"),
          helpers.createValidationRule(
            "policy-loaded-successfully",
            (spans) => {
              const loadSpan = spans.find((s) => s.name === "policy.load");
              return (
                loadSpan &&
                loadSpan.attributes["policy.loaded"] === true &&
                loadSpan.status === "ok"
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "policy-activate",
      description: "Policy pack activation validation",
      config: {
        expectedSpans: [
          "policy.activate",
          "policy.hooks.register",
          "policy.ready",
        ],
        requiredAttributes: [
          "policy.name",
          "policy.active",
          "policy.hooks_registered",
        ],
        performanceThresholds: {
          maxLatency: 600,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 30 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("policy.activate", {
            "policy.active": true,
          }),
          helpers.createSpanStatusRule("policy.activate", "ok"),
          helpers.createPerformanceRule("latency", 600, "<"),
          helpers.createValidationRule(
            "policy-activated-successfully",
            (spans) => {
              const activateSpan = spans.find(
                (s) => s.name === "policy.activate",
              );
              return (
                activateSpan &&
                activateSpan.attributes["policy.active"] === true &&
                activateSpan.status === "ok"
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "policy-hooks",
      description: "Policy pack hook integration validation",
      config: {
        expectedSpans: [
          "policy.hook.execute",
          "policy.hook.evaluate",
          "policy.hook.result",
        ],
        requiredAttributes: [
          "hook.policy",
          "hook.name",
          "hook.fired",
        ],
        performanceThresholds: {
          maxLatency: 800,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 40 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("policy.hook.execute", {
            "hook.policy": "test-policy",
          }),
          helpers.createSpanStatusRule("policy.hook.execute", "ok"),
          helpers.createValidationRule(
            "policy-hook-executed",
            (spans) => {
              const hookSpan = spans.find(
                (s) => s.name === "policy.hook.execute",
              );
              return hookSpan && hookSpan.status === "ok";
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
 * Run policy packs validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runPolicyPacksValidation() {
  console.log("🔍 Starting Policy Packs OTEL Validation...");

  try {
    const report = await runner.runSuite(policyPacksSuite);

    // Print summary
    console.log("\n📊 Policy Packs Validation Summary:");
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
    console.error("❌ Policy packs validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { policyPacksSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runPolicyPacksValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
