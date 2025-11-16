/**
 * @file Isolated-VM Security Validation Suite
 * @module validation/isolated-vm-security
 *
 * @description
 * OTEL span-based validation for isolated-vm sandbox security.
 * Tests sandboxed hook execution and security properties.
 *
 * v3.1.0 Feature: Replace vm2 with isolated-vm for better security
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Isolated-VM security validation suite
 */
const isolatedVMSuite = {
  name: "isolated-vm-security",
  description: "OTEL span-based validation for isolated-vm sandbox security",

  features: [
    {
      name: "isolated-vm-sandbox",
      description: "Isolated-VM sandbox execution validation",
      config: {
        expectedSpans: [
          "sandbox.create",
          "sandbox.execute",
          "sandbox.cleanup",
        ],
        requiredAttributes: [
          "sandbox.type",
          "sandbox.memory_limit",
          "sandbox.timeout",
          "sandbox.success",
        ],
        performanceThresholds: {
          maxLatency: 800,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 40 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("sandbox.create", {
            "sandbox.type": "isolated-vm",
          }),
          helpers.createSpanExistenceRule("sandbox.execute", {
            "sandbox.success": true,
          }),
          helpers.createSpanStatusRule("sandbox.create", "ok"),
          helpers.createSpanStatusRule("sandbox.execute", "ok"),
          helpers.createPerformanceRule("latency", 800, "<"),
          helpers.createValidationRule(
            "sandbox-isolation",
            (spans) => {
              const executeSpan = spans.find((s) => s.name === "sandbox.execute");
              return executeSpan && executeSpan.attributes["sandbox.type"] === "isolated-vm";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "sandbox-security-properties",
      description: "Sandbox security properties validation",
      config: {
        expectedSpans: [
          "security.check",
          "security.validate",
          "security.enforce",
        ],
        requiredAttributes: [
          "security.level",
          "security.passed",
          "security.violations",
        ],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 30 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("security.check", {
            "security.level": "high",
          }),
          helpers.createSpanStatusRule("security.check", "ok"),
          helpers.createValidationRule(
            "no-security-violations",
            (spans) => {
              const securitySpan = spans.find((s) => s.name === "security.validate");
              return securitySpan && securitySpan.attributes["security.violations"] === 0;
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
 * Run isolated-VM security validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runIsolatedVMValidation() {
  console.log("🔍 Starting Isolated-VM Security OTEL Validation...");

  try {
    const report = await runner.runSuite(isolatedVMSuite);

    // Print summary
    console.log("\n📊 Isolated-VM Security Validation Summary:");
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
    console.error("❌ Isolated-VM security validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { isolatedVMSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runIsolatedVMValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
