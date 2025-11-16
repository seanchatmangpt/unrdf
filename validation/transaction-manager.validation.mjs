/**
 * @file Transaction Manager Validation Suite
 * @module validation/transaction-manager
 *
 * @description
 * OTEL span-based validation for transaction manager.
 * Tests ACID guarantees, rollback, and transaction lifecycle.
 *
 * v3.1.0 Feature: Transaction manager validation (10% weight)
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Transaction manager validation suite
 */
const transactionSuite = {
  name: "transaction-manager",
  description: "OTEL span-based validation for transaction manager",

  features: [
    {
      name: "transaction-lifecycle",
      description: "Transaction start-commit-rollback lifecycle validation",
      config: {
        expectedSpans: [
          "transaction.start",
          "transaction.commit",
          "transaction.rollback",
        ],
        requiredAttributes: [
          "transaction.id",
          "transaction.type",
          "transaction.success",
        ],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 25 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("transaction.start", {
            "transaction.type": "rdf",
          }),
          helpers.createSpanStatusRule("transaction.start", "ok"),
          helpers.createPerformanceRule("latency", 500, "<"),
          helpers.createValidationRule(
            "transaction-lifecycle",
            (spans) => {
              const startSpan = spans.find(
                (s) => s.name === "transaction.start",
              );
              const commitSpan = spans.find(
                (s) => s.name === "transaction.commit",
              );
              return (
                startSpan &&
                commitSpan &&
                startSpan.status === "ok" &&
                commitSpan.status === "ok"
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "transaction-acid",
      description: "Transaction ACID guarantees validation",
      config: {
        expectedSpans: [
          "transaction.atomic",
          "transaction.consistent",
          "transaction.isolated",
          "transaction.durable",
        ],
        requiredAttributes: [
          "transaction.id",
          "transaction.acid_passed",
        ],
        performanceThresholds: {
          maxLatency: 600,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 30 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("transaction.atomic", {
            "transaction.acid_passed": true,
          }),
          helpers.createSpanStatusRule("transaction.atomic", "ok"),
          helpers.createValidationRule(
            "acid-guarantees",
            (spans) => {
              const acidSpans = spans.filter((s) =>
                s.name.startsWith("transaction."),
              );
              return acidSpans.every((s) => s.status === "ok");
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "transaction-rollback",
      description: "Transaction rollback validation",
      config: {
        expectedSpans: [
          "transaction.start",
          "transaction.error",
          "transaction.rollback",
        ],
        requiredAttributes: [
          "transaction.id",
          "transaction.rollback_success",
        ],
        performanceThresholds: {
          maxLatency: 400,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 20 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("transaction.rollback", {
            "transaction.rollback_success": true,
          }),
          helpers.createSpanStatusRule("transaction.rollback", "ok"),
          helpers.createPerformanceRule("latency", 400, "<"),
          helpers.createValidationRule(
            "rollback-successful",
            (spans) => {
              const rollbackSpan = spans.find(
                (s) => s.name === "transaction.rollback",
              );
              return (
                rollbackSpan &&
                rollbackSpan.attributes["transaction.rollback_success"] === true
              );
            },
            "error",
          ),
        ],
      },
    },
  ],

  globalConfig: {
    timeout: 25000,
    retries: 1,
    parallel: false,
  },
};

/**
 * Run transaction manager validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runTransactionManagerValidation() {
  console.log("🔍 Starting Transaction Manager OTEL Validation...");

  try {
    const report = await runner.runSuite(transactionSuite);

    // Print summary
    console.log("\n📊 Transaction Manager Validation Summary:");
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
    console.error("❌ Transaction manager validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { transactionSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runTransactionManagerValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
