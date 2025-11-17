/**
 * @file Lockchain Integrity Validation Suite
 * @module validation/lockchain-integrity
 *
 * @description
 * OTEL span-based validation for lockchain cryptographic audit trail.
 * Tests receipt writing, verification, Merkle root validation, and Git anchoring.
 *
 * v3.1.0 Feature: Lockchain cryptographic integrity validation (15% weight)
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Lockchain integrity validation suite
 */
const lockchainSuite = {
  name: "lockchain-integrity",
  description: "OTEL span-based validation for lockchain cryptographic integrity",

  features: [
    {
      name: "lockchain-write",
      description: "Lockchain receipt writing validation",
      config: {
        expectedSpans: [
          "lockchain.write",
          "lockchain.sign",
          "lockchain.hash",
        ],
        requiredAttributes: [
          "lockchain.entry_id",
          "lockchain.signature",
          "lockchain.timestamp",
        ],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 30 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("lockchain.write", {
            "lockchain.entry_id": true,
          }),
          helpers.createSpanExistenceRule("lockchain.sign", {
            "lockchain.signature": true,
          }),
          helpers.createSpanStatusRule("lockchain.write", "ok"),
          helpers.createSpanStatusRule("lockchain.sign", "ok"),
          helpers.createPerformanceRule("latency", 500, "<"),
          helpers.createValidationRule(
            "receipt-written-successfully",
            (spans) => {
              const writeSpan = spans.find((s) => s.name === "lockchain.write");
              return (
                writeSpan &&
                writeSpan.attributes["lockchain.entry_id"] &&
                writeSpan.status === "ok"
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "lockchain-verify",
      description: "Lockchain cryptographic verification validation",
      config: {
        expectedSpans: [
          "lockchain.verify",
          "lockchain.verify.signature",
          "lockchain.verify.merkle",
        ],
        requiredAttributes: [
          "lockchain.entry_id",
          "lockchain.valid",
          "lockchain.merkle_valid",
        ],
        performanceThresholds: {
          maxLatency: 600,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 35 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("lockchain.verify", {
            "lockchain.valid": true,
          }),
          helpers.createSpanExistenceRule("lockchain.verify.merkle", {
            "lockchain.merkle_valid": true,
          }),
          helpers.createSpanStatusRule("lockchain.verify", "ok"),
          helpers.createSpanStatusRule("lockchain.verify.merkle", "ok"),
          helpers.createPerformanceRule("latency", 600, "<"),
          helpers.createValidationRule(
            "cryptographic-verification-passed",
            (spans) => {
              const verifySpan = spans.find((s) => s.name === "lockchain.verify");
              const merkleSpan = spans.find(
                (s) => s.name === "lockchain.verify.merkle",
              );
              return (
                verifySpan &&
                verifySpan.attributes["lockchain.valid"] === true &&
                merkleSpan &&
                merkleSpan.attributes["lockchain.merkle_valid"] === true
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "lockchain-commit",
      description: "Lockchain Git anchoring validation",
      config: {
        expectedSpans: [
          "lockchain.commit",
          "lockchain.git.add",
          "lockchain.git.commit",
        ],
        requiredAttributes: [
          "lockchain.batch_id",
          "lockchain.commit_hash",
          "lockchain.entry_count",
        ],
        performanceThresholds: {
          maxLatency: 800,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 40 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("lockchain.commit", {
            "lockchain.batch_id": true,
          }),
          helpers.createSpanExistenceRule("lockchain.git.commit", {
            "lockchain.commit_hash": true,
          }),
          helpers.createSpanStatusRule("lockchain.commit", "ok"),
          helpers.createSpanStatusRule("lockchain.git.commit", "ok"),
          helpers.createPerformanceRule("latency", 800, "<"),
          helpers.createValidationRule(
            "git-anchoring-successful",
            (spans) => {
              const commitSpan = spans.find((s) => s.name === "lockchain.commit");
              const gitSpan = spans.find(
                (s) => s.name === "lockchain.git.commit",
              );
              return (
                commitSpan &&
                gitSpan &&
                gitSpan.attributes["lockchain.commit_hash"] &&
                gitSpan.status === "ok"
              );
            },
            "error",
          ),
        ],
      },
    },
  ],

  globalConfig: {
    timeout: 35000,
    retries: 1,
    parallel: false,
  },
};

/**
 * Run lockchain integrity validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runLockchainValidation() {
  console.log("🔍 Starting Lockchain Integrity OTEL Validation...");

  try {
    const report = await runner.runSuite(lockchainSuite);

    // Print summary
    console.log("\n📊 Lockchain Integrity Validation Summary:");
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
    console.error("❌ Lockchain integrity validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { lockchainSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runLockchainValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
