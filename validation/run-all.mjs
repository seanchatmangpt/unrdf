/**
 * @file Run All OTEL Validations
 * @module validation/run-all
 *
 * @description
 * Main entry point for running all OTEL span-based validations.
 * Replaces traditional test runners with comprehensive span analysis.
 */

import { runKnowledgeEngineValidation } from "./knowledge-engine.validation.mjs";
import { runCLIValidation } from "./cli.validation.mjs";
import { createValidationRunner } from "../src/validation/index.mjs";

const runner = createValidationRunner({ verbose: true });

/**
 * Comprehensive validation suite combining all feature validations
 */
const comprehensiveSuite = {
  name: "comprehensive",
  description:
    "Comprehensive OTEL span-based validation for all UNRDF features",

  features: [
    // Knowledge Engine features
    {
      name: "knowledge-engine",
      description: "Core knowledge engine operations",
      config: {
        expectedSpans: [
          "parse.turtle",
          "query.sparql",
          "validate.shacl",
          "reason.n3",
          "canonicalize",
        ],
        requiredAttributes: [
          "service.name",
          "operation.type",
          "input.size",
          "output.size",
        ],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 50 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    // CLI features
    {
      name: "cli-parse",
      description: "CLI parse command validation",
      config: {
        expectedSpans: ["cli.parse", "cli.output", "parse.turtle"],
        requiredAttributes: ["input.file", "output.file", "format", "triples"],
        performanceThresholds: {
          maxLatency: 2000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 100 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    {
      name: "cli-query",
      description: "CLI query command validation",
      config: {
        expectedSpans: ["cli.query", "cli.format", "query.sparql"],
        requiredAttributes: ["query", "format", "results", "size"],
        performanceThresholds: {
          maxLatency: 3000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 100 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    {
      name: "cli-validate",
      description: "CLI validate command validation",
      config: {
        expectedSpans: ["cli.validate", "validate.shacl", "cli.report"],
        requiredAttributes: [
          "input.file",
          "shapes.file",
          "conforms",
          "violations",
        ],
        performanceThresholds: {
          maxLatency: 5000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 150 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    {
      name: "cli-hook",
      description: "CLI hook command validation",
      config: {
        expectedSpans: ["cli.hook", "hook.evaluate", "hook.result"],
        requiredAttributes: [
          "hook.name",
          "hook.kind",
          "hook.fired",
          "execution.time",
        ],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 50 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    {
      name: "transaction-manager",
      description: "Transaction manager validation",
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
        validationRules: [],
      },
    },
  ],

  globalConfig: {
    timeout: 60000, // 1 minute timeout for comprehensive validation
    retries: 2, // More retries for comprehensive validation
    parallel: true, // Run features in parallel for speed
  },
};

/**
 * Run comprehensive validation suite
 * @returns {Promise<Object>} Validation report
 */
export async function runComprehensiveValidation() {
  console.log("🚀 Starting Comprehensive OTEL Validation...");
  console.log(
    "   This replaces traditional unit tests with span-based validation",
  );
  console.log(
    "   Features are validated by analyzing OTEL spans, metrics, and traces\n",
  );

  try {
    const report = await runner.runSuite(comprehensiveSuite);

    // Print comprehensive summary
    console.log("\n🎯 Comprehensive Validation Results:");
    console.log(`   Overall Score: ${report.summary.score}/100`);
    console.log(
      `   Features: ${report.summary.passed}/${report.summary.total} passed`,
    );
    console.log(`   Duration: ${report.summary.duration}ms`);
    console.log(
      `   Status: ${report.summary.failed === 0 ? "✅ PASSED" : "❌ FAILED"}`,
    );

    if (report.summary.failed > 0) {
      console.log("\n❌ Failed Features:");
      for (const feature of report.features) {
        if (!feature.passed) {
          console.log(
            `   - ${feature.name}: ${feature.score}/100 (${feature.violations.length} violations)`,
          );
          if (feature.violations.length > 0) {
            console.log(
              `     Violations: ${feature.violations.slice(0, 2).join(", ")}${feature.violations.length > 2 ? "..." : ""}`,
            );
          }
        }
      }
    }

    // Print performance summary
    console.log("\n📊 Performance Summary:");
    for (const feature of report.features) {
      const metrics = feature.metrics;
      console.log(`   ${feature.name}:`);
      console.log(`     Latency: ${metrics.latency}ms`);
      console.log(`     Error Rate: ${(metrics.errorRate * 100).toFixed(2)}%`);
      console.log(`     Throughput: ${metrics.throughput} ops`);
      console.log(
        `     Memory: ${(metrics.memoryUsage / 1024 / 1024).toFixed(2)}MB`,
      );
    }

    return report;
  } catch (error) {
    console.error("❌ Comprehensive validation failed:", error.message);
    throw error;
  }
}

/**
 * Run individual validation suites
 * @returns {Promise<Object>} Combined validation results
 */
export async function runIndividualSuites() {
  console.log("🔍 Running Individual Validation Suites...\n");

  const results = {};

  try {
    // Run knowledge engine validation
    console.log("1. Knowledge Engine Validation:");
    results.knowledgeEngine = await runKnowledgeEngineValidation();
    console.log(
      `   Result: ${results.knowledgeEngine.summary.failed === 0 ? "✅ PASSED" : "❌ FAILED"}\n`,
    );

    // Run CLI validation
    console.log("2. CLI Validation:");
    results.cli = await runCLIValidation();
    console.log(
      `   Result: ${results.cli.summary.failed === 0 ? "✅ PASSED" : "❌ FAILED"}\n`,
    );

    // Calculate overall results
    const totalFeatures =
      results.knowledgeEngine.summary.total + results.cli.summary.total;
    const totalPassed =
      results.knowledgeEngine.summary.passed + results.cli.summary.passed;
    const totalFailed = totalFeatures - totalPassed;
    const overallScore = Math.round((totalPassed / totalFeatures) * 100);

    console.log("📊 Overall Results:");
    console.log(`   Total Features: ${totalFeatures}`);
    console.log(`   Passed: ${totalPassed}`);
    console.log(`   Failed: ${totalFailed}`);
    console.log(`   Overall Score: ${overallScore}/100`);
    console.log(
      `   Status: ${totalFailed === 0 ? "✅ ALL PASSED" : "❌ SOME FAILED"}`,
    );

    return {
      overall: {
        total: totalFeatures,
        passed: totalPassed,
        failed: totalFailed,
        score: overallScore,
      },
      suites: results,
    };
  } catch (error) {
    console.error("❌ Individual suite validation failed:", error.message);
    throw error;
  }
}

/**
 * Main validation entry point
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Validation results
 */
export async function runAllValidations(options = {}) {
  const { mode = "comprehensive", verbose = true } = options;

  console.log("🎯 UNRDF OTEL Span-Based Validation");
  console.log(
    "   Replacing traditional unit tests with OpenTelemetry span analysis",
  );
  console.log(`   Mode: ${mode}\n`);

  try {
    let results;

    if (mode === "comprehensive") {
      results = await runComprehensiveValidation();
    } else if (mode === "individual") {
      results = await runIndividualSuites();
    } else {
      throw new Error(`Unknown validation mode: ${mode}`);
    }

    // Exit with appropriate code
    const exitCode =
      mode === "comprehensive"
        ? results.summary.failed === 0
          ? 0
          : 1
        : results.overall.failed === 0
          ? 0
          : 1;

    if (exitCode === 0) {
      console.log(
        "\n🎉 All validations passed! Features are working correctly.",
      );
    } else {
      console.log(
        "\n❌ Some validations failed. Please review the results above.",
      );
    }

    return results;
  } catch (error) {
    console.error("❌ Validation execution failed:", error.message);
    throw error;
  }
}

// Auto-run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const mode = process.argv[2] || "comprehensive";

  runAllValidations({ mode })
    .then((results) => {
      const exitCode =
        mode === "comprehensive"
          ? results.summary.failed === 0
            ? 0
            : 1
          : results.overall.failed === 0
            ? 0
            : 1;
      process.exit(exitCode);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
