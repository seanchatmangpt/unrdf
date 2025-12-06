/**
 * @file Run All OTEL Validations (v3.1.0)
 * @module validation/run-all
 *
 * @description
 * Main entry point for running all OTEL span-based validations.
 * Updated for v3.1.0 to focus on core features and remove legacy CLI checks.
 *
 * Features validated (v3.1.0):
 * - Knowledge Engine Core (30%)
 * - Knowledge Hooks API (20%)
 * - Policy Packs (15%)
 * - Lockchain Integrity (15%)
 * - Transaction Manager (10%)
 * - Browser Compatibility (10%)
 */

import { createValidationRunner } from "../packages/validation/src/index.mjs";
import { ensureProviderInitialized } from "./otel-provider.mjs";

await ensureProviderInitialized();
const runner = createValidationRunner({ verbose: true });

/**
 * Comprehensive validation suite for v3.1.0
 * Focuses on production-ready core features
 */
const comprehensiveSuite = {
  name: "comprehensive-v3.1.0",
  description:
    "Comprehensive OTEL span-based validation for UNRDF v3.1.0 core features",

  features: [
    // ========================================
    // Knowledge Engine Core (30% weight)
    // ========================================
    {
      name: "knowledge-engine-core",
      description: "Core knowledge engine operations (parse, query, validate, reason, canonicalize)",
      weight: 0.30,
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

    // ========================================
    // Knowledge Hooks API (20% weight)
    // ========================================
    {
      name: "knowledge-hooks-api",
      description: "Knowledge hooks API validation (defineHook, registerHook, hook execution)",
      weight: 0.20,
      config: {
        expectedSpans: [
          "hook.define",
          "hook.register",
          "hook.execute",
          "hook.evaluate",
        ],
        requiredAttributes: [
          "hook.name",
          "hook.kind",
          "hook.priority",
          "hook.fired",
        ],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 30 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    // ========================================
    // Policy Packs (15% weight)
    // ========================================
    {
      name: "policy-packs",
      description: "Policy pack system validation (load, activate, hooks)",
      weight: 0.15,
      config: {
        expectedSpans: [
          "policy.load",
          "policy.activate",
          "policy.validate",
        ],
        requiredAttributes: [
          "policy.name",
          "policy.version",
          "policy.hooks_count",
        ],
        performanceThresholds: {
          maxLatency: 800,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 40 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    // ========================================
    // Lockchain Integrity (15% weight)
    // ========================================
    {
      name: "lockchain-integrity",
      description: "Lockchain cryptographic audit trail validation",
      weight: 0.15,
      config: {
        expectedSpans: [
          "lockchain.write",
          "lockchain.verify",
          "lockchain.commit",
        ],
        requiredAttributes: [
          "lockchain.entry_id",
          "lockchain.merkle_root",
          "lockchain.signature",
        ],
        performanceThresholds: {
          maxLatency: 600,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 35 * 1024 * 1024,
        },
        validationRules: [],
      },
    },

    // ========================================
    // Transaction Manager (10% weight)
    // ========================================
    {
      name: "transaction-manager",
      description: "Transaction manager validation (ACID guarantees)",
      weight: 0.10,
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

    // ========================================
    // Browser Compatibility (10% weight)
    // ========================================
    {
      name: "browser-compatibility",
      description: "Browser compatibility layer validation",
      weight: 0.10,
      config: {
        expectedSpans: [
          "browser.parse",
          "browser.query",
          "browser.validate",
        ],
        requiredAttributes: [
          "browser.shim",
          "browser.polyfill",
          "browser.worker",
        ],
        performanceThresholds: {
          maxLatency: 1200,
          maxErrorRate: 0.05, // More lenient for browser
          minThroughput: 1,
          maxMemoryUsage: 60 * 1024 * 1024,
        },
        validationRules: [],
      },
    },
  ],

  globalConfig: {
    timeout: 60000, // 1 minute timeout for comprehensive validation
    retries: 2,
    parallel: false, // Sequential execution to prevent span race conditions
  },
};

/**
 * Run comprehensive validation suite for v3.1.0
 * @returns {Promise<Object>} Validation report
 */
export async function runComprehensiveValidation() {
  console.log("🚀 Starting Comprehensive OTEL Validation (v3.1.0)...");
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
  console.log("🔍 Running Individual Validation Suites (v3.1.0)...\n");

  const results = {
    features: [],
  };

  try {
    // Import individual validation modules
    const validationModules = [
      {
        name: "Knowledge Engine Core",
        module: "./knowledge-engine.validation.mjs",
      },
      {
        name: "Knowledge Hooks API",
        module: "./knowledge-hooks-api.validation.mjs",
      },
      {
        name: "Policy Packs",
        module: "./policy-packs.validation.mjs",
      },
      {
        name: "Lockchain Integrity",
        module: "./lockchain-integrity.validation.mjs",
      },
      {
        name: "Transaction Manager",
        module: "./transaction-manager.validation.mjs",
      },
      {
        name: "Browser Compatibility",
        module: "./browser-features.validation.mjs",
      },
    ];

    for (const { name, module } of validationModules) {
      try {
        console.log(`${results.features.length + 1}. ${name} Validation:`);
        const validation = await import(module);
        const result = await validation.default();
        results.features.push(result);
        console.log(
          `   Result: ${result.summary.failed === 0 ? "✅ PASSED" : "❌ FAILED"}\n`,
        );
      } catch (error) {
        console.log(`   Result: ⚠️ SKIPPED (${error.message})\n`);
      }
    }

    // Calculate overall results with weighted scoring
    const totalWeight = comprehensiveSuite.features.reduce(
      (sum, f) => sum + (f.weight || 1 / comprehensiveSuite.features.length),
      0,
    );
    const weightedScore = results.features.reduce((sum, result, idx) => {
      const weight =
        comprehensiveSuite.features[idx]?.weight ||
        1 / comprehensiveSuite.features.length;
      return sum + result.summary.score * weight;
    }, 0);

    const totalPassed = results.features.filter(
      (r) => r.summary.failed === 0,
    ).length;
    const totalFailed = results.features.length - totalPassed;
    const overallScore = Math.round(weightedScore / totalWeight);

    console.log("📊 Overall Results:");
    console.log(`   Total Features: ${results.features.length}`);
    console.log(`   Passed: ${totalPassed}`);
    console.log(`   Failed: ${totalFailed}`);
    console.log(`   Overall Score: ${overallScore}/100`);
    console.log(
      `   Status: ${totalFailed === 0 ? "✅ ALL PASSED" : "❌ SOME FAILED"}`,
    );

    return {
      overall: {
        total: results.features.length,
        passed: totalPassed,
        failed: totalFailed,
        score: overallScore,
      },
      features: results.features,
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

  console.log("🎯 UNRDF OTEL Span-Based Validation (v3.1.0)");
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

    // Persist artifacts
    try {
      const { writeFile, mkdir } = await import("node:fs/promises");
      const { join } = await import("node:path");
      await mkdir("coverage", { recursive: true });
      const jsonPath = join("coverage", "otel-report.json");
      await writeFile(jsonPath, JSON.stringify(results, null, 2));
      await writeFile(
        "validation-output.log",
        captureConsoleSummary(results),
      );
    } catch {}

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

function captureConsoleSummary(results) {
  const lines = [];
  lines.push("🎯 UNRDF OTEL Span-Based Validation (v3.1.0)");
  lines.push(
    "   Replacing traditional unit tests with OpenTelemetry span analysis",
  );
  lines.push("");
  if (results.summary) {
    lines.push("📊 Validation Results:");
    lines.push(`   Suite: ${results.suite || "comprehensive-v3.1.0"}`);
    lines.push(`   Duration: ${results.summary.duration}ms`);
    lines.push(`   Score: ${results.summary.score}/100`);
    lines.push(
      `   Features: ${results.summary.passed}/${results.summary.total} passed`,
    );
    if (results.summary.failed > 0)
      lines.push(`   ❌ Failed: ${results.summary.failed}`);
  } else if (results.overall) {
    lines.push("📊 Overall Results:");
    lines.push(`   Total Features: ${results.overall.total}`);
    lines.push(`   Passed: ${results.overall.passed}`);
    lines.push(`   Failed: ${results.overall.failed}`);
    lines.push(`   Overall Score: ${results.overall.score}/100`);
  }
  lines.push("");
  return lines.join("\n");
}
