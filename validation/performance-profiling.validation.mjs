/**
 * @file Performance Profiling Validation Suite
 * @module validation/performance-profiling
 *
 * @description
 * OTEL span-based validation for performance profiling features.
 * Tests profiler initialization, metric collection, and bottleneck detection.
 *
 * v3.1.0 Feature: Add performance profiling tools
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../packages/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Performance profiling validation suite
 */
const performanceSuite = {
  name: "performance-profiling",
  description: "OTEL span-based validation for performance profiling",

  features: [
    {
      name: "profiler-init",
      description: "Performance profiler initialization",
      config: {
        expectedSpans: [
          "profiler.init",
          "profiler.configure",
          "profiler.start",
        ],
        requiredAttributes: [
          "profiler.enabled",
          "profiler.interval",
          "profiler.metrics",
        ],
        performanceThresholds: {
          maxLatency: 300,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 20 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("profiler.init", {
            "profiler.enabled": true,
          }),
          helpers.createSpanStatusRule("profiler.init", "ok"),
          helpers.createPerformanceRule("latency", 300, "<"),
        ],
      },
    },

    {
      name: "metric-collection",
      description: "Performance metric collection validation",
      config: {
        expectedSpans: [
          "metrics.collect",
          "metrics.latency",
          "metrics.throughput",
          "metrics.memory",
        ],
        requiredAttributes: [
          "metrics.timestamp",
          "metrics.value",
          "metrics.unit",
        ],
        performanceThresholds: {
          maxLatency: 200,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 15 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("metrics.collect", {
            "metrics.timestamp": true,
          }),
          helpers.createSpanStatusRule("metrics.collect", "ok"),
          helpers.createValidationRule(
            "metrics-collected",
            (spans) => {
              const metricsSpan = spans.find((s) => s.name === "metrics.collect");
              return metricsSpan && metricsSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "bottleneck-detection",
      description: "Performance bottleneck detection",
      config: {
        expectedSpans: [
          "bottleneck.analyze",
          "bottleneck.detect",
          "bottleneck.report",
        ],
        requiredAttributes: [
          "bottleneck.type",
          "bottleneck.severity",
          "bottleneck.threshold",
        ],
        performanceThresholds: {
          maxLatency: 500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 25 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("bottleneck.analyze", {
            "bottleneck.type": "latency",
          }),
          helpers.createSpanStatusRule("bottleneck.analyze", "ok"),
          helpers.createPerformanceRule("latency", 500, "<"),
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
 * Run performance profiling validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runPerformanceValidation() {
  console.log("🔍 Starting Performance Profiling OTEL Validation...");

  try {
    const report = await runner.runSuite(performanceSuite);

    // Print summary
    console.log("\n📊 Performance Profiling Validation Summary:");
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
    console.error("❌ Performance profiling validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { performanceSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runPerformanceValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
