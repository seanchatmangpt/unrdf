/**
 * @file Browser Features Validation Suite
 * @module validation/browser-features
 *
 * @description
 * OTEL span-based validation for browser compatibility layer.
 * Tests browser shims, polyfills, and Worker support.
 *
 * v3.1.0 Feature: Complete browser support
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../packages/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Browser features validation suite
 */
const browserSuite = {
  name: "browser-features",
  description: "OTEL span-based validation for browser compatibility",

  features: [
    {
      name: "browser-shims",
      description: "Browser shim layer validation",
      config: {
        expectedSpans: [
          "browser.shim.init",
          "browser.shim.fs",
          "browser.shim.crypto",
        ],
        requiredAttributes: [
          "browser.shim",
          "browser.platform",
          "browser.version",
        ],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.05, // More lenient for browser
          minThroughput: 1,
          maxMemoryUsage: 50 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("browser.shim.init", {
            "browser.platform": "browser",
          }),
          helpers.createSpanStatusRule("browser.shim.init", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
        ],
      },
    },

    {
      name: "browser-parse",
      description: "Browser RDF parsing validation",
      config: {
        expectedSpans: ["browser.parse", "parse.turtle"],
        requiredAttributes: ["format", "input.size", "output.size"],
        performanceThresholds: {
          maxLatency: 1200,
          maxErrorRate: 0.05,
          minThroughput: 1,
          maxMemoryUsage: 60 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("browser.parse", { format: "turtle" }),
          helpers.createSpanStatusRule("browser.parse", "ok"),
          helpers.createValidationRule(
            "browser-parse-success",
            (spans) => {
              const parseSpan = spans.find((s) => s.name === "browser.parse");
              return parseSpan && parseSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "browser-query",
      description: "Browser SPARQL query validation",
      config: {
        expectedSpans: ["browser.query", "query.sparql"],
        requiredAttributes: ["query.type", "results", "size"],
        performanceThresholds: {
          maxLatency: 1500,
          maxErrorRate: 0.05,
          minThroughput: 1,
          maxMemoryUsage: 60 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("browser.query", {
            "query.type": "SELECT",
          }),
          helpers.createSpanStatusRule("browser.query", "ok"),
          helpers.createPerformanceRule("latency", 1500, "<"),
        ],
      },
    },

    {
      name: "browser-validate",
      description: "Browser SHACL validation",
      config: {
        expectedSpans: ["browser.validate", "validate.shacl"],
        requiredAttributes: ["conforms", "violations"],
        performanceThresholds: {
          maxLatency: 2000,
          maxErrorRate: 0.05,
          minThroughput: 1,
          maxMemoryUsage: 70 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("browser.validate", {
            conforms: true,
          }),
          helpers.createSpanStatusRule("browser.validate", "ok"),
        ],
      },
    },
  ],

  globalConfig: {
    timeout: 40000,
    retries: 2,
    parallel: false,
  },
};

/**
 * Run browser features validation suite
 * @returns {Promise<Object>} Validation report
 */
export default async function runBrowserValidation() {
  console.log("🔍 Starting Browser Features OTEL Validation...");

  try {
    const report = await runner.runSuite(browserSuite);

    // Print summary
    console.log("\n📊 Browser Features Validation Summary:");
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
    console.error("❌ Browser features validation failed:", error.message);
    throw error;
  }
}

// Export suite configuration
export { browserSuite };

// Auto-run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runBrowserValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
