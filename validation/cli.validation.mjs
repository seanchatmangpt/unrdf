/**
 * @file CLI OTEL Validation Suite
 * @module validation/cli
 *
 * @description
 * OTEL span-based validation for CLI commands.
 * Replaces traditional CLI unit tests with span analysis and metric validation.
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * CLI validation suite
 * Validates CLI command functionality using OTEL spans
 */
const cliSuite = {
  name: "cli-commands",
  description: "OTEL span-based validation for CLI command functionality",

  features: [
    {
      name: "cli-parse",
      description: "CLI parse command validation",
      config: {
        expectedSpans: ["cli.parse", "cli.output", "parse.turtle"],
        requiredAttributes: ["input.file", "output.file", "format", "triples"],
        performanceThresholds: {
          maxLatency: 2000, // 2 seconds for CLI operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 100 * 1024 * 1024, // 100MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("cli.parse", { format: "turtle" }),
          helpers.createSpanExistenceRule("cli.output", { triples: 100 }),
          helpers.createSpanStatusRule("cli.parse", "ok"),
          helpers.createSpanStatusRule("cli.output", "ok"),
          helpers.createPerformanceRule("latency", 2000, "<"),
          helpers.createValidationRule(
            "parse-success",
            (spans) => {
              const parseSpan = spans.find((s) => s.name === "cli.parse");
              return parseSpan && parseSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "output-generated",
            (spans) => {
              const outputSpan = spans.find((s) => s.name === "cli.output");
              return outputSpan && outputSpan.attributes.triples > 0;
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "cli-query",
      description: "CLI query command validation",
      config: {
        expectedSpans: ["cli.query", "cli.format", "query.sparql"],
        requiredAttributes: ["query", "format", "results", "size"],
        performanceThresholds: {
          maxLatency: 3000, // 3 seconds for query operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 100 * 1024 * 1024, // 100MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("cli.query", {
            query: "SELECT * WHERE { ?s ?p ?o }",
          }),
          helpers.createSpanExistenceRule("cli.format", { format: "json" }),
          helpers.createSpanStatusRule("cli.query", "ok"),
          helpers.createSpanStatusRule("cli.format", "ok"),
          helpers.createPerformanceRule("latency", 3000, "<"),
          helpers.createValidationRule(
            "query-execution",
            (spans) => {
              const querySpan = spans.find((s) => s.name === "cli.query");
              return querySpan && querySpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "results-formatted",
            (spans) => {
              const formatSpan = spans.find((s) => s.name === "cli.format");
              return formatSpan && formatSpan.attributes.size > 0;
            },
            "error",
          ),
        ],
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
          maxLatency: 5000, // 5 seconds for validation
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 150 * 1024 * 1024, // 150MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("cli.validate", {
            "input.file": "test.ttl",
          }),
          helpers.createSpanExistenceRule("validate.shacl", { conforms: true }),
          helpers.createSpanStatusRule("cli.validate", "ok"),
          helpers.createSpanStatusRule("validate.shacl", "ok"),
          helpers.createPerformanceRule("latency", 5000, "<"),
          helpers.createValidationRule(
            "validation-success",
            (spans) => {
              const validateSpan = spans.find((s) => s.name === "cli.validate");
              return validateSpan && validateSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "shacl-conforms",
            (spans) => {
              const shaclSpan = spans.find((s) => s.name === "validate.shacl");
              return shaclSpan && shaclSpan.attributes.conforms === true;
            },
            "error",
          ),
        ],
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
          maxLatency: 1000, // 1 second for hook evaluation
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 50 * 1024 * 1024, // 50MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("cli.hook", {
            "hook.name": "test-hook",
          }),
          helpers.createSpanExistenceRule("hook.evaluate", {
            "hook.kind": "sparql-ask",
          }),
          helpers.createSpanStatusRule("cli.hook", "ok"),
          helpers.createSpanStatusRule("hook.evaluate", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "hook-execution",
            (spans) => {
              const hookSpan = spans.find((s) => s.name === "cli.hook");
              return hookSpan && hookSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "hook-evaluation",
            (spans) => {
              const evaluateSpan = spans.find(
                (s) => s.name === "hook.evaluate",
              );
              return evaluateSpan && evaluateSpan.status === "ok";
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
 * Run CLI validation suite
 * @returns {Promise<Object>} Validation report
 */
export async function runCLIValidation() {
  console.log("🔍 Starting CLI OTEL Validation...");

  try {
    const report = await runner.runSuite(cliSuite);

    // Print summary
    console.log("\n📊 CLI Validation Summary:");
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
    console.error("❌ CLI validation failed:", error.message);
    throw error;
  }
}

/**
 * Validate specific CLI feature
 * @param {string} featureName - Feature name to validate
 * @returns {Promise<Object>} Feature validation result
 */
export async function validateCLIFeature(featureName) {
  const feature = cliSuite.features.find((f) => f.name === featureName);

  if (!feature) {
    throw new Error(`Feature '${featureName}' not found in CLI suite`);
  }

  console.log(`🔍 Validating CLI feature: ${featureName}`);

  try {
    const result = await runner.validator.validateFeature(
      featureName,
      feature.config,
    );

    console.log(`   Score: ${result.score}/100`);
    console.log(`   Passed: ${result.passed ? "✅" : "❌"}`);
    console.log(`   Violations: ${result.violations.length}`);

    if (result.violations.length > 0) {
      console.log("   Violations:");
      for (const violation of result.violations) {
        console.log(`      - ${violation}`);
      }
    }

    return result;
  } catch (error) {
    console.error(`❌ Feature validation failed: ${error.message}`);
    throw error;
  }
}

// Export suite configuration for external use
export { cliSuite };

// Auto-run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runCLIValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
