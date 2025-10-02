/**
 * @file Knowledge Engine OTEL Validation Suite
 * @module validation/knowledge-engine
 *
 * @description
 * OTEL span-based validation for knowledge engine features.
 * Replaces traditional unit tests with span analysis and metric validation.
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Knowledge Engine validation suite
 * Validates core knowledge engine functionality using OTEL spans
 */
const knowledgeEngineSuite = {
  name: "knowledge-engine",
  description:
    "OTEL span-based validation for knowledge engine core functionality",

  features: [
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
          maxLatency: 1000, // 1 second
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 50 * 1024 * 1024, // 50MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("parse.turtle", { format: "turtle" }),
          helpers.createSpanExistenceRule("query.sparql", {
            "query.type": "select",
          }),
          helpers.createSpanExistenceRule("validate.shacl", { conforms: true }),
          helpers.createSpanStatusRule("parse.turtle", "ok"),
          helpers.createSpanStatusRule("query.sparql", "ok"),
          helpers.createSpanStatusRule("validate.shacl", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createPerformanceRule("errorRate", 0.01, "<"),
          helpers.createValidationRule(
            "no-error-spans",
            (spans) => spans.every((s) => s.status === "ok"),
            "error",
          ),
          helpers.createValidationRule(
            "required-spans-present",
            (spans) => {
              const spanNames = spans.map((s) => s.name);
              return ["parse.turtle", "query.sparql", "validate.shacl"].every(
                (name) => spanNames.includes(name),
              );
            },
            "error",
          ),
        ],
      },
    },

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
          maxLatency: 500, // 500ms for transactions
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
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
  ],

  globalConfig: {
    timeout: 30000,
    retries: 1,
    parallel: false,
  },
};

/**
 * Run knowledge engine validation suite
 * @returns {Promise<Object>} Validation report
 */
export async function runKnowledgeEngineValidation() {
  console.log("🔍 Starting Knowledge Engine OTEL Validation...");

  try {
    const report = await runner.runSuite(knowledgeEngineSuite);

    // Print summary
    console.log("\n📊 Knowledge Engine Validation Summary:");
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
    console.error("❌ Knowledge Engine validation failed:", error.message);
    throw error;
  }
}

/**
 * Validate specific knowledge engine feature
 * @param {string} featureName - Feature name to validate
 * @returns {Promise<Object>} Feature validation result
 */
export async function validateKnowledgeEngineFeature(featureName) {
  const feature = knowledgeEngineSuite.features.find(
    (f) => f.name === featureName,
  );

  if (!feature) {
    throw new Error(
      `Feature '${featureName}' not found in knowledge engine suite`,
    );
  }

  console.log(`🔍 Validating knowledge engine feature: ${featureName}`);

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
export { knowledgeEngineSuite };

// Auto-run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runKnowledgeEngineValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
