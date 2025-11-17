/**
 * @file Knowledge Engine OTEL Validation Suite (v3.1.0)
 * @module validation/knowledge-engine
 *
 * @description
 * OTEL span-based validation for knowledge engine core features.
 * Updated for v3.1.0 to remove CLI checks and focus on core RDF operations.
 *
 * v3.1.0 Changes:
 * - Removed CLI-specific validations (moved to CLI package)
 * - Enhanced core engine validations
 * - Added comprehensive RDF operation coverage
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Knowledge Engine validation suite (v3.1.0)
 * Validates core knowledge engine functionality using OTEL spans
 */
const knowledgeEngineSuite = {
  name: "knowledge-engine-core",
  description:
    "OTEL span-based validation for knowledge engine core functionality (v3.1.0)",

  features: [
    {
      name: "rdf-parsing",
      description: "RDF parsing operations (Turtle, N-Quads, JSON-LD)",
      config: {
        expectedSpans: [
          "parse.turtle",
          "parse.nquads",
          "parse.jsonld",
        ],
        requiredAttributes: [
          "service.name",
          "operation.type",
          "input.size",
          "output.size",
          "parse.format",
        ],
        performanceThresholds: {
          maxLatency: 800,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 45 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("parse.turtle", { format: "turtle" }),
          helpers.createSpanStatusRule("parse.turtle", "ok"),
          helpers.createPerformanceRule("latency", 800, "<"),
          helpers.createValidationRule(
            "parse-success",
            (spans) => {
              const parseSpan = spans.find((s) => s.name === "parse.turtle");
              return parseSpan && parseSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "sparql-query",
      description: "SPARQL query execution (SELECT, ASK, CONSTRUCT, UPDATE)",
      config: {
        expectedSpans: [
          "query.sparql",
          "query.optimize",
          "query.execute",
        ],
        requiredAttributes: [
          "query.type",
          "query.length",
          "query.result_count",
        ],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 50 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("query.sparql", {
            "query.type": "SELECT",
          }),
          helpers.createSpanStatusRule("query.sparql", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "query-execution",
            (spans) => {
              const querySpan = spans.find((s) => s.name === "query.sparql");
              return querySpan && querySpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "shacl-validation",
      description: "SHACL shape validation",
      config: {
        expectedSpans: [
          "validate.shacl",
          "validate.shapes",
          "validate.report",
        ],
        requiredAttributes: [
          "validate.conforms",
          "validate.total_results",
          "validate.error_count",
        ],
        performanceThresholds: {
          maxLatency: 1200,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 55 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("validate.shacl", { conforms: true }),
          helpers.createSpanStatusRule("validate.shacl", "ok"),
          helpers.createPerformanceRule("latency", 1200, "<"),
          helpers.createValidationRule(
            "validation-success",
            (spans) => {
              const validateSpan = spans.find((s) => s.name === "validate.shacl");
              return validateSpan && validateSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "n3-reasoning",
      description: "N3 rule-based reasoning",
      config: {
        expectedSpans: [
          "reason.n3",
          "reason.rules",
          "reason.infer",
        ],
        requiredAttributes: [
          "reason.rules_count",
          "reason.input_size",
          "reason.inferred_count",
        ],
        performanceThresholds: {
          maxLatency: 1500,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 60 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("reason.n3", {
            "reason.rules_count": 0,
          }),
          helpers.createSpanStatusRule("reason.n3", "ok"),
          helpers.createPerformanceRule("latency", 1500, "<"),
        ],
      },
    },

    {
      name: "rdf-canonicalization",
      description: "RDF canonicalization (RDFC-1.0)",
      config: {
        expectedSpans: [
          "canonicalize",
          "canonicalize.hash",
          "canonicalize.normalize",
        ],
        requiredAttributes: [
          "canonicalize.algorithm",
          "canonicalize.input_size",
          "canonicalize.hash",
        ],
        performanceThresholds: {
          maxLatency: 1000,
          maxErrorRate: 0.01,
          minThroughput: 1,
          maxMemoryUsage: 50 * 1024 * 1024,
        },
        validationRules: [
          helpers.createSpanExistenceRule("canonicalize", {
            "canonicalize.algorithm": "RDFC-1.0",
          }),
          helpers.createSpanStatusRule("canonicalize", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
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
 * Run knowledge engine validation suite (v3.1.0)
 * @returns {Promise<Object>} Validation report
 */
export default async function runKnowledgeEngineValidation() {
  console.log("🔍 Starting Knowledge Engine Core OTEL Validation (v3.1.0)...");

  try {
    const report = await runner.runSuite(knowledgeEngineSuite);

    // Print summary
    console.log("\n📊 Knowledge Engine Core Validation Summary:");
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
    console.error("❌ Knowledge Engine Core validation failed:", error.message);
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
