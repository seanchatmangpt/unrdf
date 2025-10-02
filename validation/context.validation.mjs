/**
 * @file Context Commands OTEL Validation Suite
 * @module validation/context
 *
 * @description
 * OTEL span-based validation for context management commands.
 * Replaces traditional unit tests with span analysis and metric validation.
 */

import {
  createValidationRunner,
  createValidationHelpers,
} from "../src/validation/index.mjs";

const helpers = createValidationHelpers();
const runner = createValidationRunner({ verbose: true });

/**
 * Context commands validation suite
 * Validates context management functionality using OTEL spans
 */
const contextSuite = {
  name: "context-commands",
  description: "OTEL span-based validation for context management commands",

  features: [
    {
      name: "context-manager",
      description: "Context manager core operations",
      config: {
        feature: "context-manager",
        expectedSpans: [
          "context.manager.init",
          "context.manager.load",
          "context.manager.save",
          "context.manager.create",
          "context.manager.get",
          "context.manager.list",
          "context.manager.use",
          "context.manager.delete",
        ],
        requiredAttributes: [
          "context.dir",
          "context.file",
          "context.name",
          "context.count",
        ],
        performanceThresholds: {
          maxLatency: 1000, // 1 second for context operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.manager.init", {
            "context.dir": "~/.unrdf",
          }),
          helpers.createSpanExistenceRule("context.manager.load", {
            "context.file": "~/.unrdf/contexts.json",
          }),
          helpers.createSpanStatusRule("context.manager.init", "ok"),
          helpers.createSpanStatusRule("context.manager.load", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "context-manager-initialized",
            (spans) => {
              const initSpan = spans.find(
                (s) => s.name === "context.manager.init",
              );
              return initSpan && initSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "context-file-loaded",
            (spans) => {
              const loadSpan = spans.find(
                (s) => s.name === "context.manager.load",
              );
              return loadSpan && loadSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "context-create",
      description: "Context create command validation",
      config: {
        feature: "context-create",
        expectedSpans: [
          "context.create",
          "context.manager.init",
          "context.manager.create",
          "context.manager.save",
        ],
        requiredAttributes: [
          "context.name",
          "context.sidecar.endpoint",
          "context.created",
        ],
        performanceThresholds: {
          maxLatency: 2000, // 2 seconds for create operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.create", {
            "context.name": "test-context",
          }),
          helpers.createSpanExistenceRule("context.manager.create", {
            "context.sidecar.endpoint": "http://localhost:50051",
          }),
          helpers.createSpanStatusRule("context.create", "ok"),
          helpers.createSpanStatusRule("context.manager.create", "ok"),
          helpers.createPerformanceRule("latency", 2000, "<"),
          helpers.createValidationRule(
            "context-creation-success",
            (spans) => {
              const createSpan = spans.find((s) => s.name === "context.create");
              return createSpan && createSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "context-manager-create-success",
            (spans) => {
              const managerCreateSpan = spans.find(
                (s) => s.name === "context.manager.create",
              );
              return managerCreateSpan && managerCreateSpan.status === "ok";
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "context-get",
      description: "Context get command validation",
      config: {
        feature: "context-get",
        expectedSpans: [
          "context.get",
          "context.manager.init",
          "context.manager.get",
        ],
        requiredAttributes: ["context.name", "context.found", "output.format"],
        performanceThresholds: {
          maxLatency: 1000, // 1 second for get operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.get", {
            "context.name": "test-context",
          }),
          helpers.createSpanExistenceRule("context.manager.get", {
            "context.name": "test-context",
          }),
          helpers.createSpanStatusRule("context.get", "ok"),
          helpers.createSpanStatusRule("context.manager.get", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "context-get-success",
            (spans) => {
              const getSpan = spans.find((s) => s.name === "context.get");
              return getSpan && getSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "context-found",
            (spans) => {
              const managerGetSpan = spans.find(
                (s) => s.name === "context.manager.get",
              );
              return (
                managerGetSpan &&
                managerGetSpan.attributes["context.found"] === true
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "context-list",
      description: "Context list command validation",
      config: {
        feature: "context-list",
        expectedSpans: [
          "context.list",
          "context.manager.init",
          "context.manager.list",
        ],
        requiredAttributes: [
          "context.count",
          "current.context",
          "output.format",
        ],
        performanceThresholds: {
          maxLatency: 1000, // 1 second for list operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.list", {
            "output.format": "table",
          }),
          helpers.createSpanExistenceRule("context.manager.list", {}),
          helpers.createSpanStatusRule("context.list", "ok"),
          helpers.createSpanStatusRule("context.manager.list", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "context-list-success",
            (spans) => {
              const listSpan = spans.find((s) => s.name === "context.list");
              return listSpan && listSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "context-count-returned",
            (spans) => {
              const managerListSpan = spans.find(
                (s) => s.name === "context.manager.list",
              );
              return (
                managerListSpan &&
                typeof managerListSpan.attributes["context.count"] === "number"
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "context-use",
      description: "Context use command validation",
      config: {
        expectedSpans: [
          "context.use",
          "context.manager.init",
          "context.manager.use",
          "context.manager.save.current",
        ],
        requiredAttributes: [
          "context.name",
          "context.switched",
          "current.context",
        ],
        performanceThresholds: {
          maxLatency: 1000, // 1 second for use operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.use", {
            "context.name": "test-context",
          }),
          helpers.createSpanExistenceRule("context.manager.use", {
            "context.name": "test-context",
          }),
          helpers.createSpanStatusRule("context.use", "ok"),
          helpers.createSpanStatusRule("context.manager.use", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "context-use-success",
            (spans) => {
              const useSpan = spans.find((s) => s.name === "context.use");
              return useSpan && useSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "context-switched",
            (spans) => {
              const managerUseSpan = spans.find(
                (s) => s.name === "context.manager.use",
              );
              return (
                managerUseSpan &&
                managerUseSpan.attributes["context.switched"] === true
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "context-delete",
      description: "Context delete command validation",
      config: {
        expectedSpans: [
          "context.delete",
          "context.manager.init",
          "context.manager.delete",
          "context.manager.save",
        ],
        requiredAttributes: [
          "context.name",
          "context.deleted",
          "context.count",
        ],
        performanceThresholds: {
          maxLatency: 1000, // 1 second for delete operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.delete", {
            "context.name": "test-context",
          }),
          helpers.createSpanExistenceRule("context.manager.delete", {
            "context.name": "test-context",
          }),
          helpers.createSpanStatusRule("context.delete", "ok"),
          helpers.createSpanStatusRule("context.manager.delete", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "context-delete-success",
            (spans) => {
              const deleteSpan = spans.find((s) => s.name === "context.delete");
              return deleteSpan && deleteSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "context-deleted",
            (spans) => {
              const managerDeleteSpan = spans.find(
                (s) => s.name === "context.manager.delete",
              );
              return (
                managerDeleteSpan &&
                managerDeleteSpan.attributes["context.deleted"] === true
              );
            },
            "error",
          ),
        ],
      },
    },

    {
      name: "context-current",
      description: "Context current command validation",
      config: {
        expectedSpans: [
          "context.current",
          "context.manager.init",
          "context.manager.get.current",
        ],
        requiredAttributes: ["current.context", "current.context.found"],
        performanceThresholds: {
          maxLatency: 1000, // 1 second for current operations
          maxErrorRate: 0.01, // 1%
          minThroughput: 1, // 1 operation per validation
          maxMemoryUsage: 25 * 1024 * 1024, // 25MB
        },
        validationRules: [
          helpers.createSpanExistenceRule("context.current", {}),
          helpers.createSpanExistenceRule("context.manager.get.current", {}),
          helpers.createSpanStatusRule("context.current", "ok"),
          helpers.createSpanStatusRule("context.manager.get.current", "ok"),
          helpers.createPerformanceRule("latency", 1000, "<"),
          helpers.createValidationRule(
            "context-current-success",
            (spans) => {
              const currentSpan = spans.find(
                (s) => s.name === "context.current",
              );
              return currentSpan && currentSpan.status === "ok";
            },
            "error",
          ),
          helpers.createValidationRule(
            "current-context-retrieved",
            (spans) => {
              const managerCurrentSpan = spans.find(
                (s) => s.name === "context.manager.get.current",
              );
              return managerCurrentSpan && managerCurrentSpan.status === "ok";
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
 * Run context commands validation suite
 * @returns {Promise<Object>} Validation report
 */
export async function runContextValidation() {
  console.log("🔍 Starting Context Commands OTEL Validation...");

  try {
    const report = await runner.runSuite(contextSuite);

    // Print summary
    console.log("\n📊 Context Commands Validation Summary:");
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
    console.error("❌ Context commands validation failed:", error.message);
    throw error;
  }
}

/**
 * Validate specific context feature
 * @param {string} featureName - Feature name to validate
 * @returns {Promise<Object>} Feature validation result
 */
export async function validateContextFeature(featureName) {
  const feature = contextSuite.features.find((f) => f.name === featureName);

  if (!feature) {
    throw new Error(
      `Feature '${featureName}' not found in context commands suite`,
    );
  }

  console.log(`🔍 Validating context feature: ${featureName}`);

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
export { contextSuite };

// Auto-run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runContextValidation()
    .then((report) => {
      process.exit(report.summary.failed === 0 ? 0 : 1);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
