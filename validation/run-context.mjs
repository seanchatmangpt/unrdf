/**
 * @file Run Context Commands Validation
 * @module validation/run-context
 *
 * @description
 * Entry point for running context commands OTEL validation.
 * This replaces traditional unit tests with span-based validation.
 */

import {
  runContextValidation,
  validateContextFeature,
} from "./context.validation.mjs";

/**
 * Main validation entry point for context commands
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Validation results
 */
export async function runContextCommandsValidation(options = {}) {
  const { feature = null, verbose = true } = options;

  console.log("🎯 UNRDF Context Commands OTEL Validation");
  console.log(
    "   Replacing traditional unit tests with OpenTelemetry span analysis",
  );
  console.log(`   Feature: ${feature || "all"}\n`);

  try {
    let results;

    if (feature) {
      // Validate specific feature
      results = await validateContextFeature(feature);
    } else {
      // Run full validation suite
      results = await runContextValidation();
    }

    // Exit with appropriate code
    const exitCode = feature
      ? results.passed
        ? 0
        : 1
      : results.summary.failed === 0
        ? 0
        : 1;

    if (exitCode === 0) {
      console.log(
        "\n🎉 Context commands validation passed! Features are working correctly.",
      );
    } else {
      console.log(
        "\n❌ Context commands validation failed. Please review the results above.",
      );
    }

    return results;
  } catch (error) {
    console.error(
      "❌ Context commands validation execution failed:",
      error.message,
    );
    throw error;
  }
}

// Auto-run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const feature = process.argv[2] || null;

  runContextCommandsValidation({ feature })
    .then((results) => {
      const exitCode = feature
        ? results.passed
          ? 0
          : 1
        : results.summary.failed === 0
          ? 0
          : 1;
      process.exit(exitCode);
    })
    .catch((error) => {
      console.error("Validation failed:", error);
      process.exit(1);
    });
}
