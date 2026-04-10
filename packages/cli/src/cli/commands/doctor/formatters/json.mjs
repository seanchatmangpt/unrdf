/**
 * @file JSON Output Formatter
 * @module cli/commands/doctor/formatters/json
 *
 * @description
 * Formats doctor check results as machine-readable JSON
 * for CI/CD integration and programmatic consumption.
 */

/**
 * Format results as JSON
 */
export function formatJSON(results) {
  const summary = {
    total: results.totalChecks,
    passed: results.passedChecks,
    warnings: results.warnings,
    failed: results.failedChecks,
    overallStatus:
      results.failedChecks === 0 ? 'healthy' : results.failedChecks <= 3 ? 'degraded' : 'unhealthy',
  };

  const categories = results.categories.map(category => {
    const checks = category.checks.map(check => {
      const result = {
        name: check.name,
        status: check.status,
      };

      if (check.expected) result.expected = check.expected;
      if (check.actual) result.actual = check.actual;
      if (check.fix) result.fix = check.fix;
      if (check.violations) result.violations = check.violations;
      if (check.critical) result.critical = true;
      if (check.details) result.details = check.details;

      return result;
    });

    return {
      category: category.category,
      checks,
    };
  });

  return JSON.stringify(
    {
      timestamp: new Date().toISOString(),
      summary,
      categories,
    },
    null,
    2
  );
}
