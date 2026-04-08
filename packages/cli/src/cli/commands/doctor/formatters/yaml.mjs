/**
 * @file YAML Output Formatter
 * @module cli/commands/doctor/formatters/yaml
 *
 * @description
 * Formats doctor check results as YAML for configuration
 * file generation and human-readable structured data.
 */

/**
 * Format results as YAML
 */
export function formatYAML(results) {
  const summary = {
    total: results.totalChecks,
    passed: results.passedChecks,
    warnings: results.warnings,
    failed: results.failedChecks,
    overallStatus:
      results.failedChecks === 0
        ? 'healthy'
        : results.failedChecks <= 3
          ? 'degraded'
          : 'unhealthy',
  };

  let yaml = `# UNRDF Doctor Health Check Report\n`;
  yaml += `timestamp: ${new Date().toISOString()}\n\n`;

  yaml += `summary:\n`;
  yaml += `  total: ${summary.total}\n`;
  yaml += `  passed: ${summary.passed}\n`;
  yaml += `  warnings: ${summary.warnings}\n`;
  yaml += `  failed: ${summary.failed}\n`;
  yaml += `  overallStatus: ${summary.overallStatus}\n\n`;

  yaml += `categories:\n`;

  for (const category of results.categories) {
    yaml += `  - name: ${category.category}\n`;
    yaml += `    checks:\n`;

    for (const check of category.checks) {
      yaml += `      - name: ${check.name}\n`;
      yaml += `        status: ${check.status}\n`;

      if (check.expected) {
        yaml += `        expected: ${escapeYaml(check.expected)}\n`;
      }
      if (check.actual) {
        yaml += `        actual: ${escapeYaml(check.actual)}\n`;
      }
      if (check.fix) {
        yaml += `        fix: ${escapeYaml(check.fix)}\n`;
      }
      if (check.violations) {
        yaml += `        violations:\n`;
        for (const violation of check.violations.slice(0, 5)) {
          if (typeof violation === 'string') {
            yaml += `          - ${escapeYaml(violation)}\n`;
          } else if (violation.file) {
            yaml += `          - file: ${escapeYaml(violation.file)}\n`;
            if (violation.count) {
              yaml += `            count: ${violation.count}\n`;
            }
          } else if (violation.tool) {
            yaml += `          - tool: ${escapeYaml(violation.tool)}\n`;
            yaml += `            present: ${violation.present}\n`;
          }
        }
        if (check.violations.length > 5) {
          yaml += `          # ... and ${check.violations.length - 5} more\n`;
        }
      }
      if (check.critical) {
        yaml += `        critical: true\n`;
      }
      if (check.details) {
        yaml += `        details: ${JSON.stringify(check.details)}\n`;
      }

      yaml += `\n`;
    }
  }

  return yaml;
}

/**
 * Escape YAML special characters
 */
function escapeYaml(str) {
  if (typeof str !== 'string') {
    return str;
  }

  // Replace special characters that break YAML
  return str.replace(/[:#]/g, '\\$&');
}
