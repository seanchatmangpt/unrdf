/**
 * @file OpenTelemetry Reporter
 * @module validation/otel-reporter
 *
 * @description
 * Reporting utilities for OTEL-based validation results.
 */

/**
 * Format validation result for console output
 * @param {Object} result - Validation result
 * @returns {string} Formatted output
 */
export function formatValidationResult(result) {
  const statusIcon = result.passed ? '✓' : '×';
  const statusColor = result.passed ? '\x1b[32m' : '\x1b[31m';
  const reset = '\x1b[0m';

  let output = `\n${statusColor}${statusIcon}${reset} Feature: ${result.feature}\n`;
  output += `  Score: ${result.score}/100\n`;
  output += `  Status: ${result.passed ? 'PASSED' : 'FAILED'}\n`;
  output += `  Timestamp: ${result.timestamp}\n`;

  // Metrics
  output += `  Metrics:\n`;
  output += `    Latency: ${result.metrics.latency.toFixed(2)}ms\n`;
  output += `    Error Rate: ${(result.metrics.errorRate * 100).toFixed(1)}%\n`;
  output += `    Throughput: ${result.metrics.throughput} ops\n`;
  output += `    Memory: ${formatBytes(result.metrics.memoryUsage)}\n`;

  // Spans
  output += `  Spans (${result.spans.length}):\n`;
  for (const span of result.spans.slice(0, 5)) {
    const spanIcon = span.status === 'ok' ? '✓' : '×';
    output += `    ${spanIcon} ${span.name} (${span.duration.toFixed(1)}ms)\n`;
  }
  if (result.spans.length > 5) {
    output += `    ... and ${result.spans.length - 5} more\n`;
  }

  // Violations
  if (result.violations.length > 0) {
    output += `  Violations (${result.violations.length}):\n`;
    for (const violation of result.violations) {
      output += `    ! ${violation}\n`;
    }
  }

  return output;
}

/**
 * Format multiple validation results as a summary
 * @param {Array<Object>} results - Array of validation results
 * @returns {string} Formatted summary
 */
export function formatValidationSummary(results) {
  const passed = results.filter(r => r.passed).length;
  const failed = results.length - passed;
  const avgScore = results.reduce((sum, r) => sum + r.score, 0) / results.length;

  let output = '\n' + '='.repeat(60) + '\n';
  output += '  VALIDATION SUMMARY\n';
  output += '='.repeat(60) + '\n\n';

  output += `  Total Features: ${results.length}\n`;
  output += `  Passed: ${passed}\n`;
  output += `  Failed: ${failed}\n`;
  output += `  Average Score: ${avgScore.toFixed(1)}/100\n\n`;

  // Per-feature summary
  output += '  Features:\n';
  for (const result of results) {
    const icon = result.passed ? '✓' : '×';
    const color = result.passed ? '\x1b[32m' : '\x1b[31m';
    const reset = '\x1b[0m';
    output += `    ${color}${icon}${reset} ${result.feature}: ${result.score}/100\n`;
  }

  output += '\n' + '='.repeat(60) + '\n';

  return output;
}

/**
 * Format validation result as JSON
 * @param {Object} result - Validation result
 * @param {boolean} [pretty=true] - Pretty print
 * @returns {string} JSON string
 */
export function formatAsJSON(result, pretty = true) {
  return pretty ? JSON.stringify(result, null, 2) : JSON.stringify(result);
}

/**
 * Format validation result as Markdown
 * @param {Object} result - Validation result
 * @returns {string} Markdown string
 */
export function formatAsMarkdown(result) {
  let md = `# Validation Report: ${result.feature}\n\n`;

  md += `**Status:** ${result.passed ? 'PASSED' : 'FAILED'}\n`;
  md += `**Score:** ${result.score}/100\n`;
  md += `**Timestamp:** ${result.timestamp}\n\n`;

  md += `## Metrics\n\n`;
  md += `| Metric | Value |\n`;
  md += `|--------|-------|\n`;
  md += `| Latency | ${result.metrics.latency.toFixed(2)}ms |\n`;
  md += `| Error Rate | ${(result.metrics.errorRate * 100).toFixed(1)}% |\n`;
  md += `| Throughput | ${result.metrics.throughput} ops |\n`;
  md += `| Memory | ${formatBytes(result.metrics.memoryUsage)} |\n\n`;

  md += `## Spans (${result.spans.length})\n\n`;
  md += `| Name | Status | Duration |\n`;
  md += `|------|--------|----------|\n`;
  for (const span of result.spans) {
    md += `| ${span.name} | ${span.status} | ${span.duration.toFixed(1)}ms |\n`;
  }
  md += '\n';

  if (result.violations.length > 0) {
    md += `## Violations (${result.violations.length})\n\n`;
    for (const violation of result.violations) {
      md += `- ${violation}\n`;
    }
  }

  return md;
}

/**
 * Create a validation report object
 * @param {Array<Object>} results - Validation results
 * @returns {Object} Report object
 */
export function createReport(results) {
  const passed = results.filter(r => r.passed).length;
  const failed = results.length - passed;
  const avgScore =
    results.length > 0 ? results.reduce((sum, r) => sum + r.score, 0) / results.length : 0;

  const totalLatency = results.reduce((sum, r) => sum + r.metrics.latency, 0);
  const avgLatency = results.length > 0 ? totalLatency / results.length : 0;

  const allViolations = results.flatMap(r =>
    r.violations.map(v => ({ feature: r.feature, violation: v }))
  );

  return {
    summary: {
      total: results.length,
      passed,
      failed,
      avgScore,
      avgLatency,
      timestamp: new Date().toISOString(),
    },
    features: results.map(r => ({
      name: r.feature,
      passed: r.passed,
      score: r.score,
      metrics: r.metrics,
      spanCount: r.spans.length,
      violationCount: r.violations.length,
    })),
    violations: allViolations,
    details: results,
  };
}

/**
 * Format bytes to human readable string
 * @param {number} bytes - Bytes to format
 * @returns {string} Formatted string
 */
function formatBytes(bytes) {
  const units = ['B', 'KB', 'MB', 'GB'];
  let index = 0;
  let value = bytes;

  while (value >= 1024 && index < units.length - 1) {
    value /= 1024;
    index++;
  }

  return `${value.toFixed(2)} ${units[index]}`;
}

/**
 * Print validation result to console
 * @param {Object} result - Validation result
 */
export function printResult(result) {
  console.log(formatValidationResult(result));
}

/**
 * Print validation summary to console
 * @param {Array<Object>} results - Validation results
 */
export function printSummary(results) {
  console.log(formatValidationSummary(results));
}

/**
 * Reporter class for managing output
 */
export class ValidationReporter {
  /**
   * Create a new reporter
   * @param {Object} [options={}] - Reporter options
   */
  constructor(options = {}) {
    this.format = options.format || 'console';
    this.verbose = options.verbose || false;
    this.output = options.output || console.log;
  }

  /**
   * Report a single validation result
   * @param {Object} result - Validation result
   */
  report(result) {
    switch (this.format) {
      case 'json':
        this.output(formatAsJSON(result));
        break;
      case 'markdown':
        this.output(formatAsMarkdown(result));
        break;
      case 'console':
      default:
        this.output(formatValidationResult(result));
        break;
    }
  }

  /**
   * Report multiple validation results
   * @param {Array<Object>} results - Validation results
   */
  reportAll(results) {
    if (this.format === 'json') {
      this.output(formatAsJSON(createReport(results)));
    } else if (this.format === 'markdown') {
      for (const result of results) {
        this.output(formatAsMarkdown(result));
      }
    } else {
      for (const result of results) {
        this.output(formatValidationResult(result));
      }
      this.output(formatValidationSummary(results));
    }
  }
}

/**
 * Default reporter instance
 */
export const defaultReporter = new ValidationReporter();

/**
 * Create a new reporter
 * @param {Object} [options] - Reporter options
 * @returns {ValidationReporter} New reporter instance
 */
export function createReporter(options) {
  return new ValidationReporter(options);
}
