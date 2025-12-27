/**
 * @file OTEL Validation Reporter
 * @module validation/otel-reporter
 *
 * @description
 * Formats and reports OTEL validation results.
 */

/**
 * Format validation result as human-readable text
 * @param {Object} result - Validation result
 * @returns {string} Formatted result
 */
export function formatValidationResult(result) {
  const status = result.passed ? '✅ PASSED' : '❌ FAILED';
  let output = `${status} ${result.feature} (Score: ${result.score}/100)\n`;
  output += `  Latency: ${result.metrics.latency}ms\n`;
  output += `  Error Rate: ${(result.metrics.errorRate * 100).toFixed(2)}%\n`;
  output += `  Throughput: ${result.metrics.throughput} ops\n`;
  output += `  Memory: ${(result.metrics.memoryUsage / 1024 / 1024).toFixed(2)}MB\n`;

  if (result.violations.length > 0) {
    output += `  Violations:\n`;
    for (const violation of result.violations) {
      output += `    - ${violation}\n`;
    }
  }

  return output;
}

/**
 * Format validation summary
 * @param {Object} summary - Validation summary
 * @returns {string} Formatted summary
 */
export function formatValidationSummary(summary) {
  let output = `\n📊 Validation Summary:\n`;
  output += `  Total: ${summary.total}\n`;
  output += `  Passed: ${summary.passed}\n`;
  output += `  Failed: ${summary.failed}\n`;
  output += `  Score: ${summary.score}/100\n`;
  output += `  Duration: ${summary.duration}ms\n`;
  return output;
}

/**
 * Format validation result as JSON
 * @param {Object} result - Validation result
 * @returns {string} JSON formatted result
 */
export function formatAsJSON(result) {
  return JSON.stringify(result, null, 2);
}

/**
 * Format validation result as Markdown
 * @param {Object} result - Validation result
 * @returns {string} Markdown formatted result
 */
export function formatAsMarkdown(result) {
  const status = result.passed ? '✅ PASSED' : '❌ FAILED';
  let md = `## ${status} ${result.feature}\n\n`;
  md += `**Score:** ${result.score}/100\n\n`;
  md += `### Metrics\n\n`;
  md += `- **Latency:** ${result.metrics.latency}ms\n`;
  md += `- **Error Rate:** ${(result.metrics.errorRate * 100).toFixed(2)}%\n`;
  md += `- **Throughput:** ${result.metrics.throughput} ops\n`;
  md += `- **Memory:** ${(result.metrics.memoryUsage / 1024 / 1024).toFixed(2)}MB\n`;

  if (result.violations.length > 0) {
    md += `\n### Violations\n\n`;
    for (const violation of result.violations) {
      md += `- ${violation}\n`;
    }
  }

  return md;
}

/**
 * Create validation report
 * @param {Array} results - Validation results
 * @param {Object} [options] - Report options
 * @returns {Object} Report data
 */
export function createReport(results, options = {}) {
  const { format = 'text' } = options;

  const summary = {
    total: results.length,
    passed: results.filter(r => r.passed).length,
    failed: results.filter(r => !r.passed).length,
    score: results.length > 0
      ? Math.round(results.reduce((sum, r) => sum + r.score, 0) / results.length)
      : 0,
    duration: results.reduce((sum, r) => sum + (r.duration || 0), 0),
  };

  return {
    summary,
    results,
    format,
    timestamp: new Date().toISOString(),
  };
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
 * @param {Object} summary - Validation summary
 */
export function printSummary(summary) {
  console.log(formatValidationSummary(summary));
}

/**
 * Validation reporter class
 */
export class ValidationReporter {
  /**
   * Create a validation reporter
   * @param {Object} [config] - Reporter configuration
   */
  constructor(config = {}) {
    this.config = {
      format: 'text',
      verbose: false,
      ...config,
    };
  }

  /**
   * Report validation result
   * @param {Object} result - Validation result
   */
  report(result) {
    if (this.config.format === 'json') {
      console.log(formatAsJSON(result));
    } else if (this.config.format === 'markdown') {
      console.log(formatAsMarkdown(result));
    } else {
      printResult(result);
    }
  }

  /**
   * Report validation summary
   * @param {Object} summary - Validation summary
   */
  reportSummary(summary) {
    printSummary(summary);
  }

  /**
   * Create and save report
   * @param {Array} results - Validation results
   * @returns {Object} Report data
   */
  createReport(results) {
    return createReport(results, { format: this.config.format });
  }
}

/**
 * Create a validation reporter instance
 * @param {Object} [config] - Configuration
 * @returns {ValidationReporter} Reporter instance
 */
export function createReporter(config = {}) {
  return new ValidationReporter(config);
}

/**
 * Default reporter instance
 */
export const defaultReporter = createReporter();
