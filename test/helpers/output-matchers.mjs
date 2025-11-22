/**
 * @file Flexible output matchers for test assertions
 * @module test-helpers/output-matchers
 *
 * @description
 * Provides flexible pattern matching for CLI output validation,
 * reducing false negatives from strict string matching.
 */

/**
 * Pattern matchers for common CLI output scenarios
 */
export const matchers = {
  /**
   * Check if hook creation was successful
   * @param {string} output - CLI output
   * @param {string} hookName - Expected hook name
   * @returns {boolean}
   */
  hookCreated: (output, hookName) => {
    const patterns = [
      /Hook created/i,
      /Successfully created/i,
      /Generated hook/i,
      /✓.*created/i,
      new RegExp(`Hook.*${hookName}`, 'i'),
      new RegExp(`Created.*${hookName}`, 'i'),
    ];
    return patterns.some(p => p.test(output)) && output.includes(hookName);
  },

  /**
   * Check if policy validation passed
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  policyPassed: output => {
    const patterns = [
      /Status:.*PASSED/i,
      /Validation.*PASSED/i,
      /Policy.*passed/i,
      /✓.*policy/i,
      /All policies passed/i,
      /Validation successful/i,
    ];
    return patterns.some(p => p.test(output));
  },

  /**
   * Check if policy validation failed
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  policyFailed: output => {
    const patterns = [
      /Status:.*FAILED/i,
      /Validation.*FAILED/i,
      /Policy.*failed/i,
      /×.*policy/i,
      /Validation failed/i,
      /Policy violation/i,
    ];
    return patterns.some(p => p.test(output));
  },

  /**
   * Check if command completed successfully
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  commandSuccess: output => {
    const patterns = [/Success/i, /Complete/i, /✓/, /Done/i, /Finished/i];
    const errorPatterns = [/Error/i, /Failed/i, /×/, /Exception/i];
    return patterns.some(p => p.test(output)) && !errorPatterns.some(p => p.test(output));
  },

  /**
   * Check if command failed
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  commandFailure: output => {
    const patterns = [/Error/i, /Failed/i, /×/, /Exception/i, /Cannot/i, /Unable to/i];
    return patterns.some(p => p.test(output));
  },

  /**
   * Check if output contains SPARQL query results
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  sparqlResults: output => {
    const patterns = [
      /Results:/i,
      /Bindings:/i,
      /Query results/i,
      /Found \d+ result/i,
      /"results"/i,
      /"bindings"/i,
    ];
    return patterns.some(p => p.test(output));
  },

  /**
   * Check if output contains performance metrics
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  performanceMetrics: output => {
    const patterns = [/\d+ms/, /Duration:/i, /Execution time/i, /Performance:/i, /Benchmark/i];
    return patterns.some(p => p.test(output));
  },

  /**
   * Check if output indicates file was created
   * @param {string} output - CLI output
   * @param {string} fileName - Expected file name
   * @returns {boolean}
   */
  fileCreated: (output, fileName) => {
    const patterns = [
      new RegExp(`Created.*${fileName}`, 'i'),
      new RegExp(`Generated.*${fileName}`, 'i'),
      new RegExp(`Wrote.*${fileName}`, 'i'),
      new RegExp(`✓.*${fileName}`, 'i'),
    ];
    return patterns.some(p => p.test(output));
  },

  /**
   * Check if output contains validation errors
   * @param {string} output - CLI output
   * @returns {boolean}
   */
  validationErrors: output => {
    const patterns = [
      /Validation error/i,
      /Invalid/i,
      /Schema error/i,
      /SHACL violation/i,
      /Constraint violation/i,
    ];
    return patterns.some(p => p.test(output));
  },

  /**
   * Extract numeric value from output
   * @param {string} output - CLI output
   * @param {RegExp} pattern - Pattern to match
   * @returns {number|null}
   */
  extractNumber: (output, pattern) => {
    const match = output.match(pattern);
    return match ? parseFloat(match[1]) : null;
  },

  /**
   * Check if execution time meets target
   * @param {string} output - CLI output
   * @param {number} maxMs - Maximum milliseconds
   * @returns {boolean}
   */
  meetsPerformanceTarget: (output, maxMs) => {
    const timeMatch = output.match(/(\d+)ms/);
    if (!timeMatch) return false;
    const actualMs = parseInt(timeMatch[1], 10);
    return actualMs <= maxMs;
  },
};

/**
 * Assertion helpers that use matchers
 */
export const assertions = {
  /**
   * Assert hook was created successfully
   */
  assertHookCreated: (output, hookName) => {
    if (!matchers.hookCreated(output, hookName)) {
      throw new Error(`Expected hook '${hookName}' to be created.\nOutput: ${output}`);
    }
  },

  /**
   * Assert policy validation passed
   */
  assertPolicyPassed: output => {
    if (!matchers.policyPassed(output)) {
      throw new Error(`Expected policy validation to pass.\nOutput: ${output}`);
    }
  },

  /**
   * Assert command succeeded
   */
  assertSuccess: output => {
    if (!matchers.commandSuccess(output)) {
      throw new Error(`Expected command to succeed.\nOutput: ${output}`);
    }
  },

  /**
   * Assert performance target met
   */
  assertPerformanceTarget: (output, maxMs, operation) => {
    if (!matchers.meetsPerformanceTarget(output, maxMs)) {
      const timeMatch = output.match(/(\d+)ms/);
      const actual = timeMatch ? timeMatch[1] : 'unknown';
      throw new Error(`${operation} exceeded performance target: ${actual}ms > ${maxMs}ms`);
    }
  },
};

export default { matchers, assertions };
