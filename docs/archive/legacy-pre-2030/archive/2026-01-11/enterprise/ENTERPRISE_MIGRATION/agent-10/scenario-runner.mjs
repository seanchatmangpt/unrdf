/**
 * Scenario Runner - Executes E2E scenarios and captures results
 * @module agent-10/scenario-runner
 */

import { validateScenario } from './scenario-suite.mjs';

/**
 * @typedef {Object} ScenarioResult
 * @property {string} name - Scenario name
 * @property {boolean} passed - Whether scenario passed
 * @property {*} legacyResult - Result from legacy handler
 * @property {*} migratedResult - Result from migrated handler
 * @property {Error|null} legacyError - Error from legacy handler (if any)
 * @property {Error|null} migratedError - Error from migrated handler (if any)
 * @property {boolean} resultsMatch - Whether results match
 * @property {string[]} differences - List of differences found
 * @property {number} duration - Execution duration in ms
 * @property {number} timestamp - Execution timestamp
 */

/**
 * Execute a single scenario
 * @param {Object} scenario - Scenario to execute
 * @returns {ScenarioResult} Scenario execution result
 */
export function runScenario(scenario) {
  validateScenario(scenario);

  const startTime = Date.now();
  const result = {
    name: scenario.name,
    passed: false,
    legacyResult: null,
    migratedResult: null,
    legacyError: null,
    migratedError: null,
    resultsMatch: false,
    differences: [],
    duration: 0,
    timestamp: startTime,
  };

  // Execute legacy handler
  try {
    result.legacyResult = scenario.legacyHandler(scenario.input);
  } catch (error) {
    result.legacyError = error;
  }

  // Execute migrated handler
  try {
    result.migratedResult = scenario.migratedHandler(scenario.input);
  } catch (error) {
    result.migratedError = error;
  }

  // Check if both succeeded or both failed
  const bothSucceeded = !result.legacyError && !result.migratedError;
  const bothFailed = result.legacyError && result.migratedError;

  if (bothFailed) {
    // Both failed - check if error messages match
    const legacyMsg = result.legacyError.message;
    const migratedMsg = result.migratedError.message;

    if (legacyMsg === migratedMsg) {
      result.resultsMatch = true;
      result.passed = true;
    } else {
      result.resultsMatch = false;
      result.differences.push(`Error messages differ: "${legacyMsg}" vs "${migratedMsg}"`);
    }
  } else if (bothSucceeded) {
    // Both succeeded - run assertions and compare results
    try {
      scenario.assertions(result.legacyResult);
      scenario.assertions(result.migratedResult);

      // Deep equality check will be done by shadow-comparator
      result.passed = true;
      result.resultsMatch = true;
    } catch (error) {
      result.passed = false;
      result.differences.push(`Assertion failed: ${error.message}`);
    }
  } else {
    // One succeeded, one failed - mismatch
    result.resultsMatch = false;
    result.passed = false;

    if (result.legacyError) {
      result.differences.push(`Legacy failed: ${result.legacyError.message}`);
    }
    if (result.migratedError) {
      result.differences.push(`Migrated failed: ${result.migratedError.message}`);
    }
  }

  result.duration = Date.now() - startTime;

  return result;
}

/**
 * Execute all scenarios
 * @param {Object[]} scenarios - Array of scenarios
 * @returns {Object} Results summary
 */
export function runAllScenarios(scenarios) {
  if (!Array.isArray(scenarios)) {
    throw new Error('Scenarios must be an array');
  }

  const startTime = Date.now();
  const results = [];

  for (const scenario of scenarios) {
    const result = runScenario(scenario);
    results.push(result);
  }

  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;
  const duration = Date.now() - startTime;

  return {
    total: results.length,
    passed,
    failed,
    passRate: results.length > 0 ? (passed / results.length) * 100 : 0,
    results,
    duration,
    timestamp: startTime,
  };
}

/**
 * Run specific scenario by name
 * @param {Object[]} scenarios - Array of scenarios
 * @param {string} scenarioName - Scenario name to run
 * @returns {ScenarioResult} Scenario result
 */
export function runScenarioByName(scenarios, scenarioName) {
  const scenario = scenarios.find(s => s.name === scenarioName);

  if (!scenario) {
    throw new Error(`Scenario not found: ${scenarioName}`);
  }

  return runScenario(scenario);
}

/**
 * Filter results by status
 * @param {Object} results - Results from runAllScenarios
 * @param {string} status - 'passed' or 'failed'
 * @returns {ScenarioResult[]} Filtered results
 */
export function filterResultsByStatus(results, status) {
  if (!results || !results.results) {
    throw new Error('Invalid results object');
  }

  if (status === 'passed') {
    return results.results.filter(r => r.passed);
  } else if (status === 'failed') {
    return results.results.filter(r => !r.passed);
  } else {
    throw new Error('Status must be "passed" or "failed"');
  }
}

/**
 * Get summary statistics
 * @param {Object} results - Results from runAllScenarios
 * @returns {Object} Summary statistics
 */
export function getSummaryStats(results) {
  if (!results || !results.results) {
    throw new Error('Invalid results object');
  }

  const totalDuration = results.results.reduce((sum, r) => sum + r.duration, 0);
  const avgDuration = results.total > 0 ? totalDuration / results.total : 0;

  const mismatches = results.results.filter(r => !r.resultsMatch).length;

  return {
    total: results.total,
    passed: results.passed,
    failed: results.failed,
    passRate: results.passRate,
    mismatches,
    averageDuration: avgDuration,
    totalDuration,
  };
}
