/**
 * @fileoverview Lens execution engine with timing and logging
 * @module lens-executor
 */

import { deepClone } from './lens-utils.mjs';
import { isCompiledLens } from './lens-compiler.mjs';

/**
 * @typedef {Object} ExecutionResult
 * @property {*} result - Transformation result
 * @property {number} durationMs - Execution time in milliseconds
 * @property {string} direction - Transformation direction
 * @property {string} lensName - Lens name
 * @property {number} timestamp - Execution timestamp
 */

/**
 * Execution statistics tracker
 * @type {Map<string, Array<number>>}
 */
const executionStats = new Map();

/**
 * Executes lens transformation from legacy request to substrate operations
 *
 * @param {Object} lens - Compiled lens or lens definition
 * @param {*} legacyRequest - Legacy request data
 * @returns {ExecutionResult} Transformation result with metadata
 * @throws {Error} If transformation fails
 *
 * @example
 * const result = executeToSubstrate(lens, { type: 'CREATE_USER', body: { name: 'Alice' } });
 * console.log(result.result); // { type: 'UPSERT_USER', payload: { name: 'Alice' } }
 */
export function executeToSubstrate(lens, legacyRequest) {
  return executeLens(lens, legacyRequest, 'toSubstrate');
}

/**
 * Executes lens transformation from substrate data to legacy response
 *
 * @param {Object} lens - Compiled lens or lens definition
 * @param {*} substrateData - Substrate data
 * @returns {ExecutionResult} Transformation result with metadata
 * @throws {Error} If transformation fails
 *
 * @example
 * const result = executeFromSubstrate(lens, { id: '123', name: 'Alice', email: 'alice@example.com' });
 * console.log(result.result); // { id: '123', name: 'Alice' }
 */
export function executeFromSubstrate(lens, substrateData) {
  return executeLens(lens, substrateData, 'fromSubstrate');
}

/**
 * Core lens execution logic with timing and error handling
 *
 * @param {Object} lens - Lens definition
 * @param {*} input - Input data
 * @param {string} direction - Transformation direction ('toSubstrate' or 'fromSubstrate')
 * @returns {ExecutionResult} Execution result
 * @throws {Error} If lens is invalid or transformation fails
 * @private
 */
function executeLens(lens, input, direction) {
  // Validate lens
  validateLens(lens, direction);

  const lensName = lens.name;
  const transformFn = lens[direction];

  // Clone input to ensure determinism (prevent mutation)
  const clonedInput = deepClone(input);

  // Execute with timing
  const startTime = performance.now();
  let result;
  let error = null;

  try {
    result = transformFn(clonedInput);
  } catch (err) {
    error = err;
  }

  const endTime = performance.now();
  const durationMs = endTime - startTime;

  // Record statistics
  recordExecution(lensName, direction, durationMs);

  // Handle errors
  if (error) {
    const executionError = new Error(
      `Lens execution failed: ${lensName}.${direction} - ${error.message}`
    );
    executionError.cause = error;
    executionError.lens = lensName;
    executionError.direction = direction;
    executionError.durationMs = durationMs;
    executionError.input = clonedInput;
    throw executionError;
  }

  // Return execution result
  return {
    result,
    durationMs,
    direction,
    lensName,
    timestamp: Date.now(),
  };
}

/**
 * Validates lens has required transformation function
 *
 * @param {Object} lens - Lens to validate
 * @param {string} direction - Required transformation direction
 * @throws {TypeError} If lens is invalid
 * @private
 */
function validateLens(lens, direction) {
  if (!lens || typeof lens !== 'object') {
    throw new TypeError('Lens must be an object');
  }

  if (!lens.name || typeof lens.name !== 'string') {
    throw new TypeError('Lens must have a name');
  }

  if (typeof lens[direction] !== 'function') {
    throw new TypeError(`Lens '${lens.name}' missing ${direction} function`);
  }
}

/**
 * Records execution statistics for performance monitoring
 *
 * @param {string} lensName - Lens name
 * @param {string} direction - Transformation direction
 * @param {number} durationMs - Execution duration
 * @private
 */
function recordExecution(lensName, direction, durationMs) {
  const key = `${lensName}:${direction}`;

  if (!executionStats.has(key)) {
    executionStats.set(key, []);
  }

  const stats = executionStats.get(key);
  stats.push(durationMs);

  // Keep only last 100 executions to prevent memory growth
  if (stats.length > 100) {
    stats.shift();
  }
}

/**
 * Gets execution statistics for a lens
 *
 * @param {string} lensName - Lens name
 * @param {string} [direction] - Optional direction filter
 * @returns {Object} Statistics object
 *
 * @example
 * const stats = getExecutionStats('user-profile', 'toSubstrate');
 * console.log(`Avg: ${stats.avg}ms, Min: ${stats.min}ms, Max: ${stats.max}ms`);
 */
export function getExecutionStats(lensName, direction = null) {
  const keys = direction
    ? [`${lensName}:${direction}`]
    : Array.from(executionStats.keys()).filter(k => k.startsWith(`${lensName}:`));

  if (keys.length === 0) {
    return { count: 0, avg: 0, min: 0, max: 0 };
  }

  let allDurations = [];
  for (const key of keys) {
    const durations = executionStats.get(key) || [];
    allDurations = allDurations.concat(durations);
  }

  if (allDurations.length === 0) {
    return { count: 0, avg: 0, min: 0, max: 0 };
  }

  const count = allDurations.length;
  const sum = allDurations.reduce((a, b) => a + b, 0);
  const avg = sum / count;
  const min = Math.min(...allDurations);
  const max = Math.max(...allDurations);

  return { count, avg, min, max };
}

/**
 * Clears execution statistics (useful for testing)
 *
 * @returns {void}
 *
 * @example
 * clearExecutionStats();
 */
export function clearExecutionStats() {
  executionStats.clear();
}

/**
 * Executes a batch of transformations
 *
 * @param {Object} lens - Lens definition
 * @param {Array<*>} inputs - Array of inputs
 * @param {string} direction - Transformation direction
 * @returns {Array<ExecutionResult>} Array of results
 *
 * @example
 * const results = executeBatch(lens, [req1, req2, req3], 'toSubstrate');
 */
export function executeBatch(lens, inputs, direction) {
  if (!Array.isArray(inputs)) {
    throw new TypeError('Inputs must be an array');
  }

  const results = [];
  const errors = [];

  for (let i = 0; i < inputs.length; i++) {
    try {
      const result = executeLens(lens, inputs[i], direction);
      results.push(result);
    } catch (error) {
      errors.push({ index: i, input: inputs[i], error });
    }
  }

  if (errors.length > 0) {
    const batchError = new Error(
      `Batch execution had ${errors.length}/${inputs.length} failures`
    );
    batchError.errors = errors;
    batchError.successCount = results.length;
    batchError.failureCount = errors.length;
    throw batchError;
  }

  return results;
}

/**
 * Tests lens bidirectionality (round-trip consistency)
 *
 * @param {Object} lens - Lens to test
 * @param {*} legacyInput - Legacy request to test with
 * @returns {Object} Test result with success flag and details
 *
 * @example
 * const test = testBidirectionality(lens, { id: '123', name: 'Alice' });
 * console.log(test.roundTripMatch); // true if legacy -> substrate -> legacy is consistent
 */
export function testBidirectionality(lens, legacyInput) {
  const startTime = performance.now();

  try {
    // Legacy -> Substrate
    const toResult = executeToSubstrate(lens, legacyInput);
    const substrateData = toResult.result;

    // Substrate -> Legacy
    const fromResult = executeFromSubstrate(lens, substrateData);
    const recoveredLegacy = fromResult.result;

    const endTime = performance.now();
    const totalDuration = endTime - startTime;

    return {
      success: true,
      roundTripMatch: JSON.stringify(legacyInput) === JSON.stringify(recoveredLegacy),
      originalInput: legacyInput,
      substrateData,
      recoveredLegacy,
      toSubstrateDuration: toResult.durationMs,
      fromSubstrateDuration: fromResult.durationMs,
      totalDuration,
    };
  } catch (error) {
    return {
      success: false,
      error: error.message,
      originalInput: legacyInput,
    };
  }
}
