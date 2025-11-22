/**
 * @file Assertion Helpers for Tests
 * @module test-utils/assertion-helpers
 *
 * @description
 * Custom assertion helpers for common test patterns
 * and complex validation scenarios.
 */

/**
 * Assert that a hook execution result is successful
 * @param {Object} result - Hook execution result
 * @param {string} [message] - Custom error message
 */
export function assertHookSuccess(result, message = 'Hook should succeed') {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (result.success !== true) {
    throw new Error(`${message}: Expected success=true, got ${result.success}`);
  }

  if (result.error) {
    throw new Error(`${message}: Result has error: ${result.error}`);
  }
}

/**
 * Assert that a hook execution result is a failure
 * @param {Object} result - Hook execution result
 * @param {string} [expectedError] - Expected error message pattern
 * @param {string} [message] - Custom error message
 */
export function assertHookFailure(result, expectedError = null, message = 'Hook should fail') {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (result.success !== false) {
    throw new Error(`${message}: Expected success=false, got ${result.success}`);
  }

  if (!result.error && !result.cancelled) {
    throw new Error(`${message}: Result should have error or be cancelled`);
  }

  if (expectedError && result.error && !result.error.includes(expectedError)) {
    throw new Error(
      `${message}: Expected error to contain "${expectedError}", got "${result.error}"`
    );
  }
}

/**
 * Assert that multiple hook results have expected success/failure pattern
 * @param {Array} results - Array of hook execution results
 * @param {Array<boolean>} expectedSuccess - Array of expected success values
 * @param {string} [message] - Custom error message
 */
export function assertHookResults(
  results,
  expectedSuccess,
  message = 'Hook results should match expectations'
) {
  if (!Array.isArray(results)) {
    throw new Error(`${message}: Results is not an array`);
  }

  if (!Array.isArray(expectedSuccess)) {
    throw new Error(`${message}: Expected success is not an array`);
  }

  if (results.length !== expectedSuccess.length) {
    throw new Error(
      `${message}: Expected ${expectedSuccess.length} results, got ${results.length}`
    );
  }

  results.forEach((result, index) => {
    const expected = expectedSuccess[index];
    if (result.success !== expected) {
      throw new Error(
        `${message}: Result ${index} expected success=${expected}, got ${result.success}`
      );
    }
  });
}

/**
 * Assert that a hook execution took expected time
 * @param {Object} result - Hook execution result
 * @param {number} maxDurationMs - Maximum expected duration in milliseconds
 * @param {string} [message] - Custom error message
 */
export function assertHookDuration(
  result,
  maxDurationMs,
  message = 'Hook should complete within time limit'
) {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (typeof result.durationMs !== 'number') {
    throw new Error(`${message}: Result does not have durationMs`);
  }

  if (result.durationMs > maxDurationMs) {
    throw new Error(`${message}: Hook took ${result.durationMs}ms, expected <= ${maxDurationMs}ms`);
  }
}

/**
 * Assert that a hook execution consumed expected memory
 * @param {Object} result - Hook execution result
 * @param {number} maxMemoryMB - Maximum expected memory in MB
 * @param {string} [message] - Custom error message
 */
export function assertHookMemory(
  result,
  maxMemoryMB,
  message = 'Hook should use reasonable memory'
) {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (typeof result.memoryUsed !== 'number') {
    throw new Error(`${message}: Result does not have memoryUsed`);
  }

  if (result.memoryUsed > maxMemoryMB) {
    throw new Error(`${message}: Hook used ${result.memoryUsed}MB, expected <= ${maxMemoryMB}MB`);
  }
}

/**
 * Assert that a hook was cancelled
 * @param {Object} result - Hook execution result
 * @param {string} [expectedReason] - Expected cancellation reason
 * @param {string} [message] - Custom error message
 */
export function assertHookCancelled(
  result,
  expectedReason = null,
  message = 'Hook should be cancelled'
) {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (result.cancelled !== true) {
    throw new Error(`${message}: Expected cancelled=true, got ${result.cancelled}`);
  }

  if (!result.cancelReason) {
    throw new Error(`${message}: Result should have cancelReason`);
  }

  if (expectedReason && !result.cancelReason.includes(expectedReason)) {
    throw new Error(
      `${message}: Expected cancel reason to contain "${expectedReason}", got "${result.cancelReason}"`
    );
  }
}

/**
 * Assert that a hook execution phase completed
 * @param {Object} result - Hook execution result
 * @param {string} expectedPhase - Expected completion phase
 * @param {string} [message] - Custom error message
 */
export function assertHookPhase(
  result,
  expectedPhase,
  message = 'Hook should complete expected phase'
) {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (result.phase !== expectedPhase) {
    throw new Error(`${message}: Expected phase="${expectedPhase}", got "${result.phase}"`);
  }
}

/**
 * Assert that a hook result contains expected data
 * @param {Object} result - Hook execution result
 * @param {Object} expectedData - Expected data properties
 * @param {string} [message] - Custom error message
 */
export function assertHookData(
  result,
  expectedData,
  message = 'Hook result should contain expected data'
) {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  Object.entries(expectedData).forEach(([key, expectedValue]) => {
    if (!(key in result)) {
      throw new Error(`${message}: Result missing property "${key}"`);
    }

    if (result[key] !== expectedValue) {
      throw new Error(
        `${message}: Result.${key} expected "${expectedValue}", got "${result[key]}"`
      );
    }
  });
}

/**
 * Assert that a validation error contains expected information
 * @param {Error} error - Validation error
 * @param {string} expectedField - Expected field name
 * @param {string} [expectedMessage] - Expected error message pattern
 * @param {string} [message] - Custom error message
 */
export function assertValidationError(
  error,
  expectedField,
  expectedMessage = null,
  message = 'Should throw validation error'
) {
  if (!error || !(error instanceof Error)) {
    throw new Error(`${message}: Expected Error instance, got ${typeof error}`);
  }

  if (!error.message.includes(expectedField)) {
    throw new Error(
      `${message}: Error message should contain field "${expectedField}", got "${error.message}"`
    );
  }

  if (expectedMessage && !error.message.includes(expectedMessage)) {
    throw new Error(
      `${message}: Error message should contain "${expectedMessage}", got "${error.message}"`
    );
  }
}

/**
 * Assert that a security check passed
 * @param {Object} result - Security check result
 * @param {string} [message] - Custom error message
 */
export function assertSecurityPassed(result, message = 'Security check should pass') {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (result.securityViolation) {
    throw new Error(`${message}: Security violation detected: ${result.securityViolation}`);
  }

  if (result.blocked) {
    throw new Error(`${message}: Operation was blocked: ${result.blockReason}`);
  }
}

/**
 * Assert that a security check failed
 * @param {Object} result - Security check result
 * @param {string} [expectedViolation] - Expected violation type
 * @param {string} [message] - Custom error message
 */
export function assertSecurityFailed(
  result,
  expectedViolation = null,
  message = 'Security check should fail'
) {
  if (!result || typeof result !== 'object') {
    throw new Error(`${message}: Result is not an object`);
  }

  if (!result.securityViolation && !result.blocked) {
    throw new Error(`${message}: Expected security violation or block, but check passed`);
  }

  if (
    expectedViolation &&
    result.securityViolation &&
    !result.securityViolation.includes(expectedViolation)
  ) {
    throw new Error(
      `${message}: Expected violation "${expectedViolation}", got "${result.securityViolation}"`
    );
  }
}
