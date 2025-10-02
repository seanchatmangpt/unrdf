/**
 * @file Error Classes
 * @description Custom error classes for KGC Sidecar
 */

/**
 * Base API error class
 */
export class ApiError extends Error {
  /**
   * @param {string} message - Error message
   * @param {number} statusCode - HTTP status code
   * @param {string} [code] - Error code
   */
  constructor(message, statusCode, code) {
    super(message)
    this.name = 'ApiError'
    this.statusCode = statusCode
    this.code = code || 'API_ERROR'
  }
}

/**
 * Validation error (400)
 */
export class ValidationError extends ApiError {
  /**
   * @param {string} message - Error message
   * @param {Array<Object>} [issues] - Validation issues
   */
  constructor(message, issues) {
    super(message, 400, 'VALIDATION_ERROR')
    this.name = 'ValidationError'
    this.issues = issues
  }
}

/**
 * Not found error (404)
 */
export class NotFoundError extends ApiError {
  /**
   * @param {string} resource - Resource type
   * @param {string} [id] - Resource ID
   */
  constructor(resource, id) {
    const message = id
      ? `${resource} with ID '${id}' not found`
      : `${resource} not found`
    super(message, 404, 'NOT_FOUND')
    this.name = 'NotFoundError'
  }
}

/**
 * Hook execution error (422)
 */
export class HookExecutionError extends ApiError {
  /**
   * @param {string} hookId - Hook identifier
   * @param {string} reason - Failure reason
   */
  constructor(hookId, reason) {
    super(`Hook '${hookId}' execution failed: ${reason}`, 422, 'HOOK_EXECUTION_ERROR')
    this.name = 'HookExecutionError'
    this.hookId = hookId
  }
}

/**
 * Policy violation error (422)
 */
export class PolicyViolationError extends ApiError {
  /**
   * @param {string} policyId - Policy identifier
   * @param {Array<Object>} violations - Validation violations
   */
  constructor(policyId, violations) {
    super(`Policy '${policyId}' validation failed`, 422, 'POLICY_VIOLATION')
    this.name = 'PolicyViolationError'
    this.policyId = policyId
    this.violations = violations
  }
}

/**
 * Effect timeout error (408)
 */
export class EffectTimeoutError extends ApiError {
  /**
   * @param {string} effectId - Effect identifier
   * @param {number} timeout - Timeout value (ms)
   */
  constructor(effectId, timeout) {
    super(`Effect '${effectId}' timed out after ${timeout}ms`, 408, 'EFFECT_TIMEOUT')
    this.name = 'EffectTimeoutError'
    this.effectId = effectId
    this.timeout = timeout
  }
}

/**
 * Lockchain error (500)
 */
export class LockchainError extends ApiError {
  /**
   * @param {string} message - Error message
   * @param {Error} [cause] - Original error
   */
  constructor(message, cause) {
    super(message, 500, 'LOCKCHAIN_ERROR')
    this.name = 'LockchainError'
    this.cause = cause
  }
}

/**
 * Internal server error (500)
 */
export class InternalError extends ApiError {
  /**
   * @param {string} message - Error message
   * @param {Error} [cause] - Original error
   */
  constructor(message, cause) {
    super(message, 500, 'INTERNAL_ERROR')
    this.name = 'InternalError'
    this.cause = cause
  }
}
