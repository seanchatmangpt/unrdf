/**
 * @file Response Builders
 * @description Standardized response formatting utilities
 */

/**
 * Send success response
 * @param {Object} event - H3 event
 * @param {any} data - Response data
 * @param {number} [statusCode=200] - HTTP status code
 * @returns {Object} Response object
 */
export function sendSuccess(event, data, statusCode = 200) {
  event.node.res.statusCode = statusCode
  return {
    success: true,
    data
  }
}

/**
 * Send error response
 * @param {Object} event - H3 event
 * @param {Error} error - Error object
 * @returns {Object} Error response
 */
export function sendError(event, error) {
  const statusCode = error.statusCode || 500
  const code = error.code || 'INTERNAL_ERROR'

  event.node.res.statusCode = statusCode

  const response = {
    success: false,
    error: {
      code,
      message: error.message
    }
  }

  // Include additional error details if available
  if (error.issues) {
    response.error.issues = error.issues
  }
  if (error.violations) {
    response.error.violations = error.violations
  }
  if (error.hookId) {
    response.error.hookId = error.hookId
  }
  if (error.policyId) {
    response.error.policyId = error.policyId
  }

  return response
}

/**
 * Send validation error response
 * @param {Object} event - H3 event
 * @param {Object} zodError - Zod validation error
 * @returns {Object} Validation error response
 */
export function sendValidationError(event, zodError) {
  event.node.res.statusCode = 400

  return {
    success: false,
    error: {
      code: 'VALIDATION_ERROR',
      message: 'Request validation failed',
      issues: zodError.errors.map(err => ({
        path: err.path.join('.'),
        message: err.message,
        code: err.code
      }))
    }
  }
}

/**
 * Wrap async handler with error handling
 * @param {Function} handler - Async handler function
 * @returns {Function} Wrapped handler
 */
export function asyncHandler(handler) {
  return async (event) => {
    try {
      return await handler(event)
    } catch (error) {
      console.error('Handler error:', error)
      return sendError(event, error)
    }
  }
}
