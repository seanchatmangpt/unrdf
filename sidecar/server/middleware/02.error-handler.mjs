/**
 * @file Global Error Handler Middleware
 * @description Catches and formats unhandled errors with OpenAPI compliance
 *
 * OpenAPI Error Response Format:
 * {
 *   "success": false,
 *   "error": {
 *     "code": "ERROR_CODE",
 *     "message": "Human-readable message",
 *     "requestId": "uuid",
 *     "traceId": "otel-trace-id",
 *     "timestamp": "ISO8601",
 *     "path": "/api/endpoint",
 *     "issues": [] // Optional validation errors
 *   }
 * }
 */

import { defineEventHandler } from '#imports'
import { sendError } from '../utils/response.mjs'
import { InternalError } from '../utils/errors.mjs'
import { getTraceIdForLogging, recordExceptionWithContext } from '../utils/otel-context-propagation.mjs'
import { trace, context } from '@opentelemetry/api'

/**
 * Global error handling middleware
 * @param {Object} event - H3 event
 */
export default defineEventHandler(async (event) => {
  try {
    // Let request proceed
    return
  } catch (error) {
    // Record exception in OTEL span
    recordExceptionWithContext(error, {
      'error.handler': 'global-middleware',
      'http.path': event.path,
      'http.method': event.node.req.method
    })

    console.error('[Error Handler] Unhandled error:', {
      message: error.message,
      path: event.path,
      method: event.node.req.method,
      requestId: event.context.requestId,
      traceId: getTraceIdForLogging(),
      stack: error.stack
    })

    // Convert to InternalError if not already an ApiError
    const apiError = error.statusCode
      ? error
      : new InternalError('An unexpected error occurred', error)

    return sendError(event, apiError)
  }
})
