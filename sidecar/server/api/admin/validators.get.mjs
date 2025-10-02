/**
 * @file Get Byzantine Validators Endpoint
 * @description GET /api/admin/validators - List all Byzantine consensus validators
 */
import { defineEventHandler, createError } from '#imports'
import { getAllValidators, hasRole } from '../../utils/auth.mjs'
import { sendSuccess, asyncHandler } from '../../utils/response.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-admin')

/**
 * Get validators handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} List of validators
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('admin.get_validators', async (span) => {
    try {
      // Check authentication
      if (!event.context.auth?.authenticated) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Unauthorized'
        })
        throw createError({
          statusCode: 401,
          statusMessage: 'Unauthorized',
          message: 'Authentication required'
        })
      }

      // Check admin role
      const token = event.node.req.headers.authorization?.substring(7)
      if (!token || !hasRole(token, 'admin')) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Forbidden'
        })
        throw createError({
          statusCode: 403,
          statusMessage: 'Forbidden',
          message: 'Admin access required'
        })
      }

      span.setAttributes({
        'admin.user_id': event.context.auth.userId
      })

      // Get all validators
      const validators = getAllValidators()

      span.setAttributes({
        'admin.validator_count': validators.length
      })

      span.setStatus({ code: SpanStatusCode.OK })

      return sendSuccess(event, {
        validators,
        totalValidators: validators.length,
        consensusThreshold: 3
      })

    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      })
      span.recordException(error)
      throw error
    } finally {
      span.end()
    }
  })
}))
