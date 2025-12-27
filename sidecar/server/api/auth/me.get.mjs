/**
 * @file Get Current User Endpoint
 * @description GET /api/auth/me - Get authenticated user information
 */
import { defineEventHandler, createError } from '#imports'
import { sendSuccess, asyncHandler } from '../../utils/response.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-auth')

/**
 * Get current user handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Current user information
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('auth.me', async (span) => {
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

      const auth = event.context.auth

      span.setAttributes({
        'auth.user_id': auth.userId,
        'auth.email': auth.email,
        'auth.roles': auth.roles.join(',')
      })

      span.setStatus({ code: SpanStatusCode.OK })

      return sendSuccess(event, {
        user: {
          id: auth.userId,
          email: auth.email,
          roles: auth.roles,
          role: auth.role
        }
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
