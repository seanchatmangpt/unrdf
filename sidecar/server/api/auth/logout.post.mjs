/**
 * @file Logout Endpoint
 * @description POST /api/auth/logout - Logout user and clear tokens
 */
import { defineEventHandler, deleteCookie } from '#imports'
import { sendSuccess, asyncHandler } from '../../utils/response.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-auth')

/**
 * Logout handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Logout response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('auth.logout', async (span) => {
    try {
      const auth = event.context.auth

      span.setAttributes({
        'auth.operation': 'logout',
        'auth.user_id': auth?.userId || 'anonymous'
      })

      // Clear cookies
      deleteCookie(event, 'access_token')
      deleteCookie(event, 'refresh_token')

      span.setStatus({ code: SpanStatusCode.OK })

      return sendSuccess(event, {
        message: 'Logged out successfully'
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
