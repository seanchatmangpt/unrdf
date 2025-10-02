/**
 * @file Refresh Token Endpoint
 * @description POST /api/auth/refresh - Refresh access token using refresh token
 */
import { defineEventHandler, readBody, getCookie, setCookie } from '#imports'
import { z } from 'zod'
import { verifyRefreshToken, getUserById, generateTokenPair } from '../../utils/auth.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { AuthenticationError } from '../../utils/errors.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-auth')

/**
 * Refresh request schema
 */
const refreshSchema = z.object({
  refreshToken: z.string().optional()
})

/**
 * Refresh token handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} New token pair
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('auth.refresh', async (span) => {
    try {
      const body = await readBody(event)

      span.setAttributes({
        'auth.operation': 'refresh_token'
      })

      // Validate request
      const validation = refreshSchema.safeParse(body)
      if (!validation.success) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Validation failed'
        })
        return sendValidationError(event, validation.error)
      }

      // Get refresh token from body or cookie
      let refreshToken = validation.data.refreshToken
      if (!refreshToken) {
        refreshToken = getCookie(event, 'refresh_token')
      }

      if (!refreshToken) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'No refresh token provided'
        })
        throw new AuthenticationError('Refresh token required')
      }

      // Verify refresh token
      const payload = verifyRefreshToken(refreshToken)

      if (!payload || payload.tokenType !== 'refresh') {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Invalid refresh token'
        })
        span.recordException(new AuthenticationError('Invalid refresh token'))

        throw new AuthenticationError('Invalid or expired refresh token')
      }

      // Get user
      const user = getUserById(payload.userId)

      if (!user) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'User not found'
        })
        throw new AuthenticationError('User not found')
      }

      span.setAttributes({
        'auth.user_id': user.id,
        'auth.email': user.email
      })

      // Generate new token pair (refresh token rotation)
      const tokens = generateTokenPair(user.id, user.email, user.roles)

      // Update cookies
      setCookie(event, 'access_token', tokens.accessToken, {
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
        sameSite: 'strict',
        maxAge: tokens.expiresIn
      })

      setCookie(event, 'refresh_token', tokens.refreshToken, {
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
        sameSite: 'strict',
        maxAge: 7 * 24 * 60 * 60 // 7 days
      })

      span.setStatus({ code: SpanStatusCode.OK })

      return sendSuccess(event, {
        accessToken: tokens.accessToken,
        refreshToken: tokens.refreshToken,
        expiresIn: tokens.expiresIn,
        tokenType: 'Bearer'
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
