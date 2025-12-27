/**
 * @file Login Endpoint
 * @description POST /api/auth/login - Authenticate user and generate JWT tokens
 */
import { defineEventHandler, readBody, setCookie } from '#imports'
import { z } from 'zod'
import { authenticateUser } from '../../utils/auth.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { AuthenticationError } from '../../utils/errors.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-auth')

/**
 * Login request schema
 */
const loginSchema = z.object({
  email: z.string().email('Invalid email format'),
  password: z.string().min(6, 'Password must be at least 6 characters'),
  rememberMe: z.boolean().optional().default(false)
})

/**
 * Login handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Login response with tokens
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('auth.login', async (span) => {
    try {
      const body = await readBody(event)

      span.setAttributes({
        'auth.operation': 'login',
        'auth.email': body.email
      })

      // Validate request
      const validation = loginSchema.safeParse(body)
      if (!validation.success) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Validation failed'
        })
        return sendValidationError(event, validation.error)
      }

      const { email, password, rememberMe } = validation.data

      // Authenticate user
      const result = await authenticateUser(email, password)

      if (!result) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Authentication failed'
        })
        span.recordException(new AuthenticationError('Invalid credentials'))

        throw new AuthenticationError('Invalid email or password')
      }

      const { user, tokens } = result

      span.setAttributes({
        'auth.user_id': user.id,
        'auth.roles': user.roles.join(','),
        'auth.remember_me': rememberMe
      })

      // Set cookies if rememberMe is true
      if (rememberMe) {
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
      }

      span.setStatus({ code: SpanStatusCode.OK })

      return sendSuccess(event, {
        user: {
          id: user.id,
          email: user.email,
          roles: user.roles
        },
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
