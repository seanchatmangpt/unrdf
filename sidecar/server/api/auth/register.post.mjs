/**
 * @file User Registration Endpoint
 * @description POST /api/auth/register - Register new user account
 */
import { defineEventHandler, readBody } from '#imports'
import { z } from 'zod'
import { registerUser, generateTokenPair } from '../../utils/auth.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { ValidationError } from '../../utils/errors.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-auth')

/**
 * Registration request schema
 */
const registerSchema = z.object({
  email: z.string().email('Invalid email format'),
  password: z.string()
    .min(8, 'Password must be at least 8 characters')
    .regex(/[A-Z]/, 'Password must contain at least one uppercase letter')
    .regex(/[a-z]/, 'Password must contain at least one lowercase letter')
    .regex(/[0-9]/, 'Password must contain at least one number'),
  confirmPassword: z.string(),
  roles: z.array(z.string()).optional().default(['user'])
}).refine(data => data.password === data.confirmPassword, {
  message: 'Passwords do not match',
  path: ['confirmPassword']
})

/**
 * Register handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Registration response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('auth.register', async (span) => {
    try {
      const body = await readBody(event)

      span.setAttributes({
        'auth.operation': 'register',
        'auth.email': body.email
      })

      // Validate request
      const validation = registerSchema.safeParse(body)
      if (!validation.success) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Validation failed'
        })
        return sendValidationError(event, validation.error)
      }

      const { email, password, roles } = validation.data

      // Register user
      try {
        const user = await registerUser(email, password, roles)

        span.setAttributes({
          'auth.user_id': user.userId,
          'auth.roles': user.roles.join(',')
        })

        // Generate tokens for immediate login
        const tokens = generateTokenPair(user.userId, user.email, user.roles)

        span.setStatus({ code: SpanStatusCode.OK })

        return sendSuccess(event, {
          user: {
            id: user.userId,
            email: user.email,
            roles: user.roles
          },
          accessToken: tokens.accessToken,
          refreshToken: tokens.refreshToken,
          expiresIn: tokens.expiresIn,
          tokenType: 'Bearer'
        }, 201)

      } catch (error) {
        if (error.message === 'User already exists') {
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: 'User already exists'
          })
          throw new ValidationError('User with this email already exists')
        }
        throw error
      }

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
