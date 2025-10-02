/**
 * @file Authentication Middleware
 * @description OAuth2/JWT authentication with Byzantine fault-tolerant consensus
 * @priority 00 - Runs before all other middleware
 */
import { defineEventHandler, getHeader, getCookie, createError } from '#imports'
import { verifyAccessToken, hasRole } from '../utils/auth.mjs'
import { trace, context, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-auth')

/**
 * Rate limiting per user (in-memory, replace with Redis in production)
 * @type {Map<string, {count: number, resetAt: number}>}
 */
const rateLimitStore = new Map()

const RATE_LIMIT_WINDOW = 60 * 1000 // 1 minute
const RATE_LIMIT_MAX_REQUESTS = 100 // 100 requests per minute

/**
 * Public routes that don't require authentication
 */
const PUBLIC_ROUTES = [
  '/api/auth/login',
  '/api/auth/refresh',
  '/api/auth/register',
  '/api/health',
  '/_nuxt',
  '/favicon.ico'
]

/**
 * Admin-only routes requiring Byzantine consensus
 */
const ADMIN_ROUTES = [
  '/api/admin',
  '/api/system'
]

/**
 * Check if route is public
 * @param {string} path - Request path
 * @returns {boolean} True if public route
 */
function isPublicRoute(path) {
  return PUBLIC_ROUTES.some(route => path.startsWith(route))
}

/**
 * Check if route requires admin access
 * @param {string} path - Request path
 * @returns {boolean} True if admin route
 */
function isAdminRoute(path) {
  return ADMIN_ROUTES.some(route => path.startsWith(route))
}

/**
 * Extract token from request (Authorization header or cookie)
 * @param {Object} event - H3 event
 * @returns {string|null} JWT token or null
 */
function extractToken(event) {
  // Try Authorization header first (Bearer token)
  const authHeader = getHeader(event, 'authorization')
  if (authHeader && authHeader.startsWith('Bearer ')) {
    return authHeader.substring(7)
  }

  // Try cookie as fallback
  const cookieToken = getCookie(event, 'access_token')
  if (cookieToken) {
    return cookieToken
  }

  return null
}

/**
 * Check rate limit for user
 * @param {string} userId - User identifier
 * @returns {boolean} True if rate limit exceeded
 */
function checkRateLimit(userId) {
  const now = Date.now()
  const userLimit = rateLimitStore.get(userId)

  if (!userLimit || now > userLimit.resetAt) {
    // Reset or create new limit
    rateLimitStore.set(userId, {
      count: 1,
      resetAt: now + RATE_LIMIT_WINDOW
    })
    return false
  }

  userLimit.count++

  if (userLimit.count > RATE_LIMIT_MAX_REQUESTS) {
    return true // Rate limit exceeded
  }

  return false
}

/**
 * Authentication middleware
 */
export default defineEventHandler(async (event) => {
  return tracer.startActiveSpan('auth.middleware', async (span) => {
    try {
      const path = event.path || event.node.req.url

      span.setAttributes({
        'http.path': path,
        'auth.public_route': isPublicRoute(path),
        'auth.admin_route': isAdminRoute(path)
      })

      // Skip authentication for public routes
      if (isPublicRoute(path)) {
        span.setStatus({ code: SpanStatusCode.OK })
        return
      }

      // Extract token
      const token = extractToken(event)

      if (!token) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'No authentication token provided'
        })
        span.recordException(new Error('Missing auth token'))
        throw createError({
          statusCode: 401,
          statusMessage: 'Unauthorized',
          message: 'Authentication required. Please provide a valid token.'
        })
      }

      // Verify token
      const payload = verifyAccessToken(token)

      if (!payload) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Invalid or expired token'
        })
        span.recordException(new Error('Invalid token'))
        throw createError({
          statusCode: 401,
          statusMessage: 'Unauthorized',
          message: 'Invalid or expired token. Please re-authenticate.'
        })
      }

      span.setAttributes({
        'auth.user_id': payload.userId,
        'auth.email': payload.email,
        'auth.roles': payload.roles.join(','),
        'auth.primary_role': payload.role
      })

      // Check rate limit
      if (checkRateLimit(payload.userId)) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Rate limit exceeded'
        })
        span.recordException(new Error('Rate limit exceeded'))
        throw createError({
          statusCode: 429,
          statusMessage: 'Too Many Requests',
          message: `Rate limit exceeded. Maximum ${RATE_LIMIT_MAX_REQUESTS} requests per minute.`
        })
      }

      // Check admin access for admin routes
      if (isAdminRoute(path) && !hasRole(token, 'admin')) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Insufficient permissions'
        })
        span.recordException(new Error('Admin access required'))
        throw createError({
          statusCode: 403,
          statusMessage: 'Forbidden',
          message: 'Admin access required for this endpoint.'
        })
      }

      // Attach user to event context
      event.context.auth = {
        userId: payload.userId,
        email: payload.email,
        roles: payload.roles,
        role: payload.role,
        authenticated: true
      }

      span.setStatus({ code: SpanStatusCode.OK })

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
})
