/**
 * @fileoverview Minimal optional bearer auth for Nitro routes (80/20)
 */

/**
 * Validate bearer token if AUTH_REQUIRED=true; otherwise allow anonymous.
 * Token is compared against AUTH_TOKEN or JWT_SECRET for simplicity.
 */
export function requireAuth(event) {
  const authRequired = process.env.AUTH_REQUIRED === 'true'
  if (!authRequired) return { user: { username: 'anonymous' } }

  const header = getHeader(event, 'authorization') || ''
  const token = header.toLowerCase().startsWith('bearer ')
    ? header.slice(7)
    : null

  const expected = process.env.AUTH_TOKEN || process.env.JWT_SECRET

  if (!token || !expected || token !== expected) {
    throw createError({ statusCode: 401, statusMessage: 'Unauthorized' })
  }

  return { user: { username: 'token-user' } }
}



