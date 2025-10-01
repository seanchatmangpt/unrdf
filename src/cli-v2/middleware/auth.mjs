/**
 * @file Authentication Middleware
 * @module cli-v2/middleware/auth
 */

/**
 * Authentication middleware
 * @param {Object} ctx - CLI context
 */
export async function authMiddleware(ctx) {
  // Check if authentication is required
  const config = ctx.config || {};

  if (config.auth && config.auth.required) {
    // TODO: Implement authentication
    // For now, just check for API key in env
    if (!process.env.UNRDF_API_KEY && !config.auth.apiKey) {
      throw new Error('Authentication required. Set UNRDF_API_KEY environment variable.');
    }

    ctx.auth = {
      apiKey: process.env.UNRDF_API_KEY || config.auth.apiKey,
      authenticated: true
    };
  }
}
