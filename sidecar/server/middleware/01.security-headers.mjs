/**
 * Security Headers Middleware Entry Point
 * Applies comprehensive security headers to all responses
 */

import { applySecurityHeaders } from '../utils/security-headers.mjs';

/**
 * Apply security headers to all requests
 */
export default defineEventHandler((event) => {
  applySecurityHeaders(event);
});
