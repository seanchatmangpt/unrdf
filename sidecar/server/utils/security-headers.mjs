/**
 * Security Headers Middleware
 * Implements comprehensive security headers using Helmet.js principles
 */

import { z } from 'zod';

const SecurityConfigSchema = z.object({
  contentSecurityPolicy: z.object({
    directives: z.record(z.array(z.string()))
  }).optional(),
  hsts: z.object({
    maxAge: z.number(),
    includeSubDomains: z.boolean(),
    preload: z.boolean()
  }).optional(),
  frameguard: z.object({
    action: z.enum(['deny', 'sameorigin'])
  }).optional()
});

/**
 * @typedef {z.infer<typeof SecurityConfigSchema>} SecurityConfig
 */

/**
 * Default security configuration
 * @type {SecurityConfig}
 */
const defaultConfig = {
  contentSecurityPolicy: {
    directives: {
      'default-src': ["'self'"],
      'script-src': ["'self'", "'unsafe-inline'"], // Nuxt requires unsafe-inline
      'style-src': ["'self'", "'unsafe-inline'"],
      'img-src': ["'self'", 'data:', 'https:'],
      'font-src': ["'self'", 'data:'],
      'connect-src': ["'self'"],
      'frame-ancestors': ["'none'"],
      'base-uri': ["'self'"],
      'form-action': ["'self'"]
    }
  },
  hsts: {
    maxAge: 31536000, // 1 year
    includeSubDomains: true,
    preload: true
  },
  frameguard: {
    action: 'deny'
  }
};

/**
 * Apply security headers to response
 * @param {import('h3').H3Event} event - H3 event
 * @param {SecurityConfig} [config] - Custom security configuration
 */
export function applySecurityHeaders(event, config = defaultConfig) {
  const headers = buildSecurityHeaders(config);

  Object.entries(headers).forEach(([name, value]) => {
    setResponseHeader(event, name, value);
  });
}

/**
 * Build security headers object
 * @param {SecurityConfig} config - Security configuration
 * @returns {Record<string, string>}
 */
export function buildSecurityHeaders(config) {
  const headers = {};

  // Content Security Policy
  if (config.contentSecurityPolicy) {
    const csp = Object.entries(config.contentSecurityPolicy.directives)
      .map(([directive, values]) => `${directive} ${values.join(' ')}`)
      .join('; ');
    headers['Content-Security-Policy'] = csp;
  }

  // HSTS
  if (config.hsts) {
    const { maxAge, includeSubDomains, preload } = config.hsts;
    let hsts = `max-age=${maxAge}`;
    if (includeSubDomains) hsts += '; includeSubDomains';
    if (preload) hsts += '; preload';
    headers['Strict-Transport-Security'] = hsts;
  }

  // X-Frame-Options
  if (config.frameguard) {
    headers['X-Frame-Options'] = config.frameguard.action.toUpperCase();
  }

  // Additional security headers
  headers['X-Content-Type-Options'] = 'nosniff';
  headers['X-XSS-Protection'] = '1; mode=block';
  headers['Referrer-Policy'] = 'strict-origin-when-cross-origin';
  headers['Permissions-Policy'] = 'geolocation=(), microphone=(), camera=()';
  headers['X-Permitted-Cross-Domain-Policies'] = 'none';
  headers['Cross-Origin-Embedder-Policy'] = 'require-corp';
  headers['Cross-Origin-Opener-Policy'] = 'same-origin';
  headers['Cross-Origin-Resource-Policy'] = 'same-origin';

  return headers;
}

/**
 * Security headers middleware
 * @param {import('h3').H3Event} event - H3 event
 */
export default defineEventHandler((event) => {
  applySecurityHeaders(event);
});
