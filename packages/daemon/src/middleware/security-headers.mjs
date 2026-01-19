/**
 * @file Security Headers Middleware
 * @module @unrdf/daemon/middleware/security-headers
 * @description Production-grade security middleware with CSP, CORS, rate limiting, and input sanitization
 */

import { createHash } from 'crypto';
import {
  CSPConfigSchema,
  CORSConfigSchema,
  RequestLimitsSchema,
  RateLimitConfigSchema,
  SecurityHeadersConfigSchema,
} from './security-schemas.mjs';
import { SANITIZATION_PATTERNS } from './sanitization-patterns.mjs';
import { DEFAULT_SECURITY_CONFIG } from './security-defaults.mjs';
import { generateCSPHeader, generateNonce } from './csp-utils.mjs';

// Re-export schemas for backward compatibility
export {
  CSPConfigSchema,
  CORSConfigSchema,
  RequestLimitsSchema,
  RateLimitConfigSchema,
  SecurityHeadersConfigSchema,
};

// Re-export defaults
export { DEFAULT_SECURITY_CONFIG };

/**
 * SecurityHeadersMiddleware - Comprehensive security middleware
 */
export class SecurityHeadersMiddleware {
  /**
   * @param {Object} config - Security configuration
   */
  constructor(config = {}) {
    this.config = SecurityHeadersConfigSchema.parse(config);
    this.rateLimitStore = new Map();
    this.requestTimeouts = new Map();
    this.nonceCache = new Map();
  }

  /**
   * Generate CSP header value (wrapper for backward compatibility)
   * @param {string} [nonce] - Optional nonce for inline scripts
   * @returns {string} CSP header value
   */
  generateCSPHeader(nonce) {
    return generateCSPHeader(this.config.csp, nonce);
  }

  /**
   * Generate random nonce for CSP (wrapper for backward compatibility)
   * @returns {string} Base64 nonce
   */
  generateNonce() {
    return generateNonce();
  }

  /**
   * Check CORS origin
   * @param {string} origin - Request origin
   * @returns {boolean} Whether origin is allowed
   */
  checkCORSOrigin(origin) {
    const cors = this.config.cors || CORSConfigSchema.parse({});

    if (typeof cors.origin === 'string') {
      return cors.origin === '*' || cors.origin === origin;
    }

    if (Array.isArray(cors.origin)) {
      return cors.origin.includes(origin);
    }

    if (typeof cors.origin === 'function') {
      return cors.origin(origin);
    }

    return false;
  }

  /**
   * Apply CORS headers
   * @param {Object} request - Request object
   * @param {Object} response - Response object
   */
  applyCORSHeaders(request, response) {
    const cors = this.config.cors || CORSConfigSchema.parse({});
    const origin = request.headers?.origin || request.headers?.Origin;

    if (origin && this.checkCORSOrigin(origin)) {
      response.headers = response.headers || {};
      response.headers['Access-Control-Allow-Origin'] = origin;

      if (cors.credentials) {
        response.headers['Access-Control-Allow-Credentials'] = 'true';
      }
    } else if (cors.origin === '*') {
      response.headers = response.headers || {};
      response.headers['Access-Control-Allow-Origin'] = '*';
    }

    if (request.method === 'OPTIONS') {
      response.headers = response.headers || {};
      response.headers['Access-Control-Allow-Methods'] = cors.methods.join(', ');
      response.headers['Access-Control-Allow-Headers'] = cors.allowedHeaders.join(', ');
      response.headers['Access-Control-Max-Age'] = cors.maxAge.toString();

      if (cors.exposedHeaders.length > 0) {
        response.headers['Access-Control-Expose-Headers'] = cors.exposedHeaders.join(', ');
      }
    }
  }

  /**
   * Check rate limit
   * @param {string} key - Rate limit key (e.g., IP address)
   * @returns {Object} Rate limit result
   */
  checkRateLimit(key) {
    const rateLimit = this.config.rateLimit || RateLimitConfigSchema.parse({});
    const now = Date.now();

    const record = this.rateLimitStore.get(key) || {
      count: 0,
      resetTime: now + rateLimit.windowMs,
    };

    // Reset if window expired
    if (now >= record.resetTime) {
      record.count = 0;
      record.resetTime = now + rateLimit.windowMs;
    }

    record.count++;
    this.rateLimitStore.set(key, record);

    const allowed = record.count <= rateLimit.maxRequests;
    const remaining = Math.max(0, rateLimit.maxRequests - record.count);
    const resetIn = Math.max(0, Math.ceil((record.resetTime - now) / 1000));

    return {
      allowed,
      remaining,
      resetIn,
      limit: rateLimit.maxRequests,
    };
  }

  /**
   * Sanitize input string
   * @param {string} input - Input to sanitize
   * @param {Object} [options] - Sanitization options
   * @returns {string} Sanitized input
   */
  sanitizeInput(input, options = {}) {
    if (typeof input !== 'string') {
      return input;
    }

    let sanitized = input;

    // Apply sanitization patterns (multiple passes for nested patterns)
    if (options.checkXSS !== false) {
      // Multiple passes to handle nested/escaped patterns
      for (let i = 0; i < 3; i++) {
        sanitized = sanitized.replace(SANITIZATION_PATTERNS.xss, '');
      }
    }

    if (options.checkSQL !== false) {
      sanitized = sanitized.replace(SANITIZATION_PATTERNS.sql, '');
    }

    if (options.checkPathTraversal !== false) {
      sanitized = sanitized.replace(SANITIZATION_PATTERNS.pathTraversal, '');
    }

    if (options.checkCommandInjection !== false) {
      sanitized = sanitized.replace(SANITIZATION_PATTERNS.commandInjection, '');
    }

    // Trim whitespace
    sanitized = sanitized.trim();

    // Limit length if specified
    if (options.maxLength && sanitized.length > options.maxLength) {
      sanitized = sanitized.substring(0, options.maxLength);
    }

    return sanitized;
  }

  /**
   * Sanitize object recursively
   * @param {Object} obj - Object to sanitize
   * @param {Object} [options] - Sanitization options
   * @returns {Object} Sanitized object
   */
  sanitizeObject(obj, options = {}) {
    if (typeof obj !== 'object' || obj === null) {
      // If it's a string, sanitize it
      if (typeof obj === 'string') {
        return this.sanitizeInput(obj, options);
      }
      return obj;
    }

    if (Array.isArray(obj)) {
      return obj.map(item => {
        if (typeof item === 'string') {
          return this.sanitizeInput(item, options);
        }
        return this.sanitizeObject(item, options);
      });
    }

    const sanitized = {};
    for (const [key, value] of Object.entries(obj)) {
      const sanitizedKey = this.sanitizeInput(key, options);

      if (typeof value === 'string') {
        sanitized[sanitizedKey] = this.sanitizeInput(value, options);
      } else if (typeof value === 'object' && value !== null) {
        sanitized[sanitizedKey] = this.sanitizeObject(value, options);
      } else {
        sanitized[sanitizedKey] = value;
      }
    }

    return sanitized;
  }

  /**
   * Check request size limits
   * @param {Object} request - Request object
   * @returns {Object} Validation result
   */
  checkRequestLimits(request) {
    const limits = this.config.requestLimits || RequestLimitsSchema.parse({});
    const errors = [];

    // Check body size
    if (request.body) {
      const bodySize = Buffer.byteLength(JSON.stringify(request.body), 'utf8');
      if (bodySize > limits.maxBodySize) {
        errors.push(`Body size ${bodySize} exceeds limit ${limits.maxBodySize}`);
      }
    }

    // Check URL length
    if (request.url && request.url.length > limits.maxUrlLength) {
      errors.push(`URL length ${request.url.length} exceeds limit ${limits.maxUrlLength}`);
    }

    // Check header size
    if (request.headers) {
      const headerSize = Buffer.byteLength(JSON.stringify(request.headers), 'utf8');
      if (headerSize > limits.maxHeaderSize) {
        errors.push(`Header size ${headerSize} exceeds limit ${limits.maxHeaderSize}`);
      }
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }

  /**
   * Apply security headers to response
   * @param {Object} response - Response object
   * @param {Object} [options] - Options
   */
  applySecurityHeaders(response, options = {}) {
    response.headers = response.headers || {};

    // Content Security Policy
    if (this.config.csp) {
      const nonce = options.nonce || generateNonce();
      const cspHeader = generateCSPHeader(this.config.csp, nonce);
      const headerName = this.config.csp.reportOnly
        ? 'Content-Security-Policy-Report-Only'
        : 'Content-Security-Policy';
      response.headers[headerName] = cspHeader;
      response.cspNonce = nonce;
    }

    // HSTS
    if (this.config.enableHSTS) {
      response.headers['Strict-Transport-Security'] =
        `max-age=${this.config.hstsMaxAge}; includeSubDomains`;
    }

    // X-Content-Type-Options
    if (this.config.enableNoSniff) {
      response.headers['X-Content-Type-Options'] = 'nosniff';
    }

    // X-Frame-Options
    if (this.config.enableXFrameOptions) {
      response.headers['X-Frame-Options'] = this.config.xFrameOptions;
    }

    // X-XSS-Protection
    if (this.config.enableXSSProtection) {
      response.headers['X-XSS-Protection'] = '1; mode=block';
    }

    // Referrer-Policy
    if (this.config.enableReferrerPolicy) {
      response.headers['Referrer-Policy'] = this.config.referrerPolicy;
    }

    // Permissions-Policy
    if (this.config.enablePermissionsPolicy) {
      response.headers['Permissions-Policy'] =
        'geolocation=(), microphone=(), camera=()';
    }

    // Custom headers
    Object.assign(response.headers, this.config.customHeaders);
  }

  /**
   * Process request with timeout
   * @param {string} requestId - Request ID
   * @param {Function} handler - Request handler
   * @param {number} [timeout] - Timeout in milliseconds
   * @returns {Promise<any>} Handler result
   */
  async processWithTimeout(requestId, handler, timeout) {
    const limits = this.config.requestLimits || RequestLimitsSchema.parse({});
    const timeoutMs = timeout || limits.timeout;

    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.requestTimeouts.delete(requestId);
        reject(new Error(`Request timeout after ${timeoutMs}ms`));
      }, timeoutMs);

      this.requestTimeouts.set(requestId, timer);

      handler()
        .then(result => {
          clearTimeout(timer);
          this.requestTimeouts.delete(requestId);
          resolve(result);
        })
        .catch(error => {
          clearTimeout(timer);
          this.requestTimeouts.delete(requestId);
          reject(error);
        });
    });
  }

  /**
   * Middleware handler
   * @param {Object} request - Request object
   * @param {Object} response - Response object
   * @param {Function} next - Next middleware
   * @returns {Promise<void>}
   */
  async handle(request, response, next) {
    try {
      // Generate request ID
      const requestId = createHash('sha256')
        .update(`${Date.now()}-${Math.random()}`)
        .digest('hex')
        .substring(0, 16);

      // Check request limits
      const limitsCheck = this.checkRequestLimits(request);
      if (!limitsCheck.valid) {
        response.statusCode = 413;
        response.body = { error: 'Request too large', details: limitsCheck.errors };
        return;
      }

      // Check rate limit
      if (this.config.rateLimit) {
        const rateLimitKey = this.config.rateLimit.keyGenerator
          ? this.config.rateLimit.keyGenerator(request)
          : request.ip || request.headers?.['x-forwarded-for'] || 'unknown';

        const rateLimit = this.checkRateLimit(rateLimitKey);

        response.headers = response.headers || {};
        response.headers['X-RateLimit-Limit'] = rateLimit.limit.toString();
        response.headers['X-RateLimit-Remaining'] = rateLimit.remaining.toString();
        response.headers['X-RateLimit-Reset'] = rateLimit.resetIn.toString();

        if (!rateLimit.allowed) {
          response.statusCode = 429;
          response.body = {
            error: 'Too many requests',
            retryAfter: rateLimit.resetIn,
          };
          return;
        }
      }

      // Apply CORS headers
      if (this.config.cors) {
        this.applyCORSHeaders(request, response);

        // Handle preflight
        if (request.method === 'OPTIONS') {
          response.statusCode = 204;
          return;
        }
      }

      // Apply security headers
      this.applySecurityHeaders(response);

      // Sanitize request body
      if (request.body) {
        request.sanitizedBody = this.sanitizeObject(request.body);
      }

      // Process with timeout
      if (next) {
        await this.processWithTimeout(requestId, () => next(request, response));
      }
    } catch (error) {
      response.statusCode = 500;
      response.body = { error: 'Internal server error' };
      // Don't leak error details in production
      if (process.env.NODE_ENV !== 'production') {
        response.body.details = error.message;
      }
    }
  }

  /**
   * Create middleware function
   * @returns {Function} Middleware function
   */
  middleware() {
    return (request, response, next) => this.handle(request, response, next);
  }

  /**
   * Cleanup expired rate limit entries
   */
  cleanupRateLimits() {
    const now = Date.now();
    for (const [key, record] of this.rateLimitStore.entries()) {
      if (now >= record.resetTime) {
        this.rateLimitStore.delete(key);
      }
    }
  }

  /**
   * Cleanup timeouts
   */
  cleanup() {
    for (const timer of this.requestTimeouts.values()) {
      clearTimeout(timer);
    }
    this.requestTimeouts.clear();
    this.rateLimitStore.clear();
    this.nonceCache.clear();
  }
}

/**
 * Create security headers middleware
 * @param {Object} config - Security configuration
 * @returns {SecurityHeadersMiddleware} Middleware instance
 */
export function createSecurityMiddleware(config) {
  return new SecurityHeadersMiddleware(config);
}
