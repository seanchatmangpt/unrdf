/**
 * @fileoverview Security utilities for UNRDF
 * Provides input validation, sanitization, rate limiting, and CSRF protection
 * Following OWASP Top 10 best practices
 * @module security
 */

import crypto from 'node:crypto';

/**
 * Sanitizes HTML input to prevent XSS attacks
 * @param {string} input - Raw HTML string
 * @returns {string} Sanitized string
 * @example
 * const safe = sanitizeHTML('<script>alert("xss")</script>Hello');
 * // Returns: '&lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;Hello'
 */
export function sanitizeHTML(input) {
  if (typeof input !== 'string') return '';

  return input
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#x27;')
    .replace(/\//g, '&#x2F;');
}

/**
 * Validates and sanitizes URL to prevent open redirect vulnerabilities
 * @param {string} url - URL to validate
 * @param {string[]} allowedDomains - Whitelist of allowed domains
 * @returns {string|null} Sanitized URL or null if invalid
 * @example
 * const safe = sanitizeURL('https://example.com/path', ['example.com']);
 * // Returns: 'https://example.com/path'
 *
 * const unsafe = sanitizeURL('https://evil.com', ['example.com']);
 * // Returns: null
 */
export function sanitizeURL(url, allowedDomains = []) {
  try {
    const parsed = new URL(url);

    // Only allow http/https protocols
    if (!['http:', 'https:'].includes(parsed.protocol)) {
      return null;
    }

    // Check domain whitelist if provided
    if (allowedDomains.length > 0) {
      const isAllowed = allowedDomains.some(domain =>
        parsed.hostname === domain || parsed.hostname.endsWith(`.${domain}`)
      );

      if (!isAllowed) {
        return null;
      }
    }

    return parsed.toString();
  } catch {
    return null;
  }
}

/**
 * Validates file path to prevent directory traversal attacks
 * @param {string} filePath - File path to validate
 * @param {string} baseDir - Base directory to restrict access to
 * @returns {boolean} True if path is safe
 * @example
 * isPathSafe('./file.txt', '/app/uploads') // true
 * isPathSafe('../../../etc/passwd', '/app/uploads') // false
 */
export function isPathSafe(filePath, baseDir) {
  if (typeof filePath !== 'string' || typeof baseDir !== 'string') {
    return false;
  }

  // Check for null bytes
  if (filePath.includes('\0')) {
    return false;
  }

  // Check for path traversal patterns
  const dangerous = ['..', '~', '//'];
  if (dangerous.some(pattern => filePath.includes(pattern))) {
    return false;
  }

  return true;
}

/**
 * Rate limiter implementation using token bucket algorithm
 * @class RateLimiter
 * @example
 * const limiter = new RateLimiter({ maxRequests: 100, windowMs: 60000 });
 * if (!limiter.tryConsume('user123')) {
 *   throw new Error('Rate limit exceeded');
 * }
 */
export class RateLimiter {
  /**
   * @param {Object} options - Configuration options
   * @param {number} options.maxRequests - Maximum requests per window
   * @param {number} options.windowMs - Time window in milliseconds
   */
  constructor({ maxRequests = 100, windowMs = 60000 } = {}) {
    this.maxRequests = maxRequests;
    this.windowMs = windowMs;
    this.buckets = new Map();
  }

  /**
   * Try to consume a token for the given key
   * @param {string} key - Unique identifier (e.g., IP address, user ID)
   * @returns {boolean} True if request is allowed
   */
  tryConsume(key) {
    const now = Date.now();
    const bucket = this.buckets.get(key) || { count: 0, resetAt: now + this.windowMs };

    // Reset bucket if window expired
    if (now >= bucket.resetAt) {
      bucket.count = 0;
      bucket.resetAt = now + this.windowMs;
    }

    // Check if limit exceeded
    if (bucket.count >= this.maxRequests) {
      this.buckets.set(key, bucket);
      return false;
    }

    bucket.count++;
    this.buckets.set(key, bucket);
    return true;
  }

  /**
   * Get current usage for a key
   * @param {string} key - Unique identifier
   * @returns {Object} Usage information
   */
  getUsage(key) {
    const bucket = this.buckets.get(key);
    if (!bucket) {
      return { count: 0, remaining: this.maxRequests, resetAt: Date.now() + this.windowMs };
    }

    return {
      count: bucket.count,
      remaining: Math.max(0, this.maxRequests - bucket.count),
      resetAt: bucket.resetAt
    };
  }

  /**
   * Clear rate limit for a key
   * @param {string} key - Unique identifier
   */
  reset(key) {
    this.buckets.delete(key);
  }

  /**
   * Clear all rate limits
   */
  resetAll() {
    this.buckets.clear();
  }
}

/**
 * CSRF token manager
 * @class CSRFTokenManager
 * @example
 * const csrf = new CSRFTokenManager();
 * const token = csrf.generate('session123');
 * const valid = csrf.verify('session123', token);
 */
export class CSRFTokenManager {
  /**
   * @param {Object} options - Configuration options
   * @param {number} options.tokenLength - Length of token in bytes
   * @param {number} options.expiryMs - Token expiry time in milliseconds
   */
  constructor({ tokenLength = 32, expiryMs = 3600000 } = {}) {
    this.tokenLength = tokenLength;
    this.expiryMs = expiryMs;
    this.tokens = new Map();
  }

  /**
   * Generate a new CSRF token
   * @param {string} sessionId - Session identifier
   * @returns {string} CSRF token
   */
  generate(sessionId) {
    const token = crypto.randomBytes(this.tokenLength).toString('base64url');
    const expiresAt = Date.now() + this.expiryMs;

    this.tokens.set(sessionId, { token, expiresAt });

    // Clean up expired tokens periodically
    this.cleanup();

    return token;
  }

  /**
   * Verify a CSRF token
   * @param {string} sessionId - Session identifier
   * @param {string} token - Token to verify
   * @returns {boolean} True if token is valid
   */
  verify(sessionId, token) {
    const stored = this.tokens.get(sessionId);

    if (!stored) {
      return false;
    }

    // Check expiry
    if (Date.now() >= stored.expiresAt) {
      this.tokens.delete(sessionId);
      return false;
    }

    // Constant-time comparison to prevent timing attacks
    return crypto.timingSafeEqual(
      Buffer.from(stored.token),
      Buffer.from(token)
    );
  }

  /**
   * Clean up expired tokens
   */
  cleanup() {
    const now = Date.now();
    for (const [sessionId, data] of this.tokens.entries()) {
      if (now >= data.expiresAt) {
        this.tokens.delete(sessionId);
      }
    }
  }

  /**
   * Revoke a token
   * @param {string} sessionId - Session identifier
   */
  revoke(sessionId) {
    this.tokens.delete(sessionId);
  }
}

/**
 * Generate a cryptographically secure random string
 * @param {number} length - Length in bytes
 * @returns {string} Random string
 * @example
 * const secret = generateSecureRandom(32);
 */
export function generateSecureRandom(length = 32) {
  return crypto.randomBytes(length).toString('base64url');
}

/**
 * Hash a password using PBKDF2
 * @param {string} password - Password to hash
 * @param {string} [salt] - Salt (auto-generated if not provided)
 * @returns {Promise<{hash: string, salt: string}>} Hash and salt
 * @example
 * const { hash, salt } = await hashPassword('myPassword');
 */
export async function hashPassword(password, salt = null) {
  const actualSalt = salt || crypto.randomBytes(16).toString('hex');

  return new Promise((resolve, reject) => {
    crypto.pbkdf2(password, actualSalt, 100000, 64, 'sha512', (err, derivedKey) => {
      if (err) reject(err);
      else resolve({
        hash: derivedKey.toString('hex'),
        salt: actualSalt
      });
    });
  });
}

/**
 * Verify a password against a hash
 * @param {string} password - Password to verify
 * @param {string} hash - Stored hash
 * @param {string} salt - Salt used for hashing
 * @returns {Promise<boolean>} True if password matches
 * @example
 * const valid = await verifyPassword('myPassword', storedHash, storedSalt);
 */
export async function verifyPassword(password, hash, salt) {
  const { hash: computedHash } = await hashPassword(password, salt);
  return crypto.timingSafeEqual(
    Buffer.from(hash, 'hex'),
    Buffer.from(computedHash, 'hex')
  );
}

/**
 * Security headers middleware configuration
 * @returns {Object} Security headers
 * @example
 * const headers = getSecurityHeaders();
 * response.setHeaders(headers);
 */
export function getSecurityHeaders() {
  return {
    // Prevent XSS attacks
    'Content-Security-Policy': "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'",
    'X-Content-Type-Options': 'nosniff',
    'X-Frame-Options': 'DENY',
    'X-XSS-Protection': '1; mode=block',

    // HTTPS enforcement
    'Strict-Transport-Security': 'max-age=31536000; includeSubDomains; preload',

    // Prevent information leakage
    'Referrer-Policy': 'strict-origin-when-cross-origin',
    'Permissions-Policy': 'geolocation=(), microphone=(), camera=()',

    // Additional security
    'X-Permitted-Cross-Domain-Policies': 'none',
    'Cross-Origin-Embedder-Policy': 'require-corp',
    'Cross-Origin-Opener-Policy': 'same-origin',
    'Cross-Origin-Resource-Policy': 'same-origin'
  };
}

/**
 * Validate input against common injection patterns
 * @param {string} input - Input to validate
 * @returns {Object} Validation result
 * @example
 * const { valid, issues } = validateInput(userInput);
 * if (!valid) throw new Error(issues.join(', '));
 */
export function validateInput(input) {
  const issues = [];

  if (typeof input !== 'string') {
    return { valid: false, issues: ['Input must be a string'] };
  }

  // SQL injection patterns
  const sqlPatterns = [
    /(\b(SELECT|INSERT|UPDATE|DELETE|DROP|CREATE|ALTER|EXEC|EXECUTE)\b)/i,
    /(--|;|\/\*|\*\/)/,
    /('|(\\'))/
  ];

  // Command injection patterns
  const cmdPatterns = [
    /[;&|`$()]/,
    /\n|\r/
  ];

  // Check for SQL injection
  if (sqlPatterns.some(pattern => pattern.test(input))) {
    issues.push('Potential SQL injection detected');
  }

  // Check for command injection
  if (cmdPatterns.some(pattern => pattern.test(input))) {
    issues.push('Potential command injection detected');
  }

  // Check for null bytes
  if (input.includes('\0')) {
    issues.push('Null byte detected');
  }

  return {
    valid: issues.length === 0,
    issues
  };
}
