/**
 * @file Security Audit and Hardening Module
 * @module @unrdf/daemon/security
 * @description Comprehensive security validation, threat detection, and audit logging.
 * Prevents injection attacks, path traversal, timing attacks, and provides cryptographic verification.
 */

import crypto from 'crypto';
import { validateInputSafety, detectInjection } from './utils/injection-detection.mjs';
import { validatePathSafety, sanitizePath } from './utils/path-validation.mjs';
import { detectSecrets } from './utils/credential-detection.mjs';
import {
  getAuditLog,
  clearAuditLog,
  getSecurityStats as getSecurityStatsBase,
  addAuditEvent,
} from './utils/audit-utils.mjs';

// Re-export for backward compatibility
export { validateInputSafety, detectInjection } from './utils/injection-detection.mjs';
export { validatePathSafety, sanitizePath } from './utils/path-validation.mjs';
export { detectSecrets } from './utils/credential-detection.mjs';
export { getAuditLog, clearAuditLog } from './utils/audit-utils.mjs';

/**
 * Rate limiter state tracking
 * @private
 */
const rateLimiters = new Map();

/**
 * Timing-safe string comparison to prevent timing attacks
 * @param {string} a - First string to compare
 * @param {string} b - Second string to compare
 * @returns {boolean} Whether strings are equal (constant-time)
 */
export function timingSafeCompare(a, b) {
  if (typeof a !== 'string' || typeof b !== 'string') {
    return false;
  }

  // Ensure equal length comparison
  const aBuffer = Buffer.from(a, 'utf8');
  const bBuffer = Buffer.from(b, 'utf8');

  try {
    return crypto.timingSafeEqual(aBuffer, bBuffer);
  } catch {
    return false;
  }
}

/**
 * Rate limiting with sliding window algorithm
 * @param {string} identifier - Rate limit identifier (IP, user ID, etc)
 * @param {number} maxRequests - Maximum requests allowed
 * @param {number} windowMs - Time window in milliseconds
 * @returns {Object} Rate limit status
 */
export function checkRateLimit(identifier, maxRequests = 100, windowMs = 60000) {
  const eventId = crypto.randomUUID();
  const now = Date.now();

  if (!rateLimiters.has(identifier)) {
    rateLimiters.set(identifier, []);
  }

  const timestamps = rateLimiters.get(identifier);

  // Remove old entries outside the window
  const validTimestamps = timestamps.filter(t => now - t < windowMs);
  rateLimiters.set(identifier, validTimestamps);

  const isLimited = validTimestamps.length >= maxRequests;

  if (isLimited) {
    const event = {
      eventId,
      timestamp: now,
      eventType: 'rate_limit',
      severity: 'warning',
      source: identifier,
      message: 'Rate limit exceeded',
      details: { requests: validTimestamps.length, maxRequests, windowMs },
    };

    addAuditEvent(event);
  }

  validTimestamps.push(now);
  rateLimiters.set(identifier, validTimestamps);

  return {
    allowed: !isLimited,
    eventId,
    remaining: Math.max(0, maxRequests - validTimestamps.length),
    resetAfter: validTimestamps.length > 0 ? validTimestamps[0] + windowMs : now + windowMs,
  };
}

/**
 * Verifies cryptographic integrity using BLAKE3 (via SHA-256 as fallback)
 * @param {Buffer|string} data - Data to hash
 * @param {string} expectedHash - Expected hash value
 * @returns {Object} Verification result
 */
export function verifyCryptographicHash(data, expectedHash) {
  const eventId = crypto.randomUUID();
  const timestamp = Date.now();

  const buffer = typeof data === 'string' ? Buffer.from(data, 'utf8') : data;

  // Use SHA-256 as BLAKE3 substitute (Node.js doesn't have native BLAKE3)
  const hash = crypto.createHash('sha256').update(buffer).digest('hex');
  const verified = timingSafeCompare(hash, expectedHash);

  if (!verified) {
    const event = {
      eventId,
      timestamp,
      eventType: 'crypto_verify',
      severity: 'critical',
      source: 'crypto',
      message: 'Cryptographic verification failed',
      details: { hashLength: hash.length, dataSize: buffer.length },
    };

    addAuditEvent(event);
  }

  return {
    verified,
    eventId,
    hash,
    algorithm: 'SHA-256',
  };
}

/**
 * Comprehensive payload validation combining all security checks
 * @param {*} payload - Payload to validate
 * @param {Object} options - Validation options
 * @param {string} [options.type] - Input type for injection detection
 * @param {boolean} [options.checkPath] - Whether to check for path traversal
 * @param {string} [options.rateLimitId] - Rate limiter identifier
 * @returns {Object} Comprehensive validation result
 */
export function validatePayload(payload, options = {}) {
  const eventId = crypto.randomUUID();
  const timestamp = Date.now();
  const validationResults = {};

  // Input safety check
  const safetyCheck = validateInputSafety(payload, options.type);
  validationResults.injection = safetyCheck;

  if (!safetyCheck.safe) {
    const event = {
      eventId,
      timestamp,
      eventType: 'validation',
      severity: 'critical',
      source: 'payload_validation',
      message: 'Payload validation failed: injection detected',
      details: validationResults,
    };

    addAuditEvent(event);

    return {
      valid: false,
      eventId,
      reason: safetyCheck.reason,
      validationResults,
    };
  }

  // Path traversal check if requested
  if (options.checkPath && typeof payload === 'string') {
    const pathCheck = validatePathSafety(payload);
    validationResults.pathTraversal = pathCheck;

    if (!pathCheck.safe) {
      const event = {
        eventId,
        timestamp,
        eventType: 'validation',
        severity: 'critical',
        source: 'payload_validation',
        message: 'Payload validation failed: path traversal detected',
        details: validationResults,
      };

      addAuditEvent(event);

      return {
        valid: false,
        eventId,
        reason: pathCheck.reason,
        validationResults,
      };
    }
  }

  // Rate limiting check if identifier provided
  if (options.rateLimitId) {
    const rateLimit = checkRateLimit(
      options.rateLimitId,
      options.maxRequests || 100,
      options.windowMs || 60000
    );
    validationResults.rateLimit = rateLimit;

    if (!rateLimit.allowed) {
      const event = {
        eventId,
        timestamp,
        eventType: 'validation',
        severity: 'warning',
        source: 'payload_validation',
        message: 'Payload validation failed: rate limit exceeded',
        details: validationResults,
      };

      addAuditEvent(event);

      return {
        valid: false,
        eventId,
        reason: 'Rate limit exceeded',
        validationResults,
      };
    }
  }

  const event = {
    eventId,
    timestamp,
    eventType: 'validation',
    severity: 'info',
    source: 'payload_validation',
    message: 'Payload validation passed',
    details: validationResults,
  };

  addAuditEvent(event);

  return {
    valid: true,
    eventId,
    validationResults,
  };
}

/**
 * Gets security statistics
 * @returns {Object} Security statistics
 */
export function getSecurityStats() {
  return getSecurityStatsBase(rateLimiters.size);
}

/**
 * Sanitize error messages to remove sensitive information
 * @param {Error} error - Error to sanitize
 * @returns {Error} Sanitized error
 */
export function sanitizeError(error) {
  if (!(error instanceof Error)) {
    return error;
  }

  const sensitivePatterns = [
    /api[_-]?key[_-]?[=:]\s*['"]?[a-zA-Z0-9_-]+['"]?/gi,
    /password[_-]?[=:]\s*['"]?[^\s'"]+['"]?/gi,
    /token[_-]?[=:]\s*['"]?[a-zA-Z0-9_-]+['"]?/gi,
    /\/home\/[^\/\s]+/gi,
    /\/users\/[^\/\s]+/gi,
  ];

  let sanitized = error.message;
  for (const pattern of sensitivePatterns) {
    sanitized = sanitized.replace(pattern, '[REDACTED]');
  }

  const sanitizedError = new Error(sanitized);
  sanitizedError.code = error.code;
  sanitizedError.stack = undefined; // Remove stack trace in production
  return sanitizedError;
}
