/**
 * @file Security Audit and Hardening Module
 * @module @unrdf/daemon/security
 * @description Comprehensive security validation, threat detection, and audit logging.
 * Prevents injection attacks, path traversal, timing attacks, and provides cryptographic verification.
 */

import crypto from 'crypto';

/**
 * Security event audit log
 * @private
 */
const auditLog = [];

/**
 * Rate limiter state tracking
 * @private
 */
const rateLimiters = new Map();

/**
 * Injection pattern detection for command injection attacks
 * @private
 */
const INJECTION_PATTERNS = {
  command: [
    /[;&|`$(){}[\]<>\\]/,
    /\b(cat|rm|exec|eval|spawn|fork)\b/i,
    /(\$\(|\`|&&|\|\|)/,
  ],
  sql: [
    /(\b(union|select|insert|update|delete|drop|create|alter)\b)/i,
    /(['\"].*?(or|and).*?['\"])/i,
    /(--|\*\/|\/\*)/,
  ],
  rdf: [
    /<\s*script/i,
    /javascript:/i,
    /on\w+\s*=/i,
    /BIND\s*\(\s*CONCAT/i,
    /FILTER\s*\(/i,
  ],
};

/**
 * Path traversal patterns
 * @private
 */
const PATH_TRAVERSAL_PATTERNS = [
  /\.\.\//,
  /\.\.\\/,
  /\.\.%2f/i,
  /\.\.%5c/i,
  /%2e%2e%2f/i,
  /%2e%2e%5c/i,
];

/**
 * Validates input against injection attack patterns
 * @param {*} input - Input to validate
 * @param {string} type - Input type: 'command', 'sql', 'rdf'
 * @returns {Object} Validation result
 */
export function validateInputSafety(input, type = 'command') {
  const eventId = crypto.randomUUID();
  const timestamp = Date.now();

  if (input === null || input === undefined) {
    return { safe: true, eventId };
  }

  const stringInput = typeof input === 'string' ? input : JSON.stringify(input);

  const patterns = INJECTION_PATTERNS[type] || INJECTION_PATTERNS.command;
  for (const pattern of patterns) {
    if (pattern.test(stringInput)) {
      const event = {
        eventId,
        timestamp,
        eventType: 'injection_attempt',
        severity: 'critical',
        source: type,
        message: `Injection attack detected: ${type}`,
        details: { pattern: pattern.toString(), inputLength: stringInput.length },
      };

      auditLog.push(event);

      return {
        safe: false,
        eventId,
        reason: `Malicious ${type} injection pattern detected`,
      };
    }
  }

  return { safe: true, eventId };
}

/**
 * Prevents path traversal attacks
 * @param {string} path - Path to validate
 * @returns {Object} Validation result
 */
export function validatePathSafety(path) {
  const eventId = crypto.randomUUID();
  const timestamp = Date.now();

  if (typeof path !== 'string') {
    return { safe: false, eventId, reason: 'Path must be a string' };
  }

  // Normalize and check for traversal
  const normalized = path.replace(/\\/g, '/');

  for (const pattern of PATH_TRAVERSAL_PATTERNS) {
    if (pattern.test(normalized)) {
      const event = {
        eventId,
        timestamp,
        eventType: 'path_traversal',
        severity: 'critical',
        source: 'path',
        message: 'Path traversal attack detected',
        details: { pattern: pattern.toString(), pathLength: path.length },
      };

      auditLog.push(event);

      return {
        safe: false,
        eventId,
        reason: 'Path traversal attack detected',
      };
    }
  }

  return { safe: true, eventId };
}

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

    auditLog.push(event);
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

    auditLog.push(event);
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

    auditLog.push(event);

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

      auditLog.push(event);

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

      auditLog.push(event);

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

  auditLog.push(event);

  return {
    valid: true,
    eventId,
    validationResults,
  };
}

/**
 * Retrieves the audit log
 * @param {Object} options - Filter options
 * @param {string} [options.severity] - Filter by severity
 * @param {string} [options.eventType] - Filter by event type
 * @param {number} [options.limit] - Limit results
 * @returns {Array} Audit log entries
 */
export function getAuditLog(options = {}) {
  let filtered = [...auditLog];

  if (options.severity) {
    filtered = filtered.filter(e => e.severity === options.severity);
  }

  if (options.eventType) {
    filtered = filtered.filter(e => e.eventType === options.eventType);
  }

  if (options.limit) {
    filtered = filtered.slice(-options.limit);
  }

  return filtered;
}

/**
 * Clears the audit log
 * @returns {number} Number of entries cleared
 */
export function clearAuditLog() {
  const count = auditLog.length;
  auditLog.length = 0;
  return count;
}

/**
 * Gets security statistics
 * @returns {Object} Security statistics
 */
export function getSecurityStats() {
  const byType = {};
  const bySeverity = {};

  auditLog.forEach(event => {
    byType[event.eventType] = (byType[event.eventType] || 0) + 1;
    bySeverity[event.severity] = (bySeverity[event.severity] || 0) + 1;
  });

  return {
    totalEvents: auditLog.length,
    byType,
    bySeverity,
    activeRateLimiters: rateLimiters.size,
    timestamp: Date.now(),
  };
}
