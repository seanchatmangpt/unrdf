/**
 * @file Path Traversal Validation
 * @module @unrdf/daemon/security/path-validation
 * @description Prevention of path traversal attacks
 */

import crypto from 'crypto';
import { addAuditEvent } from './audit-utils.mjs';

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

      addAuditEvent(event);

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
 * Sanitize file path (convenience wrapper)
 * @param {string} path - Path to sanitize
 * @returns {string} Sanitized path or throws on unsafe path
 * @throws {Error} If path contains traversal attacks
 */
export function sanitizePath(path) {
  const result = validatePathSafety(path);
  if (!result.safe) {
    throw new Error(`Unsafe path detected: ${result.reason}`);
  }
  return path;
}
