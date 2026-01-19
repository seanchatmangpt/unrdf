/**
 * @file Audit Log Utilities
 * @module @unrdf/daemon/security/audit-utils
 * @description Audit log management and string extraction utilities for security validation
 */

/**
 * Security event audit log
 * @private
 */
const auditLog = [];

/**
 * Extracts all string values from an object/array recursively
 * @param {*} value - Value to extract strings from
 * @param {Set} visited - Set of visited objects to prevent circular refs
 * @returns {string[]} Array of string values
 */
export function extractStringValues(value, visited = new Set()) {
  if (value === null || value === undefined) {
    return [];
  }

  // Prevent circular reference infinite loops
  if (typeof value === 'object' && visited.has(value)) {
    return [];
  }

  if (typeof value === 'string') {
    return [value];
  }

  if (typeof value === 'bigint' || typeof value === 'number' || typeof value === 'boolean') {
    // Convert to string for validation
    return [String(value)];
  }

  if (Array.isArray(value)) {
    visited.add(value);
    const strings = [];
    for (const item of value) {
      strings.push(...extractStringValues(item, visited));
    }
    return strings;
  }

  if (typeof value === 'object') {
    visited.add(value);
    const strings = [];
    for (const val of Object.values(value)) {
      strings.push(...extractStringValues(val, visited));
    }
    return strings;
  }

  return [];
}

/**
 * Adds an event to the audit log
 * @param {Object} event - Audit event
 */
export function addAuditEvent(event) {
  auditLog.push(event);
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
 * @param {number} activeRateLimiters - Number of active rate limiters
 * @returns {Object} Security statistics
 */
export function getSecurityStats(activeRateLimiters = 0) {
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
    activeRateLimiters,
    timestamp: Date.now(),
  };
}
