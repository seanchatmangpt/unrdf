/**
 * @file Injection Attack Detection
 * @module @unrdf/daemon/security/injection-detection
 * @description Detection of command, SQL, and RDF injection attacks
 */

import crypto from 'crypto';
import { extractStringValues, addAuditEvent } from './audit-utils.mjs';

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

  // For non-string inputs, extract all string values and validate each
  // This avoids false positives from JSON structural characters
  const stringsToValidate = typeof input === 'string'
    ? [input]
    : extractStringValues(input);

  const patterns = INJECTION_PATTERNS[type] || INJECTION_PATTERNS.command;

  for (const stringInput of stringsToValidate) {
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

        addAuditEvent(event);

        return {
          safe: false,
          eventId,
          reason: `Malicious ${type} injection pattern detected`,
        };
      }
    }
  }

  return { safe: true, eventId };
}

/**
 * Detect injection attacks (convenience wrapper)
 * @param {*} input - Input to validate
 * @param {string} type - Input type
 * @returns {Object} Detection result
 */
export function detectInjection(input, type = 'command') {
  const result = validateInputSafety(input, type);
  return {
    detected: !result.safe,
    type,
    reason: result.reason,
    eventId: result.eventId,
  };
}
