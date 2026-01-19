/**
 * @file Credential and Secret Detection
 * @module @unrdf/daemon/security/credential-detection
 * @description Detection of API keys, passwords, and other secrets
 */

import crypto from 'crypto';
import { addAuditEvent } from './audit-utils.mjs';

/**
 * Secret patterns for detection
 * @private
 */
const SECRET_PATTERNS = [
  /api[_-]?key[_-]?[=:]\s*['"]?([a-zA-Z0-9_-]{20,})['"]?/gi,
  /access[_-]?token[_-]?[=:]\s*['"]?([a-zA-Z0-9_-]{20,})['"]?/gi,
  /secret[_-]?key[_-]?[=:]\s*['"]?([a-zA-Z0-9_-]{20,})['"]?/gi,
  /password[_-]?[=:]\s*['"]?([^\s'"]{8,})['"]?/gi,
  /aws[_-]?access[_-]?key[_-]?id[_-]?[=:]\s*['"]?(AKIA[0-9A-Z]{16})['"]?/gi,
  /aws[_-]?secret[_-]?access[_-]?key[_-]?[=:]\s*['"]?([a-zA-Z0-9/+=]{40})['"]?/gi,
  /private[_-]?key[_-]?[=:]/gi,
  /-----BEGIN\s+(RSA\s+)?PRIVATE\s+KEY-----/gi,
];

/**
 * Detect potential secrets in strings
 * @param {string} input - Input string to scan
 * @returns {Object} Detection result with matches
 */
export function detectSecrets(input) {
  const eventId = crypto.randomUUID();
  const timestamp = Date.now();
  const detected = [];

  if (typeof input !== 'string') {
    return { detected: false, eventId, matches: [] };
  }

  for (const pattern of SECRET_PATTERNS) {
    const matches = input.matchAll(pattern);
    for (const match of matches) {
      detected.push({
        type: 'secret',
        pattern: pattern.toString(),
        position: match.index,
      });
    }
  }

  if (detected.length > 0) {
    const event = {
      eventId,
      timestamp,
      eventType: 'secret_detected',
      severity: 'critical',
      source: 'secret_detection',
      message: `Potential secrets detected in input`,
      details: { matchCount: detected.length },
    };
    addAuditEvent(event);
  }

  return {
    detected: detected.length > 0,
    eventId,
    matches: detected,
  };
}
