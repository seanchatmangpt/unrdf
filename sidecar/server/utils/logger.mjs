/**
 * @file Logger Utility
 * @module sidecar/server/utils/logger
 *
 * Structured logging with Winston and sensitive data redaction
 */

import winston from 'winston';

/**
 * List of sensitive field names that should be redacted from logs
 * @constant {string[]}
 */
const SENSITIVE_FIELDS = [
  'apiKey', 'password', 'privateKey', 'secret', 'token',
  'authorization', 'cookie', 'sessionId', 'ssn', 'creditCard',
  'accessToken', 'refreshToken', 'bearer', 'auth', 'key'
];

/**
 * Sanitize sensitive data for logging
 * Recursively redacts sensitive fields from objects
 *
 * @param {any} data - Data to sanitize (object, array, or primitive)
 * @returns {any} Sanitized data with sensitive fields redacted
 */
export function sanitizeForLogging(data) {
  if (data === null || data === undefined) {
    return data;
  }

  // Handle arrays
  if (Array.isArray(data)) {
    return data.map(item => sanitizeForLogging(item));
  }

  // Handle objects
  if (typeof data === 'object') {
    const redacted = { ...data };

    for (const key in redacted) {
      // Check if key is sensitive (case-insensitive)
      const isSensitive = SENSITIVE_FIELDS.some(
        field => key.toLowerCase().includes(field.toLowerCase())
      );

      if (isSensitive) {
        redacted[key] = '***REDACTED***';
      } else if (typeof redacted[key] === 'object') {
        // Recursively sanitize nested objects
        redacted[key] = sanitizeForLogging(redacted[key]);
      }
    }

    return redacted;
  }

  // Return primitives as-is
  return data;
}

/**
 * Custom Winston format for redacting sensitive data
 */
const redactionFormat = winston.format((info) => {
  // Redact sensitive fields from the log info object
  return sanitizeForLogging(info);
})();

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.errors({ stack: true }),
    redactionFormat, // Apply redaction before JSON formatting
    winston.format.json()
  ),
  defaultMeta: { service: 'unrdf-sidecar' },
  transports: [
    new winston.transports.Console({
      format: winston.format.combine(
        winston.format.colorize(),
        winston.format.simple()
      )
    })
  ]
});

export default logger;
