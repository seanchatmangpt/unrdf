/**
 * @file Error Message Sanitizer
 * @module error-sanitizer
 *
 * @description
 * Sanitizes error messages to prevent information disclosure vulnerabilities.
 * Removes sensitive data like passwords, file paths, stack traces, and environment variables.
 */

import { z } from 'zod';

/**
 * Schema for sanitization options
 */
const SanitizationOptionsSchema = z.object({
  removeStackTraces: z.boolean().default(true),
  removeFilePaths: z.boolean().default(true),
  removeCredentials: z.boolean().default(true),
  removeEnvironmentVars: z.boolean().default(true),
  genericErrorMessage: z.string().default('An error occurred'),
}).strict();

/**
 * Patterns to detect and sanitize sensitive information
 */
const SENSITIVE_PATTERNS = {
  // File paths (absolute paths)
  filePaths: [
    /\/[a-zA-Z0-9_\-./]+\.js(?::\d+:\d+)?/g, // Unix paths with line numbers
    /\\[a-zA-Z0-9_\-.\\]+\.js(?::\d+:\d+)?/g, // Windows paths
    /\/app\/[^\s]*/g, // Docker app paths
    /\/usr\/[^\s]*/g, // System paths
    /\/etc\/[^\s]*/g,
    /\/var\/[^\s]*/g,
    /\/home\/[^\s]*/g,
    /C:\\[^\s]*/g, // Windows paths
    /D:\\[^\s]*/g,
  ],

  // Database connection strings
  credentials: [
    /(?:postgres|mysql|mongodb):\/\/[^:]+:[^@]+@[^\s]+/gi, // DB URLs with passwords
    /password\s*[:=]\s*["']?[^"'\s]+["']?/gi, // password= or password:
    /api[_-]?key\s*[:=]\s*["']?[^"'\s]+["']?/gi, // API keys
    /secret\s*[:=]\s*["']?[^"'\s]+["']?/gi, // secrets
    /token\s*[:=]\s*["']?[^"'\s]+["']?/gi, // tokens
    /authorization\s*:\s*["']?[^"'\s]+["']?/gi, // auth headers
  ],

  // Environment variables
  environmentVars: [
    /DATABASE_URL\s*=\s*[^\s]+/gi,
    /API_KEY\s*=\s*[^\s]+/gi,
    /SECRET\s*=\s*[^\s]+/gi,
    /PASSWORD\s*=\s*[^\s]+/gi,
    /TOKEN\s*=\s*[^\s]+/gi,
    /\w+_KEY\s*=\s*[^\s]+/gi,
    /\w+_SECRET\s*=\s*[^\s]+/gi,
  ],

  // Stack trace patterns
  stackTraces: [
    /at\s+[^\s]+\s+\([^)]+:\d+:\d+\)/g, // at Function (file:line:col)
    /at\s+[^(]+\([^)]+\)/g, // at Function(...)
    /^\s*at\s.+$/gm, // Full stack trace lines
  ],
};

/**
 * Error Sanitizer for preventing information disclosure
 */
export class ErrorSanitizer {
  /**
   * @param {Object} [options] - Sanitization options
   */
  constructor(options = {}) {
    const validated = SanitizationOptionsSchema.parse(options);
    this.options = validated;
  }

  /**
   * Sanitize an error message
   * @param {Error | string} error - Error to sanitize
   * @returns {string} Sanitized error message
   */
  sanitize(error) {
    let message = error instanceof Error ? error.message : String(error);

    // Remove credentials
    if (this.options.removeCredentials) {
      message = this._removeCredentials(message);
    }

    // Remove file paths
    if (this.options.removeFilePaths) {
      message = this._removeFilePaths(message);
    }

    // Remove environment variables
    if (this.options.removeEnvironmentVars) {
      message = this._removeEnvironmentVars(message);
    }

    // If sanitization removed too much, return generic message
    if (message.trim().length < 10) {
      return this.options.genericErrorMessage;
    }

    return message;
  }

  /**
   * Sanitize a complete error object
   * @param {Error} error - Error object
   * @returns {Object} Sanitized error object
   */
  sanitizeError(error) {
    if (!error || typeof error !== 'object') {
      return {
        message: this.options.genericErrorMessage,
        sanitized: true,
      };
    }

    const sanitized = {
      message: this.sanitize(error.message || 'Unknown error'),
      sanitized: true,
    };

    // Only include stack if not removing stack traces
    if (!this.options.removeStackTraces && error.stack) {
      sanitized.stack = this._sanitizeStack(error.stack);
    }

    return sanitized;
  }

  /**
   * Remove credential patterns from text
   * @param {string} text - Text to sanitize
   * @returns {string} Sanitized text
   * @private
   */
  _removeCredentials(text) {
    let sanitized = text;

    for (const pattern of SENSITIVE_PATTERNS.credentials) {
      sanitized = sanitized.replace(pattern, (match) => {
        if (match.includes('://')) {
          // Replace password in connection string
          return match.replace(/:\/\/[^:]+:[^@]+@/, '://***:***@');
        }
        // Replace entire credential assignment
        return match.split(/[:=]/)[0] + '=***';
      });
    }

    return sanitized;
  }

  /**
   * Remove file path patterns from text
   * @param {string} text - Text to sanitize
   * @returns {string} Sanitized text
   * @private
   */
  _removeFilePaths(text) {
    let sanitized = text;

    for (const pattern of SENSITIVE_PATTERNS.filePaths) {
      sanitized = sanitized.replace(pattern, '[file path removed]');
    }

    return sanitized;
  }

  /**
   * Remove environment variable patterns from text
   * @param {string} text - Text to sanitize
   * @returns {string} Sanitized text
   * @private
   */
  _removeEnvironmentVars(text) {
    let sanitized = text;

    for (const pattern of SENSITIVE_PATTERNS.environmentVars) {
      sanitized = sanitized.replace(pattern, (match) => {
        return match.split('=')[0] + '=***';
      });
    }

    return sanitized;
  }

  /**
   * Sanitize stack trace
   * @param {string} stack - Stack trace
   * @returns {string} Sanitized stack trace
   * @private
   */
  _sanitizeStack(stack) {
    let sanitized = stack;

    // Remove file paths from stack trace
    for (const pattern of SENSITIVE_PATTERNS.filePaths) {
      sanitized = sanitized.replace(pattern, '[sanitized]');
    }

    // Remove full stack trace lines if configured
    if (this.options.removeStackTraces) {
      return 'Stack trace removed for security';
    }

    return sanitized;
  }

  /**
   * Check if text contains sensitive information
   * @param {string} text - Text to check
   * @returns {boolean} True if sensitive info detected
   */
  containsSensitiveInfo(text) {
    for (const patterns of Object.values(SENSITIVE_PATTERNS)) {
      for (const pattern of patterns) {
        if (pattern.test(text)) {
          return true;
        }
      }
    }
    return false;
  }
}

/**
 * Create an error sanitizer instance
 * @param {Object} [options] - Sanitization options
 * @returns {ErrorSanitizer} Sanitizer instance
 */
export function createErrorSanitizer(options = {}) {
  return new ErrorSanitizer(options);
}

/**
 * Default error sanitizer instance (strict mode)
 */
export const defaultErrorSanitizer = new ErrorSanitizer();

/**
 * Sanitize an error message (convenience function)
 * @param {Error | string} error - Error to sanitize
 * @returns {string} Sanitized error message
 */
export function sanitizeError(error) {
  return defaultErrorSanitizer.sanitize(error);
}
