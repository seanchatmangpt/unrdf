/**
 * @file Path Traversal Validator
 * @module path-validator
 *
 * @description
 * Validates file paths to prevent directory traversal and unauthorized file access attacks.
 */

import { z } from 'zod';
import { resolve, normalize, isAbsolute as _isAbsolute } from 'path';
import { fileURLToPath } from 'url';

/**
 * Schema for path validation options
 */
const PathValidationOptionsSchema = z
  .object({
    basePath: z.string().optional(),
    allowAbsolutePaths: z.boolean().default(false),
    allowedDirectories: z.array(z.string()).default([]),
    blockedDirectories: z
      .array(z.string())
      .default([
        '/etc',
        '/usr',
        '/bin',
        '/sbin',
        '/var',
        '/root',
        'C:\\Windows',
        'C:\\System32',
        'D:\\Windows',
      ]),
  })
  .strict();

/**
 * Path Validator for preventing traversal attacks
 */
export class PathValidator {
  /**
   * @param {Object} [options] - Validation options
   */
  constructor(options = {}) {
    const validated = PathValidationOptionsSchema.parse(options);
    this.basePath = validated.basePath || process.cwd();
    this.allowAbsolutePaths = validated.allowAbsolutePaths;
    this.allowedDirectories = validated.allowedDirectories;
    this.blockedDirectories = validated.blockedDirectories;
  }

  /**
   * Validate a file URI for path traversal attacks
   * @param {string} uri - File URI to validate
   * @returns {Object} Validation result { valid, violations, sanitizedPath }
   */
  validateFileUri(uri) {
    const violations = [];

    if (!uri || typeof uri !== 'string') {
      return {
        valid: false,
        violations: ['Invalid URI: must be a non-empty string'],
        sanitizedPath: null,
      };
    }

    try {
      // Convert file:// URI to path
      let filePath = uri;
      if (uri.startsWith('file://')) {
        filePath = fileURLToPath(uri);
      } else if (uri.includes('://')) {
        violations.push('Only file:// URIs are supported');
        return { valid: false, violations, sanitizedPath: null };
      }

      // Decode URI encoding (including double encoding)
      let decodedPath = filePath;
      let previousPath = '';
      // Decode up to 3 times to catch double/triple encoding
      for (let i = 0; i < 3 && decodedPath !== previousPath; i++) {
        previousPath = decodedPath;
        decodedPath = decodeURIComponent(decodedPath);
      }

      // Check for null byte injection
      if (decodedPath.includes('\x00') || decodedPath.includes('%00')) {
        violations.push('Null byte injection detected');
        return { valid: false, violations, sanitizedPath: null };
      }

      // Check for path traversal patterns
      if (this._hasTraversalPattern(decodedPath)) {
        violations.push('Path traversal pattern detected');
        return { valid: false, violations, sanitizedPath: null };
      }

      // Normalize and resolve path
      const normalizedPath = normalize(decodedPath);
      const resolvedPath = resolve(this.basePath, normalizedPath);

      // Check if resolved path is within base path
      if (!this.allowAbsolutePaths && !resolvedPath.startsWith(resolve(this.basePath))) {
        violations.push('Path escapes base directory');
        return { valid: false, violations, sanitizedPath: null };
      }

      // Check against blocked directories
      for (const blockedDir of this.blockedDirectories) {
        if (resolvedPath.startsWith(blockedDir)) {
          violations.push(`Access to blocked directory: ${blockedDir}`);
          return { valid: false, violations, sanitizedPath: null };
        }
      }

      // If allowed directories are specified, check inclusion
      if (this.allowedDirectories.length > 0) {
        const isAllowed = this.allowedDirectories.some(allowedDir =>
          resolvedPath.startsWith(resolve(allowedDir))
        );
        if (!isAllowed) {
          violations.push('Path not in allowed directories');
          return { valid: false, violations, sanitizedPath: null };
        }
      }

      return {
        valid: true,
        violations: [],
        sanitizedPath: resolvedPath,
      };
    } catch (error) {
      violations.push(`Path validation error: ${error.message}`);
      return { valid: false, violations, sanitizedPath: null };
    }
  }

  /**
   * Check for path traversal patterns
   * @param {string} path - Path to check
   * @returns {boolean} True if traversal pattern detected
   * @private
   */
  _hasTraversalPattern(path) {
    const traversalPatterns = [
      /\.\.\//g, // ../
      /\.\.\\/g, // ..\
      /\.\.%2f/gi, // ..%2f (URL encoded)
      /\.\.%5c/gi, // ..%5c (URL encoded)
      /\.\.%252f/gi, // ..%252f (double URL encoded)
      /\.\.%255c/gi, // ..%255c (double URL encoded)
      /\.\.%c0%af/gi, // Unicode bypass
      /\.\.%c1%9c/gi, // Unicode bypass
    ];

    return traversalPatterns.some(pattern => pattern.test(path));
  }

  /**
   * Sanitize a file path (best effort - returns null if unsafe)
   * @param {string} path - Path to sanitize
   * @returns {string | null} Sanitized path or null if unsafe
   */
  sanitizePath(path) {
    const validation = this.validateFileUri(path);
    return validation.valid ? validation.sanitizedPath : null;
  }
}

/**
 * Create a path validator instance
 * @param {Object} [options] - Validation options
 * @returns {PathValidator} Validator instance
 */
export function createPathValidator(options = {}) {
  return new PathValidator(options);
}

/**
 * Default path validator instance
 */
export const defaultPathValidator = new PathValidator();

/**
 * Validate a file URI (convenience function)
 * @param {string} uri - URI to validate
 * @param {string} [basePath] - Base path for validation
 * @returns {Object} Validation result
 */
export function validatePath(uri, basePath) {
  const validator = basePath ? new PathValidator({ basePath }) : defaultPathValidator;
  return validator.validateFileUri(uri);
}
