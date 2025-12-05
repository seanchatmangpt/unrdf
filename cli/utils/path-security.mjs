/**
 * @file Path security validation
 * @module cli/utils/path-security
 *
 * Prevents path traversal attacks and symlink vulnerabilities
 * FM-CLI-010: File path security gap
 */

import { resolve, normalize } from 'node:path';
import { realpath } from 'node:fs/promises';

/**
 * Validate file path for security issues
 *
 * @param {string} filePath - Path to validate
 * @param {string} basePath - Base directory (optional, defaults to cwd)
 * @returns {Promise<Object>} Validation result
 *
 * @example
 * const result = await validatePathSecurity('../../etc/passwd');
 * if (!result.valid) {
 *   throw new Error(result.error);
 * }
 */
export async function validatePathSecurity(filePath, basePath = process.cwd()) {
  try {
    // Normalize the path
    const normalizedPath = normalize(filePath);

    // Check for directory traversal attempts
    if (normalizedPath.includes('..')) {
      return {
        valid: false,
        error: 'Path contains directory traversal (..) - security violation',
        suggestion: 'Use an absolute or relative path within the current directory'
      };
    }

    // Resolve to absolute path
    const absolutePath = resolve(basePath, normalizedPath);
    const absoluteBase = resolve(basePath);

    // Check if resolved path is within base directory
    if (!absolutePath.startsWith(absoluteBase)) {
      return {
        valid: false,
        error: 'Path resolves outside base directory - security violation',
        suggestion: 'Use a path within the current directory or its subdirectories'
      };
    }

    // Check for symlink traversal (if enabled)
    try {
      const realPath = await realpath(absolutePath);
      if (!realPath.startsWith(absoluteBase)) {
        return {
          valid: false,
          error: 'Symlink resolves outside base directory - security violation',
          suggestion: 'Symlinks to external directories are not allowed'
        };
      }
    } catch (error) {
      // File may not exist yet, which is OK
      if (error.code !== 'ENOENT') {
        return {
          valid: false,
          error: `Cannot validate symlink: ${error.message}`,
          suggestion: 'Check file permissions'
        };
      }
    }

    return {
      valid: true,
      path: absolutePath,
      normalized: normalizedPath
    };
  } catch (error) {
    return {
      valid: false,
      error: `Path validation error: ${error.message}`,
      suggestion: 'Check the path format'
    };
  }
}

/**
 * Sanitize file path for safe display
 */
export function sanitizePathForDisplay(filePath) {
  const normalized = normalize(filePath);
  return normalized.replace(/\\/g, '/');
}

export default {
  validatePathSecurity,
  sanitizePathForDisplay
};
