/**
 * @fileoverview Path normalization for VFS
 *
 * Re-exports path normalization utilities from parent module.
 * Provides POSIX path normalization, Windows path handling, and
 * deterministic path sorting for VFS operations.
 *
 * @module @unrdf/kgc-cli/lib/latex/vfs/normalize
 */

export {
  normalizeToVFS,
  vfsToRelative,
  isValidVFSPath,
  sortVFSPaths,
} from '../path-normalize.mjs';

import { sep } from 'node:path';

/**
 * Normalize path to POSIX format (forward slashes)
 *
 * Converts Windows-style paths (backslashes) to POSIX-style (forward slashes).
 * Removes leading './' and ensures no leading slash.
 *
 * @param {string} path - Path to normalize
 * @returns {string} Normalized path
 *
 * @example
 * normalizePath('foo\\bar\\baz.tex')
 * // => 'foo/bar/baz.tex'
 *
 * normalizePath('./main.tex')
 * // => 'main.tex'
 *
 * normalizePath('/absolute/path.tex')
 * // => 'absolute/path.tex'
 */
export function normalizePath(path) {
  if (typeof path !== 'string') {
    throw new TypeError('Path must be a string');
  }

  // Convert all backslashes to forward slashes (handles both single and escaped)
  let normalized = path.replace(/\\/g, '/');

  // Remove leading './'
  normalized = normalized.replace(/^\.\//, '');

  // Remove leading '/'
  normalized = normalized.replace(/^\/+/, '');

  // Remove duplicate slashes
  normalized = normalized.replace(/\/+/g, '/');

  // Remove trailing slash
  normalized = normalized.replace(/\/$/, '');

  return normalized;
}

/**
 * Check if path is relative (no leading slash, no drive letter)
 *
 * @param {string} path - Path to check
 * @returns {boolean} True if path is relative
 *
 * @example
 * isRelativePath('foo/bar.tex')  // => true
 * isRelativePath('/foo/bar.tex')  // => false
 * isRelativePath('C:\\foo\\bar.tex')  // => false
 */
export function isRelativePath(path) {
  if (typeof path !== 'string') return false;

  // Check for leading slash
  if (path.startsWith('/')) return false;

  // Check for Windows drive letter
  if (/^[a-zA-Z]:/.test(path)) return false;

  return true;
}

/**
 * Validate path for VFS usage
 *
 * Ensures path is safe for use in VFS (no path traversal, valid format).
 *
 * @param {string} path - Path to validate
 * @returns {boolean} True if valid for VFS
 *
 * @example
 * isValidVfsPath('foo/bar.tex')  // => true
 * isValidVfsPath('../etc/passwd')  // => false
 * isValidVfsPath('foo//bar.tex')  // => false
 */
export function isValidVfsPath(path) {
  if (typeof path !== 'string') return false;
  if (path.length === 0) return false;

  // No path traversal
  if (path.includes('..')) return false;

  // No double slashes
  if (path.includes('//')) return false;

  // No backslashes (must be normalized)
  if (path.includes('\\')) return false;

  // No leading slash
  if (path.startsWith('/')) return false;

  return true;
}
