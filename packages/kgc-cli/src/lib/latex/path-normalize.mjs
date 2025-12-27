/**
 * @fileoverview Path normalization utilities for VFS
 *
 * Normalizes filesystem paths to VFS paths with consistent forward-slash
 * separators and work/ prefix for container compatibility.
 *
 * @module @unrdf/kgc-cli/lib/latex/path-normalize
 */

import { relative, normalize, sep } from 'node:path';

/**
 * Normalize a filesystem path to a VFS path
 *
 * @param {string} absolutePath - Absolute filesystem path
 * @param {string} projectRoot - Project root directory
 * @returns {string} Normalized VFS path (e.g., "work/main.tex")
 *
 * @example
 * normalizeToVFS('/home/user/project/main.tex', '/home/user/project')
 * // => 'work/main.tex'
 *
 * normalizeToVFS('/home/user/project/packages/foo.tex', '/home/user/project')
 * // => 'work/packages/foo.tex'
 */
export function normalizeToVFS(absolutePath, projectRoot) {
  // Get relative path from project root
  const relPath = relative(projectRoot, absolutePath);

  // Normalize and convert to forward slashes
  const normalized = normalize(relPath);
  const vfsPath = normalized.split(sep).join('/');

  // Ensure no leading slash and prefix with work/
  const cleaned = vfsPath.replace(/^\/+/, '');

  return `work/${cleaned}`;
}

/**
 * Convert VFS path back to relative project path
 *
 * @param {string} vfsPath - VFS path (e.g., "work/main.tex")
 * @returns {string} Relative project path (e.g., "main.tex")
 *
 * @example
 * vfsToRelative('work/main.tex')
 * // => 'main.tex'
 */
export function vfsToRelative(vfsPath) {
  return vfsPath.replace(/^work\//, '');
}

/**
 * Validate VFS path format
 *
 * @param {string} vfsPath - Path to validate
 * @returns {boolean} True if valid VFS path
 */
export function isValidVFSPath(vfsPath) {
  if (typeof vfsPath !== 'string') return false;
  if (!vfsPath.startsWith('work/')) return false;
  if (vfsPath.includes('\\')) return false;
  if (vfsPath.includes('..')) return false;
  if (vfsPath.includes('//')) return false;

  return true;
}

/**
 * Sort VFS paths deterministically
 *
 * Ensures consistent ordering:
 * 1. Depth-first (fewer slashes first)
 * 2. Alphabetical within same depth
 *
 * @param {string[]} paths - Array of VFS paths
 * @returns {string[]} Sorted paths
 */
export function sortVFSPaths(paths) {
  return [...paths].sort((a, b) => {
    const depthA = (a.match(/\//g) || []).length;
    const depthB = (b.match(/\//g) || []).length;

    if (depthA !== depthB) {
      return depthA - depthB;
    }

    return a.localeCompare(b);
  });
}
