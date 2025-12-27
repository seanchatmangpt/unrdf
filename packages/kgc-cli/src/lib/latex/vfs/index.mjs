/**
 * @fileoverview VFS (Virtual File System) utilities for LaTeX compilation
 *
 * Provides deterministic file packing, path normalization, and hashing
 * for reproducible LaTeX builds. All operations guarantee stable ordering
 * to ensure identical inputs produce identical outputs.
 *
 * @module @unrdf/kgc-cli/lib/latex/vfs
 */

// Hash utilities
export {
  hashFile,
  hashVfs,
  hashVfsByExtension,
  areVfsEqual,
  getVfsHashMetadata,
} from './hash.mjs';

// Path normalization
export {
  normalizeToVFS,
  vfsToRelative,
  isValidVFSPath,
  sortVFSPaths,
  normalizePath,
  isRelativePath,
  isValidVfsPath,
} from './normalize.mjs';

// File packing
export {
  packDirectory,
  packDirectoryClean,
  collectProjectFiles,
  listProjectFilesSorted,
  getVFSStats,
  filterVFSByExtension,
} from './pack.mjs';

/**
 * Create empty VFS
 *
 * @returns {Map<string, Uint8Array>} Empty VFS map
 *
 * @example
 * const vfs = createVfs();
 * vfs.set('work/main.tex', new Uint8Array([...]));
 */
export function createVfs() {
  return new Map();
}

/**
 * Clone VFS to a new Map instance
 *
 * @param {Map<string, Uint8Array>} vfs - VFS to clone
 * @returns {Map<string, Uint8Array>} Cloned VFS
 *
 * @example
 * const vfs2 = cloneVfs(vfs1);
 * // vfs2 is a separate Map with same contents
 */
export function cloneVfs(vfs) {
  return new Map(vfs);
}

/**
 * Merge multiple VFS instances
 *
 * Later maps override earlier ones in case of path conflicts.
 *
 * @param {...Map<string, Uint8Array>} vfsList - VFS maps to merge
 * @returns {Map<string, Uint8Array>} Merged VFS
 *
 * @example
 * const merged = mergeVfs(baseVfs, customVfs);
 * // customVfs overrides baseVfs for conflicting paths
 */
export function mergeVfs(...vfsList) {
  const merged = new Map();

  for (const vfs of vfsList) {
    for (const [path, content] of vfs.entries()) {
      merged.set(path, content);
    }
  }

  return merged;
}

/**
 * Get VFS entry as UTF-8 string
 *
 * Useful for reading text files like .tex sources.
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map
 * @param {string} path - VFS path
 * @returns {string|null} File content as string, or null if not found
 *
 * @example
 * const source = getVfsText(vfs, 'work/main.tex');
 * // => '\\documentclass{article}...'
 */
export function getVfsText(vfs, path) {
  const content = vfs.get(path);
  if (!content) return null;

  return Buffer.from(content).toString('utf8');
}

/**
 * Set VFS entry from UTF-8 string
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map
 * @param {string} path - VFS path
 * @param {string} text - Text content
 *
 * @example
 * setVfsText(vfs, 'work/main.tex', '\\documentclass{article}...');
 */
export function setVfsText(vfs, path, text) {
  const content = Buffer.from(text, 'utf8');
  vfs.set(path, new Uint8Array(content));
}
