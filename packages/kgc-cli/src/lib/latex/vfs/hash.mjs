/**
 * @fileoverview Deterministic VFS hashing utilities
 *
 * Provides SHA256 hashing for VFS content with guaranteed stable ordering
 * to ensure identical inputs always produce identical hashes.
 *
 * @module @unrdf/kgc-cli/lib/latex/vfs/hash
 */

import { createHash } from 'node:crypto';
import { sortVFSPaths } from '../path-normalize.mjs';

/**
 * Hash a single file's content using SHA256
 *
 * @param {Uint8Array} content - File content as binary data
 * @returns {string} SHA256 hash as hexadecimal string
 *
 * @example
 * const hash = hashFile(new Uint8Array([1, 2, 3]));
 * // => 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3'
 */
export function hashFile(content) {
  if (!(content instanceof Uint8Array)) {
    throw new TypeError('Content must be a Uint8Array');
  }

  return createHash('sha256')
    .update(content)
    .digest('hex');
}

/**
 * Hash entire VFS with deterministic ordering
 *
 * Creates a single hash representing the entire VFS state. The hash is
 * stable across runs because:
 * 1. Paths are sorted alphabetically
 * 2. Each entry is hashed as: path-length + path + content-length + content
 * 3. Individual hashes are combined in sorted order
 *
 * This ensures that identical VFS contents always produce identical hashes,
 * regardless of Map iteration order.
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map (path -> binary content)
 * @returns {string} SHA256 hash of entire VFS as hexadecimal string
 *
 * @example
 * const vfs = new Map([
 *   ['work/main.tex', new Uint8Array([...])],
 *   ['work/preamble.tex', new Uint8Array([...])]
 * ]);
 * const hash = hashVfs(vfs);
 * // => 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
 */
export function hashVfs(vfs) {
  if (!(vfs instanceof Map)) {
    throw new TypeError('VFS must be a Map');
  }

  if (vfs.size === 0) {
    // Return hash of empty string for empty VFS
    return createHash('sha256').update('').digest('hex');
  }

  // Sort paths for deterministic ordering
  const sortedPaths = sortVFSPaths([...vfs.keys()]);

  // Create a combined hash by hashing entries in sorted order
  const hash = createHash('sha256');

  for (const path of sortedPaths) {
    const content = vfs.get(path);

    if (!content) {
      throw new Error(`VFS path missing content: ${path}`);
    }

    // Hash format: path-length + path + content-length + content
    // This ensures path and content boundaries are unambiguous
    const pathBytes = Buffer.from(path, 'utf8');
    const pathLength = Buffer.allocUnsafe(4);
    pathLength.writeUInt32BE(pathBytes.length, 0);

    const contentLength = Buffer.allocUnsafe(4);
    contentLength.writeUInt32BE(content.byteLength, 0);

    hash.update(pathLength);
    hash.update(pathBytes);
    hash.update(contentLength);
    hash.update(content);
  }

  return hash.digest('hex');
}

/**
 * Hash VFS subset by file extensions
 *
 * Creates a hash of only files with specified extensions. Useful for
 * tracking changes to specific file types (e.g., only .tex files).
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map
 * @param {string[]} extensions - Extensions to include (e.g., ['.tex', '.sty'])
 * @returns {string} SHA256 hash of filtered VFS
 *
 * @example
 * const texHash = hashVfsByExtension(vfs, ['.tex']);
 * // Only includes .tex files in hash
 */
export function hashVfsByExtension(vfs, extensions) {
  const filtered = new Map();
  const normalizedExts = extensions.map(ext => ext.toLowerCase());

  for (const [path, content] of vfs.entries()) {
    const ext = path.slice(path.lastIndexOf('.')).toLowerCase();
    if (normalizedExts.includes(ext)) {
      filtered.set(path, content);
    }
  }

  return hashVfs(filtered);
}

/**
 * Compare two VFS instances for equality using hashes
 *
 * @param {Map<string, Uint8Array>} vfs1 - First VFS
 * @param {Map<string, Uint8Array>} vfs2 - Second VFS
 * @returns {boolean} True if VFS instances have identical hashes
 *
 * @example
 * const same = areVfsEqual(vfs1, vfs2);
 * // => true if identical content and paths
 */
export function areVfsEqual(vfs1, vfs2) {
  return hashVfs(vfs1) === hashVfs(vfs2);
}

/**
 * Get VFS hash metadata
 *
 * Returns hash along with metadata about what was hashed.
 *
 * @param {Map<string, Uint8Array>} vfs - VFS map
 * @returns {Object} Hash metadata
 * @property {string} hash - SHA256 hash
 * @property {number} fileCount - Number of files
 * @property {number} totalBytes - Total bytes
 * @property {string[]} paths - Sorted paths
 *
 * @example
 * const metadata = getVfsHashMetadata(vfs);
 * // => {
 * //   hash: 'e3b0c44...',
 * //   fileCount: 10,
 * //   totalBytes: 52480,
 * //   paths: ['work/main.tex', ...]
 * // }
 */
export function getVfsHashMetadata(vfs) {
  const sortedPaths = sortVFSPaths([...vfs.keys()]);
  let totalBytes = 0;

  for (const content of vfs.values()) {
    totalBytes += content.byteLength;
  }

  return {
    hash: hashVfs(vfs),
    fileCount: vfs.size,
    totalBytes,
    paths: sortedPaths,
  };
}
