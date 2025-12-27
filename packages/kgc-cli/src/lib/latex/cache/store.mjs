/**
 * @fileoverview Local cache store for LaTeX packages and files.
 *
 * Manages on-disk cache of downloaded packages, fonts, etc.
 * Provides content-addressable storage with hash-based verification.
 *
 * Cache structure:
 *   .latex-cache/
 *     packages/
 *       hyperref/hyperref.sty
 *       beamer/beamer.cls
 *     fonts/
 *       texgyre/texgyrepagella-regular.otf
 *
 * @module lib/latex/cache/store
 */

import { readFile, writeFile, mkdir, readdir, stat } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { join, dirname, relative } from 'node:path';

/**
 * @typedef {Object} CacheEntry
 * @property {string} name - File name
 * @property {string} path - Absolute path to cached file
 * @property {string} relativePath - Path relative to cache root
 * @property {string} hash - SHA-256 hash
 * @property {number} size - File size in bytes
 */

/**
 * Get cached file by name and optional hash.
 *
 * If hash provided, verifies content matches before returning.
 *
 * @param {string} cacheDir - Cache directory (absolute path)
 * @param {string} name - File name to retrieve
 * @param {string} [expectedHash] - Expected SHA-256 hash (optional)
 * @returns {Promise<Uint8Array|null>} File content or null if not found/invalid
 */
export async function getCached(cacheDir, name, expectedHash) {
  try {
    // Search for file in cache (could be in packages/ or fonts/)
    const searchDirs = [
      join(cacheDir, 'packages'),
      join(cacheDir, 'fonts')
    ];

    for (const searchDir of searchDirs) {
      const found = await findFileRecursive(searchDir, name);
      if (found) {
        const content = await readFile(found);

        // Verify hash if provided
        if (expectedHash) {
          const actualHash = hashContent(content);
          if (actualHash !== expectedHash) {
            console.warn(`Hash mismatch for ${name}: expected ${expectedHash}, got ${actualHash}`);
            return null;
          }
        }

        return content;
      }
    }

    return null; // Not found
  } catch (err) {
    console.warn(`Error reading cached file ${name}:`, err.message);
    return null;
  }
}

/**
 * Store file in cache.
 *
 * Creates subdirectories as needed.
 * Returns the relative path where file was stored.
 *
 * @param {string} cacheDir - Cache directory (absolute path)
 * @param {string} name - File name
 * @param {string} hash - SHA-256 hash of content
 * @param {Uint8Array|Buffer|string} content - File content
 * @param {Object} [options] - Storage options
 * @param {string} [options.type='packages'] - Cache subdirectory (packages, fonts)
 * @param {string} [options.packageName] - Package name for nested storage
 * @returns {Promise<string>} Relative path where file was stored
 */
export async function setCached(cacheDir, name, hash, content, options = {}) {
  const { type = 'packages', packageName } = options;

  // Determine storage path
  let relativePath;
  if (packageName) {
    relativePath = join(type, packageName, name);
  } else {
    // Use first component of filename as package name (e.g., hyperref.sty -> hyperref/)
    const baseName = name.replace(/\.[^.]+$/, '');
    relativePath = join(type, baseName, name);
  }

  const absolutePath = join(cacheDir, relativePath);

  // Create parent directories
  await mkdir(dirname(absolutePath), { recursive: true });

  // Verify hash before writing
  const actualHash = hashContent(content);
  if (actualHash !== hash) {
    throw new Error(
      `Hash mismatch when caching ${name}: expected ${hash}, got ${actualHash}`
    );
  }

  // Write file
  await writeFile(absolutePath, content);

  return relativePath;
}

/**
 * List all cached files.
 *
 * Returns metadata for all files in cache.
 *
 * @param {string} cacheDir - Cache directory (absolute path)
 * @returns {Promise<CacheEntry[]>} Array of cache entries
 */
export async function listCached(cacheDir) {
  const entries = [];

  try {
    const searchDirs = [
      join(cacheDir, 'packages'),
      join(cacheDir, 'fonts')
    ];

    for (const searchDir of searchDirs) {
      await walkDirectory(searchDir, async (filePath) => {
        const content = await readFile(filePath);
        const stats = await stat(filePath);
        const hash = hashContent(content);
        const relativePath = relative(cacheDir, filePath);
        const name = filePath.split('/').pop();

        entries.push({
          name,
          path: filePath,
          relativePath,
          hash,
          size: stats.size
        });
      });
    }
  } catch (err) {
    if (err.code !== 'ENOENT') {
      throw err;
    }
    // Cache directory doesn't exist - return empty array
  }

  return entries;
}

/**
 * Get cache statistics.
 *
 * @param {string} cacheDir - Cache directory (absolute path)
 * @returns {Promise<{fileCount: number, totalSize: number}>}
 */
export async function getCacheStats(cacheDir) {
  const entries = await listCached(cacheDir);

  return {
    fileCount: entries.length,
    totalSize: entries.reduce((sum, entry) => sum + entry.size, 0)
  };
}

/**
 * Clear cache directory.
 *
 * Removes all cached files but preserves directory structure.
 *
 * @param {string} cacheDir - Cache directory (absolute path)
 * @returns {Promise<number>} Number of files removed
 */
export async function clearCache(cacheDir) {
  const entries = await listCached(cacheDir);

  // Remove all files (use Promise.all for parallel operations)
  await Promise.all(
    entries.map(entry =>
      import('node:fs/promises').then(fs => fs.unlink(entry.path))
    )
  );

  return entries.length;
}

/**
 * Find file by name in directory (recursive).
 *
 * @private
 * @param {string} dir - Directory to search
 * @param {string} targetName - File name to find
 * @returns {Promise<string|null>} Absolute path if found, null otherwise
 */
async function findFileRecursive(dir, targetName) {
  try {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory()) {
        const found = await findFileRecursive(fullPath, targetName);
        if (found) return found;
      } else if (entry.name === targetName) {
        return fullPath;
      }
    }

    return null;
  } catch (err) {
    if (err.code === 'ENOENT') {
      return null;
    }
    throw err;
  }
}

/**
 * Walk directory recursively, calling callback for each file.
 *
 * @private
 * @param {string} dir - Directory to walk
 * @param {Function} callback - Async function called with file path
 * @returns {Promise<void>}
 */
async function walkDirectory(dir, callback) {
  try {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory()) {
        await walkDirectory(fullPath, callback);
      } else {
        await callback(fullPath);
      }
    }
  } catch (err) {
    if (err.code === 'ENOENT') {
      return; // Directory doesn't exist
    }
    throw err;
  }
}

/**
 * Compute SHA-256 hash of content.
 *
 * @private
 * @param {Uint8Array|Buffer|string} content - Content to hash
 * @returns {string} Hex-encoded SHA-256 hash
 */
function hashContent(content) {
  const hash = createHash('sha256');
  hash.update(content);
  return hash.digest('hex');
}

/**
 * Verify that cached file matches expected hash.
 *
 * @param {string} cacheDir - Cache directory (absolute path)
 * @param {string} relativePath - Relative path to cached file
 * @param {string} expectedHash - Expected SHA-256 hash
 * @returns {Promise<boolean>} True if hash matches, false otherwise
 */
export async function verifyCached(cacheDir, relativePath, expectedHash) {
  try {
    const absolutePath = join(cacheDir, relativePath);
    const content = await readFile(absolutePath);
    const actualHash = hashContent(content);
    return actualHash === expectedHash;
  } catch (err) {
    return false;
  }
}
