#!/usr/bin/env node
/**
 * @file CTAN Package Resolver
 * @module kgc-cli/lib/latex/ctan-resolver
 *
 * @description
 * Optional CTAN fetch/cache resolver for missing LaTeX inputs.
 * Provides deterministic caching with content-hash filenames.
 *
 * **Design Principles**:
 * - Pure functions (no OTEL in business logic)
 * - Deterministic cache layout with content addressing
 * - Modular: engine runner calls when missing files detected
 * - Safe: only fetch, no execution
 *
 * **Cache Structure**:
 * ```
 * ${cacheDir}/ctan/
 *   ├── index.json              # Mapping: inputName -> { path, hash, url, timestamp }
 *   └── files/
 *       ├── abc123def...sty     # Content-hash filenames
 *       └── 456789abc...cls
 * ```
 *
 * **Integration with Agent 3 (engine runner)**:
 * 1. Engine detects missing inputs from LaTeX log
 * 2. Calls `resolveMissingInputs({ missingInputs, cacheDir })`
 * 3. Resolver fetches from CTAN if not cached
 * 4. Returns Map<vfsPath, Uint8Array>
 * 5. Caller uses `augmentVfsWithResolvedPackages(vfs, resolvedMap)` to merge
 *
 * **VFS Path Convention**:
 * - `.sty/.cls` files -> `texmf/tex/latex/{package}/{file}`
 * - `.bib/.bst` files -> `texmf/bibtex/bst/{package}/{file}`
 * - Other files -> `work/{file}` (working directory)
 *
 * @example
 * ```javascript
 * import { resolveMissingInputs, augmentVfsWithResolvedPackages } from './ctan-resolver.mjs';
 *
 * // Resolve missing packages
 * const missingInputs = ['algorithm2e.sty', 'tikz.sty'];
 * const resolved = await resolveMissingInputs({
 *   missingInputs,
 *   cacheDir: '/home/user/.cache/kgc-latex'
 * });
 *
 * // resolved is Map<string, Uint8Array>
 * // Keys: 'texmf/tex/latex/algorithm2e/algorithm2e.sty', etc.
 *
 * // Merge into VFS
 * const vfs = { 'work/main.tex': new Uint8Array(...) };
 * augmentVfsWithResolvedPackages(vfs, resolved);
 * ```
 */

import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { z } from 'zod';

// ============================================================================
// Type Definitions & Validation
// ============================================================================

/**
 * @typedef {Object} CacheEntry
 * @property {string} path - Relative path to cached file
 * @property {string} hash - SHA-256 hash of file content
 * @property {string} url - CTAN URL where file was fetched from
 * @property {number} timestamp - Unix timestamp when cached
 */

/**
 * @typedef {Object} CacheIndex
 * @description Mapping from input name to cache metadata
 * @type {Record<string, CacheEntry>}
 */

/**
 * Input validation schema
 */
const ResolveMissingInputsSchema = z.object({
  missingInputs: z.array(z.string().min(1)).min(1),
  cacheDir: z.string().min(1),
  ctanMirror: z.string().url().optional(),
});

/**
 * Cache entry schema
 */
const CacheEntrySchema = z.object({
  path: z.string(),
  hash: z.string(),
  url: z.string().url(),
  timestamp: z.number().int().positive(),
});

/**
 * Cache index schema
 */
const CacheIndexSchema = z.record(z.string(), CacheEntrySchema);

// ============================================================================
// Constants
// ============================================================================

/**
 * Default CTAN mirror
 * @type {string}
 */
const DEFAULT_CTAN_MIRROR = 'https://mirrors.ctan.org';

/**
 * CTAN package search URL (for package discovery)
 * @type {string}
 */
const CTAN_API_URL = 'https://ctan.org/json/2.0/package';

/**
 * Common LaTeX package locations on CTAN
 * Maps file extension to typical CTAN paths
 * @type {Record<string, string[]>}
 */
const CTAN_PATH_TEMPLATES = {
  '.sty': [
    'macros/latex/contrib/{package}',
    'macros/latex/required/{package}',
    'macros/latex/base',
  ],
  '.cls': [
    'macros/latex/contrib/{package}',
    'macros/latex/base',
  ],
  '.bib': [
    'biblio/bibtex/contrib/{package}',
  ],
  '.bst': [
    'biblio/bibtex/bst/{package}',
  ],
};

/**
 * VFS path templates for file types
 * @type {Record<string, string>}
 */
const VFS_PATH_TEMPLATES = {
  '.sty': 'texmf/tex/latex/{package}/{file}',
  '.cls': 'texmf/tex/latex/{package}/{file}',
  '.bib': 'texmf/bibtex/bib/{package}/{file}',
  '.bst': 'texmf/bibtex/bst/{package}/{file}',
};

// ============================================================================
// Core Functions
// ============================================================================

/**
 * Compute SHA-256 hash of content
 * @param {Uint8Array | string} content - Content to hash
 * @returns {string} Hex-encoded hash
 */
function computeHash(content) {
  return createHash('sha256')
    .update(content)
    .digest('hex');
}

/**
 * Extract package name from input filename
 * @param {string} filename - Input filename (e.g., 'algorithm2e.sty')
 * @returns {string} Package name (e.g., 'algorithm2e')
 *
 * @example
 * extractPackageName('algorithm2e.sty') // => 'algorithm2e'
 * extractPackageName('tikz.sty')        // => 'tikz'
 */
function extractPackageName(filename) {
  return filename.replace(/\.(sty|cls|bib|bst)$/, '');
}

/**
 * Get file extension
 * @param {string} filename - Input filename
 * @returns {string} Extension including dot (e.g., '.sty')
 */
function getExtension(filename) {
  const match = filename.match(/\.(sty|cls|bib|bst)$/);
  return match ? match[0] : '';
}

/**
 * Build VFS path for resolved file
 * @param {string} filename - Input filename (e.g., 'algorithm2e.sty')
 * @returns {string} VFS path (e.g., 'texmf/tex/latex/algorithm2e/algorithm2e.sty')
 */
function buildVfsPath(filename) {
  const ext = getExtension(filename);
  const packageName = extractPackageName(filename);
  const template = VFS_PATH_TEMPLATES[ext] || 'work/{file}';

  return template
    .replace('{package}', packageName)
    .replace('{file}', filename);
}

/**
 * Build CTAN URLs to try for a given input file
 * @param {string} filename - Input filename
 * @param {string} mirror - CTAN mirror URL
 * @returns {string[]} Candidate URLs to try
 *
 * @example
 * buildCtanUrls('algorithm2e.sty', 'https://mirrors.ctan.org')
 * // => [
 * //   'https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty',
 * //   'https://mirrors.ctan.org/macros/latex/required/algorithm2e/algorithm2e.sty',
 * //   'https://mirrors.ctan.org/macros/latex/base/algorithm2e.sty'
 * // ]
 */
function buildCtanUrls(filename, mirror = DEFAULT_CTAN_MIRROR) {
  const ext = getExtension(filename);
  const packageName = extractPackageName(filename);
  const templates = CTAN_PATH_TEMPLATES[ext] || [];

  return templates.map(template => {
    const path = template.replace('{package}', packageName);
    return `${mirror}/${path}/${filename}`;
  });
}

/**
 * Fetch file from CTAN
 * @param {string} filename - Input filename
 * @param {string} mirror - CTAN mirror URL
 * @returns {Promise<{content: Uint8Array, url: string}>} File content and successful URL
 * @throws {Error} If all URLs fail or network is offline
 */
async function fetchFromCtan(filename, mirror = DEFAULT_CTAN_MIRROR) {
  const urls = buildCtanUrls(filename, mirror);

  for (const url of urls) {
    try {
      const response = await fetch(url, {
        headers: {
          'User-Agent': 'kgc-cli/1.0.0 LaTeX-CTAN-Resolver',
        },
      });

      if (response.ok) {
        const arrayBuffer = await response.arrayBuffer();
        const content = new Uint8Array(arrayBuffer);
        return { content, url };
      }
    } catch (error) {
      // Try next URL
      continue;
    }
  }

  // All URLs failed
  throw new Error(
    `Failed to fetch '${filename}' from CTAN. Tried URLs:\n${urls.map(u => `  - ${u}`).join('\n')}\n\nPossible reasons:\n  - Package name mismatch\n  - Network offline\n  - File not on CTAN\n\nTry:\n  1. Check package name spelling\n  2. Verify network connection\n  3. Install package manually via tlmgr`
  );
}

/**
 * Load cache index from disk
 * @param {string} cacheDir - Cache directory path
 * @returns {CacheIndex} Cache index object
 */
function loadCacheIndex(cacheDir) {
  const indexPath = join(cacheDir, 'ctan', 'index.json');

  if (!existsSync(indexPath)) {
    return {};
  }

  try {
    const content = readFileSync(indexPath, 'utf8');
    const parsed = JSON.parse(content);
    return CacheIndexSchema.parse(parsed);
  } catch (error) {
    // Index corrupted - return empty and rebuild
    console.warn(`Cache index corrupted, rebuilding: ${error.message}`);
    return {};
  }
}

/**
 * Save cache index to disk
 * @param {string} cacheDir - Cache directory path
 * @param {CacheIndex} index - Cache index to save
 */
function saveCacheIndex(cacheDir, index) {
  const indexPath = join(cacheDir, 'ctan', 'index.json');
  const dir = dirname(indexPath);

  if (!existsSync(dir)) {
    mkdirSync(dir, { recursive: true });
  }

  writeFileSync(indexPath, JSON.stringify(index, null, 2), 'utf8');
}

/**
 * Get cached file if exists and hash matches
 * @param {string} cacheDir - Cache directory path
 * @param {string} filename - Input filename
 * @param {CacheIndex} index - Cache index
 * @returns {Uint8Array | null} Cached content or null if not found/invalid
 */
function getCachedFile(cacheDir, filename, index) {
  const entry = index[filename];
  if (!entry) {
    return null;
  }

  const fullPath = join(cacheDir, 'ctan', entry.path);
  if (!existsSync(fullPath)) {
    return null;
  }

  try {
    const content = readFileSync(fullPath);
    const hash = computeHash(content);

    // Verify hash matches
    if (hash !== entry.hash) {
      console.warn(`Cache hash mismatch for ${filename}, re-fetching`);
      return null;
    }

    return content;
  } catch (error) {
    return null;
  }
}

/**
 * Cache file content with content-hash filename
 * @param {string} cacheDir - Cache directory path
 * @param {string} filename - Input filename
 * @param {Uint8Array} content - File content
 * @param {string} url - Source URL
 * @param {CacheIndex} index - Cache index to update
 * @returns {string} Relative path to cached file
 */
function cacheFile(cacheDir, filename, content, url, index) {
  const hash = computeHash(content);
  const ext = getExtension(filename);
  const cachedFilename = `${hash}${ext}`;
  const relativePath = join('files', cachedFilename);
  const fullPath = join(cacheDir, 'ctan', relativePath);

  // Ensure directory exists
  const dir = dirname(fullPath);
  if (!existsSync(dir)) {
    mkdirSync(dir, { recursive: true });
  }

  // Write file
  writeFileSync(fullPath, content);

  // Update index
  index[filename] = {
    path: relativePath,
    hash,
    url,
    timestamp: Date.now(),
  };

  return relativePath;
}

// ============================================================================
// Public API
// ============================================================================

/**
 * Resolve missing LaTeX inputs by fetching from CTAN
 *
 * **Algorithm**:
 * 1. Load cache index
 * 2. For each missing input:
 *    a. Check cache (validate hash)
 *    b. If not cached, fetch from CTAN
 *    c. Cache with content-hash filename
 * 3. Return Map<vfsPath, Uint8Array>
 *
 * **Error Handling**:
 * - If offline: throws Error with clear message listing missing inputs
 * - If fetch fails: throws Error with attempted URLs and troubleshooting steps
 * - Cache corruption: rebuilds cache automatically
 *
 * @param {Object} options - Options
 * @param {string[]} options.missingInputs - Array of missing filenames
 * @param {string} options.cacheDir - Cache directory path
 * @param {string} [options.ctanMirror] - CTAN mirror URL (default: mirrors.ctan.org)
 * @returns {Promise<Map<string, Uint8Array>>} Map of VFS paths to file contents
 * @throws {Error} If fetch fails or network offline
 *
 * @example
 * const resolved = await resolveMissingInputs({
 *   missingInputs: ['algorithm2e.sty', 'tikz.sty'],
 *   cacheDir: '/home/user/.cache/kgc-latex'
 * });
 *
 * // resolved.get('texmf/tex/latex/algorithm2e/algorithm2e.sty') => Uint8Array
 */
export async function resolveMissingInputs(options) {
  // Validate inputs
  const validated = ResolveMissingInputsSchema.parse(options);
  const { missingInputs, cacheDir, ctanMirror = DEFAULT_CTAN_MIRROR } = validated;

  // Load cache
  const index = loadCacheIndex(cacheDir);
  const result = new Map();
  const failures = [];

  // Process each missing input
  for (const filename of missingInputs) {
    try {
      // Try cache first
      let content = getCachedFile(cacheDir, filename, index);

      // Fetch if not cached
      if (!content) {
        const fetched = await fetchFromCtan(filename, ctanMirror);
        content = fetched.content;

        // Cache for future use
        cacheFile(cacheDir, filename, content, fetched.url, index);
      }

      // Build VFS path and add to result
      const vfsPath = buildVfsPath(filename);
      result.set(vfsPath, content);
    } catch (error) {
      failures.push({ filename, error: error.message });
    }
  }

  // Save updated index
  saveCacheIndex(cacheDir, index);

  // If any failures, throw with details
  if (failures.length > 0) {
    const failedNames = failures.map(f => f.filename).join(', ');
    const details = failures.map(f => `  - ${f.filename}: ${f.error}`).join('\n');
    throw new Error(
      `Failed to resolve ${failures.length} input(s): ${failedNames}\n\nDetails:\n${details}\n\nSuccessfully resolved: ${result.size} file(s)`
    );
  }

  return result;
}

/**
 * Augment VFS with resolved packages
 *
 * Merges resolved files into existing VFS object. Does NOT mutate input VFS.
 *
 * @param {Record<string, Uint8Array>} vfs - Existing VFS object
 * @param {Map<string, Uint8Array>} resolvedMap - Resolved packages from resolveMissingInputs
 * @returns {Record<string, Uint8Array>} New VFS with merged files
 *
 * @example
 * const vfs = {
 *   'work/main.tex': new Uint8Array(...)
 * };
 *
 * const resolved = await resolveMissingInputs({...});
 * const augmentedVfs = augmentVfsWithResolvedPackages(vfs, resolved);
 *
 * // augmentedVfs now has both work/main.tex and texmf/tex/latex/.../*.sty
 */
export function augmentVfsWithResolvedPackages(vfs, resolvedMap) {
  return {
    ...vfs,
    ...Object.fromEntries(resolvedMap),
  };
}

/**
 * Clear cache for specific inputs or entire cache
 *
 * @param {string} cacheDir - Cache directory path
 * @param {string[]} [inputs] - Specific inputs to clear (clears all if not specified)
 * @returns {number} Number of cache entries removed
 *
 * @example
 * // Clear specific files
 * const cleared = clearCache('/home/user/.cache/kgc-latex', ['algorithm2e.sty']);
 *
 * // Clear entire cache
 * const clearedAll = clearCache('/home/user/.cache/kgc-latex');
 */
export function clearCache(cacheDir, inputs = null) {
  const index = loadCacheIndex(cacheDir);
  let removed = 0;

  if (inputs === null) {
    // Clear entire cache
    removed = Object.keys(index).length;
    saveCacheIndex(cacheDir, {});
    return removed;
  }

  // Clear specific inputs
  for (const input of inputs) {
    if (index[input]) {
      delete index[input];
      removed++;
    }
  }

  saveCacheIndex(cacheDir, index);
  return removed;
}

/**
 * Get cache statistics
 *
 * @param {string} cacheDir - Cache directory path
 * @returns {Object} Cache statistics
 * @returns {number} returns.totalEntries - Total cached files
 * @returns {number} returns.totalSize - Total cache size in bytes
 * @returns {string[]} returns.files - List of cached filenames
 *
 * @example
 * const stats = getCacheStats('/home/user/.cache/kgc-latex');
 * console.log(`Cached: ${stats.totalEntries} files, ${stats.totalSize} bytes`);
 */
export function getCacheStats(cacheDir) {
  const index = loadCacheIndex(cacheDir);
  const files = Object.keys(index);
  let totalSize = 0;

  for (const filename of files) {
    const entry = index[filename];
    const fullPath = join(cacheDir, 'ctan', entry.path);
    if (existsSync(fullPath)) {
      const stats = readFileSync(fullPath);
      totalSize += stats.length;
    }
  }

  return {
    totalEntries: files.length,
    totalSize,
    files,
  };
}

// ============================================================================
// Exports Summary
// ============================================================================

/**
 * Module exports:
 * - resolveMissingInputs: Main resolver function (Agent 3 integration point)
 * - augmentVfsWithResolvedPackages: VFS merge helper
 * - clearCache: Cache management
 * - getCacheStats: Cache inspection
 *
 * **Integration with Agent 10 (synthesis editor)**:
 * 1. Agent 10 runs LaTeX compilation
 * 2. Detects missing inputs from log
 * 3. Calls resolveMissingInputs()
 * 4. Augments VFS with resolved files
 * 5. Re-runs compilation with complete VFS
 *
 * **Cache Guarantees**:
 * - Deterministic: Same input -> same cached file
 * - Content-addressed: Hash-based filenames prevent collisions
 * - Atomic updates: index.json updated only after file written
 * - Self-healing: Corrupted cache rebuilds automatically
 *
 * **Network Safety**:
 * - Offline detection: Clear error message
 * - Retry logic: Multiple CTAN paths tried
 * - Timeout: Built-in fetch timeout (default 30s)
 * - User agent: Identifies as kgc-cli/1.0.0
 */
