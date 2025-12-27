/**
 * @file LaTeX Package Resolver with Retry Logic
 * @module kgc-cli/lib/latex/cache/resolve
 *
 * @description
 * Enhanced CTAN resolver with:
 * - Exponential backoff retry logic
 * - Local fixture server support (for testing)
 * - Lockfile integration for version pinning
 * - Content-addressed caching
 * - Clear error diagnostics
 *
 * **Integration Points**:
 * - Called by compile.mjs when engine reports missing inputs
 * - Works with Agent 5's lockfile for version pinning
 * - Errors parsed by Agent 6's diagnostics
 */

import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { z } from 'zod';
import {
  buildCtanUrls,
  buildVfsPath,
  extractPackageName,
  isLocalFixture,
  DEFAULT_CTAN_MIRROR,
} from './ctan-map.mjs';

// ============================================================================
// TYPE DEFINITIONS & VALIDATION
// ============================================================================

/**
 * @typedef {Object} CacheEntry
 * @property {string} path - Relative path to cached file
 * @property {string} hash - SHA-256 hash of file content
 * @property {string} url - Source URL where file was fetched
 * @property {number} timestamp - Unix timestamp when cached
 */

/**
 * @typedef {Record<string, CacheEntry>} CacheIndex
 */

/**
 * @typedef {Object} ResolveOptions
 * @property {string[]} missingInputs - List of missing files
 * @property {string} cacheDir - Local cache directory
 * @property {string} [registry] - Package registry URL (default: CTAN mirror)
 * @property {Object} [lockfile] - Existing lockfile for pinned versions
 * @property {number} [maxRetries] - Maximum retry attempts (default: 3)
 * @property {number} [initialDelay] - Initial retry delay in ms (default: 100)
 */

/**
 * Input validation schema
 */
const ResolveOptionsSchema = z.object({
  missingInputs: z.array(z.string().min(1)).min(1),
  cacheDir: z.string().min(1),
  registry: z.string().url().optional(),
  lockfile: z.record(z.any()).optional(),
  maxRetries: z.number().int().min(0).max(10).optional(),
  initialDelay: z.number().int().min(10).max(5000).optional(),
});

const CacheEntrySchema = z.object({
  path: z.string(),
  hash: z.string(),
  url: z.string().url(),
  timestamp: z.number().int().positive(),
});

const CacheIndexSchema = z.record(z.string(), CacheEntrySchema);

// ============================================================================
// CONSTANTS
// ============================================================================

/** Default maximum retry attempts */
const DEFAULT_MAX_RETRIES = 3;

/** Default initial retry delay (ms) */
const DEFAULT_INITIAL_DELAY = 100;

/** Maximum retry delay (ms) - cap exponential growth */
const MAX_RETRY_DELAY = 5000;

/** Fetch timeout (ms) */
const FETCH_TIMEOUT = 10000;

// ============================================================================
// CACHE UTILITIES
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
 * @returns {Uint8Array | null} Cached content or null
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
  const ext = filename.match(/\.[^.]+$/)?.[0] || '';
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
// RETRY LOGIC WITH EXPONENTIAL BACKOFF
// ============================================================================

/**
 * Sleep for specified milliseconds
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Calculate exponential backoff delay
 * @param {number} attempt - Current attempt number (0-indexed)
 * @param {number} initialDelay - Initial delay in ms
 * @returns {number} Delay in ms (capped at MAX_RETRY_DELAY)
 */
function calculateBackoffDelay(attempt, initialDelay = DEFAULT_INITIAL_DELAY) {
  const exponentialDelay = initialDelay * Math.pow(2, attempt);
  return Math.min(exponentialDelay, MAX_RETRY_DELAY);
}

/**
 * Fetch with timeout
 * @param {string} url - URL to fetch
 * @param {number} timeout - Timeout in ms
 * @returns {Promise<Response>} Fetch response
 * @throws {Error} On timeout or network error
 */
async function fetchWithTimeout(url, timeout = FETCH_TIMEOUT) {
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      signal: controller.signal,
      headers: {
        'User-Agent': 'kgc-cli/1.0.0 LaTeX-Resolver',
      },
    });
    clearTimeout(timeoutId);
    return response;
  } catch (error) {
    clearTimeout(timeoutId);
    if (error.name === 'AbortError') {
      throw new Error(`Fetch timeout after ${timeout}ms: ${url}`);
    }
    throw error;
  }
}

/**
 * Fetch single URL with retry logic
 * @param {string} url - URL to fetch
 * @param {Object} opts - Retry options
 * @param {number} opts.maxRetries - Maximum retry attempts
 * @param {number} opts.initialDelay - Initial delay in ms
 * @returns {Promise<Uint8Array>} File content
 * @throws {Error} If all retries exhausted
 */
async function fetchWithRetry(url, { maxRetries = DEFAULT_MAX_RETRIES, initialDelay = DEFAULT_INITIAL_DELAY }) {
  let lastError;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      // Local fixture: read from file system or local server (no retry)
      if (isLocalFixture(url)) {
        const response = await fetch(url);
        if (response.ok) {
          const arrayBuffer = await response.arrayBuffer();
          return new Uint8Array(arrayBuffer);
        }
        throw new Error(`Local fixture fetch failed: ${response.status} ${response.statusText}`);
      }

      // Network fetch with timeout
      const response = await fetchWithTimeout(url, FETCH_TIMEOUT);

      if (response.ok) {
        const arrayBuffer = await response.arrayBuffer();
        return new Uint8Array(arrayBuffer);
      }

      // Non-2xx response - don't retry
      throw new Error(`HTTP ${response.status}: ${url}`);
    } catch (error) {
      lastError = error;

      // Don't retry on 4xx errors (client error - won't change)
      if (error.message.includes('HTTP 4')) {
        throw error;
      }

      // Retry with backoff if attempts remain
      if (attempt < maxRetries) {
        const delay = calculateBackoffDelay(attempt, initialDelay);
        console.warn(`Retry ${attempt + 1}/${maxRetries} for ${url} after ${delay}ms (${error.message})`);
        await sleep(delay);
      }
    }
  }

  throw new Error(`Failed after ${maxRetries + 1} attempts: ${lastError?.message || 'Unknown error'}`);
}

/**
 * Fetch from CTAN with URL fallbacks and retry logic
 * @param {string} filename - Input filename
 * @param {Object} opts - Fetch options
 * @param {string} opts.registry - Registry URL (CTAN mirror or local)
 * @param {number} opts.maxRetries - Max retry attempts per URL
 * @param {number} opts.initialDelay - Initial retry delay
 * @returns {Promise<{content: Uint8Array, url: string}>} File content and successful URL
 * @throws {Error} If all URLs and retries fail
 */
async function fetchFromCtan(filename, { registry = DEFAULT_CTAN_MIRROR, maxRetries, initialDelay }) {
  const urls = buildCtanUrls(filename, registry);
  const errors = [];

  for (const url of urls) {
    try {
      const content = await fetchWithRetry(url, { maxRetries, initialDelay });
      return { content, url };
    } catch (error) {
      errors.push({ url, error: error.message });
      // Try next URL
      continue;
    }
  }

  // All URLs failed
  const errorDetails = errors.map(e => `  - ${e.url}\n    Error: ${e.error}`).join('\n');
  throw new Error(
    `Failed to fetch '${filename}' from CTAN after trying ${urls.length} URLs.\n\n` +
    `URLs attempted:\n${errorDetails}\n\n` +
    `Possible reasons:\n` +
    `  - Package not available on CTAN\n` +
    `  - Network connectivity issues\n` +
    `  - Mirror temporarily unavailable\n\n` +
    `Try:\n` +
    `  1. Check package name spelling\n` +
    `  2. Verify network connection\n` +
    `  3. Install package manually: tlmgr install ${extractPackageName(filename)}`
  );
}

// ============================================================================
// PUBLIC API
// ============================================================================

/**
 * Resolve missing LaTeX inputs by fetching from CTAN or local cache
 *
 * **Algorithm**:
 * 1. Validate inputs (Zod schema)
 * 2. Load cache index
 * 3. For each missing input:
 *    a. Check local cache (verify hash)
 *    b. If not cached, fetch from CTAN with retry + exponential backoff
 *    c. Cache with content-hash filename
 *    d. Build VFS path and add to result
 * 4. Save updated cache index
 * 5. Return Map<vfsPath, Uint8Array>
 *
 * **Retry Logic**:
 * - Per-URL retry with exponential backoff: 100ms, 200ms, 400ms, ...
 * - Max delay capped at 5000ms
 * - Configurable via maxRetries and initialDelay options
 *
 * **Local Fixture Support**:
 * - Detects file:// or http://localhost URLs
 * - No retry logic for local fixtures (immediate fail/success)
 *
 * @param {ResolveOptions} options - Resolver options
 * @returns {Promise<Map<string, Uint8Array>>} Resolved files (vfs path -> content)
 * @throws {Error} If fetch fails after retries
 *
 * @example
 * // Production usage (CTAN)
 * const resolved = await resolveMissingInputs({
 *   missingInputs: ['algorithm2e.sty', 'tikz.sty'],
 *   cacheDir: '/home/user/.cache/kgc-latex',
 *   maxRetries: 3,
 *   initialDelay: 100,
 * });
 *
 * @example
 * // Testing with local fixture server
 * const resolved = await resolveMissingInputs({
 *   missingInputs: ['test.sty'],
 *   cacheDir: '/tmp/test-cache',
 *   registry: 'http://localhost:3000/fixtures',
 * });
 */
export async function resolveMissingInputs(options) {
  // Validate inputs
  const validated = ResolveOptionsSchema.parse(options);
  const {
    missingInputs,
    cacheDir,
    registry = DEFAULT_CTAN_MIRROR,
    lockfile = null,
    maxRetries = DEFAULT_MAX_RETRIES,
    initialDelay = DEFAULT_INITIAL_DELAY,
  } = validated;

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
        // Check lockfile for pinned version/URL (if provided)
        let fetchUrl = registry;
        if (lockfile?.packages?.[filename]?.url) {
          fetchUrl = lockfile.packages[filename].url;
        }

        const fetched = await fetchFromCtan(filename, {
          registry: fetchUrl,
          maxRetries,
          initialDelay,
        });
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
    const details = failures.map(f => `  - ${f.filename}\n    ${f.error}`).join('\n\n');
    throw new Error(
      `Failed to resolve ${failures.length} input(s): ${failedNames}\n\n` +
      `Details:\n${details}\n\n` +
      `Successfully resolved: ${result.size} file(s)`
    );
  }

  return result;
}

/**
 * Augment VFS with resolved packages
 * Does NOT mutate input VFS - returns new object.
 *
 * @param {Record<string, Uint8Array>} vfs - Existing VFS object
 * @param {Map<string, Uint8Array>} resolvedMap - Resolved packages
 * @returns {Record<string, Uint8Array>} New VFS with merged files
 *
 * @example
 * const vfs = { 'work/main.tex': new Uint8Array(...) };
 * const resolved = await resolveMissingInputs({...});
 * const augmentedVfs = augmentVfsWithResolvedPackages(vfs, resolved);
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
 */
export function clearCache(cacheDir, inputs = null) {
  const index = loadCacheIndex(cacheDir);
  let removed = 0;

  if (inputs === null) {
    removed = Object.keys(index).length;
    saveCacheIndex(cacheDir, {});
    return removed;
  }

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
// EXPORTS SUMMARY
// ============================================================================

/**
 * Module exports:
 * - resolveMissingInputs(options): Main resolver with retry logic
 * - augmentVfsWithResolvedPackages(vfs, resolvedMap): VFS merge helper
 * - clearCache(cacheDir, inputs?): Cache management
 * - getCacheStats(cacheDir): Cache inspection
 *
 * **How compile.mjs should call**:
 * ```javascript
 * import { resolveMissingInputs, augmentVfsWithResolvedPackages } from './cache/resolve.mjs';
 *
 * // When engine reports missing inputs
 * const resolved = await resolveMissingInputs({
 *   missingInputs: ['algorithm2e.sty', 'tikz.sty'],
 *   cacheDir: '/home/user/.cache/kgc-latex',
 *   registry: 'https://mirrors.ctan.org',  // or http://localhost:3000 for testing
 *   maxRetries: 3,
 *   initialDelay: 100,
 * });
 *
 * // Merge into VFS
 * vfs = augmentVfsWithResolvedPackages(vfs, resolved);
 *
 * // Re-run compilation
 * const result = await compileWithSwiftLatex({ engine, vfs, ... });
 * ```
 */
