/**
 * @file Query Cache (Legacy - Deprecated)
 * @module query-cache
 *
 * @description
 * This module has been deprecated with the migration from Comunica to Oxigraph.
 * Oxigraph uses synchronous execution with <1ms cold start, eliminating the need
 * for QueryEngine initialization caching.
 *
 * Maintained for backward compatibility only.
 */

import { createHash } from 'node:crypto';

/**
 * LRU cache for file content (keyed by SHA-256)
 * @type {Map<string, string>}
 */
const fileContentCache = new Map();

/**
 * Maximum cache entries
 */
const MAX_FILE_CACHE = 50;

/**
 * Cache statistics
 */
const stats = {
  fileCacheHits: 0,
  fileCacheMisses: 0,
};

/**
 * Get query hash
 *
 * @param {string} sparql - SPARQL query string
 * @returns {string} Query hash for caching
 */
export function getQueryHash(sparql) {
  return createHash('sha256').update(sparql.trim()).digest('hex');
}

/**
 * Cache file content by SHA-256 hash.
 *
 * @param {string} sha256 - Content hash
 * @param {string} content - File content
 */
export function cacheFileContent(sha256, content) {
  // LRU eviction: Remove oldest entry if cache is full
  if (fileContentCache.size >= MAX_FILE_CACHE && !fileContentCache.has(sha256)) {
    const firstKey = fileContentCache.keys().next().value;
    fileContentCache.delete(firstKey);
  }

  fileContentCache.set(sha256, content);
}

/**
 * Get cached file content by SHA-256 hash.
 *
 * @param {string} sha256 - Content hash
 * @returns {string|null} Cached content or null
 */
export function getCachedFileContent(sha256) {
  if (fileContentCache.has(sha256)) {
    stats.fileCacheHits++;
    return fileContentCache.get(sha256);
  }
  stats.fileCacheMisses++;
  return null;
}

/**
 * Get cache statistics.
 *
 * @returns {Object} Cache statistics
 */
export function getCacheStats() {
  return {
    ...stats,
    fileCacheSize: fileContentCache.size,
    fileCacheHitRate: stats.fileCacheHits / (stats.fileCacheHits + stats.fileCacheMisses) || 0,
  };
}

/**
 * Clear all caches (for testing).
 */
export function clearCaches() {
  fileContentCache.clear();
  stats.fileCacheHits = 0;
  stats.fileCacheMisses = 0;
}

/**
 * Shutdown hook for cleanup.
 * Should be called when process is exiting.
 */
export function shutdown() {
  clearCaches();
}

// Register shutdown hook
if (typeof process !== 'undefined') {
  process.on('exit', shutdown);
  process.on('SIGINT', () => {
    shutdown();
    process.exit(0);
  });
  process.on('SIGTERM', () => {
    shutdown();
    process.exit(0);
  });
}
