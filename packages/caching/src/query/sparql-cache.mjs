/**
 * @file SPARQL Cache - Query result caching with semantic key generation
 * @module @unrdf/caching/query
 *
 * Provides intelligent caching for SPARQL query results with:
 * - Semantic cache key generation (normalized queries)
 * - Query result caching
 * - Partial result reuse
 * - Integration with dependency tracker
 *
 * @example
 * import { SparqlCache } from '@unrdf/caching/query';
 * import { createStore } from '@unrdf/oxigraph';
 *
 * const cache = new SparqlCache({
 *   store: createStore(),
 *   cache: multiLayerCache,
 *   tracker: dependencyTracker
 * });
 *
 * const results = await cache.query('SELECT * WHERE { ?s ?p ?o }');
 */

import { createHash } from 'node:crypto';
import { z } from 'zod';

// =============================================================================
// SCHEMAS & TYPES
// =============================================================================

/**
 * SPARQL cache configuration
 */
const SparqlCacheConfigSchema = z.object({
  store: z.any(), // OxigraphStore instance
  cache: z.any(), // MultiLayerCache instance
  tracker: z.any().optional(), // DependencyTracker instance
  enableNormalization: z.boolean().default(true),
  enableDependencyTracking: z.boolean().default(true),
  defaultTtl: z.number().int().positive().default(300), // 5 minutes
  maxCachedResults: z.number().int().positive().default(10000),
});

/**
 * @typedef {z.infer<typeof SparqlCacheConfigSchema>} SparqlCacheConfig
 */

/**
 * Cache statistics
 * @typedef {Object} QueryCacheStats
 * @property {number} hits - Cache hits
 * @property {number} misses - Cache misses
 * @property {number} queries - Total queries
 * @property {number} cachedQueries - Number of cached queries
 * @property {number} avgResultSize - Average result size
 */

// =============================================================================
// SPARQL CACHE
// =============================================================================

/**
 * SPARQL query cache with semantic key generation
 */
export class SparqlCache {
  /**
   * Create a new SPARQL cache
   * @param {Partial<SparqlCacheConfig>} config - Cache configuration
   */
  constructor(config = {}) {
    const validated = SparqlCacheConfigSchema.parse(config);

    this.store = validated.store;
    this.cache = validated.cache;
    this.tracker = validated.tracker;
    this.config = validated;

    // Statistics
    this.stats = {
      hits: 0,
      misses: 0,
      queries: 0,
      cachedQueries: 0,
      totalResultSize: 0,
    };
  }

  /**
   * Execute SPARQL query with caching
   * @param {string} query - SPARQL query string
   * @param {Object} [options] - Query options
   * @param {boolean} [options.useCache=true] - Use cache
   * @param {number} [options.ttl] - Cache TTL in seconds
   * @returns {Promise<any>} Query results
   */
  async query(query, options = {}) {
    const useCache = options.useCache !== false;
    const ttl = options.ttl || this.config.defaultTtl;

    this.stats.queries++;

    // Generate cache key
    const cacheKey = this._generateCacheKey(query);

    // Check cache
    if (useCache) {
      const cached = await this.cache.get(cacheKey);

      if (cached !== null) {
        this.stats.hits++;
        return cached;
      }
    }

    this.stats.misses++;

    // Execute query
    const results = await this._executeQuery(query);

    // Cache results
    if (useCache && results) {
      await this._cacheResults(cacheKey, query, results, ttl);
    }

    return results;
  }

  /**
   * Execute query without caching (direct to store)
   * @private
   * @param {string} query - SPARQL query
   * @returns {Promise<any>} Query results
   */
  async _executeQuery(query) {
    try {
      const results = this.store.query(query);

      // Convert to array if iterable
      if (results && typeof results[Symbol.iterator] === 'function') {
        return Array.from(results);
      }

      return results;
    } catch (error) {
      console.error('[SparqlCache] Query execution error:', error);
      throw error;
    }
  }

  /**
   * Cache query results
   * @private
   * @param {string} cacheKey - Cache key
   * @param {string} query - Original query
   * @param {any} results - Query results
   * @param {number} ttl - TTL in seconds
   */
  async _cacheResults(cacheKey, query, results, ttl) {
    // Store results
    await this.cache.set(cacheKey, results, { ttl });

    this.stats.cachedQueries++;
    this.stats.totalResultSize += this._getResultSize(results);

    // Track dependencies
    if (this.config.enableDependencyTracking && this.tracker) {
      const subjects = this._extractSubjects(query);
      const graph = this._extractGraph(query);

      if (subjects.length > 0) {
        this.tracker.trackQuery(cacheKey, subjects, graph);
      }
    }
  }

  /**
   * Generate semantic cache key from query
   * @private
   * @param {string} query - SPARQL query
   * @returns {string} Cache key
   */
  _generateCacheKey(query) {
    const normalized = this.config.enableNormalization
      ? this._normalizeQuery(query)
      : query;

    const hash = createHash('sha256')
      .update(normalized)
      .digest('hex')
      .substring(0, 16);

    return `sparql:${hash}`;
  }

  /**
   * Normalize SPARQL query for consistent caching
   * @private
   * @param {string} query - SPARQL query
   * @returns {string} Normalized query
   */
  _normalizeQuery(query) {
    return query
      .replace(/\s+/g, ' ') // Collapse whitespace
      .replace(/\s*([{}(),;.])\s*/g, '$1') // Remove whitespace around punctuation
      .replace(/\s*\.\s*}/g, '}') // Remove trailing dots before }
      .trim()
      .toLowerCase();
  }

  /**
   * Extract subject URIs from query
   * @private
   * @param {string} query - SPARQL query
   * @returns {Array<string>} Subject URIs
   */
  _extractSubjects(query) {
    const subjects = new Set();

    // Extract URIs from angle brackets
    const uriMatches = query.matchAll(/<([^>]+)>/g);
    for (const match of uriMatches) {
      subjects.add(match[1]);
    }

    // Extract from FILTER clauses
    const filterMatches = query.matchAll(/FILTER\s*\([^)]*<([^>]+)>/gi);
    for (const match of filterMatches) {
      subjects.add(match[1]);
    }

    // Extract from VALUES clauses
    const valuesMatches = query.matchAll(/VALUES\s+\?[^\s]+\s+{[^}]*<([^>]+)>/gi);
    for (const match of valuesMatches) {
      subjects.add(match[1]);
    }

    return Array.from(subjects);
  }

  /**
   * Extract graph URI from query
   * @private
   * @param {string} query - SPARQL query
   * @returns {string|null} Graph URI or null
   */
  _extractGraph(query) {
    // Look for GRAPH <uri> patterns
    const graphMatch = query.match(/GRAPH\s+<([^>]+)>/i);
    return graphMatch ? graphMatch[1] : null;
  }

  /**
   * Get size of query results
   * @private
   * @param {any} results - Query results
   * @returns {number} Approximate size
   */
  _getResultSize(results) {
    if (Array.isArray(results)) {
      return results.length;
    }
    if (typeof results === 'boolean') {
      return 1;
    }
    if (typeof results === 'object') {
      return Object.keys(results).length;
    }
    return 1;
  }

  /**
   * Invalidate cached query
   * @param {string} query - SPARQL query or cache key
   * @returns {Promise<void>}
   */
  async invalidate(query) {
    const cacheKey = query.startsWith('sparql:')
      ? query
      : this._generateCacheKey(query);

    await this.cache.delete(cacheKey);
  }

  /**
   * Invalidate queries matching pattern
   * @param {string} pattern - Cache key pattern
   * @returns {Promise<number>} Number of queries invalidated
   */
  async invalidatePattern(pattern) {
    return await this.cache.deletePattern(pattern);
  }

  /**
   * Pre-warm cache with common queries
   * @param {Array<string>} queries - Queries to pre-execute and cache
   * @returns {Promise<number>} Number of queries cached
   */
  async prewarm(queries) {
    let cached = 0;

    for (const query of queries) {
      try {
        await this.query(query);
        cached++;
      } catch (error) {
        console.warn('[SparqlCache] Prewarm failed for query:', error.message);
      }
    }

    return cached;
  }

  /**
   * Get cache statistics
   * @returns {QueryCacheStats} Current statistics
   */
  getStats() {
    return {
      ...this.stats,
      hitRate: this.stats.hits / this.stats.queries || 0,
      avgResultSize:
        this.stats.cachedQueries > 0
          ? this.stats.totalResultSize / this.stats.cachedQueries
          : 0,
    };
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      hits: 0,
      misses: 0,
      queries: 0,
      cachedQueries: 0,
      totalResultSize: 0,
    };
  }

  /**
   * Clear all cached queries
   * @returns {Promise<void>}
   */
  async clear() {
    await this.cache.deletePattern('sparql:*');
    this.stats.cachedQueries = 0;
  }
}

/**
 * Create a SPARQL cache instance
 * @param {Partial<SparqlCacheConfig>} config - Cache configuration
 * @returns {SparqlCache} Cache instance
 */
export function createSparqlCache(config = {}) {
  return new SparqlCache(config);
}

export default {
  SparqlCache,
  createSparqlCache,
};
