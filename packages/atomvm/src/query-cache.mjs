/**
 * Query Result Cache with LRU Eviction and TTL
 *
 * Implements an LRU cache for SPARQL query results with:
 * - Time-to-live (TTL) expiration
 * - Triple pattern-based invalidation
 * - OTEL instrumentation for metrics
 *
 * **Poka-Yoke Design**: Validates inputs and prevents cache pollution.
 *
 * @module query-cache
 */

import { trace, metrics } from '@opentelemetry/api';

/**
 * Get tracer lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Tracer} OpenTelemetry tracer
 */
function getTracer() {
  return trace.getTracer('query-cache');
}

/**
 * Get meter lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Meter} OpenTelemetry meter
 */
function getMeter() {
  return metrics.getMeter('query-cache');
}

/**
 * Cache entry with metadata for LRU tracking and TTL
 * @typedef {Object} CacheEntry
 * @property {any} result - The cached query result
 * @property {number} timestamp - When the entry was created
 * @property {number} expiry - When the entry expires (timestamp + ttl)
 * @property {string} queryHash - Hash of the query for identification
 * @property {Set<string>} patterns - Triple patterns affected by this query
 */

/**
 * Options for QueryCache constructor
 * @typedef {Object} QueryCacheOptions
 * @property {number} [maxSize=100] - Maximum number of cached entries
 * @property {number} [ttl=60000] - Time-to-live in milliseconds (default: 1 minute)
 */

/**
 * Cache statistics
 * @typedef {Object} CacheStats
 * @property {number} hits - Number of cache hits
 * @property {number} misses - Number of cache misses
 * @property {number} evictions - Number of LRU evictions
 * @property {number} expirations - Number of TTL expirations
 * @property {number} size - Current cache size
 * @property {number} hitRate - Hit rate (hits / (hits + misses))
 */

/**
 * LRU Cache for SPARQL query results
 *
 * Uses Map's insertion order for LRU tracking.
 * Entries are moved to the end on access (most recently used).
 * Eviction removes from the beginning (least recently used).
 */
export class QueryCache {
  /**
   * Create a new QueryCache
   *
   * @param {QueryCacheOptions} [options={}] - Cache configuration
   */
  constructor(options = {}) {
    const { maxSize = 100, ttl = 60000 } = options;

    // Poka-yoke: Validate configuration
    if (typeof maxSize !== 'number' || maxSize < 1) {
      throw new Error('maxSize must be a positive integer');
    }
    if (typeof ttl !== 'number' || ttl < 0) {
      throw new Error('ttl must be a non-negative number');
    }

    /** @type {number} */
    this.maxSize = Math.floor(maxSize);

    /** @type {number} */
    this.ttl = ttl;

    /** @type {Map<string, CacheEntry>} */
    this._cache = new Map();

    /** @type {number} */
    this._hits = 0;

    /** @type {number} */
    this._misses = 0;

    /** @type {number} */
    this._evictions = 0;

    /** @type {number} */
    this._expirations = 0;

    // Initialize OTEL metrics (lazy)
    this._metricsInitialized = false;
    this._hitCounter = null;
    this._missCounter = null;
    this._evictionCounter = null;
  }

  /**
   * Initialize OTEL metrics lazily
   * @private
   */
  _initMetrics() {
    if (this._metricsInitialized) return;

    try {
      const meter = getMeter();
      this._hitCounter = meter.createCounter('query_cache_hits', {
        description: 'Number of cache hits',
      });
      this._missCounter = meter.createCounter('query_cache_misses', {
        description: 'Number of cache misses',
      });
      this._evictionCounter = meter.createCounter('query_cache_evictions', {
        description: 'Number of cache evictions',
      });
      this._metricsInitialized = true;
    } catch {
      // OTEL not available, continue without metrics
      this._metricsInitialized = true;
    }
  }

  /**
   * Generate cache key from query and bindings
   *
   * @param {string} query - SPARQL query string
   * @param {Object} [bindings={}] - Variable bindings
   * @returns {string} Cache key
   * @private
   */
  _generateKey(query, bindings = {}) {
    const normalizedQuery = query.trim().replace(/\s+/g, ' ');
    const bindingsStr = JSON.stringify(bindings, Object.keys(bindings).sort());
    return `${normalizedQuery}::${bindingsStr}`;
  }

  /**
   * Extract triple patterns from a SPARQL query for invalidation tracking
   *
   * @param {string} query - SPARQL query string
   * @returns {Set<string>} Set of pattern signatures
   * @private
   */
  _extractPatterns(query) {
    const patterns = new Set();

    // Simple pattern extraction: match triple patterns in WHERE clause
    // Format: ?s ?p ?o or <uri> ?p ?o etc.
    const triplePattern = /([?$]\w+|<[^>]+>)\s+([?$]\w+|<[^>]+>)\s+([?$]\w+|<[^>]+>|"[^"]*"|\d+)/g;

    let match;
    while ((match = triplePattern.exec(query)) !== null) {
      const [, s, p, o] = match;
      // Create pattern signature: variable positions are marked with *
      const sig = [
        s.startsWith('?') || s.startsWith('$') ? '*' : s,
        p.startsWith('?') || p.startsWith('$') ? '*' : p,
        o.startsWith('?') || o.startsWith('$') ? '*' : o,
      ].join(' ');
      patterns.add(sig);
    }

    return patterns;
  }

  /**
   * Check if an entry has expired
   *
   * @param {CacheEntry} entry - Cache entry to check
   * @returns {boolean} True if expired
   * @private
   */
  _isExpired(entry) {
    return Date.now() > entry.expiry;
  }

  /**
   * Move entry to end of Map (most recently used)
   *
   * @param {string} key - Cache key
   * @param {CacheEntry} entry - Cache entry
   * @private
   */
  _touch(key, entry) {
    this._cache.delete(key);
    this._cache.set(key, entry);
  }

  /**
   * Evict least recently used entry
   *
   * @private
   */
  _evictLRU() {
    if (this._cache.size === 0) return;

    // First key is least recently used (Map iteration order)
    const lruKey = this._cache.keys().next().value;
    this._cache.delete(lruKey);
    this._evictions++;

    if (this._evictionCounter) {
      this._evictionCounter.add(1);
    }
  }

  /**
   * Cache a query result
   *
   * @param {string} query - SPARQL query string
   * @param {Object} bindings - Variable bindings used in query
   * @param {any} result - Query result to cache
   */
  set(query, bindings, result) {
    this._initMetrics();

    // Poka-yoke: Validate inputs
    if (typeof query !== 'string' || query.trim().length === 0) {
      throw new Error('query must be a non-empty string');
    }

    const key = this._generateKey(query, bindings);
    const now = Date.now();

    // Evict if at capacity and key doesn't exist
    if (this._cache.size >= this.maxSize && !this._cache.has(key)) {
      this._evictLRU();
    }

    /** @type {CacheEntry} */
    const entry = {
      result,
      timestamp: now,
      expiry: now + this.ttl,
      queryHash: key,
      patterns: this._extractPatterns(query),
    };

    // Delete first to ensure insertion at end (most recently used)
    this._cache.delete(key);
    this._cache.set(key, entry);
  }

  /**
   * Retrieve a cached query result
   *
   * @param {string} query - SPARQL query string
   * @param {Object} [bindings={}] - Variable bindings
   * @returns {any|undefined} Cached result or undefined if not found/expired
   */
  get(query, bindings = {}) {
    this._initMetrics();

    // Poka-yoke: Validate inputs
    if (typeof query !== 'string' || query.trim().length === 0) {
      throw new Error('query must be a non-empty string');
    }

    const key = this._generateKey(query, bindings);
    const entry = this._cache.get(key);

    if (!entry) {
      this._misses++;
      if (this._missCounter) {
        this._missCounter.add(1);
      }
      return undefined;
    }

    // Check TTL expiration
    if (this._isExpired(entry)) {
      this._cache.delete(key);
      this._expirations++;
      this._misses++;
      if (this._missCounter) {
        this._missCounter.add(1);
      }
      return undefined;
    }

    // Move to end (most recently used)
    this._touch(key, entry);

    this._hits++;
    if (this._hitCounter) {
      this._hitCounter.add(1);
    }

    return entry.result;
  }

  /**
   * Invalidate cache entries matching a triple pattern
   *
   * Pattern format: { subject: uri|'*', predicate: uri|'*', object: uri|'*' }
   * Use '*' for wildcard matching any value.
   *
   * @param {Object} pattern - Triple pattern for invalidation
   * @param {string} [pattern.subject='*'] - Subject URI or '*' for any
   * @param {string} [pattern.predicate='*'] - Predicate URI or '*' for any
   * @param {string} [pattern.object='*'] - Object value or '*' for any
   * @returns {number} Number of entries invalidated
   */
  invalidate(pattern) {
    const { subject = '*', predicate = '*', object = '*' } = pattern || {};

    // Build pattern signature for matching
    const patternSig = `${subject} ${predicate} ${object}`;

    let invalidatedCount = 0;
    const keysToDelete = [];

    for (const [key, entry] of this._cache) {
      // Check if any of the entry's patterns match
      for (const entryPattern of entry.patterns) {
        if (this._patternsMatch(patternSig, entryPattern)) {
          keysToDelete.push(key);
          invalidatedCount++;
          break;
        }
      }
    }

    // Delete matched entries
    for (const key of keysToDelete) {
      this._cache.delete(key);
    }

    return invalidatedCount;
  }

  /**
   * Check if two pattern signatures match
   *
   * @param {string} pattern1 - First pattern
   * @param {string} pattern2 - Second pattern
   * @returns {boolean} True if patterns match
   * @private
   */
  _patternsMatch(pattern1, pattern2) {
    const parts1 = pattern1.split(' ');
    const parts2 = pattern2.split(' ');

    if (parts1.length !== parts2.length) return false;

    for (let i = 0; i < parts1.length; i++) {
      // Wildcard matches anything
      if (parts1[i] === '*' || parts2[i] === '*') continue;
      // Exact match required
      if (parts1[i] !== parts2[i]) return false;
    }

    return true;
  }

  /**
   * Clear all cached entries
   */
  invalidateAll() {
    const size = this._cache.size;
    this._cache.clear();
    return size;
  }

  /**
   * Get cache statistics
   *
   * @returns {CacheStats} Cache statistics
   */
  stats() {
    const total = this._hits + this._misses;
    return {
      hits: this._hits,
      misses: this._misses,
      evictions: this._evictions,
      expirations: this._expirations,
      size: this._cache.size,
      hitRate: total > 0 ? this._hits / total : 0,
    };
  }

  /**
   * Check if cache contains an entry for query (without affecting stats)
   *
   * @param {string} query - SPARQL query string
   * @param {Object} [bindings={}] - Variable bindings
   * @returns {boolean} True if entry exists and is not expired
   */
  has(query, bindings = {}) {
    const key = this._generateKey(query, bindings);
    const entry = this._cache.get(key);

    if (!entry) return false;
    if (this._isExpired(entry)) {
      this._cache.delete(key);
      this._expirations++;
      return false;
    }

    return true;
  }

  /**
   * Get current cache size
   *
   * @returns {number} Number of entries in cache
   */
  get size() {
    return this._cache.size;
  }

  /**
   * Clean up expired entries
   *
   * @returns {number} Number of entries removed
   */
  prune() {
    let removed = 0;
    const now = Date.now();

    for (const [key, entry] of this._cache) {
      if (now > entry.expiry) {
        this._cache.delete(key);
        this._expirations++;
        removed++;
      }
    }

    return removed;
  }
}

/**
 * Create a QueryCache instance with default options
 *
 * @param {QueryCacheOptions} [options] - Cache options
 * @returns {QueryCache} New QueryCache instance
 */
export function createQueryCache(options) {
  return new QueryCache(options);
}

export default QueryCache;
