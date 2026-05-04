/**
 * Query Cache - Shared Cache for SPARQL and Temporal Queries
 * 
 * Features:
 * 1. Snapshot-based invalidation
 * 2. Request coalescing (deduplication)
 * 3. LRU eviction
 * 4. OTEL-ready metrics
 * 
 * @module @unrdf/kgc-4d/cache
 */

export class QueryCache {
  /**
   * @param {Object} [options]
   * @param {number} [options.maxSize=1000] - Maximum entries in cache
   * @param {number} [options.ttlMs=0] - Time-to-live in ms (0 = no expiration)
   */
  constructor(options = {}) {
    this.maxSize = options.maxSize || 1000;
    this.ttlMs = options.ttlMs || 0;
    
    /** @type {Map<string, {value: any, timestamp: number}>} */
    this.cache = new Map();
    
    /** @type {Map<string, Promise<any>>} */
    this.pendingQueries = new Map();
    
    this.stats = {
      hits: 0,
      misses: 0,
      coalesced: 0,
      evictions: 0,
      invalidations: 0
    };
  }

  /**
   * Get query result from cache or execute query
   * 
   * @param {string} key - Unique key for the query (e.g. SPARQL string + snapshot ID)
   * @param {Function} queryFn - Async function to execute if cache miss
   * @returns {Promise<any>}
   */
  async get(key, queryFn) {
    const now = Date.now();
    const entry = this.cache.get(key);

    if (entry) {
      if (this.ttlMs === 0 || now - entry.timestamp < this.ttlMs) {
        this.stats.hits++;
        // Move to end for LRU
        this.cache.delete(key);
        this.cache.set(key, entry);
        return entry.value;
      }
      this.cache.delete(key);
    }

    // Check for pending query (Request Coalescing)
    if (this.pendingQueries.has(key)) {
      this.stats.coalesced++;
      return this.pendingQueries.get(key);
    }

    this.stats.misses++;
    
    const promise = queryFn().then(value => {
      this._set(key, value);
      this.pendingQueries.delete(key);
      return value;
    }).catch(err => {
      this.pendingQueries.delete(key);
      throw err;
    });

    this.pendingQueries.set(key, promise);
    return promise;
  }

  /**
   * Set value in cache directly
   * @private
   */
  _set(key, value) {
    if (this.cache.size >= this.maxSize) {
      const oldestKey = this.cache.keys().next().value;
      this.cache.delete(oldestKey);
      this.stats.evictions++;
    }
    this.cache.set(key, { value, timestamp: Date.now() });
  }

  /**
   * Invalidate cache entries
   * 
   * @param {string|RegExp|Function} [pattern] - Invalidation pattern. 
   * If string, match as prefix. If omitted, clear everything.
   */
  invalidate(pattern) {
    if (!pattern) {
      this.cache.clear();
      this.stats.invalidations++;
      return;
    }

    let count = 0;
    for (const key of this.cache.keys()) {
      let shouldInvalidate = false;
      if (typeof pattern === 'string') {
        shouldInvalidate = key.startsWith(pattern) || key.includes(pattern);
      } else if (pattern instanceof RegExp) {
        shouldInvalidate = pattern.test(key);
      } else if (typeof pattern === 'function') {
        shouldInvalidate = pattern(key);
      }

      if (shouldInvalidate) {
        this.cache.delete(key);
        count++;
      }
    }
    this.stats.invalidations += count > 0 ? 1 : 0;
  }

  /**
   * Get cache statistics
   */
  getStats() {
    const total = this.stats.hits + this.stats.misses;
    return {
      ...this.stats,
      size: this.cache.size,
      hitRate: total > 0 ? this.stats.hits / total : 0
    };
  }
}
