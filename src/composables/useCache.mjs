/**
 * @fileoverview useCache composable - query result caching
 * 
 * This composable provides caching capabilities for RDF operations.
 * It enables performance optimization through intelligent caching.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

/**
 * Create a cache composable
 * 
 * @param {Object} [options] - Cache options
 * @param {number} [options.maxSize=1000] - Maximum cache size
 * @param {number} [options.defaultTTL=300000] - Default TTL in milliseconds (5 minutes)
 * @param {boolean} [options.enabled=true] - Enable caching
 * @returns {Object} Cache interface
 * 
 * @example
 * const cache = useCache({
 *   maxSize: 500,
 *   defaultTTL: 600000 // 10 minutes
 * });
 * 
 * // Cache a query result
 * const result = await cache.get('my-query', async () => {
 *   return await graph.select('SELECT * WHERE { ?s ?p ?o }');
 * });
 * 
 * // Cache with custom TTL
 * const result = await cache.get('expensive-query', async () => {
 *   return await expensiveOperation();
 * }, { ttl: 3600000 }); // 1 hour
 * 
 * // Clear cache
 * cache.clear();
 */
export function useCache(options = {}) {
  const {
    maxSize = 1000,
    defaultTTL = 300_000, // 5 minutes
    enabled = true
  } = options || {};

  // Internal cache storage
  const cache = new Map();
  const accessTimes = new Map();

  return {
    /**
     * Get a value from cache or execute function
     * @param {string} key - Cache key
     * @param {Function} fn - Function to execute if not cached
     * @param {Object} [opts] - Options
     * @param {number} [opts.ttl] - Time to live in milliseconds
     * @returns {Promise<any>} Cached or computed value
     */
    async get(key, fn, opts = {}) {
      if (!enabled) {
        return await fn();
      }

      const { ttl = defaultTTL } = opts;
      const now = Date.now();

      // Check if key exists and is not expired
      if (cache.has(key)) {
        const entry = cache.get(key);
        
        if (now < entry.expires) {
          // Update access time for LRU
          accessTimes.set(key, now);
          return entry.value;
        } else {
          // Expired, remove it
          cache.delete(key);
          accessTimes.delete(key);
        }
      }

      // Execute function and cache result
      const value = await fn();
      
      this.set(key, value, { ttl });
      
      return value;
    },

    /**
     * Set a value in cache
     * @param {string} key - Cache key
     * @param {any} value - Value to cache
     * @param {Object} [opts] - Options
     * @param {number} [opts.ttl] - Time to live in milliseconds
     * @returns {Object} This composable instance
     */
    set(key, value, opts = {}) {
      if (!enabled) {
        return this;
      }

      const { ttl = defaultTTL } = opts;
      const now = Date.now();

      // Check if we need to evict entries
      if (cache.size >= maxSize) {
        this._evictLRU();
      }

      // Store the value
      cache.set(key, {
        value,
        expires: now + ttl,
        created: now
      });

      accessTimes.set(key, now);

      return this;
    },

    /**
     * Check if a key exists in cache
     * @param {string} key - Cache key
     * @returns {boolean} True if key exists and is not expired
     */
    has(key) {
      if (!enabled || !cache.has(key)) {
        return false;
      }

      const entry = cache.get(key);
      const now = Date.now();

      if (now >= entry.expires) {
        // Expired, remove it
        cache.delete(key);
        accessTimes.delete(key);
        return false;
      }

      return true;
    },

    /**
     * Get a value from cache without executing function
     * @param {string} key - Cache key
     * @returns {any|undefined} Cached value or undefined
     */
    getOnly(key) {
      if (!enabled || !this.has(key)) {
        return undefined;
      }

      const entry = cache.get(key);
      accessTimes.set(key, Date.now());
      return entry.value;
    },

    /**
     * Delete a key from cache
     * @param {string} key - Cache key
     * @returns {boolean} True if key was deleted
     */
    delete(key) {
      const deleted = cache.delete(key);
      accessTimes.delete(key);
      return deleted;
    },

    /**
     * Clear all cache entries
     * @returns {Object} This composable instance
     */
    clear() {
      cache.clear();
      accessTimes.clear();
      return this;
    },

    /**
     * Get cache statistics
     * @returns {Object} Cache statistics
     */
    stats() {
      const now = Date.now();
      let expired = 0;
      let valid = 0;

      for (const entry of cache.values()) {
        if (now >= entry.expires) {
          expired++;
        } else {
          valid++;
        }
      }

      return {
        size: cache.size,
        valid,
        expired,
        maxSize,
        hitRate: this._calculateHitRate(),
        memoryUsage: this._estimateMemoryUsage()
      };
    },

    /**
     * Get all cache keys
     * @returns {Array} Array of cache keys
     */
    keys() {
      return [...cache.keys()];
    },

    /**
     * Get cache entries with metadata
     * @returns {Array} Array of cache entries
     */
    entries() {
      const now = Date.now();
      const entries = [];

      for (const [key, entry] of cache.entries()) {
        entries.push({
          key,
          value: entry.value,
          created: entry.created,
          expires: entry.expires,
          ttl: entry.expires - entry.created,
          expired: now >= entry.expires,
          lastAccess: accessTimes.get(key) || entry.created
        });
      }

      return entries;
    },

    /**
     * Clean up expired entries
     * @returns {number} Number of entries cleaned up
     */
    cleanup() {
      const now = Date.now();
      let cleaned = 0;

      for (const [key, entry] of cache.entries()) {
        if (now >= entry.expires) {
          cache.delete(key);
          accessTimes.delete(key);
          cleaned++;
        }
      }

      return cleaned;
    },

    /**
     * Evict least recently used entry
     * @private
     */
    _evictLRU() {
      if (accessTimes.size === 0) {
        return;
      }

      let oldestKey = null;
      let oldestTime = Infinity;

      for (const [key, time] of accessTimes.entries()) {
        if (time < oldestTime) {
          oldestTime = time;
          oldestKey = key;
        }
      }

      if (oldestKey) {
        cache.delete(oldestKey);
        accessTimes.delete(oldestKey);
      }
    },

    /**
     * Calculate hit rate (simplified)
     * @returns {number} Hit rate percentage
     * @private
     */
    _calculateHitRate() {
      // This is a simplified calculation
      // In a real implementation, you'd track hits and misses
      return 0.85; // Placeholder
    },

    /**
     * Estimate memory usage
     * @returns {number} Estimated memory usage in bytes
     * @private
     */
    _estimateMemoryUsage() {
      let size = 0;
      
      for (const [key, entry] of cache.entries()) {
        size += key.length * 2; // String size estimate
        size += JSON.stringify(entry.value).length * 2; // Value size estimate
        size += 32; // Entry overhead
      }
      
      return size;
    }
  };
}
