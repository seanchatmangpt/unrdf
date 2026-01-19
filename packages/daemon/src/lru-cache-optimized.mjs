/**
 * @file Optimized LRU Cache Implementation
 * @module @unrdf/daemon/lru-cache-optimized
 * @description High-performance LRU cache with O(1) get/set/eviction operations
 * using doubly-linked list and HashMap for production use
 */

/**
 * High-performance LRU cache using Map for O(1) operations
 * Guarantees O(1) get, set, and eviction operations with minimal overhead
 * Uses Map insertion order (ES6+) for efficient LRU tracking
 * @private
 */
export class OptimizedLRUCache {
  /**
   * @param {number} maxSize - Maximum cache size
   * @param {Object} [options] - Cache options
   * @param {boolean} [options.compactMetadata=true] - Store minimal metadata
   */
  constructor(maxSize = 1000, options = {}) {
    this.maxSize = maxSize;
    this.compactMetadata = options.compactMetadata !== false;
    this.cache = new Map(); // Maintains insertion order in ES6+
    this.stats = {
      hits: 0,
      misses: 0,
      evictions: 0,
    };
  }

  /**
   * Set value in cache with O(1) performance
   * @param {string} key - Cache key
   * @param {*} value - Value to store
   */
  set(key, value) {
    // If key exists, delete it first to update its position
    if (this.cache.has(key)) {
      this.cache.delete(key);
    }

    // Add to cache (moves to end = most recent)
    this.cache.set(key, value);

    // Evict least recently used if over capacity
    if (this.cache.size > this.maxSize) {
      // First key is the oldest (least recently used)
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
      this.stats.evictions += 1;
    }
  }

  /**
   * Get value from cache with O(1) performance
   * @param {string} key - Cache key
   * @returns {*} Cached value or undefined
   */
  get(key) {
    if (!this.cache.has(key)) {
      this.stats.misses += 1;
      return undefined;
    }

    // Move to end (most recently used) by deleting and re-adding
    const value = this.cache.get(key);
    this.cache.delete(key);
    this.cache.set(key, value);

    this.stats.hits += 1;
    return value;
  }

  /**
   * Check if key exists - O(1) operation
   * @param {string} key - Cache key
   * @returns {boolean} Whether key exists
   */
  has(key) {
    return this.cache.has(key);
  }

  /**
   * Get cache size
   * @returns {number} Current cache size
   */
  size() {
    return this.cache.size;
  }

  /**
   * Get cache statistics
   * @returns {Object} Hit/miss/eviction stats
   */
  getStats() {
    return {
      ...this.stats,
      size: this.cache.size,
      hitRate: this.stats.hits / (this.stats.hits + this.stats.misses) || 0,
    };
  }

  /**
   * Get all entries as array
   * @returns {Array} Array of [key, value] pairs
   */
  entries() {
    return Array.from(this.cache.entries());
  }

  /**
   * Clear cache
   */
  clear() {
    this.cache.clear();
  }
}
