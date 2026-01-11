/**
 * @file Temporal Query Cache - LRU Cache for Time-Travel Queries
 * @module @unrdf/kgc-4d/temporal-cache
 * @description LRU cache for temporal SPARQL queries with configurable size and TTL
 *
 * Performance target: >80% cache hit rate for repeated queries
 *
 * @example
 * import { TemporalCache } from './temporal-cache.mjs';
 *
 * const cache = new TemporalCache({ maxSize: 100, ttl: 300000 });
 * cache.set('key', { results: [] });
 * const cached = cache.get('key');
 */

import { blake3 } from 'hash-wasm';
import { TemporalCacheConfigSchema } from './schemas/temporal-sparql-schema.mjs';

/**
 * LRU Cache for temporal queries
 *
 * Caches query results by (query, timestamp) tuple with automatic eviction
 * of least recently used entries when capacity is exceeded.
 */
export class TemporalCache {
  /**
   * Create temporal cache
   *
   * @param {Object} [config] - Cache configuration
   * @param {number} [config.maxSize=1000] - Maximum cache entries
   * @param {number} [config.ttl=300000] - Time-to-live in milliseconds (5 min default)
   * @param {boolean} [config.enabled=true] - Enable/disable caching
   *
   * @example
   * const cache = new TemporalCache({ maxSize: 100, ttl: 60000 });
   */
  constructor(config = {}) {
    // Validate config with Zod
    const validatedConfig = TemporalCacheConfigSchema.parse(config);

    this.maxSize = validatedConfig.maxSize;
    this.ttl = validatedConfig.ttl;
    this.enabled = validatedConfig.enabled;

    // Cache storage: Map for O(1) access
    this.cache = new Map();

    // LRU ordering: most recent at end
    this.accessOrder = [];

    // Metrics
    this.hits = 0;
    this.misses = 0;
    this.evictions = 0;
  }

  /**
   * Generate cache key from query and timestamp
   *
   * @param {string} baseSparql - Base SPARQL query
   * @param {bigint} timestampNs - Timestamp in nanoseconds
   * @returns {Promise<string>} Cache key (BLAKE3 hash)
   *
   * @example
   * const key = await cache.generateKey('SELECT * WHERE { ?s ?p ?o }', 123456789n);
   */
  async generateKey(baseSparql, timestampNs) {
    const composite = `${baseSparql}|${timestampNs.toString()}`;
    const hash = await blake3(composite);
    return hash;
  }

  /**
   * Get cached result
   *
   * @param {string} key - Cache key
   * @returns {Object|null} Cached result or null if miss
   *
   * @example
   * const result = cache.get('abc123...');
   */
  get(key) {
    if (!this.enabled) {
      this.misses++;
      return null;
    }

    const entry = this.cache.get(key);

    if (!entry) {
      this.misses++;
      return null;
    }

    // Check TTL
    const now = Date.now();
    if (now - entry.timestamp > this.ttl) {
      // Expired - remove
      this.cache.delete(key);
      this.accessOrder = this.accessOrder.filter(k => k !== key);
      this.misses++;
      return null;
    }

    // Hit - update access order
    this.hits++;
    this._updateAccessOrder(key);

    return entry.value;
  }

  /**
   * Set cached result
   *
   * @param {string} key - Cache key
   * @param {Object} value - Value to cache
   *
   * @example
   * cache.set('abc123...', { results: [], metadata: {} });
   */
  set(key, value) {
    if (!this.enabled) {
      return;
    }

    // Check if update or insert
    if (this.cache.has(key)) {
      // Update existing
      this.cache.set(key, {
        value,
        timestamp: Date.now(),
      });
      this._updateAccessOrder(key);
    } else {
      // Insert new - check capacity
      if (this.cache.size >= this.maxSize) {
        this._evictLRU();
      }

      this.cache.set(key, {
        value,
        timestamp: Date.now(),
      });
      this.accessOrder.push(key);
    }
  }

  /**
   * Update access order for LRU
   * @private
   */
  _updateAccessOrder(key) {
    // Remove from current position
    this.accessOrder = this.accessOrder.filter(k => k !== key);
    // Add to end (most recent)
    this.accessOrder.push(key);
  }

  /**
   * Evict least recently used entry
   * @private
   */
  _evictLRU() {
    if (this.accessOrder.length === 0) {
      return;
    }

    // First element is least recently used
    const lruKey = this.accessOrder.shift();
    this.cache.delete(lruKey);
    this.evictions++;
  }

  /**
   * Clear all cached entries
   *
   * @example
   * cache.clear();
   */
  clear() {
    this.cache.clear();
    this.accessOrder = [];
  }

  /**
   * Get cache statistics
   *
   * @returns {Object} Cache statistics
   *
   * @example
   * const stats = cache.getStats();
   * console.log(`Hit rate: ${stats.hitRate}%`);
   */
  getStats() {
    const total = this.hits + this.misses;
    const hitRate = total > 0 ? (this.hits / total) * 100 : 0;

    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hits: this.hits,
      misses: this.misses,
      evictions: this.evictions,
      hitRate: hitRate.toFixed(2),
      total,
    };
  }

  /**
   * Reset cache statistics
   *
   * @example
   * cache.resetStats();
   */
  resetStats() {
    this.hits = 0;
    this.misses = 0;
    this.evictions = 0;
  }

  /**
   * Check if cache has key
   *
   * @param {string} key - Cache key
   * @returns {boolean} True if key exists and not expired
   *
   * @example
   * if (cache.has('abc123...')) { }
   */
  has(key) {
    if (!this.enabled) {
      return false;
    }

    const entry = this.cache.get(key);
    if (!entry) {
      return false;
    }

    // Check TTL
    const now = Date.now();
    if (now - entry.timestamp > this.ttl) {
      // Expired
      this.cache.delete(key);
      this.accessOrder = this.accessOrder.filter(k => k !== key);
      return false;
    }

    return true;
  }

  /**
   * Get current cache size
   *
   * @returns {number} Number of cached entries
   *
   * @example
   * const size = cache.size();
   */
  size() {
    return this.cache.size;
  }
}
