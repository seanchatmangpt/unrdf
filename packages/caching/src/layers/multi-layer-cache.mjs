/**
 * @file Multi-Layer Cache - 3-tier caching system (L1: Memory, L2: Redis, L3: Store)
 * @module @unrdf/caching/layers
 *
 * Provides high-performance distributed caching with:
 * - L1: In-memory LRU cache (fastest, process-local)
 * - L2: Redis distributed cache (shared across instances)
 * - L3: Persistent RDF store (Oxigraph)
 *
 * @example
 * import { MultiLayerCache } from '@unrdf/caching/layers';
 * import { createStore } from '@unrdf/oxigraph';
 *
 * const cache = new MultiLayerCache({
 *   store: createStore(),
 *   redisUrl: 'redis://localhost:6379',
 *   l1MaxSize: 1000,
 *   l2TtlSeconds: 300
 * });
 *
 * const result = await cache.get('sparql:query:hash123');
 */

import { LRUCache } from 'lru-cache';
import Redis from 'ioredis';
import { pack, unpack } from 'msgpackr';
import { z } from 'zod';

// =============================================================================
// SCHEMAS & TYPES
// =============================================================================

/**
 * Cache configuration schema
 */
const CacheConfigSchema = z.object({
  store: z.any(), // OxigraphStore instance
  redisUrl: z.string().optional().default('redis://localhost:6379'),
  l1MaxSize: z.number().int().positive().default(1000),
  l1TtlMs: z.number().int().positive().default(60000), // 1 minute
  l2TtlSeconds: z.number().int().positive().default(300), // 5 minutes
  enableL2: z.boolean().default(true),
  keyPrefix: z.string().default('unrdf:cache:'),
});

/**
 * @typedef {z.infer<typeof CacheConfigSchema>} CacheConfig
 */

/**
 * Cache statistics
 * @typedef {Object} CacheStats
 * @property {number} l1Hits - L1 cache hits
 * @property {number} l1Misses - L1 cache misses
 * @property {number} l2Hits - L2 cache hits
 * @property {number} l2Misses - L2 cache misses
 * @property {number} l3Hits - L3 store hits
 * @property {number} l3Misses - L3 store misses
 * @property {number} sets - Total cache sets
 * @property {number} deletes - Total cache deletes
 * @property {number} l1Size - Current L1 cache size
 */

// =============================================================================
// MULTI-LAYER CACHE
// =============================================================================

/**
 * Multi-layer cache with L1 (LRU), L2 (Redis), L3 (Store)
 */
export class MultiLayerCache {
  /**
   * Create a new multi-layer cache
   * @param {Partial<CacheConfig>} config - Cache configuration
   */
  constructor(config = {}) {
    const validated = CacheConfigSchema.parse(config);

    this.store = validated.store;
    this.config = validated;

    // L1: In-memory LRU cache
    this.l1 = new LRUCache({
      max: validated.l1MaxSize,
      ttl: validated.l1TtlMs,
      updateAgeOnGet: true,
      updateAgeOnHas: true,
    });

    // L2: Redis distributed cache (optional)
    this.l2 = validated.enableL2
      ? new Redis(validated.redisUrl, {
          lazyConnect: true,
          enableOfflineQueue: false,
          maxRetriesPerRequest: 1,
        })
      : null;

    // Statistics
    this.stats = {
      l1Hits: 0,
      l1Misses: 0,
      l2Hits: 0,
      l2Misses: 0,
      l3Hits: 0,
      l3Misses: 0,
      sets: 0,
      deletes: 0,
      l1Size: 0,
    };

    // Connection state
    this.l2Connected = false;

    // Connect to Redis asynchronously
    if (this.l2) {
      this._connectRedis();
    }
  }

  /**
   * Connect to Redis
   * @private
   */
  async _connectRedis() {
    if (!this.l2) return;

    try {
      await this.l2.connect();
      this.l2Connected = true;
    } catch (error) {
      console.warn('[MultiLayerCache] Redis connection failed, L2 disabled:', error.message);
      this.l2Connected = false;
    }
  }

  /**
   * Get value from cache (checks L1 → L2 → L3)
   * @param {string} key - Cache key
   * @param {Function} [fetcher] - Function to fetch value if not cached
   * @returns {Promise<any>} Cached or fetched value
   */
  async get(key, fetcher = null) {
    const prefixedKey = this.config.keyPrefix + key;

    // Check L1 (memory)
    if (this.l1.has(key)) {
      this.stats.l1Hits++;
      return this.l1.get(key);
    }

    this.stats.l1Misses++;

    // Check L2 (Redis)
    if (this.l2Connected) {
      try {
        const l2Value = await this.l2.getBuffer(prefixedKey);
        if (l2Value) {
          this.stats.l2Hits++;
          const unpacked = unpack(l2Value);

          // Promote to L1
          this.l1.set(key, unpacked);
          this.stats.l1Size = this.l1.size;

          return unpacked;
        }
      } catch (error) {
        // Redis error, fall through to L3
        console.warn('[MultiLayerCache] L2 get error:', error.message);
      }
    }

    this.stats.l2Misses++;

    // Check L3 (Store) or use fetcher
    if (fetcher) {
      try {
        const value = await fetcher();

        if (value !== null && value !== undefined) {
          this.stats.l3Hits++;

          // Populate caches
          await this.set(key, value);

          return value;
        }

        this.stats.l3Misses++;
        return null;
      } catch (error) {
        this.stats.l3Misses++;
        throw error;
      }
    }

    this.stats.l3Misses++;
    return null;
  }

  /**
   * Set value in all cache layers
   * @param {string} key - Cache key
   * @param {any} value - Value to cache
   * @param {Object} [options] - Set options
   * @param {number} [options.ttl] - TTL in seconds (for L2)
   * @returns {Promise<void>}
   */
  async set(key, value, options = {}) {
    const prefixedKey = this.config.keyPrefix + key;
    const ttl = options.ttl || this.config.l2TtlSeconds;

    this.stats.sets++;

    // Set in L1 (memory)
    this.l1.set(key, value);
    this.stats.l1Size = this.l1.size;

    // Set in L2 (Redis)
    if (this.l2Connected) {
      try {
        const packed = pack(value);
        await this.l2.setex(prefixedKey, ttl, packed);
      } catch (error) {
        console.warn('[MultiLayerCache] L2 set error:', error.message);
      }
    }

    // L3 (Store) is the source of truth, not cached
  }

  /**
   * Delete value from all cache layers
   * @param {string} key - Cache key
   * @returns {Promise<void>}
   */
  async delete(key) {
    const prefixedKey = this.config.keyPrefix + key;

    this.stats.deletes++;

    // Delete from L1
    this.l1.delete(key);
    this.stats.l1Size = this.l1.size;

    // Delete from L2
    if (this.l2Connected) {
      try {
        await this.l2.del(prefixedKey);
      } catch (error) {
        console.warn('[MultiLayerCache] L2 delete error:', error.message);
      }
    }
  }

  /**
   * Delete multiple keys matching a pattern
   * @param {string} pattern - Key pattern (e.g., 'sparql:*')
   * @returns {Promise<number>} Number of keys deleted
   */
  async deletePattern(pattern) {
    let count = 0;

    // Delete from L1 (iterate and match)
    for (const key of this.l1.keys()) {
      if (this._matchPattern(key, pattern)) {
        this.l1.delete(key);
        count++;
      }
    }
    this.stats.l1Size = this.l1.size;

    // Delete from L2 (use SCAN)
    if (this.l2Connected) {
      try {
        const prefixedPattern = this.config.keyPrefix + pattern;
        const keys = await this._scanKeys(prefixedPattern);

        if (keys.length > 0) {
          await this.l2.del(...keys);
          count += keys.length;
        }
      } catch (error) {
        console.warn('[MultiLayerCache] L2 pattern delete error:', error.message);
      }
    }

    this.stats.deletes += count;
    return count;
  }

  /**
   * Scan Redis keys matching pattern
   * @private
   * @param {string} pattern - Redis key pattern
   * @returns {Promise<Array<string>>} Matching keys
   */
  async _scanKeys(pattern) {
    const keys = [];
    let cursor = '0';

    do {
      const [nextCursor, batch] = await this.l2.scan(
        cursor,
        'MATCH',
        pattern,
        'COUNT',
        100
      );

      cursor = nextCursor;
      keys.push(...batch);
    } while (cursor !== '0');

    return keys;
  }

  /**
   * Match key against pattern (simple glob-style)
   * @private
   * @param {string} key - Key to match
   * @param {string} pattern - Pattern with * wildcards
   * @returns {boolean} True if matches
   */
  _matchPattern(key, pattern) {
    const regex = new RegExp(
      '^' + pattern.replace(/\*/g, '.*') + '$'
    );
    return regex.test(key);
  }

  /**
   * Clear all cache layers
   * @returns {Promise<void>}
   */
  async clear() {
    // Clear L1
    this.l1.clear();
    this.stats.l1Size = 0;

    // Clear L2 (delete all keys with prefix)
    if (this.l2Connected) {
      try {
        const pattern = this.config.keyPrefix + '*';
        const keys = await this._scanKeys(pattern);

        if (keys.length > 0) {
          await this.l2.del(...keys);
        }
      } catch (error) {
        console.warn('[MultiLayerCache] L2 clear error:', error.message);
      }
    }
  }

  /**
   * Get cache statistics
   * @returns {CacheStats} Current statistics
   */
  getStats() {
    return {
      ...this.stats,
      l1Size: this.l1.size,
      l1HitRate: this.stats.l1Hits / (this.stats.l1Hits + this.stats.l1Misses) || 0,
      l2HitRate: this.stats.l2Hits / (this.stats.l2Hits + this.stats.l2Misses) || 0,
      l3HitRate: this.stats.l3Hits / (this.stats.l3Hits + this.stats.l3Misses) || 0,
      overallHitRate:
        (this.stats.l1Hits + this.stats.l2Hits) /
        (this.stats.l1Hits + this.stats.l1Misses + this.stats.l2Hits + this.stats.l2Misses) || 0,
    };
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      l1Hits: 0,
      l1Misses: 0,
      l2Hits: 0,
      l2Misses: 0,
      l3Hits: 0,
      l3Misses: 0,
      sets: 0,
      deletes: 0,
      l1Size: this.l1.size,
    };
  }

  /**
   * Close connections
   * @returns {Promise<void>}
   */
  async close() {
    if (this.l2Connected && this.l2) {
      await this.l2.quit();
      this.l2Connected = false;
    }
  }
}

/**
 * Create a multi-layer cache instance
 * @param {Partial<CacheConfig>} config - Cache configuration
 * @returns {MultiLayerCache} Cache instance
 */
export function createMultiLayerCache(config = {}) {
  return new MultiLayerCache(config);
}

export default {
  MultiLayerCache,
  createMultiLayerCache,
};
