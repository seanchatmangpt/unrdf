/**
 * @fileoverview Condition Cache - Eliminates duplicate condition evaluation
 *
 * @description
 * Caches SPARQL-ASK condition evaluation results by (hookId, storeVersion)
 * to eliminate redundant re-evaluation of the same condition.
 *
 * Cache Strategy:
 * - Key: `${hookId}-${storeVersion}`
 * - Value: boolean (condition satisfied)
 * - TTL: 60s default (invalidates on store version change)
 * - Invalidation: Manual clear on store version change
 *
 * Expected Impact: 40-50% latency reduction
 *
 * @module knowledge-engine/condition-cache
 */

/**
 * Condition Cache - Caches SPARQL condition evaluation results
 *
 * @class ConditionCache
 */
export class ConditionCache {
  /**
   * Create a new condition cache
   * @param {object} options - Configuration options
   * @param {number} options.ttl - Time-to-live in milliseconds (default: 60000)
   */
  constructor(options = {}) {
    this.cache = new Map(); // `${hookId}-${storeVersion}` → { value, timestamp }
    this.ttl = options.ttl || 60000; // 60 seconds default
  }

  /**
   * Get cached condition result
   *
   * Returns undefined if:
   * - Entry doesn't exist
   * - Entry has expired (TTL exceeded)
   *
   * @param {string} hookId - Hook identifier
   * @param {string} storeVersion - Store version hash
   * @returns {boolean|undefined} Cached result or undefined
   */
  get(hookId, storeVersion) {
    if (!hookId || !storeVersion) {
      return undefined;
    }

    const key = `${hookId}-${storeVersion}`;
    const entry = this.cache.get(key);

    if (!entry) {
      return undefined;
    }

    // TTL check - if expired, remove and return undefined
    if (Date.now() - entry.timestamp > this.ttl) {
      this.cache.delete(key);
      return undefined;
    }

    return entry.value;
  }

  /**
   * Store condition result in cache
   *
   * @param {string} hookId - Hook identifier
   * @param {string} storeVersion - Store version hash
   * @param {boolean} value - Evaluation result (true/false)
   */
  set(hookId, storeVersion, value) {
    if (!hookId || !storeVersion) {
      return;
    }

    const key = `${hookId}-${storeVersion}`;
    this.cache.set(key, {
      value: Boolean(value),
      timestamp: Date.now(),
    });
  }

  /**
   * Clear entire cache (call on store version change)
   *
   * This should be called whenever the store is modified
   * (delta applied, new transaction, etc.)
   */
  clear() {
    this.cache.clear();
  }

  /**
   * Get cache statistics
   * @returns {object} Cache stats (size, ttl, entries)
   */
  stats() {
    return {
      size: this.cache.size,
      ttl: this.ttl,
      entries: Array.from(this.cache.keys()),
    };
  }
}

export default ConditionCache;
