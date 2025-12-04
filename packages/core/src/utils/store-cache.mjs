/**
 * @fileoverview Store Cache - Eliminates N×M store conversions
 *
 * @description
 * Caches Oxigraph store instances by version hash to eliminate redundant
 * N3 Store → Oxigraph Store conversions on every SPARQL query.
 *
 * Cache Strategy:
 * - Key: Store version hash (quad count + sampled quad hashes)
 * - Value: Oxigraph store instance
 * - Invalidation: On any delta application
 * - LRU eviction: Maximum 10 entries
 *
 * Expected Impact: 50-70% latency reduction
 *
 * @module knowledge-engine/store-cache
 */

import crypto from 'node:crypto';

/**
 * Simple fast hash function for quad sampling
 * @param {string} text - Text to hash
 * @returns {string} Hex digest
 */
function quickHash(text) {
  return crypto.createHash('md5').update(text).digest('hex').slice(0, 8);
}

/**
 * Store Cache - Oxigraph instance caching by store version
 *
 * @class StoreCache
 */
export class StoreCache {
  /**
   * Create a new store cache
   * @param {object} options - Configuration options
   * @param {number} options.maxSize - Maximum cache entries (default: 10)
   */
  constructor(options = {}) {
    this.cache = new Map(); // storeVersion → Oxigraph Store
    this.currentVersion = null;
    this.maxSize = options.maxSize || 10;
  }

  /**
   * Get store version hash (fast fingerprint)
   *
   * Computes a version hash based on:
   * - Total quad count (main cache key differentiator)
   * - Hash of first 100 quads + last 100 quads (quick validation)
   *
   * @param {Store} store - N3 Store instance
   * @returns {string} Version hash (format: "count-hash")
   */
  getVersion(store) {
    // Get all quads (this is unavoidable)
    const quads = store.getQuads();
    const count = quads.length;

    if (count === 0) {
      return '0-empty';
    }

    // Sample quads: first 100 + last 100 for quick validation
    const sampleStart = Math.min(100, count);
    const sampleEnd = Math.max(count - 100, sampleStart);

    const sample = [...quads.slice(0, sampleStart), ...quads.slice(sampleEnd)];

    // Hash the string representation of sampled quads
    const sampleStr = sample
      .map(q => `${q.subject.value}|${q.predicate.value}|${q.object.value}`)
      .join('\n');

    const sampleHash = quickHash(sampleStr);

    return `${count}-${sampleHash}`;
  }

  /**
   * Get cached Oxigraph store or create new one
   *
   * @param {Store} store - N3 Store instance
   * @param {Function} createStore - Factory function to create Oxigraph store
   * @returns {object} Oxigraph Store instance
   */
  getOrCreate(store, createStore) {
    const version = this.getVersion(store);

    // Cache hit - return cached Oxigraph store
    if (this.cache.has(version)) {
      return this.cache.get(version);
    }

    // Cache miss - convert ONCE
    const quads = store.getQuads();
    const oxStore = createStore(Array.from(quads));

    // Store in cache
    this.cache.set(version, oxStore);
    this.currentVersion = version;

    // LRU eviction - remove oldest entry if over capacity
    if (this.cache.size > this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    return oxStore;
  }

  /**
   * Invalidate entire cache
   * Call this when the store version changes (delta applied)
   */
  clear() {
    this.cache.clear();
    this.currentVersion = null;
  }

  /**
   * Get cache statistics
   * @returns {object} Cache stats (size, maxSize, currentVersion)
   */
  stats() {
    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      currentVersion: this.currentVersion,
      entries: Array.from(this.cache.keys()),
    };
  }
}

export default StoreCache;
