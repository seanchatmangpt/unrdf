/**
 * @fileoverview Store Cache - Oxigraph store caching for performance optimization
 *
 * @description
 * Caches Oxigraph store conversions to eliminate redundant N3 → Oxigraph transformations.
 *
 * Cache Strategy:
 * - Key: N3 store hash based on size and version
 * - Value: Oxigraph store instance
 * - Invalidation: Manual clear when store changes
 * - Max Size: LRU eviction when cache exceeds maxSize
 *
 * Expected Impact: 50-70% latency reduction
 *
 * @module knowledge-engine/store-cache
 */

import { createHash } from 'crypto';

/**
 * Store Cache - Caches Oxigraph store instances
 *
 * @class StoreCache
 */
export class StoreCache {
  /**
   * Create a new store cache
   * @param {object} options - Configuration options
   * @param {number} options.maxSize - Maximum cached stores (default: 10)
   */
  constructor(options = {}) {
    this.cache = new Map(); // storeVersion → Oxigraph store
    this.maxSize = options.maxSize || 10;
    this.accessOrder = []; // LRU tracking
  }

  /**
   * Get cached Oxigraph store or create new one
   *
   * @param {object} n3Store - N3 Store instance
   * @param {Function} createStore - Factory function to create Oxigraph store
   * @returns {object} Oxigraph store instance
   */
  getOrCreate(n3Store, createStore) {
    if (!n3Store || !createStore) {
      throw new Error('StoreCache: n3Store and createStore are required');
    }

    const version = this.getVersion(n3Store);

    // Check cache first
    if (this.cache.has(version)) {
      this._updateLRU(version);
      return this.cache.get(version);
    }

    // Create new Oxigraph store
    const oxStore = this._convertStore(n3Store, createStore);

    // Add to cache with LRU eviction
    this._addToCache(version, oxStore);

    return oxStore;
  }

  /**
   * Get store version identifier
   *
   * Uses store size + hash of first/last quads for quick versioning
   *
   * @param {object} n3Store - N3 Store instance
   * @returns {string} Store version hash
   */
  getVersion(n3Store) {
    if (!n3Store) {
      return 'empty';
    }

    const size = n3Store.size || 0;

    if (size === 0) {
      return 'empty';
    }

    // Quick hash based on size and sample quads
    const quads = Array.from(n3Store);
    const hash = createHash('sha1');

    hash.update(`size:${size}`);

    // Hash first and last quad for quick versioning
    if (quads.length > 0) {
      const first = quads[0];
      const last = quads[quads.length - 1];

      hash.update(this._quadToString(first));
      if (first !== last) {
        hash.update(this._quadToString(last));
      }
    }

    return hash.digest('hex');
  }

  /**
   * Convert N3 store to Oxigraph store
   *
   * @param {object} n3Store - N3 Store instance
   * @param {Function} createStore - Factory function
   * @returns {object} Oxigraph store
   * @private
   */
  _convertStore(n3Store, createStore) {
    const oxStore = createStore();

    // Copy all quads from N3 store to Oxigraph store
    for (const quad of n3Store) {
      oxStore.add(quad);
    }

    return oxStore;
  }

  /**
   * Add store to cache with LRU eviction
   *
   * @param {string} version - Store version
   * @param {object} store - Oxigraph store
   * @private
   */
  _addToCache(version, store) {
    // Evict oldest if at capacity
    if (this.cache.size >= this.maxSize && this.accessOrder.length > 0) {
      const oldest = this.accessOrder.shift();
      this.cache.delete(oldest);
    }

    this.cache.set(version, store);
    this.accessOrder.push(version);
  }

  /**
   * Update LRU access order
   *
   * @param {string} version - Store version
   * @private
   */
  _updateLRU(version) {
    const index = this.accessOrder.indexOf(version);
    if (index > -1) {
      this.accessOrder.splice(index, 1);
      this.accessOrder.push(version);
    }
  }

  /**
   * Convert quad to string for hashing
   *
   * @param {object} quad - RDF quad
   * @returns {string} Quad string representation
   * @private
   */
  _quadToString(quad) {
    return `${quad.subject?.value || ''}:${quad.predicate?.value || ''}:${quad.object?.value || ''}:${quad.graph?.value || ''}`;
  }

  /**
   * Clear entire cache
   */
  clear() {
    this.cache.clear();
    this.accessOrder = [];
  }

  /**
   * Get cache statistics
   * @returns {object} Cache stats
   */
  stats() {
    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      versions: Array.from(this.cache.keys()),
      accessOrder: [...this.accessOrder],
    };
  }
}

export default StoreCache;
