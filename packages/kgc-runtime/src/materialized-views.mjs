/**
 * @fileoverview Materialized Views - Cached SPARQL Query Results
 * Cache query results as projections with TTL and invalidation
 */

import { z } from 'zod';
import { generateReceipt } from './receipt.mjs';

/**
 * Materialized view schema
 */
export const MaterializedViewSchema = z.object({
  id: z.string(),
  query: z.string(),
  projection: z.any(),
  createdAt: z.number(),
  expiresAt: z.number(),
  hits: z.number().default(0),
  lastAccessedAt: z.number().optional(),
});

/**
 * @typedef {z.infer<typeof MaterializedViewSchema>} MaterializedView
 */

/**
 * View cache configuration
 * @typedef {{
 *   defaultTTL?: number,
 *   maxSize?: number,
 *   evictionPolicy?: 'lru' | 'lfu' | 'fifo',
 * }} ViewCacheConfig
 */

/**
 * View cache for materialized SPARQL query results
 */
export class ViewCache {
  /**
   * @param {ViewCacheConfig} [config={}] - Cache configuration
   */
  constructor(config = {}) {
    this.config = {
      defaultTTL: config.defaultTTL ?? 300000, // 5 minutes
      maxSize: config.maxSize ?? 1000,
      evictionPolicy: config.evictionPolicy ?? 'lru',
    };

    /** @type {Map<string, MaterializedView>} */
    this.views = new Map();

    /** @type {Map<string, Set<string>>} */
    this.dependencies = new Map();
  }

  /**
   * Create or update materialized view
   * @param {string} id - View ID
   * @param {string} query - SPARQL query
   * @param {any} projection - Query result projection
   * @param {object} [options] - View options
   * @param {number} [options.ttl] - Time to live in milliseconds
   * @param {string[]} [options.dependsOn] - Resource dependencies for invalidation
   * @returns {MaterializedView} Created view
   */
  materialize(id, query, projection, options = {}) {
    const now = Date.now();
    const ttl = options.ttl ?? this.config.defaultTTL;

    // Check capacity and evict if needed
    if (this.views.size >= this.config.maxSize && !this.views.has(id)) {
      this._evict();
    }

    const view = MaterializedViewSchema.parse({
      id,
      query,
      projection,
      createdAt: now,
      expiresAt: now + ttl,
      hits: 0,
    });

    this.views.set(id, view);

    // Register dependencies
    if (options.dependsOn) {
      for (const resource of options.dependsOn) {
        if (!this.dependencies.has(resource)) {
          this.dependencies.set(resource, new Set());
        }
        this.dependencies.get(resource).add(id);
      }
    }

    return view;
  }

  /**
   * Get materialized view
   * @param {string} id - View ID
   * @returns {MaterializedView | null} View or null if not found/expired
   */
  get(id) {
    const view = this.views.get(id);

    if (!view) {
      return null;
    }

    const now = Date.now();

    // Check expiration
    if (now >= view.expiresAt) {
      this.views.delete(id);
      return null;
    }

    // Update access statistics
    view.hits++;
    view.lastAccessedAt = now;

    return view;
  }

  /**
   * Get projection from view
   * @param {string} id - View ID
   * @returns {any | null} Projection or null
   */
  getProjection(id) {
    const view = this.get(id);
    return view ? view.projection : null;
  }

  /**
   * Invalidate view by ID
   * @param {string} id - View ID
   * @returns {boolean} True if view was invalidated
   */
  invalidate(id) {
    return this.views.delete(id);
  }

  /**
   * Invalidate views by resource dependency
   * @param {string} resource - Resource that changed
   * @returns {number} Number of views invalidated
   */
  invalidateByResource(resource) {
    const viewIds = this.dependencies.get(resource);

    if (!viewIds) {
      return 0;
    }

    let count = 0;
    for (const viewId of viewIds) {
      if (this.views.delete(viewId)) {
        count++;
      }
    }

    this.dependencies.delete(resource);
    return count;
  }

  /**
   * Invalidate all views
   * @returns {number} Number of views invalidated
   */
  invalidateAll() {
    const count = this.views.size;
    this.views.clear();
    this.dependencies.clear();
    return count;
  }

  /**
   * Refresh view with new projection
   * @param {string} id - View ID
   * @param {any} projection - New projection
   * @param {object} [options] - Refresh options
   * @param {number} [options.ttl] - New TTL
   * @returns {MaterializedView | null} Refreshed view or null if not found
   */
  refresh(id, projection, options = {}) {
    const view = this.views.get(id);

    if (!view) {
      return null;
    }

    const now = Date.now();
    const ttl = options.ttl ?? this.config.defaultTTL;

    view.projection = projection;
    view.createdAt = now;
    view.expiresAt = now + ttl;

    return view;
  }

  /**
   * Evict view according to eviction policy
   * @returns {string | null} Evicted view ID or null
   * @private
   */
  _evict() {
    if (this.views.size === 0) {
      return null;
    }

    let victimId = null;

    switch (this.config.evictionPolicy) {
      case 'lru': // Least Recently Used
        victimId = this._evictLRU();
        break;
      case 'lfu': // Least Frequently Used
        victimId = this._evictLFU();
        break;
      case 'fifo': // First In First Out
        victimId = this._evictFIFO();
        break;
    }

    if (victimId) {
      this.views.delete(victimId);
    }

    return victimId;
  }

  /**
   * Evict least recently used view
   * @returns {string | null} Evicted view ID
   * @private
   */
  _evictLRU() {
    let oldestTime = Infinity;
    let victimId = null;

    for (const [id, view] of this.views.entries()) {
      const lastAccess = view.lastAccessedAt ?? view.createdAt;
      if (lastAccess < oldestTime) {
        oldestTime = lastAccess;
        victimId = id;
      }
    }

    return victimId;
  }

  /**
   * Evict least frequently used view
   * @returns {string | null} Evicted view ID
   * @private
   */
  _evictLFU() {
    let lowestHits = Infinity;
    let victimId = null;

    for (const [id, view] of this.views.entries()) {
      if (view.hits < lowestHits) {
        lowestHits = view.hits;
        victimId = id;
      }
    }

    return victimId;
  }

  /**
   * Evict oldest view (FIFO)
   * @returns {string | null} Evicted view ID
   * @private
   */
  _evictFIFO() {
    let oldestTime = Infinity;
    let victimId = null;

    for (const [id, view] of this.views.entries()) {
      if (view.createdAt < oldestTime) {
        oldestTime = view.createdAt;
        victimId = id;
      }
    }

    return victimId;
  }

  /**
   * Clean up expired views
   * @returns {number} Number of expired views removed
   */
  cleanupExpired() {
    const now = Date.now();
    let count = 0;

    for (const [id, view] of this.views.entries()) {
      if (now >= view.expiresAt) {
        this.views.delete(id);
        count++;
      }
    }

    return count;
  }

  /**
   * Get cache statistics
   * @returns {object} Cache statistics
   */
  getStats() {
    const now = Date.now();
    let totalHits = 0;
    let expired = 0;

    for (const view of this.views.values()) {
      totalHits += view.hits;
      if (now >= view.expiresAt) {
        expired++;
      }
    }

    return {
      size: this.views.size,
      maxSize: this.config.maxSize,
      totalHits,
      expired,
      evictionPolicy: this.config.evictionPolicy,
      defaultTTL: this.config.defaultTTL,
    };
  }

  /**
   * Generate receipt for cache operation
   * @param {string} operation - Operation name
   * @param {Record<string, any>} inputs - Operation inputs
   * @param {Record<string, any>} outputs - Operation outputs
   * @returns {Promise<import('./receipt.mjs').Receipt>} Receipt
   */
  async generateReceipt(operation, inputs, outputs) {
    return generateReceipt(
      `view-cache:${operation}`,
      inputs,
      { ...outputs, stats: this.getStats() }
    );
  }
}

/**
 * Create view cache with automatic cleanup
 * @param {ViewCacheConfig} [config] - Cache configuration
 * @param {number} [cleanupInterval=60000] - Cleanup interval in ms
 * @returns {ViewCache} View cache with auto-cleanup
 */
export function createAutoCleanupCache(config, cleanupInterval = 60000) {
  const cache = new ViewCache(config);

  // Set up automatic cleanup
  const intervalId = setInterval(() => {
    cache.cleanupExpired();
  }, cleanupInterval);

  // Allow cleanup to be stopped
  cache.stopAutoCleanup = () => clearInterval(intervalId);

  return cache;
}
