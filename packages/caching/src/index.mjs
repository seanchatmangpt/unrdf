/**
 * @file UNRDF Caching - Multi-layer caching system for RDF queries
 * @module @unrdf/caching
 *
 * High-performance distributed caching for RDF queries with:
 * - Multi-layer cache (L1: Memory, L2: Redis, L3: Store)
 * - Smart dependency tracking and invalidation
 * - SPARQL query result caching
 * - Semantic cache key generation
 *
 * @example
 * import { createCachingSystem } from '@unrdf/caching';
 * import { createStore } from '@unrdf/oxigraph';
 *
 * const store = createStore();
 * const caching = createCachingSystem({ store });
 *
 * // Execute cached query
 * const results = await caching.sparqlCache.query('SELECT * WHERE { ?s ?p ?o }');
 *
 * // Invalidate when data changes
 * await caching.tracker.invalidateSubject('http://example.org/resource1');
 */

export { MultiLayerCache, createMultiLayerCache } from './layers/multi-layer-cache.mjs';
export { DependencyTracker, createDependencyTracker, extractQuerySubjects } from './invalidation/dependency-tracker.mjs';
export { SparqlCache, createSparqlCache } from './query/sparql-cache.mjs';

/**
 * Create a complete caching system with all components
 * @param {Object} config - System configuration
 * @param {any} config.store - OxigraphStore instance
 * @param {string} [config.redisUrl] - Redis connection URL
 * @param {number} [config.l1MaxSize] - L1 cache max size
 * @param {number} [config.l2TtlSeconds] - L2 cache TTL
 * @returns {Promise<Object>} Caching system with cache, tracker, and sparqlCache
 */
export async function createCachingSystem(config = {}) {
  const { MultiLayerCache } = await import('./layers/multi-layer-cache.mjs');
  const { DependencyTracker } = await import('./invalidation/dependency-tracker.mjs');
  const { SparqlCache } = await import('./query/sparql-cache.mjs');

  const cache = new MultiLayerCache(config);
  const tracker = new DependencyTracker(cache);
  const sparqlCache = new SparqlCache({
    store: config.store,
    cache,
    tracker,
  });

  return {
    cache,
    tracker,
    sparqlCache,

    /**
     * Get combined statistics
     */
    getStats() {
      return {
        cache: cache.getStats(),
        tracker: tracker.getStats(),
        sparql: sparqlCache.getStats(),
      };
    },

    /**
     * Clear all caches
     */
    async clear() {
      await cache.clear();
      tracker.clear();
      await sparqlCache.clear();
    },

    /**
     * Close connections
     */
    async close() {
      await cache.close();
    },
  };
}

export default {
  createCachingSystem,
};
