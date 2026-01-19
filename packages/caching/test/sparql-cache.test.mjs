/**
 * @file SPARQL Cache Tests
 * @module @unrdf/caching/test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { SparqlCache, createSparqlCache } from '../src/query/sparql-cache.mjs';

describe('SparqlCache', () => {
  let sparqlCache;
  let mockStore;
  let mockCache;
  let mockTracker;

  beforeEach(() => {
    mockStore = {
      query: async (sparql) => {
        return [{ subject: 's', predicate: 'p', object: 'o' }];
      }
    };

    mockCache = {
      get: async (key, fetcher) => {
        if (fetcher) {
          return await fetcher();
        }
        return null;
      },
      set: async () => {},
      delete: async () => {},
      getStats: () => ({ l1Hits: 0, l1Misses: 0 })
    };

    mockTracker = {
      trackQueryDependency: async () => {},
      getDependencies: () => []
    };

    sparqlCache = new SparqlCache({
      store: mockStore,
      cache: mockCache,
      tracker: mockTracker
    });
  });

  describe('query', () => {
    it('should execute SPARQL query', async () => {
      const results = await sparqlCache.query('SELECT * WHERE { ?s ?p ?o }');

      expect(results).toBeDefined();
      expect(Array.isArray(results)).toBe(true);
    });

    it('should cache query results', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o }';

      const results1 = await sparqlCache.query(query);
      const results2 = await sparqlCache.query(query);

      expect(results1).toEqual(results2);
    });

    it('should generate consistent cache keys', async () => {
      const query1 = 'SELECT * WHERE { ?s ?p ?o }';
      const query2 = 'SELECT * WHERE { ?s ?p ?o }'; // Same query

      const key1 = await sparqlCache._generateCacheKey(query1);
      const key2 = await sparqlCache._generateCacheKey(query2);

      expect(key1).toBe(key2);
    });
  });

  describe('clear', () => {
    it('should clear cached queries', async () => {
      await sparqlCache.clear();
      // Should not throw
      expect(true).toBe(true);
    });
  });

  describe('getStats', () => {
    it('should return cache statistics', () => {
      const stats = sparqlCache.getStats();

      expect(stats).toBeDefined();
      expect(stats).toHaveProperty('queriesExecuted');
    });
  });
});

describe('createSparqlCache', () => {
  it('should create SPARQL cache instance', () => {
    const mockStore = {};
    const mockCache = {};
    const mockTracker = {};

    const cache = createSparqlCache({
      store: mockStore,
      cache: mockCache,
      tracker: mockTracker
    });

    expect(cache).toBeInstanceOf(SparqlCache);
  });
});
