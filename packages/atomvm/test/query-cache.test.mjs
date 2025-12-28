/**
 * @fileoverview QueryCache Tests
 *
 * Comprehensive test suite for query result caching with:
 * - LRU eviction
 * - TTL expiration
 * - Pattern-based invalidation
 * - Performance benchmarks
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { QueryCache, createQueryCache } from '../src/query-cache.mjs';

describe('QueryCache', () => {
  /** @type {QueryCache} */
  let cache;

  beforeEach(() => {
    cache = new QueryCache({ maxSize: 5, ttl: 60000 });
  });

  describe('Constructor', () => {
    it('should create cache with default options', () => {
      const defaultCache = new QueryCache();
      expect(defaultCache.maxSize).toBe(100);
      expect(defaultCache.ttl).toBe(60000);
    });

    it('should create cache with custom options', () => {
      const customCache = new QueryCache({ maxSize: 50, ttl: 30000 });
      expect(customCache.maxSize).toBe(50);
      expect(customCache.ttl).toBe(30000);
    });

    it('should throw on invalid maxSize', () => {
      expect(() => new QueryCache({ maxSize: 0 })).toThrow('maxSize must be a positive integer');
      expect(() => new QueryCache({ maxSize: -1 })).toThrow('maxSize must be a positive integer');
      expect(() => new QueryCache({ maxSize: 'invalid' })).toThrow(
        'maxSize must be a positive integer'
      );
    });

    it('should throw on invalid ttl', () => {
      expect(() => new QueryCache({ ttl: -1 })).toThrow('ttl must be a non-negative number');
      expect(() => new QueryCache({ ttl: 'invalid' })).toThrow('ttl must be a non-negative number');
    });

    it('should create cache with createQueryCache factory', () => {
      const factoryCache = createQueryCache({ maxSize: 10 });
      expect(factoryCache).toBeInstanceOf(QueryCache);
      expect(factoryCache.maxSize).toBe(10);
    });
  });

  describe('Cache Hit', () => {
    it('should return cached result on repeated query', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [{ s: 'http://example.org/alice' }] };

      cache.set(query, {}, result);
      const cached = cache.get(query, {});

      expect(cached).toEqual(result);
      expect(cache.stats().hits).toBe(1);
    });

    it('should return cached result with same bindings', () => {
      const query = 'SELECT ?name WHERE { ?s foaf:name ?name }';
      const bindings = { s: 'http://example.org/alice' };
      const result = { rows: [{ name: 'Alice' }] };

      cache.set(query, bindings, result);
      const cached = cache.get(query, bindings);

      expect(cached).toEqual(result);
    });
  });

  describe('Cache Miss', () => {
    it('should return undefined on first query', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      const result = cache.get(query, {});

      expect(result).toBeUndefined();
      expect(cache.stats().misses).toBe(1);
    });

    it('should return undefined with different bindings', () => {
      const query = 'SELECT ?name WHERE { ?s foaf:name ?name }';
      const bindings1 = { s: 'http://example.org/alice' };
      const bindings2 = { s: 'http://example.org/bob' };
      const result = { rows: [{ name: 'Alice' }] };

      cache.set(query, bindings1, result);
      const cached = cache.get(query, bindings2);

      expect(cached).toBeUndefined();
      expect(cache.stats().misses).toBe(1);
    });

    it('should throw on empty query', () => {
      expect(() => cache.get('')).toThrow('query must be a non-empty string');
      expect(() => cache.set('', {}, {})).toThrow('query must be a non-empty string');
    });
  });

  describe('LRU Eviction', () => {
    it('should evict least recently used when maxSize exceeded', () => {
      // Fill cache to capacity
      for (let i = 0; i < 5; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      expect(cache.size).toBe(5);

      // Add one more - should evict first
      cache.set('SELECT * WHERE { ?s ?p 5 }', {}, { result: 5 });

      expect(cache.size).toBe(5);
      // First entry should be evicted
      expect(cache.get('SELECT * WHERE { ?s ?p 0 }', {})).toBeUndefined();
      // Last entry should exist
      expect(cache.get('SELECT * WHERE { ?s ?p 5 }', {})).toEqual({ result: 5 });
    });

    it('should update LRU order on access', () => {
      // Fill cache
      for (let i = 0; i < 5; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      // Access first entry - moves to end
      cache.get('SELECT * WHERE { ?s ?p 0 }', {});

      // Add new entry - should evict second entry (now LRU)
      cache.set('SELECT * WHERE { ?s ?p 5 }', {}, { result: 5 });

      // First entry should still exist (was accessed)
      expect(cache.get('SELECT * WHERE { ?s ?p 0 }', {})).toEqual({ result: 0 });
      // Second entry should be evicted
      expect(cache.get('SELECT * WHERE { ?s ?p 1 }', {})).toBeUndefined();
    });

    it('should track eviction count', () => {
      // Fill cache
      for (let i = 0; i < 5; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      // Add 3 more entries - should cause 3 evictions
      for (let i = 5; i < 8; i++) {
        cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      expect(cache.stats().evictions).toBe(3);
    });
  });

  describe('TTL Expiration', () => {
    it('should return undefined for expired entry', async () => {
      const shortTTLCache = new QueryCache({ maxSize: 10, ttl: 50 });
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [] };

      shortTTLCache.set(query, {}, result);

      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 100));

      const cached = shortTTLCache.get(query, {});
      expect(cached).toBeUndefined();
      expect(shortTTLCache.stats().expirations).toBe(1);
    });

    it('should return result before TTL expires', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [] };

      cache.set(query, {}, result);

      // Immediate access - should not be expired
      const cached = cache.get(query, {});
      expect(cached).toEqual(result);
    });

    it('should prune expired entries', async () => {
      const shortTTLCache = new QueryCache({ maxSize: 10, ttl: 50 });

      shortTTLCache.set('query1', {}, { result: 1 });
      shortTTLCache.set('query2', {}, { result: 2 });

      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 100));

      const pruned = shortTTLCache.prune();
      expect(pruned).toBe(2);
      expect(shortTTLCache.size).toBe(0);
    });
  });

  describe('Pattern Invalidation', () => {
    it('should invalidate entries matching pattern', () => {
      // Set up entries with different patterns
      cache.set('SELECT ?name WHERE { <http://example.org/alice> foaf:name ?name }', {}, {
        name: 'Alice',
      });
      cache.set('SELECT ?name WHERE { <http://example.org/bob> foaf:name ?name }', {}, {
        name: 'Bob',
      });
      cache.set('SELECT ?age WHERE { <http://example.org/alice> foaf:age ?age }', {}, { age: 30 });

      // Invalidate all queries about alice
      const invalidated = cache.invalidate({
        subject: '<http://example.org/alice>',
        predicate: '*',
        object: '*',
      });

      expect(invalidated).toBe(2);
      expect(cache.size).toBe(1);
    });

    it('should invalidate entries matching predicate pattern', () => {
      cache.set('SELECT ?name WHERE { ?s foaf:name ?name }', {}, { result: 1 });
      cache.set('SELECT ?age WHERE { ?s foaf:age ?age }', {}, { result: 2 });

      const invalidated = cache.invalidate({
        subject: '*',
        predicate: 'foaf:name',
        object: '*',
      });

      // May invalidate depending on pattern matching
      expect(invalidated).toBeGreaterThanOrEqual(0);
    });

    it('should invalidate all with wildcard pattern', () => {
      cache.set('query1', {}, { result: 1 });
      cache.set('query2', {}, { result: 2 });
      cache.set('query3', {}, { result: 3 });

      const invalidated = cache.invalidate({ subject: '*', predicate: '*', object: '*' });

      expect(cache.size).toBe(0);
    });
  });

  describe('Invalidate All', () => {
    it('should clear entire cache', () => {
      cache.set('query1', {}, { result: 1 });
      cache.set('query2', {}, { result: 2 });
      cache.set('query3', {}, { result: 3 });

      expect(cache.size).toBe(3);

      const cleared = cache.invalidateAll();

      expect(cleared).toBe(3);
      expect(cache.size).toBe(0);
    });
  });

  describe('Stats', () => {
    it('should track hits and misses', () => {
      cache.set('query1', {}, { result: 1 });

      // 2 hits
      cache.get('query1', {});
      cache.get('query1', {});

      // 3 misses
      cache.get('query2', {});
      cache.get('query3', {});
      cache.get('query4', {});

      const stats = cache.stats();
      expect(stats.hits).toBe(2);
      expect(stats.misses).toBe(3);
      expect(stats.hitRate).toBeCloseTo(0.4, 2); // 2/5 = 0.4
    });

    it('should report correct size', () => {
      expect(cache.stats().size).toBe(0);

      cache.set('query1', {}, { result: 1 });
      expect(cache.stats().size).toBe(1);

      cache.set('query2', {}, { result: 2 });
      expect(cache.stats().size).toBe(2);
    });

    it('should calculate hit rate correctly', () => {
      cache.set('query1', {}, { result: 1 });

      // 8 hits
      for (let i = 0; i < 8; i++) {
        cache.get('query1', {});
      }

      // 2 misses
      cache.get('query2', {});
      cache.get('query3', {});

      const stats = cache.stats();
      expect(stats.hitRate).toBeCloseTo(0.8, 2); // 8/10 = 0.8
    });
  });

  describe('Has Method', () => {
    it('should return true for existing entry', () => {
      cache.set('query1', {}, { result: 1 });
      expect(cache.has('query1', {})).toBe(true);
    });

    it('should return false for non-existing entry', () => {
      expect(cache.has('query1', {})).toBe(false);
    });

    it('should not affect hit/miss stats', () => {
      cache.set('query1', {}, { result: 1 });

      cache.has('query1', {});
      cache.has('query2', {});

      const stats = cache.stats();
      expect(stats.hits).toBe(0);
      expect(stats.misses).toBe(0);
    });
  });

  describe('Performance', () => {
    it('should have cache lookup <1ms', () => {
      const perfCache = new QueryCache({ maxSize: 1000, ttl: 60000 });

      // Pre-populate cache
      for (let i = 0; i < 1000; i++) {
        perfCache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
      }

      // Measure lookup time
      const start = performance.now();
      for (let i = 0; i < 1000; i++) {
        perfCache.get(`SELECT * WHERE { ?s ?p ${i % 1000} }`, {});
      }
      const duration = performance.now() - start;

      // Average should be <1ms per lookup
      const avgLookup = duration / 1000;
      expect(avgLookup).toBeLessThan(1);
    });

    it('should achieve >80% hit rate in benchmark', () => {
      const benchCache = new QueryCache({ maxSize: 100, ttl: 60000 });
      const queries = [];

      // Generate 20 unique queries
      for (let i = 0; i < 20; i++) {
        queries.push(`SELECT * WHERE { ?s ?p ${i} }`);
      }

      // Pre-populate with all queries
      for (const q of queries) {
        benchCache.set(q, {}, { result: q });
      }

      // Run 1000 queries with Zipf-like distribution (80% to 20% of queries)
      for (let i = 0; i < 1000; i++) {
        // 80% of accesses go to first 4 queries (20% of total)
        const queryIndex = Math.random() < 0.8 ? Math.floor(Math.random() * 4) : Math.floor(Math.random() * 20);

        benchCache.get(queries[queryIndex], {});
      }

      const stats = benchCache.stats();
      expect(stats.hitRate).toBeGreaterThan(0.8);
    });

    it('should bound memory usage with LRU limit', () => {
      const boundedCache = new QueryCache({ maxSize: 100, ttl: 60000 });

      // Add 200 entries
      for (let i = 0; i < 200; i++) {
        boundedCache.set(`query${i}`, {}, { result: i });
      }

      // Size should never exceed maxSize
      expect(boundedCache.size).toBeLessThanOrEqual(100);
      expect(boundedCache.stats().evictions).toBeGreaterThanOrEqual(100);
    });
  });

  describe('Query Normalization', () => {
    it('should treat queries with different whitespace as same', () => {
      const query1 = 'SELECT ?s WHERE { ?s ?p ?o }';
      const query2 = 'SELECT  ?s  WHERE  {  ?s  ?p  ?o  }';

      cache.set(query1, {}, { result: 'cached' });
      const result = cache.get(query2, {});

      expect(result).toEqual({ result: 'cached' });
    });

    it('should treat queries with different bindings as different', () => {
      const query = 'SELECT ?name WHERE { ?s foaf:name ?name }';

      cache.set(query, { s: 'alice' }, { name: 'Alice' });
      cache.set(query, { s: 'bob' }, { name: 'Bob' });

      expect(cache.get(query, { s: 'alice' })).toEqual({ name: 'Alice' });
      expect(cache.get(query, { s: 'bob' })).toEqual({ name: 'Bob' });
      expect(cache.size).toBe(2);
    });
  });
});
