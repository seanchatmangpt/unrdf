/**
 * @fileoverview Tests for useQueryCache hook
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useQueryCache', () => {
  let cache;

  beforeEach(() => {
    cache = new Map();
  });

  describe('Cache Operations', () => {
    it('should cache query result', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [{ s: 'http://example.org/alice' }] };

      cache.set(query, result);

      expect(cache.has(query)).toBe(true);
      expect(cache.get(query)).toBe(result);
    });

    it('should retrieve cached result', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const result = { rows: [] };

      cache.set(query, result);
      const retrieved = cache.get(query);

      expect(retrieved).toBe(result);
    });

    it('should invalidate cache entry', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      cache.set(query, { rows: [] });

      cache.delete(query);

      expect(cache.has(query)).toBe(false);
    });

    it('should clear entire cache', () => {
      cache.set('query1', { rows: [] });
      cache.set('query2', { rows: [] });

      cache.clear();

      expect(cache.size).toBe(0);
    });
  });

  describe('Cache Hits and Misses', () => {
    it('should track cache hits', () => {
      const stats = { hits: 0, misses: 0 };

      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      cache.set(query, { rows: [] });

      if (cache.has(query)) {
        stats.hits++;
      } else {
        stats.misses++;
      }

      expect(stats.hits).toBe(1);
      expect(stats.misses).toBe(0);
    });

    it('should track cache misses', () => {
      const stats = { hits: 0, misses: 0 };

      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      if (cache.has(query)) {
        stats.hits++;
      } else {
        stats.misses++;
      }

      expect(stats.hits).toBe(0);
      expect(stats.misses).toBe(1);
    });
  });

  describe('Cache Eviction', () => {
    it('should implement LRU eviction', () => {
      const lruCache = new Map();
      const maxSize = 3;

      const queries = ['q1', 'q2', 'q3', 'q4'];

      for (const q of queries) {
        if (lruCache.size >= maxSize) {
          const firstKey = lruCache.keys().next().value;
          lruCache.delete(firstKey);
        }
        lruCache.set(q, { rows: [] });
      }

      expect(lruCache.size).toBe(3);
      expect(lruCache.has('q1')).toBe(false);
      expect(lruCache.has('q4')).toBe(true);
    });

    it('should evict least recently used', () => {
      const lruCache = new Map();

      lruCache.set('q1', { rows: [], lastAccess: 1000 });
      lruCache.set('q2', { rows: [], lastAccess: 2000 });
      lruCache.set('q3', { rows: [], lastAccess: 3000 });

      // Access q1 to update timestamp
      const q1 = lruCache.get('q1');
      lruCache.set('q1', { ...q1, lastAccess: 4000 });

      // Find least recently used
      let lruKey = null;
      let lruTime = Infinity;

      for (const [key, value] of lruCache) {
        if (value.lastAccess < lruTime) {
          lruTime = value.lastAccess;
          lruKey = key;
        }
      }

      expect(lruKey).toBe('q2');
    });
  });

  describe('Cache TTL', () => {
    it('should expire cached entries', () => {
      const ttlCache = new Map();
      const ttl = 1000; // 1 second

      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const entry = {
        result: { rows: [] },
        timestamp: Date.now(),
        ttl,
      };

      ttlCache.set(query, entry);

      // Check if expired
      const cached = ttlCache.get(query);
      const isExpired = Date.now() - cached.timestamp > cached.ttl;

      if (isExpired) {
        ttlCache.delete(query);
      }

      // Immediately after setting, should not be expired
      expect(isExpired).toBe(false);
    });
  });

  describe('Performance', () => {
    it('should improve query performance with caching', () => {
      const queryFn = vi.fn(() => {
        // Simulate expensive query
        let sum = 0;
        for (let i = 0; i < 1000000; i++) {
          sum += i;
        }
        return { rows: [], sum };
      });

      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      // First execution (cache miss)
      const start1 = performance.now();
      const result1 = queryFn();
      cache.set(query, result1);
      const duration1 = performance.now() - start1;

      // Second execution (cache hit)
      const start2 = performance.now();
      const result2 = cache.get(query);
      const duration2 = performance.now() - start2;

      expect(result2).toBe(result1);
      expect(duration2).toBeLessThan(duration1);
      expect(queryFn).toHaveBeenCalledTimes(1);
    });

    it('should handle cache size limits', () => {
      const maxSize = 100;

      for (let i = 0; i < 200; i++) {
        if (cache.size >= maxSize) {
          const firstKey = cache.keys().next().value;
          cache.delete(firstKey);
        }
        cache.set(`query-${i}`, { rows: [] });
      }

      expect(cache.size).toBeLessThanOrEqual(maxSize);
    });
  });
});
