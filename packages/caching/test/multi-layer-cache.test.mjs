/**
 * @file Multi-Layer Cache Tests
 * @module @unrdf/caching/test
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { MultiLayerCache, createMultiLayerCache } from '../src/layers/multi-layer-cache.mjs';

describe('MultiLayerCache', () => {
  let cache;
  let mockStore;

  beforeEach(() => {
    mockStore = {
      match: async (pattern) => {
        return [{ subject: 's', predicate: 'p', object: 'o' }];
      }
    };

    cache = new MultiLayerCache({
      store: mockStore,
      l1MaxSize: 100,
      l1TtlMs: 60000,
      l2TtlSeconds: 300,
      enableL2: false, // Disable Redis for unit tests
    });
  });

  afterEach(async () => {
    await cache.close();
  });

  describe('constructor', () => {
    it('should initialize with default config', () => {
      const c = new MultiLayerCache({ store: mockStore, enableL2: false });
      expect(c.config.l1MaxSize).toBe(1000);
      expect(c.config.l2TtlSeconds).toBe(300);
    });

    it('should validate configuration with Zod', () => {
      expect(() =>
        new MultiLayerCache({ store: mockStore, l1MaxSize: -10 })
      ).toThrow();
    });

    it('should initialize statistics', () => {
      expect(cache.stats.l1Hits).toBe(0);
      expect(cache.stats.l1Misses).toBe(0);
      expect(cache.stats.sets).toBe(0);
    });
  });

  describe('get', () => {
    it('should return null for missing key', async () => {
      const result = await cache.get('missing-key');
      expect(result).toBeNull();
      expect(cache.stats.l1Misses).toBe(1);
    });

    it('should use fetcher for missing key', async () => {
      const fetcher = async () => ({ data: 'fetched' });
      const result = await cache.get('key1', fetcher);

      expect(result).toEqual({ data: 'fetched' });
      expect(cache.stats.l3Hits).toBe(1);
    });

    it('should cache fetched value in L1', async () => {
      const fetcher = async () => ({ data: 'fetched' });
      await cache.get('key1', fetcher);

      // Second get should hit L1
      const result = await cache.get('key1');
      expect(result).toEqual({ data: 'fetched' });
      expect(cache.stats.l1Hits).toBe(1);
    });

    it('should track L1 hits', async () => {
      await cache.set('key1', { value: 123 });
      await cache.get('key1');
      await cache.get('key1');

      expect(cache.stats.l1Hits).toBe(2);
    });

    it('should track L1 misses', async () => {
      await cache.get('missing1');
      await cache.get('missing2');

      expect(cache.stats.l1Misses).toBe(2);
    });
  });

  describe('set', () => {
    it('should store value in L1', async () => {
      await cache.set('key1', { value: 'test' });

      const result = await cache.get('key1');
      expect(result).toEqual({ value: 'test' });
      expect(cache.stats.sets).toBe(1);
    });

    it('should handle complex objects', async () => {
      const complexObj = {
        triples: [{ s: 'a', p: 'b', o: 'c' }],
        metadata: { count: 42, timestamp: Date.now() }
      };

      await cache.set('complex', complexObj);
      const result = await cache.get('complex');

      expect(result).toEqual(complexObj);
    });

    it('should track L1 size', async () => {
      await cache.set('key1', 'value1');
      await cache.set('key2', 'value2');

      expect(cache.stats.l1Size).toBe(2);
    });

    it('should evict when L1 exceeds max size', async () => {
      const smallCache = new MultiLayerCache({
        store: mockStore,
        l1MaxSize: 2,
        enableL2: false
      });

      await smallCache.set('key1', 'value1');
      await smallCache.set('key2', 'value2');
      await smallCache.set('key3', 'value3'); // Should evict key1

      const result1 = await smallCache.get('key1');
      const result3 = await smallCache.get('key3');

      expect(result1).toBeNull(); // Evicted
      expect(result3).toBe('value3');
    });
  });

  describe('delete', () => {
    it('should delete from L1', async () => {
      await cache.set('key1', 'value1');
      await cache.delete('key1');

      const result = await cache.get('key1');
      expect(result).toBeNull();
      expect(cache.stats.deletes).toBe(1);
    });

    it('should update L1 size after delete', async () => {
      await cache.set('key1', 'value1');
      await cache.set('key2', 'value2');

      const sizeBefore = cache.stats.l1Size;
      await cache.delete('key1');

      expect(cache.stats.l1Size).toBe(sizeBefore - 1);
    });
  });

  describe('deletePattern', () => {
    it('should delete matching keys', async () => {
      await cache.set('sparql:query1', 'value1');
      await cache.set('sparql:query2', 'value2');
      await cache.set('other:key', 'value3');

      const count = await cache.deletePattern('sparql:*');

      expect(count).toBeGreaterThanOrEqual(2);
      expect(await cache.get('sparql:query1')).toBeNull();
      expect(await cache.get('other:key')).toBe('value3');
    });

    it('should handle wildcard patterns', async () => {
      await cache.set('user:123', 'alice');
      await cache.set('user:456', 'bob');
      await cache.set('post:789', 'hello');

      await cache.deletePattern('user:*');

      expect(await cache.get('user:123')).toBeNull();
      expect(await cache.get('post:789')).toBe('hello');
    });
  });

  describe('clear', () => {
    it('should clear all L1 entries', async () => {
      await cache.set('key1', 'value1');
      await cache.set('key2', 'value2');

      await cache.clear();

      expect(await cache.get('key1')).toBeNull();
      expect(await cache.get('key2')).toBeNull();
      expect(cache.stats.l1Size).toBe(0);
    });
  });

  describe('getStats', () => {
    it('should return current statistics', async () => {
      await cache.set('key1', 'value1');
      await cache.get('key1'); // Hit
      await cache.get('missing'); // Miss

      const stats = cache.getStats();

      expect(stats.l1Hits).toBe(1);
      expect(stats.l1Misses).toBe(1);
      expect(stats.sets).toBe(1);
      expect(stats.l1Size).toBeGreaterThan(0);
      expect(stats).toHaveProperty('l1HitRate');
      expect(stats).toHaveProperty('overallHitRate');
    });

    it('should calculate hit rates', async () => {
      await cache.set('key1', 'value1');
      await cache.get('key1'); // Hit
      await cache.get('key1'); // Hit
      await cache.get('missing'); // Miss

      const stats = cache.getStats();

      expect(stats.l1HitRate).toBeCloseTo(2 / 3, 1);
    });

    it('should handle zero division in hit rates', () => {
      const stats = cache.getStats();

      expect(stats.l1HitRate).toBe(0);
      expect(stats.overallHitRate).toBe(0);
    });
  });

  describe('resetStats', () => {
    it('should reset all statistics except size', async () => {
      await cache.set('key1', 'value1');
      await cache.get('key1');

      cache.resetStats();

      expect(cache.stats.l1Hits).toBe(0);
      expect(cache.stats.l1Misses).toBe(0);
      expect(cache.stats.sets).toBe(0);
    });
  });

  describe('createMultiLayerCache', () => {
    it('should create cache instance', () => {
      const c = createMultiLayerCache({ store: mockStore, enableL2: false });
      expect(c).toBeInstanceOf(MultiLayerCache);
    });
  });
});

describe('MultiLayerCache - L1 TTL Behavior', () => {
  it('should respect L1 TTL', async () => {
    const cache = new MultiLayerCache({
      store: {},
      l1MaxSize: 100,
      l1TtlMs: 100, // 100ms TTL
      enableL2: false
    });

    await cache.set('key1', 'value1');

    // Should be available immediately
    expect(await cache.get('key1')).toBe('value1');

    // Wait for TTL expiration
    await new Promise(resolve => setTimeout(resolve, 150));

    // Should be null after TTL
    expect(await cache.get('key1')).toBeNull();
  });
});

describe('MultiLayerCache - Pattern Matching', () => {
  let cache;

  beforeEach(() => {
    cache = new MultiLayerCache({
      store: {},
      enableL2: false
    });
  });

  it('should match simple wildcards', () => {
    expect(cache._matchPattern('user:123', 'user:*')).toBe(true);
    expect(cache._matchPattern('post:456', 'user:*')).toBe(false);
  });

  it('should match prefix patterns', () => {
    expect(cache._matchPattern('sparql:query:hash123', 'sparql:*')).toBe(true);
  });

  it('should match exact patterns', () => {
    expect(cache._matchPattern('exact-key', 'exact-key')).toBe(true);
    expect(cache._matchPattern('exact-key', 'other-key')).toBe(false);
  });

  it('should match multiple wildcards', () => {
    expect(cache._matchPattern('a:b:c', '*:*:*')).toBe(true);
    expect(cache._matchPattern('a:b:c', '*:b:*')).toBe(true);
  });
});
