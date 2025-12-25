/**
 * Snapshot Cache Tests
 * Tests for LRU cache, TTL, memory limits, and prefetching
 */
import { describe, it, expect, beforeEach } from 'vitest';
import {
  SnapshotLRUCache,
  CachedSnapshotManager,
  createSnapshotCache,
  createCachedSnapshotManager,
} from '../src/snapshot-cache.mjs';

describe('SnapshotLRUCache', () => {
  let cache;

  beforeEach(() => {
    cache = new SnapshotLRUCache({
      maxSize: 5,
      maxMemoryMB: 1,
      ttlMs: 1000,
    });
  });

  describe('Basic Operations', () => {
    it('should set and get values', () => {
      cache.set('key1', { data: 'value1' });
      const result = cache.get('key1');
      expect(result).toEqual({ data: 'value1' });
    });

    it('should return undefined for missing keys', () => {
      const result = cache.get('nonexistent');
      expect(result).toBeUndefined();
    });

    it('should check existence with has()', () => {
      cache.set('key1', { data: 'value1' });
      expect(cache.has('key1')).toBe(true);
      expect(cache.has('key2')).toBe(false);
    });

    it('should delete entries', () => {
      cache.set('key1', { data: 'value1' });
      cache.delete('key1');
      expect(cache.has('key1')).toBe(false);
    });

    it('should clear all entries', () => {
      cache.set('key1', { data: 'value1' });
      cache.set('key2', { data: 'value2' });
      cache.clear();
      expect(cache.has('key1')).toBe(false);
      expect(cache.has('key2')).toBe(false);
    });
  });

  describe('LRU Eviction by Count', () => {
    it('should evict oldest entry when maxSize exceeded', () => {
      // Fill cache to max
      for (let i = 0; i < 5; i++) {
        cache.set(`key${i}`, { data: `value${i}` });
      }

      // Add one more - should evict key0
      cache.set('key5', { data: 'value5' });

      expect(cache.has('key0')).toBe(false);
      expect(cache.has('key5')).toBe(true);
      expect(cache.cache.size).toBe(5);
    });

    it('should update LRU order on get', () => {
      cache.set('key0', { data: 'value0' });
      cache.set('key1', { data: 'value1' });
      cache.set('key2', { data: 'value2' });

      // Access key0 - moves to end
      cache.get('key0');

      // Fill to capacity
      cache.set('key3', { data: 'value3' });
      cache.set('key4', { data: 'value4' });

      // Add one more - should evict key1 (oldest after key0 was accessed)
      cache.set('key5', { data: 'value5' });

      expect(cache.has('key0')).toBe(true); // Still exists (was accessed)
      expect(cache.has('key1')).toBe(false); // Evicted
    });
  });

  describe('TTL Expiration', () => {
    it('should expire entries after TTL', async () => {
      const shortTtlCache = new SnapshotLRUCache({
        maxSize: 10,
        ttlMs: 50, // 50ms TTL
      });

      shortTtlCache.set('key1', { data: 'value1' });
      expect(shortTtlCache.has('key1')).toBe(true);

      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(shortTtlCache.has('key1')).toBe(false);
      expect(shortTtlCache.get('key1')).toBeUndefined();
    });

    it('should count miss on expired entry', async () => {
      const shortTtlCache = new SnapshotLRUCache({ ttlMs: 50 });
      shortTtlCache.set('key1', { data: 'value1' });

      await new Promise(resolve => setTimeout(resolve, 100));

      const initialMisses = shortTtlCache.stats.misses;
      shortTtlCache.get('key1');
      expect(shortTtlCache.stats.misses).toBe(initialMisses + 1);
    });
  });

  describe('Memory Management', () => {
    it('should track memory usage', () => {
      cache.set('key1', { data: 'x'.repeat(1000) });
      const stats = cache.getStats();
      expect(stats.memoryUsedMB).toBeGreaterThan(0);
    });

    it('should evict by memory when limit exceeded', () => {
      // Create cache with very small memory limit
      const smallCache = new SnapshotLRUCache({
        maxSize: 100,
        maxMemoryMB: 0.001, // 1KB
      });

      // Add entries until memory limit hit
      for (let i = 0; i < 10; i++) {
        smallCache.set(`key${i}`, { data: 'x'.repeat(500) });
      }

      const stats = smallCache.getStats();
      // Should have evicted some entries to stay under limit
      expect(stats.size).toBeLessThan(10);
    });
  });

  describe('Statistics', () => {
    it('should track hits and misses', () => {
      cache.set('key1', { data: 'value1' });

      cache.get('key1'); // Hit
      cache.get('key2'); // Miss
      cache.get('key1'); // Hit
      cache.get('key3'); // Miss

      const stats = cache.getStats();
      expect(stats.hits).toBe(2);
      expect(stats.misses).toBe(2);
      expect(stats.hitRate).toBe(0.5);
    });

    it('should track evictions', () => {
      for (let i = 0; i < 10; i++) {
        cache.set(`key${i}`, { data: `value${i}` });
      }

      const stats = cache.getStats();
      expect(stats.evictions).toBeGreaterThan(0);
    });

    it('should track loads', () => {
      cache.set('key1', { data: 'value1' });
      cache.set('key2', { data: 'value2' });

      const stats = cache.getStats();
      expect(stats.loads).toBe(2);
    });
  });

  describe('Serialization', () => {
    it('should serialize objects for size calculation', () => {
      const data = { nested: { value: 'test' } };
      cache.set('key1', data);
      expect(cache.has('key1')).toBe(true);
    });

    it('should handle objects with dump method', () => {
      const mockStore = {
        dump: () => 'NQUADS_DATA',
      };
      cache.set('key1', mockStore);
      const result = cache.get('key1');
      expect(result).toEqual(mockStore);
    });
  });

  describe('Factory Function', () => {
    it('should create cache via factory', () => {
      const factoryCache = createSnapshotCache({ maxSize: 10 });
      expect(factoryCache).toBeInstanceOf(SnapshotLRUCache);
      expect(factoryCache.maxSize).toBe(10);
    });
  });
});

describe('CachedSnapshotManager', () => {
  let manager;
  let mockGitBackbone;

  beforeEach(() => {
    mockGitBackbone = {
      readSnapshot: async (ref) => `NQUADS_FOR_${ref}`,
    };

    manager = new CachedSnapshotManager({
      gitBackbone: mockGitBackbone,
      cacheOptions: { maxSize: 5, ttlMs: 10000 },
      enablePrefetch: false, // Disable for predictable tests
    });
  });

  describe('Snapshot Loading', () => {
    it('should load snapshot from git on cache miss', async () => {
      const data = await manager.getSnapshot('abc123');
      expect(data).toBe('NQUADS_FOR_abc123');
    });

    it('should return cached snapshot on cache hit', async () => {
      await manager.getSnapshot('abc123');
      const stats1 = manager.getStats();

      await manager.getSnapshot('abc123');
      const stats2 = manager.getStats();

      expect(stats2.cache.hits).toBeGreaterThan(stats1.cache.hits);
    });

    it('should prevent duplicate loads for same ref', async () => {
      let loadCount = 0;
      mockGitBackbone.readSnapshot = async (ref) => {
        loadCount++;
        await new Promise(resolve => setTimeout(resolve, 50));
        return `NQUADS_FOR_${ref}`;
      };

      // Trigger multiple loads concurrently
      const promises = [
        manager.getSnapshot('abc123'),
        manager.getSnapshot('abc123'),
        manager.getSnapshot('abc123'),
      ];

      await Promise.all(promises);

      // Should only load once
      expect(loadCount).toBe(1);
    });
  });

  describe('Snapshot Index', () => {
    it('should update snapshot index', () => {
      const refs = ['ref1', 'ref2', 'ref3'];
      manager.updateSnapshotIndex(refs);
      expect(manager.snapshotIndex).toEqual(refs);
    });
  });

  describe('Prefetching', () => {
    it('should prefetch adjacent snapshots when enabled', async () => {
      const prefetchManager = new CachedSnapshotManager({
        gitBackbone: mockGitBackbone,
        enablePrefetch: true,
      });

      prefetchManager.updateSnapshotIndex(['ref1', 'ref2', 'ref3']);

      // Load middle snapshot
      await prefetchManager.getSnapshot('ref2');

      // Wait for background prefetch
      await new Promise(resolve => setTimeout(resolve, 100));

      // Adjacent snapshots should be cached
      const stats = prefetchManager.getStats();
      expect(stats.cache.size).toBeGreaterThan(1);
    });
  });

  describe('Preload', () => {
    it('should preload multiple snapshots', async () => {
      const refs = ['ref1', 'ref2', 'ref3'];
      const result = await manager.preloadSnapshots(refs);

      expect(result.loaded).toBe(3);
      expect(result.failed).toBe(0);
      expect(manager.getStats().cache.size).toBe(3);
    });

    it('should handle partial failures', async () => {
      mockGitBackbone.readSnapshot = async (ref) => {
        if (ref === 'bad') throw new Error('Git error');
        return `NQUADS_FOR_${ref}`;
      };

      const result = await manager.preloadSnapshots(['ref1', 'bad', 'ref2']);

      expect(result.loaded).toBe(2);
      expect(result.failed).toBe(1);
    });
  });

  describe('Cache Invalidation', () => {
    it('should invalidate specific snapshot', async () => {
      await manager.getSnapshot('abc123');
      manager.invalidate('abc123');

      expect(manager.cache.has('abc123')).toBe(false);
    });

    it('should clear all cached snapshots', async () => {
      await manager.getSnapshot('ref1');
      await manager.getSnapshot('ref2');

      manager.clearCache();

      expect(manager.getStats().cache.size).toBe(0);
    });
  });

  describe('Statistics', () => {
    it('should track cache stats', async () => {
      await manager.getSnapshot('ref1');
      const stats = manager.getStats();

      expect(stats.cache).toBeDefined();
      expect(stats.pendingLoads).toBe(0);
      expect(stats.indexedSnapshots).toBe(0);
    });
  });

  describe('Factory Function', () => {
    it('should create manager via factory', () => {
      const factoryManager = createCachedSnapshotManager({
        gitBackbone: mockGitBackbone,
      });
      expect(factoryManager).toBeInstanceOf(CachedSnapshotManager);
    });

    it('should require gitBackbone', () => {
      expect(() => new CachedSnapshotManager({})).toThrow('gitBackbone is required');
    });
  });
});

describe('Performance Characteristics', () => {
  it('should have O(1) cache access', () => {
    const cache = new SnapshotLRUCache({ maxSize: 1000 });

    // Fill cache
    for (let i = 0; i < 1000; i++) {
      cache.set(`key${i}`, { data: `value${i}` });
    }

    // Measure access time
    const start = performance.now();
    for (let i = 0; i < 1000; i++) {
      cache.get(`key${i}`);
    }
    const elapsed = performance.now() - start;

    // Should be < 1ms per access on average
    expect(elapsed / 1000).toBeLessThan(1);
  });

  it('should handle high cache hit rates efficiently', async () => {
    const manager = new CachedSnapshotManager({
      gitBackbone: {
        readSnapshot: async () => 'NQUADS_DATA',
      },
    });

    // Prime cache
    await manager.getSnapshot('ref1');

    // Measure cached access
    const start = performance.now();
    for (let i = 0; i < 100; i++) {
      await manager.getSnapshot('ref1');
    }
    const elapsed = performance.now() - start;

    // Cached access should be < 0.1ms each
    expect(elapsed / 100).toBeLessThan(0.1);
  });
});
