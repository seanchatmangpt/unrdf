/**
 * Tests for Materialized Views - Cached SPARQL Results
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  ViewCache,
  createAutoCleanupCache,
  MaterializedViewSchema,
} from '../src/materialized-views.mjs';

describe('Materialized Views - Cached SPARQL Results', () => {
  describe('ViewCache', () => {
    let cache;

    beforeEach(() => {
      cache = new ViewCache({
        defaultTTL: 1000, // 1 second for testing
        maxSize: 5,
        evictionPolicy: 'lru',
      });
    });

    it('should materialize view with projection', () => {
      const view = cache.materialize(
        'view-001',
        'SELECT * WHERE { ?s ?p ?o }',
        { results: [{ s: 'a', p: 'b', o: 'c' }] }
      );

      expect(view.id).toBe('view-001');
      expect(view.query).toContain('SELECT');
      expect(view.projection.results).toHaveLength(1);
      expect(view.hits).toBe(0);

      // Validate schema
      MaterializedViewSchema.parse(view);
    });

    it('should get materialized view and track hits', () => {
      cache.materialize('view-001', 'SELECT', { data: 'test' });

      const view1 = cache.get('view-001');
      const view2 = cache.get('view-001');

      expect(view1).toBeDefined();
      expect(view1.hits).toBe(1);
      expect(view2.hits).toBe(2);
      expect(view2.lastAccessedAt).toBeDefined();
    });

    it('should return null for expired views', async () => {
      cache.materialize('view-001', 'SELECT', { data: 'test' }, { ttl: 50 });

      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 100));

      const view = cache.get('view-001');

      expect(view).toBeNull();
    });

    it('should get projection directly', () => {
      cache.materialize('view-001', 'SELECT', { result: 'data' });

      const projection = cache.getProjection('view-001');

      expect(projection).toEqual({ result: 'data' });
    });

    it('should invalidate view by ID', () => {
      cache.materialize('view-001', 'SELECT', { data: 'test' });

      const invalidated = cache.invalidate('view-001');

      expect(invalidated).toBe(true);
      expect(cache.get('view-001')).toBeNull();
    });

    it('should invalidate views by resource dependency', () => {
      cache.materialize('view-001', 'SELECT', { data: 'a' }, {
        dependsOn: ['resource-1', 'resource-2'],
      });

      cache.materialize('view-002', 'SELECT', { data: 'b' }, {
        dependsOn: ['resource-1'],
      });

      cache.materialize('view-003', 'SELECT', { data: 'c' }, {
        dependsOn: ['resource-3'],
      });

      const count = cache.invalidateByResource('resource-1');

      expect(count).toBe(2); // view-001 and view-002
      expect(cache.get('view-001')).toBeNull();
      expect(cache.get('view-002')).toBeNull();
      expect(cache.get('view-003')).toBeDefined(); // Not invalidated
    });

    it('should invalidate all views', () => {
      cache.materialize('view-001', 'SELECT', { data: '1' });
      cache.materialize('view-002', 'SELECT', { data: '2' });
      cache.materialize('view-003', 'SELECT', { data: '3' });

      const count = cache.invalidateAll();

      expect(count).toBe(3);
      expect(cache.get('view-001')).toBeNull();
    });

    it('should refresh view with new projection', () => {
      cache.materialize('view-001', 'SELECT', { data: 'old' });

      const refreshed = cache.refresh('view-001', { data: 'new' });

      expect(refreshed).toBeDefined();
      expect(refreshed.projection.data).toBe('new');

      const view = cache.get('view-001');
      expect(view.projection.data).toBe('new');
    });

    it('should evict LRU when at capacity', () => {
      // Fill cache to capacity (5)
      for (let i = 1; i <= 5; i++) {
        cache.materialize(`view-${i}`, 'SELECT', { data: i });
      }

      // Access view-1 and view-2 to make them recently used
      cache.get('view-1');
      cache.get('view-2');

      // Add new view (should evict least recently used, not view-1 or view-2)
      cache.materialize('view-6', 'SELECT', { data: 6 });

      expect(cache.get('view-1')).toBeDefined();
      expect(cache.get('view-2')).toBeDefined();
      expect(cache.get('view-3')).toBeNull(); // Evicted (LRU)
    });

    it('should evict LFU when configured', () => {
      const lfuCache = new ViewCache({
        maxSize: 3,
        evictionPolicy: 'lfu',
      });

      lfuCache.materialize('view-1', 'SELECT', { data: 1 });
      lfuCache.materialize('view-2', 'SELECT', { data: 2 });
      lfuCache.materialize('view-3', 'SELECT', { data: 3 });

      // Access view-1 and view-2 multiple times
      lfuCache.get('view-1');
      lfuCache.get('view-1');
      lfuCache.get('view-2');

      // Add new view (should evict view-3 with lowest hits)
      lfuCache.materialize('view-4', 'SELECT', { data: 4 });

      expect(lfuCache.get('view-1')).toBeDefined();
      expect(lfuCache.get('view-2')).toBeDefined();
      expect(lfuCache.get('view-3')).toBeNull(); // Evicted (LFU)
      expect(lfuCache.get('view-4')).toBeDefined();
    });

    it('should evict FIFO when configured', () => {
      const fifoCache = new ViewCache({
        maxSize: 3,
        evictionPolicy: 'fifo',
      });

      fifoCache.materialize('view-1', 'SELECT', { data: 1 });
      fifoCache.materialize('view-2', 'SELECT', { data: 2 });
      fifoCache.materialize('view-3', 'SELECT', { data: 3 });

      // Add new view (should evict view-1, first in)
      fifoCache.materialize('view-4', 'SELECT', { data: 4 });

      expect(fifoCache.get('view-1')).toBeNull(); // Evicted (FIFO)
      expect(fifoCache.get('view-2')).toBeDefined();
    });

    it('should cleanup expired views', async () => {
      cache.materialize('view-1', 'SELECT', { data: 1 }, { ttl: 50 });
      cache.materialize('view-2', 'SELECT', { data: 2 }, { ttl: 200 });

      // Wait for view-1 to expire
      await new Promise(resolve => setTimeout(resolve, 100));

      const cleaned = cache.cleanupExpired();

      expect(cleaned).toBe(1);
      expect(cache.get('view-1')).toBeNull();
      expect(cache.get('view-2')).toBeDefined();
    });

    it('should provide cache statistics', () => {
      cache.materialize('view-1', 'SELECT', { data: 1 });
      cache.materialize('view-2', 'SELECT', { data: 2 });
      cache.get('view-1');
      cache.get('view-1');

      const stats = cache.getStats();

      expect(stats.size).toBe(2);
      expect(stats.maxSize).toBe(5);
      expect(stats.totalHits).toBe(2);
      expect(stats.evictionPolicy).toBe('lru');
    });

    it('should generate receipt for cache operations', async () => {
      cache.materialize('view-1', 'SELECT', { data: 1 });

      const receipt = await cache.generateReceipt(
        'materialize',
        { viewId: 'view-1' },
        { success: true }
      );

      expect(receipt.operation).toContain('view-cache:materialize');
      expect(receipt.outputs.stats).toBeDefined();
    });
  });

  describe('createAutoCleanupCache', () => {
    it('should create cache with auto-cleanup', async () => {
      const autoCache = createAutoCleanupCache(
        { defaultTTL: 50, maxSize: 10 },
        100 // Cleanup every 100ms
      );

      autoCache.materialize('view-1', 'SELECT', { data: 1 }, { ttl: 50 });

      // Wait for cleanup to run
      await new Promise(resolve => setTimeout(resolve, 150));

      expect(autoCache.get('view-1')).toBeNull();

      // Stop cleanup
      autoCache.stopAutoCleanup();
    });
  });
});
