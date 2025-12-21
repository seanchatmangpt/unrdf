/**
 * @vitest-environment node
 * @fileoverview LRU cache tests for Real-Time Validator
 *
 * @description
 * Tests for validation cache eviction and TTL expiration.
 * Verifies LRU eviction policy, memory bounds, and stale entry cleanup.
 *
 * CRITICAL ISSUES DETECTED:
 * 1. Cache has size limit but no TTL implementation
 * 2. LRU eviction is simple FIFO, not true LRU
 * 3. No background cleanup of stale entries
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { createRealTimeValidator } from '../src/streaming/real-time-validator.mjs';
import { createStore } from '@unrdf/oxigraph';
import { namedNode, literal } from '@rdfjs/data-model';
import { createDelta } from '@unrdf/core';

describe('Real-Time Validator Cache', () => {
  let validator;

  const shapeTurtle = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
      ] .
  `;

  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(async () => {
    if (validator) {
      await validator.cleanup();
      validator = null;
    }
    vi.restoreAllMocks();
    vi.useRealTimers();
  });

  describe('LRU Eviction Policy', () => {
    /**
     * Helper to create a delta
     * @param {number} index - Delta index
     * @returns {Object} Delta
     */
    function createDelta(index) {
      return {
        additions: [
          {
            subject: namedNode(`http://example.org/person${index}`),
            predicate: namedNode('http://example.org/name'),
            object: literal(`Person ${index}`),
          },
        ],
        removals: [],
      };
    }

    it('CURRENT BEHAVIOR: evicts oldest when cache is full', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const store = createStore();

      // Add 200 validations (exceed cache size of 100)
      for (let i = 0; i < 200; i++) {
        const delta = createDelta(i);
        await validator.validateDelta(delta, store);
      }

      // Cache should have exactly 100 entries
      expect(validator.validationCache.size).toBe(100);

      // ISSUE: Current implementation is FIFO, not true LRU
      // First 100 deltas (0-99) should be evicted
      // Last 100 deltas (100-199) should remain

      // Validate delta 0 again - should be cache miss (was evicted)
      const metrics1 = validator.getMetrics();
      const cacheHitsBefore = metrics1.cacheHits;

      await validator.validateDelta(createDelta(0), store);

      const metrics2 = validator.getMetrics();
      const cacheHitsAfter = metrics2.cacheHits;

      // Should be cache miss (hit count unchanged)
      expect(cacheHitsAfter).toBe(cacheHitsBefore);
    });

    it('CURRENT BEHAVIOR: cache hit returns same result', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const store = createStore();
      const delta = createDelta(1);

      // First validation - cache miss
      const result1 = await validator.validateDelta(delta, store);
      const metrics1 = validator.getMetrics();

      expect(metrics1.cacheMisses).toBe(1);
      expect(metrics1.cacheHits).toBe(0);

      // Second validation - cache hit
      const result2 = await validator.validateDelta(delta, store);
      const metrics2 = validator.getMetrics();

      expect(metrics2.cacheMisses).toBe(1);
      expect(metrics2.cacheHits).toBe(1);

      // Results should be identical
      expect(result2.id).toBe(result1.id);
      expect(result2.conforms).toBe(result1.conforms);
    });

    it('ISSUE: uses FIFO instead of true LRU', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 5, // Small cache for testing
      });

      const store = createStore();

      // Add 5 deltas (fill cache)
      for (let i = 0; i < 5; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      // Access delta 0 (should move to end in true LRU)
      await validator.validateDelta(createDelta(0), store);

      // Access delta 1 (should move to end in true LRU)
      await validator.validateDelta(createDelta(1), store);

      // Add delta 5 (should evict delta 2 in true LRU, but evicts delta 0 in FIFO)
      await validator.validateDelta(createDelta(5), store);

      const metricsBefore = validator.getMetrics();
      const hitsBeforeCheck = metricsBefore.cacheHits;

      // Check if delta 0 is still cached
      await validator.validateDelta(createDelta(0), store);

      const metricsAfter = validator.getMetrics();
      const hitsAfterCheck = metricsAfter.cacheHits;

      // ISSUE: In true LRU, delta 0 should still be cached (recently used)
      // In FIFO, delta 0 was evicted (oldest entry)

      // Current behavior: delta 0 was evicted (FIFO)
      // Expected behavior: delta 0 is cached (LRU)

      // This documents the issue - test will PASS with current FIFO behavior
      // Should FAIL with true LRU implementation
      expect(hitsAfterCheck).toBe(hitsBeforeCheck); // Cache miss with FIFO
    });

    it('PROPOSED: should implement true LRU eviction', async () => {
      // This test describes correct LRU behavior
      // Currently SKIPPED because implementation uses FIFO

      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 3,
      });

      const store = createStore();

      // Add 3 deltas (fill cache): [0, 1, 2]
      await validator.validateDelta(createDelta(0), store);
      await validator.validateDelta(createDelta(1), store);
      await validator.validateDelta(createDelta(2), store);

      // Access delta 0 (should move to end): [1, 2, 0]
      await validator.validateDelta(createDelta(0), store);

      // Add delta 3 (should evict delta 1, least recently used): [2, 0, 3]
      await validator.validateDelta(createDelta(3), store);

      // Delta 0 should still be cached
      const metrics1 = validator.getMetrics();
      const hits1 = metrics1.cacheHits;

      await validator.validateDelta(createDelta(0), store);

      const metrics2 = validator.getMetrics();
      const hits2 = metrics2.cacheHits;

      // Should be cache hit
      // Note: This will FAIL with current FIFO implementation
      expect(hits2).toBe(hits1 + 1);

      // Delta 1 should be evicted
      const hits3 = metrics2.cacheHits;

      await validator.validateDelta(createDelta(1), store);

      const metrics3 = validator.getMetrics();
      const hits4 = metrics3.cacheHits;

      // Should be cache miss
      expect(hits4).toBe(hits3);
    });
  });

  describe('TTL (Time To Live) Expiration', () => {
    it('ISSUE: no TTL implementation in current code', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
        // PROPOSED: cacheTTL: 60000, // 60 seconds
      });

      const store = createStore();
      const delta = createDelta(1);

      // First validation
      await validator.validateDelta(delta, store);

      // Wait 70 seconds (beyond proposed 60s TTL)
      vi.advanceTimersByTime(70 * 1000);

      const metricsBefore = validator.getMetrics();
      const hitsBefore = metricsBefore.cacheHits;

      // Validate again
      await validator.validateDelta(delta, store);

      const metricsAfter = validator.getMetrics();
      const hitsAfter = metricsAfter.cacheHits;

      // ISSUE: Current implementation will cache hit (no TTL)
      // Expected: Should be cache miss (entry expired)

      // This documents the missing feature
      expect(hitsAfter).toBe(hitsBefore + 1); // Current: cache hit
      // Expected: expect(hitsAfter).toBe(hitsBefore); // Cache miss after TTL
    });

    it.skip('PROPOSED: should expire entries after TTL', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
        cacheTTL: 60000, // 60 seconds
      });

      const store = createStore();
      const delta = createDelta(1);

      // First validation (cache miss)
      await validator.validateDelta(delta, store);

      // Immediately validate again (cache hit)
      const metrics1 = validator.getMetrics();
      await validator.validateDelta(delta, store);
      const metrics2 = validator.getMetrics();

      expect(metrics2.cacheHits).toBe(metrics1.cacheHits + 1);

      // Wait 59 seconds (just under TTL)
      vi.advanceTimersByTime(59 * 1000);

      // Should still be cached
      await validator.validateDelta(delta, store);
      const metrics3 = validator.getMetrics();

      expect(metrics3.cacheHits).toBe(metrics2.cacheHits + 1);

      // Wait another 2 seconds (total 61s, beyond TTL)
      vi.advanceTimersByTime(2 * 1000);

      // Should be expired (cache miss)
      const metrics4 = validator.getMetrics();
      await validator.validateDelta(delta, store);
      const metrics5 = validator.getMetrics();

      expect(metrics5.cacheHits).toBe(metrics4.cacheHits); // No hit increase
      expect(metrics5.cacheMisses).toBe(metrics4.cacheMisses + 1);
    });

    it.skip('PROPOSED: should have configurable TTL', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
        cacheTTL: 10000, // 10 seconds
      });

      const store = createStore();
      const delta = createDelta(1);

      await validator.validateDelta(delta, store);

      // Wait 11 seconds
      vi.advanceTimersByTime(11 * 1000);

      const metricsBefore = validator.getMetrics();
      await validator.validateDelta(delta, store);
      const metricsAfter = validator.getMetrics();

      // Should be cache miss (expired after 10s)
      expect(metricsAfter.cacheHits).toBe(metricsBefore.cacheHits);
    });

    it.skip('PROPOSED: should clean up expired entries in background', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
        cacheTTL: 60000,
        cacheCleanupInterval: 30000, // Cleanup every 30 seconds
      });

      const store = createStore();

      // Add 50 validations
      for (let i = 0; i < 50; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      expect(validator.validationCache.size).toBe(50);

      // Wait 70 seconds (all entries expire)
      vi.advanceTimersByTime(70 * 1000);

      // Cache size should still be 50 (cleanup hasn't run)
      expect(validator.validationCache.size).toBe(50);

      // Trigger cleanup (via background timer or manual call)
      // PROPOSED: validator.cleanupExpiredCache();

      // After cleanup, cache should be empty
      // PROPOSED: expect(validator.validationCache.size).toBe(0);
    });
  });

  describe('Memory Bounds', () => {
    /**
     * Helper to measure memory usage
     * @returns {number} Heap used in bytes
     */
    function getHeapUsed() {
      if (global.gc) {
        global.gc();
      }
      return process.memoryUsage().heapUsed;
    }

    it('should not exceed cacheSize limit', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const store = createStore();

      // Add 1000 validations
      for (let i = 0; i < 1000; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      // Cache should have exactly 100 entries
      expect(validator.validationCache.size).toBe(100);
    });

    it('should bound memory with cache size limit', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const store = createStore();
      const beforeMemory = getHeapUsed();

      // Add 10,000 validations
      for (let i = 0; i < 10000; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      const afterMemory = getHeapUsed();
      const memoryIncrease = afterMemory - beforeMemory;

      console.log(`Memory with 100-entry cache: ${(memoryIncrease / 1024 / 1024).toFixed(2)} MB`);

      // Memory should be bounded by cache size
      // ~100 entries * ~1KB per entry = ~100KB + overhead
      const maxExpectedMemory = 5 * 1024 * 1024; // 5MB (generous)
      expect(memoryIncrease).toBeLessThan(maxExpectedMemory);
    });

    it('should use more memory with larger cache', async () => {
      const validator1 = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const validator2 = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 1000,
      });

      const store = createStore();

      const before1 = getHeapUsed();

      for (let i = 0; i < 2000; i++) {
        await validator1.validateDelta(createDelta(i), store);
      }

      const after1 = getHeapUsed();
      const memory1 = after1 - before1;

      const before2 = getHeapUsed();

      for (let i = 0; i < 2000; i++) {
        await validator2.validateDelta(createDelta(i), store);
      }

      const after2 = getHeapUsed();
      const memory2 = after2 - before2;

      console.log(`Memory (100 cache): ${(memory1 / 1024 / 1024).toFixed(2)} MB`);
      console.log(`Memory (1000 cache): ${(memory2 / 1024 / 1024).toFixed(2)} MB`);

      // Larger cache should use more memory
      expect(memory2).toBeGreaterThan(memory1);

      await validator1.cleanup();
      await validator2.cleanup();
    });

    it('clearCache should free memory', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 1000,
      });

      const store = createStore();

      // Fill cache
      for (let i = 0; i < 1000; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      const beforeClear = getHeapUsed();

      expect(validator.validationCache.size).toBe(1000);

      // Clear cache
      validator.clearCache();

      const afterClear = getHeapUsed();

      expect(validator.validationCache.size).toBe(0);

      const memoryFreed = beforeClear - afterClear;

      console.log(`Memory freed: ${(memoryFreed / 1024 / 1024).toFixed(2)} MB`);

      // Should free significant memory
      expect(memoryFreed).toBeGreaterThan(0);
    });
  });

  describe('Cache Hit Rate Metrics', () => {
    it('should track cache hit rate accurately', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const store = createStore();

      // Add 100 unique deltas
      for (let i = 0; i < 100; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      const metrics1 = validator.getMetrics();

      expect(metrics1.cacheHits).toBe(0);
      expect(metrics1.cacheMisses).toBe(100);
      expect(metrics1.cacheHitRate).toBe(0);

      // Validate same 100 deltas again
      for (let i = 0; i < 100; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      const metrics2 = validator.getMetrics();

      expect(metrics2.cacheHits).toBe(100);
      expect(metrics2.cacheMisses).toBe(100);
      expect(metrics2.cacheHitRate).toBe(0.5); // 100 hits / 200 total
    });

    it('should update hit rate with mixed hits and misses', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 10,
      });

      const store = createStore();

      // Add 10 deltas (fill cache)
      for (let i = 0; i < 10; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      // Validate deltas 0-9 again (10 hits)
      for (let i = 0; i < 10; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      // Validate deltas 10-19 (10 misses, evicts 0-9)
      for (let i = 10; i < 20; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      const metrics = validator.getMetrics();

      expect(metrics.cacheHits).toBe(10);
      expect(metrics.cacheMisses).toBe(20);
      expect(metrics.cacheHitRate).toBe(10 / 30); // ~0.33
    });
  });

  describe('Disable Caching', () => {
    it('should bypass cache when enableCaching is false', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: false,
      });

      const store = createStore();
      const delta = createDelta(1);

      // First validation
      await validator.validateDelta(delta, store);

      // Second validation (should not hit cache)
      await validator.validateDelta(delta, store);

      const metrics = validator.getMetrics();

      expect(metrics.cacheHits).toBe(0);
      expect(metrics.cacheMisses).toBe(0); // No cache lookups at all
    });

    it('should not allocate cache when disabled', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: false,
      });

      const store = createStore();

      // Add many validations
      for (let i = 0; i < 1000; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      // Cache should remain empty
      expect(validator.validationCache.size).toBe(0);
    });
  });

  describe('Edge Cases', () => {
    it('should handle cacheSize = 0', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 0,
      });

      const store = createStore();
      const delta = createDelta(1);

      await validator.validateDelta(delta, store);
      await validator.validateDelta(delta, store);

      const metrics = validator.getMetrics();

      // Cache size 0 = no caching
      expect(validator.validationCache.size).toBe(0);
      expect(metrics.cacheHits).toBe(0);
    });

    it('should handle cacheSize = 1', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 1,
      });

      const store = createStore();

      await validator.validateDelta(createDelta(1), store);
      await validator.validateDelta(createDelta(2), store);

      // Cache should have only most recent entry
      expect(validator.validationCache.size).toBe(1);

      // Delta 1 should be evicted
      const metricsBefore = validator.getMetrics();
      await validator.validateDelta(createDelta(1), store);
      const metricsAfter = validator.getMetrics();

      expect(metricsAfter.cacheHits).toBe(metricsBefore.cacheHits); // Cache miss
    });

    it('should handle cleanup() with cached entries', async () => {
      validator = createRealTimeValidator({
        shapes: shapeTurtle,
        enableCaching: true,
        cacheSize: 100,
      });

      const store = createStore();

      // Fill cache
      for (let i = 0; i < 100; i++) {
        await validator.validateDelta(createDelta(i), store);
      }

      expect(validator.validationCache.size).toBe(100);

      // Cleanup should clear cache
      await validator.cleanup();

      expect(validator.validationCache.size).toBe(0);
    });
  });
});
