/**
 * @fileoverview Performance benchmarks for Federation
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { createFederatedSystem } from '../../packages/knowledge-engine/federation/index.mjs';
import { StoreHealth } from '../../packages/knowledge-engine/federation/federation-coordinator.mjs';

describe('Federation Performance Benchmarks', () => {
  let federation;

  beforeAll(async () => {
    federation = await createFederatedSystem({
      federationId: 'perf-test-federation',
      enableConsensus: false, // Disable for faster testing
      replicationTopology: 'full-mesh',
      executionStrategy: 'parallel',
    });

    // Register multiple stores
    for (let i = 1; i <= 5; i++) {
      await federation.registerStore({
        storeId: `store-${i}`,
        endpoint: `http://store${i}:3000`,
        weight: 1.0,
      });

      federation.coordinator.storeHealth.set(`store-${i}`, StoreHealth.HEALTHY);
    }
  });

  afterAll(async () => {
    await federation.shutdown();
  });

  describe('query performance', () => {
    it('should execute simple queries under 200ms (p95)', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';
      const iterations = 20;
      const durations = [];

      for (let i = 0; i < iterations; i++) {
        const start = Date.now();
        await federation.query(query);
        const duration = Date.now() - start;
        durations.push(duration);
      }

      // Calculate p95
      durations.sort((a, b) => a - b);
      const p95Index = Math.floor(iterations * 0.95);
      const p95 = durations[p95Index];

      console.log(`Query p95 latency: ${p95}ms`);
      console.log(`Query mean: ${durations.reduce((a, b) => a + b, 0) / iterations}ms`);

      // Target: p95 < 200ms
      expect(p95).toBeLessThan(200);
    });

    it('should handle concurrent queries efficiently', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 5';
      const concurrency = 10;

      const start = Date.now();

      const promises = Array.from({ length: concurrency }, () => federation.query(query));

      await Promise.all(promises);

      const duration = Date.now() - start;

      console.log(`Concurrent queries (${concurrency}x) completed in: ${duration}ms`);

      // Should complete in reasonable time (not timeout)
      expect(duration).toBeLessThan(5000);
    });

    it('should scale with multiple stores', async () => {
      const query = 'SELECT ?name WHERE { ?s <http://example.org/name> ?name }';
      const results = await federation.query(query);

      expect(Array.isArray(results)).toBe(true);

      // With 5 stores, should get results from all
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('replication performance', () => {
    it('should replicate changes efficiently', async () => {
      const changes = 100;
      const durations = [];

      for (let i = 0; i < changes; i++) {
        const start = Date.now();

        await federation.replicate({
          storeId: 'store-1',
          operation: 'INSERT',
          quad: {
            subject: `http://example.org/entity-${i}`,
            predicate: 'http://example.org/value',
            object: `"value-${i}"`,
          },
        });

        const duration = Date.now() - start;
        durations.push(duration);
      }

      const avgDuration = durations.reduce((a, b) => a + b, 0) / changes;

      console.log(`Average replication latency: ${avgDuration}ms`);

      // Should average under 50ms per replication
      expect(avgDuration).toBeLessThan(50);
    });

    it('should handle batch replication', async () => {
      const batchSize = 50;
      const start = Date.now();

      const promises = Array.from({ length: batchSize }, (_, i) =>
        federation.replicate({
          storeId: 'store-1',
          operation: 'INSERT',
          quad: {
            subject: `http://example.org/batch-${i}`,
            predicate: 'http://example.org/value',
            object: `"value-${i}"`,
          },
        })
      );

      await Promise.all(promises);

      const duration = Date.now() - start;

      console.log(`Batch replication (${batchSize} items) completed in: ${duration}ms`);

      // Should complete batch in reasonable time
      expect(duration).toBeLessThan(3000);
    });
  });

  describe('store management performance', () => {
    it('should register stores quickly', async () => {
      const start = Date.now();

      await federation.registerStore({
        storeId: 'perf-test-store',
        endpoint: 'http://perftest:3000',
        weight: 1.0,
      });

      const duration = Date.now() - start;

      console.log(`Store registration took: ${duration}ms`);

      // Should register in under 100ms
      expect(duration).toBeLessThan(100);

      await federation.deregisterStore('perf-test-store');
    });

    it('should handle frequent store additions/removals', async () => {
      const cycles = 10;
      const start = Date.now();

      for (let i = 0; i < cycles; i++) {
        await federation.registerStore({
          storeId: `temp-store-${i}`,
          endpoint: `http://temp${i}:3000`,
          weight: 1.0,
        });

        await federation.deregisterStore(`temp-store-${i}`);
      }

      const duration = Date.now() - start;

      console.log(`${cycles} register/deregister cycles took: ${duration}ms`);

      // Should handle cycles efficiently
      expect(duration).toBeLessThan(1000);
    });
  });

  describe('memory usage', () => {
    it('should not leak memory during operations', async () => {
      const initialMemory = process.memoryUsage().heapUsed;

      // Perform many operations
      for (let i = 0; i < 100; i++) {
        await federation.query('SELECT * WHERE { ?s ?p ?o } LIMIT 5');

        await federation.replicate({
          storeId: 'store-1',
          operation: 'INSERT',
          quad: {
            subject: `http://example.org/mem-${i}`,
            predicate: 'http://example.org/value',
            object: `"value-${i}"`,
          },
        });
      }

      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }

      const finalMemory = process.memoryUsage().heapUsed;
      const memoryGrowth = finalMemory - initialMemory;
      const memoryGrowthMB = memoryGrowth / 1024 / 1024;

      console.log(`Memory growth: ${memoryGrowthMB.toFixed(2)} MB`);

      // Should not grow excessively (< 50MB for 100 operations)
      expect(memoryGrowthMB).toBeLessThan(50);
    });
  });

  describe('statistics and monitoring', () => {
    it('should provide performance statistics', () => {
      const stats = federation.getStats();

      expect(stats).toMatchObject({
        coordinator: expect.any(Object),
        queryEngine: expect.any(Object),
        replication: expect.any(Object),
      });

      console.log('Federation statistics:', JSON.stringify(stats, null, 2));
    });
  });
});
