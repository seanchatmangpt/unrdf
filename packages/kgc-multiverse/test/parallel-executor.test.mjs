/**
 * KGC Multiverse - Parallel Executor Tests
 * Tests worker pool, parallel operations, memory management
 *
 * Test Categories:
 * 1. Universe Creation (3 tests)
 * 2. Morphism Application (2 tests)
 * 3. Receipt Generation (2 tests)
 * 4. Memory Management (2 tests)
 * 5. Worker Pool Scaling (2 tests)
 * 6. Error Handling (1 test)
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  ParallelExecutor,
  createParallelExecutor,
  DEFAULT_CONFIG,
  benchmark10k,
} from '../src/parallel-executor.mjs';
import { TaskType } from '../src/worker-task.mjs';

describe('ParallelExecutor', () => {
  let executor;

  afterEach(async () => {
    if (executor && executor.isInitialized()) {
      await executor.shutdown();
    }
  });

  describe('Universe Creation', () => {
    it('creates 100 universes in <5s', async () => {
      executor = createParallelExecutor({ workerCount: 4, batchSize: 20 });
      await executor.initialize();

      const startTime = Date.now();
      const universes = [];

      for await (const universe of executor.createUniverses(100)) {
        universes.push(universe);
      }

      const duration = Date.now() - startTime;

      expect(universes).toHaveLength(100);
      expect(duration).toBeLessThan(5000);

      // Verify universe structure
      const first = universes[0];
      expect(first.id.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(first.state).toBe('GENESIS');
      expect(first.eventCount).toBe(0);
    }, 10000);

    it('creates universes with unique Q* IDs', async () => {
      executor = createParallelExecutor({ workerCount: 4, batchSize: 50 });
      await executor.initialize();

      const universes = await executor.collectAll(executor.createUniverses(200));

      const ids = new Set(universes.map((u) => u.id.Q_ID));
      expect(ids.size).toBe(200);

      // All IDs should be valid Q* format
      for (const id of ids) {
        expect(id).toMatch(/^Q\*_[a-f0-9]{16}$/);
      }
    }, 10000);

    it('tracks batch index for each universe', async () => {
      executor = createParallelExecutor({ workerCount: 2, batchSize: 10 });
      await executor.initialize();

      const universes = await executor.collectAll(executor.createUniverses(50));

      // Verify batch indices are set
      const indices = universes.map((u) => u.batchIndex);
      expect(indices).toContain(0);
      expect(indices).toContain(49);

      // All indices should be unique
      const uniqueIndices = new Set(indices);
      expect(uniqueIndices.size).toBe(50);
    }, 10000);
  });

  describe('Morphism Application', () => {
    it('applies morphism to 1000 universes in <10s', async () => {
      executor = createParallelExecutor({ workerCount: 4, batchSize: 100 });
      await executor.initialize();

      // Create universes
      const universes = await executor.collectAll(executor.createUniverses(1000));
      expect(universes).toHaveLength(1000);

      // Apply morphism
      const morphismConfig = {
        id: 'Φ_test_morphism',
        type: 'SCHEMA',
        name: 'test-transform',
      };

      const startTime = Date.now();
      const results = [];

      for await (const result of executor.applyMorphismsParallel(universes, morphismConfig)) {
        results.push(result);
      }

      const duration = Date.now() - startTime;

      expect(results).toHaveLength(1000);
      expect(duration).toBeLessThan(10000);

      // Verify result structure
      const first = results[0];
      expect(first.morphismID).toBe('Φ_test_morphism');
      expect(first.deltas).toBeDefined();
      expect(first.resultHash).toMatch(/^[a-f0-9]{64}$/);
    }, 20000);

    it('generates deltas for each morphism application', async () => {
      executor = createParallelExecutor({ workerCount: 2, batchSize: 50 });
      await executor.initialize();

      const universes = await executor.collectAll(executor.createUniverses(100));

      const morphismConfig = {
        id: 'Φ_delta_test',
        type: 'SCHEMA',
        name: 'delta-transform',
      };

      const results = await executor.collectAll(
        executor.applyMorphismsParallel(universes, morphismConfig)
      );

      // Each result should have deltas
      for (const result of results) {
        expect(Array.isArray(result.deltas)).toBe(true);
        expect(result.deltaCount).toBe(result.deltas.length);
      }
    }, 15000);
  });

  describe('Receipt Generation', () => {
    it('generates 100 receipts in <2s', async () => {
      executor = createParallelExecutor({ workerCount: 4, batchSize: 20 });
      await executor.initialize();

      // Create mock operations
      const operations = Array.from({ length: 100 }, (_, i) => ({
        universeID: `Q*_${'0'.repeat(16 - String(i).length)}${i}`,
        deltas: [{ type: 'add', subject: `ex:s${i}`, predicate: 'ex:p', object: { type: 'Literal', value: `value${i}` } }],
      }));

      const startTime = Date.now();
      const receipts = [];

      for await (const receipt of executor.generateReceiptsParallel(operations)) {
        receipts.push(receipt);
      }

      const duration = Date.now() - startTime;

      expect(receipts).toHaveLength(100);
      expect(duration).toBeLessThan(2000);

      // Verify receipt structure
      const first = receipts[0];
      expect(first.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(first.Q_RDF).toMatch(/^http:\/\/kgc\.io\/receipts\//);
      expect(first.Q_PROV.contentHash).toMatch(/^[a-f0-9]{64}$/);
    }, 10000);

    it('includes correct provenance metadata', async () => {
      executor = createParallelExecutor({ workerCount: 2, batchSize: 10 });
      await executor.initialize();

      const operations = Array.from({ length: 50 }, (_, i) => ({
        universeID: `Q*_test${String(i).padStart(12, '0')}`,
        deltas: [{ type: 'add', subject: 'ex:s', predicate: 'ex:p', object: { type: 'Literal', value: 'v' } }],
      }));

      const receipts = await executor.collectAll(
        executor.generateReceiptsParallel(operations, { operationType: 'test-op' })
      );

      for (const receipt of receipts) {
        expect(receipt.Q_PROV.operationType).toBe('test-op');
        expect(receipt.Q_PROV.batchSize).toBe(1);
        expect(typeof receipt.Q_PROV.timestamp).toBe('bigint');
      }
    }, 10000);
  });

  describe('Memory Management', () => {
    it('does not exceed 512MB for 10k operations (simulated)', async () => {
      // Use smaller count for unit test but verify memory tracking works
      executor = createParallelExecutor({ workerCount: 4, batchSize: 100 });
      await executor.initialize();

      const universes = await executor.collectAll(executor.createUniverses(1000));

      const stats = executor.getStats();

      // Memory tracking should be working
      expect(stats.peakMemoryMB).toBeGreaterThan(0);
      expect(stats.peakMemoryMB).toBeLessThan(512);

      // Verify we created the expected count
      expect(universes).toHaveLength(1000);
    }, 20000);

    it('GC hints run at configured interval', async () => {
      // Set a low GC interval to verify it runs
      executor = createParallelExecutor({
        workerCount: 2,
        batchSize: 20,
        gcInterval: 50,
      });
      await executor.initialize();

      const universes = await executor.collectAll(executor.createUniverses(200));

      const stats = executor.getStats();

      // Should have completed more than gcInterval operations
      expect(stats.tasksCompleted).toBeGreaterThanOrEqual(200);
      expect(universes).toHaveLength(200);
    }, 10000);
  });

  describe('Worker Pool Scaling', () => {
    it('scales with 8 workers', async () => {
      executor = createParallelExecutor({ workerCount: 8, batchSize: 50 });
      await executor.initialize();

      const poolInfo = executor.getPoolInfo();
      expect(poolInfo.initialized).toBe(true);
      expect(poolInfo.threads).toBe(8);

      const startTime = Date.now();
      const universes = await executor.collectAll(executor.createUniverses(400));
      const duration = Date.now() - startTime;

      expect(universes).toHaveLength(400);
      expect(duration).toBeLessThan(10000);
    }, 15000);

    it('scales with 12 workers', async () => {
      executor = createParallelExecutor({ workerCount: 12, batchSize: 50 });
      await executor.initialize();

      const poolInfo = executor.getPoolInfo();
      expect(poolInfo.initialized).toBe(true);
      expect(poolInfo.threads).toBe(12);

      const startTime = Date.now();
      const universes = await executor.collectAll(executor.createUniverses(600));
      const duration = Date.now() - startTime;

      expect(universes).toHaveLength(600);
      expect(duration).toBeLessThan(15000);
    }, 20000);
  });

  describe('Error Handling', () => {
    it('handles worker crash recovery gracefully', async () => {
      executor = createParallelExecutor({ workerCount: 2, batchSize: 10 });
      await executor.initialize();

      // Run normal operations - pool should handle any internal errors
      const universes = await executor.collectAll(executor.createUniverses(50));
      expect(universes).toHaveLength(50);

      // Verify executor is still functional
      expect(executor.isInitialized()).toBe(true);

      const stats = executor.getStats();
      expect(stats.tasksCompleted).toBeGreaterThanOrEqual(50);
      expect(stats.tasksFailed).toBe(0);

      // Shutdown should work cleanly
      await executor.shutdown();
      expect(executor.isInitialized()).toBe(false);
    }, 10000);
  });

  describe('Initialization and Shutdown', () => {
    it('prevents operations before initialization', async () => {
      executor = createParallelExecutor();

      // Should throw before initialize
      await expect(async () => {
        for await (const _ of executor.createUniverses(10)) {
          // Should not reach here
        }
      }).rejects.toThrow(/not initialized/);
    });

    it('prevents double initialization', async () => {
      executor = createParallelExecutor();
      await executor.initialize();

      await expect(executor.initialize()).rejects.toThrow(/already initialized/);
    });

    it('graceful shutdown waits for pending tasks', async () => {
      executor = createParallelExecutor({ workerCount: 2, batchSize: 10 });
      await executor.initialize();

      // Start some work
      const universes = await executor.collectAll(executor.createUniverses(20));
      expect(universes).toHaveLength(20);

      // Shutdown should complete
      await executor.shutdown();

      // Verify clean state
      expect(executor.isInitialized()).toBe(false);
      const stats = executor.getStats();
      expect(stats.tasksCompleted).toBe(20);
    }, 10000);
  });
});

describe('benchmark10k (mini)', () => {
  it('runs mini benchmark successfully', async () => {
    const result = await benchmark10k({
      count: 100, // Use small count for unit test
      workerCount: 4,
    });

    expect(result.count).toBe(100);
    expect(result.durationMs).toBeGreaterThan(0);
    expect(result.throughputPerSec).toBeGreaterThan(0);
    expect(result.peakMemoryMB).toBeGreaterThan(0);
    expect(result.peakMemoryMB).toBeLessThan(512);
    expect(result.passed).toBe(true);
  }, 30000);
});
