/**
 * Tests for Bulkhead Isolation Pattern
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { BulkheadManager, BulkheadCoordinator } from '../src/bulkhead-manager.mjs';

describe('Bulkhead Isolation Pattern', () => {
  describe('BulkheadManager', () => {
    let bulkhead;

    beforeEach(() => {
      bulkhead = new BulkheadManager({
        name: 'test-bulkhead',
        maxConcurrent: 2,
        maxQueueSize: 3,
        timeout: 1000,
      });
    });

    it('should execute function immediately when under capacity', async () => {
      const result = await bulkhead.execute(async () => {
        return 'success';
      });

      expect(result).toBe('success');
      expect(bulkhead.getStats().completed).toBe(1);
    });

    it('should queue tasks when at capacity', async () => {
      const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));

      // Start 2 concurrent tasks (at capacity)
      const task1 = bulkhead.execute(async () => {
        await delay(50);
        return 'task1';
      });

      const task2 = bulkhead.execute(async () => {
        await delay(50);
        return 'task2';
      });

      // Third task should be queued
      const task3 = bulkhead.execute(async () => 'task3');

      expect(bulkhead.getStats().active).toBe(2);

      const results = await Promise.all([task1, task2, task3]);

      expect(results).toEqual(['task1', 'task2', 'task3']);
      expect(bulkhead.getStats().completed).toBe(3);
    });

    it('should reject when queue is full', async () => {
      const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));

      // Fill capacity (2)
      bulkhead.execute(async () => await delay(100));
      bulkhead.execute(async () => await delay(100));

      // Fill queue (3)
      bulkhead.execute(async () => 'q1');
      bulkhead.execute(async () => 'q2');
      bulkhead.execute(async () => 'q3');

      // This should be rejected
      await expect(
        bulkhead.execute(async () => 'overflow')
      ).rejects.toThrow('queue full');

      expect(bulkhead.getStats().rejected).toBe(1);
    });

    it('should respect priority in queue', async () => {
      const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));
      const results = [];

      // Fill capacity
      const blocker1 = bulkhead.execute(async () => await delay(50));
      const blocker2 = bulkhead.execute(async () => await delay(50));

      // Queue with different priorities
      const low = bulkhead.execute(async () => {
        results.push('low');
        return 'low';
      }, { priority: 1 });

      const high = bulkhead.execute(async () => {
        results.push('high');
        return 'high';
      }, { priority: 10 });

      await Promise.all([blocker1, blocker2, low, high]);

      // High priority should execute first
      expect(results[0]).toBe('high');
      expect(results[1]).toBe('low');
    });

    it('should timeout long-running tasks', async () => {
      const bulkheadWithTimeout = new BulkheadManager({
        name: 'timeout-test',
        maxConcurrent: 1,
        maxQueueSize: 0,
        timeout: 50,
      });

      await expect(
        bulkheadWithTimeout.execute(async () => {
          await new Promise(resolve => setTimeout(resolve, 200));
          return 'never';
        })
      ).rejects.toThrow('timed out');

      expect(bulkheadWithTimeout.getStats().timedOut).toBe(1);
    });

    it('should drain all active and queued tasks', async () => {
      const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));

      bulkhead.execute(async () => await delay(50));
      bulkhead.execute(async () => await delay(50));

      const drainPromise = bulkhead.drain();

      expect(bulkhead.getStats().active).toBeGreaterThan(0);

      await drainPromise;

      expect(bulkhead.getStats().active).toBe(0);
      expect(bulkhead.getStats().queued).toBe(0);
    });

    it('should clear queue and reject pending tasks', async () => {
      const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));

      // Fill capacity
      bulkhead.execute(async () => await delay(100));
      bulkhead.execute(async () => await delay(100));

      // Add to queue
      const queued = bulkhead.execute(async () => 'queued');

      const cleared = bulkhead.clearQueue();

      expect(cleared).toBe(1);
      await expect(queued).rejects.toThrow('queue cleared');
    });

    it('should generate receipt for operations', async () => {
      await bulkhead.execute(async () => 'test');

      const receipt = await bulkhead.generateReceipt(
        'test-operation',
        { input: 'data' },
        { output: 'result' }
      );

      expect(receipt.operation).toContain('bulkhead:test-bulkhead');
      expect(receipt.outputs.stats).toBeDefined();
    });
  });

  describe('BulkheadCoordinator', () => {
    let coordinator;

    beforeEach(() => {
      coordinator = new BulkheadCoordinator();
    });

    it('should register multiple bulkheads', () => {
      coordinator.register({
        name: 'db',
        maxConcurrent: 10,
        maxQueueSize: 100,
      });

      coordinator.register({
        name: 'api',
        maxConcurrent: 20,
        maxQueueSize: 50,
      });

      const stats = coordinator.getAllStats();

      expect(stats.db).toBeDefined();
      expect(stats.api).toBeDefined();
      expect(stats.db.maxConcurrent).toBe(10);
      expect(stats.api.maxConcurrent).toBe(20);
    });

    it('should execute on named bulkhead', async () => {
      coordinator.register({
        name: 'test',
        maxConcurrent: 1,
        maxQueueSize: 0,
      });

      const result = await coordinator.execute('test', async () => 'success');

      expect(result).toBe('success');
    });

    it('should throw when bulkhead not found', async () => {
      await expect(
        coordinator.execute('nonexistent', async () => 'never')
      ).rejects.toThrow('not found');
    });

    it('should drain all bulkheads', async () => {
      const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));

      coordinator.register({
        name: 'b1',
        maxConcurrent: 1,
        maxQueueSize: 0,
      });

      coordinator.register({
        name: 'b2',
        maxConcurrent: 1,
        maxQueueSize: 0,
      });

      coordinator.execute('b1', async () => await delay(50));
      coordinator.execute('b2', async () => await delay(50));

      await coordinator.drainAll();

      const stats = coordinator.getAllStats();
      expect(stats.b1.active).toBe(0);
      expect(stats.b2.active).toBe(0);
    });
  });
});
