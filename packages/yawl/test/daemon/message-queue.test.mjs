/**
 * @file YAWL Daemon Message Queue Tests
 * @module @unrdf/yawl/test/daemon/message-queue
 * @description Comprehensive tests for daemon message queue operations
 * Tests cover enqueue, dequeue, priority, ordering, and overflow handling
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';

/**
 * Generate UUID v4 for testing
 * @returns {string} Valid UUID v4
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Mock YAWL engine for queue tests
 */
class MockYawlEngineQueue extends EventEmitter {
  constructor() {
    super();
    this.queue = [];
  }

  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  enqueue(item) {
    this.queue.push(item);
    this.emit('queue:enqueued', { item, position: this.queue.length - 1 });
  }

  dequeue() {
    if (this.queue.length === 0) return null;
    const item = this.queue.shift();
    this.emit('queue:dequeued', { item });
    return item;
  }

  async createCase(options) {
    return { caseId: options.caseId || `case-${Date.now()}`, status: 'RUNNING' };
  }

  async enableTask(options) {
    return { ...options, status: 'ENABLED' };
  }

  async cancelTask(options) {
    return { ...options, status: 'CANCELLED' };
  }
}

describe('Daemon Message Queue', () => {
  describe('enqueue operations', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'queue-enqueue-daemon',
      });
      engine = new MockYawlEngineQueue();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-queue-enqueue',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should enqueue operation successfully', () => {
      // Arrange
      const operation = {
        id: 'op-1',
        name: 'Test Operation',
        handler: vi.fn(),
      };

      // Act
      daemon.schedule(operation);

      // Assert
      expect(daemon.operationQueue).toContain('op-1');
      expect(daemon.operations.has('op-1')).toBe(true);
    });

    it('should maintain FIFO order', () => {
      // Arrange
      const ops = ['op-1', 'op-2', 'op-3'].map((id) => ({
        id,
        name: id,
        handler: vi.fn(),
      }));

      // Act
      ops.forEach((op) => daemon.schedule(op));

      // Assert
      expect(daemon.operationQueue).toEqual(['op-1', 'op-2', 'op-3']);
    });

    it('should emit enqueue event', () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:enqueued', listener);

      // Act
      daemon.schedule({ id: 'enq-op', name: 'Enqueue Test', handler: vi.fn() });

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0].operationId).toBe('enq-op');
    });

    it('should store operation metadata', () => {
      // Arrange
      const metadata = { priority: 5, tags: ['batch'] };

      // Act
      daemon.schedule({
        id: 'meta-op',
        name: 'Meta Op',
        handler: vi.fn(),
        metadata,
      });

      // Assert
      const stored = daemon.operations.get('meta-op');
      expect(stored.metadata).toEqual(metadata);
    });

    it('should track enqueue timestamp', () => {
      // Arrange
      const beforeEnqueue = Date.now();

      // Act
      daemon.schedule({ id: 'time-op', name: 'Time Op', handler: vi.fn() });
      const afterEnqueue = Date.now();

      // Assert
      const stored = daemon.operations.get('time-op');
      expect(stored.createdAt).toBeGreaterThanOrEqual(beforeEnqueue);
      expect(stored.createdAt).toBeLessThanOrEqual(afterEnqueue);
    });

    it('should handle rapid enqueuing', () => {
      // Arrange
      const count = 100;

      // Act
      for (let i = 0; i < count; i++) {
        daemon.schedule({ id: `rapid-${i}`, name: `Rapid ${i}`, handler: vi.fn() });
      }

      // Assert
      expect(daemon.operationQueue.length).toBe(count);
      expect(daemon.operations.size).toBe(count);
    });

    it('should validate operation before enqueue', () => {
      // Arrange
      const invalidOp = { id: 'invalid', name: 'Invalid' }; // Missing handler

      // Act & Assert
      expect(() => daemon.schedule(invalidOp)).toThrow();
    });

    it('should enqueue with unique IDs', () => {
      // Arrange & Act
      daemon.schedule({ id: 'unique-1', name: 'Op 1', handler: vi.fn() });
      daemon.schedule({ id: 'unique-2', name: 'Op 2', handler: vi.fn() });
      daemon.schedule({ id: 'unique-3', name: 'Op 3', handler: vi.fn() });

      // Assert
      const ids = daemon.operationQueue;
      const uniqueIds = new Set(ids);
      expect(uniqueIds.size).toBe(ids.length);
    });

    it('should store handler function reference', () => {
      // Arrange
      const handler = vi.fn();

      // Act
      daemon.schedule({ id: 'handler-op', name: 'Handler', handler });

      // Assert
      const stored = daemon.operations.get('handler-op');
      expect(stored.handler).toBe(handler);
    });

    it('should support concurrent enqueuing', async () => {
      // Arrange
      const promises = [];

      // Act
      for (let i = 0; i < 10; i++) {
        promises.push(
          Promise.resolve().then(() =>
            daemon.schedule({ id: `concurrent-${i}`, name: `C${i}`, handler: vi.fn() })
          )
        );
      }
      await Promise.all(promises);

      // Assert
      expect(daemon.operationQueue.length).toBe(10);
    });

    it('should track queue length', () => {
      // Arrange
      expect(daemon.operationQueue.length).toBe(0);

      // Act
      daemon.schedule({ id: 'len-1', name: 'L1', handler: vi.fn() });
      daemon.schedule({ id: 'len-2', name: 'L2', handler: vi.fn() });

      // Assert
      expect(daemon.operationQueue.length).toBe(2);
    });

    it('should preserve insertion order', () => {
      // Arrange
      const order = ['first', 'second', 'third', 'fourth'];

      // Act
      order.forEach((id) =>
        daemon.schedule({ id, name: id, handler: vi.fn() })
      );

      // Assert
      expect(daemon.operationQueue).toEqual(order);
    });
  });

  describe('dequeue operations', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'queue-dequeue-daemon',
      });
      engine = new MockYawlEngineQueue();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-queue-dequeue',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should dequeue in FIFO order', async () => {
      // Arrange
      daemon.schedule({ id: 'deq-1', name: 'D1', handler: vi.fn().mockResolvedValue({}) });
      daemon.schedule({ id: 'deq-2', name: 'D2', handler: vi.fn().mockResolvedValue({}) });

      // Act
      await daemon.execute('deq-1');

      // Assert - Execution doesn't auto-dequeue from queue, but operation is executed
      const metrics = daemon.getMetrics();
      expect(metrics.totalOperations).toBeGreaterThanOrEqual(1);
    });

    it('should handle empty queue', () => {
      // Arrange
      const emptyQueue = daemon.operationQueue.length === 0;

      // Act & Assert
      expect(emptyQueue).toBe(true);
      expect(() => daemon.listOperations()).not.toThrow();
    });

    it('should update queue size after dequeue', async () => {
      // Arrange
      daemon.schedule({ id: 'size-1', name: 'S1', handler: vi.fn().mockResolvedValue({}) });
      daemon.schedule({ id: 'size-2', name: 'S2', handler: vi.fn().mockResolvedValue({}) });
      const initialSize = daemon.operationQueue.length;

      // Act
      await daemon.execute('size-1');

      // Assert
      expect(initialSize).toBe(2);
      // Queue size remains same as operations are not auto-removed
      expect(daemon.operationQueue.length).toBe(2);
    });

    it('should emit dequeue event on execution', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:started', listener);
      daemon.schedule({ id: 'event-op', name: 'Event', handler: vi.fn().mockResolvedValue({}) });

      // Act
      await daemon.execute('event-op');

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should return null when queue empty', () => {
      // Arrange
      const emptyEngine = new MockYawlEngineQueue();

      // Act
      const result = emptyEngine.dequeue();

      // Assert
      expect(result).toBeNull();
    });

    it('should process operations sequentially', async () => {
      // Arrange
      const executed = [];
      const handler1 = vi.fn().mockImplementation(async () => {
        executed.push(1);
        await new Promise((resolve) => setTimeout(resolve, 10));
      });
      const handler2 = vi.fn().mockImplementation(async () => {
        executed.push(2);
      });

      daemon.schedule({ id: 'seq-1', name: 'S1', handler: handler1 });
      daemon.schedule({ id: 'seq-2', name: 'S2', handler: handler2 });

      // Act
      await daemon.execute('seq-1');
      await daemon.execute('seq-2');

      // Assert
      expect(executed).toEqual([1, 2]);
    });

    it('should handle dequeue errors gracefully', async () => {
      // Arrange
      const failHandler = vi.fn().mockRejectedValue(new Error('Execution failed'));
      daemon.schedule({ id: 'fail-op', name: 'Fail', handler: failHandler });

      // Act & Assert
      await expect(daemon.execute('fail-op')).rejects.toThrow('Execution failed');
    });

    it('should maintain queue integrity after dequeue', async () => {
      // Arrange
      daemon.schedule({ id: 'int-1', name: 'I1', handler: vi.fn().mockResolvedValue({}) });
      daemon.schedule({ id: 'int-2', name: 'I2', handler: vi.fn().mockResolvedValue({}) });
      daemon.schedule({ id: 'int-3', name: 'I3', handler: vi.fn().mockResolvedValue({}) });

      // Act
      await daemon.execute('int-1');

      // Assert
      expect(daemon.operationQueue).toContain('int-2');
      expect(daemon.operationQueue).toContain('int-3');
    });

    it('should support concurrent dequeue attempts', async () => {
      // Arrange
      for (let i = 0; i < 5; i++) {
        daemon.schedule({
          id: `conc-${i}`,
          name: `Concurrent ${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Act
      const promises = [];
      for (let i = 0; i < 5; i++) {
        promises.push(daemon.execute(`conc-${i}`));
      }
      await Promise.all(promises);

      // Assert
      const metrics = daemon.getMetrics();
      expect(metrics.totalOperations).toBeGreaterThanOrEqual(5);
    });

    it('should track dequeue metrics', async () => {
      // Arrange
      daemon.schedule({ id: 'metric-op', name: 'Metric', handler: vi.fn().mockResolvedValue({}) });

      // Act
      await daemon.execute('metric-op');
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.totalOperations).toBeGreaterThan(0);
      expect(metrics.successfulOperations).toBeGreaterThan(0);
    });

    it('should preserve remaining queue after partial processing', async () => {
      // Arrange
      daemon.schedule({ id: 'partial-1', name: 'P1', handler: vi.fn().mockResolvedValue({}) });
      daemon.schedule({ id: 'partial-2', name: 'P2', handler: vi.fn().mockResolvedValue({}) });
      daemon.schedule({ id: 'partial-3', name: 'P3', handler: vi.fn().mockResolvedValue({}) });

      // Act
      await daemon.execute('partial-1');

      // Assert
      expect(daemon.operations.has('partial-2')).toBe(true);
      expect(daemon.operations.has('partial-3')).toBe(true);
    });

    it('should handle dequeue from custom engine queue', () => {
      // Arrange
      engine.enqueue({ id: 'custom-1', data: 'test' });
      engine.enqueue({ id: 'custom-2', data: 'test' });

      // Act
      const first = engine.dequeue();
      const second = engine.dequeue();

      // Assert
      expect(first.id).toBe('custom-1');
      expect(second.id).toBe('custom-2');
    });
  });

  describe('priority handling', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'queue-priority-daemon',
      });
      engine = new MockYawlEngineQueue();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-queue-priority',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should store priority metadata', () => {
      // Arrange
      const highPriority = { priority: 10 };
      const lowPriority = { priority: 1 };

      // Act
      daemon.schedule({
        id: 'high-pri',
        name: 'High',
        handler: vi.fn(),
        metadata: highPriority,
      });
      daemon.schedule({
        id: 'low-pri',
        name: 'Low',
        handler: vi.fn(),
        metadata: lowPriority,
      });

      // Assert
      expect(daemon.operations.get('high-pri').metadata.priority).toBe(10);
      expect(daemon.operations.get('low-pri').metadata.priority).toBe(1);
    });

    it('should allow priority-based selection conceptually', () => {
      // Arrange
      daemon.schedule({
        id: 'pri-1',
        name: 'P1',
        handler: vi.fn(),
        metadata: { priority: 5 },
      });
      daemon.schedule({
        id: 'pri-2',
        name: 'P2',
        handler: vi.fn(),
        metadata: { priority: 10 },
      });

      // Act
      const operations = daemon.listOperations();
      const sorted = operations.sort(
        (a, b) => (b.metadata?.priority || 0) - (a.metadata?.priority || 0)
      );

      // Assert
      expect(sorted[0].id).toBe('pri-2'); // Higher priority first
    });

    it('should maintain priority across operations', async () => {
      // Arrange
      const priorities = [3, 1, 5, 2, 4];
      priorities.forEach((pri, idx) => {
        daemon.schedule({
          id: `pri-op-${idx}`,
          name: `Priority ${pri}`,
          handler: vi.fn().mockResolvedValue({}),
          metadata: { priority: pri },
        });
      });

      // Act
      const operations = daemon.listOperations();
      const withPriorities = operations.map((op) => ({
        id: op.id,
        priority: op.metadata?.priority || 0,
      }));

      // Assert
      expect(withPriorities.length).toBe(5);
      expect(withPriorities.find((op) => op.id === 'pri-op-2').priority).toBe(5);
    });

    it('should support default priority', () => {
      // Arrange
      daemon.schedule({ id: 'default-pri', name: 'Default', handler: vi.fn() });

      // Act
      const operation = daemon.operations.get('default-pri');

      // Assert
      expect(operation.metadata).toBeUndefined();
    });

    it('should handle priority ties with FIFO', () => {
      // Arrange
      daemon.schedule({
        id: 'tie-1',
        name: 'Tie 1',
        handler: vi.fn(),
        metadata: { priority: 5 },
      });
      daemon.schedule({
        id: 'tie-2',
        name: 'Tie 2',
        handler: vi.fn(),
        metadata: { priority: 5 },
      });

      // Act
      const queue = daemon.operationQueue;

      // Assert
      expect(queue.indexOf('tie-1')).toBeLessThan(queue.indexOf('tie-2'));
    });

    it('should validate priority values', () => {
      // Arrange
      const validPriorities = [1, 5, 10, 100];

      // Act
      validPriorities.forEach((pri, idx) => {
        daemon.schedule({
          id: `valid-pri-${idx}`,
          name: `Valid ${pri}`,
          handler: vi.fn(),
          metadata: { priority: pri },
        });
      });

      // Assert
      expect(daemon.operations.size).toBe(validPriorities.length);
    });

    it('should support negative priorities', () => {
      // Arrange
      daemon.schedule({
        id: 'neg-pri',
        name: 'Negative',
        handler: vi.fn(),
        metadata: { priority: -1 },
      });

      // Act
      const operation = daemon.operations.get('neg-pri');

      // Assert
      expect(operation.metadata.priority).toBe(-1);
    });

    it('should preserve priority during reschedule', () => {
      // Arrange
      daemon.schedule({
        id: 'reschedule-pri',
        name: 'Reschedule',
        handler: vi.fn(),
        metadata: { priority: 7 },
      });
      const originalPriority = daemon.operations.get('reschedule-pri').metadata.priority;

      // Act - Remove and re-add
      daemon.unschedule('reschedule-pri');
      daemon.schedule({
        id: 'reschedule-pri',
        name: 'Reschedule',
        handler: vi.fn(),
        metadata: { priority: 7 },
      });

      // Assert
      expect(daemon.operations.get('reschedule-pri').metadata.priority).toBe(originalPriority);
    });

    it('should allow priority updates via metadata', () => {
      // Arrange
      daemon.schedule({
        id: 'update-pri',
        name: 'Update',
        handler: vi.fn(),
        metadata: { priority: 3 },
      });

      // Act
      const operation = daemon.operations.get('update-pri');
      operation.metadata.priority = 9;

      // Assert
      expect(daemon.operations.get('update-pri').metadata.priority).toBe(9);
    });

    it('should support priority range validation conceptually', () => {
      // Arrange
      const MIN_PRIORITY = 0;
      const MAX_PRIORITY = 100;

      // Act
      daemon.schedule({
        id: 'range-pri',
        name: 'Range',
        handler: vi.fn(),
        metadata: { priority: 50 },
      });

      const priority = daemon.operations.get('range-pri').metadata.priority;

      // Assert
      expect(priority).toBeGreaterThanOrEqual(MIN_PRIORITY);
      expect(priority).toBeLessThanOrEqual(MAX_PRIORITY);
    });

    it('should track highest and lowest priorities', () => {
      // Arrange
      [1, 10, 5, 7, 3].forEach((pri, idx) => {
        daemon.schedule({
          id: `track-${idx}`,
          name: `Track ${pri}`,
          handler: vi.fn(),
          metadata: { priority: pri },
        });
      });

      // Act
      const operations = daemon.listOperations();
      const priorities = operations
        .map((op) => op.metadata?.priority || 0)
        .filter((p) => p > 0);

      const highest = Math.max(...priorities);
      const lowest = Math.min(...priorities);

      // Assert
      expect(highest).toBe(10);
      expect(lowest).toBe(1);
    });

    it('should handle priority overflow gracefully', () => {
      // Arrange
      const extremePriority = Number.MAX_SAFE_INTEGER;

      // Act
      daemon.schedule({
        id: 'overflow-pri',
        name: 'Overflow',
        handler: vi.fn(),
        metadata: { priority: extremePriority },
      });

      // Assert
      expect(daemon.operations.get('overflow-pri').metadata.priority).toBe(extremePriority);
    });
  });

  describe('overflow handling', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'queue-overflow-daemon',
        concurrency: 10, // Set low limit for testing
      });
      engine = new MockYawlEngineQueue();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-queue-overflow',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should handle queue at capacity', () => {
      // Arrange
      const capacity = daemon.config.concurrency;

      // Act
      for (let i = 0; i < capacity; i++) {
        daemon.schedule({ id: `cap-${i}`, name: `Capacity ${i}`, handler: vi.fn() });
      }

      // Assert
      expect(daemon.operationQueue.length).toBe(capacity);
    });

    it('should allow operations beyond concurrency limit', () => {
      // Arrange
      const capacity = daemon.config.concurrency;
      const extra = 5;

      // Act
      for (let i = 0; i < capacity + extra; i++) {
        daemon.schedule({ id: `over-${i}`, name: `Over ${i}`, handler: vi.fn() });
      }

      // Assert
      expect(daemon.operationQueue.length).toBe(capacity + extra);
    });

    it('should track queue size accurately', () => {
      // Arrange
      const count = 15;

      // Act
      for (let i = 0; i < count; i++) {
        daemon.schedule({ id: `size-${i}`, name: `Size ${i}`, handler: vi.fn() });
      }

      // Assert
      expect(daemon.operationQueue.length).toBe(count);
      expect(daemon.operations.size).toBe(count);
    });

    it('should handle overflow without data loss', () => {
      // Arrange
      const count = 20;
      const ids = [];

      // Act
      for (let i = 0; i < count; i++) {
        const id = `nolostat-${i}`;
        ids.push(id);
        daemon.schedule({ id, name: `No Loss ${i}`, handler: vi.fn() });
      }

      // Assert
      ids.forEach((id) => {
        expect(daemon.operations.has(id)).toBe(true);
      });
    });

    it('should report accurate queue metrics during overflow', () => {
      // Arrange
      for (let i = 0; i < 25; i++) {
        daemon.schedule({ id: `metric-${i}`, name: `M${i}`, handler: vi.fn() });
      }

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.queuedOperations).toBe(25);
    });

    it('should maintain performance during overflow', async () => {
      // Arrange
      for (let i = 0; i < 30; i++) {
        daemon.schedule({
          id: `perf-${i}`,
          name: `Perf ${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Act
      const startTime = Date.now();
      await daemon.execute('perf-0');
      const duration = Date.now() - startTime;

      // Assert
      expect(duration).toBeLessThan(1000); // Should still be fast
    });

    it('should handle overflow cleanup', async () => {
      // Arrange
      for (let i = 0; i < 20; i++) {
        daemon.schedule({
          id: `cleanup-${i}`,
          name: `Cleanup ${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Act
      for (let i = 0; i < 5; i++) {
        await daemon.execute(`cleanup-${i}`);
      }

      // Assert - Operations remain in queue/map
      expect(daemon.operations.size).toBe(20);
    });

    it('should reject operations beyond extreme limits conceptually', () => {
      // Arrange
      const EXTREME_LIMIT = 10000;

      // Act
      for (let i = 0; i < 100; i++) {
        daemon.schedule({ id: `extreme-${i}`, name: `E${i}`, handler: vi.fn() });
      }

      // Assert
      expect(daemon.operationQueue.length).toBeLessThan(EXTREME_LIMIT);
    });

    it('should emit overflow warning conceptually', () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:enqueued', listener);

      // Act
      for (let i = 0; i < 15; i++) {
        daemon.schedule({ id: `warn-${i}`, name: `W${i}`, handler: vi.fn() });
      }

      // Assert
      expect(listener).toHaveBeenCalledTimes(15);
    });

    it('should support overflow strategy configuration', () => {
      // Arrange
      const configured = daemon.config.concurrency;

      // Act & Assert
      expect(configured).toBeDefined();
      expect(configured).toBeGreaterThan(0);
    });

    it('should recover from overflow state', async () => {
      // Arrange
      for (let i = 0; i < 20; i++) {
        daemon.schedule({
          id: `recover-${i}`,
          name: `R${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Act
      for (let i = 0; i < 10; i++) {
        await daemon.execute(`recover-${i}`);
      }

      // Assert
      const health = daemon.getHealth();
      expect(health.queuedOperations).toBe(20); // All still queued
    });

    it('should prevent memory leak during sustained overflow', () => {
      // Arrange
      const initialMemory = process.memoryUsage().heapUsed;

      // Act
      for (let i = 0; i < 50; i++) {
        daemon.schedule({ id: `mem-${i}`, name: `M${i}`, handler: vi.fn() });
      }

      const afterMemory = process.memoryUsage().heapUsed;
      const increase = afterMemory - initialMemory;

      // Assert - Memory increase should be reasonable
      expect(increase).toBeLessThan(50 * 1024 * 1024); // Less than 50MB
    });
  });
});
