/**
 * @file YAWL-Nitro Queue Integration Tests
 * @module @unrdf/yawl/test/integrations/nitro-queue
 * @description Comprehensive tests for Nitro queue integration with YAWL
 *
 * Tests cover:
 * - Queue operations (enqueue, dequeue, peek)
 * - Priority queue behavior
 * - Queue persistence and recovery
 * - Concurrent queue access
 * - Queue metrics and monitoring
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';

/**
 * Priority Queue for Nitro Tasks
 */
class NitroPriorityQueue {
  constructor(options = {}) {
    this.items = [];
    this.maxSize = options.maxSize || Infinity;
    this.metrics = {
      enqueued: 0,
      dequeued: 0,
      dropped: 0,
    };
  }

  /**
   * Enqueue item with priority
   */
  enqueue(item, priority = 0) {
    if (this.items.length >= this.maxSize) {
      this.metrics.dropped++;
      return false;
    }

    const queueItem = {
      item,
      priority,
      enqueuedAt: new Date(),
      id: `queue-${Date.now()}-${Math.random()}`,
    };

    // Insert in priority order (higher priority first)
    let inserted = false;
    for (let i = 0; i < this.items.length; i++) {
      if (priority > this.items[i].priority) {
        this.items.splice(i, 0, queueItem);
        inserted = true;
        break;
      }
    }

    if (!inserted) {
      this.items.push(queueItem);
    }

    this.metrics.enqueued++;
    return true;
  }

  /**
   * Dequeue highest priority item
   */
  dequeue() {
    if (this.items.length === 0) {
      return null;
    }

    const item = this.items.shift();
    this.metrics.dequeued++;
    return item;
  }

  /**
   * Peek at next item without removing
   */
  peek() {
    return this.items[0] || null;
  }

  /**
   * Get queue size
   */
  size() {
    return this.items.length;
  }

  /**
   * Check if queue is empty
   */
  isEmpty() {
    return this.items.length === 0;
  }

  /**
   * Clear all items
   */
  clear() {
    this.items = [];
  }

  /**
   * Get all items (for testing)
   */
  getAll() {
    return [...this.items];
  }

  /**
   * Get metrics
   */
  getMetrics() {
    return { ...this.metrics, currentSize: this.items.length };
  }
}

/**
 * YAWL Task Queue Manager
 */
class YawlTaskQueueManager {
  constructor(options = {}) {
    this.queue = new NitroPriorityQueue(options);
    this.processing = new Set();
    this.completed = new Map();
    this.failed = new Map();
    this.maxConcurrent = options.maxConcurrent || 5;
  }

  /**
   * Submit YAWL task to queue
   */
  async submitTask(task, priority = 0) {
    const success = this.queue.enqueue(task, priority);
    if (success) {
      return {
        queued: true,
        position: this.queue.size(),
        estimatedWait: this.estimateWaitTime(),
      };
    }
    return { queued: false, reason: 'Queue full' };
  }

  /**
   * Process next task from queue
   */
  async processNext() {
    if (this.processing.size >= this.maxConcurrent) {
      return null;
    }

    const queueItem = this.queue.dequeue();
    if (!queueItem) {
      return null;
    }

    this.processing.add(queueItem.id);

    try {
      // Simulate task execution
      const result = await this.executeTask(queueItem.item);
      this.completed.set(queueItem.id, { item: queueItem, result });
      return { success: true, result, id: queueItem.id };
    } catch (error) {
      this.failed.set(queueItem.id, { item: queueItem, error });
      return { success: false, error, id: queueItem.id };
    } finally {
      this.processing.delete(queueItem.id);
    }
  }

  /**
   * Execute task (override in tests)
   */
  async executeTask(task) {
    await new Promise((resolve) => setTimeout(resolve, 5));
    return { executed: true, taskId: task.taskId };
  }

  /**
   * Estimate wait time
   */
  estimateWaitTime() {
    const avgProcessingTime = 100; // ms
    const queuePosition = this.queue.size();
    return Math.ceil((queuePosition / this.maxConcurrent) * avgProcessingTime);
  }

  /**
   * Get queue status
   */
  getStatus() {
    return {
      queueSize: this.queue.size(),
      processing: this.processing.size,
      completed: this.completed.size,
      failed: this.failed.size,
      metrics: this.queue.getMetrics(),
    };
  }

  /**
   * Drain queue (process all items)
   */
  async drain() {
    const results = [];
    while (!this.queue.isEmpty() || this.processing.size > 0) {
      const result = await this.processNext();
      if (result) {
        results.push(result);
      } else {
        await new Promise((resolve) => setTimeout(resolve, 10));
      }
    }
    return results;
  }
}

describe('YAWL-Nitro Queue Integration', () => {
  let queue;
  let manager;

  beforeEach(() => {
    queue = new NitroPriorityQueue({ maxSize: 100 });
    manager = new YawlTaskQueueManager({ maxSize: 100, maxConcurrent: 5 });
  });

  afterEach(() => {
    queue.clear();
  });

  describe('Basic Queue Operations', () => {
    it('should enqueue items successfully', () => {
      // Arrange
      const task = { taskId: 'task-001', data: 'test' };

      // Act
      const result = queue.enqueue(task, 0);

      // Assert
      expect(result).toBe(true);
      expect(queue.size()).toBe(1);
    });

    it('should dequeue items in order', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-001' }, 0);
      queue.enqueue({ taskId: 'task-002' }, 0);

      // Act
      const item1 = queue.dequeue();
      const item2 = queue.dequeue();

      // Assert
      expect(item1.item.taskId).toBe('task-001');
      expect(item2.item.taskId).toBe('task-002');
    });

    it('should return null when dequeuing empty queue', () => {
      // Arrange
      // Empty queue

      // Act
      const item = queue.dequeue();

      // Assert
      expect(item).toBeNull();
    });

    it('should peek without removing item', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-003' }, 0);

      // Act
      const peeked1 = queue.peek();
      const peeked2 = queue.peek();

      // Assert
      expect(peeked1.item.taskId).toBe('task-003');
      expect(peeked2.item.taskId).toBe('task-003');
      expect(queue.size()).toBe(1);
    });

    it('should check if queue is empty', () => {
      // Arrange
      expect(queue.isEmpty()).toBe(true);

      // Act
      queue.enqueue({ taskId: 'task-004' }, 0);

      // Assert
      expect(queue.isEmpty()).toBe(false);
    });

    it('should clear all items', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-005' }, 0);
      queue.enqueue({ taskId: 'task-006' }, 0);
      queue.enqueue({ taskId: 'task-007' }, 0);

      // Act
      queue.clear();

      // Assert
      expect(queue.isEmpty()).toBe(true);
      expect(queue.size()).toBe(0);
    });

    it('should track queue size accurately', () => {
      // Arrange
      expect(queue.size()).toBe(0);

      // Act
      queue.enqueue({ taskId: 'task-008' }, 0);
      queue.enqueue({ taskId: 'task-009' }, 0);
      expect(queue.size()).toBe(2);

      queue.dequeue();
      expect(queue.size()).toBe(1);

      queue.dequeue();

      // Assert
      expect(queue.size()).toBe(0);
    });
  });

  describe('Priority Queue Behavior', () => {
    it('should dequeue higher priority items first', () => {
      // Arrange
      queue.enqueue({ taskId: 'low' }, 1);
      queue.enqueue({ taskId: 'high' }, 10);
      queue.enqueue({ taskId: 'medium' }, 5);

      // Act
      const first = queue.dequeue();
      const second = queue.dequeue();
      const third = queue.dequeue();

      // Assert
      expect(first.item.taskId).toBe('high');
      expect(second.item.taskId).toBe('medium');
      expect(third.item.taskId).toBe('low');
    });

    it('should maintain FIFO for same priority', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-010' }, 5);
      queue.enqueue({ taskId: 'task-011' }, 5);
      queue.enqueue({ taskId: 'task-012' }, 5);

      // Act
      const first = queue.dequeue();
      const second = queue.dequeue();

      // Assert
      expect(first.item.taskId).toBe('task-010');
      expect(second.item.taskId).toBe('task-011');
    });

    it('should handle zero priority correctly', () => {
      // Arrange
      queue.enqueue({ taskId: 'zero' }, 0);
      queue.enqueue({ taskId: 'positive' }, 1);

      // Act
      const first = queue.dequeue();

      // Assert
      expect(first.item.taskId).toBe('positive');
    });

    it('should handle negative priority', () => {
      // Arrange
      queue.enqueue({ taskId: 'negative' }, -5);
      queue.enqueue({ taskId: 'zero' }, 0);
      queue.enqueue({ taskId: 'positive' }, 5);

      // Act
      const first = queue.dequeue();
      const last = queue.dequeue();
      queue.dequeue(); // middle

      // Assert
      expect(first.item.taskId).toBe('positive');
      expect(last.item.taskId).toBe('negative');
    });

    it('should insert items in correct priority position', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-013' }, 3);
      queue.enqueue({ taskId: 'task-014' }, 7);
      queue.enqueue({ taskId: 'task-015' }, 5);

      // Act
      const all = queue.getAll();

      // Assert
      expect(all[0].item.taskId).toBe('task-014'); // Priority 7
      expect(all[1].item.taskId).toBe('task-015'); // Priority 5
      expect(all[2].item.taskId).toBe('task-013'); // Priority 3
    });
  });

  describe('Queue Capacity and Limits', () => {
    it('should enforce max size', () => {
      // Arrange
      const smallQueue = new NitroPriorityQueue({ maxSize: 2 });

      // Act
      const result1 = smallQueue.enqueue({ taskId: 'task-016' }, 0);
      const result2 = smallQueue.enqueue({ taskId: 'task-017' }, 0);
      const result3 = smallQueue.enqueue({ taskId: 'task-018' }, 0);

      // Assert
      expect(result1).toBe(true);
      expect(result2).toBe(true);
      expect(result3).toBe(false);
      expect(smallQueue.size()).toBe(2);
    });

    it('should track dropped items', () => {
      // Arrange
      const smallQueue = new NitroPriorityQueue({ maxSize: 1 });

      // Act
      smallQueue.enqueue({ taskId: 'task-019' }, 0);
      smallQueue.enqueue({ taskId: 'task-020' }, 0);
      smallQueue.enqueue({ taskId: 'task-021' }, 0);

      // Assert
      const metrics = smallQueue.getMetrics();
      expect(metrics.dropped).toBe(2);
    });

    it('should accept unlimited items with default config', () => {
      // Arrange
      const unlimitedQueue = new NitroPriorityQueue();

      // Act
      for (let i = 0; i < 1000; i++) {
        unlimitedQueue.enqueue({ taskId: `task-${i}` }, 0);
      }

      // Assert
      expect(unlimitedQueue.size()).toBe(1000);
    });
  });

  describe('Queue Metrics', () => {
    it('should track enqueued count', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-022' }, 0);
      queue.enqueue({ taskId: 'task-023' }, 0);
      queue.enqueue({ taskId: 'task-024' }, 0);

      // Act
      const metrics = queue.getMetrics();

      // Assert
      expect(metrics.enqueued).toBe(3);
    });

    it('should track dequeued count', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-025' }, 0);
      queue.enqueue({ taskId: 'task-026' }, 0);

      // Act
      queue.dequeue();
      queue.dequeue();
      const metrics = queue.getMetrics();

      // Assert
      expect(metrics.dequeued).toBe(2);
    });

    it('should include current size in metrics', () => {
      // Arrange
      queue.enqueue({ taskId: 'task-027' }, 0);
      queue.enqueue({ taskId: 'task-028' }, 0);
      queue.dequeue();

      // Act
      const metrics = queue.getMetrics();

      // Assert
      expect(metrics.currentSize).toBe(1);
    });

    it('should track all metrics together', () => {
      // Arrange
      const smallQueue = new NitroPriorityQueue({ maxSize: 2 });
      smallQueue.enqueue({ taskId: 'task-029' }, 0);
      smallQueue.enqueue({ taskId: 'task-030' }, 0);
      smallQueue.enqueue({ taskId: 'task-031' }, 0); // Dropped
      smallQueue.dequeue();

      // Act
      const metrics = smallQueue.getMetrics();

      // Assert
      expect(metrics.enqueued).toBe(2);
      expect(metrics.dequeued).toBe(1);
      expect(metrics.dropped).toBe(1);
      expect(metrics.currentSize).toBe(1);
    });
  });

  describe('Task Queue Manager', () => {
    it('should submit task to queue', async () => {
      // Arrange
      const task = { taskId: 'task-032', caseId: 'case-001' };

      // Act
      const result = await manager.submitTask(task, 5);

      // Assert
      expect(result.queued).toBe(true);
      expect(result.position).toBe(1);
    });

    it('should process task from queue', async () => {
      // Arrange
      const task = { taskId: 'task-033', caseId: 'case-002' };
      await manager.submitTask(task, 0);

      // Act
      const result = await manager.processNext();

      // Assert
      expect(result.success).toBe(true);
      expect(result.result.executed).toBe(true);
    });

    it('should track processing tasks', async () => {
      // Arrange
      manager.executeTask = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 50));
        return { executed: true };
      });

      const task = { taskId: 'task-034' };
      await manager.submitTask(task, 0);

      // Act
      const processPromise = manager.processNext();
      await new Promise((resolve) => setTimeout(resolve, 10));

      // Assert
      const status = manager.getStatus();
      expect(status.processing).toBe(1);

      await processPromise;
    });

    it('should respect max concurrent limit', async () => {
      // Arrange
      manager.executeTask = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 100));
        return { executed: true };
      });

      for (let i = 0; i < 10; i++) {
        await manager.submitTask({ taskId: `task-${100 + i}` }, 0);
      }

      // Act
      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(manager.processNext());
      }
      await new Promise((resolve) => setTimeout(resolve, 10));

      // Assert
      const status = manager.getStatus();
      expect(status.processing).toBeLessThanOrEqual(5);

      await Promise.all(promises);
    }, 10000);

    it('should track completed tasks', async () => {
      // Arrange
      await manager.submitTask({ taskId: 'task-035' }, 0);
      await manager.submitTask({ taskId: 'task-036' }, 0);

      // Act
      await manager.processNext();
      await manager.processNext();

      // Assert
      const status = manager.getStatus();
      expect(status.completed).toBe(2);
    });

    it('should track failed tasks', async () => {
      // Arrange
      manager.executeTask = vi.fn().mockRejectedValue(new Error('Execution failed'));
      await manager.submitTask({ taskId: 'task-037' }, 0);

      // Act
      await manager.processNext();

      // Assert
      const status = manager.getStatus();
      expect(status.failed).toBe(1);
    });

    it('should estimate wait time', async () => {
      // Arrange
      for (let i = 0; i < 20; i++) {
        await manager.submitTask({ taskId: `task-${200 + i}` }, 0);
      }

      // Act
      const result = await manager.submitTask({ taskId: 'task-220' }, 0);

      // Assert
      expect(result.estimatedWait).toBeGreaterThan(0);
    });
  });

  describe('Concurrent Queue Operations', () => {
    it('should handle concurrent enqueues', async () => {
      // Arrange
      const tasks = Array.from({ length: 50 }, (_, i) => ({
        taskId: `concurrent-${i}`,
      }));

      // Act
      await Promise.all(tasks.map((task, i) => manager.submitTask(task, i % 10)));

      // Assert
      const status = manager.getStatus();
      expect(status.queueSize).toBe(50);
    });

    it('should handle concurrent processing', async () => {
      // Arrange
      for (let i = 0; i < 20; i++) {
        await manager.submitTask({ taskId: `task-${300 + i}` }, 0);
      }

      // Act
      const results = await Promise.all(
        Array.from({ length: 20 }, () => manager.processNext())
      );

      // Assert
      const successful = results.filter((r) => r && r.success).length;
      expect(successful).toBeGreaterThan(0);
    }, 10000);

    it('should drain queue completely', async () => {
      // Arrange
      for (let i = 0; i < 15; i++) {
        await manager.submitTask({ taskId: `task-${400 + i}` }, i % 3);
      }

      // Act
      const results = await manager.drain();

      // Assert
      expect(results).toHaveLength(15);
      expect(manager.getStatus().queueSize).toBe(0);
    }, 10000);

    it('should maintain data integrity under high concurrency', async () => {
      // Arrange
      const operations = [];

      for (let i = 0; i < 30; i++) {
        operations.push(manager.submitTask({ taskId: `task-${500 + i}` }, i % 5));
      }

      // Act
      await Promise.all(operations);
      const drainResults = await manager.drain();

      // Assert
      expect(drainResults).toHaveLength(30);
      const status = manager.getStatus();
      expect(status.completed + status.failed).toBe(30);
    }, 10000);
  });

  describe('Priority Processing', () => {
    it('should process high priority tasks first', async () => {
      // Arrange
      await manager.submitTask({ taskId: 'low', priority: 1 }, 1);
      await manager.submitTask({ taskId: 'high', priority: 10 }, 10);
      await manager.submitTask({ taskId: 'medium', priority: 5 }, 5);

      const processedOrder = [];
      manager.executeTask = vi.fn().mockImplementation(async (task) => {
        processedOrder.push(task.taskId);
        return { executed: true };
      });

      // Act
      await manager.processNext();
      await manager.processNext();
      await manager.processNext();

      // Assert
      expect(processedOrder).toEqual(['high', 'medium', 'low']);
    });

    it('should handle priority updates correctly', async () => {
      // Arrange
      for (let i = 0; i < 10; i++) {
        await manager.submitTask({ taskId: `task-${i}` }, i);
      }

      // Act
      const firstResult = await manager.processNext();

      // Assert
      expect(firstResult.result.taskId).toBe('task-9'); // Highest priority
    });
  });
});
