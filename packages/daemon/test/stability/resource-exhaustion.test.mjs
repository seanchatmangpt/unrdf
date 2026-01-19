/**
 * @file Resource Exhaustion Tests
 * @module @unrdf/daemon/test/stability/resource-exhaustion
 * @description Tests for daemon behavior under resource pressure including
 * high operation load, memory constraints, and queue saturation.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { Daemon } from '../../src/daemon.mjs';

/**
 * Generate a valid UUID v4 for testing
 * @returns {string} UUID v4 string
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Get memory usage in MB
 * @returns {number} Heap used in megabytes
 */
function getMemoryUsageMB() {
  const usage = process.memoryUsage();
  return usage.heapUsed / (1024 * 1024);
}

/**
 * Create a daemon with default test config
 * @param {Object} overrides - Config overrides
 * @returns {Daemon} Configured daemon instance
 */
function createTestDaemon(overrides = {}) {
  return new Daemon({
    daemonId: generateUUID(),
    name: 'resource-test-daemon',
    ...overrides,
  });
}

describe('Resource Exhaustion Tests', () => {
  let daemon;

  beforeEach(() => {
    daemon = createTestDaemon();
  });

  afterEach(async () => {
    if (daemon && daemon.isRunning) {
      await daemon.stop();
    }
    daemon = null;
  });

  describe('Operation Queue Saturation', () => {
    it('should handle queue saturation with many pending operations', async () => {
      // Arrange
      await daemon.start();
      const operationCount = 5000;

      // Act - Fill queue with operations
      for (let i = 0; i < operationCount; i++) {
        daemon.schedule({
          id: `queue-${i}`,
          name: `queue-op-${i}`,
          handler: vi.fn().mockResolvedValue({ index: i }),
        });
      }

      // Assert
      expect(daemon.operationQueue.length).toBe(operationCount);
      expect(daemon.operations.size).toBe(operationCount);

      // Should still be responsive
      const health = daemon.getHealth();
      expect(health.queuedOperations).toBe(operationCount);
    });

    it('should process large queue efficiently', async () => {
      // Arrange
      await daemon.start();
      const batchSize = 100;
      const startTime = Date.now();

      // Act - Schedule and execute operations
      for (let i = 0; i < batchSize; i++) {
        daemon.schedule({
          id: `batch-${i}`,
          name: `batch-op-${i}`,
          handler: vi.fn().mockResolvedValue({ index: i }),
        });
      }

      const results = [];
      for (let i = 0; i < batchSize; i++) {
        results.push(await daemon.execute(`batch-${i}`));
      }

      const duration = Date.now() - startTime;

      // Assert - Should complete in reasonable time
      expect(results.length).toBe(batchSize);
      expect(duration).toBeLessThan(5000); // 5 second budget
    });

    it('should maintain FIFO order in queue', async () => {
      // Arrange
      await daemon.start();
      const orderCheck = [];

      // Act
      for (let i = 0; i < 20; i++) {
        daemon.schedule({
          id: `order-${i}`,
          name: `order-op-${i}`,
          handler: vi.fn().mockImplementation(() => {
            orderCheck.push(i);
            return Promise.resolve({ order: i });
          }),
        });
      }

      for (let i = 0; i < 20; i++) {
        await daemon.execute(`order-${i}`);
      }

      // Assert - Operations should be executed in order
      expect(orderCheck).toEqual([...Array(20).keys()]);
    });
  });

  describe('Memory Pressure Handling', () => {
    it('should handle operations with large payloads', async () => {
      // Arrange
      await daemon.start();
      const largePayload = 'x'.repeat(1024 * 100); // 100KB payload

      daemon.schedule({
        id: 'large-payload-op',
        name: 'large-payload-operation',
        handler: vi.fn().mockResolvedValue({ data: largePayload }),
      });

      // Act
      const result = await daemon.execute('large-payload-op');

      // Assert
      expect(result.data.length).toBe(1024 * 100);
    });

    it('should handle many operations with moderate payloads', async () => {
      // Arrange
      await daemon.start();
      const operationCount = 200;
      const payloadSize = 1024 * 10; // 10KB each
      const initialMemory = getMemoryUsageMB();

      // Act
      for (let i = 0; i < operationCount; i++) {
        daemon.schedule({
          id: `moderate-${i}`,
          name: `moderate-op-${i}`,
          handler: vi.fn().mockResolvedValue({
            data: 'x'.repeat(payloadSize),
            index: i,
          }),
        });
        await daemon.execute(`moderate-${i}`);
      }

      if (global.gc) {
        global.gc();
      }

      const finalMemory = getMemoryUsageMB();
      const memoryGrowth = finalMemory - initialMemory;

      // Assert - Memory growth should be bounded
      expect(memoryGrowth).toBeLessThan(100); // < 100MB growth
    });

    it('should release memory for completed operations via LRU', async () => {
      // Arrange
      await daemon.start();
      const initialCacheSize = daemon.completedOperations.entries().length;

      // Act - Fill beyond LRU capacity
      for (let i = 0; i < 1200; i++) {
        daemon.schedule({
          id: `lru-test-${i}`,
          name: `lru-test-op-${i}`,
          handler: vi.fn().mockResolvedValue({ index: i }),
        });
        await daemon.execute(`lru-test-${i}`);
      }

      // Assert - Cache should be capped at 1000
      const finalCacheSize = daemon.completedOperations.entries().length;
      expect(finalCacheSize).toBeLessThanOrEqual(1000);
      expect(finalCacheSize).toBeGreaterThan(initialCacheSize);
    });
  });

  describe('Concurrent Operation Load', () => {
    it('should handle concurrent execute calls', async () => {
      // Arrange
      await daemon.start();
      const concurrentCount = 50;

      for (let i = 0; i < concurrentCount; i++) {
        daemon.schedule({
          id: `concurrent-${i}`,
          name: `concurrent-op-${i}`,
          handler: vi.fn().mockImplementation(async () => {
            await new Promise(resolve => setTimeout(resolve, 5));
            return { index: i };
          }),
        });
      }

      // Act - Execute all concurrently
      const promises = [];
      for (let i = 0; i < concurrentCount; i++) {
        promises.push(daemon.execute(`concurrent-${i}`));
      }

      const results = await Promise.all(promises);

      // Assert
      expect(results.length).toBe(concurrentCount);
      results.forEach((result, i) => {
        expect(result.index).toBe(i);
      });
    });

    it('should track active operation count accurately under load', async () => {
      // Arrange
      await daemon.start();
      const maxActiveObserved = [];

      for (let i = 0; i < 10; i++) {
        daemon.schedule({
          id: `active-${i}`,
          name: `active-op-${i}`,
          handler: vi.fn().mockImplementation(async () => {
            maxActiveObserved.push(daemon.activeCount);
            await new Promise(resolve => setTimeout(resolve, 10));
            return {};
          }),
        });
      }

      // Act
      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(daemon.execute(`active-${i}`));
      }
      await Promise.all(promises);

      // Assert
      expect(Math.max(...maxActiveObserved)).toBeGreaterThan(0);
      expect(daemon.activeCount).toBe(0); // Should be zero after completion
    });

    it('should maintain metrics accuracy under concurrent load', async () => {
      // Arrange
      await daemon.start();
      const successCount = 30;
      const failCount = 20;

      // Schedule successes
      for (let i = 0; i < successCount; i++) {
        daemon.schedule({
          id: `metric-success-${i}`,
          name: `metric-success-op-${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Schedule failures
      for (let i = 0; i < failCount; i++) {
        daemon.schedule({
          id: `metric-fail-${i}`,
          name: `metric-fail-op-${i}`,
          handler: vi.fn().mockRejectedValue(new Error('Expected')),
        });
      }

      // Act - Execute all concurrently
      const successPromises = [];
      for (let i = 0; i < successCount; i++) {
        successPromises.push(daemon.execute(`metric-success-${i}`));
      }

      const failPromises = [];
      for (let i = 0; i < failCount; i++) {
        failPromises.push(
          daemon.execute(`metric-fail-${i}`).catch(() => null)
        );
      }

      await Promise.all([...successPromises, ...failPromises]);

      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.successfulOperations).toBe(successCount);
      expect(metrics.failedOperations).toBe(failCount);
      expect(metrics.totalOperations).toBe(successCount + failCount);
    });
  });

  describe('Event Listener Pressure', () => {
    it('should handle many listeners on single event', async () => {
      // Arrange
      await daemon.start();
      const listenerCount = 100;
      const callCounts = new Array(listenerCount).fill(0);

      for (let i = 0; i < listenerCount; i++) {
        daemon.on('operation:enqueued', () => {
          callCounts[i]++;
        });
      }

      // Act
      daemon.schedule({
        id: 'listener-pressure-op',
        name: 'listener-pressure-operation',
        handler: vi.fn(),
      });

      // Assert - All listeners should have been called
      callCounts.forEach((count) => {
        expect(count).toBe(1);
      });
    });

    it('should handle rapid event emissions', async () => {
      // Arrange
      await daemon.start();
      let eventCount = 0;
      daemon.on('operation:enqueued', () => {
        eventCount++;
      });

      // Act - Fire many events rapidly
      for (let i = 0; i < 500; i++) {
        daemon.schedule({
          id: `rapid-event-${i}`,
          name: `rapid-event-op-${i}`,
          handler: vi.fn(),
        });
      }

      // Assert
      expect(eventCount).toBe(500);
    });
  });

  describe('Stress Patterns', () => {
    it('should handle interleaved schedule/unschedule operations', async () => {
      // Arrange
      await daemon.start();
      const iterations = 200;

      // Act - Interleave schedule and unschedule
      for (let i = 0; i < iterations; i++) {
        daemon.schedule({
          id: `interleave-${i}`,
          name: `interleave-op-${i}`,
          handler: vi.fn(),
        });

        if (i > 0 && i % 2 === 0) {
          daemon.unschedule(`interleave-${i - 1}`);
        }
      }

      // Assert - Should have ~half the operations remaining
      expect(daemon.operations.size).toBeGreaterThan(0);
      expect(daemon.operations.size).toBeLessThan(iterations);
    });

    it('should handle burst scheduling after idle period', async () => {
      // Arrange
      await daemon.start();

      // Simulate idle period
      await new Promise(resolve => setTimeout(resolve, 50));

      // Act - Burst schedule
      const burstSize = 100;
      const startTime = Date.now();

      for (let i = 0; i < burstSize; i++) {
        daemon.schedule({
          id: `burst-${i}`,
          name: `burst-op-${i}`,
          handler: vi.fn().mockResolvedValue({ burst: true }),
        });
      }

      const scheduleDuration = Date.now() - startTime;

      // Assert - Scheduling should be fast even after idle
      expect(scheduleDuration).toBeLessThan(100);
      expect(daemon.operationQueue.length).toBe(burstSize);
    });

    it('should maintain health reporting under stress', async () => {
      // Arrange
      await daemon.start();

      // Act - Create stress conditions while checking health
      const healthSnapshots = [];

      for (let i = 0; i < 50; i++) {
        daemon.schedule({
          id: `stress-${i}`,
          name: `stress-op-${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });

        if (i % 10 === 0) {
          healthSnapshots.push(daemon.getHealth());
        }
      }

      // Assert - Health should always be reportable
      healthSnapshots.forEach((health) => {
        expect(health.nodeId).toBeDefined();
        expect(typeof health.isRunning).toBe('boolean');
        expect(typeof health.queuedOperations).toBe('number');
      });
    });
  });
});
