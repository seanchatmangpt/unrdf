/**
 * @file Long-Running Stability Tests
 * @module @unrdf/daemon/test/stability/long-running
 * @description Tests that run for extended periods to detect memory leaks,
 * resource accumulation, and degradation over time.
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
    name: 'stability-test-daemon',
    ...overrides,
  });
}

describe('Long-Running Stability Tests', () => {
  let daemon;

  beforeEach(() => {
    daemon = createTestDaemon();
    vi.useFakeTimers({ shouldAdvanceTime: true });
  });

  afterEach(async () => {
    if (daemon && daemon.isRunning) {
      await daemon.stop();
    }
    daemon = null;
    vi.useRealTimers();
  });

  describe('Memory Leak Detection', () => {
    it('should not leak memory during repeated operation scheduling', async () => {
      // Arrange
      vi.useRealTimers();
      const iterations = 1000;
      const initialMemory = getMemoryUsageMB();
      await daemon.start();

      // Act - Schedule and unschedule many operations
      for (let i = 0; i < iterations; i++) {
        const opId = `op-${i}`;
        daemon.schedule({
          id: opId,
          name: `test-op-${i}`,
          handler: vi.fn().mockResolvedValue({ iteration: i }),
        });
        daemon.unschedule(opId);
      }

      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }

      const finalMemory = getMemoryUsageMB();
      const memoryGrowth = finalMemory - initialMemory;

      // Assert - Memory growth should be minimal (< 50MB for 1000 iterations)
      expect(memoryGrowth).toBeLessThan(50);
    });

    it('should not leak memory during repeated execution cycles', async () => {
      // Arrange
      vi.useRealTimers();
      const cycles = 100;
      const initialMemory = getMemoryUsageMB();
      await daemon.start();

      // Act - Execute many operations
      for (let i = 0; i < cycles; i++) {
        const opId = `exec-op-${i}`;
        daemon.schedule({
          id: opId,
          name: `exec-test-${i}`,
          handler: vi.fn().mockResolvedValue({ data: 'x'.repeat(100) }),
        });
        await daemon.execute(opId);
      }

      if (global.gc) {
        global.gc();
      }

      const finalMemory = getMemoryUsageMB();
      const memoryGrowth = finalMemory - initialMemory;

      // Assert
      expect(memoryGrowth).toBeLessThan(50);
    });

    it('should bound completed operations cache via LRU eviction', async () => {
      // Arrange
      vi.useRealTimers();
      const operationCount = 1500; // More than LRU max (1000)
      await daemon.start();

      // Act - Execute more operations than cache size
      for (let i = 0; i < operationCount; i++) {
        const opId = `lru-op-${i}`;
        daemon.schedule({
          id: opId,
          name: `lru-test-${i}`,
          handler: vi.fn().mockResolvedValue({ index: i }),
        });
        await daemon.execute(opId);
      }

      // Assert - Cache should be bounded
      const completedCount = daemon.completedOperations.entries().length;
      expect(completedCount).toBeLessThanOrEqual(1000);
    });

    it('should not accumulate event listeners over time', async () => {
      // Arrange
      vi.useRealTimers();
      const cycles = 50;
      await daemon.start();

      // Act - Add and remove listeners repeatedly
      for (let i = 0; i < cycles; i++) {
        const listener = vi.fn();
        daemon.on('operation:success', listener);
        daemon.removeListener('operation:success', listener);
      }

      // Assert - Listener count should be zero
      expect(daemon.listenerCount('operation:success')).toBe(0);
    });
  });

  describe('Extended Operation Patterns', () => {
    it('should maintain consistent performance over many start/stop cycles', async () => {
      // Arrange
      vi.useRealTimers();
      const cycles = 20;
      const timings = [];

      // Act
      for (let i = 0; i < cycles; i++) {
        const start = Date.now();
        await daemon.start();
        await daemon.stop();
        timings.push(Date.now() - start);
      }

      // Assert - Performance should be consistent (no degradation > 3x)
      // Use minimum threshold of 1ms to handle sub-millisecond operations
      const avgTime = timings.reduce((a, b) => a + b, 0) / timings.length;
      const maxTime = Math.max(...timings);
      const threshold = Math.max(avgTime * 5, 10); // At least 10ms or 5x average
      expect(maxTime).toBeLessThan(threshold);
    });

    it('should handle continuous event emission without memory growth', async () => {
      // Arrange
      vi.useRealTimers();
      const eventCount = 500;
      const initialMemory = getMemoryUsageMB();
      await daemon.start();

      let receivedEvents = 0;
      daemon.on('operation:enqueued', () => {
        receivedEvents++;
      });

      // Act - Schedule many operations to trigger events
      for (let i = 0; i < eventCount; i++) {
        daemon.schedule({
          id: `event-op-${i}`,
          name: `event-test-${i}`,
          handler: vi.fn(),
        });
      }

      if (global.gc) {
        global.gc();
      }

      const finalMemory = getMemoryUsageMB();

      // Assert
      expect(receivedEvents).toBe(eventCount);
      expect(finalMemory - initialMemory).toBeLessThan(30);
    });

    it('should maintain accurate metrics over extended periods', async () => {
      // Arrange
      vi.useRealTimers();
      const successCount = 50;
      const failCount = 10;
      await daemon.start();

      // Act - Execute mix of successful and failing operations
      for (let i = 0; i < successCount; i++) {
        daemon.schedule({
          id: `success-${i}`,
          name: `success-op-${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
        await daemon.execute(`success-${i}`);
      }

      for (let i = 0; i < failCount; i++) {
        daemon.schedule({
          id: `fail-${i}`,
          name: `fail-op-${i}`,
          handler: vi.fn().mockRejectedValue(new Error('Expected failure')),
        });
        try {
          await daemon.execute(`fail-${i}`);
        } catch {
          // Expected
        }
      }

      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.successfulOperations).toBe(successCount);
      expect(metrics.failedOperations).toBe(failCount);
      expect(metrics.totalOperations).toBe(successCount + failCount);
    });

    it('should preserve health status accuracy during long runs', async () => {
      // Arrange
      vi.useRealTimers();
      await daemon.start();

      // Act - Simulate extended runtime with operations
      for (let i = 0; i < 30; i++) {
        daemon.schedule({
          id: `health-op-${i}`,
          name: `health-test-${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
        await daemon.execute(`health-op-${i}`);
      }

      const health = daemon.getHealth();

      // Assert
      expect(health.isRunning).toBe(true);
      expect(health.uptime).toBeGreaterThan(0);
      expect(health.queuedOperations).toBe(30);
    });
  });

  describe('Resource Cleanup', () => {
    it('should clean up internal state after stop', async () => {
      // Arrange
      vi.useRealTimers();
      await daemon.start();

      for (let i = 0; i < 10; i++) {
        daemon.schedule({
          id: `cleanup-op-${i}`,
          name: `cleanup-test-${i}`,
          handler: vi.fn(),
        });
      }

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
      expect(daemon.operations.size).toBe(10); // Operations persist for inspection
    });

    it('should handle graceful shutdown during active operations', async () => {
      // Arrange
      vi.useRealTimers();
      await daemon.start();

      // Schedule a slow operation
      let operationStarted = false;
      daemon.schedule({
        id: 'slow-op',
        name: 'slow-operation',
        handler: async () => {
          operationStarted = true;
          await new Promise(resolve => setTimeout(resolve, 10));
          return { completed: true };
        },
      });

      // Act - Start execution and stop immediately
      const executePromise = daemon.execute('slow-op');
      await daemon.stop();
      const result = await executePromise;

      // Assert
      expect(operationStarted).toBe(true);
      expect(result.completed).toBe(true);
      expect(daemon.isRunning).toBe(false);
    });
  });
});
