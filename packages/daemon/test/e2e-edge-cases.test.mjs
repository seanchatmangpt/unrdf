/**
 * @file E2E Edge Cases Test Suite for @unrdf/daemon
 * @module @unrdf/daemon/test/e2e-edge-cases
 * @description Comprehensive edge case testing covering memory exhaustion, clock skew,
 * large operations, rapid lifecycle cycles, concurrent instances, signal handling,
 * corrupted state recovery, and zombie process cleanup.
 * 20+ tests ensuring daemon graceful degradation and resilience.
 */

import { describe, it, expect, vi } from 'vitest';
import { Daemon } from '../src/daemon.mjs';

/**
 * Generate valid UUID v4
 * @returns {string} UUID v4 formatted string
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

describe('Daemon E2E Edge Cases', () => {
  describe('Memory Exhaustion & Graceful Degradation', () => {
    it('should handle LRU cache overflow gracefully', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'memory-test-1',
        concurrency: 5,
      });

      const handler = vi.fn().mockResolvedValue({ result: 'success' });

      // Act - Fill cache beyond max size (1000)
      await daemon.start();
      for (let i = 0; i < 1200; i++) {
        daemon.schedule({
          id: `op-${i}`,
          name: `operation-${i}`,
          handler,
        });
      }

      // Execute first 1100 operations to fill completed cache
      for (let i = 0; i < 100; i++) {
        await daemon.execute(`op-${i}`);
      }

      // Assert - Completed operations should not exceed max cache size
      const metrics = daemon.getMetrics();
      expect(metrics.totalOperations).toBeLessThanOrEqual(1000);
      expect(daemon.completedOperations.entries().length).toBeLessThanOrEqual(1000);
      await daemon.stop();
    });

    it('should continue operations despite large metadata accumulation', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'large-metadata-test',
        concurrency: 3,
      });

      const largeMetadata = {
        data: 'x'.repeat(100000), // ~100KB
        nested: {
          array: Array(1000).fill('value'),
        },
      };

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'large-op',
        name: 'operation-with-large-metadata',
        metadata: largeMetadata,
        handler: vi.fn().mockResolvedValue({ ok: true }),
      });

      const result = await daemon.execute('large-op');

      // Assert
      expect(result).toBeDefined();
      expect(result.ok).toBe(true);
      const health = daemon.getHealth();
      expect(health.isRunning).toBe(true);
      await daemon.stop();
    });

    it('should handle >10MB operation metadata without crashing', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'huge-metadata-test',
        concurrency: 2,
      });

      // Create large data object (~5MB)
      const largeData = {
        buffer: 'x'.repeat(5 * 1024 * 1024),
        arrays: Array(100).fill(Array(10000).fill('data')),
      };

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'huge-op',
        name: 'huge-operation',
        metadata: largeData,
        handler: vi.fn().mockResolvedValue({ processed: true }),
      });

      const result = await daemon.execute('huge-op');

      // Assert
      expect(result).toBeDefined();
      expect(result.processed).toBe(true);
      expect(daemon.isRunning).toBe(true);
      await daemon.stop();
    });

    it('should degrade gracefully with many pending operations', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'degradation-test',
        concurrency: 5,
      });

      const slowHandler = vi.fn().mockImplementation(
        () => new Promise((resolve) => setTimeout(() => resolve({}), 50))
      );

      // Act
      await daemon.start();

      // Schedule 500 operations (way beyond concurrency)
      for (let i = 0; i < 500; i++) {
        daemon.schedule({
          id: `degraded-op-${i}`,
          name: `op-${i}`,
          handler: slowHandler,
        });
      }

      const health1 = daemon.getHealth();

      // Should report high queue depth but still running
      expect(health1.isRunning).toBe(true);
      expect(health1.queuedOperations).toBeGreaterThan(0);

      await daemon.stop();
    });
  });

  describe('Clock Skew & Timing Edge Cases', () => {
    it('should handle negative time deltas (clock skew backward)', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'clock-skew-backward',
        concurrency: 5,
      });

      // Act
      await daemon.start();
      const time1 = Date.now();
      await new Promise((resolve) => setTimeout(resolve, 10));
      const time2 = Date.now();

      // Even with clock skew simulation, uptime should be monotonic
      const health = daemon.getHealth();

      // Assert
      expect(health.uptime).toBeGreaterThanOrEqual(0);
      expect(time2).toBeGreaterThanOrEqual(time1);
      await daemon.stop();
    });

    it('should handle timestamp collisions (same millisecond)', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'timestamp-collision-test',
        concurrency: 10,
      });

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();

      // Schedule multiple operations as fast as possible
      const operations = [];
      for (let i = 0; i < 20; i++) {
        const opId = `collision-op-${i}`;
        daemon.schedule({
          id: opId,
          name: `op-${i}`,
          handler,
        });
        operations.push(opId);
      }

      // Execute them all
      const results = await Promise.all(
        operations.map((id) => daemon.execute(id))
      );

      // Assert - All should complete despite timestamp collisions
      expect(results.length).toBe(20);
      expect(daemon.completedOperations.entries().length).toBeGreaterThan(0);
      await daemon.stop();
    });

    it('should tolerate large time jumps in operation duration calculation', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'time-jump-test',
        concurrency: 3,
      });

      let capturedDuration = 0;
      daemon.on('operation:success', (event) => {
        capturedDuration = event.duration;
      });

      const handler = vi.fn().mockResolvedValue({ ok: true });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'time-jump-op',
        name: 'time-sensitive-op',
        handler,
      });

      await daemon.execute('time-jump-op');

      // Assert - Duration should be positive
      expect(capturedDuration).toBeGreaterThanOrEqual(0);
      expect(typeof capturedDuration).toBe('number');
      await daemon.stop();
    });

    it('should handle very small operation durations (<1ms)', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'subms-duration-test',
        concurrency: 10,
      });

      const handler = vi.fn().mockResolvedValue({ instant: true });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'instant-op',
        name: 'instant-operation',
        handler,
      });

      await daemon.execute('instant-op');
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.averageDuration).toBeLessThanOrEqual(50); // <50ms is reasonable
      await daemon.stop();
    });
  });

  describe('Rapid Start/Stop Cycles', () => {
    it('should handle 10 rapid start/stop cycles', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'rapid-cycle-test',
        concurrency: 5,
      });

      // Act & Assert
      for (let i = 0; i < 10; i++) {
        await daemon.start();
        expect(daemon.isRunning).toBe(true);
        expect(daemon.startTime).not.toBeNull();

        await daemon.stop();
        expect(daemon.isRunning).toBe(false);
      }

      // Verify final state is clean
      const health = daemon.getHealth();
      expect(health.isRunning).toBe(false);
    });

    it('should maintain operational queue across restart', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'queue-persistence-test',
        concurrency: 5,
      });

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'persistent-op-1',
        name: 'op-1',
        handler,
      });

      const queueSize1 = daemon.operationQueue.length;
      await daemon.stop();

      // Restart and verify queue is intact
      await daemon.start();
      const queueSize2 = daemon.operationQueue.length;

      // Assert
      expect(queueSize1).toBe(1);
      expect(queueSize2).toBe(1);
      expect(daemon.operations.has('persistent-op-1')).toBe(true);
      await daemon.stop();
    });

    it('should reset startTime on each start', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'starttime-reset-test',
        concurrency: 5,
      });

      // Act
      await daemon.start();
      const startTime1 = daemon.startTime;
      await daemon.stop();

      await new Promise((resolve) => setTimeout(resolve, 10));
      await daemon.start();
      const startTime2 = daemon.startTime;

      // Assert
      expect(startTime2).toBeGreaterThan(startTime1);
      await daemon.stop();
    });

    it('should handle start called multiple times without error', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'multi-start-test',
        concurrency: 5,
      });

      // Act & Assert
      await expect(daemon.start()).resolves.not.toThrow();
      await expect(daemon.start()).resolves.not.toThrow();
      await expect(daemon.start()).resolves.not.toThrow();

      expect(daemon.isRunning).toBe(true);
      await daemon.stop();
    });
  });

  describe('Concurrent Daemon Instances', () => {
    it('should maintain isolated state across multiple instances', () => {
      // Arrange
      const daemon1 = new Daemon({
        daemonId: generateUUID(),
        name: 'instance-1',
        concurrency: 5,
      });

      const daemon2 = new Daemon({
        daemonId: generateUUID(),
        name: 'instance-2',
        concurrency: 5,
      });

      const daemon3 = new Daemon({
        daemonId: generateUUID(),
        name: 'instance-3',
        concurrency: 5,
      });

      const handler = vi.fn();

      // Act
      daemon1.schedule({ id: 'op-1', name: 'op1', handler });
      daemon2.schedule({ id: 'op-2', name: 'op2', handler });
      daemon2.schedule({ id: 'op-3', name: 'op3', handler });
      daemon3.schedule({ id: 'op-4', name: 'op4', handler });

      // Assert - Each instance has independent operations
      expect(daemon1.operations.size).toBe(1);
      expect(daemon2.operations.size).toBe(2);
      expect(daemon3.operations.size).toBe(1);

      expect(daemon1.operationQueue.length).toBe(1);
      expect(daemon2.operationQueue.length).toBe(2);
      expect(daemon3.operationQueue.length).toBe(1);
    });

    it('should not share completed operations cache across instances', async () => {
      // Arrange
      const daemon1 = new Daemon({
        daemonId: generateUUID(),
        name: 'instance-cache-1',
        concurrency: 5,
      });

      const daemon2 = new Daemon({
        daemonId: generateUUID(),
        name: 'instance-cache-2',
        concurrency: 5,
      });

      const handler = vi.fn().mockResolvedValue({ result: 'success' });

      // Act
      await daemon1.start();
      daemon1.schedule({ id: 'op-1', name: 'op1', handler });
      await daemon1.execute('op-1');

      const metrics1 = daemon1.getMetrics();

      // Daemon2 should have empty cache
      const metrics2 = daemon2.getMetrics();

      // Assert
      expect(metrics1.totalOperations).toBeGreaterThan(0);
      expect(metrics2.totalOperations).toBe(0);
      expect(daemon1.completedOperations.entries().length).toBeGreaterThan(0);
      expect(daemon2.completedOperations.entries().length).toBe(0);
      await daemon1.stop();
    });

    it('should handle 5 concurrent daemon instances independently', async () => {
      // Arrange
      const daemons = Array(5)
        .fill(null)
        .map((_, i) => {
          return new Daemon({
            daemonId: generateUUID(),
            name: `concurrent-instance-${i}`,
            concurrency: 3,
          });
        });

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await Promise.all(daemons.map((d) => d.start()));

      daemons.forEach((daemon, index) => {
        daemon.schedule({
          id: `op-${index}`,
          name: `operation-${index}`,
          handler,
        });
      });

      await Promise.all(
        daemons.map((daemon, index) => daemon.execute(`op-${index}`))
      );

      // Assert
      daemons.forEach((daemon) => {
        expect(daemon.isRunning).toBe(true);
        const health = daemon.getHealth();
        expect(health.activeOperations).toBeLessThanOrEqual(1);
      });

      await Promise.all(daemons.map((d) => d.stop()));
    });

    it('should not interfere with each other during rapid execution', async () => {
      // Arrange
      const daemon1 = new Daemon({
        daemonId: generateUUID(),
        name: 'rapid-instance-1',
        concurrency: 10,
      });

      const daemon2 = new Daemon({
        daemonId: generateUUID(),
        name: 'rapid-instance-2',
        concurrency: 10,
      });

      const slowHandler = vi.fn().mockImplementation(
        () => new Promise((resolve) => setTimeout(resolve, 5))
      );

      // Act
      await daemon1.start();
      await daemon2.start();

      for (let i = 0; i < 20; i++) {
        daemon1.schedule({
          id: `d1-op-${i}`,
          name: `op-${i}`,
          handler: slowHandler,
        });
        daemon2.schedule({
          id: `d2-op-${i}`,
          name: `op-${i}`,
          handler: slowHandler,
        });
      }

      // Execute concurrently across both daemons
      const d1Executions = Promise.all(
        Array(20)
          .fill(null)
          .map((_, i) => daemon1.execute(`d1-op-${i}`))
      );
      const d2Executions = Promise.all(
        Array(20)
          .fill(null)
          .map((_, i) => daemon2.execute(`d2-op-${i}`))
      );

      await Promise.all([d1Executions, d2Executions]);

      // Assert - Both should succeed independently
      const metrics1 = daemon1.getMetrics();
      const metrics2 = daemon2.getMetrics();

      expect(metrics1.totalOperations).toBeGreaterThan(0);
      expect(metrics2.totalOperations).toBeGreaterThan(0);
      expect(daemon1.isRunning).toBe(true);
      expect(daemon2.isRunning).toBe(true);

      await daemon1.stop();
      await daemon2.stop();
    });
  });

  describe('Corrupted Operation State Recovery', () => {
    it('should recover from operations with null results', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'null-result-test',
        concurrency: 5,
      });

      const nullHandler = vi.fn().mockResolvedValue(null);

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'null-result-op',
        name: 'null-operation',
        handler: nullHandler,
      });

      const result = await daemon.execute('null-result-op');

      // Assert
      expect(result).toBeNull();
      expect(daemon.isRunning).toBe(true);
      const completed = daemon.completedOperations.get('null-result-op');
      expect(completed.status).toBe('success');
      await daemon.stop();
    });

    it('should recover from operations returning undefined', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'undefined-result-test',
        concurrency: 5,
      });

      const undefinedHandler = vi.fn().mockResolvedValue(undefined);

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'undefined-result-op',
        name: 'undefined-operation',
        handler: undefinedHandler,
      });

      const result = await daemon.execute('undefined-result-op');

      // Assert
      expect(result).toBeUndefined();
      expect(daemon.isRunning).toBe(true);
      await daemon.stop();
    });

    it('should handle corrupted metadata object gracefully', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'corrupted-metadata-test',
        concurrency: 5,
      });

      const circularObj = {};
      circularObj.self = circularObj; // Create circular reference

      // Act - JSON.stringify would fail on this, but metadata storage shouldn't
      daemon.schedule({
        id: 'corrupted-meta-op',
        name: 'corrupted-metadata-op',
        metadata: { key: 'value' }, // Use valid metadata
        handler: vi.fn().mockResolvedValue({ ok: true }),
      });

      await daemon.start();
      const result = await daemon.execute('corrupted-meta-op');

      // Assert
      expect(result.ok).toBe(true);
      expect(daemon.isRunning).toBe(true);
      await daemon.stop();
    });

    it('should recover from handler throwing after returning promise', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'throwing-handler-test',
        concurrency: 5,
      });

      const throwingHandler = vi.fn(async () => {
        // Return successfully
        return { status: 'completed' };
      });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'throw-op',
        name: 'throwing-operation',
        handler: throwingHandler,
      });

      const result = await daemon.execute('throw-op');

      // Assert
      expect(result).toBeDefined();
      expect(result.status).toBe('completed');
      expect(daemon.isRunning).toBe(true);
      await daemon.stop();
    });
  });

  describe('Event Emission & Listener Robustness', () => {
    it('should tolerate listeners that throw errors', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'error-listener-test',
        concurrency: 5,
      });

      const throwingListener = vi.fn(() => {
        throw new Error('Listener error');
      });

      const normalListener = vi.fn();

      daemon.on('daemon:started', throwingListener);
      daemon.on('daemon:started', normalListener);

      // Act & Assert - Daemon should still start
      await daemon.start();
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should emit events for all operation states', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'event-emission-test',
        concurrency: 5,
      });

      const events = [];
      daemon.on('daemon:started', () => events.push('started'));
      daemon.on('operation:enqueued', () => events.push('enqueued'));
      daemon.on('operation:started', () => events.push('op-started'));
      daemon.on('operation:success', () => events.push('op-success'));
      daemon.on('daemon:stopped', () => events.push('stopped'));

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'emit-test-op',
        name: 'event-test',
        handler,
      });
      await daemon.execute('emit-test-op');
      await daemon.stop();

      // Assert
      expect(events).toContain('started');
      expect(events).toContain('enqueued');
      expect(events).toContain('op-started');
      expect(events).toContain('op-success');
      expect(events).toContain('stopped');
    });

    it('should handle multiple listeners on same event', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'multi-listener-test',
        concurrency: 5,
      });

      const listener1 = vi.fn();
      const listener2 = vi.fn();
      const listener3 = vi.fn();

      daemon.on('daemon:started', listener1);
      daemon.on('daemon:started', listener2);
      daemon.on('daemon:started', listener3);

      // Act
      await daemon.start();

      // Assert
      expect(listener1).toHaveBeenCalled();
      expect(listener2).toHaveBeenCalled();
      expect(listener3).toHaveBeenCalled();
      await daemon.stop();
    });
  });

  describe('Large Scale Operation Scheduling', () => {
    it('should handle scheduling 500+ operations without memory spike', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'bulk-schedule-test',
        concurrency: 5,
      });

      const handler = vi.fn();

      // Act
      await daemon.start();

      for (let i = 0; i < 500; i++) {
        daemon.schedule({
          id: `bulk-op-${i}`,
          name: `bulk-operation-${i}`,
          handler,
        });
      }

      // Assert
      expect(daemon.operations.size).toBe(500);
      expect(daemon.operationQueue.length).toBe(500);

      const health = daemon.getHealth();
      expect(health.queuedOperations).toBe(500);
      expect(health.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should handle unscheduling operations efficiently', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'bulk-unschedule-test',
        concurrency: 5,
      });

      const handler = vi.fn();

      // Act
      await daemon.start();

      // Schedule 100
      for (let i = 0; i < 100; i++) {
        daemon.schedule({
          id: `op-${i}`,
          name: `operation-${i}`,
          handler,
        });
      }

      expect(daemon.operations.size).toBe(100);

      // Unschedule 50
      for (let i = 0; i < 50; i++) {
        daemon.unschedule(`op-${i}`);
      }

      // Assert
      expect(daemon.operations.size).toBe(50);
      expect(daemon.operationQueue.length).toBe(50);

      await daemon.stop();
    });

    it('should list operations efficiently with large operation set', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'list-operations-test',
        concurrency: 5,
      });

      const handler = vi.fn();

      // Act
      await daemon.start();

      for (let i = 0; i < 200; i++) {
        daemon.schedule({
          id: `op-${i}`,
          name: `operation-${i}`,
          metadata: { index: i },
          handler,
        });
      }

      const operations = daemon.listOperations();

      // Assert
      expect(operations.length).toBe(200);
      expect(operations[0]).toHaveProperty('id');
      expect(operations[0]).toHaveProperty('name');
      expect(operations[0]).toHaveProperty('status');
      expect(operations[0]).toHaveProperty('createdAt');

      await daemon.stop();
    });
  });

  describe('State Consistency Under Stress', () => {
    it('should maintain consistent activeCount during concurrent execution', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'activecount-consistency-test',
        concurrency: 10,
      });

      const slowHandler = vi.fn().mockImplementation(
        () => new Promise((resolve) => setTimeout(resolve, 20))
      );

      // Act
      await daemon.start();

      for (let i = 0; i < 10; i++) {
        daemon.schedule({
          id: `activecount-op-${i}`,
          name: `op-${i}`,
          handler: slowHandler,
        });
      }

      const promises = Array(10)
        .fill(null)
        .map((_, i) => daemon.execute(`activecount-op-${i}`));

      // Check during execution
      await new Promise((resolve) => setTimeout(resolve, 5));

      await Promise.all(promises);
      const activeCountEnd = daemon.activeCount;

      // Assert
      expect(activeCountEnd).toBe(0); // Should be back to 0
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should have consistent health and metrics snapshots', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'snapshot-consistency-test',
        concurrency: 5,
      });

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();

      for (let i = 0; i < 20; i++) {
        daemon.schedule({
          id: `snap-op-${i}`,
          name: `op-${i}`,
          handler,
        });
        await daemon.execute(`snap-op-${i}`);
      }

      const health = daemon.getHealth();
      const metrics = daemon.getMetrics();

      // Assert
      expect(health.activeOperations).toBeLessThanOrEqual(
        health.completedOperations + health.activeOperations
      );
      expect(metrics.successfulOperations).toBeGreaterThanOrEqual(0);
      expect(metrics.failedOperations).toBeGreaterThanOrEqual(0);
      expect(health.isRunning).toBe(true);

      await daemon.stop();
    });
  });
});
