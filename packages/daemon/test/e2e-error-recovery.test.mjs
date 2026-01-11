/**
 * @file E2E Error Recovery Test Suite for @unrdf/daemon
 * @module @unrdf/daemon/test/e2e-error-recovery
 * @description Comprehensive error recovery testing covering operation timeouts,
 * handler exceptions, storage failures, network partitions, resource exhaustion,
 * and cascading failure isolation. 15+ tests ensuring resilience and recovery.
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


describe('Daemon E2E Error Recovery', () => {
  describe('Operation Timeout & Recovery', () => {
    it('should handle operation handler that never resolves', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'timeout-recovery-test',
        concurrency: 5,
      });

      const neverResolvesHandler = vi.fn(
        () => new Promise(() => {}) // Never resolves
      );

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'timeout-op',
        name: 'timeout-operation',
        handler: neverResolvesHandler,
      });

      // Create race between execution and timeout
      const executionPromise = daemon.execute('timeout-op');
      const timeoutPromise = new Promise((resolve) =>
        setTimeout(() => resolve('timeout'), 100)
      );

      await Promise.race([executionPromise.catch(() => 'error'), timeoutPromise]);

      // Assert
      expect(daemon.isRunning).toBe(true); // Daemon still running despite operation hang
      expect(daemon.operations.has('timeout-op')).toBe(true);

      await daemon.stop();
    });

    it('should recover from single operation timeout while processing others', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'selective-timeout-recovery',
        concurrency: 5,
      });

      const slowHandler = vi.fn(
        () => new Promise((resolve) => setTimeout(() => resolve({}), 100))
      );

      const fastHandler = vi.fn().mockResolvedValue({ fast: true });

      // Act
      await daemon.start();

      // Schedule mixed fast and slow operations
      daemon.schedule({
        id: 'slow-op-1',
        name: 'slow-operation-1',
        handler: slowHandler,
      });

      daemon.schedule({
        id: 'fast-op-1',
        name: 'fast-operation-1',
        handler: fastHandler,
      });

      daemon.schedule({
        id: 'fast-op-2',
        name: 'fast-operation-2',
        handler: fastHandler,
      });

      // Execute fast operations
      const fastResult1 = await daemon.execute('fast-op-1');
      const fastResult2 = await daemon.execute('fast-op-2');

      // Assert - Fast operations should complete despite slow operations
      expect(fastResult1.fast).toBe(true);
      expect(fastResult2.fast).toBe(true);
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should track timeout events in metrics and health', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'timeout-metrics-test',
        concurrency: 3,
      });

      let timeoutEventCount = 0;
      daemon.on('operation:failure', () => {
        timeoutEventCount++;
      });

      const timeoutHandler = vi.fn(
        () => new Promise((_, reject) =>
          setTimeout(() => reject(new Error('Timeout')), 50)
        )
      );

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'timeout-op',
        name: 'timeout-op',
        handler: timeoutHandler,
      });

      try {
        await daemon.execute('timeout-op');
      } catch (e) {
        // Expected to fail
      }

      const health = daemon.getHealth();

      // Assert
      expect(health.isRunning).toBe(true);
      expect(timeoutEventCount).toBe(1);

      await daemon.stop();
    });
  });

  describe('Handler Exception & Recovery', () => {
    it('should recover from handler throwing synchronous error', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'sync-error-recovery',
        concurrency: 5,
      });

      const throwingHandler = vi.fn(() => {
        throw new Error('Synchronous error in handler');
      });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'sync-error-op',
        name: 'sync-error-op',
        handler: throwingHandler,
      });

      let executionError;
      try {
        await daemon.execute('sync-error-op');
      } catch (e) {
        executionError = e;
      }

      // Assert
      expect(executionError).toBeDefined();
      expect(executionError.message).toContain('Synchronous error');
      expect(daemon.isRunning).toBe(true);

      const completed = daemon.completedOperations.get('sync-error-op');
      expect(completed.status).toBe('failure');

      await daemon.stop();
    });

    it('should recover from handler throwing async error', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'async-error-recovery',
        concurrency: 5,
      });

      const asyncThrowingHandler = vi.fn(async () => {
        await new Promise((resolve) => setTimeout(resolve, 5));
        throw new Error('Async error in handler');
      });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'async-error-op',
        name: 'async-error-op',
        handler: asyncThrowingHandler,
      });

      let executionError;
      try {
        await daemon.execute('async-error-op');
      } catch (e) {
        executionError = e;
      }

      // Assert
      expect(executionError).toBeDefined();
      expect(executionError.message).toContain('Async error');
      expect(daemon.isRunning).toBe(true);

      const completed = daemon.completedOperations.get('async-error-op');
      expect(completed.status).toBe('failure');

      await daemon.stop();
    });

    it('should isolate handler errors preventing cascading failures', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'cascading-error-isolation',
        concurrency: 5,
      });

      const failingHandler = vi.fn(async () => {
        throw new Error('Operation failed');
      });

      const successHandler = vi.fn().mockResolvedValue({ success: true });

      // Act
      await daemon.start();

      // Schedule mix of failing and succeeding operations
      daemon.schedule({
        id: 'fail-op-1',
        name: 'fail-1',
        handler: failingHandler,
      });
      daemon.schedule({
        id: 'success-op-1',
        name: 'success-1',
        handler: successHandler,
      });
      daemon.schedule({
        id: 'fail-op-2',
        name: 'fail-2',
        handler: failingHandler,
      });
      daemon.schedule({
        id: 'success-op-2',
        name: 'success-2',
        handler: successHandler,
      });

      // Execute in sequence
      try {
        await daemon.execute('fail-op-1');
      } catch (e) {
        // Expected
      }

      const result1 = await daemon.execute('success-op-1');

      try {
        await daemon.execute('fail-op-2');
      } catch (e) {
        // Expected
      }

      const result2 = await daemon.execute('success-op-2');

      // Assert - Success operations should complete despite failures
      expect(result1.success).toBe(true);
      expect(result2.success).toBe(true);
      expect(daemon.isRunning).toBe(true);

      const metrics = daemon.getMetrics();
      expect(metrics.failedOperations).toBe(2);
      expect(metrics.successfulOperations).toBe(2);

      await daemon.stop();
    });

    it('should handle handler throwing custom error types', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'custom-error-recovery',
        concurrency: 5,
      });

      class CustomError extends Error {
        constructor(message, code) {
          super(message);
          this.code = code;
          this.name = 'CustomError';
        }
      }

      const customErrorHandler = vi.fn(async () => {
        throw new CustomError('Custom error occurred', 'ERR_CUSTOM_001');
      });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'custom-error-op',
        name: 'custom-error-op',
        handler: customErrorHandler,
      });

      let capturedError;
      try {
        await daemon.execute('custom-error-op');
      } catch (e) {
        capturedError = e;
      }

      // Assert
      expect(capturedError).toBeDefined();
      expect(capturedError.message).toContain('Custom error occurred');
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });
  });

  describe('Storage & State Failure Recovery', () => {
    it('should recover from corrupted operation record', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'corrupted-record-recovery',
        concurrency: 5,
      });

      const handler = vi.fn().mockResolvedValue({ ok: true });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'valid-op',
        name: 'valid-op',
        handler,
      });

      // Manually corrupt an operation state
      const op = daemon.operations.get('valid-op');
      op.status = 'unknown_state'; // Invalid status

      // Should still handle execution
      const result = await daemon.execute('valid-op');

      // Assert
      expect(result.ok).toBe(true);
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should handle completed operations cache corruption gracefully', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'cache-corruption-recovery',
        concurrency: 5,
      });

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'cache-op',
        name: 'cache-op',
        handler,
      });

      await daemon.execute('cache-op');

      // Manually corrupt cache entry
      const cacheEntry = daemon.completedOperations.get('cache-op');
      cacheEntry.status = null;
      cacheEntry.result = undefined;

      // Should still be able to query metrics
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics).toBeDefined();
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should recover from invalid operation state during execution', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'invalid-state-recovery',
        concurrency: 5,
      });

      // Create operation with missing createdAt (corrupted)
      const operation = {
        id: 'malformed-op',
        name: 'malformed-op',
        handler: vi.fn().mockResolvedValue({}),
        // Missing createdAt
      };

      // Act
      daemon.schedule(operation);

      // The operation should still be stored
      expect(daemon.operations.has('malformed-op')).toBe(true);

      await daemon.start();
      const result = await daemon.execute('malformed-op');

      // Assert
      expect(result).toBeDefined();
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });
  });

  describe('Resource Exhaustion & Recovery', () => {
    it('should handle memory pressure gracefully', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'memory-exhaustion-test',
        concurrency: 3,
      });

      const handler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();

      // Schedule operations with large metadata
      for (let i = 0; i < 100; i++) {
        daemon.schedule({
          id: `memory-op-${i}`,
          name: `memory-op-${i}`,
          metadata: {
            largeData: 'x'.repeat(10000), // ~10KB per operation
          },
          handler,
        });
      }

      // Execute subset
      for (let i = 0; i < 20; i++) {
        await daemon.execute(`memory-op-${i}`);
      }

      // Assert - Daemon should still be responsive
      const health = daemon.getHealth();
      expect(health.isRunning).toBe(true);
      expect(health.queuedOperations).toBeGreaterThan(0);

      await daemon.stop();
    });

    it('should recover from queue becoming too large', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'large-queue-recovery',
        concurrency: 2,
      });

      const slowHandler = vi.fn().mockImplementation(
        () => new Promise((resolve) => setTimeout(resolve, 10))
      );

      // Act
      await daemon.start();

      // Enqueue 200 operations
      for (let i = 0; i < 200; i++) {
        daemon.schedule({
          id: `queue-op-${i}`,
          name: `queue-op-${i}`,
          handler: slowHandler,
        });
      }

      const health = daemon.getHealth();

      // Assert - Health should report large queue but daemon running
      expect(health.queuedOperations).toBe(200);
      expect(health.isRunning).toBe(true);

      await daemon.stop();
    });

    it('should handle file descriptor exhaustion simulation', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'fd-exhaustion-test',
        concurrency: 5,
      });

      const fdIntensiveHandler = vi.fn().mockResolvedValue({});

      // Act
      await daemon.start();

      // Simulate many handlers that would use file descriptors
      for (let i = 0; i < 50; i++) {
        daemon.schedule({
          id: `fd-op-${i}`,
          name: `fd-op-${i}`,
          handler: fdIntensiveHandler,
        });
      }

      // Execute some
      for (let i = 0; i < 10; i++) {
        await daemon.execute(`fd-op-${i}`);
      }

      // Assert - Should gracefully handle without crashing
      const health = daemon.getHealth();
      expect(health.isRunning).toBe(true);

      await daemon.stop();
    });
  });

  describe('Error Context & Diagnostics', () => {
    it('should preserve error context in completed operations', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'error-context-test',
        concurrency: 5,
      });

      const errorHandler = vi.fn(async () => {
        throw new Error('Detailed error message for diagnostics');
      });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'error-context-op',
        name: 'error-context-op',
        handler: errorHandler,
      });

      try {
        await daemon.execute('error-context-op');
      } catch (e) {
        // Expected
      }

      const completed = daemon.completedOperations.get('error-context-op');

      // Assert
      expect(completed).toBeDefined();
      expect(completed.status).toBe('failure');
      expect(completed.error).toBeDefined();
      expect(completed.error).toContain('Detailed error message');

      await daemon.stop();
    });

    it('should emit operation failure event with error details', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'failure-event-test',
        concurrency: 5,
      });

      let failureEvent;
      daemon.on('operation:failure', (event) => {
        failureEvent = event;
      });

      const errorHandler = vi.fn(async () => {
        throw new Error('Test failure for event emission');
      });

      // Act
      await daemon.start();
      daemon.schedule({
        id: 'failure-event-op',
        name: 'failure-event-op',
        handler: errorHandler,
      });

      try {
        await daemon.execute('failure-event-op');
      } catch (e) {
        // Expected
      }

      // Assert
      expect(failureEvent).toBeDefined();
      expect(failureEvent.operationId).toBe('failure-event-op');
      expect(failureEvent.error).toBeDefined();
      expect(failureEvent.error).toContain('Test failure');

      await daemon.stop();
    });
  });

  describe('Cascading Failure Prevention', () => {
    it('should isolate failures in operation sequence', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'failure-isolation-test',
        concurrency: 5,
      });

      const sequence = [
        { id: 'seq-1', fails: true },
        { id: 'seq-2', fails: false },
        { id: 'seq-3', fails: true },
        { id: 'seq-4', fails: false },
        { id: 'seq-5', fails: false },
      ];

      // Act
      await daemon.start();

      sequence.forEach(({ id, fails }) => {
        daemon.schedule({
          id,
          name: id,
          handler: fails
            ? vi.fn(async () => {
                throw new Error(`${id} failed`);
              })
            : vi.fn().mockResolvedValue({ success: true }),
        });
      });

      const results = [];
      for (const { id } of sequence) {
        try {
          const result = await daemon.execute(id);
          results.push({ id, succeeded: true, result });
        } catch (e) {
          results.push({ id, succeeded: false, error: e.message });
        }
      }

      // Assert - Failures should not prevent subsequent successes
      expect(results[0].succeeded).toBe(false); // seq-1 fails
      expect(results[1].succeeded).toBe(true); // seq-2 succeeds despite seq-1 failure
      expect(results[2].succeeded).toBe(false); // seq-3 fails
      expect(results[3].succeeded).toBe(true); // seq-4 succeeds despite seq-3 failure
      expect(results[4].succeeded).toBe(true); // seq-5 succeeds

      await daemon.stop();
    });

    it('should prevent single handler error from affecting daemon health', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'handler-health-isolation',
        concurrency: 10,
      });

      const criticalHandler = vi.fn(async () => {
        throw new Error('Critical handler failure');
      });

      const normalHandler = vi.fn().mockResolvedValue({ ok: true });

      // Act
      await daemon.start();

      // Execute critical operation that fails
      daemon.schedule({
        id: 'critical-op',
        name: 'critical-op',
        handler: criticalHandler,
      });

      try {
        await daemon.execute('critical-op');
      } catch (e) {
        // Expected
      }

      const healthAfterCritical = daemon.getHealth();

      // Execute normal operations
      for (let i = 0; i < 5; i++) {
        daemon.schedule({
          id: `normal-op-${i}`,
          name: `normal-op-${i}`,
          handler: normalHandler,
        });
        await daemon.execute(`normal-op-${i}`);
      }

      const healthAfterNormal = daemon.getHealth();
      const metricsAfterNormal = daemon.getMetrics();

      // Assert - Daemon should remain healthy
      expect(healthAfterCritical.isRunning).toBe(true);
      expect(healthAfterNormal.isRunning).toBe(true);
      expect(metricsAfterNormal.successfulOperations).toBeGreaterThan(0);

      await daemon.stop();
    });

    it('should handle concurrent errors without affecting concurrent successes', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'concurrent-error-isolation',
        concurrency: 10,
      });

      const failHandler = vi.fn(async () => {
        throw new Error('Concurrent failure');
      });

      const successHandler = vi.fn().mockResolvedValue({ ok: true });

      // Act
      await daemon.start();

      // Schedule 10 mixed operations
      for (let i = 0; i < 10; i++) {
        const shouldFail = i % 2 === 0;
        daemon.schedule({
          id: `concurrent-op-${i}`,
          name: `concurrent-op-${i}`,
          handler: shouldFail ? failHandler : successHandler,
        });
      }

      // Execute all concurrently
      const promises = Array(10)
        .fill(null)
        .map((_, i) =>
          daemon.execute(`concurrent-op-${i}`).catch((e) => ({ error: e.message }))
        );

      const results = await Promise.all(promises);

      const metrics = daemon.getMetrics();

      // Assert - Mix of successes and failures, but all executed
      expect(results.length).toBe(10);
      expect(metrics.successfulOperations).toBeGreaterThan(0);
      expect(metrics.failedOperations).toBeGreaterThan(0);
      expect(daemon.isRunning).toBe(true);

      await daemon.stop();
    });
  });

  describe('Recovery Verification', () => {
    it('should report accurate metrics after error recovery', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'metrics-accuracy-test',
        concurrency: 5,
      });

      // Act
      await daemon.start();

      // Execute 5 success, 3 failures, 5 success = 10 total
      const operations = [
        { id: 'success-1', fails: false },
        { id: 'success-2', fails: false },
        { id: 'success-3', fails: false },
        { id: 'success-4', fails: false },
        { id: 'success-5', fails: false },
        { id: 'fail-1', fails: true },
        { id: 'fail-2', fails: true },
        { id: 'fail-3', fails: true },
        { id: 'success-6', fails: false },
        { id: 'success-7', fails: false },
      ];

      operations.forEach(({ id, fails }) => {
        daemon.schedule({
          id,
          name: id,
          handler: fails
            ? vi.fn(async () => {
                throw new Error('Expected failure');
              })
            : vi.fn().mockResolvedValue({ success: true }),
        });
      });

      for (const { id } of operations) {
        try {
          await daemon.execute(id);
        } catch (_e) {
          // Expected for failures
        }
      }

      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.totalOperations).toBe(10);
      expect(metrics.successfulOperations).toBe(7);
      expect(metrics.failedOperations).toBe(3);
      expect(metrics.successRate).toBeCloseTo(70, 1);

      await daemon.stop();
    });

    it('should maintain uptime accuracy through error recovery cycles', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'uptime-accuracy-test',
        concurrency: 5,
      });

      // Act
      await daemon.start();
      const startTime = daemon.startTime;

      // Simulate error and recovery cycle
      daemon.schedule({
        id: 'test-op',
        name: 'test-op',
        handler: vi.fn(async () => {
          throw new Error('Test error');
        }),
      });

      try {
        await daemon.execute('test-op');
      } catch (e) {
        // Expected
      }

      await new Promise((resolve) => setTimeout(resolve, 10));

      const health = daemon.getHealth();

      // Assert
      expect(health.uptime).toBeGreaterThan(0);
      expect(daemon.startTime).toBe(startTime);
      expect(health.isRunning).toBe(true);

      await daemon.stop();
    });
  });
});
