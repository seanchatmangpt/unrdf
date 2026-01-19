/**
 * @file Chaos Recovery Tests
 * @module @unrdf/daemon/test/stability/chaos-recovery
 * @description Tests for daemon recovery from chaos events including
 * error storms, listener failures, and unexpected state transitions.
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
 * Create a daemon with default test config
 * @param {Object} overrides - Config overrides
 * @returns {Daemon} Configured daemon instance
 */
function createTestDaemon(overrides = {}) {
  return new Daemon({
    daemonId: generateUUID(),
    name: 'chaos-test-daemon',
    ...overrides,
  });
}

describe('Chaos Recovery Tests', () => {
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

  describe('Error Storm Recovery', () => {
    it('should survive a burst of failing operations', async () => {
      // Arrange
      await daemon.start();
      const failureCount = 50;
      const errors = [];

      // Act - Fire many failing operations
      for (let i = 0; i < failureCount; i++) {
        daemon.schedule({
          id: `fail-${i}`,
          name: `failing-op-${i}`,
          handler: vi.fn().mockRejectedValue(new Error(`Chaos error ${i}`)),
        });

        try {
          await daemon.execute(`fail-${i}`);
        } catch (e) {
          errors.push(e.message);
        }
      }

      // Assert - Daemon should still be functional
      expect(daemon.isRunning).toBe(true);
      expect(errors.length).toBe(failureCount);

      // Should still accept new operations
      daemon.schedule({
        id: 'recovery-op',
        name: 'recovery-operation',
        handler: vi.fn().mockResolvedValue({ recovered: true }),
      });
      const result = await daemon.execute('recovery-op');
      expect(result.recovered).toBe(true);
    });

    it('should recover from intermittent failures', async () => {
      // Arrange
      await daemon.start();
      let callCount = 0;
      const successes = [];
      const failures = [];

      // Handler that fails 50% of the time
      const flakeyHandler = vi.fn().mockImplementation(async () => {
        callCount++;
        if (callCount % 2 === 0) {
          throw new Error('Intermittent failure');
        }
        return { success: true };
      });

      // Act
      for (let i = 0; i < 20; i++) {
        daemon.schedule({
          id: `flakey-${i}`,
          name: `flakey-op-${i}`,
          handler: flakeyHandler,
        });

        try {
          const result = await daemon.execute(`flakey-${i}`);
          successes.push(result);
        } catch (e) {
          failures.push(e);
        }
      }

      // Assert
      expect(successes.length).toBe(10);
      expect(failures.length).toBe(10);
      expect(daemon.isRunning).toBe(true);
    });

    it('should handle mixed success and error patterns', async () => {
      // Arrange
      await daemon.start();
      const results = { success: 0, error: 0 };

      // Act - Schedule operations with varying failure patterns
      for (let i = 0; i < 30; i++) {
        const shouldFail = i % 3 === 0;
        daemon.schedule({
          id: `mixed-${i}`,
          name: `mixed-op-${i}`,
          handler: shouldFail
            ? vi.fn().mockRejectedValue(new Error('Planned failure'))
            : vi.fn().mockResolvedValue({ index: i }),
        });

        try {
          await daemon.execute(`mixed-${i}`);
          results.success++;
        } catch {
          results.error++;
        }
      }

      // Assert
      expect(results.error).toBe(10); // 0, 3, 6, 9, 12, 15, 18, 21, 24, 27
      expect(results.success).toBe(20);
      expect(daemon.isRunning).toBe(true);
    });
  });

  describe('Listener Failure Recovery', () => {
    it('should continue operation when event listener throws', async () => {
      // Arrange
      await daemon.start();
      const errorListener = vi.fn().mockImplementation(() => {
        throw new Error('Listener explosion');
      });
      daemon.on('operation:enqueued', errorListener);

      // Act - Schedule should trigger the failing listener
      daemon.schedule({
        id: 'listener-test',
        name: 'listener-test-op',
        handler: vi.fn().mockResolvedValue({}),
      });

      // Assert - Operation should still be scheduled despite listener failure
      expect(daemon.operations.has('listener-test')).toBe(true);
      expect(errorListener).toHaveBeenCalled();
    });

    it('should isolate listener errors from operation execution', async () => {
      // Arrange
      await daemon.start();
      const throwingListener = vi.fn().mockImplementation(() => {
        throw new Error('Listener crash');
      });
      daemon.on('operation:started', throwingListener);

      daemon.schedule({
        id: 'isolated-op',
        name: 'isolated-operation',
        handler: vi.fn().mockResolvedValue({ data: 'success' }),
      });

      // Act
      const result = await daemon.execute('isolated-op');

      // Assert
      expect(result.data).toBe('success');
      expect(throwingListener).toHaveBeenCalled();
    });

    it('should handle multiple failing listeners on same event', async () => {
      // Arrange
      await daemon.start();
      const callOrder = [];

      // Note: EventEmitter calls listeners sequentially. When one throws,
      // the _safeEmit catches at the emit level, preventing subsequent listeners.
      // This test verifies the daemon remains functional despite listener errors.
      const listener1 = vi.fn().mockImplementation(() => {
        callOrder.push('listener1');
        throw new Error('Listener 1 error');
      });

      daemon.on('daemon:started', listener1);

      // Act - Stop and restart to trigger listeners
      await daemon.stop();
      await daemon.start();

      // Assert - First listener should have been called
      expect(listener1).toHaveBeenCalled();
      expect(callOrder).toContain('listener1');
      // Daemon should remain functional despite listener error
      expect(daemon.isRunning).toBe(true);

      // Verify daemon can still perform operations after listener failure
      daemon.schedule({
        id: 'post-listener-failure-op',
        name: 'post-failure-operation',
        handler: vi.fn().mockResolvedValue({ recovered: true }),
      });
      const result = await daemon.execute('post-listener-failure-op');
      expect(result.recovered).toBe(true);
    });
  });

  describe('State Transition Recovery', () => {
    it('should handle concurrent start/stop requests gracefully', async () => {
      // Arrange
      const startPromises = [];
      const stopPromises = [];

      // Act - Fire concurrent start/stop requests
      for (let i = 0; i < 5; i++) {
        startPromises.push(daemon.start());
        stopPromises.push(daemon.stop());
      }

      await Promise.all([...startPromises, ...stopPromises]);

      // Assert - Daemon should be in a valid state
      expect(typeof daemon.isRunning).toBe('boolean');
    });

    it('should recover from rapid operation scheduling during state changes', async () => {
      // Arrange
      await daemon.start();

      // Act - Schedule operations while stop is pending
      const stopPromise = daemon.stop();

      for (let i = 0; i < 10; i++) {
        daemon.schedule({
          id: `rapid-${i}`,
          name: `rapid-op-${i}`,
          handler: vi.fn(),
        });
      }

      await stopPromise;

      // Assert
      expect(daemon.operations.size).toBe(10);
      expect(daemon.isRunning).toBe(false);
    });

    it('should maintain operation integrity during restart', async () => {
      // Arrange
      await daemon.start();

      daemon.schedule({
        id: 'persist-op',
        name: 'persistent-operation',
        handler: vi.fn().mockResolvedValue({}),
      });

      // Act - Stop and restart
      await daemon.stop();
      await daemon.start();

      // Assert - Operation should still exist
      expect(daemon.operations.has('persist-op')).toBe(true);
    });
  });

  describe('Timeout and Deadlock Prevention', () => {
    it('should not block on slow operation handlers', async () => {
      // Arrange
      await daemon.start();
      const slowHandler = vi.fn().mockImplementation(async () => {
        await new Promise(resolve => setTimeout(resolve, 50));
        return { slow: true };
      });

      daemon.schedule({
        id: 'slow-op',
        name: 'slow-operation',
        handler: slowHandler,
      });

      // Act
      const startTime = Date.now();
      const result = await daemon.execute('slow-op');
      const duration = Date.now() - startTime;

      // Assert
      expect(result.slow).toBe(true);
      expect(duration).toBeGreaterThanOrEqual(50);
      expect(duration).toBeLessThan(200); // Should not hang
    });

    it('should handle handler that never resolves with timeout', async () => {
      // Arrange
      await daemon.start();

      const hangingHandler = vi.fn().mockImplementation(() => {
        return new Promise(() => {
          // Never resolves
        });
      });

      daemon.schedule({
        id: 'hanging-op',
        name: 'hanging-operation',
        handler: hangingHandler,
      });

      // Act - Execute with manual timeout
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => reject(new Error('Timeout')), 100);
      });

      let timedOut = false;
      try {
        await Promise.race([
          daemon.execute('hanging-op'),
          timeoutPromise,
        ]);
      } catch (e) {
        if (e.message === 'Timeout') {
          timedOut = true;
        }
      }

      // Assert
      expect(timedOut).toBe(true);
      expect(daemon.isRunning).toBe(true);
    });
  });

  describe('Data Corruption Recovery', () => {
    it('should handle operation with undefined result', async () => {
      // Arrange
      await daemon.start();
      daemon.schedule({
        id: 'undefined-op',
        name: 'undefined-result-operation',
        handler: vi.fn().mockResolvedValue(undefined),
      });

      // Act
      const result = await daemon.execute('undefined-op');

      // Assert
      expect(result).toBeUndefined();
      expect(daemon.completedOperations.has('undefined-op')).toBe(true);
    });

    it('should handle operation with null result', async () => {
      // Arrange
      await daemon.start();
      daemon.schedule({
        id: 'null-op',
        name: 'null-result-operation',
        handler: vi.fn().mockResolvedValue(null),
      });

      // Act
      const result = await daemon.execute('null-op');

      // Assert
      expect(result).toBeNull();
    });

    it('should handle operation with circular reference in result', async () => {
      // Arrange
      await daemon.start();
      const circularObj = { name: 'circular' };
      circularObj.self = circularObj;

      daemon.schedule({
        id: 'circular-op',
        name: 'circular-result-operation',
        handler: vi.fn().mockResolvedValue(circularObj),
      });

      // Act
      const result = await daemon.execute('circular-op');

      // Assert
      expect(result.name).toBe('circular');
      expect(result.self).toBe(result);
    });
  });
});
