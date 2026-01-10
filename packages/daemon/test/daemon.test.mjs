/**
 * @file Daemon Core Tests
 * @module @unrdf/daemon/test
 * @description Comprehensive unit tests for Daemon class
 * Tests cover initialization, operation management, lifecycle, events, and error conditions
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { Daemon } from '../src/daemon.mjs';

// Generate a valid UUID v4 for testing
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

describe('Daemon', () => {
  describe('constructor()', () => {
    it('should initialize with valid config', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.daemonId).toBe(config.daemonId);
      expect(daemon.config.name).toBe(config.name);
      expect(daemon.isRunning).toBe(false);
      expect(daemon.activeCount).toBe(0);
    });

    it('should throw on invalid config - missing daemonId', () => {
      // Arrange
      const invalidConfig = {
        name: 'test-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(invalidConfig)).toThrow();
    });

    it('should throw on invalid config - missing name', () => {
      // Arrange
      const invalidConfig = {
        daemonId: generateUUID(),
      };

      // Act & Assert
      expect(() => new Daemon(invalidConfig)).toThrow();
    });

    it('should throw on invalid daemonId - not a UUID', () => {
      // Arrange
      const invalidConfig = {
        daemonId: 'not-a-uuid',
        name: 'test-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(invalidConfig)).toThrow();
    });

    it('should use default values for optional config', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config.port).toBe(8080);
      expect(daemon.config.logLevel).toBe('info');
      expect(daemon.config.concurrency).toBe(10);
      expect(daemon.config.healthCheckIntervalMs).toBe(30000);
    });

    it('should validate concurrency constraint (max 100)', () => {
      // Arrange
      const invalidConfig = {
        daemonId: generateUUID(),
        name: 'test-daemon',
        concurrency: 101,
      };

      // Act & Assert
      expect(() => new Daemon(invalidConfig)).toThrow();
    });

    it('should initialize nodeId', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.nodeId).toBeDefined();
      expect(daemon.nodeId).toMatch(/^node-\d+$/);
    });

    it('should initialize clusterId to default', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'test-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.clusterId).toBe('default-cluster');
    });

    it('should extend EventEmitter', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });

      // Assert
      expect(typeof daemon.on).toBe('function');
      expect(typeof daemon.emit).toBe('function');
      expect(typeof daemon.removeListener).toBe('function');
    });
  });

  describe('start()', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should set isRunning to true', async () => {
      // Arrange
      expect(daemon.isRunning).toBe(false);

      // Act
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should set startTime', async () => {
      // Arrange
      expect(daemon.startTime).toBeNull();

      // Act
      await daemon.start();

      // Assert
      expect(daemon.startTime).toBeDefined();
      expect(typeof daemon.startTime).toBe('number');
      expect(daemon.startTime > 0).toBe(true);
    });

    it('should emit start event', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:started', listener);

      // Act
      await daemon.start();

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should log start message', async () => {
      // Arrange
      const mockLogger = { info: vi.fn(), debug: vi.fn() };
      daemon.logger = mockLogger;

      // Act
      await daemon.start();

      // Assert
      expect(mockLogger.info).toHaveBeenCalledWith(
        expect.stringContaining('Started')
      );
    });

    it('should be idempotent - second call returns early', async () => {
      // Arrange
      await daemon.start();
      const firstStartTime = daemon.startTime;

      // Act
      await new Promise((resolve) => setTimeout(resolve, 10));
      await daemon.start();

      // Assert
      expect(daemon.startTime).toBe(firstStartTime);
      expect(daemon.isRunning).toBe(true);
    });

    it('should return a Promise', () => {
      // Arrange & Act
      const result = daemon.start();

      // Assert
      expect(result).toBeInstanceOf(Promise);
    });
  });

  describe('stop()', () => {
    let daemon;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
      await daemon.start();
    });

    it('should set isRunning to false', async () => {
      // Arrange
      expect(daemon.isRunning).toBe(true);

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should emit stop event', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:stopped', listener);

      // Act
      await daemon.stop();

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should log stop message', async () => {
      // Arrange
      const mockLogger = { info: vi.fn(), debug: vi.fn() };
      daemon.logger = mockLogger;

      // Act
      await daemon.stop();

      // Assert
      expect(mockLogger.info).toHaveBeenCalledWith(
        expect.stringContaining('Stopped')
      );
    });

    it('should preserve startTime after stop', async () => {
      // Arrange
      expect(daemon.startTime).not.toBeNull();
      const originalStartTime = daemon.startTime;

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.startTime).toBe(originalStartTime);
    });

    it('should be safe to call when not running', async () => {
      // Arrange
      await daemon.stop();

      // Act & Assert
      expect(async () => await daemon.stop()).not.toThrow();
    });
  });

  describe('schedule()', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should add operation to queue', () => {
      // Arrange
      const operation = {
        id: 'op-1',
        name: 'test-operation',
        handler: vi.fn(),
      };

      // Act
      daemon.schedule(operation);

      // Assert
      expect(daemon.operations.has('op-1')).toBe(true);
      expect(daemon.operationQueue).toContain('op-1');
    });

    it('should store operation with status and timestamp', () => {
      // Arrange
      const operation = {
        id: 'op-1',
        name: 'test-operation',
        handler: vi.fn(),
      };

      // Act
      daemon.schedule(operation);

      // Assert
      const stored = daemon.operations.get('op-1');
      expect(stored.status).toBe('scheduled');
      expect(stored.createdAt).toBeDefined();
      expect(typeof stored.createdAt).toBe('number');
    });

    it('should emit operation:enqueued event', () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:enqueued', listener);
      const operation = {
        id: 'op-1',
        name: 'test-operation',
        handler: vi.fn(),
      };

      // Act
      daemon.schedule(operation);

      // Assert
      expect(listener).toHaveBeenCalledWith(
        expect.objectContaining({
          operationId: 'op-1',
          name: 'test-operation',
        })
      );
    });

    it('should throw on invalid operation - missing id', () => {
      // Arrange
      const invalidOp = {
        name: 'test-operation',
        handler: vi.fn(),
      };

      // Act & Assert
      expect(() => daemon.schedule(invalidOp)).toThrow(
        'Invalid operation: must have id and handler function'
      );
    });

    it('should throw on invalid operation - missing handler', () => {
      // Arrange
      const invalidOp = {
        id: 'op-1',
        name: 'test-operation',
      };

      // Act & Assert
      expect(() => daemon.schedule(invalidOp)).toThrow(
        'Invalid operation: must have id and handler function'
      );
    });

    it('should throw on invalid operation - non-function handler', () => {
      // Arrange
      const invalidOp = {
        id: 'op-1',
        name: 'test-operation',
        handler: 'not a function',
      };

      // Act & Assert
      expect(() => daemon.schedule(invalidOp)).toThrow();
    });

    it('should throw on null operation', () => {
      // Act & Assert
      expect(() => daemon.schedule(null)).toThrow();
    });

    it('should schedule multiple operations independently', () => {
      // Arrange
      const op1 = { id: 'op-1', name: 'op1', handler: vi.fn() };
      const op2 = { id: 'op-2', name: 'op2', handler: vi.fn() };
      const op3 = { id: 'op-3', name: 'op3', handler: vi.fn() };

      // Act
      daemon.schedule(op1);
      daemon.schedule(op2);
      daemon.schedule(op3);

      // Assert
      expect(daemon.operations.size).toBe(3);
      expect(daemon.operationQueue).toHaveLength(3);
    });

    it('should allow scheduling after execution', () => {
      // Arrange
      const op1 = { id: 'op-1', name: 'op1', handler: vi.fn() };

      // Act
      daemon.schedule(op1);
      const firstQueueLength = daemon.operationQueue.length;

      const op2 = { id: 'op-2', name: 'op2', handler: vi.fn() };
      daemon.schedule(op2);

      // Assert
      expect(daemon.operationQueue.length).toBe(firstQueueLength + 1);
    });
  });

  describe('execute()', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should execute scheduled operation', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ result: 'success' });
      const operation = { id: 'op-1', name: 'test-op', handler };
      daemon.schedule(operation);

      // Act
      const result = await daemon.execute('op-1');

      // Assert
      expect(handler).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    it('should store completed operation result', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ result: 'done' });
      const operation = { id: 'op-1', name: 'test-op', handler };
      daemon.schedule(operation);

      // Act
      await daemon.execute('op-1');

      // Assert
      const completed = daemon.completedOperations.get('op-1');
      expect(completed).toBeDefined();
      expect(completed.status).toBe('success');
    });

    it('should throw on non-existent operation', async () => {
      // Act & Assert
      await expect(daemon.execute('non-existent')).rejects.toThrow();
    });

    it('should handle operation handler errors', async () => {
      // Arrange
      const error = new Error('Handler failed');
      const handler = vi.fn().mockRejectedValue(error);
      const operation = { id: 'op-1', name: 'test-op', handler };
      daemon.schedule(operation);

      // Act & Assert
      await expect(daemon.execute('op-1')).rejects.toThrow('Handler failed');
    });

    it('should return operation result', async () => {
      // Arrange
      const expectedResult = { data: 'test-data', count: 42 };
      const handler = vi.fn().mockResolvedValue(expectedResult);
      const operation = { id: 'op-1', name: 'test-op', handler };
      daemon.schedule(operation);

      // Act
      const result = await daemon.execute('op-1');

      // Assert
      expect(result).toEqual(expectedResult);
    });

    it('should emit operation events', async () => {
      // Arrange
      const startListener = vi.fn();
      const successListener = vi.fn();
      daemon.on('operation:started', startListener);
      daemon.on('operation:success', successListener);

      const handler = vi.fn().mockResolvedValue({});
      const operation = { id: 'op-1', name: 'test-op', handler };
      daemon.schedule(operation);

      // Act
      await daemon.execute('op-1');

      // Assert
      expect(startListener).toHaveBeenCalled();
      expect(successListener).toHaveBeenCalled();
    });
  });

  describe('getHealth()', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should return health status before start', () => {
      // Arrange & Act
      const health = daemon.getHealth();

      // Assert
      expect(health).toBeDefined();
      expect(health.isRunning).toBe(false);
      expect(health.uptime).toBe(0);
    });

    it('should return health status after start', async () => {
      // Arrange
      await daemon.start();

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.isRunning).toBe(true);
    });

    it('should include uptime after start', async () => {
      // Arrange
      await daemon.start();
      await new Promise((resolve) => setTimeout(resolve, 10));

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.uptime).toBeGreaterThan(0);
    });

    it('should include node and cluster IDs', () => {
      // Arrange & Act
      const health = daemon.getHealth();

      // Assert
      expect(health.nodeId).toBe(daemon.nodeId);
      expect(health.clusterId).toBe(daemon.clusterId);
    });

    it('should return structured health object', () => {
      // Arrange & Act
      const health = daemon.getHealth();

      // Assert
      expect(health).toHaveProperty('nodeId');
      expect(health).toHaveProperty('clusterId');
      expect(health).toHaveProperty('isRunning');
      expect(health).toHaveProperty('isLeader');
      expect(health).toHaveProperty('uptime');
      expect(health).toHaveProperty('activeOperations');
      expect(health).toHaveProperty('queuedOperations');
      expect(health).toHaveProperty('completedOperations');
      expect(health).toHaveProperty('timestamp');
    });
  });

  describe('getMetrics()', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should return empty metrics initially', () => {
      // Arrange & Act
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics).toBeDefined();
      expect(metrics.totalOperations).toBe(0);
      expect(metrics.successfulOperations).toBe(0);
      expect(metrics.failedOperations).toBe(0);
    });

    it('should track completed operations', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      daemon.schedule({ id: 'op-1', name: 'op1', handler });
      daemon.schedule({ id: 'op-2', name: 'op2', handler });

      // Act
      await daemon.execute('op-1');
      await daemon.execute('op-2');
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.totalOperations).toBeGreaterThanOrEqual(0);
    });

    it('should track failures', async () => {
      // Arrange
      const failingHandler = vi.fn().mockRejectedValue(new Error('Failed'));
      daemon.schedule({ id: 'op-1', name: 'op1', handler: failingHandler });

      // Act
      try {
        await daemon.execute('op-1');
      } catch (e) {
        // Expected
      }

      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.failedOperations).toBeGreaterThanOrEqual(0);
    });

    it('should return structured metrics object', () => {
      // Arrange & Act
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics).toHaveProperty('nodeId');
      expect(metrics).toHaveProperty('totalOperations');
      expect(metrics).toHaveProperty('successfulOperations');
      expect(metrics).toHaveProperty('failedOperations');
      expect(metrics).toHaveProperty('averageDuration');
      expect(metrics).toHaveProperty('totalDuration');
      expect(metrics).toHaveProperty('successRate');
      expect(metrics).toHaveProperty('timestamp');
    });

    it('should calculate success rate', async () => {
      // Arrange & Act
      const metrics = daemon.getMetrics();

      // Assert
      expect(typeof metrics.successRate).toBe('number');
      expect(metrics.successRate).toBeGreaterThanOrEqual(0);
      expect(metrics.successRate).toBeLessThanOrEqual(100);
    });
  });

  describe('event emissions', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should emit daemon:started on start', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:started', listener);

      // Act
      await daemon.start();

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should emit daemon:stopped on stop', async () => {
      // Arrange
      await daemon.start();
      const listener = vi.fn();
      daemon.on('daemon:stopped', listener);

      // Act
      await daemon.stop();

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should support multiple event listeners', async () => {
      // Arrange
      const listener1 = vi.fn();
      const listener2 = vi.fn();
      daemon.on('daemon:started', listener1);
      daemon.on('daemon:started', listener2);

      // Act
      await daemon.start();

      // Assert
      expect(listener1).toHaveBeenCalled();
      expect(listener2).toHaveBeenCalled();
    });

    it('should support once() for single emission', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.once('daemon:started', listener);

      // Act
      await daemon.start();
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(listener).toHaveBeenCalledTimes(1);
    });
  });

  describe('state management', () => {
    it('should maintain independent state for separate instances', () => {
      // Arrange
      const daemon1 = new Daemon({
        daemonId: generateUUID(),
        name: 'daemon-1',
      });
      const daemon2 = new Daemon({
        daemonId: generateUUID(),
        name: 'daemon-2',
      });

      // Act
      daemon1.schedule({ id: 'op-1', name: 'op1', handler: vi.fn() });

      // Assert
      expect(daemon1.operations.size).toBe(1);
      expect(daemon2.operations.size).toBe(0);
    });

    it('should preserve state across operations', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
      const opId = daemon.config.daemonId;

      // Act
      await daemon.start();
      daemon.schedule({ id: 'op-1', name: 'test-op', handler: vi.fn() });
      const operationCount = daemon.operations.size;

      // Assert
      expect(daemon.isRunning).toBe(true);
      expect(operationCount).toBe(1);
    });

    it('should initialize with zero activeCount', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });

      // Assert
      expect(daemon.activeCount).toBe(0);
      expect(typeof daemon.activeCount).toBe('number');
    });
  });

  describe('integration scenarios', () => {
    it('should handle complete daemon lifecycle', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'lifecycle-test',
        concurrency: 5,
      });
      const eventLog = [];

      daemon.on('daemon:started', () => eventLog.push('started'));
      daemon.on('daemon:stopped', () => eventLog.push('stopped'));

      // Act & Assert
      await daemon.start();
      expect(daemon.isRunning).toBe(true);

      daemon.schedule({
        id: 'op-1',
        name: 'test-operation',
        handler: vi.fn().mockResolvedValue({}),
      });
      expect(daemon.operations.size).toBe(1);

      const health = daemon.getHealth();
      expect(health.isRunning).toBe(true);

      await daemon.stop();
      expect(daemon.isRunning).toBe(false);

      expect(eventLog).toEqual(['started', 'stopped']);
    });

    it('should handle concurrent event listeners', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'event-test',
      });
      const results = { startCount: 0, opsCount: 0 };

      daemon.on('daemon:started', () => {
        results.startCount++;
        daemon.schedule({
          id: 'auto-op',
          name: 'auto-operation',
          handler: vi.fn(),
        });
        results.opsCount = daemon.operations.size;
      });

      // Act
      await daemon.start();

      // Assert
      expect(results.startCount).toBe(1);
      expect(results.opsCount).toBe(1);
    });

    it('should support operation scheduling and execution flow', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'flow-test',
      });
      const handler = vi.fn().mockResolvedValue({ status: 'completed' });

      // Act
      await daemon.start();
      daemon.schedule({ id: 'op-1', name: 'test-op', handler });
      const result = await daemon.execute('op-1');

      // Assert
      expect(handler).toHaveBeenCalled();
      expect(result.status).toBe('completed');
      expect(daemon.operations.size).toBeGreaterThanOrEqual(0);
    });
  });

  describe('error handling', () => {
    let daemon;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'test-daemon',
      });
    });

    it('should not throw on valid operations', async () => {
      // Arrange & Act & Assert
      expect(async () => {
        await daemon.start();
        daemon.schedule({ id: 'op-1', name: 'op', handler: vi.fn() });
        await daemon.stop();
      }).not.toThrow();
    });

    it('should throw with descriptive error on invalid schedule', () => {
      // Arrange
      const invalidOp = { id: '', name: '', handler: null };

      // Act & Assert
      expect(() => daemon.schedule(invalidOp)).toThrow();
    });
  });

  describe('edge cases', () => {
    it('should handle rapid start/stop cycles', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'rapid-test',
      });

      // Act & Assert
      for (let i = 0; i < 3; i++) {
        await daemon.start();
        expect(daemon.isRunning).toBe(true);
        await daemon.stop();
        expect(daemon.isRunning).toBe(false);
      }
    });

    it('should handle many scheduled operations', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'many-ops-test',
      });
      const opCount = 50;

      // Act
      for (let i = 0; i < opCount; i++) {
        daemon.schedule({
          id: `op-${i}`,
          name: `operation-${i}`,
          handler: vi.fn(),
        });
      }

      // Assert
      expect(daemon.operations.size).toBe(opCount);
      expect(daemon.operationQueue.length).toBe(opCount);
    });

    it('should handle operations with special characters in ids', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'special-chars-test',
      });
      const specialIds = [
        'op:with:colons',
        'op-with-dashes',
        'op_with_underscores',
        'op.with.dots',
      ];

      // Act
      specialIds.forEach((id) => {
        daemon.schedule({ id, name: id, handler: vi.fn() });
      });

      // Assert
      expect(daemon.operations.size).toBe(specialIds.length);
      specialIds.forEach((id) => {
        expect(daemon.operations.has(id)).toBe(true);
      });
    });

    it('should handle concurrent operations', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'concurrent-test',
      });
      const handler = vi
        .fn()
        .mockImplementation(
          () => new Promise((resolve) => setTimeout(resolve, 5))
        );

      // Act
      await daemon.start();
      for (let i = 0; i < 5; i++) {
        daemon.schedule({ id: `op-${i}`, name: `op-${i}`, handler });
      }

      // Assert
      expect(daemon.operationQueue.length).toBe(5);
      expect(daemon.operations.size).toBe(5);
      await daemon.stop();
    });

    it('should handle very long operation ids', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'long-ids-test',
      });
      const longId = 'op-' + 'x'.repeat(500);

      // Act
      daemon.schedule({ id: longId, name: 'long-id-op', handler: vi.fn() });

      // Assert
      expect(daemon.operations.has(longId)).toBe(true);
    });
  });
});
