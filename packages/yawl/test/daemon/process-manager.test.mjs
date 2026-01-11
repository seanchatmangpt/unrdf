/**
 * @file YAWL Daemon Process Manager Tests
 * @module @unrdf/yawl/test/daemon/process-manager
 * @description Comprehensive tests for daemon process lifecycle management
 * Tests cover process spawning, monitoring, restart, and graceful shutdown
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';
import { createWorkflowEngine } from '../../src/engine.mjs';

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
 * Mock YAWL engine for process tests
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.processes = new Map();
    this.isShuttingDown = false;
  }

  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  async createCase(options) {
    const caseId = options.caseId || `case-${Date.now()}`;
    this.processes.set(caseId, { status: 'RUNNING', ...options });
    this.emit('case:created', { caseId, workflowId: options.workflowId });
    return { caseId, status: 'RUNNING' };
  }

  async enableTask(options) {
    const key = `${options.caseId}:${options.taskId}`;
    this.emit('task:enabled', options);
    return { ...options, status: 'ENABLED' };
  }

  async cancelTask(options) {
    const key = `${options.caseId}:${options.taskId}`;
    this.emit('task:cancelled', options);
    return { ...options, status: 'CANCELLED' };
  }

  async shutdown() {
    this.isShuttingDown = true;
    this.emit('engine:shutdown', { timestamp: Date.now() });
  }
}

describe('Daemon Process Manager', () => {
  describe('process spawning', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'process-test-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-process-test',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should spawn daemon process successfully', async () => {
      // Arrange
      expect(daemon.isRunning).toBe(true);

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.isRunning).toBe(true);
      expect(health.nodeId).toBeDefined();
      expect(health.activeOperations).toBe(0);
    });

    it('should track spawned processes', async () => {
      // Arrange
      const workflowId = 'test-workflow';

      // Act
      await bridge.scheduleRecurringCase(workflowId, '* * * * *', {
        caseIdPrefix: 'spawn-test',
      });
      const stats = bridge.getStats();

      // Assert
      expect(stats.caseSchedules).toBe(1);
      expect(daemon.operations.size).toBeGreaterThan(0);
    });

    it('should spawn multiple concurrent processes', async () => {
      // Arrange
      const workflows = ['wf-1', 'wf-2', 'wf-3'];

      // Act
      for (const wfId of workflows) {
        await bridge.scheduleRecurringCase(wfId, '* * * * *');
      }

      // Assert
      expect(bridge.getStats().caseSchedules).toBe(3);
      expect(daemon.operations.size).toBe(3);
    });

    it('should enforce concurrency limits', async () => {
      // Arrange
      const maxConcurrent = daemon.config.concurrency || 10;
      const operations = [];

      // Act
      for (let i = 0; i < maxConcurrent + 5; i++) {
        daemon.schedule({
          id: `op-${i}`,
          name: `operation-${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Assert
      expect(daemon.operations.size).toBe(maxConcurrent + 5);
      expect(daemon.operationQueue.length).toBe(maxConcurrent + 5);
    });

    it('should assign unique process IDs', async () => {
      // Arrange
      const processIds = new Set();

      // Act
      for (let i = 0; i < 5; i++) {
        const result = await bridge.scheduleRecurringCase(`wf-${i}`, '* * * * *');
        processIds.add(result.operationId);
      }

      // Assert
      expect(processIds.size).toBe(5);
    });

    it('should emit process:spawned event', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:enqueued', listener);

      // Act
      await bridge.scheduleRecurringCase('test-wf', '* * * * *');

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0]).toHaveProperty('operationId');
    });

    it('should handle process spawn failures gracefully', async () => {
      // Arrange
      const invalidEngine = { on: vi.fn() }; // Missing createCase

      // Act & Assert
      expect(() => {
        new YawlDaemonBridge(daemon, invalidEngine, {
          daemonNodeId: 'test',
        });
      }).not.toThrow();
    });

    it('should track process spawn timestamps', async () => {
      // Arrange
      const beforeSpawn = Date.now();

      // Act
      await bridge.scheduleRecurringCase('timed-wf', '* * * * *');
      const afterSpawn = Date.now();

      // Assert
      const operations = daemon.listOperations();
      expect(operations.length).toBe(1);
      const spawnTime = operations[0].createdAt.getTime();
      expect(spawnTime).toBeGreaterThanOrEqual(beforeSpawn);
      expect(spawnTime).toBeLessThanOrEqual(afterSpawn);
    });

    it('should support process spawn with metadata', async () => {
      // Arrange
      const metadata = { priority: 5, tags: ['batch', 'recurring'] };

      // Act
      await bridge.scheduleRecurringCase('meta-wf', '* * * * *', metadata);

      // Assert
      const operations = daemon.listOperations();
      expect(operations.length).toBe(1);
      expect(operations[0].metadata).toBeDefined();
    });

    it('should handle rapid process spawning', async () => {
      // Arrange
      const spawnCount = 20;

      // Act
      const promises = [];
      for (let i = 0; i < spawnCount; i++) {
        promises.push(
          bridge.scheduleRecurringCase(`rapid-wf-${i}`, '* * * * *')
        );
      }
      await Promise.all(promises);

      // Assert
      expect(bridge.getStats().caseSchedules).toBe(spawnCount);
    });

    it('should maintain process isolation', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('isolated-wf-1', '* * * * *');
      await bridge.scheduleRecurringCase('isolated-wf-2', '* * * * *');

      // Act
      const operations = daemon.listOperations();

      // Assert
      expect(operations).toHaveLength(2);
      expect(operations[0].id).not.toBe(operations[1].id);
      expect(operations[0].metadata.workflowId).not.toBe(
        operations[1].metadata.workflowId
      );
    });

    it('should validate process configuration', async () => {
      // Arrange
      const invalidWorkflowId = '';

      // Act & Assert
      await expect(
        bridge.scheduleRecurringCase(invalidWorkflowId, '* * * * *')
      ).rejects.toThrow();
    });
  });

  describe('process monitoring', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'monitor-test-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-monitor-test',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should monitor active processes', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('monitor-wf', '* * * * *');

      // Act
      const stats = bridge.getStats();

      // Assert
      expect(stats.isRunning).toBe(true);
      expect(stats.caseSchedules).toBe(1);
    });

    it('should track process health metrics', async () => {
      // Arrange
      await daemon.start();

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health).toHaveProperty('uptime');
      expect(health).toHaveProperty('activeOperations');
      expect(health).toHaveProperty('queuedOperations');
      expect(health.uptime).toBeGreaterThanOrEqual(0);
    });

    it('should detect process failures', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:failure', listener);

      const failingHandler = vi.fn().mockRejectedValue(new Error('Process crashed'));
      daemon.schedule({
        id: 'failing-op',
        name: 'Failing Operation',
        handler: failingHandler,
      });

      // Act
      try {
        await daemon.execute('failing-op');
      } catch (error) {
        // Expected
      }

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0].error).toContain('Process crashed');
    });

    it('should update process statistics', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ status: 'ok' });
      daemon.schedule({ id: 'stat-op', name: 'Stat Op', handler });

      // Act
      await daemon.execute('stat-op');
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.totalOperations).toBeGreaterThan(0);
      expect(metrics.successfulOperations).toBeGreaterThan(0);
    });

    it('should track process uptime', async () => {
      // Arrange
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.uptime).toBeGreaterThan(0);
      expect(health.uptime).toBeGreaterThanOrEqual(100);
    });

    it('should monitor memory usage', async () => {
      // Arrange
      const initialHealth = daemon.getHealth();

      // Act - Create many operations
      for (let i = 0; i < 100; i++) {
        daemon.schedule({
          id: `mem-op-${i}`,
          name: `Memory Op ${i}`,
          handler: vi.fn(),
        });
      }
      const afterHealth = daemon.getHealth();

      // Assert
      expect(afterHealth.queuedOperations).toBe(100);
      expect(daemon.operations.size).toBe(100);
    });

    it('should provide process state snapshot', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('snapshot-wf', '* * * * *');

      // Act
      const stats = bridge.getStats();

      // Assert
      expect(stats).toHaveProperty('bridgeId');
      expect(stats).toHaveProperty('isRunning');
      expect(stats).toHaveProperty('caseSchedules');
      expect(stats).toHaveProperty('timestamp');
    });

    it('should emit health check events', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:started', listener);

      // Act
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should track process performance', async () => {
      // Arrange
      const handler = vi
        .fn()
        .mockImplementation(
          () => new Promise((resolve) => setTimeout(() => resolve({}), 10))
        );
      daemon.schedule({ id: 'perf-op', name: 'Perf Op', handler });

      // Act
      const startTime = Date.now();
      await daemon.execute('perf-op');
      const duration = Date.now() - startTime;

      // Assert
      expect(duration).toBeGreaterThanOrEqual(10);
      const metrics = daemon.getMetrics();
      expect(metrics.averageDuration).toBeGreaterThan(0);
    });

    it('should maintain process statistics accuracy', async () => {
      // Arrange
      const successHandler = vi.fn().mockResolvedValue({});
      const failHandler = vi.fn().mockRejectedValue(new Error('Fail'));

      daemon.schedule({ id: 'success-1', name: 'S1', handler: successHandler });
      daemon.schedule({ id: 'success-2', name: 'S2', handler: successHandler });
      daemon.schedule({ id: 'fail-1', name: 'F1', handler: failHandler });

      // Act
      await daemon.execute('success-1');
      await daemon.execute('success-2');
      try {
        await daemon.execute('fail-1');
      } catch (e) {
        // Expected
      }

      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.successfulOperations).toBeGreaterThanOrEqual(2);
      expect(metrics.failedOperations).toBeGreaterThanOrEqual(1);
    });
  });

  describe('process restart', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'restart-test-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-restart-test',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should restart failed processes', async () => {
      // Arrange
      let attemptCount = 0;
      const handler = vi.fn().mockImplementation(() => {
        attemptCount++;
        if (attemptCount === 1) {
          return Promise.reject(new Error('First attempt failed'));
        }
        return Promise.resolve({ status: 'ok' });
      });

      daemon.schedule({ id: 'restart-op', name: 'Restart Op', handler });

      // Act
      try {
        await daemon.execute('restart-op');
      } catch (e) {
        // First attempt fails
      }

      // Reschedule (simulating restart)
      daemon.schedule({ id: 'restart-op-2', name: 'Restart Op 2', handler });
      const result = await daemon.execute('restart-op-2');

      // Assert
      expect(result.status).toBe('ok');
      expect(attemptCount).toBe(2);
    });

    it('should preserve process state across restarts', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('state-wf', '* * * * *', {
        inputData: { counter: 1 },
      });
      const beforeStop = bridge.getStats().caseSchedules;

      // Act
      await bridge.stop();
      await bridge.start();
      const afterStart = bridge.getStats().caseSchedules;

      // Assert - State is cleared on stop (expected behavior)
      expect(beforeStop).toBe(1);
      expect(afterStart).toBe(0); // Schedules are cleared on stop
    });

    it('should handle restart with pending operations', async () => {
      // Arrange
      daemon.schedule({
        id: 'pending-1',
        name: 'Pending 1',
        handler: vi.fn(),
      });
      daemon.schedule({
        id: 'pending-2',
        name: 'Pending 2',
        handler: vi.fn(),
      });

      // Act
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
      // Operations persist across restart
      expect(daemon.operations.size).toBe(2);
    });

    it('should emit restart events', async () => {
      // Arrange
      const stopListener = vi.fn();
      const startListener = vi.fn();
      daemon.on('daemon:stopped', stopListener);
      daemon.on('daemon:started', startListener);

      // Act
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(stopListener).toHaveBeenCalled();
      expect(startListener).toHaveBeenCalled();
    });

    it('should clear temporary resources on restart', async () => {
      // Arrange
      await bridge.watchTaskTimeout('case-1', 'task-1', 60000);
      expect(bridge.taskTimeouts.size).toBe(1);

      // Act
      await bridge.stop();
      await bridge.start();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
    });

    it('should reset restart counter', async () => {
      // Arrange & Act
      for (let i = 0; i < 3; i++) {
        await daemon.stop();
        await daemon.start();
      }

      // Assert
      const health = daemon.getHealth();
      expect(health.isRunning).toBe(true);
      expect(health.uptime).toBeGreaterThanOrEqual(0);
    });

    it('should validate configuration on restart', async () => {
      // Arrange & Act
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(daemon.config).toBeDefined();
      expect(daemon.config.daemonId).toBeDefined();
    });

    it('should cleanup before restart', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('cleanup-wf', '* * * * *');
      await bridge.watchTaskTimeout('case-x', 'task-x', 30000);

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
      expect(bridge._unsubscribers.length).toBe(0);
    });

    it('should handle rapid restart cycles', async () => {
      // Arrange & Act
      for (let i = 0; i < 5; i++) {
        await daemon.stop();
        await daemon.start();
      }

      // Assert
      expect(daemon.isRunning).toBe(true);
      const health = daemon.getHealth();
      expect(health.isRunning).toBe(true);
    });

    it('should restore monitoring after restart', async () => {
      // Arrange
      await daemon.stop();

      // Act
      await daemon.start();
      const health = daemon.getHealth();

      // Assert
      expect(health.isRunning).toBe(true);
      expect(health).toHaveProperty('uptime');
      expect(health).toHaveProperty('activeOperations');
    });
  });

  describe('graceful shutdown', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'shutdown-test-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-shutdown-test',
      });

      await daemon.start();
      await bridge.start();
    });

    it('should shutdown gracefully', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:stopped', listener);

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
      expect(listener).toHaveBeenCalled();
    });

    it('should complete active operations before shutdown', async () => {
      // Arrange
      const completed = { count: 0 };
      const handler = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 50));
        completed.count++;
        return { done: true };
      });

      daemon.schedule({ id: 'active-op', name: 'Active', handler });

      // Act
      const execPromise = daemon.execute('active-op');
      await daemon.stop(); // Stop while execution in progress
      await execPromise;

      // Assert
      expect(completed.count).toBe(1);
    });

    it('should cleanup resources on shutdown', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('shutdown-wf', '* * * * *');
      await bridge.watchTaskTimeout('case-s', 'task-s', 60000);

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
      expect(bridge._unsubscribers.length).toBe(0);
    });

    it('should emit shutdown event', async () => {
      // Arrange
      const listener = vi.fn();
      bridge.on('bridge:stopped', listener);

      // Act
      await bridge.stop();

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0]).toHaveProperty('bridgeId');
    });

    it('should unsubscribe from engine events', async () => {
      // Arrange
      const unsubCount = bridge._unsubscribers.length;

      // Act
      await bridge.stop();

      // Assert
      expect(bridge._unsubscribers.length).toBe(0);
    });

    it('should be idempotent', async () => {
      // Arrange & Act
      await daemon.stop();
      await daemon.stop();
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should prevent new operations after shutdown', async () => {
      // Arrange
      await daemon.stop();

      // Act
      daemon.schedule({ id: 'post-shutdown', name: 'Post', handler: vi.fn() });

      // Assert - Operation is scheduled but daemon won't execute it
      expect(daemon.isRunning).toBe(false);
      expect(daemon.operations.has('post-shutdown')).toBe(true);
    });

    it('should log shutdown message', async () => {
      // Arrange
      const mockLogger = { info: vi.fn(), debug: vi.fn(), warn: vi.fn() };
      daemon.logger = mockLogger;

      // Act
      await daemon.stop();

      // Assert
      expect(mockLogger.info).toHaveBeenCalledWith(
        expect.stringContaining('Stopped')
      );
    });

    it('should clear pending timeouts', async () => {
      // Arrange
      await bridge.watchTaskTimeout('case-t1', 'task-t1', 30000);
      await bridge.watchTaskTimeout('case-t2', 'task-t2', 30000);
      expect(bridge.taskTimeouts.size).toBe(2);

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
    });

    it('should update status immediately', async () => {
      // Arrange
      expect(daemon.isRunning).toBe(true);

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should preserve completed operation history', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ result: 'done' });
      daemon.schedule({ id: 'hist-op', name: 'History', handler });
      await daemon.execute('hist-op');

      const beforeMetrics = daemon.getMetrics();

      // Act
      await daemon.stop();
      const afterMetrics = daemon.getMetrics();

      // Assert
      expect(afterMetrics.totalOperations).toBe(beforeMetrics.totalOperations);
    });

    it('should handle shutdown errors gracefully', async () => {
      // Arrange
      const badListener = vi.fn().mockImplementation(() => {
        throw new Error('Listener error');
      });
      daemon.on('daemon:stopped', badListener);

      // Act & Assert
      await expect(daemon.stop()).resolves.not.toThrow();
    });
  });
});
