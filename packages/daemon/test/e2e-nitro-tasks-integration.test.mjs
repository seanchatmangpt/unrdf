/**
 * @file Nitro Tasks Integration E2E Tests
 * @module @unrdf/daemon/test
 * @description End-to-end tests for Nitro task executor integration
 * Tests cover registration, execution, scheduling, error handling, and metrics
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { Daemon } from '../src/daemon.mjs';
import { NitroTaskExecutor, createNitroTaskExecutor, integrateNitroTasks } from '../src/integrations/nitro-tasks.mjs';

// UUID generator helper
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

describe('NitroTaskExecutor Integration', () => {
  let daemon;
  let executor;

  beforeEach(async () => {
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon',
      logLevel: 'error',
    });

    executor = new NitroTaskExecutor(daemon, {
      autoStart: false,
      enableEventRelay: true,
      enableMetrics: true,
    });
  });

  afterEach(async () => {
    if (daemon.isRunning) {
      await daemon.stop();
    }
  });

  describe('constructor()', () => {
    it('should create executor with valid daemon', () => {
      // Assert
      expect(executor).toBeDefined();
      expect(executor.daemon).toBe(daemon);
      expect(executor.id).toBeDefined();
      expect(executor.config.timeout).toBe(30000);
      expect(executor.config.maxRetries).toBe(3);
    });

    it('should throw on invalid daemon', () => {
      // Act & Assert
      expect(() => new NitroTaskExecutor(null)).toThrow();
      expect(() => new NitroTaskExecutor({})).toThrow();
    });

    it('should use custom configuration', () => {
      // Arrange
      const customConfig = {
        timeout: 5000,
        maxRetries: 5,
        taskPrefix: 'custom:',
        enableMetrics: false,
      };

      // Act
      const customExecutor = new NitroTaskExecutor(daemon, customConfig);

      // Assert
      expect(customExecutor.config.timeout).toBe(5000);
      expect(customExecutor.config.maxRetries).toBe(5);
      expect(customExecutor.config.taskPrefix).toBe('custom:');
      expect(customExecutor.config.enableMetrics).toBe(false);
    });

    it('should auto-start by default', async () => {
      // Arrange
      const autoStartExecutor = new NitroTaskExecutor(daemon, { autoStart: true });

      // Assert
      expect(daemon.isRunning).toBe(true);

      // Cleanup
      await autoStartExecutor.stop();
    });

    it('should not auto-start when disabled', async () => {
      // Assert
      expect(daemon.isRunning).toBe(false);
    });
  });

  describe('start/stop lifecycle', () => {
    it('should start executor and daemon', async () => {
      // Act
      await executor.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should stop executor and daemon', async () => {
      // Arrange
      await executor.start();

      // Act
      await executor.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should emit executor:started event', async () => {
      // Arrange
      let startedEvent = null;
      executor.on('executor:started', (data) => {
        startedEvent = data;
      });

      // Act
      await executor.start();

      // Assert
      expect(startedEvent).toBeDefined();
      expect(startedEvent.executorId).toBe(executor.id);
    });

    it('should emit executor:stopped event', async () => {
      // Arrange
      await executor.start();
      let stoppedEvent = null;
      executor.on('executor:stopped', (data) => {
        stoppedEvent = data;
      });

      // Act
      await executor.stop();

      // Assert
      expect(stoppedEvent).toBeDefined();
      expect(stoppedEvent.executorId).toBe(executor.id);
    });
  });

  describe('operation registration as Nitro task', () => {
    beforeEach(async () => {
      await executor.start();
      daemon.schedule({
        id: generateUUID(),
        name: 'test-operation',
        handler: async () => ({ result: 'success' }),
        metadata: { category: 'test' },
      });
    });

    it('should register operation as Nitro task', () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;

      // Act
      const result = executor.registerOperationAsTask(opId, 'test-task', {
        description: 'A test task',
        priority: 'high',
      });

      // Assert
      expect(result.success).toBe(true);
      expect(result.operationId).toBe(opId);
      expect(result.nitroTaskId).toBeDefined();
      expect(result.nitroTaskId).toContain('daemon:');
      expect(result.taskMetadata.operationType).toBe('test-task');
      expect(result.taskMetadata.priority).toBe('high');
    });

    it('should throw on invalid operation', () => {
      // Act & Assert
      expect(() => executor.registerOperationAsTask('invalid-id', 'task')).toThrow();
    });

    it('should emit task:registered event', async () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      let registeredEvent = null;
      executor.on('task:registered', (data) => {
        registeredEvent = data;
      });

      // Act
      executor.registerOperationAsTask(opId, 'test-task');

      // Assert
      expect(registeredEvent).toBeDefined();
      expect(registeredEvent.daemonOperationId).toBe(opId);
      expect(registeredEvent.nitroTaskId).toBeDefined();
    });

    it('should map daemon operation to Nitro task', () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;

      // Act
      executor.registerOperationAsTask(opId, 'test-task');

      // Assert
      const nitroTaskId = executor.daemonToNitroMap.get(opId);
      expect(nitroTaskId).toBeDefined();
      expect(executor.nitroToDaemonMap.get(nitroTaskId)).toBe(opId);
    });

    it('should store task metadata', () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;

      // Act
      executor.registerOperationAsTask(opId, 'test-task', {
        description: 'Test operation',
        cronExpression: '0 0 * * *',
        tags: ['daily', 'critical'],
      });

      // Assert
      const metadata = executor.taskMetadata.get(opId);
      expect(metadata).toBeDefined();
      expect(metadata.description).toBe('Test operation');
      expect(metadata.cronExpression).toBe('0 0 * * *');
      expect(metadata.tags).toContain('daily');
      expect(metadata.tags).toContain('critical');
    });
  });

  describe('task execution via Nitro', () => {
    let opId;
    let nitroTaskId;
    let executionResult;

    beforeEach(async () => {
      await executor.start();
      daemon.schedule({
        id: generateUUID(),
        name: 'test-operation',
        handler: async () => ({ data: 'test-result', timestamp: Date.now() }),
      });

      opId = daemon.listOperations()[0].id;
      const result = executor.registerOperationAsTask(opId, 'test-task');
      nitroTaskId = result.nitroTaskId;
    });

    it('should execute task via runTask()', async () => {
      // Act
      executionResult = await executor.runTask(nitroTaskId);

      // Assert
      expect(executionResult.success).toBe(true);
      expect(executionResult.taskId).toBe(nitroTaskId);
      expect(executionResult.operationId).toBe(opId);
      expect(executionResult.result).toBeDefined();
    });

    it('should throw on missing task', async () => {
      // Act & Assert
      await expect(executor.runTask('non-existent')).rejects.toThrow();
    });

    it('should handle task timeout', async () => {
      // Arrange
      const shortTimeoutExecutor = new NitroTaskExecutor(daemon, {
        timeout: 10,
        autoStart: false,
      });
      await shortTimeoutExecutor.start();

      daemon.schedule({
        id: generateUUID(),
        name: 'slow-operation',
        handler: async () => {
          await new Promise((r) => setTimeout(r, 100));
          return { result: 'complete' };
        },
      });

      const slowOpId = daemon.listOperations().find((op) => op.name === 'slow-operation').id;
      const result = shortTimeoutExecutor.registerOperationAsTask(slowOpId, 'slow-task');

      // Act & Assert
      await expect(shortTimeoutExecutor.runTask(result.nitroTaskId)).rejects.toThrow('timeout');

      // Cleanup
      await shortTimeoutExecutor.stop();
    });

    it('should increment metrics on execution', async () => {
      // Arrange
      const initialMetrics = executor.metrics.tasksExecuted;

      // Act
      await executor.runTask(nitroTaskId);

      // Assert
      expect(executor.metrics.tasksExecuted).toBe(initialMetrics + 1);
    });

    it('should track successful execution in metrics', async () => {
      // Arrange
      const initialSucceeded = executor.metrics.tasksSucceeded;

      // Act
      await executor.runTask(nitroTaskId);
      // Note: daemon.execute() emits operation:success automatically

      // Assert
      expect(executor.metrics.tasksSucceeded).toBe(initialSucceeded + 1);
    });

    it('should store execution result', async () => {
      // Act
      await executor.runTask(nitroTaskId);

      // Assert
      const stored = executor.nitroTaskResults.get(nitroTaskId);
      expect(stored).toBeDefined();
      expect(stored.success).toBe(true);
      expect(stored.taskId).toBe(nitroTaskId);
    });

    it('should relay task:started event', async () => {
      // Arrange
      let taskStartedEvent = null;
      executor.on('task:started', (data) => {
        taskStartedEvent = data;
      });

      // Act
      daemon.emit('operation:started', { operationId: opId });

      // Assert
      expect(taskStartedEvent).toBeDefined();
      expect(taskStartedEvent.daemonOperationId).toBe(opId);
      expect(taskStartedEvent.nitroTaskId).toBe(nitroTaskId);
    });

    it('should relay task:succeeded event', async () => {
      // Arrange
      let taskSucceededEvent = null;
      executor.on('task:succeeded', (data) => {
        taskSucceededEvent = data;
      });

      // Act
      daemon.emit('operation:success', { operationId: opId, duration: 100 });

      // Assert
      expect(taskSucceededEvent).toBeDefined();
      expect(taskSucceededEvent.daemonOperationId).toBe(opId);
      expect(taskSucceededEvent.duration).toBe(100);
    });

    it('should relay task:failed event', async () => {
      // Arrange
      let taskFailedEvent = null;
      executor.on('task:failed', (data) => {
        taskFailedEvent = data;
      });

      // Act
      daemon.emit('operation:failure', {
        operationId: opId,
        error: 'Test error',
        duration: 50,
      });

      // Assert
      expect(taskFailedEvent).toBeDefined();
      expect(taskFailedEvent.daemonOperationId).toBe(opId);
      expect(taskFailedEvent.error).toBe('Test error');
    });
  });

  describe('task management', () => {
    beforeEach(async () => {
      await executor.start();

      for (let i = 0; i < 3; i++) {
        daemon.schedule({
          id: generateUUID(),
          name: `operation-${i}`,
          handler: async () => ({ index: i }),
        });
      }

      const ops = daemon.listOperations();
      for (let i = 0; i < ops.length; i++) {
        executor.registerOperationAsTask(ops[i].id, `task-${i}`);
      }
    });

    it('should list registered tasks', () => {
      // Act
      const tasks = executor.listTasks();

      // Assert
      expect(tasks.length).toBe(3);
      expect(tasks[0].daemonOperationId).toBeDefined();
      expect(tasks[0].nitroTaskId).toBeDefined();
      expect(tasks[0].operationType).toBeDefined();
    });

    it('should unregister task', () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      const initialCount = executor.listTasks().length;

      // Act
      const result = executor.unregisterTask(opId);

      // Assert
      expect(result).toBe(true);
      expect(executor.listTasks().length).toBe(initialCount - 1);
      expect(executor.daemonToNitroMap.has(opId)).toBe(false);
    });

    it('should return false on unregister of non-existent task', () => {
      // Act
      const result = executor.unregisterTask('non-existent');

      // Assert
      expect(result).toBe(false);
    });

    it('should emit task:unregistered event', () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      let unregEvent = null;
      executor.on('task:unregistered', (data) => {
        unregEvent = data;
      });

      // Act
      executor.unregisterTask(opId);

      // Assert
      expect(unregEvent).toBeDefined();
      expect(unregEvent.daemonOperationId).toBe(opId);
    });

    it('should discover operations as task candidates', () => {
      // Act
      const candidates = executor.discoverOperationsAsTaskCandidates();

      // Assert
      expect(candidates.length).toBe(3);
      expect(candidates[0].id).toBeDefined();
      expect(candidates[0].name).toBeDefined();
    });

    it('should filter discovered operations', () => {
      // Act
      const filtered = executor.discoverOperationsAsTaskCandidates((op) => op.name.includes('operation-1'));

      // Assert
      expect(filtered.length).toBe(1);
      expect(filtered[0].name).toBe('operation-1');
    });

    it('should get task execution history', async () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      const taskId = executor.daemonToNitroMap.get(opId);
      await executor.runTask(taskId);

      // Act
      const history = executor.getExecutionHistory();

      // Assert
      expect(history.length).toBeGreaterThan(0);
      expect(history[0].taskId).toBeDefined();
      expect(history[0].success).toBeDefined();
    });

    it('should get specific task history', async () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      const taskId = executor.daemonToNitroMap.get(opId);
      await executor.runTask(taskId);

      // Act
      const history = executor.getExecutionHistory(taskId);

      // Assert
      expect(history.length).toBe(1);
      expect(history[0].taskId).toBe(taskId);
    });
  });

  describe('metrics and monitoring', () => {
    beforeEach(async () => {
      await executor.start();
      daemon.schedule({
        id: generateUUID(),
        name: 'test-operation',
        handler: async () => ({ result: 'ok' }),
      });
      const opId = daemon.listOperations()[0].id;
      executor.registerOperationAsTask(opId, 'test-task');
    });

    it('should get executor metrics', () => {
      // Act
      const metrics = executor.getMetrics();

      // Assert
      expect(metrics.executorId).toBe(executor.id);
      expect(metrics.tasksExecuted).toBe(0);
      expect(metrics.tasksSucceeded).toBe(0);
      expect(metrics.tasksFailed).toBe(0);
      expect(metrics.registeredTasks).toBe(1);
    });

    it('should calculate average duration', async () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      const taskId = executor.daemonToNitroMap.get(opId);
      const initialSucceeded = executor.metrics.tasksSucceeded;

      // Act
      // runTask() will trigger daemon.execute() which emits operation:success automatically
      await executor.runTask(taskId);
      await executor.runTask(taskId);

      // Assert
      const metrics = executor.getMetrics();
      expect(metrics.tasksSucceeded).toBe(initialSucceeded + 2);
      // Average duration should be a non-negative number
      // (may be 0 for very fast operations, or positive for slower operations)
      expect(metrics.averageDuration).toBeGreaterThanOrEqual(0);
      expect(typeof metrics.averageDuration).toBe('number');
    });

    it('should get executor status', () => {
      // Act
      const status = executor.getStatus();

      // Assert
      expect(status.executorId).toBe(executor.id);
      expect(status.running).toBe(true);
      expect(status.registeredTasks).toBe(1);
      expect(status.metrics).toBeDefined();
      expect(status.tasks).toBeDefined();
    });

    it('should reset metrics', async () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;
      const taskId = executor.daemonToNitroMap.get(opId);
      await executor.runTask(taskId);
      daemon.emit('operation:success', { operationId: opId, duration: 100 });

      // Act
      executor.resetMetrics();

      // Assert
      const metrics = executor.getMetrics();
      expect(metrics.tasksExecuted).toBe(0);
      expect(metrics.tasksSucceeded).toBe(0);
      expect(metrics.totalDuration).toBe(0);
      expect(metrics.averageDuration).toBe(0);
    });

    it('should track failed executions', async () => {
      // Arrange
      const opId = daemon.listOperations()[0].id;

      // Act
      daemon.emit('operation:failure', { operationId: opId, error: 'Test error', duration: 50 });

      // Assert
      const metrics = executor.getMetrics();
      expect(metrics.tasksFailed).toBe(1);
    });
  });

  describe('task validation and pre-flight checks', () => {
    let opId;
    let nitroTaskId;

    beforeEach(async () => {
      await executor.start();
      daemon.schedule({
        id: generateUUID(),
        name: 'test-operation',
        handler: async () => ({ result: 'ok' }),
      });
      opId = daemon.listOperations()[0].id;
      const result = executor.registerOperationAsTask(opId, 'test-task');
      nitroTaskId = result.nitroTaskId;
    });

    it('should validate existing task', () => {
      // Act
      const validation = executor.validateTask(nitroTaskId);

      // Assert
      expect(validation.valid).toBe(true);
      expect(validation.taskId).toBe(nitroTaskId);
      expect(validation.operationId).toBe(opId);
    });

    it('should reject non-existent task', () => {
      // Act
      const validation = executor.validateTask('non-existent');

      // Assert
      expect(validation.valid).toBe(false);
      expect(validation.reason).toContain('not found');
    });

    it('should reject validation when daemon stopped', async () => {
      // Arrange
      await executor.stop();

      // Act
      const validation = executor.validateTask(nitroTaskId);

      // Assert
      expect(validation.valid).toBe(false);
      expect(validation.reason).toContain('Daemon not running');
    });
  });

  describe('helper functions', () => {
    it('should create executor with createNitroTaskExecutor()', async () => {
      // Arrange
      const newDaemon = new Daemon({
        daemonId: generateUUID(),
        name: 'helper-test',
      });

      // Act
      const newExecutor = createNitroTaskExecutor(newDaemon, { autoStart: false });

      // Assert
      expect(newExecutor).toBeInstanceOf(NitroTaskExecutor);
      expect(newExecutor.daemon).toBe(newDaemon);
    });

    it('should integrate with integrateNitroTasks()', async () => {
      // Arrange
      const newDaemon = new Daemon({
        daemonId: generateUUID(),
        name: 'integration-test',
      });

      newDaemon.schedule({
        id: generateUUID(),
        name: 'op1',
        handler: async () => ({}),
      });

      newDaemon.schedule({
        id: generateUUID(),
        name: 'op2',
        handler: async () => ({}),
      });

      const ops = newDaemon.listOperations();

      // Act
      const integratedExecutor = await integrateNitroTasks(newDaemon, ops, {
        autoStart: false,
        autoRegister: true,
      });

      // Assert
      expect(integratedExecutor).toBeInstanceOf(NitroTaskExecutor);
      expect(integratedExecutor.listTasks().length).toBe(2);

      // Cleanup
      await newDaemon.stop();
    });
  });

  describe('concurrent task execution', () => {
    beforeEach(async () => {
      await executor.start();
      for (let i = 0; i < 5; i++) {
        daemon.schedule({
          id: generateUUID(),
          name: `task-${i}`,
          handler: async () => {
            await new Promise((r) => setTimeout(r, 10));
            return { index: i };
          },
        });
      }
      const ops = daemon.listOperations();
      for (let i = 0; i < ops.length; i++) {
        executor.registerOperationAsTask(ops[i].id, `nitro-task-${i}`);
      }
    });

    it('should execute multiple tasks concurrently', async () => {
      // Arrange
      const taskIds = executor.listTasks().map((t) => executor.daemonToNitroMap.get(t.daemonOperationId));

      // Act
      const results = await Promise.all(taskIds.map((taskId) => executor.runTask(taskId)));

      // Assert
      expect(results.length).toBe(5);
      expect(results.every((r) => r.success)).toBe(true);
      expect(executor.metrics.tasksExecuted).toBe(5);
    });

    it('should handle partial concurrent failures', async () => {
      // Arrange
      const newOp = generateUUID();
      daemon.schedule({
        id: newOp,
        name: 'failing-task',
        handler: async () => {
          throw new Error('Intentional failure');
        },
      });
      executor.registerOperationAsTask(newOp, 'failing-nitro-task');

      const taskIds = executor.listTasks().map((t) => executor.daemonToNitroMap.get(t.daemonOperationId));
      const failingTaskId = executor.daemonToNitroMap.get(newOp);

      // Act
      const results = await Promise.allSettled(
        taskIds.map((taskId) => {
          try {
            return executor.runTask(taskId);
          } catch (e) {
            return Promise.resolve({ success: false, error: e.message });
          }
        })
      );

      // Assert
      expect(results.length).toBe(6);
      const fulfilled = results.filter((r) => r.status === 'fulfilled').length;
      expect(fulfilled).toBeGreaterThan(0);
    });
  });

  describe('event relay with disabled flag', () => {
    it('should not relay events when disabled', async () => {
      // Arrange
      const noRelayExecutor = new NitroTaskExecutor(daemon, {
        autoStart: true,
        enableEventRelay: false,
      });

      daemon.schedule({
        id: generateUUID(),
        name: 'test-op',
        handler: async () => ({}),
      });

      const opId = daemon.listOperations()[0].id;
      noRelayExecutor.registerOperationAsTask(opId, 'test-task');

      let eventFired = false;
      noRelayExecutor.on('task:started', () => {
        eventFired = true;
      });

      // Act
      daemon.emit('operation:started', { operationId: opId });

      // Assert
      expect(eventFired).toBe(false);

      // Cleanup
      await noRelayExecutor.stop();
    });
  });
});
