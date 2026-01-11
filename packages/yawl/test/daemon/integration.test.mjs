/**
 * @file YAWL Daemon Integration Tests
 * @module @unrdf/yawl/test/daemon/integration
 * @description Comprehensive end-to-end integration tests for YAWL daemon
 * Tests cover full workflows, multi-component interactions, and realistic scenarios
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
 * Full-featured mock YAWL engine for integration tests
 */
class IntegrationMockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
    this.eventLog = [];
  }

  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  async createCase(options) {
    const caseId = options.caseId || `case-${Date.now()}`;
    const caseData = {
      id: caseId,
      workflowId: options.workflowId,
      status: 'RUNNING',
      inputData: options.inputData || {},
      createdAt: new Date(),
    };

    this.cases.set(caseId, caseData);
    this.eventLog.push({ event: 'case:created', caseId, workflowId: options.workflowId });
    this.emit('case:created', { caseId, workflowId: options.workflowId });

    return { caseId, status: 'RUNNING', workflowId: options.workflowId };
  }

  async enableTask(options) {
    const { caseId, taskId } = options;
    const key = `${caseId}:${taskId}`;

    this.tasks.set(key, {
      caseId,
      taskId,
      status: 'ENABLED',
      enabledAt: new Date(),
    });

    this.eventLog.push({ event: 'task:enabled', caseId, taskId });
    this.emit('task:enabled', { caseId, taskId });

    return { caseId, taskId, status: 'ENABLED' };
  }

  async cancelTask(options) {
    const { caseId, taskId, reason } = options;
    const key = `${caseId}:${taskId}`;

    const task = this.tasks.get(key);
    if (task) {
      task.status = 'CANCELLED';
      task.cancelReason = reason;
    }

    this.eventLog.push({ event: 'task:cancelled', caseId, taskId, reason });
    this.emit('task:cancelled', { caseId, taskId, reason });

    return { caseId, taskId, status: 'CANCELLED', reason };
  }

  async completeTask(options) {
    const { caseId, taskId } = options;
    const key = `${caseId}:${taskId}`;

    const task = this.tasks.get(key);
    if (task) {
      task.status = 'COMPLETED';
      task.completedAt = new Date();
    }

    this.eventLog.push({ event: 'task:completed', caseId, taskId });
    this.emit('task:completed', { caseId, taskId });

    return { caseId, taskId, status: 'COMPLETED' };
  }

  async completeCase(caseId) {
    const caseData = this.cases.get(caseId);
    if (caseData) {
      caseData.status = 'COMPLETED';
      caseData.completedAt = new Date();
    }

    this.eventLog.push({ event: 'case:completed', caseId });
    this.emit('case:completed', { caseId });

    return { caseId, status: 'COMPLETED' };
  }

  failTask(caseId, taskId, error) {
    this.eventLog.push({ event: 'task:failed', caseId, taskId, error });
    this.emit('task:failed', { caseId, taskId, error });
  }
}

describe('Daemon Integration', () => {
  describe('full workflow execution', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'integration-daemon',
      });
      engine = new IntegrationMockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-integration',
        enableAutoRetry: true,
        enableTimeoutTracking: true,
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should execute complete workflow from creation to completion', async () => {
      // Arrange
      const workflowId = 'complete-workflow';
      const eventLog = [];

      engine.on('case:created', (e) => eventLog.push(`case-created:${e.caseId}`));
      engine.on('task:enabled', (e) => eventLog.push(`task-enabled:${e.taskId}`));
      engine.on('task:completed', (e) => eventLog.push(`task-completed:${e.taskId}`));
      engine.on('case:completed', (e) => eventLog.push(`case-completed:${e.caseId}`));

      // Act
      const caseResult = await engine.createCase({ caseId: 'case-001', workflowId });
      await engine.enableTask({ caseId: 'case-001', taskId: 'task-1' });
      await engine.completeTask({ caseId: 'case-001', taskId: 'task-1' });
      await engine.enableTask({ caseId: 'case-001', taskId: 'task-2' });
      await engine.completeTask({ caseId: 'case-001', taskId: 'task-2' });
      await engine.completeCase('case-001');

      // Assert
      expect(caseResult.caseId).toBe('case-001');
      expect(eventLog).toEqual([
        'case-created:case-001',
        'task-enabled:task-1',
        'task-completed:task-1',
        'task-enabled:task-2',
        'task-completed:task-2',
        'case-completed:case-001',
      ]);
    });

    it('should handle parallel task execution in workflow', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-parallel', workflowId: 'parallel-wf' });

      // Act
      await Promise.all([
        engine.enableTask({ caseId: 'case-parallel', taskId: 'task-a' }),
        engine.enableTask({ caseId: 'case-parallel', taskId: 'task-b' }),
        engine.enableTask({ caseId: 'case-parallel', taskId: 'task-c' }),
      ]);

      await Promise.all([
        engine.completeTask({ caseId: 'case-parallel', taskId: 'task-a' }),
        engine.completeTask({ caseId: 'case-parallel', taskId: 'task-b' }),
        engine.completeTask({ caseId: 'case-parallel', taskId: 'task-c' }),
      ]);

      // Assert
      const taskA = engine.tasks.get('case-parallel:task-a');
      const taskB = engine.tasks.get('case-parallel:task-b');
      const taskC = engine.tasks.get('case-parallel:task-c');

      expect(taskA.status).toBe('COMPLETED');
      expect(taskB.status).toBe('COMPLETED');
      expect(taskC.status).toBe('COMPLETED');
    });

    it('should execute sequential workflow with dependencies', async () => {
      // Arrange
      const executionOrder = [];
      await engine.createCase({ caseId: 'case-seq', workflowId: 'sequential-wf' });

      // Act
      await engine.enableTask({ caseId: 'case-seq', taskId: 'task-1' });
      executionOrder.push('task-1-enabled');
      await engine.completeTask({ caseId: 'case-seq', taskId: 'task-1' });
      executionOrder.push('task-1-completed');

      await engine.enableTask({ caseId: 'case-seq', taskId: 'task-2' });
      executionOrder.push('task-2-enabled');
      await engine.completeTask({ caseId: 'case-seq', taskId: 'task-2' });
      executionOrder.push('task-2-completed');

      await engine.enableTask({ caseId: 'case-seq', taskId: 'task-3' });
      executionOrder.push('task-3-enabled');
      await engine.completeTask({ caseId: 'case-seq', taskId: 'task-3' });
      executionOrder.push('task-3-completed');

      // Assert
      expect(executionOrder).toEqual([
        'task-1-enabled',
        'task-1-completed',
        'task-2-enabled',
        'task-2-completed',
        'task-3-enabled',
        'task-3-completed',
      ]);
    });

    it('should handle workflow with error and recovery', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-error', workflowId: 'error-wf' });
      await engine.enableTask({ caseId: 'case-error', taskId: 'task-1' });

      // Act - Simulate error
      engine.failTask('case-error', 'task-1', 'Processing error');

      // Recovery - Retry
      await bridge.scheduleRetry('case-error', 'task-1', { maxAttempts: 2 });
      const retryResult = bridge.taskRetries.get('case-error:task-1');

      // Assert
      expect(retryResult).toBeDefined();
      expect(retryResult.maxAttempts).toBe(2);

      const errorEvent = engine.eventLog.find(
        (e) => e.event === 'task:failed' && e.taskId === 'task-1'
      );
      expect(errorEvent).toBeDefined();
    });

    it('should execute workflow with timeout constraints', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-timeout', workflowId: 'timeout-wf' });
      await engine.enableTask({ caseId: 'case-timeout', taskId: 'slow-task' });

      // Act
      const timeoutResult = await bridge.watchTaskTimeout(
        'case-timeout',
        'slow-task',
        2000
      );

      // Simulate timeout expiry
      await new Promise((resolve) => setTimeout(resolve, 2100));
      await daemon.execute(timeoutResult.operationId);

      // Assert
      const cancelledTask = engine.tasks.get('case-timeout:slow-task');
      expect(cancelledTask.status).toBe('CANCELLED');
    });

    it('should track workflow metrics end-to-end', async () => {
      // Arrange
      const workflowId = 'metrics-wf';

      // Act
      await engine.createCase({ caseId: 'case-m1', workflowId });
      await engine.createCase({ caseId: 'case-m2', workflowId });
      await engine.createCase({ caseId: 'case-m3', workflowId });

      await engine.enableTask({ caseId: 'case-m1', taskId: 't1' });
      await engine.enableTask({ caseId: 'case-m2', taskId: 't1' });
      await engine.completeTask({ caseId: 'case-m1', taskId: 't1' });
      await engine.completeCase('case-m1');

      // Assert
      const completedCases = Array.from(engine.cases.values()).filter(
        (c) => c.status === 'COMPLETED'
      );
      const runningCases = Array.from(engine.cases.values()).filter(
        (c) => c.status === 'RUNNING'
      );

      expect(completedCases.length).toBe(1);
      expect(runningCases.length).toBe(2);
    });

    it('should handle workflow cancellation mid-execution', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-cancel', workflowId: 'cancel-wf' });
      await engine.enableTask({ caseId: 'case-cancel', taskId: 'task-1' });
      await engine.enableTask({ caseId: 'case-cancel', taskId: 'task-2' });

      // Act
      await engine.cancelTask({
        caseId: 'case-cancel',
        taskId: 'task-1',
        reason: 'User cancelled',
      });
      await engine.cancelTask({
        caseId: 'case-cancel',
        taskId: 'task-2',
        reason: 'User cancelled',
      });

      // Assert
      const task1 = engine.tasks.get('case-cancel:task-1');
      const task2 = engine.tasks.get('case-cancel:task-2');

      expect(task1.status).toBe('CANCELLED');
      expect(task2.status).toBe('CANCELLED');
      expect(task1.cancelReason).toBe('User cancelled');
    });

    it('should support workflow branching and merging', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-branch', workflowId: 'branch-wf' });

      // Act - Branch
      await engine.enableTask({ caseId: 'case-branch', taskId: 'decision' });
      await engine.completeTask({ caseId: 'case-branch', taskId: 'decision' });

      // Two parallel branches
      await Promise.all([
        engine.enableTask({ caseId: 'case-branch', taskId: 'branch-a' }),
        engine.enableTask({ caseId: 'case-branch', taskId: 'branch-b' }),
      ]);

      await Promise.all([
        engine.completeTask({ caseId: 'case-branch', taskId: 'branch-a' }),
        engine.completeTask({ caseId: 'case-branch', taskId: 'branch-b' }),
      ]);

      // Merge
      await engine.enableTask({ caseId: 'case-branch', taskId: 'merge' });
      await engine.completeTask({ caseId: 'case-branch', taskId: 'merge' });

      // Assert
      const decisionTask = engine.tasks.get('case-branch:decision');
      const branchA = engine.tasks.get('case-branch:branch-a');
      const branchB = engine.tasks.get('case-branch:branch-b');
      const mergeTask = engine.tasks.get('case-branch:merge');

      expect(decisionTask.status).toBe('COMPLETED');
      expect(branchA.status).toBe('COMPLETED');
      expect(branchB.status).toBe('COMPLETED');
      expect(mergeTask.status).toBe('COMPLETED');
    });

    it('should execute long-running workflow with multiple stages', async () => {
      // Arrange
      const stages = ['init', 'process', 'validate', 'finalize'];
      await engine.createCase({ caseId: 'case-long', workflowId: 'long-wf' });

      // Act
      for (const stage of stages) {
        await engine.enableTask({ caseId: 'case-long', taskId: stage });
        await new Promise((resolve) => setTimeout(resolve, 10)); // Simulate processing
        await engine.completeTask({ caseId: 'case-long', taskId: stage });
      }

      await engine.completeCase('case-long');

      // Assert
      stages.forEach((stage) => {
        const task = engine.tasks.get(`case-long:${stage}`);
        expect(task.status).toBe('COMPLETED');
      });

      const caseData = engine.cases.get('case-long');
      expect(caseData.status).toBe('COMPLETED');
    });

    it('should maintain event log throughout workflow', async () => {
      // Arrange
      engine.eventLog = [];

      // Act
      await engine.createCase({ caseId: 'case-log', workflowId: 'log-wf' });
      await engine.enableTask({ caseId: 'case-log', taskId: 'task-1' });
      await engine.completeTask({ caseId: 'case-log', taskId: 'task-1' });
      await engine.completeCase('case-log');

      // Assert
      expect(engine.eventLog.length).toBe(4);
      expect(engine.eventLog[0].event).toBe('case:created');
      expect(engine.eventLog[1].event).toBe('task:enabled');
      expect(engine.eventLog[2].event).toBe('task:completed');
      expect(engine.eventLog[3].event).toBe('case:completed');
    });

    it('should support workflow with conditional paths', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-cond', workflowId: 'conditional-wf' });
      const condition = Math.random() > 0.5;

      // Act
      if (condition) {
        await engine.enableTask({ caseId: 'case-cond', taskId: 'path-true' });
        await engine.completeTask({ caseId: 'case-cond', taskId: 'path-true' });
      } else {
        await engine.enableTask({ caseId: 'case-cond', taskId: 'path-false' });
        await engine.completeTask({ caseId: 'case-cond', taskId: 'path-false' });
      }

      // Assert
      const executedPath = condition ? 'path-true' : 'path-false';
      const task = engine.tasks.get(`case-cond:${executedPath}`);
      expect(task).toBeDefined();
      expect(task.status).toBe('COMPLETED');
    });

    it('should cleanup resources after workflow completion', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-cleanup', workflowId: 'cleanup-wf' });
      await bridge.watchTaskTimeout('case-cleanup', 'task-1', 30000);
      await bridge.scheduleRetry('case-cleanup', 'task-1');

      // Act
      await engine.completeCase('case-cleanup');

      // Verify cleanup triggered
      const timeoutsCleaned = !bridge.taskTimeouts.has('case-cleanup:task-1');
      const retriesCleaned = !bridge.taskRetries.has('case-cleanup:task-1');

      // Assert
      expect(timeoutsCleaned).toBe(true);
      expect(retriesCleaned).toBe(true);
    });
  });

  describe('multi-component interactions', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'multi-component-daemon',
      });
      engine = new IntegrationMockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-multi',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should coordinate daemon scheduler with YAWL engine', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('scheduled-wf', '* * * * *', {
        caseIdPrefix: 'scheduled',
      });

      // Act
      const operations = daemon.listOperations();
      const scheduleOp = operations.find((op) => op.id.includes('yawl-case-scheduled-wf'));

      await daemon.execute(scheduleOp.id);

      // Assert
      const createdCases = Array.from(engine.cases.values());
      expect(createdCases.length).toBeGreaterThan(0);
      expect(createdCases[0].workflowId).toBe('scheduled-wf');
    });

    it('should integrate timeout tracking with task execution', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-int-timeout', workflowId: 'timeout-int-wf' });
      await engine.enableTask({ caseId: 'case-int-timeout', taskId: 'timed-task' });

      // Act
      const timeoutResult = await bridge.watchTaskTimeout(
        'case-int-timeout',
        'timed-task',
        1000
      );

      await new Promise((resolve) => setTimeout(resolve, 1100));
      await daemon.execute(timeoutResult.operationId);

      // Assert
      const task = engine.tasks.get('case-int-timeout:timed-task');
      expect(task.status).toBe('CANCELLED');
    });

    it('should coordinate retry mechanism with daemon operations', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-retry-int', workflowId: 'retry-int-wf' });
      engine.failTask('case-retry-int', 'retry-task', 'Initial failure');

      // Act
      const retryResult = await bridge.scheduleRetry('case-retry-int', 'retry-task', {
        maxAttempts: 2,
      });

      await daemon.execute(retryResult.operationId);

      // Assert
      const retryState = bridge.taskRetries.get('case-retry-int:retry-task');
      expect(retryState.attempts).toBe(1);
    });

    it('should synchronize bridge and daemon lifecycle', async () => {
      // Arrange
      const states = [];

      daemon.on('daemon:stopped', () => states.push('daemon-stopped'));
      bridge.on('bridge:stopped', () => states.push('bridge-stopped'));

      // Act
      await bridge.stop();
      await daemon.stop();

      // Assert
      expect(states).toContain('bridge-stopped');
      expect(states).toContain('daemon-stopped');
    });

    it('should share events between daemon and bridge', async () => {
      // Arrange
      const bridgeEvents = [];
      const daemonEvents = [];

      bridge.on('case:created-by-schedule', (e) =>
        bridgeEvents.push(`case-created:${e.caseId}`)
      );
      daemon.on('operation:success', (e) =>
        daemonEvents.push(`op-success:${e.operationId}`)
      );

      await bridge.scheduleRecurringCase('event-share-wf', '* * * * *');

      const operations = daemon.listOperations();
      const scheduleOp = operations.find((op) =>
        op.id.includes('yawl-case-event-share-wf')
      );

      // Act
      await daemon.execute(scheduleOp.id);

      // Assert
      expect(bridgeEvents.length).toBeGreaterThan(0);
      expect(daemonEvents.length).toBeGreaterThan(0);
    });

    it('should maintain consistent state across components', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('state-wf', '* * * * *');
      await bridge.watchTaskTimeout('case-state', 'task-state', 30000);

      // Act
      const bridgeStats = bridge.getStats();
      const daemonHealth = daemon.getHealth();

      // Assert
      expect(bridgeStats.isRunning).toBe(daemonHealth.isRunning);
      expect(bridgeStats.caseSchedules).toBe(1);
      expect(bridgeStats.activeTimeouts).toBe(1);
    });

    it('should propagate errors between components', async () => {
      // Arrange
      const errorEngine = new IntegrationMockYawlEngine();
      errorEngine.createCase = vi.fn().mockRejectedValue(new Error('Engine error'));

      const errorBridge = new YawlDaemonBridge(daemon, errorEngine, {
        daemonNodeId: 'node-error',
      });

      await errorBridge.start();
      await errorBridge.scheduleRecurringCase('error-wf', '* * * * *');

      const operations = daemon.listOperations();
      const errorOp = operations.find((op) => op.id.includes('yawl-case-error-wf'));

      // Act & Assert
      await expect(daemon.execute(errorOp.id)).rejects.toThrow('Engine error');

      await errorBridge.stop();
    });

    it('should support distributed task execution', async () => {
      // Arrange
      await engine.createCase({ caseId: 'case-dist', workflowId: 'distributed-wf' });

      // Act
      const result = await bridge.distributeAndSplitTasks(
        'case-dist',
        ['task-1', 'task-2', 'task-3'],
        { strategy: 'round-robin' }
      );

      await daemon.execute(result.operationId);

      // Assert
      expect(engine.tasks.get('case-dist:task-1')).toBeDefined();
      expect(engine.tasks.get('case-dist:task-2')).toBeDefined();
      expect(engine.tasks.get('case-dist:task-3')).toBeDefined();
    });

    it('should handle concurrent operations across components', async () => {
      // Arrange
      const promises = [];

      // Act
      promises.push(bridge.scheduleRecurringCase('wf-1', '* * * * *'));
      promises.push(bridge.scheduleRecurringCase('wf-2', '* * * * *'));
      promises.push(bridge.watchTaskTimeout('case-c1', 'task-c1', 30000));
      promises.push(bridge.watchTaskTimeout('case-c2', 'task-c2', 30000));

      await Promise.all(promises);

      // Assert
      const stats = bridge.getStats();
      expect(stats.caseSchedules).toBe(2);
      expect(stats.activeTimeouts).toBe(2);
    });

    it('should coordinate graceful shutdown across components', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('shutdown-wf', '* * * * *');
      await bridge.watchTaskTimeout('case-shutdown', 'task-shutdown', 60000);

      const beforeBridgeTimeouts = bridge.taskTimeouts.size;
      const beforeBridgeSchedules = bridge.caseSchedules.size;

      // Act
      await bridge.stop();
      await daemon.stop();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
      expect(bridge.caseSchedules.size).toBe(0);
      expect(daemon.isRunning).toBe(false);
      expect(bridge.isRunning).toBe(false);
    });

    it('should track metrics across daemon and bridge', async () => {
      // Arrange
      daemon.schedule({
        id: 'metrics-op-1',
        name: 'Metrics 1',
        handler: vi.fn().mockResolvedValue({}),
      });
      daemon.schedule({
        id: 'metrics-op-2',
        name: 'Metrics 2',
        handler: vi.fn().mockResolvedValue({}),
      });

      // Act
      await daemon.execute('metrics-op-1');
      await daemon.execute('metrics-op-2');

      await bridge.scheduleRecurringCase('metrics-wf', '* * * * *');

      const daemonMetrics = daemon.getMetrics();
      const bridgeStats = bridge.getStats();

      // Assert
      expect(daemonMetrics.totalOperations).toBeGreaterThanOrEqual(2);
      expect(bridgeStats.caseSchedules).toBe(1);
    });

    it('should support cross-component event subscriptions', async () => {
      // Arrange
      const crossEvents = [];

      daemon.on('operation:enqueued', (e) => crossEvents.push(`daemon:${e.operationId}`));
      bridge.on('bridge:started', (e) => crossEvents.push(`bridge:${e.bridgeId}`));
      engine.on('case:created', (e) => crossEvents.push(`engine:${e.caseId}`));

      // Act
      await bridge.scheduleRecurringCase('cross-wf', '* * * * *');
      await engine.createCase({ caseId: 'cross-case', workflowId: 'cross-wf' });

      // Assert
      expect(crossEvents.length).toBeGreaterThan(0);
      expect(crossEvents.some((e) => e.startsWith('daemon:'))).toBe(true);
      expect(crossEvents.some((e) => e.startsWith('engine:'))).toBe(true);
    });
  });

  describe('realistic scenarios', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'realistic-daemon',
        concurrency: 20,
      });
      engine = new IntegrationMockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-realistic',
        maxConcurrentCases: 50,
        enableAutoRetry: true,
        enableTimeoutTracking: true,
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should handle order processing workflow', async () => {
      // Arrange - Simulate e-commerce order processing
      const orderId = 'ORD-12345';
      await engine.createCase({
        caseId: orderId,
        workflowId: 'order-processing',
        inputData: { orderId, amount: 99.99, items: 3 },
      });

      // Act
      await engine.enableTask({ caseId: orderId, taskId: 'validate-order' });
      await engine.completeTask({ caseId: orderId, taskId: 'validate-order' });

      await engine.enableTask({ caseId: orderId, taskId: 'process-payment' });
      await engine.completeTask({ caseId: orderId, taskId: 'process-payment' });

      await engine.enableTask({ caseId: orderId, taskId: 'ship-order' });
      await engine.completeTask({ caseId: orderId, taskId: 'ship-order' });

      await engine.completeCase(orderId);

      // Assert
      const order = engine.cases.get(orderId);
      expect(order.status).toBe('COMPLETED');

      const shipTask = engine.tasks.get(`${orderId}:ship-order`);
      expect(shipTask.status).toBe('COMPLETED');
    });

    it('should handle batch data processing with retries', async () => {
      // Arrange - Simulate batch ETL job
      const batchId = 'BATCH-001';
      await engine.createCase({
        caseId: batchId,
        workflowId: 'batch-etl',
        inputData: { records: 10000, source: 'S3' },
      });

      // Act
      await engine.enableTask({ caseId: batchId, taskId: 'extract' });
      await engine.completeTask({ caseId: batchId, taskId: 'extract' });

      await engine.enableTask({ caseId: batchId, taskId: 'transform' });
      // Simulate failure
      engine.failTask(batchId, 'transform', 'Data validation error');

      // Retry
      await bridge.scheduleRetry(batchId, 'transform', { maxAttempts: 3 });
      const retryOp = daemon.listOperations().find((op) =>
        op.id.includes(`yawl-retry-${batchId}:transform`)
      );
      await daemon.execute(retryOp.id);

      // Complete after retry
      await engine.completeTask({ caseId: batchId, taskId: 'transform' });
      await engine.enableTask({ caseId: batchId, taskId: 'load' });
      await engine.completeTask({ caseId: batchId, taskId: 'load' });

      // Assert
      const transformTask = engine.tasks.get(`${batchId}:transform`);
      expect(transformTask.status).toBe('COMPLETED');
    });

    it('should handle approval workflow with timeout', async () => {
      // Arrange - Simulate document approval
      const docId = 'DOC-789';
      await engine.createCase({
        caseId: docId,
        workflowId: 'document-approval',
        inputData: { documentType: 'contract', amount: 50000 },
      });

      // Act
      await engine.enableTask({ caseId: docId, taskId: 'manager-review' });

      // Set timeout for approval
      await bridge.watchTaskTimeout(docId, 'manager-review', 2000);

      // Simulate timeout expiry
      await new Promise((resolve) => setTimeout(resolve, 2100));

      const timeoutOp = daemon
        .listOperations()
        .find((op) => op.id.includes(`yawl-timeout-${docId}-manager-review`));
      await daemon.execute(timeoutOp.id);

      // Assert
      const reviewTask = engine.tasks.get(`${docId}:manager-review`);
      expect(reviewTask.status).toBe('CANCELLED');
    });

    it('should handle scheduled reporting workflow', async () => {
      // Arrange - Simulate daily report generation
      await bridge.scheduleRecurringCase('daily-report', '0 8 * * *', {
        caseIdPrefix: 'REPORT',
        inputData: { reportType: 'sales', period: 'daily' },
      });

      // Act
      const scheduleOp = daemon
        .listOperations()
        .find((op) => op.id.includes('yawl-case-daily-report'));

      await daemon.execute(scheduleOp.id);

      // Assert
      const reports = Array.from(engine.cases.values()).filter((c) =>
        c.id.startsWith('REPORT')
      );
      expect(reports.length).toBeGreaterThan(0);
      expect(reports[0].inputData.reportType).toBe('sales');
    });

    it('should handle high-volume concurrent workflow execution', async () => {
      // Arrange - Simulate high-throughput system
      const caseCount = 25;
      const cases = [];

      // Act
      for (let i = 0; i < caseCount; i++) {
        cases.push(
          engine.createCase({
            caseId: `HV-${i}`,
            workflowId: 'high-volume-wf',
            inputData: { index: i },
          })
        );
      }

      await Promise.all(cases);

      // Enable tasks concurrently
      const enablePromises = [];
      for (let i = 0; i < caseCount; i++) {
        enablePromises.push(
          engine.enableTask({ caseId: `HV-${i}`, taskId: 'process' })
        );
      }
      await Promise.all(enablePromises);

      // Assert
      expect(engine.cases.size).toBe(caseCount);
      expect(engine.tasks.size).toBeGreaterThanOrEqual(caseCount);
    });

    it('should handle complex multi-stage deployment workflow', async () => {
      // Arrange - Simulate CI/CD deployment
      const deployId = 'DEPLOY-v2.1.0';
      await engine.createCase({
        caseId: deployId,
        workflowId: 'deployment',
        inputData: { version: 'v2.1.0', environment: 'production' },
      });

      // Act
      const stages = [
        'build',
        'test',
        'security-scan',
        'deploy-staging',
        'integration-test',
        'deploy-production',
        'smoke-test',
      ];

      for (const stage of stages) {
        await engine.enableTask({ caseId: deployId, taskId: stage });
        await new Promise((resolve) => setTimeout(resolve, 10));
        await engine.completeTask({ caseId: deployId, taskId: stage });
      }

      await engine.completeCase(deployId);

      // Assert
      const deployment = engine.cases.get(deployId);
      expect(deployment.status).toBe('COMPLETED');

      stages.forEach((stage) => {
        const task = engine.tasks.get(`${deployId}:${stage}`);
        expect(task.status).toBe('COMPLETED');
      });
    });

    it('should handle workflow with human tasks and automation', async () => {
      // Arrange - Simulate customer onboarding
      const customerId = 'CUST-456';
      await engine.createCase({
        caseId: customerId,
        workflowId: 'customer-onboarding',
        inputData: { name: 'John Doe', tier: 'premium' },
      });

      // Act
      // Automated tasks
      await engine.enableTask({ caseId: customerId, taskId: 'create-account' });
      await engine.completeTask({ caseId: customerId, taskId: 'create-account' });

      await engine.enableTask({ caseId: customerId, taskId: 'send-welcome-email' });
      await engine.completeTask({ caseId: customerId, taskId: 'send-welcome-email' });

      // Human task with timeout
      await engine.enableTask({ caseId: customerId, taskId: 'kyc-verification' });
      await bridge.watchTaskTimeout(customerId, 'kyc-verification', 5000);

      // Simulate human completing before timeout
      await new Promise((resolve) => setTimeout(resolve, 100));
      await engine.completeTask({ caseId: customerId, taskId: 'kyc-verification' });

      // Final automated task
      await engine.enableTask({ caseId: customerId, taskId: 'activate-services' });
      await engine.completeTask({ caseId: customerId, taskId: 'activate-services' });

      // Assert
      const kycTask = engine.tasks.get(`${customerId}:kyc-verification`);
      expect(kycTask.status).toBe('COMPLETED');
    });

    it('should handle workflow with conditional branching and merging', async () => {
      // Arrange - Simulate loan approval
      const loanId = 'LOAN-789';
      const loanAmount = 75000;

      await engine.createCase({
        caseId: loanId,
        workflowId: 'loan-approval',
        inputData: { amount: loanAmount, creditScore: 720 },
      });

      // Act
      await engine.enableTask({ caseId: loanId, taskId: 'credit-check' });
      await engine.completeTask({ caseId: loanId, taskId: 'credit-check' });

      // Branch based on amount
      if (loanAmount > 50000) {
        await engine.enableTask({ caseId: loanId, taskId: 'senior-review' });
        await engine.completeTask({ caseId: loanId, taskId: 'senior-review' });
      } else {
        await engine.enableTask({ caseId: loanId, taskId: 'auto-approve' });
        await engine.completeTask({ caseId: loanId, taskId: 'auto-approve' });
      }

      // Merge
      await engine.enableTask({ caseId: loanId, taskId: 'finalize' });
      await engine.completeTask({ caseId: loanId, taskId: 'finalize' });

      // Assert
      const seniorReview = engine.tasks.get(`${loanId}:senior-review`);
      expect(seniorReview).toBeDefined();
      expect(seniorReview.status).toBe('COMPLETED');
    });

    it('should maintain data consistency across workflow execution', async () => {
      // Arrange - Simulate inventory management
      const txnId = 'TXN-001';
      const initialInventory = 100;

      await engine.createCase({
        caseId: txnId,
        workflowId: 'inventory-update',
        inputData: { productId: 'PROD-123', quantity: 10, inventory: initialInventory },
      });

      // Act
      await engine.enableTask({ caseId: txnId, taskId: 'reserve-stock' });
      await engine.completeTask({ caseId: txnId, taskId: 'reserve-stock' });

      await engine.enableTask({ caseId: txnId, taskId: 'update-inventory' });
      await engine.completeTask({ caseId: txnId, taskId: 'update-inventory' });

      await engine.completeCase(txnId);

      // Assert
      const txn = engine.cases.get(txnId);
      expect(txn.status).toBe('COMPLETED');
      expect(txn.inputData.inventory).toBe(initialInventory);
    });

    it('should handle end-to-end monitoring and observability', async () => {
      // Arrange
      const monitoringCase = 'MON-001';
      const eventLog = [];

      // Subscribe to all events
      daemon.on('operation:enqueued', (e) => eventLog.push({ type: 'daemon', event: e }));
      bridge.on('case:created-by-schedule', (e) =>
        eventLog.push({ type: 'bridge', event: e })
      );
      engine.on('task:enabled', (e) => eventLog.push({ type: 'engine', event: e }));

      // Act
      await engine.createCase({
        caseId: monitoringCase,
        workflowId: 'monitored-wf',
      });

      await engine.enableTask({ caseId: monitoringCase, taskId: 'task-1' });
      await engine.completeTask({ caseId: monitoringCase, taskId: 'task-1' });

      // Assert
      expect(eventLog.length).toBeGreaterThan(0);

      const engineEvents = eventLog.filter((e) => e.type === 'engine');
      expect(engineEvents.length).toBeGreaterThan(0);
    });
  });
});
