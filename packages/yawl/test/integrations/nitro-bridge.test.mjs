/**
 * @file YAWL-Nitro Bridge Integration Tests
 * @module @unrdf/yawl/test/integrations/nitro-bridge
 * @description Comprehensive tests for bidirectional YAWL ↔ Nitro bridge
 *
 * Tests cover:
 * - Bidirectional event flow (YAWL → Nitro → YAWL)
 * - State synchronization
 * - Event propagation and transformation
 * - Bridge lifecycle management
 * - Error recovery and resilience
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';

/**
 * Mock YAWL Engine
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
  }

  async createCase(options) {
    const caseData = {
      id: options.caseId,
      workflowId: options.workflowId,
      status: 'active',
      variables: options.variables || {},
    };
    this.cases.set(options.caseId, caseData);
    this.emit('case:created', caseData);
    return caseData;
  }

  async enableTask(caseId, taskId) {
    const task = { caseId, taskId, status: 'enabled' };
    this.tasks.set(`${caseId}:${taskId}`, task);
    this.emit('task:enabled', task);
    return task;
  }

  async completeTask(caseId, taskId, output) {
    const key = `${caseId}:${taskId}`;
    const task = this.tasks.get(key);
    if (task) {
      task.status = 'completed';
      task.output = output;
      this.emit('task:completed', task);
    }
    return task;
  }

  async cancelCase(caseId) {
    const caseData = this.cases.get(caseId);
    if (caseData) {
      caseData.status = 'cancelled';
      this.emit('case:cancelled', caseData);
    }
    return caseData;
  }
}

/**
 * Mock Nitro System
 */
class MockNitroSystem extends EventEmitter {
  constructor() {
    super();
    this.tasks = new Map();
    this.queue = [];
  }

  async submitTask(config) {
    const task = {
      id: config.id,
      type: config.type,
      payload: config.payload,
      status: 'queued',
      createdAt: new Date(),
    };
    this.tasks.set(task.id, task);
    this.queue.push(task.id);
    this.emit('task:queued', task);
    return task;
  }

  async executeTask(taskId) {
    const task = this.tasks.get(taskId);
    if (task) {
      task.status = 'executing';
      this.emit('task:executing', task);

      // Simulate execution
      await new Promise((resolve) => setTimeout(resolve, 10));

      task.status = 'completed';
      task.result = { success: true, data: task.payload };
      this.emit('task:completed', task);
    }
    return task;
  }

  async cancelTask(taskId) {
    const task = this.tasks.get(taskId);
    if (task) {
      task.status = 'cancelled';
      this.emit('task:cancelled', task);
    }
    return task;
  }

  getTaskStatus(taskId) {
    return this.tasks.get(taskId)?.status || null;
  }
}

/**
 * YAWL-Nitro Bridge
 */
class YawlNitroBridge extends EventEmitter {
  constructor(yawlEngine, nitroSystem) {
    super();
    this.yawlEngine = yawlEngine;
    this.nitroSystem = nitroSystem;
    this.bridgeState = 'stopped';
    this.mappings = new Map(); // YAWL task ID → Nitro task ID
    this.reverseMappings = new Map(); // Nitro task ID → YAWL task ID
    this.eventLog = [];
  }

  /**
   * Start the bridge
   */
  start() {
    if (this.bridgeState === 'running') {
      throw new Error('Bridge already running');
    }

    this.bridgeState = 'running';

    // Listen to YAWL events
    this.yawlEngine.on('task:enabled', this.handleYawlTaskEnabled.bind(this));
    this.yawlEngine.on('case:cancelled', this.handleYawlCaseCancelled.bind(this));

    // Listen to Nitro events
    this.nitroSystem.on('task:completed', this.handleNitroTaskCompleted.bind(this));
    this.nitroSystem.on('task:cancelled', this.handleNitroTaskCancelled.bind(this));

    this.emit('bridge:started');
    this.eventLog.push({ type: 'bridge:started', timestamp: new Date() });
  }

  /**
   * Stop the bridge
   */
  stop() {
    if (this.bridgeState === 'stopped') {
      return;
    }

    this.bridgeState = 'stopped';

    this.yawlEngine.removeAllListeners();
    this.nitroSystem.removeAllListeners();

    this.emit('bridge:stopped');
    this.eventLog.push({ type: 'bridge:stopped', timestamp: new Date() });
  }

  /**
   * Handle YAWL task enabled - forward to Nitro
   */
  async handleYawlTaskEnabled(task) {
    if (this.bridgeState !== 'running') return;

    const nitroTask = await this.nitroSystem.submitTask({
      id: `nitro-${task.caseId}-${task.taskId}`,
      type: 'workflow-task',
      payload: {
        caseId: task.caseId,
        taskId: task.taskId,
      },
    });

    this.mappings.set(`${task.caseId}:${task.taskId}`, nitroTask.id);
    this.reverseMappings.set(nitroTask.id, `${task.caseId}:${task.taskId}`);

    this.emit('yawl:task:forwarded', { yawlTask: task, nitroTask });
    this.eventLog.push({
      type: 'yawl:task:forwarded',
      yawlTaskId: task.taskId,
      nitroTaskId: nitroTask.id,
      timestamp: new Date(),
    });

    // Auto-execute Nitro task
    await this.nitroSystem.executeTask(nitroTask.id);
  }

  /**
   * Handle YAWL case cancelled - cancel all related Nitro tasks
   */
  async handleYawlCaseCancelled(caseData) {
    if (this.bridgeState !== 'running') return;

    const tasksToCancel = [];
    for (const [yawlKey, nitroTaskId] of this.mappings.entries()) {
      if (yawlKey.startsWith(`${caseData.id}:`)) {
        tasksToCancel.push(nitroTaskId);
      }
    }

    for (const nitroTaskId of tasksToCancel) {
      await this.nitroSystem.cancelTask(nitroTaskId);
    }

    this.emit('yawl:case:cancelled:propagated', {
      caseId: caseData.id,
      cancelledTasks: tasksToCancel,
    });
  }

  /**
   * Handle Nitro task completed - update YAWL
   */
  async handleNitroTaskCompleted(nitroTask) {
    if (this.bridgeState !== 'running') return;

    const yawlKey = this.reverseMappings.get(nitroTask.id);
    if (yawlKey) {
      const [caseId, taskId] = yawlKey.split(':');
      await this.yawlEngine.completeTask(caseId, taskId, nitroTask.result);

      this.emit('nitro:task:completed:propagated', { nitroTask, caseId, taskId });
      this.eventLog.push({
        type: 'nitro:task:completed:propagated',
        nitroTaskId: nitroTask.id,
        yawlTaskId: taskId,
        timestamp: new Date(),
      });
    }
  }

  /**
   * Handle Nitro task cancelled - update YAWL
   */
  async handleNitroTaskCancelled(nitroTask) {
    if (this.bridgeState !== 'running') return;

    const yawlKey = this.reverseMappings.get(nitroTask.id);
    if (yawlKey) {
      this.emit('nitro:task:cancelled:detected', { nitroTask, yawlKey });
    }
  }

  /**
   * Get bridge statistics
   */
  getStatistics() {
    return {
      state: this.bridgeState,
      activeMappings: this.mappings.size,
      eventLogSize: this.eventLog.length,
    };
  }
}

describe('YAWL-Nitro Bridge Integration', () => {
  let yawlEngine;
  let nitroSystem;
  let bridge;

  beforeEach(() => {
    yawlEngine = new MockYawlEngine();
    nitroSystem = new MockNitroSystem();
    bridge = new YawlNitroBridge(yawlEngine, nitroSystem);
  });

  afterEach(() => {
    if (bridge.bridgeState === 'running') {
      bridge.stop();
    }
    yawlEngine.removeAllListeners();
    nitroSystem.removeAllListeners();
    bridge.removeAllListeners();
  });

  describe('Bridge Lifecycle', () => {
    it('should start bridge successfully', () => {
      // Arrange
      const startSpy = vi.fn();
      bridge.on('bridge:started', startSpy);

      // Act
      bridge.start();

      // Assert
      expect(bridge.bridgeState).toBe('running');
      expect(startSpy).toHaveBeenCalledOnce();
    });

    it('should stop bridge successfully', () => {
      // Arrange
      bridge.start();
      const stopSpy = vi.fn();
      bridge.on('bridge:stopped', stopSpy);

      // Act
      bridge.stop();

      // Assert
      expect(bridge.bridgeState).toBe('stopped');
      expect(stopSpy).toHaveBeenCalledOnce();
    });

    it('should throw error when starting already running bridge', () => {
      // Arrange
      bridge.start();

      // Act & Assert
      expect(() => bridge.start()).toThrow('Bridge already running');
    });

    it('should not throw when stopping already stopped bridge', () => {
      // Arrange
      // Bridge is stopped by default

      // Act & Assert
      expect(() => bridge.stop()).not.toThrow();
    });

    it('should log lifecycle events', () => {
      // Arrange
      // Act
      bridge.start();
      bridge.stop();

      // Assert
      expect(bridge.eventLog).toHaveLength(2);
      expect(bridge.eventLog[0].type).toBe('bridge:started');
      expect(bridge.eventLog[1].type).toBe('bridge:stopped');
    });

    it('should initialize with stopped state', () => {
      // Arrange & Act
      const newBridge = new YawlNitroBridge(yawlEngine, nitroSystem);

      // Assert
      expect(newBridge.bridgeState).toBe('stopped');
    });
  });

  describe('YAWL to Nitro Event Flow', () => {
    it('should forward YAWL task enabled event to Nitro', async () => {
      // Arrange
      bridge.start();
      const forwardSpy = vi.fn();
      bridge.on('yawl:task:forwarded', forwardSpy);

      // Act
      await yawlEngine.enableTask('case-001', 'task-001');
      await new Promise((resolve) => setTimeout(resolve, 20));

      // Assert
      expect(forwardSpy).toHaveBeenCalledOnce();
      const event = forwardSpy.mock.calls[0][0];
      expect(event.yawlTask.taskId).toBe('task-001');
      expect(event.nitroTask.id).toContain('nitro-case-001-task-001');
    });

    it('should create bidirectional mapping on task forward', async () => {
      // Arrange
      bridge.start();

      // Act
      await yawlEngine.enableTask('case-002', 'task-002');
      await new Promise((resolve) => setTimeout(resolve, 20));

      // Assert
      const nitroTaskId = bridge.mappings.get('case-002:task-002');
      expect(nitroTaskId).toBeDefined();
      expect(bridge.reverseMappings.get(nitroTaskId)).toBe('case-002:task-002');
    });

    it('should forward multiple YAWL tasks to Nitro', async () => {
      // Arrange
      bridge.start();
      const forwardSpy = vi.fn();
      bridge.on('yawl:task:forwarded', forwardSpy);

      // Act
      await yawlEngine.enableTask('case-003', 'task-003');
      await yawlEngine.enableTask('case-003', 'task-004');
      await yawlEngine.enableTask('case-003', 'task-005');
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(forwardSpy).toHaveBeenCalledTimes(3);
      expect(bridge.mappings.size).toBe(3);
    });

    it('should not forward tasks when bridge is stopped', async () => {
      // Arrange
      const forwardSpy = vi.fn();
      bridge.on('yawl:task:forwarded', forwardSpy);

      // Act
      await yawlEngine.enableTask('case-004', 'task-006');
      await new Promise((resolve) => setTimeout(resolve, 20));

      // Assert
      expect(forwardSpy).not.toHaveBeenCalled();
    });

    it('should auto-execute Nitro task after forwarding', async () => {
      // Arrange
      bridge.start();
      const executeSpy = vi.fn();
      nitroSystem.on('task:executing', executeSpy);

      // Act
      await yawlEngine.enableTask('case-005', 'task-007');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      expect(executeSpy).toHaveBeenCalledOnce();
    });
  });

  describe('Nitro to YAWL Event Flow', () => {
    it('should propagate Nitro task completion to YAWL', async () => {
      // Arrange
      bridge.start();
      const propagateSpy = vi.fn();
      bridge.on('nitro:task:completed:propagated', propagateSpy);

      // Act
      await yawlEngine.enableTask('case-006', 'task-008');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      expect(propagateSpy).toHaveBeenCalledOnce();
      const event = propagateSpy.mock.calls[0][0];
      expect(event.caseId).toBe('case-006');
      expect(event.taskId).toBe('task-008');
    });

    it('should update YAWL task status on Nitro completion', async () => {
      // Arrange
      bridge.start();
      const completeSpy = vi.fn();
      yawlEngine.on('task:completed', completeSpy);

      // Act
      await yawlEngine.enableTask('case-007', 'task-009');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      expect(completeSpy).toHaveBeenCalledOnce();
      const task = completeSpy.mock.calls[0][0];
      expect(task.status).toBe('completed');
      expect(task.output).toBeDefined();
    });

    it('should handle Nitro task cancellation event', async () => {
      // Arrange
      bridge.start();
      const cancelSpy = vi.fn();
      bridge.on('nitro:task:cancelled:detected', cancelSpy);

      const nitroTask = await nitroSystem.submitTask({
        id: 'nitro-manual-001',
        type: 'test',
        payload: {},
      });
      bridge.reverseMappings.set('nitro-manual-001', 'case-008:task-010');

      // Act
      await nitroSystem.cancelTask('nitro-manual-001');
      await new Promise((resolve) => setTimeout(resolve, 20));

      // Assert
      expect(cancelSpy).toHaveBeenCalledOnce();
    });

    it('should preserve result data in propagation', async () => {
      // Arrange
      bridge.start();
      const completeSpy = vi.fn();
      yawlEngine.on('task:completed', completeSpy);

      // Act
      await yawlEngine.enableTask('case-009', 'task-011');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      const task = completeSpy.mock.calls[0][0];
      expect(task.output.success).toBe(true);
      expect(task.output.data).toBeDefined();
    });
  });

  describe('Case-Level Operations', () => {
    it('should cancel all Nitro tasks when YAWL case is cancelled', async () => {
      // Arrange
      bridge.start();
      await yawlEngine.createCase({ caseId: 'case-010', workflowId: 'wf-001' });
      await yawlEngine.enableTask('case-010', 'task-012');
      await yawlEngine.enableTask('case-010', 'task-013');
      await new Promise((resolve) => setTimeout(resolve, 50));

      const cancelSpy = vi.fn();
      nitroSystem.on('task:cancelled', cancelSpy);

      // Act
      await yawlEngine.cancelCase('case-010');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      expect(cancelSpy).toHaveBeenCalledTimes(2);
    });

    it('should emit case cancellation propagation event', async () => {
      // Arrange
      bridge.start();
      await yawlEngine.createCase({ caseId: 'case-011', workflowId: 'wf-002' });
      await yawlEngine.enableTask('case-011', 'task-014');
      await new Promise((resolve) => setTimeout(resolve, 30));

      const propagateSpy = vi.fn();
      bridge.on('yawl:case:cancelled:propagated', propagateSpy);

      // Act
      await yawlEngine.cancelCase('case-011');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      expect(propagateSpy).toHaveBeenCalledOnce();
      const event = propagateSpy.mock.calls[0][0];
      expect(event.caseId).toBe('case-011');
      expect(event.cancelledTasks).toHaveLength(1);
    });

    it('should handle case cancellation with no active tasks', async () => {
      // Arrange
      bridge.start();
      await yawlEngine.createCase({ caseId: 'case-012', workflowId: 'wf-003' });

      const propagateSpy = vi.fn();
      bridge.on('yawl:case:cancelled:propagated', propagateSpy);

      // Act
      await yawlEngine.cancelCase('case-012');
      await new Promise((resolve) => setTimeout(resolve, 20));

      // Assert
      expect(propagateSpy).toHaveBeenCalledOnce();
      const event = propagateSpy.mock.calls[0][0];
      expect(event.cancelledTasks).toHaveLength(0);
    });
  });

  describe('State Synchronization', () => {
    it('should maintain mapping consistency', async () => {
      // Arrange
      bridge.start();

      // Act
      await yawlEngine.enableTask('case-013', 'task-015');
      await yawlEngine.enableTask('case-013', 'task-016');
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(bridge.mappings.size).toBe(bridge.reverseMappings.size);
      expect(bridge.mappings.size).toBe(2);
    });

    it('should track event log correctly', async () => {
      // Arrange
      bridge.start();

      // Act
      await yawlEngine.enableTask('case-014', 'task-017');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Assert
      expect(bridge.eventLog.length).toBeGreaterThan(0);
      const forwardEvent = bridge.eventLog.find((e) => e.type === 'yawl:task:forwarded');
      expect(forwardEvent).toBeDefined();
    });

    it('should provide accurate statistics', async () => {
      // Arrange
      bridge.start();
      await yawlEngine.enableTask('case-015', 'task-018');
      await new Promise((resolve) => setTimeout(resolve, 30));

      // Act
      const stats = bridge.getStatistics();

      // Assert
      expect(stats.state).toBe('running');
      expect(stats.activeMappings).toBeGreaterThan(0);
      expect(stats.eventLogSize).toBeGreaterThan(0);
    });
  });

  describe('Error Recovery', () => {
    it('should handle YAWL event when Nitro submission fails', async () => {
      // Arrange
      bridge.start();
      nitroSystem.submitTask = vi.fn().mockRejectedValue(new Error('Nitro unavailable'));

      // Act & Assert
      await expect(yawlEngine.enableTask('case-016', 'task-019')).rejects.toThrow();
    });

    it('should continue processing after single task failure', async () => {
      // Arrange
      bridge.start();
      let callCount = 0;
      nitroSystem.submitTask = vi.fn().mockImplementation((config) => {
        callCount++;
        if (callCount === 1) {
          return Promise.reject(new Error('First task failed'));
        }
        return Promise.resolve({
          id: config.id,
          type: config.type,
          payload: config.payload,
          status: 'queued',
        });
      });

      // Act
      try {
        await yawlEngine.enableTask('case-017', 'task-020');
      } catch (e) {
        // Expected to fail
      }
      await yawlEngine.enableTask('case-017', 'task-021');
      await new Promise((resolve) => setTimeout(resolve, 20));

      // Assert
      expect(callCount).toBe(2);
    });

    it('should handle missing mapping gracefully', async () => {
      // Arrange
      bridge.start();
      const orphanTask = {
        id: 'orphan-nitro-task',
        status: 'completed',
        result: { success: true },
      };

      // Act & Assert
      expect(() => bridge.handleNitroTaskCompleted(orphanTask)).not.toThrow();
    });
  });

  describe('Concurrent Operations', () => {
    it('should handle concurrent YAWL task enabling', async () => {
      // Arrange
      bridge.start();
      const tasks = Array.from({ length: 10 }, (_, i) => ({
        caseId: 'case-018',
        taskId: `task-${100 + i}`,
      }));

      // Act
      await Promise.all(tasks.map((t) => yawlEngine.enableTask(t.caseId, t.taskId)));
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Assert
      expect(bridge.mappings.size).toBe(10);
    });

    it('should handle concurrent Nitro completions', async () => {
      // Arrange
      bridge.start();
      const completeSpy = vi.fn();
      yawlEngine.on('task:completed', completeSpy);

      // Act
      await Promise.all([
        yawlEngine.enableTask('case-019', 'task-110'),
        yawlEngine.enableTask('case-019', 'task-111'),
        yawlEngine.enableTask('case-019', 'task-112'),
      ]);
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Assert
      expect(completeSpy).toHaveBeenCalledTimes(3);
    });

    it('should maintain data integrity under high concurrency', async () => {
      // Arrange
      bridge.start();

      // Act
      const operations = [];
      for (let i = 0; i < 20; i++) {
        operations.push(yawlEngine.enableTask('case-020', `task-${200 + i}`));
      }
      await Promise.all(operations);
      await new Promise((resolve) => setTimeout(resolve, 200));

      // Assert
      expect(bridge.mappings.size).toBe(20);
      expect(bridge.reverseMappings.size).toBe(20);

      // Verify bidirectional consistency
      for (const [yawlKey, nitroId] of bridge.mappings.entries()) {
        expect(bridge.reverseMappings.get(nitroId)).toBe(yawlKey);
      }
    });
  });
});
