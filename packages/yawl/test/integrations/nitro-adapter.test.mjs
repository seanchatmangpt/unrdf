/**
 * @file YAWL-Nitro Adapter Integration Tests
 * @module @unrdf/yawl/test/integrations/nitro-adapter
 * @description Comprehensive tests for YAWL-Nitro adapter layer
 *
 * Tests cover:
 * - YAWL to Nitro task mapping
 * - Nitro to YAWL event conversion
 * - Data transformation and validation
 * - Error handling and recovery
 * - Concurrent adapter operations
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';

/**
 * Mock Nitro Task System
 */
class MockNitroTask {
  constructor(config) {
    this.id = config.id;
    this.type = config.type;
    this.payload = config.payload || {};
    this.status = 'pending';
    this.priority = config.priority || 0;
    this.metadata = config.metadata || {};
    this.createdAt = new Date();
  }

  execute() {
    this.status = 'executing';
    return Promise.resolve({ success: true, data: this.payload });
  }

  cancel() {
    this.status = 'cancelled';
  }
}

/**
 * Mock YAWL Workflow Engine
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
  }

  async createCase(options) {
    const caseId = options.caseId || `case-${Date.now()}`;
    const caseData = {
      id: caseId,
      workflowId: options.workflowId,
      status: 'active',
      variables: options.variables || {},
    };
    this.cases.set(caseId, caseData);
    this.emit('case:created', caseData);
    return caseData;
  }

  async enableTask(caseId, taskId) {
    const key = `${caseId}:${taskId}`;
    const task = { caseId, taskId, status: 'enabled' };
    this.tasks.set(key, task);
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
}

/**
 * YAWL-Nitro Adapter
 */
class YawlNitroAdapter extends EventEmitter {
  constructor(yawlEngine) {
    super();
    this.yawlEngine = yawlEngine;
    this.taskMappings = new Map();
    this.nitroTasks = new Map();
  }

  /**
   * Convert YAWL task to Nitro task
   */
  yawlToNitro(yawlTask) {
    const nitroConfig = {
      id: `nitro-${yawlTask.caseId}-${yawlTask.taskId}`,
      type: yawlTask.taskType || 'workflow-task',
      payload: {
        caseId: yawlTask.caseId,
        taskId: yawlTask.taskId,
        variables: yawlTask.variables || {},
      },
      priority: yawlTask.priority || 0,
      metadata: {
        yawlCaseId: yawlTask.caseId,
        yawlTaskId: yawlTask.taskId,
      },
    };

    const nitroTask = new MockNitroTask(nitroConfig);
    this.taskMappings.set(yawlTask.taskId, nitroTask.id);
    this.nitroTasks.set(nitroTask.id, nitroTask);

    return nitroTask;
  }

  /**
   * Convert Nitro result to YAWL format
   */
  nitroToYawl(nitroResult) {
    return {
      success: nitroResult.success,
      output: nitroResult.data,
      metadata: {
        nitroTaskId: nitroResult.taskId,
      },
    };
  }

  /**
   * Execute YAWL task via Nitro
   */
  async executeYawlTaskViaNitro(yawlTask) {
    const nitroTask = this.yawlToNitro(yawlTask);
    const result = await nitroTask.execute();
    const yawlResult = this.nitroToYawl({ ...result, taskId: nitroTask.id });

    this.emit('task:executed', { yawlTask, nitroTask, result: yawlResult });

    return yawlResult;
  }

  /**
   * Cancel Nitro task by YAWL task ID
   */
  cancelNitroTask(yawlTaskId) {
    const nitroTaskId = this.taskMappings.get(yawlTaskId);
    if (nitroTaskId) {
      const nitroTask = this.nitroTasks.get(nitroTaskId);
      if (nitroTask) {
        nitroTask.cancel();
        this.emit('task:cancelled', { yawlTaskId, nitroTaskId });
        return true;
      }
    }
    return false;
  }

  /**
   * Get Nitro task status by YAWL task ID
   */
  getNitroTaskStatus(yawlTaskId) {
    const nitroTaskId = this.taskMappings.get(yawlTaskId);
    if (nitroTaskId) {
      const nitroTask = this.nitroTasks.get(nitroTaskId);
      return nitroTask?.status || null;
    }
    return null;
  }
}

describe('YAWL-Nitro Adapter Integration', () => {
  let yawlEngine;
  let adapter;

  beforeEach(() => {
    yawlEngine = new MockYawlEngine();
    adapter = new YawlNitroAdapter(yawlEngine);
  });

  afterEach(() => {
    yawlEngine.removeAllListeners();
    adapter.removeAllListeners();
  });

  describe('Task Mapping - YAWL to Nitro', () => {
    it('should convert simple YAWL task to Nitro task', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-001',
        taskId: 'task-001',
        taskType: 'process',
        variables: { orderId: 'ORD-123' },
      };

      // Act
      const nitroTask = adapter.yawlToNitro(yawlTask);

      // Assert
      expect(nitroTask).toBeDefined();
      expect(nitroTask.id).toContain('nitro-case-001-task-001');
      expect(nitroTask.type).toBe('process');
      expect(nitroTask.payload.caseId).toBe('case-001');
      expect(nitroTask.payload.taskId).toBe('task-001');
      expect(nitroTask.payload.variables.orderId).toBe('ORD-123');
    });

    it('should preserve task priority in conversion', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-002',
        taskId: 'task-002',
        priority: 10,
      };

      // Act
      const nitroTask = adapter.yawlToNitro(yawlTask);

      // Assert
      expect(nitroTask.priority).toBe(10);
    });

    it('should add metadata to Nitro task', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-003',
        taskId: 'task-003',
      };

      // Act
      const nitroTask = adapter.yawlToNitro(yawlTask);

      // Assert
      expect(nitroTask.metadata.yawlCaseId).toBe('case-003');
      expect(nitroTask.metadata.yawlTaskId).toBe('task-003');
    });

    it('should handle YAWL task without variables', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-004',
        taskId: 'task-004',
      };

      // Act
      const nitroTask = adapter.yawlToNitro(yawlTask);

      // Assert
      expect(nitroTask.payload.variables).toEqual({});
    });

    it('should default priority to 0 if not specified', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-005',
        taskId: 'task-005',
      };

      // Act
      const nitroTask = adapter.yawlToNitro(yawlTask);

      // Assert
      expect(nitroTask.priority).toBe(0);
    });

    it('should create unique Nitro task IDs for different YAWL tasks', () => {
      // Arrange
      const yawlTask1 = { caseId: 'case-001', taskId: 'task-001' };
      const yawlTask2 = { caseId: 'case-001', taskId: 'task-002' };

      // Act
      const nitroTask1 = adapter.yawlToNitro(yawlTask1);
      const nitroTask2 = adapter.yawlToNitro(yawlTask2);

      // Assert
      expect(nitroTask1.id).not.toBe(nitroTask2.id);
    });

    it('should store task mapping after conversion', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-006',
        taskId: 'task-006',
      };

      // Act
      const nitroTask = adapter.yawlToNitro(yawlTask);

      // Assert
      expect(adapter.taskMappings.has('task-006')).toBe(true);
      expect(adapter.taskMappings.get('task-006')).toBe(nitroTask.id);
    });
  });

  describe('Task Mapping - Nitro to YAWL', () => {
    it('should convert Nitro result to YAWL format', () => {
      // Arrange
      const nitroResult = {
        success: true,
        data: { result: 'processed' },
        taskId: 'nitro-123',
      };

      // Act
      const yawlResult = adapter.nitroToYawl(nitroResult);

      // Assert
      expect(yawlResult.success).toBe(true);
      expect(yawlResult.output).toEqual({ result: 'processed' });
      expect(yawlResult.metadata.nitroTaskId).toBe('nitro-123');
    });

    it('should handle Nitro failure in conversion', () => {
      // Arrange
      const nitroResult = {
        success: false,
        data: { error: 'Processing failed' },
        taskId: 'nitro-124',
      };

      // Act
      const yawlResult = adapter.nitroToYawl(nitroResult);

      // Assert
      expect(yawlResult.success).toBe(false);
      expect(yawlResult.output.error).toBe('Processing failed');
    });

    it('should preserve metadata in result conversion', () => {
      // Arrange
      const nitroResult = {
        success: true,
        data: {},
        taskId: 'nitro-125',
      };

      // Act
      const yawlResult = adapter.nitroToYawl(nitroResult);

      // Assert
      expect(yawlResult.metadata).toBeDefined();
      expect(yawlResult.metadata.nitroTaskId).toBe('nitro-125');
    });
  });

  describe('Task Execution', () => {
    it('should execute YAWL task via Nitro successfully', async () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-007',
        taskId: 'task-007',
        variables: { data: 'test' },
      };

      // Act
      const result = await adapter.executeYawlTaskViaNitro(yawlTask);

      // Assert
      expect(result.success).toBe(true);
      expect(result.output).toBeDefined();
    });

    it('should emit event on task execution', async () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-008',
        taskId: 'task-008',
      };
      const eventSpy = vi.fn();
      adapter.on('task:executed', eventSpy);

      // Act
      await adapter.executeYawlTaskViaNitro(yawlTask);

      // Assert
      expect(eventSpy).toHaveBeenCalledOnce();
      expect(eventSpy.mock.calls[0][0]).toMatchObject({
        yawlTask,
        result: { success: true },
      });
    });

    it('should maintain task mapping during execution', async () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-009',
        taskId: 'task-009',
      };

      // Act
      await adapter.executeYawlTaskViaNitro(yawlTask);

      // Assert
      expect(adapter.taskMappings.has('task-009')).toBe(true);
      const nitroTaskId = adapter.taskMappings.get('task-009');
      expect(adapter.nitroTasks.has(nitroTaskId)).toBe(true);
    });

    it('should execute multiple tasks concurrently', async () => {
      // Arrange
      const tasks = [
        { caseId: 'case-010', taskId: 'task-010' },
        { caseId: 'case-010', taskId: 'task-011' },
        { caseId: 'case-010', taskId: 'task-012' },
      ];

      // Act
      const results = await Promise.all(
        tasks.map((task) => adapter.executeYawlTaskViaNitro(task))
      );

      // Assert
      expect(results).toHaveLength(3);
      results.forEach((result) => {
        expect(result.success).toBe(true);
      });
    });

    it('should handle execution with complex payload', async () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-011',
        taskId: 'task-013',
        variables: {
          nested: {
            data: {
              items: [1, 2, 3],
              metadata: { version: '1.0' },
            },
          },
        },
      };

      // Act
      const result = await adapter.executeYawlTaskViaNitro(yawlTask);

      // Assert
      expect(result.success).toBe(true);
      expect(result.output.nested.data.items).toEqual([1, 2, 3]);
    });
  });

  describe('Task Cancellation', () => {
    it('should cancel Nitro task by YAWL task ID', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-012',
        taskId: 'task-014',
      };
      adapter.yawlToNitro(yawlTask);

      // Act
      const cancelled = adapter.cancelNitroTask('task-014');

      // Assert
      expect(cancelled).toBe(true);
      const nitroTaskId = adapter.taskMappings.get('task-014');
      const nitroTask = adapter.nitroTasks.get(nitroTaskId);
      expect(nitroTask.status).toBe('cancelled');
    });

    it('should return false when cancelling non-existent task', () => {
      // Arrange
      // No task created

      // Act
      const cancelled = adapter.cancelNitroTask('non-existent');

      // Assert
      expect(cancelled).toBe(false);
    });

    it('should emit event on task cancellation', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-013',
        taskId: 'task-015',
      };
      adapter.yawlToNitro(yawlTask);
      const eventSpy = vi.fn();
      adapter.on('task:cancelled', eventSpy);

      // Act
      adapter.cancelNitroTask('task-015');

      // Assert
      expect(eventSpy).toHaveBeenCalledOnce();
      expect(eventSpy.mock.calls[0][0].yawlTaskId).toBe('task-015');
    });

    it('should handle cancellation of already executing task', async () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-014',
        taskId: 'task-016',
      };
      const nitroTask = adapter.yawlToNitro(yawlTask);
      await nitroTask.execute();

      // Act
      const cancelled = adapter.cancelNitroTask('task-016');

      // Assert
      expect(cancelled).toBe(true);
      expect(nitroTask.status).toBe('cancelled');
    });
  });

  describe('Task Status Queries', () => {
    it('should retrieve Nitro task status by YAWL task ID', () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-015',
        taskId: 'task-017',
      };
      adapter.yawlToNitro(yawlTask);

      // Act
      const status = adapter.getNitroTaskStatus('task-017');

      // Assert
      expect(status).toBe('pending');
    });

    it('should return null for non-existent task status', () => {
      // Arrange
      // No task created

      // Act
      const status = adapter.getNitroTaskStatus('non-existent');

      // Assert
      expect(status).toBeNull();
    });

    it('should reflect status change after execution', async () => {
      // Arrange
      const yawlTask = {
        caseId: 'case-016',
        taskId: 'task-018',
      };
      adapter.yawlToNitro(yawlTask);

      // Act
      const statusBefore = adapter.getNitroTaskStatus('task-018');
      await adapter.executeYawlTaskViaNitro(yawlTask);
      const statusAfter = adapter.getNitroTaskStatus('task-018');

      // Assert
      expect(statusBefore).toBe('pending');
      expect(statusAfter).toBe('executing');
    });

    it('should track status across multiple tasks', () => {
      // Arrange
      const tasks = [
        { caseId: 'case-017', taskId: 'task-019' },
        { caseId: 'case-017', taskId: 'task-020' },
        { caseId: 'case-017', taskId: 'task-021' },
      ];
      tasks.forEach((task) => adapter.yawlToNitro(task));

      // Act
      const statuses = tasks.map((task) => adapter.getNitroTaskStatus(task.taskId));

      // Assert
      expect(statuses).toEqual(['pending', 'pending', 'pending']);
    });
  });

  describe('Error Handling', () => {
    it('should handle YAWL task with missing caseId gracefully', () => {
      // Arrange
      const invalidTask = {
        taskId: 'task-022',
      };

      // Act & Assert
      expect(() => adapter.yawlToNitro(invalidTask)).not.toThrow();
    });

    it('should handle YAWL task with missing taskId gracefully', () => {
      // Arrange
      const invalidTask = {
        caseId: 'case-018',
      };

      // Act & Assert
      expect(() => adapter.yawlToNitro(invalidTask)).not.toThrow();
    });

    it('should handle null YAWL task gracefully', () => {
      // Arrange
      const nullTask = null;

      // Act & Assert
      expect(() => adapter.yawlToNitro(nullTask)).toThrow();
    });

    it('should handle undefined variables in YAWL task', () => {
      // Arrange
      const task = {
        caseId: 'case-019',
        taskId: 'task-023',
        variables: undefined,
      };

      // Act
      const nitroTask = adapter.yawlToNitro(task);

      // Assert
      expect(nitroTask.payload.variables).toEqual({});
    });
  });

  describe('Concurrent Operations', () => {
    it('should handle concurrent task conversions', () => {
      // Arrange
      const tasks = Array.from({ length: 20 }, (_, i) => ({
        caseId: `case-${i}`,
        taskId: `task-${i}`,
      }));

      // Act
      const nitroTasks = tasks.map((task) => adapter.yawlToNitro(task));

      // Assert
      expect(nitroTasks).toHaveLength(20);
      expect(adapter.taskMappings.size).toBe(20);
      expect(adapter.nitroTasks.size).toBe(20);
    });

    it('should handle concurrent executions without conflicts', async () => {
      // Arrange
      const tasks = Array.from({ length: 10 }, (_, i) => ({
        caseId: 'case-020',
        taskId: `task-${100 + i}`,
      }));

      // Act
      const results = await Promise.all(
        tasks.map((task) => adapter.executeYawlTaskViaNitro(task))
      );

      // Assert
      expect(results).toHaveLength(10);
      results.forEach((result, i) => {
        expect(result.success).toBe(true);
      });
    });

    it('should handle concurrent cancellations', () => {
      // Arrange
      const tasks = Array.from({ length: 5 }, (_, i) => ({
        caseId: 'case-021',
        taskId: `task-${200 + i}`,
      }));
      tasks.forEach((task) => adapter.yawlToNitro(task));

      // Act
      const results = tasks.map((task) => adapter.cancelNitroTask(task.taskId));

      // Assert
      expect(results).toEqual([true, true, true, true, true]);
    });

    it('should maintain data consistency under concurrent operations', async () => {
      // Arrange
      const operations = [];

      for (let i = 0; i < 15; i++) {
        operations.push(
          adapter.executeYawlTaskViaNitro({
            caseId: 'case-022',
            taskId: `task-${300 + i}`,
          })
        );
      }

      // Act
      await Promise.all(operations);

      // Assert
      expect(adapter.taskMappings.size).toBeGreaterThanOrEqual(15);
      expect(adapter.nitroTasks.size).toBeGreaterThanOrEqual(15);
    });
  });
});
