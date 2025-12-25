/**
 * @file YAWL Cancellation Tests
 * @description Tests for YAWL cancellation semantics
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  YawlCancellationManager,
  createCancellationManager,
  TaskCircuitBreaker,
  CancellationRegionManager,
  CancellationReceiptLogger,
} from '../src/cancellation/index.mjs';

describe('YawlCancellationManager', () => {
  let manager;

  beforeEach(() => {
    manager = createCancellationManager({
      defaultTimeout: 1000,
      circuitBreakerThreshold: 3,
    });
  });

  describe('Work Item Lifecycle', () => {
    it('should create a work item', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      expect(workItem.id).toBeDefined();
      expect(workItem.taskId).toBe('task-1');
      expect(workItem.state).toBe('pending');
      expect(workItem.timeoutMs).toBe(1000);
    });

    it('should enable a work item', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      const enabled = manager.enableWorkItem(workItem.id);
      expect(enabled.state).toBe('enabled');
    });

    it('should start execution with timeout enforcement', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.enableWorkItem(workItem.id);
      const executing = manager.startExecution(workItem.id);

      expect(executing.state).toBe('executing');
      expect(executing.startedAt).toBeDefined();
    });

    it('should complete a work item', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.enableWorkItem(workItem.id);
      manager.startExecution(workItem.id);
      const completed = manager.completeWorkItem(workItem.id);

      expect(completed.state).toBe('completed');
      expect(completed.completedAt).toBeDefined();
    });
  });

  describe('Cancellation', () => {
    it('should cancel a work item with reason', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.enableWorkItem(workItem.id);
      const result = manager.cancelWorkItem(workItem.id, 'manual');

      expect(result.success).toBe(true);
      expect(result.cancelled).toContain(workItem.id);

      const cancelled = manager.getWorkItem(workItem.id);
      expect(cancelled.state).toBe('cancelled');
      expect(cancelled.cancellationReason).toBe('manual');
    });

    it('should log cancellation receipt', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.cancelWorkItem(workItem.id, 'manual');

      const receipts = manager.receiptLogger.getReceiptsForWorkItem(workItem.id);
      expect(receipts.length).toBeGreaterThan(0);
      expect(receipts[0].type).toBe('CANCELLED_WORK_ITEM');
      expect(receipts[0].payload.reason).toBe('manual');
    });

    it('should invoke cancellation callback', () => {
      const onCancellation = vi.fn();
      const mgr = createCancellationManager({ onCancellation });

      const workItem = mgr.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      mgr.cancelWorkItem(workItem.id, 'manual');

      expect(onCancellation).toHaveBeenCalledWith(
        expect.objectContaining({
          workItemId: workItem.id,
          taskId: 'task-1',
          reason: 'manual',
        })
      );
    });
  });

  describe('Cancellation Regions', () => {
    it('should create a cancellation region', () => {
      const region = manager.regionManager.createRegion({
        name: 'approval-region',
        taskIds: ['task-1', 'task-2', 'task-3'],
      });

      expect(region.id).toBeDefined();
      expect(region.taskIds).toEqual(['task-1', 'task-2', 'task-3']);
      expect(region.active).toBe(true);
    });

    it('should cancel all siblings in region', () => {
      const region = manager.regionManager.createRegion({
        name: 'approval-region',
        taskIds: ['task-1', 'task-2'],
      });

      const wi1 = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
        regionId: region.id,
      });

      const wi2 = manager.createWorkItem({
        taskId: 'task-2',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
        regionId: region.id,
      });

      manager.enableWorkItem(wi1.id);
      manager.enableWorkItem(wi2.id);

      const result = manager.cancelWorkItem(wi1.id, 'manual');

      expect(result.cancelled).toContain(wi1.id);
      expect(result.cancelled).toContain(wi2.id);

      const wi2State = manager.getWorkItem(wi2.id);
      expect(wi2State.state).toBe('cancelled');
      expect(wi2State.cancellationReason).toBe('region_cancelled');
    });

    it('should support nested regions', () => {
      const parentRegion = manager.regionManager.createRegion({
        name: 'parent-region',
        taskIds: ['task-1'],
      });

      const childRegion = manager.regionManager.createRegion({
        name: 'child-region',
        taskIds: ['task-2', 'task-3'],
        parentRegionId: parentRegion.id,
      });

      const descendants = manager.regionManager.getDescendantRegions(parentRegion.id);
      expect(descendants.length).toBe(1);
      expect(descendants[0].id).toBe(childRegion.id);
    });
  });

  describe('Timeout Enforcement', () => {
    it('should auto-cancel on timeout', async () => {
      const mgr = createCancellationManager({
        defaultTimeout: 50, // 50ms timeout for test
      });

      const workItem = mgr.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      mgr.enableWorkItem(workItem.id);
      mgr.startExecution(workItem.id);

      // Wait for timeout
      await new Promise(resolve => setTimeout(resolve, 100));

      const wi = mgr.getWorkItem(workItem.id);
      expect(wi.state).toBe('cancelled');
      expect(wi.cancellationReason).toBe('timeout');
    });

    it('should log timeout receipt', async () => {
      const mgr = createCancellationManager({
        defaultTimeout: 50,
      });

      const workItem = mgr.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      mgr.enableWorkItem(workItem.id);
      mgr.startExecution(workItem.id);

      await new Promise(resolve => setTimeout(resolve, 100));

      const receipts = mgr.receiptLogger.getAllReceipts();
      const timeoutReceipt = receipts.find(r => r.type === 'TIMEOUT_OCCURRED');
      expect(timeoutReceipt).toBeDefined();
      expect(timeoutReceipt.payload.taskId).toBe('task-1');
    });

    it('should invoke timeout callback', async () => {
      const onTimeout = vi.fn();
      const mgr = createCancellationManager({
        defaultTimeout: 50,
        onTimeout,
      });

      const workItem = mgr.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      mgr.enableWorkItem(workItem.id);
      mgr.startExecution(workItem.id);

      await new Promise(resolve => setTimeout(resolve, 100));

      expect(onTimeout).toHaveBeenCalled();
    });

    it('should respect max timeout limit', () => {
      const workItem = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
        timeoutMs: 500000, // Exceeds 300000 max
      });

      expect(workItem.timeoutMs).toBe(300000);
    });
  });

  describe('Circuit Breaker Integration', () => {
    it('should open circuit after consecutive failures', () => {
      const workItem1 = manager.createWorkItem({
        taskId: 'flaky-task',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });
      manager.enableWorkItem(workItem1.id);
      manager.startExecution(workItem1.id);
      manager.recordFailure(workItem1.id);

      const workItem2 = manager.createWorkItem({
        taskId: 'flaky-task',
        caseId: '550e8400-e29b-41d4-a716-446655440001',
      });
      manager.enableWorkItem(workItem2.id);
      manager.startExecution(workItem2.id);
      manager.recordFailure(workItem2.id);

      const workItem3 = manager.createWorkItem({
        taskId: 'flaky-task',
        caseId: '550e8400-e29b-41d4-a716-446655440002',
      });
      manager.enableWorkItem(workItem3.id);
      manager.startExecution(workItem3.id);
      const result = manager.recordFailure(workItem3.id);

      expect(result.circuitOpened).toBe(true);
      expect(manager.isTaskEnabled('flaky-task')).toBe(false);
    });

    it('should log circuit breaker open receipt', () => {
      // Trigger 3 failures
      for (let i = 0; i < 3; i++) {
        const wi = manager.createWorkItem({
          taskId: 'flaky-task',
          caseId: `550e8400-e29b-41d4-a716-44665544000${i}`,
        });
        manager.enableWorkItem(wi.id);
        manager.startExecution(wi.id);
        manager.recordFailure(wi.id);
      }

      const receipts = manager.receiptLogger.getReceiptsForTask('flaky-task');
      const circuitReceipt = receipts.find(r => r.type === 'CIRCUIT_BREAKER_OPEN');
      expect(circuitReceipt).toBeDefined();
      expect(circuitReceipt.payload.failureCount).toBe(3);
    });

    it('should prevent work item enable when circuit is open', () => {
      // Open circuit
      for (let i = 0; i < 3; i++) {
        const wi = manager.createWorkItem({
          taskId: 'flaky-task',
          caseId: `550e8400-e29b-41d4-a716-44665544000${i}`,
        });
        manager.enableWorkItem(wi.id);
        manager.startExecution(wi.id);
        manager.recordFailure(wi.id);
      }

      // Try to enable new work item
      const newWi = manager.createWorkItem({
        taskId: 'flaky-task',
        caseId: '550e8400-e29b-41d4-a716-446655440009',
      });

      const result = manager.enableWorkItem(newWi.id);
      expect(result).toBe(null); // Should fail to enable

      const wi = manager.getWorkItem(newWi.id);
      expect(wi.state).toBe('cancelled');
      expect(wi.cancellationReason).toBe('task_disabled');
    });

    it('should allow manual task re-enable', () => {
      // Open circuit
      for (let i = 0; i < 3; i++) {
        const wi = manager.createWorkItem({
          taskId: 'flaky-task',
          caseId: `550e8400-e29b-41d4-a716-44665544000${i}`,
        });
        manager.enableWorkItem(wi.id);
        manager.startExecution(wi.id);
        manager.recordFailure(wi.id);
      }

      expect(manager.isTaskEnabled('flaky-task')).toBe(false);

      manager.enableTask('flaky-task');

      expect(manager.isTaskEnabled('flaky-task')).toBe(true);
    });
  });

  describe('Cancellation Propagation', () => {
    it('should propagate cancellation to dependent tasks', () => {
      manager.setTaskDependencies('approval', ['notification', 'logging']);

      const approvalWi = manager.createWorkItem({
        taskId: 'approval',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      const notificationWi = manager.createWorkItem({
        taskId: 'notification',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      const loggingWi = manager.createWorkItem({
        taskId: 'logging',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.enableWorkItem(approvalWi.id);
      manager.enableWorkItem(notificationWi.id);
      manager.enableWorkItem(loggingWi.id);

      const result = manager.cancelWorkItem(approvalWi.id, 'manual');

      expect(result.cancelled).toContain(notificationWi.id);
      expect(result.cancelled).toContain(loggingWi.id);

      const notification = manager.getWorkItem(notificationWi.id);
      expect(notification.cancellationReason).toBe('dependency_failed');
    });

    it('should log propagation receipt', () => {
      manager.setTaskDependencies('approval', ['notification']);

      const approvalWi = manager.createWorkItem({
        taskId: 'approval',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      const notificationWi = manager.createWorkItem({
        taskId: 'notification',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.enableWorkItem(notificationWi.id);
      manager.cancelWorkItem(approvalWi.id, 'manual');

      const receipts = manager.receiptLogger.getAllReceipts();
      const propagationReceipt = receipts.find(r => r.type === 'CANCELLATION_PROPAGATED');
      expect(propagationReceipt).toBeDefined();
      expect(propagationReceipt.payload.propagatedTo).toContain(notificationWi.id);
    });
  });

  describe('Time Travel', () => {
    it('should reconstruct state at a specific time', async () => {
      const wi1 = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.cancelWorkItem(wi1.id, 'manual');
      const afterFirstCancel = new Date();

      await new Promise(resolve => setTimeout(resolve, 10));

      const wi2 = manager.createWorkItem({
        taskId: 'task-2',
        caseId: '550e8400-e29b-41d4-a716-446655440001',
      });
      manager.cancelWorkItem(wi2.id, 'manual');

      const stateAtFirstCancel = manager.getStateAtTime(afterFirstCancel);
      expect(stateAtFirstCancel.cancelledWorkItems).toContain(wi1.id);
      expect(stateAtFirstCancel.cancelledWorkItems).not.toContain(wi2.id);
    });

    it('should get receipts in time range', async () => {
      const start = new Date();
      await new Promise(resolve => setTimeout(resolve, 5));

      const wi = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });
      manager.cancelWorkItem(wi.id, 'manual');

      await new Promise(resolve => setTimeout(resolve, 5));
      const end = new Date();

      const receipts = manager.receiptLogger.getReceiptsInRange(start, end);
      expect(receipts.length).toBeGreaterThan(0);
    });
  });

  describe('Statistics', () => {
    it('should provide comprehensive stats', () => {
      manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.regionManager.createRegion({
        name: 'region-1',
        taskIds: ['task-1'],
      });

      const stats = manager.getStats();

      expect(stats.workItems.total).toBe(1);
      expect(stats.regions.total).toBe(1);
    });
  });

  describe('Cleanup', () => {
    it('should terminate all pending work', () => {
      const wi1 = manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });
      const wi2 = manager.createWorkItem({
        taskId: 'task-2',
        caseId: '550e8400-e29b-41d4-a716-446655440001',
      });

      manager.enableWorkItem(wi1.id);
      manager.enableWorkItem(wi2.id);
      manager.startExecution(wi1.id);

      manager.terminate('workflow_terminated');

      expect(manager.getWorkItem(wi1.id).state).toBe('cancelled');
      expect(manager.getWorkItem(wi2.id).state).toBe('cancelled');
    });

    it('should clear all state', () => {
      manager.createWorkItem({
        taskId: 'task-1',
        caseId: '550e8400-e29b-41d4-a716-446655440000',
      });

      manager.clear();

      expect(manager.workItems.size).toBe(0);
      expect(manager.receiptLogger.receipts.length).toBe(0);
    });
  });
});

describe('TaskCircuitBreaker', () => {
  it('should start in closed state', () => {
    const breaker = new TaskCircuitBreaker({ taskId: 'test' });
    expect(breaker.getState().state).toBe('closed');
  });

  it('should open after failure threshold', () => {
    const breaker = new TaskCircuitBreaker({
      taskId: 'test',
      failureThreshold: 2,
    });

    breaker.recordFailure();
    expect(breaker.getState().state).toBe('closed');

    breaker.recordFailure();
    expect(breaker.getState().state).toBe('open');
  });

  it('should reset failure count on success', () => {
    const breaker = new TaskCircuitBreaker({
      taskId: 'test',
      failureThreshold: 3,
    });

    breaker.recordFailure();
    breaker.recordFailure();
    breaker.recordSuccess();

    expect(breaker.getState().failureCount).toBe(0);
  });

  it('should block execution when open', () => {
    const breaker = new TaskCircuitBreaker({
      taskId: 'test',
      failureThreshold: 1,
    });

    breaker.recordFailure();
    expect(breaker.allowExecution()).toBe(false);
  });

  it('should allow manual reset', () => {
    const breaker = new TaskCircuitBreaker({
      taskId: 'test',
      failureThreshold: 1,
    });

    breaker.recordFailure();
    expect(breaker.allowExecution()).toBe(false);

    breaker.reset();
    expect(breaker.allowExecution()).toBe(true);
  });
});

describe('CancellationRegionManager', () => {
  let regionManager;

  beforeEach(() => {
    regionManager = new CancellationRegionManager();
  });

  it('should create nested regions', () => {
    const parent = regionManager.createRegion({
      name: 'parent',
      taskIds: ['p1', 'p2'],
    });

    const child1 = regionManager.createRegion({
      name: 'child1',
      taskIds: ['c1', 'c2'],
      parentRegionId: parent.id,
    });

    const child2 = regionManager.createRegion({
      name: 'child2',
      taskIds: ['c3'],
      parentRegionId: parent.id,
    });

    expect(parent.childRegionIds).toContain(child1.id);
    expect(parent.childRegionIds).toContain(child2.id);
  });

  it('should get all tasks in region including nested', () => {
    const parent = regionManager.createRegion({
      name: 'parent',
      taskIds: ['p1'],
    });

    regionManager.createRegion({
      name: 'child',
      taskIds: ['c1', 'c2'],
      parentRegionId: parent.id,
    });

    const allTasks = regionManager.getAllTasksInRegion(parent.id);
    expect(allTasks).toContain('p1');
    expect(allTasks).toContain('c1');
    expect(allTasks).toContain('c2');
  });

  it('should get sibling tasks', () => {
    regionManager.createRegion({
      name: 'region',
      taskIds: ['a', 'b', 'c'],
    });

    const siblings = regionManager.getSiblingTasks('a');
    expect(siblings).toContain('b');
    expect(siblings).toContain('c');
    expect(siblings).not.toContain('a');
  });

  it('should deactivate region and descendants', () => {
    const parent = regionManager.createRegion({
      name: 'parent',
      taskIds: ['p1'],
    });

    const child = regionManager.createRegion({
      name: 'child',
      taskIds: ['c1'],
      parentRegionId: parent.id,
    });

    regionManager.deactivateRegion(parent.id);

    expect(regionManager.getRegion(parent.id).active).toBe(false);
    expect(regionManager.getRegion(child.id).active).toBe(false);
  });

  it('should export and import', () => {
    regionManager.createRegion({
      name: 'test-region',
      taskIds: ['t1', 't2'],
    });

    const exported = regionManager.export();
    expect(exported.regions.length).toBe(1);

    const newManager = new CancellationRegionManager();
    newManager.import(exported);

    expect(newManager.regions.size).toBe(1);
  });
});

describe('CancellationReceiptLogger', () => {
  let logger;

  beforeEach(() => {
    logger = new CancellationReceiptLogger();
  });

  it('should log and retrieve receipts', () => {
    logger.logCancelledWorkItem('wi-1', 'manual', 'region-1');

    const receipts = logger.getReceiptsForWorkItem('wi-1');
    expect(receipts.length).toBe(1);
    expect(receipts[0].type).toBe('CANCELLED_WORK_ITEM');
  });

  it('should log timeout receipts', () => {
    logger.logTimeoutOccurred('wi-1', 'task-1', 35000, 30000);

    const receipts = logger.getReceiptsForTask('task-1');
    expect(receipts.length).toBe(1);
    expect(receipts[0].payload.durationMs).toBe(35000);
    expect(receipts[0].payload.timeoutMs).toBe(30000);
  });

  it('should log circuit breaker events', () => {
    logger.logCircuitBreakerOpen('task-1', 3);
    logger.logCircuitBreakerClosed('task-1');

    const receipts = logger.getReceiptsForTask('task-1');
    expect(receipts.length).toBe(2);
    expect(receipts[0].type).toBe('CIRCUIT_BREAKER_OPEN');
    expect(receipts[1].type).toBe('CIRCUIT_BREAKER_CLOSED');
  });

  it('should export and import receipts', () => {
    logger.logCancelledWorkItem('wi-1', 'manual');
    logger.logTimeoutOccurred('wi-2', 'task-1', 5000, 3000);

    const exported = logger.export();
    expect(exported.length).toBe(2);

    const newLogger = new CancellationReceiptLogger();
    newLogger.import(exported);

    expect(newLogger.getAllReceipts().length).toBe(2);
  });
});
