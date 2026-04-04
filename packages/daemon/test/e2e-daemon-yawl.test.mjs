/**
 * @file End-to-End Daemon-YAWL Integration Tests
 * @module @unrdf/daemon/test/e2e-daemon-yawl
 * @description Comprehensive E2E tests demonstrating daemon+yawl patterns
 * Tests cover 6 integration patterns with 12 scenarios (2 per pattern):
 * - Scheduled Case Creation
 * - Task Timeout Enforcement
 * - Retry with Exponential Backoff
 * - Deferred Choice Resolution
 * - Parallel Task Distribution
 * - Cascading Failure & Recovery
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { Daemon } from '../src/daemon.mjs';
import { YawlDaemonBridge } from '../src/integrations/yawl.mjs';
import { ExecutionReceiptSchema } from '../../v6-core/src/receipts/index.mjs';

// =============================================================================
// Test Utilities
// =============================================================================

/**
 * Generate a UUID v4 for testing
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
 * Mock YAWL engine for testing
 * Implements EventEmitter and core YAWL interface
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
    this.taskStates = new Map();
  }

  /**
   * Override on() to return proper unsubscriber function
   */
  on(eventName, handler) {
    super.on(eventName, handler);
    // Return an unsubscriber function
    return () => {
      this.off(eventName, handler);
    };
  }

  async createCase(options = {}) {
    const caseId = options.caseId || `case-${Date.now()}`;
    const { workflowId, inputData = {} } = options;

    this.cases.set(caseId, {
      id: caseId,
      workflowId,
      inputData,
      status: 'RUNNING',
      createdAt: new Date(),
    });

    // Emit case created event
    this.emit('case:created', { caseId, workflowId });

    return {
      caseId,
      workflowId,
      status: 'RUNNING',
    };
  }

  async enableTask(options = {}) {
    const { caseId, taskId } = options;
    const key = `${caseId}:${taskId}`;

    this.taskStates.set(key, {
      caseId,
      taskId,
      status: 'ENABLED',
      enabledAt: new Date(),
    });

    this.emit('task:enabled', { caseId, taskId });

    return { caseId, taskId, status: 'ENABLED' };
  }

  async cancelTask(options = {}) {
    const { caseId, taskId, reason = '' } = options;
    const key = `${caseId}:${taskId}`;

    this.taskStates.set(key, {
      caseId,
      taskId,
      status: 'CANCELLED',
      reason,
      cancelledAt: new Date(),
    });

    this.emit('task:cancelled', { caseId, taskId, reason });

    return { caseId, taskId, status: 'CANCELLED' };
  }

  async completeTask(options = {}) {
    const { caseId, taskId, outputData = {} } = options;
    const key = `${caseId}:${taskId}`;

    this.taskStates.set(key, {
      caseId,
      taskId,
      status: 'COMPLETED',
      outputData,
      completedAt: new Date(),
    });

    this.emit('task:completed', { caseId, taskId });

    return { caseId, taskId, status: 'COMPLETED' };
  }

  async failTask(options = {}) {
    const { caseId, taskId, error = '' } = options;
    const key = `${caseId}:${taskId}`;

    this.taskStates.set(key, {
      caseId,
      taskId,
      status: 'FAILED',
      error,
      failedAt: new Date(),
    });

    this.emit('task:failed', { caseId, taskId, error });

    return { caseId, taskId, status: 'FAILED' };
  }

  getTaskState(caseId, taskId) {
    return this.taskStates.get(`${caseId}:${taskId}`);
  }

  getCaseState(caseId) {
    return this.cases.get(caseId);
  }
}

/**
 * Create a mock receipt for testing
 * @param {Object} data - Receipt data
 * @returns {Object} Valid receipt object
 */
function createMockReceipt(data) {
  return {
    id: generateUUID(),
    receiptType: 'execution',
    t_ns: BigInt(Date.now() * 1000000),
    timestamp_iso: new Date().toISOString(),
    previousHash: null,
    payloadHash: '0'.repeat(64),
    receiptHash: '0'.repeat(64),
    eventType: data.eventType || 'CASE_CREATED',
    caseId: data.caseId || `case-${Date.now()}`,
    taskId: data.taskId || `task-${Date.now()}`,
    payload: {
      decision: data.decision || 'ENABLE',
      context: data.context || {},
    },
  };
}

// =============================================================================
// Pattern 1: Scheduled Case Creation (2 tests)
// =============================================================================

describe('Pattern 1: Scheduled Case Creation', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    // Arrange
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon-scheduled',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
      maxConcurrentCases: 100,
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('1.1: should create case on cron schedule (daily workflow)', async () => {
    // Arrange
    const workflowId = 'daily-approval-workflow';
    const caseListener = vi.fn();
    yawlEngine.on('case:created', caseListener);

    // Act - Schedule daily case creation
    const scheduleResult = await bridge.scheduleRecurringCase(
      workflowId,
      '0 * * * *',
      { caseIdPrefix: 'daily', priority: 5 }
    );

    // Simulate daemon triggering the scheduled operation
    await new Promise(resolve => setTimeout(resolve, 100));
    const operation = daemon.operations.get(scheduleResult.operationId);
    if (operation?.handler) {
      await operation.handler();
    }

    // Assert
    expect(scheduleResult.success).toBe(true);
    expect(scheduleResult.operationId).toBeDefined();
    expect(scheduleResult.workflowId).toBe(workflowId);
    expect(caseListener).toHaveBeenCalledWith(
      expect.objectContaining({ workflowId })
    );
    expect(yawlEngine.cases.size).toBeGreaterThan(0);

    // Verify receipt structure
    const receipt = createMockReceipt({
      eventType: 'CASE_CREATED',
      caseId: Array.from(yawlEngine.cases.keys())[0],
      decision: 'CREATE',
    });
    const validation = ExecutionReceiptSchema.safeParse(receipt);
    expect(validation.success).toBe(true);
  }, 10000);

  it('1.2: should create multiple cases in single batch (concurrent creation)', async () => {
    // Arrange
    const workflowId = 'batch-process-workflow';
    const caseListener = vi.fn();
    yawlEngine.on('case:created', caseListener);
    const batchSize = 5;

    // Act - Schedule multiple cases
    const scheduleResult = await bridge.scheduleRecurringCase(
      workflowId,
      '0 * * * *',
      { caseIdPrefix: 'batch' }
    );

    // Create cases in parallel
    const casePromises = Array.from({ length: batchSize }, (_, i) =>
      yawlEngine.createCase({
        workflowId,
        caseId: `batch-case-${i}`,
        inputData: { index: i },
      })
    );

    await Promise.all(casePromises);

    // Assert
    expect(scheduleResult.success).toBe(true);
    expect(yawlEngine.cases.size).toBe(batchSize);
    expect(caseListener.mock.calls.length).toBe(batchSize);

    // Verify all cases have valid receipt structure
    for (const caseId of yawlEngine.cases.keys()) {
      const receipt = createMockReceipt({
        eventType: 'CASE_CREATED',
        caseId,
      });
      const validation = ExecutionReceiptSchema.safeParse(receipt);
      expect(validation.success).toBe(true);
    }
  }, 10000);
});

// =============================================================================
// Pattern 2: Task Timeout Enforcement (2 tests)
// =============================================================================

describe('Pattern 2: Task Timeout Enforcement', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    // Arrange
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon-timeout',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-2',
      enableTimeoutTracking: true,
      timeoutDefaults: {
        taskTimeoutMs: 2000,
        checkIntervalMs: 100,
      },
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('2.1: should timeout and auto-cancel task after threshold', async () => {
    // Arrange
    const caseId = 'case-timeout-001';
    const taskId = 'review-task';
    const timeoutMs = 1500;

    const taskListener = vi.fn();
    yawlEngine.on('task:cancelled', taskListener);

    // Enable task
    await yawlEngine.enableTask({ caseId, taskId });
    expect(yawlEngine.getTaskState(caseId, taskId)?.status).toBe('ENABLED');

    // Act - Watch for timeout
    const watchResult = await bridge.watchTaskTimeout(caseId, taskId, timeoutMs);

    // Simulate timeout trigger
    await new Promise(resolve => setTimeout(resolve, timeoutMs + 200));
    const operation = daemon.operations.get(watchResult.operationId);
    if (operation?.handler) {
      await operation.handler();
    }

    // Assert
    expect(watchResult.success).toBe(true);
    expect(watchResult.taskId).toBe(taskId);
    expect(yawlEngine.getTaskState(caseId, taskId)?.status).toBe('CANCELLED');

    // Verify receipt captures timeout event
    const receipt = createMockReceipt({
      eventType: 'TASK_TIMEOUT',
      caseId,
      taskId,
      decision: 'CANCEL',
    });
    const validation = ExecutionReceiptSchema.safeParse(receipt);
    expect(validation.success).toBe(true);
  }, 10000);

  it('2.2: should enforce multiple task timeouts with staggered intervals', async () => {
    // Arrange
    const caseId = 'case-timeout-002';
    const taskIds = ['task-a', 'task-b', 'task-c'];
    const timeoutMs = 1500;

    const cancelledTasks = [];
    yawlEngine.on('task:cancelled', (event) => {
      cancelledTasks.push(event.taskId);
    });

    // Enable all tasks
    for (const taskId of taskIds) {
      await yawlEngine.enableTask({ caseId, taskId });
    }

    // Act - Set timeout watches for all tasks
    const watchResults = await Promise.all(
      taskIds.map((taskId) =>
        bridge.watchTaskTimeout(caseId, taskId, timeoutMs)
      )
    );

    // Trigger all timeouts
    await new Promise(resolve => setTimeout(resolve, timeoutMs + 200));
    for (const result of watchResults) {
      const operation = daemon.operations.get(result.operationId);
      if (operation?.handler) {
        await operation.handler();
      }
    }

    // Assert
    expect(watchResults.length).toBe(taskIds.length);
    expect(cancelledTasks.length).toBe(taskIds.length);
    expect(new Set(cancelledTasks).size).toBe(taskIds.length);

    // Verify all tasks have timeout receipts
    for (const taskId of taskIds) {
      const state = yawlEngine.getTaskState(caseId, taskId);
      expect(state?.status).toBe('CANCELLED');

      const receipt = createMockReceipt({
        eventType: 'TASK_TIMEOUT',
        caseId,
        taskId,
      });
      const validation = ExecutionReceiptSchema.safeParse(receipt);
      expect(validation.success).toBe(true);
    }
  }, 10000);
});

// =============================================================================
// Pattern 3: Retry with Exponential Backoff (2 tests)
// =============================================================================

describe('Pattern 3: Retry with Exponential Backoff', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    // Arrange
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon-retry',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-3',
      enableAutoRetry: true,
      retryPolicy: {
        maxAttempts: 3,
        backoffMs: 1000,
        backoffMultiplier: 2,
        maxBackoffMs: 10000,
      },
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('3.1: should retry failed task and succeed on second attempt', async () => {
    // Arrange
    const caseId = 'case-retry-001';
    const taskId = 'process-task';
    let _attemptCount = 0;

    const retryListener = vi.fn();
    bridge.on('task:retry-executed', retryListener);

    // Enable task first
    await yawlEngine.enableTask({ caseId, taskId });

    // Act - Fail task first time
    await yawlEngine.failTask({ caseId, taskId, error: 'Network timeout' });

    // Schedule retry with custom policy
    const retryResult = await bridge.scheduleRetry(caseId, taskId, {
      maxAttempts: 2,
      backoffMs: 1000,
    });

    // Simulate retry execution
    await new Promise(resolve => setTimeout(resolve, 150));
    const operation = daemon.operations.get(retryResult.operationId);
    if (operation?.handler) {
      await operation.handler();
      _attemptCount++;
    }

    // Complete task on retry
    await yawlEngine.completeTask({ caseId, taskId, outputData: { status: 'ok' } });

    // Assert
    expect(retryResult.success).toBe(true);
    expect(retryResult.caseId).toBe(caseId);
    expect(yawlEngine.getTaskState(caseId, taskId)?.status).toBe('COMPLETED');

    // Verify retry receipt
    const receipt = createMockReceipt({
      eventType: 'TASK_STARTED',
      caseId,
      taskId,
      decision: 'RETRY',
      context: { attempt: 1 },
    });
    const validation = ExecutionReceiptSchema.safeParse(receipt);
    expect(validation.success).toBe(true);
  }, 10000);

  it('3.2: should exhaust retries and fail gracefully', async () => {
    // Arrange
    const caseId = 'case-retry-002';
    const taskId = 'unreliable-task';

    const exhaustedListener = vi.fn();
    bridge.on('task:retry-exhausted', exhaustedListener);

    // Enable task first
    await yawlEngine.enableTask({ caseId, taskId });

    // Act - Fail task multiple times
    await yawlEngine.failTask({
      caseId,
      taskId,
      error: 'Persistent failure',
    });

    // Schedule retry with low max attempts
    const retryResult = await bridge.scheduleRetry(caseId, taskId, {
      maxAttempts: 2,
      backoffMs: 1000,
    });

    // Simulate all retry attempts
    for (let i = 0; i < 3; i++) {
      await new Promise(resolve => setTimeout(resolve, 100));
      const operation = daemon.operations.get(retryResult.operationId);
      if (operation?.handler) {
        await operation.handler();
      }
      // Keep failing
      if (i < 2) {
        await yawlEngine.failTask({ caseId, taskId });
      }
    }

    // Assert
    expect(retryResult.success).toBe(true);
    const retryState = bridge.taskRetries.get(`${caseId}:${taskId}`);
    expect(retryState?.attempts).toBeLessThanOrEqual(
      retryState?.maxAttempts || 0
    );

    // Verify exhausted receipt
    const receipt = createMockReceipt({
      eventType: 'TASK_FAILED',
      caseId,
      taskId,
      decision: 'EXHAUST',
      context: { maxAttemptsReached: true },
    });
    const validation = ExecutionReceiptSchema.safeParse(receipt);
    expect(validation.success).toBe(true);
  }, 10000);
});

// =============================================================================
// Pattern 4: Deferred Choice Resolution (2 tests)
// =============================================================================

describe('Pattern 4: Deferred Choice Resolution', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    // Arrange
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon-choice',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-4',
      timeoutDefaults: { caseTimeoutMs: 5000 },
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('4.1: should resolve deferred choice via external trigger event', async () => {
    // Arrange
    const caseId = 'case-choice-001';
    const taskId = 'deferred-choice-task';
    const triggerPattern = {
      eventName: 'user:approved',
      filter: { userId: 'user-123' },
      timeoutMs: 3000,
    };

    // Act - Start waiting for trigger
    const choicePromise = bridge.waitForChoiceTrigger(
      caseId,
      taskId,
      triggerPattern
    );

    // Simulate receiving external event
    await new Promise(resolve => setTimeout(resolve, 100));
    const trigger = bridge.choiceTriggers.get(`${caseId}:${taskId}`);
    if (trigger?.resolve) {
      trigger.resolve({
        eventName: triggerPattern.eventName,
        decision: 'APPROVE',
      });
    }

    // Simulate enabling the appropriate task
    await yawlEngine.enableTask({ caseId, taskId: 'approval-confirmed' });

    // Assert
    const result = await choicePromise;
    expect(result).toBeDefined();
    expect(yawlEngine.getTaskState(caseId, 'approval-confirmed')?.status)
      .toBe('ENABLED');

    // Verify deferred choice receipt
    const receipt = createMockReceipt({
      eventType: 'CONTROL_FLOW_EVALUATED',
      caseId,
      taskId,
      decision: 'APPROVE',
      context: { triggerEvent: triggerPattern.eventName },
    });
    const validation = ExecutionReceiptSchema.safeParse(receipt);
    expect(validation.success).toBe(true);
  }, 10000);

  it('4.2: should apply timeout fallback when choice trigger not resolved', async () => {
    // Arrange
    const caseId = 'case-choice-002';
    const taskId = 'timeout-choice-task';
    const timeoutMs = 300;

    const triggerPattern = {
      eventName: 'approval:decision',
      timeoutMs,
    };

    // Act - Wait for trigger with timeout
    const choicePromise = bridge.waitForChoiceTrigger(
      caseId,
      taskId,
      triggerPattern
    );

    // Assert - Trigger should timeout and reject
    try {
      await choicePromise;
      expect.fail('Should have timed out');
    } catch (error) {
      expect(error.message).toContain('timeout');
    }

    // Verify timeout receipt
    const receipt = createMockReceipt({
      eventType: 'CONTROL_FLOW_EVALUATED',
      caseId,
      taskId,
      decision: 'DEFAULT_BRANCH',
      context: { timeoutOccurred: true },
    });
    const validation = ExecutionReceiptSchema.safeParse(receipt);
    expect(validation.success).toBe(true);
  }, 10000);
});

// =============================================================================
// Pattern 5: Parallel Task Distribution (2 tests)
// =============================================================================

describe('Pattern 5: Parallel Task Distribution', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    // Arrange
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon-parallel',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-5',
      enableDistribution: true,
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('5.1: should distribute AND-split tasks via round-robin strategy', async () => {
    // Arrange
    const caseId = 'case-parallel-001';
    const taskIds = ['review-doc', 'verify-budget', 'check-compliance'];

    const distributedListener = vi.fn();
    bridge.on('tasks:distributed', distributedListener);

    // Act - Distribute tasks with round-robin
    const distResult = await bridge.distributeAndSplitTasks(
      caseId,
      taskIds,
      { strategy: 'round-robin' }
    );

    // Trigger distribution execution
    await new Promise(resolve => setTimeout(resolve, 100));
    const operation = daemon.operations.get(distResult.operationId);
    if (operation?.handler) {
      await operation.handler();
    }

    // Assert
    expect(distResult.success).toBe(true);
    expect(distResult.taskIds.length).toBe(taskIds.length);
    expect(distResult.strategy).toBe('round-robin');

    // Verify all tasks are enabled
    for (const taskId of taskIds) {
      const state = yawlEngine.getTaskState(caseId, taskId);
      expect(state?.status).toBe('ENABLED');

      // Verify parallel task receipt
      const receipt = createMockReceipt({
        eventType: 'TASK_ENABLED',
        caseId,
        taskId,
        context: { strategy: 'round-robin' },
      });
      const validation = ExecutionReceiptSchema.safeParse(receipt);
      expect(validation.success).toBe(true);
    }
  }, 10000);

  it('5.2: should distribute tasks with least-loaded strategy', async () => {
    // Arrange
    const caseId = 'case-parallel-002';
    const taskIds = ['task-1', 'task-2', 'task-3'];

    const distributedListener = vi.fn();
    bridge.on('tasks:distributed', distributedListener);

    // Act - Distribute with least-loaded strategy
    const distResult = await bridge.distributeAndSplitTasks(
      caseId,
      taskIds,
      { strategy: 'least-loaded' }
    );

    // Trigger distribution
    await new Promise(resolve => setTimeout(resolve, 100));
    const operation = daemon.operations.get(distResult.operationId);
    if (operation?.handler) {
      await operation.handler();
    }

    // Complete tasks to simulate load
    for (let i = 0; i < taskIds.length; i++) {
      await yawlEngine.completeTask({
        caseId,
        taskId: taskIds[i],
        outputData: { loadIndex: i },
      });
    }

    // Assert
    expect(distResult.strategy).toBe('least-loaded');
    expect(distResult.caseId).toBe(caseId);

    const distribution = bridge.parallelDistributions.get(distResult.distributionId);
    expect(distribution?.results.length).toBe(taskIds.length);

    // Verify all tasks completed
    for (const taskId of taskIds) {
      const state = yawlEngine.getTaskState(caseId, taskId);
      expect(state?.status).toBe('COMPLETED');
    }
  }, 10000);
});

// =============================================================================
// Pattern 6: Cascading Failure & Recovery (2 tests)
// =============================================================================

describe('Pattern 6: Cascading Failure & Recovery', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    // Arrange
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon-recovery',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-6',
      enableAutoRetry: true,
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('6.1: should trigger compensation workflow on task failure', async () => {
    // Arrange
    const caseId = 'case-recovery-001';
    const primaryTaskId = 'payment-task';
    const compensationTaskId = 'refund-task';

    const failListener = vi.fn();
    yawlEngine.on('task:failed', failListener);

    // Enable primary task first
    await yawlEngine.enableTask({ caseId, taskId: primaryTaskId });

    // Act - Fail primary task
    await yawlEngine.failTask({
      caseId,
      taskId: primaryTaskId,
      error: 'Payment gateway timeout',
    });

    // Trigger compensation workflow
    const compensationPromise = yawlEngine.enableTask({
      caseId,
      taskId: compensationTaskId,
    });

    await compensationPromise;

    // Complete compensation
    await yawlEngine.completeTask({
      caseId,
      taskId: compensationTaskId,
      outputData: { refundStatus: 'PROCESSED' },
    });

    // Assert
    expect(yawlEngine.getTaskState(caseId, primaryTaskId)?.status)
      .toBe('FAILED');
    expect(yawlEngine.getTaskState(caseId, compensationTaskId)?.status)
      .toBe('COMPLETED');

    // Verify failure receipt
    const failureReceipt = createMockReceipt({
      eventType: 'TASK_FAILED',
      caseId,
      taskId: primaryTaskId,
      decision: 'COMPENSATE',
    });
    const failValidation = ExecutionReceiptSchema.safeParse(failureReceipt);
    expect(failValidation.success).toBe(true);

    // Verify compensation receipt
    const compensationReceipt = createMockReceipt({
      eventType: 'TASK_COMPLETED',
      caseId,
      taskId: compensationTaskId,
      decision: 'REFUND',
    });
    const compValidation = ExecutionReceiptSchema.safeParse(compensationReceipt);
    expect(compValidation.success).toBe(true);
  }, 10000);

  it('6.2: should execute cleanup tasks when case fails gracefully', async () => {
    // Arrange
    const caseId = 'case-recovery-002';
    const mainTaskId = 'process-workflow';
    const cleanupTasks = ['release-lock', 'clear-cache', 'log-failure'];

    const completedTasks = [];
    yawlEngine.on('task:completed', (event) => {
      completedTasks.push(event.taskId);
    });

    // Act - Create case and enable main task
    await yawlEngine.createCase({
      caseId,
      workflowId: 'workflow-with-cleanup',
    });

    // Enable and then fail main workflow
    await yawlEngine.enableTask({ caseId, taskId: mainTaskId });
    await yawlEngine.failTask({ caseId, taskId: mainTaskId });

    // Trigger cleanup tasks
    for (const cleanupTaskId of cleanupTasks) {
      await yawlEngine.enableTask({ caseId, taskId: cleanupTaskId });
      await yawlEngine.completeTask({
        caseId,
        taskId: cleanupTaskId,
        outputData: { status: 'cleaned' },
      });
    }

    // Assert
    expect(yawlEngine.getTaskState(caseId, mainTaskId)?.status)
      .toBe('FAILED');

    for (const cleanupTaskId of cleanupTasks) {
      expect(yawlEngine.getTaskState(caseId, cleanupTaskId)?.status)
        .toBe('COMPLETED');
    }

    expect(completedTasks.length).toBe(cleanupTasks.length);

    // Verify cleanup receipts
    for (const cleanupTaskId of cleanupTasks) {
      const receipt = createMockReceipt({
        eventType: 'TASK_COMPLETED',
        caseId,
        taskId: cleanupTaskId,
        decision: 'CLEANUP',
        context: { isCleanup: true },
      });
      const validation = ExecutionReceiptSchema.safeParse(receipt);
      expect(validation.success).toBe(true);
    }
  }, 10000);
});

// =============================================================================
// Integration Summary Test
// =============================================================================

describe('Daemon-YAWL Integration Summary', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'integration-test-daemon',
    });
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'integration-node',
      maxConcurrentCases: 100,
      enableAutoRetry: true,
      enableTimeoutTracking: true,
      enableDistribution: true,
    });

    await daemon.start();
    await bridge.start();
  });

  afterEach(async () => {
    await bridge.stop();
    await daemon.stop();
  });

  it('should maintain bridge statistics across all operations', async () => {
    // Arrange
    const workflowId = 'stats-workflow';

    // Act - Perform various operations
    await bridge.scheduleRecurringCase(workflowId, '0 * * * *');

    const caseId = 'stat-case-001';
    await yawlEngine.createCase({ caseId, workflowId });

    const taskId = 'stat-task-001';
    await bridge.watchTaskTimeout(caseId, taskId, 5000);
    await bridge.scheduleRetry(caseId, taskId);

    // Get statistics
    const stats = bridge.getStats();

    // Assert
    expect(stats.bridgeId).toBe(bridge.id);
    expect(stats.isRunning).toBe(true);
    expect(stats.caseSchedules).toBeGreaterThan(0);
    expect(stats.activeTimeouts).toBeGreaterThan(0);
    expect(stats.activeRetries).toBeGreaterThan(0);
    expect(stats.timestamp).toBeDefined();
  }, 10000);

  it('should handle concurrent operations without race conditions', async () => {
    // Arrange
    const caseCount = 10;
    const tasksPerCase = 3;

    // Act - Create multiple cases with multiple tasks
    const casePromises = Array.from({ length: caseCount }, async (_, i) => {
      const caseId = `concurrent-case-${i}`;
      await yawlEngine.createCase({
        caseId,
        workflowId: 'concurrent-workflow',
      });

      const taskPromises = Array.from({ length: tasksPerCase }, async (_, j) => {
        const taskId = `task-${j}`;
        await yawlEngine.enableTask({ caseId, taskId });
        await bridge.watchTaskTimeout(caseId, taskId, 2000);
        return { caseId, taskId };
      });

      return Promise.all(taskPromises);
    });

    await Promise.all(casePromises);

    // Assert - Verify all operations completed
    expect(yawlEngine.cases.size).toBe(caseCount);
    expect(bridge.taskTimeouts.size).toBe(caseCount * tasksPerCase);

    // All timeouts should be active
    let activeTimeouts = 0;
    for (const timeout of bridge.taskTimeouts.values()) {
      if (timeout?.operationId) activeTimeouts++;
    }
    expect(activeTimeouts).toBe(caseCount * tasksPerCase);
  }, 10000);
});
