/**
 * @file Durable Execution Tests - Replay, Compensation, Versioning
 * @module @unrdf/yawl-durable/test/durable.test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { DurableWorkflowEngine } from '../src/engine.mjs';
import { executeSaga, createSagaWorkflow } from '../src/saga.mjs';
import { replayFromReceipts, replayToTimestamp, verifyReceiptChain } from '../src/replay.mjs';

// =============================================================================
// Test: Deterministic Replay
// =============================================================================

describe('Deterministic Replay', () => {
  let engine;

  beforeEach(() => {
    engine = new DurableWorkflowEngine();
  });

  it('should replay workflow state from receipts', async () => {
    // Define simple workflow
    await engine.defineWorkflow({
      id: 'test-workflow',
      name: 'Test Workflow',
      activities: [
        {
          id: 'step1',
          name: 'Step 1',
          handler: async (input) => ({ ...input, step1: 'done' }),
        },
        {
          id: 'step2',
          name: 'Step 2',
          handler: async (input) => ({ ...input, step2: 'done' }),
        },
      ],
      flow: [
        { from: 'step1', to: 'step2' },
      ],
    });

    // Execute workflow
    const execution = await engine.startWorkflow('test-workflow', { value: 42 });
    const executionId = execution.executionId;

    await engine.executeActivity(executionId, 'step1', { value: 42 });
    await engine.executeActivity(executionId, 'step2', { value: 42, step1: 'done' });

    // Replay from receipts
    const receipts = engine.getReceiptHistory(executionId);
    expect(receipts.length).toBeGreaterThan(0);

    const state = await replayFromReceipts(receipts);

    expect(state.completedTasks).toContain('step1');
    expect(state.completedTasks).toContain('step2');
    expect(state.events.length).toBeGreaterThan(0);
    expect(state.receiptCount).toBe(receipts.length);
  });

  it('should verify receipt chain integrity', async () => {
    await engine.defineWorkflow({
      id: 'test-workflow',
      name: 'Test Workflow',
      activities: [
        {
          id: 'task1',
          name: 'Task 1',
          handler: async (input) => ({ result: 'ok' }),
        },
      ],
      flow: [],
    });

    const execution = await engine.startWorkflow('test-workflow');
    await engine.executeActivity(execution.executionId, 'task1', {});

    const verification = await engine.verifyReceiptChain(execution.executionId);

    expect(verification.valid).toBe(true);
    expect(verification.receiptCount).toBeGreaterThan(0);
    expect(verification.genesisHash).toBeDefined();
    expect(verification.latestHash).toBeDefined();
  });

  it('should detect broken receipt chain', async () => {
    const receipts = [
      {
        id: 'receipt1',
        caseId: 'case1',
        taskId: 'task1',
        eventType: 'CASE_CREATED',
        t_ns: 1000n,
        timestamp_iso: '2025-01-01T00:00:00.000Z',
        previousReceiptHash: null,
        receiptHash: 'hash1',
        payload: { decision: 'CREATE' },
      },
      {
        id: 'receipt2',
        caseId: 'case1',
        taskId: 'task1',
        eventType: 'TASK_ENABLED',
        t_ns: 2000n,
        timestamp_iso: '2025-01-01T00:00:01.000Z',
        previousReceiptHash: 'wrong-hash', // Chain broken!
        receiptHash: 'hash2',
        payload: { decision: 'ENABLE' },
      },
    ];

    const verification = await verifyReceiptChain(receipts);

    expect(verification.valid).toBe(false);
    expect(verification.error).toContain('Chain broken');
  });
});

// =============================================================================
// Test: Saga Compensation
// =============================================================================

describe('Saga Compensation', () => {
  let engine;
  let compensationLog;

  beforeEach(() => {
    engine = new DurableWorkflowEngine();
    compensationLog = [];
  });

  it('should execute compensation on saga failure', async () => {
    const sagaConfig = createSagaWorkflow({
      id: 'test-saga',
      name: 'Test Saga',
      steps: [
        {
          id: 'step1',
          handler: async (input) => {
            compensationLog.push('step1:execute');
            return { ...input, step1: 'done' };
          },
          compensate: async (output) => {
            compensationLog.push('step1:compensate');
          },
        },
        {
          id: 'step2',
          handler: async (input) => {
            compensationLog.push('step2:execute');
            return { ...input, step2: 'done' };
          },
          compensate: async (output) => {
            compensationLog.push('step2:compensate');
          },
        },
        {
          id: 'step3',
          handler: async (input) => {
            compensationLog.push('step3:execute');
            throw new Error('Step 3 failed!');
          },
          compensate: async (output) => {
            compensationLog.push('step3:compensate');
          },
        },
      ],
    });

    await engine.defineWorkflow(sagaConfig);

    const result = await executeSaga(engine, 'test-saga', { value: 1 });

    expect(result.success).toBe(false);
    expect(result.error).toContain('Step 3 failed');
    expect(result.compensated).toContain('step2');
    expect(result.compensated).toContain('step1');

    // Verify compensation executed in reverse order
    expect(compensationLog).toContain('step1:execute');
    expect(compensationLog).toContain('step2:execute');
    expect(compensationLog).toContain('step3:execute');
    expect(compensationLog).toContain('step2:compensate');
    expect(compensationLog).toContain('step1:compensate');

    // Verify reverse order
    const step2CompensateIdx = compensationLog.indexOf('step2:compensate');
    const step1CompensateIdx = compensationLog.indexOf('step1:compensate');
    expect(step2CompensateIdx).toBeLessThan(step1CompensateIdx);
  });

  it('should complete saga successfully when all steps succeed', async () => {
    const sagaConfig = createSagaWorkflow({
      id: 'success-saga',
      name: 'Success Saga',
      steps: [
        {
          id: 'step1',
          handler: async (input) => ({ ...input, step1: 'done' }),
          compensate: async () => {
            compensationLog.push('step1:compensate');
          },
        },
        {
          id: 'step2',
          handler: async (input) => ({ ...input, step2: 'done' }),
          compensate: async () => {
            compensationLog.push('step2:compensate');
          },
        },
      ],
    });

    await engine.defineWorkflow(sagaConfig);

    const result = await executeSaga(engine, 'success-saga', { value: 1 });

    expect(result.success).toBe(true);
    expect(result.completedActivities).toContain('step1');
    expect(result.completedActivities).toContain('step2');
    expect(result.output.step1).toBe('done');
    expect(result.output.step2).toBe('done');

    // No compensation should execute on success
    expect(compensationLog).toHaveLength(0);
  });
});

// =============================================================================
// Test: Activity Retries
// =============================================================================

describe('Activity Retries', () => {
  let engine;
  let attemptCount;

  beforeEach(() => {
    engine = new DurableWorkflowEngine();
    attemptCount = 0;
  });

  it('should retry failed activity with exponential backoff', async () => {
    await engine.defineWorkflow({
      id: 'retry-workflow',
      name: 'Retry Workflow',
      activities: [
        {
          id: 'flaky-task',
          name: 'Flaky Task',
          timeout: 5000,
          retryPolicy: {
            maxAttempts: 3,
            initialInterval: 100,
            backoffCoefficient: 2,
          },
          handler: async (input, context) => {
            attemptCount++;

            // Fail first 2 attempts, succeed on 3rd
            if (attemptCount < 3) {
              throw new Error(`Attempt ${attemptCount} failed`);
            }

            return { success: true, attempts: attemptCount };
          },
        },
      ],
      flow: [],
    });

    const execution = await engine.startWorkflow('retry-workflow');
    const result = await engine.executeActivity(execution.executionId, 'flaky-task', {});

    expect(result.success).toBe(true);
    expect(attemptCount).toBe(3);
  }, 10000);

  it('should fail after max retry attempts', async () => {
    await engine.defineWorkflow({
      id: 'fail-workflow',
      name: 'Fail Workflow',
      activities: [
        {
          id: 'always-fail',
          name: 'Always Fail',
          timeout: 5000,
          retryPolicy: {
            maxAttempts: 2,
            initialInterval: 50,
            backoffCoefficient: 1,
          },
          handler: async () => {
            attemptCount++;
            throw new Error('Always fails');
          },
        },
      ],
      flow: [],
    });

    const execution = await engine.startWorkflow('fail-workflow');

    await expect(
      engine.executeActivity(execution.executionId, 'always-fail', {})
    ).rejects.toThrow('failed after 2 attempts');

    expect(attemptCount).toBe(2);
  }, 10000);
});

// =============================================================================
// Test: Time Travel
// =============================================================================

describe('Time Travel', () => {
  let engine;

  beforeEach(() => {
    engine = new DurableWorkflowEngine();
  });

  it('should replay state at specific timestamp', async () => {
    await engine.defineWorkflow({
      id: 'time-workflow',
      name: 'Time Workflow',
      activities: [
        {
          id: 'task1',
          name: 'Task 1',
          handler: async () => ({ task1: 'done' }),
        },
        {
          id: 'task2',
          name: 'Task 2',
          handler: async () => ({ task2: 'done' }),
        },
      ],
      flow: [
        { from: 'task1', to: 'task2' },
      ],
    });

    const execution = await engine.startWorkflow('time-workflow');
    const executionId = execution.executionId;

    // Execute first task
    await engine.executeActivity(executionId, 'task1', {});
    const receiptsAfterTask1 = [...engine.getReceiptHistory(executionId)];
    const timestampAfterTask1 = receiptsAfterTask1[receiptsAfterTask1.length - 1].t_ns;

    // Wait a bit
    await new Promise(resolve => setTimeout(resolve, 100));

    // Execute second task
    await engine.executeActivity(executionId, 'task2', {});
    const receiptsAfterTask2 = engine.getReceiptHistory(executionId);

    // Replay to point after task1 but before task2
    const stateAtTask1 = await replayToTimestamp(receiptsAfterTask2, timestampAfterTask1);

    expect(stateAtTask1.completedTasks).toContain('task1');
    expect(stateAtTask1.completedTasks).not.toContain('task2');
  });
});

// =============================================================================
// Test: Workflow Versioning
// =============================================================================

describe('Workflow Versioning', () => {
  let engine;

  beforeEach(() => {
    engine = new DurableWorkflowEngine();
  });

  it('should support multiple workflow versions', async () => {
    // Define v1
    await engine.defineWorkflow({
      id: 'versioned-workflow',
      name: 'Versioned Workflow',
      version: '1.0.0',
      activities: [
        {
          id: 'task1',
          name: 'Task 1',
          handler: async () => ({ version: '1.0.0' }),
        },
      ],
      flow: [],
    });

    // Define v2
    await engine.defineWorkflow({
      id: 'versioned-workflow-v2',
      name: 'Versioned Workflow',
      version: '2.0.0',
      activities: [
        {
          id: 'task1',
          name: 'Task 1 Enhanced',
          handler: async () => ({ version: '2.0.0', enhanced: true }),
        },
      ],
      flow: [],
    });

    const execution1 = await engine.startWorkflow('versioned-workflow');
    const result1 = await engine.executeActivity(execution1.executionId, 'task1', {});
    expect(result1.version).toBe('1.0.0');

    const execution2 = await engine.startWorkflow('versioned-workflow-v2');
    const result2 = await engine.executeActivity(execution2.executionId, 'task1', {});
    expect(result2.version).toBe('2.0.0');
    expect(result2.enhanced).toBe(true);
  });
});
