/**
 * Multiple Instance Edge Cases
 *
 * Tests for boundary conditions, error paths, and unusual scenarios
 * that could expose bugs in MI pattern implementation.
 */

import { describe, test, expect, beforeEach } from 'vitest';
import {
  YawlEngine,
  YawlWorkflow,
  SPLIT_TYPE,
  JOIN_TYPE,
  sequence,
  TaskStatus,
} from '../../src/index.mjs';

function createTestEngine() {
  return new YawlEngine({ nodeId: `edge-${Date.now()}` });
}

function createTestWorkflow(config = {}) {
  return new YawlWorkflow({
    id: config.id ?? `edge-wf-${Date.now()}`,
    name: config.name ?? 'Edge Case Test',
    version: '1.0.0',
    tasks: config.tasks ?? [],
    ...config,
  });
}

describe('Zero Instances Edge Cases', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Zero instances with XOR-join completes immediately', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'zero-xor' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 0,
        instanceSync: 'XOR',
      },
    });
    workflow.addTask({ id: 'end', joinType: JOIN_TYPE.XOR });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('zero-xor');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      instanceCount: 0,
    });

    // Assert: End enabled immediately (XOR-join with 0 branches)
    const endItems = yawlCase.getEnabledWorkItems();
    expect(endItems.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(endItems[0].id)).toBe('end');
  });

  test('Zero instances with AND-join completes immediately', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'zero-and' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 0,
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'end', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('zero-and');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      instanceCount: 0,
    });

    // Assert: End enabled (AND-join of 0 branches satisfied)
    const endItems = yawlCase.getEnabledWorkItems();
    expect(endItems.length).toBe(1);
  });

  test('Zero instances validates minimum constraint', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'zero-min-violation' });

    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 3, // Require at least 3
        instanceSync: 'AND',
      },
    });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act & Assert: Should reject 0 instances
    await expect(
      engine.createCase('zero-min-violation', { instanceCount: 0 })
    ).rejects.toThrow(/minimum/i);
  });
});

describe('One Instance Edge Cases (Degenerate MI)', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('One instance behaves like atomic task', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'one-instance' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 1,
        maximum: 1,
        creationType: 'static',
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'end' });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('one-instance');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      instanceCount: 1,
    });

    // Assert: Single instance
    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(1);

    // Complete it
    await engine.startTask(yawlCase.id, instances[0].id);
    const { downstreamEnabled } = await engine.completeTask(
      yawlCase.id,
      instances[0].id
    );

    // Assert: End enabled (AND-join of 1 satisfied)
    expect(downstreamEnabled.length).toBe(1);
    expect(downstreamEnabled[0].taskId).toBe('end');
  });

  test('One instance with XOR-join', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'one-xor' });

    workflow.addTask({ id: 'process', kind: 'multiple' });
    workflow.addTask({ id: 'end', joinType: JOIN_TYPE.XOR });

    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('process');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('one-xor', {
      instanceCount: 1,
    });

    const instance = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, instance.id);
    const { downstreamEnabled } = await engine.completeTask(
      yawlCase.id,
      instance.id
    );

    // Assert: XOR-join fires immediately
    expect(downstreamEnabled.length).toBe(1);
    expect(downstreamEnabled[0].taskId).toBe('end');
  });
});

describe('Instance Failure During Barrier Wait', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('AND-join: One instance fails, barrier never satisfied', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'and-fail' });

    workflow.addTask({ id: 'split' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 5,
        maximum: 5,
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'merge', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('split', 'process'));
    workflow.addFlow(sequence('process', 'merge'));
    workflow.setStart('split');
    workflow.setEnd(['merge']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('and-fail');

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id, {
      instanceCount: 5,
    });

    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(5);

    // Complete 3 instances
    for (let i = 0; i < 3; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
      await engine.completeTask(yawlCase.id, instances[i].id);
    }

    // Fail instance 4
    await engine.startTask(yawlCase.id, instances[3].id);
    const { task: failedTask } = await engine.failTask(
      yawlCase.id,
      instances[3].id,
      'Simulated failure'
    );

    expect(failedTask.status).toBe(TaskStatus.FAILED);

    // Complete instance 5
    await engine.startTask(yawlCase.id, instances[4].id);
    await engine.completeTask(yawlCase.id, instances[4].id);

    // Assert: Merge NOT enabled (AND-join failed - one instance failed)
    const mergeItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'merge'
    );
    expect(mergeItems.length).toBe(0);

    // Case should be in failed/suspended state
    expect(['failed', 'suspended']).toContain(yawlCase.status);
  });

  test('OR-join: Instance failure if threshold still met', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'or-fail-threshold' });

    workflow.addTask({ id: 'split' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 10,
        maximum: 10,
        threshold: 6, // Need 6 to proceed
        instanceSync: 'OR',
      },
    });
    workflow.addTask({ id: 'merge', joinType: JOIN_TYPE.OR });

    workflow.addFlow(sequence('split', 'process'));
    workflow.addFlow(sequence('process', 'merge'));
    workflow.setStart('split');
    workflow.setEnd(['merge']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('or-fail-threshold');

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id, {
      instanceCount: 10,
    });

    const instances = yawlCase.getEnabledWorkItems();

    // Complete 6 instances (threshold)
    for (let i = 0; i < 6; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
      await engine.completeTask(yawlCase.id, instances[i].id);
    }

    // Assert: Merge enabled (threshold reached)
    let mergeItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'merge'
    );
    expect(mergeItems.length).toBe(1);

    // Fail remaining instances
    for (let i = 6; i < 10; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
      await engine.failTask(yawlCase.id, instances[i].id, 'Failed');
    }

    // Assert: Merge still enabled (threshold was already met)
    mergeItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'merge'
    );
    expect(mergeItems.length).toBe(1);
  });
});

describe('Timeout During Dynamic Spawning', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Instance timeout before barrier satisfied', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'timeout-barrier' });

    workflow.addTask({ id: 'split' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      timeout: 100, // 100ms timeout
      multipleInstance: {
        minimum: 3,
        maximum: 3,
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'merge', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('split', 'process'));
    workflow.addFlow(sequence('process', 'merge'));
    workflow.setStart('split');
    workflow.setEnd(['merge']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('timeout-barrier');

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id, {
      instanceCount: 3,
    });

    const instances = yawlCase.getEnabledWorkItems();

    // Complete 2 instances
    await engine.startTask(yawlCase.id, instances[0].id);
    await engine.completeTask(yawlCase.id, instances[0].id);
    await engine.startTask(yawlCase.id, instances[1].id);
    await engine.completeTask(yawlCase.id, instances[1].id);

    // Start instance 3 but let it timeout
    await engine.startTask(yawlCase.id, instances[2].id);

    // Simulate timeout
    const { task: timedOutTask } = await engine.timeoutWorkItem(
      yawlCase.id,
      instances[2].id
    );

    expect(timedOutTask.status).toBe(TaskStatus.TIMEOUT);

    // Assert: Merge NOT enabled (AND-join not satisfied)
    const mergeItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'merge'
    );
    expect(mergeItems.length).toBe(0);
  });

  test('Dynamic instance addition after timeout', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'timeout-dynamic-add' });

    workflow.addTask({ id: 'process', kind: 'multiple', timeout: 50 });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('timeout-dynamic-add', {
      instanceCount: 2,
    });

    const instances = yawlCase.getEnabledWorkItems();

    // Start instance 1, let it timeout
    await engine.startTask(yawlCase.id, instances[0].id);
    await engine.timeoutWorkItem(yawlCase.id, instances[0].id);

    // Add new instance after timeout
    await engine.addMIInstance(yawlCase.id, 'process', { id: 3 });

    // Assert: New instance added successfully
    const allInstances = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'process'
    );
    expect(allInstances.length).toBe(2); // instance 2 + new instance 3

    // Complete them
    for (const inst of allInstances) {
      await engine.startTask(yawlCase.id, inst.id);
      await engine.completeTask(yawlCase.id, inst.id);
    }

    expect(yawlCase.status).toBe('completed');
  });
});

describe('Concurrent Cancellation + Instance Addition', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Cancel while adding instances - race condition', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'race-cancel-add' });

    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      cancellationRegion: 'all',
    });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('race-cancel-add', {
      instanceCount: 10,
    });

    const instances = yawlCase.getEnabledWorkItems();

    // Complete 5 instances
    for (let i = 0; i < 5; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
      await engine.completeTask(yawlCase.id, instances[i].id);
    }

    // Concurrent: add instances AND cancel region
    const addPromises = [];
    for (let i = 0; i < 10; i++) {
      addPromises.push(
        engine.addMIInstance(yawlCase.id, 'process', { id: 10 + i })
      );
    }

    const cancelPromise = engine.cancelRegion(
      yawlCase.id,
      'all',
      'Concurrent cancel'
    );

    // Wait for both
    const [addResults, cancelResult] = await Promise.all([
      Promise.allSettled(addPromises),
      cancelPromise,
    ]);

    // Assert: System handles race gracefully
    // Either: instances added then cancelled, or add rejected
    const { cancelled } = cancelResult;
    expect(cancelled.length).toBeGreaterThan(0);

    // Remaining instances should be 0 or minimal
    const remaining = yawlCase.getEnabledWorkItems();
    expect(remaining.length).toBeLessThan(5);
  });

  test('Cancel parent MI task cancels all child instances', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'cancel-parent-mi' });

    workflow.addTask({ id: 'parent', kind: 'multiple' });
    workflow.setStart('parent');
    workflow.setEnd(['parent']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('cancel-parent-mi', {
      instanceCount: 20,
    });

    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(20);

    // Start some instances
    for (let i = 0; i < 10; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
    }

    // Cancel task 'parent' globally
    const { cancelled } = await engine.cancelTaskGlobal(
      yawlCase.id,
      'parent',
      'Parent cancelled'
    );

    // Assert: All 20 instances cancelled
    expect(cancelled.length).toBe(20);
    expect(yawlCase.getEnabledWorkItems().length).toBe(0);
  });
});

describe('Receipt Integrity Under Concurrent Operations', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Concurrent instance completions maintain receipt chain', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'concurrent-receipts' });

    workflow.addTask({ id: 'process', kind: 'multiple' });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('concurrent-receipts', {
      instanceCount: 20,
    });

    const instances = yawlCase.getEnabledWorkItems();

    // Complete all 20 instances concurrently
    await Promise.all(
      instances.map(async inst => {
        await engine.startTask(yawlCase.id, inst.id);
        await engine.completeTask(yawlCase.id, inst.id, {
          instanceId: inst.id,
        });
      })
    );

    // Assert: All receipts valid
    const receipts = yawlCase.receipts;
    expect(receipts.length).toBeGreaterThan(20);

    for (const receipt of receipts) {
      expect(receipt.valid).toBe(true);
    }

    // Verify chain (even with concurrent operations)
    for (let i = 1; i < receipts.length; i++) {
      const chainResult = await receipts[i].verifyChain(receipts[i - 1]);
      expect(chainResult.valid).toBe(true);
    }
  });

  test('Receipt contains MI metadata', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'receipt-metadata' });

    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 3,
        maximum: 3,
        instanceSync: 'AND',
      },
    });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('receipt-metadata', {
      instanceCount: 3,
    });

    const instances = yawlCase.getEnabledWorkItems();

    for (let i = 0; i < instances.length; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
      await engine.completeTask(yawlCase.id, instances[i].id, {
        instanceNumber: i,
      });
    }

    // Assert: Receipts contain MI metadata
    const receipts = yawlCase.receipts.filter(r => r.action === 'complete');

    for (let i = 0; i < receipts.length; i++) {
      const receipt = receipts[i];
      expect(receipt.metadata).toBeDefined();
      // MI receipts should track instance info
      if (receipt.metadata?.workItemId) {
        expect(instances.map(inst => inst.id)).toContain(
          receipt.metadata.workItemId
        );
      }
    }
  });
});

describe('Boundary Value Validation', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Maximum = minimum enforces exact count', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'exact-count' });

    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 7,
        maximum: 7,
        creationType: 'static',
      },
    });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act & Assert: Exactly 7 instances required
    await expect(
      engine.createCase('exact-count', { instanceCount: 5 })
    ).rejects.toThrow(/minimum/);

    await expect(
      engine.createCase('exact-count', { instanceCount: 9 })
    ).rejects.toThrow(/maximum/);

    // Valid: exactly 7
    const { case: yawlCase } = await engine.createCase('exact-count', {
      instanceCount: 7,
    });

    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(7);
  });

  test('Threshold > maximum is invalid', async () => {
    // Arrange & Assert: Invalid schema
    expect(() => {
      const workflow = createTestWorkflow({ id: 'invalid-threshold' });
      workflow.addTask({
        id: 'process',
        kind: 'multiple',
        multipleInstance: {
          minimum: 5,
          maximum: 10,
          threshold: 15, // Invalid: > maximum
        },
      });
    }).toThrow();
  });

  test('Minimum > maximum is invalid', async () => {
    // Arrange & Assert: Invalid schema
    expect(() => {
      const workflow = createTestWorkflow({ id: 'invalid-min-max' });
      workflow.addTask({
        id: 'process',
        kind: 'multiple',
        multipleInstance: {
          minimum: 10,
          maximum: 5, // Invalid: < minimum
        },
      });
    }).toThrow();
  });

  test('Negative values rejected', async () => {
    // Arrange & Assert
    expect(() => {
      const workflow = createTestWorkflow({ id: 'negative' });
      workflow.addTask({
        id: 'process',
        kind: 'multiple',
        multipleInstance: {
          minimum: -1, // Invalid
        },
      });
    }).toThrow();
  });
});
