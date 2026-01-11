/**
 * YAWL WP12 Tests - Multiple Instances without Synchronization
 *
 * Tests the WP12 pattern implementation including:
 * - Instance spawning
 * - Independent execution
 * - Failure isolation
 * - Receipt generation
 * - Aggregate status tracking
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  spawnInstancesNoSync,
  enableInstance,
  startInstance,
  completeInstance,
  failInstance,
  cancelInstance,
  getAggregateStatus,
  areAllInstancesComplete,
  getCompletionPercentage,
  MultiInstanceTracker,
  InstanceStatus,
} from '../../src/multiple-instance/wp12-no-sync.mjs';
import { TaskDefinition } from '../../src/task-core.mjs';

// =============================================================================
// Test Helpers
// =============================================================================

/**
 * Create a test task definition
 */
function createTestTaskDef(id = 'TestTask') {
  return new TaskDefinition({
    id,
    name: `Test Task ${id}`,
    kind: 'MultipleInstanceTask',
    inputConditions: [],
    outputConditions: [],
    splitType: 'and',
    joinType: 'and',
  });
}

/**
 * Simulate instance execution
 */
async function executeInstance(instanceId, tracker, shouldFail = false) {
  enableInstance(instanceId, tracker);
  await new Promise(resolve => setTimeout(resolve, 1));

  startInstance(instanceId, tracker);
  await new Promise(resolve => setTimeout(resolve, 1));

  if (shouldFail) {
    failInstance(instanceId, new Error('Simulated failure'), tracker);
  } else {
    completeInstance(instanceId, { result: `output-${instanceId}` }, tracker);
  }
}

// =============================================================================
// Basic Spawning Tests
// =============================================================================

describe('WP12 - Basic Instance Spawning', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should spawn 5 instances without synchronization', async () => {
    const taskDef = createTestTaskDef('Task1');
    const caseId = 'case-001';
    const count = 5;

    const result = await spawnInstancesNoSync(taskDef, caseId, count, {
      tracker,
      baseInputData: { shared: 'value' },
    });

    // Verify result structure
    expect(result.parentTaskId).toBeDefined();
    expect(result.instanceIds).toHaveLength(5);
    expect(result.instances).toHaveLength(5);
    expect(result.metadata.totalInstances).toBe(5);
    expect(result.metadata.spawnedAt).toBeGreaterThan(0n);

    // Verify instances created
    for (let i = 0; i < count; i++) {
      const instance = result.instances[i];
      expect(instance).toBeDefined();
      expect(instance.inputData.shared).toBe('value');
      expect(instance.inputData.__mi_metadata).toBeDefined();
      expect(instance.inputData.__mi_metadata.instanceIndex).toBe(i);
      expect(instance.inputData.__mi_metadata.totalInstances).toBe(5);
    }

    // Verify tracking
    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.totalInstances).toBe(5);
    expect(aggregate.statusCounts.spawned).toBe(5);
  });

  it('should create receipts for all instances', async () => {
    const taskDef = createTestTaskDef('Task2');
    const result = await spawnInstancesNoSync(taskDef, 'case-002', 3, {
      tracker,
      generateReceipts: true,
    });

    // Verify instance receipts
    expect(result.instanceReceipts).toBeDefined();
    expect(result.instanceReceipts).toHaveLength(3);

    for (const receipt of result.instanceReceipts) {
      expect(receipt.eventType).toBe('TASK_ENABLED');
      expect(receipt.payloadHash).toHaveLength(64);
      expect(receipt.receiptHash).toHaveLength(64);
      expect(receipt.payload.decision).toBe('SPAWN_MI_INSTANCE');
      expect(receipt.payload.context.pattern).toBe('WP12');
    }

    // Verify aggregate receipt
    expect(result.aggregateReceipt).toBeDefined();
    expect(result.aggregateReceipt.eventType).toBe('CONTROL_FLOW_EVALUATED');
    expect(result.aggregateReceipt.payload.decision).toBe('SPAWN_MI_NO_SYNC');
    expect(result.aggregateReceipt.payload.context.totalInstances).toBe(3);
    expect(result.aggregateReceipt.payload.context.merkleRoot).toBeDefined();
  });

  it('should support per-instance input data', async () => {
    const taskDef = createTestTaskDef('Task3');
    const instanceInputs = [
      { index: 0, data: 'alpha' },
      { index: 1, data: 'beta' },
      { index: 2, data: 'gamma' },
    ];

    const result = await spawnInstancesNoSync(taskDef, 'case-003', 3, {
      tracker,
      baseInputData: { shared: 'base' },
      instanceInputs,
    });

    // Verify each instance has unique input
    expect(result.instances[0].inputData.data).toBe('alpha');
    expect(result.instances[1].inputData.data).toBe('beta');
    expect(result.instances[2].inputData.data).toBe('gamma');

    // All should have shared data
    for (const instance of result.instances) {
      expect(instance.inputData.shared).toBe('base');
    }
  });

  it('should throw error if instance count is invalid', async () => {
    const taskDef = createTestTaskDef('Task4');

    await expect(
      spawnInstancesNoSync(taskDef, 'case-004', 0, { tracker })
    ).rejects.toThrow('Instance count must be positive');

    await expect(
      spawnInstancesNoSync(taskDef, 'case-004', -5, { tracker })
    ).rejects.toThrow('Instance count must be positive');
  });

  it('should throw error if instanceInputs length mismatch', async () => {
    const taskDef = createTestTaskDef('Task5');

    await expect(
      spawnInstancesNoSync(taskDef, 'case-005', 5, {
        tracker,
        instanceInputs: [{ a: 1 }, { b: 2 }], // Only 2 inputs for 5 instances
      })
    ).rejects.toThrow('instanceInputs length (2) must match count (5)');
  });
});

// =============================================================================
// Independent Execution Tests
// =============================================================================

describe('WP12 - Independent Execution', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should allow instances to complete independently', async () => {
    const taskDef = createTestTaskDef('Task6');
    const result = await spawnInstancesNoSync(taskDef, 'case-006', 5, { tracker });

    // Complete instances in random order
    await executeInstance(result.instanceIds[2], tracker);
    await executeInstance(result.instanceIds[0], tracker);
    await executeInstance(result.instanceIds[4], tracker);

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.statusCounts.completed).toBe(3);
    expect(aggregate.statusCounts.spawned).toBe(2);
    expect(aggregate.allCompleted).toBe(false);

    // Complete remaining
    await executeInstance(result.instanceIds[1], tracker);
    await executeInstance(result.instanceIds[3], tracker);

    const finalAggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(finalAggregate.statusCounts.completed).toBe(5);
    expect(finalAggregate.allCompleted).toBe(true);
  });

  it('should track instance status transitions', async () => {
    const taskDef = createTestTaskDef('Task7');
    const result = await spawnInstancesNoSync(taskDef, 'case-007', 1, { tracker });
    const instanceId = result.instanceIds[0];

    // Initial state
    let instance = tracker.getInstance(instanceId);
    expect(instance.status).toBe(InstanceStatus.SPAWNED);

    // Enable
    enableInstance(instanceId, tracker);
    instance = tracker.getInstance(instanceId);
    expect(instance.status).toBe(InstanceStatus.ENABLED);

    // Start
    startInstance(instanceId, tracker);
    instance = tracker.getInstance(instanceId);
    expect(instance.status).toBe(InstanceStatus.ACTIVE);

    // Complete
    completeInstance(instanceId, { output: 'done' }, tracker);
    instance = tracker.getInstance(instanceId);
    expect(instance.status).toBe(InstanceStatus.COMPLETED);
    expect(instance.outputData.output).toBe('done');
    expect(instance.completedAt).toBeDefined();
  });
});

// =============================================================================
// Failure Isolation Tests
// =============================================================================

describe('WP12 - Failure Isolation', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should isolate instance failures - failing instance does not block others', async () => {
    const taskDef = createTestTaskDef('Task8');
    const result = await spawnInstancesNoSync(taskDef, 'case-008', 5, { tracker });

    // Complete some instances successfully
    await executeInstance(result.instanceIds[0], tracker, false);
    await executeInstance(result.instanceIds[1], tracker, false);

    // Fail one instance
    await executeInstance(result.instanceIds[2], tracker, true);

    // Complete remaining successfully
    await executeInstance(result.instanceIds[3], tracker, false);
    await executeInstance(result.instanceIds[4], tracker, false);

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.statusCounts.completed).toBe(4);
    expect(aggregate.statusCounts.failed).toBe(1);
    expect(aggregate.anyFailed).toBe(true);
    expect(aggregate.allCompleted).toBe(true); // Terminal state

    // Verify failed instance details
    const failedInstance = tracker.getInstance(result.instanceIds[2]);
    expect(failedInstance.status).toBe(InstanceStatus.FAILED);
    expect(failedInstance.error).toBe('Simulated failure');
  });

  it('should handle multiple failures without synchronization', async () => {
    const taskDef = createTestTaskDef('Task9');
    const result = await spawnInstancesNoSync(taskDef, 'case-009', 10, { tracker });

    // Fail half, complete half
    for (let i = 0; i < 10; i++) {
      const shouldFail = i % 2 === 0;
      await executeInstance(result.instanceIds[i], tracker, shouldFail);
    }

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.statusCounts.completed).toBe(5);
    expect(aggregate.statusCounts.failed).toBe(5);
    expect(aggregate.allCompleted).toBe(true);
    expect(aggregate.anyFailed).toBe(true);
  });

  it('should support instance cancellation', async () => {
    const taskDef = createTestTaskDef('Task10');
    const result = await spawnInstancesNoSync(taskDef, 'case-010', 3, { tracker });

    enableInstance(result.instanceIds[0], tracker);
    startInstance(result.instanceIds[0], tracker);
    cancelInstance(result.instanceIds[0], tracker);

    const instance = tracker.getInstance(result.instanceIds[0]);
    expect(instance.status).toBe(InstanceStatus.CANCELLED);
    expect(instance.completedAt).toBeDefined();
  });
});

// =============================================================================
// Concurrency Tests
// =============================================================================

describe('WP12 - High Concurrency', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should spawn 100 instances efficiently', async () => {
    const taskDef = createTestTaskDef('Task11');
    const startTime = performance.now();

    const result = await spawnInstancesNoSync(taskDef, 'case-011', 100, {
      tracker,
      generateReceipts: true,
      useBatchReceipts: true,
    });

    const duration = performance.now() - startTime;

    expect(result.instanceIds).toHaveLength(100);
    expect(result.instances).toHaveLength(100);
    expect(result.instanceReceipts).toHaveLength(100);
    expect(duration).toBeLessThan(1000); // Should complete in <1s

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.totalInstances).toBe(100);
    expect(aggregate.statusCounts.spawned).toBe(100);
  });

  it('should handle concurrent completion of 100 instances', async () => {
    const taskDef = createTestTaskDef('Task12');
    const result = await spawnInstancesNoSync(taskDef, 'case-012', 100, { tracker });

    // Complete all instances concurrently
    const completions = result.instanceIds.map(id =>
      executeInstance(id, tracker, false)
    );

    await Promise.all(completions);

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.statusCounts.completed).toBe(100);
    expect(aggregate.allCompleted).toBe(true);
    expect(areAllInstancesComplete(result.parentTaskId, tracker)).toBe(true);
  });

  it('should maintain correct state with concurrent updates', async () => {
    const taskDef = createTestTaskDef('Task13');
    const result = await spawnInstancesNoSync(taskDef, 'case-013', 50, { tracker });

    // Update half to enabled, half to active concurrently
    const updates = result.instanceIds.map((id, i) => {
      if (i < 25) {
        return Promise.resolve(enableInstance(id, tracker));
      } else {
        return Promise.resolve().then(() => {
          enableInstance(id, tracker);
          startInstance(id, tracker);
        });
      }
    });

    await Promise.all(updates);

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.statusCounts.enabled).toBe(25);
    expect(aggregate.statusCounts.active).toBe(25);
  });
});

// =============================================================================
// Receipt Chain Validation Tests
// =============================================================================

describe('WP12 - Receipt Chain Validation', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should create valid receipt chain for instances', async () => {
    const taskDef = createTestTaskDef('Task14');
    const result = await spawnInstancesNoSync(taskDef, 'case-014', 5, {
      tracker,
      generateReceipts: true,
      useBatchReceipts: false, // Test individual receipts
    });

    // Verify receipt chain
    for (let i = 0; i < result.instanceReceipts.length; i++) {
      const receipt = result.instanceReceipts[i];

      // Verify basic structure
      expect(receipt.id).toBeDefined();
      expect(receipt.payloadHash).toHaveLength(64);
      expect(receipt.receiptHash).toHaveLength(64);

      // First receipt has no previous
      if (i === 0) {
        expect(receipt.previousReceiptHash).toBeNull();
      } else {
        // Subsequent receipts chain to previous
        expect(receipt.previousReceiptHash).toBe(
          result.instanceReceipts[i - 1].receiptHash
        );
      }
    }
  });

  it('should create Merkle root in aggregate receipt', async () => {
    const taskDef = createTestTaskDef('Task15');
    const result = await spawnInstancesNoSync(taskDef, 'case-015', 4, {
      tracker,
      generateReceipts: true,
    });

    const aggregateReceipt = result.aggregateReceipt;
    expect(aggregateReceipt.payload.context.merkleRoot).toBeDefined();
    expect(aggregateReceipt.payload.context.merkleRoot).toHaveLength(64);
    expect(aggregateReceipt.payload.context.instanceIds).toEqual(result.instanceIds);
  });

  it('should include MI metadata in receipts', async () => {
    const taskDef = createTestTaskDef('Task16');
    const result = await spawnInstancesNoSync(taskDef, 'case-016', 3, {
      tracker,
      generateReceipts: true,
    });

    for (let i = 0; i < result.instanceReceipts.length; i++) {
      const receipt = result.instanceReceipts[i];
      const context = receipt.payload.context;

      expect(context.pattern).toBe('WP12');
      expect(context.instanceIndex).toBe(i);
      expect(context.totalInstances).toBe(3);
      expect(context.parentTaskId).toBe(result.parentTaskId);
    }
  });
});

// =============================================================================
// Aggregate Status Tests
// =============================================================================

describe('WP12 - Aggregate Status Queries', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should compute completion percentage correctly', async () => {
    const taskDef = createTestTaskDef('Task17');
    const result = await spawnInstancesNoSync(taskDef, 'case-017', 10, { tracker });

    expect(getCompletionPercentage(result.parentTaskId, tracker)).toBe(0);

    // Complete 5/10
    for (let i = 0; i < 5; i++) {
      await executeInstance(result.instanceIds[i], tracker);
    }

    expect(getCompletionPercentage(result.parentTaskId, tracker)).toBe(50);

    // Complete remaining
    for (let i = 5; i < 10; i++) {
      await executeInstance(result.instanceIds[i], tracker);
    }

    expect(getCompletionPercentage(result.parentTaskId, tracker)).toBe(100);
  });

  it('should provide detailed aggregate status', async () => {
    const taskDef = createTestTaskDef('Task18');
    const result = await spawnInstancesNoSync(taskDef, 'case-018', 6, { tracker });

    // Create mixed state
    enableInstance(result.instanceIds[0], tracker);
    enableInstance(result.instanceIds[1], tracker);
    startInstance(result.instanceIds[1], tracker);
    await executeInstance(result.instanceIds[2], tracker, false); // complete
    await executeInstance(result.instanceIds[3], tracker, true);  // fail

    const aggregate = getAggregateStatus(result.parentTaskId, tracker);

    expect(aggregate.statusCounts.spawned).toBe(2);
    expect(aggregate.statusCounts.enabled).toBe(1);
    expect(aggregate.statusCounts.active).toBe(1);
    expect(aggregate.statusCounts.completed).toBe(1);
    expect(aggregate.statusCounts.failed).toBe(1);

    expect(aggregate.instances.spawned).toHaveLength(2);
    expect(aggregate.instances.enabled).toHaveLength(1);
    expect(aggregate.instances.active).toHaveLength(1);
    expect(aggregate.instances.completed).toHaveLength(1);
    expect(aggregate.instances.failed).toHaveLength(1);

    expect(aggregate.allCompleted).toBe(false);
    expect(aggregate.anyFailed).toBe(true);
  });

  it('should track timing correctly', async () => {
    const taskDef = createTestTaskDef('Task19');
    const result = await spawnInstancesNoSync(taskDef, 'case-019', 3, { tracker });

    const aggregate1 = getAggregateStatus(result.parentTaskId, tracker);
    expect(aggregate1.firstSpawnedAt).toBeDefined();
    expect(aggregate1.lastCompletedAt).toBeUndefined();

    // Complete instances
    await executeInstance(result.instanceIds[0], tracker);
    await executeInstance(result.instanceIds[1], tracker);
    await executeInstance(result.instanceIds[2], tracker);

    const aggregate2 = getAggregateStatus(result.parentTaskId, tracker);
    expect(aggregate2.firstSpawnedAt).toBeDefined();
    expect(aggregate2.lastCompletedAt).toBeDefined();
    expect(aggregate2.lastCompletedAt).toBeGreaterThan(aggregate2.firstSpawnedAt);
  });
});

// =============================================================================
// Edge Cases
// =============================================================================

describe('WP12 - Edge Cases', () => {
  let tracker;

  beforeEach(() => {
    tracker = new MultiInstanceTracker();
  });

  it('should handle single instance spawn', async () => {
    const taskDef = createTestTaskDef('Task20');
    const result = await spawnInstancesNoSync(taskDef, 'case-020', 1, { tracker });

    expect(result.instanceIds).toHaveLength(1);
    expect(result.instances).toHaveLength(1);

    const aggregate = tracker.getAggregateStatus(result.parentTaskId);
    expect(aggregate.totalInstances).toBe(1);
  });

  it('should support custom instance ID prefix', async () => {
    const taskDef = createTestTaskDef('Task21');
    const result = await spawnInstancesNoSync(taskDef, 'case-021', 3, {
      tracker,
      instanceIdPrefix: 'custom-prefix',
    });

    for (const instanceId of result.instanceIds) {
      expect(instanceId).toContain('custom-prefix');
    }
  });

  it('should handle empty base input data', async () => {
    const taskDef = createTestTaskDef('Task22');
    const result = await spawnInstancesNoSync(taskDef, 'case-022', 2, {
      tracker,
      baseInputData: {},
    });

    for (const instance of result.instances) {
      expect(instance.inputData.__mi_metadata).toBeDefined();
    }
  });

  it('should support receipt-less spawning', async () => {
    const taskDef = createTestTaskDef('Task23');
    const result = await spawnInstancesNoSync(taskDef, 'case-023', 5, {
      tracker,
      generateReceipts: false,
    });

    expect(result.instanceReceipts).toBeUndefined();
    expect(result.aggregateReceipt).toBeUndefined();
    expect(result.instances).toHaveLength(5);
  });
});
