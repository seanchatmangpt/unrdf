/**
 * Multiple Instance Stress Tests
 *
 * Performance and scalability tests for MI patterns under load.
 * Validates system behavior with high instance counts.
 */

import { describe, test, expect, beforeEach } from 'vitest';
import {
  YawlEngine,
  YawlWorkflow,
  SPLIT_TYPE,
  JOIN_TYPE,
  sequence,
} from '../../src/index.mjs';

function createTestEngine() {
  return new YawlEngine({ nodeId: `stress-${Date.now()}` });
}

function createTestWorkflow(config = {}) {
  return new YawlWorkflow({
    id: config.id ?? `stress-wf-${Date.now()}`,
    name: config.name ?? 'Stress Test',
    version: '1.0.0',
    tasks: config.tasks ?? [],
    ...config,
  });
}

/**
 * Measure execution time
 */
async function measureTime(fn) {
  const start = performance.now();
  const result = await fn();
  const duration = performance.now() - start;
  return { result, duration };
}

describe('WP12 Stress Tests - High Volume Without Synchronization', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('1000 instances completes under 5 seconds', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp12-1000' });

    workflow.addTask({ id: 'split' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 1000,
        maximum: 1000,
        creationType: 'static',
        instanceSync: 'XOR', // No sync
      },
    });
    workflow.addTask({ id: 'done' });

    workflow.addFlow(sequence('split', 'process'));
    workflow.addFlow(sequence('process', 'done'));
    workflow.setStart('split');
    workflow.setEnd(['done']);

    engine.registerWorkflow(workflow);

    // Act
    const { result, duration: createDuration } = await measureTime(async () => {
      return engine.createCase('wp12-1000');
    });

    const { case: yawlCase } = result;

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);

    const { duration: spawnDuration } = await measureTime(async () => {
      await engine.completeTask(yawlCase.id, splitItem.id, {
        instanceCount: 1000,
      });
    });

    // Assert: 1000 instances spawned
    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(1000);

    // Complete first instance
    const { duration: completeDuration } = await measureTime(async () => {
      await engine.startTask(yawlCase.id, instances[0].id);
      await engine.completeTask(yawlCase.id, instances[0].id);
    });

    // Assert: Performance targets
    expect(createDuration).toBeLessThan(100); // Create case < 100ms
    expect(spawnDuration).toBeLessThan(2000); // Spawn 1000 instances < 2s
    expect(completeDuration).toBeLessThan(50); // Complete one instance < 50ms

    // Total time under 5s budget
    const totalTime = createDuration + spawnDuration + completeDuration;
    expect(totalTime).toBeLessThan(5000);

    console.log(`Stress Test WP12-1000: Create ${createDuration.toFixed(2)}ms, Spawn ${spawnDuration.toFixed(2)}ms, Complete ${completeDuration.toFixed(2)}ms`);
  });

  test('Memory efficient - no leak with 1000 instances', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp12-memory' });

    workflow.addTask({ id: 'process', kind: 'multiple' });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act: Measure memory before
    const memBefore = process.memoryUsage().heapUsed;

    const { case: yawlCase } = await engine.createCase('wp12-memory', {
      instanceCount: 1000,
    });

    // Complete all 1000 instances
    const instances = yawlCase.getEnabledWorkItems();
    for (let i = 0; i < 1000; i += 100) {
      // Batch complete 100 at a time
      const batch = instances.slice(i, i + 100);
      await Promise.all(
        batch.map(async inst => {
          await engine.startTask(yawlCase.id, inst.id);
          await engine.completeTask(yawlCase.id, inst.id);
        })
      );
    }

    // Measure memory after
    if (global.gc) global.gc();
    const memAfter = process.memoryUsage().heapUsed;
    const memIncrease = memAfter - memBefore;

    // Assert: Memory increase < 100MB for 1000 instances
    expect(memIncrease).toBeLessThan(100 * 1024 * 1024);

    console.log(`Memory increase: ${(memIncrease / 1024 / 1024).toFixed(2)}MB`);
  });
});

describe('WP13 Stress Tests - Barrier Synchronization Under Load', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('100 instances with AND-join barrier < 3 seconds', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp13-100-barrier' });

    workflow.addTask({ id: 'split' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 100,
        maximum: 100,
        creationType: 'static',
        instanceSync: 'AND', // ALL must complete
      },
    });
    workflow.addTask({ id: 'merge', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('split', 'process'));
    workflow.addFlow(sequence('process', 'merge'));
    workflow.setStart('split');
    workflow.setEnd(['merge']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('wp13-100-barrier');

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id, {
      instanceCount: 100,
    });

    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(100);

    // Complete all 100 instances
    const { duration } = await measureTime(async () => {
      for (const inst of instances) {
        await engine.startTask(yawlCase.id, inst.id);
        await engine.completeTask(yawlCase.id, inst.id);
      }
    });

    // Assert: Merge enabled after all complete
    const mergeItems = yawlCase.getEnabledWorkItems();
    expect(mergeItems.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(mergeItems[0].id)).toBe('merge');

    // Assert: Completion under 3 seconds
    expect(duration).toBeLessThan(3000);

    console.log(`WP13 Barrier (100 instances): ${duration.toFixed(2)}ms`);
  });

  test('Concurrent barrier evaluation correctness', async () => {
    // Arrange: Multiple MI tasks converging
    const workflow = createTestWorkflow({ id: 'wp13-concurrent' });

    workflow.addTask({ id: 'split', splitType: SPLIT_TYPE.AND });
    workflow.addTask({
      id: 'processA',
      kind: 'multiple',
      multipleInstance: {
        minimum: 50,
        maximum: 50,
        instanceSync: 'AND',
      },
    });
    workflow.addTask({
      id: 'processB',
      kind: 'multiple',
      multipleInstance: {
        minimum: 50,
        maximum: 50,
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'merge', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('split', 'processA'));
    workflow.addFlow(sequence('split', 'processB'));
    workflow.addFlow(sequence('processA', 'merge'));
    workflow.addFlow(sequence('processB', 'merge'));
    workflow.setStart('split');
    workflow.setEnd(['merge']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('wp13-concurrent');

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id);

    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(100); // 50 A + 50 B

    const instancesA = instances.filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'processA'
    );
    const instancesB = instances.filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'processB'
    );

    // Complete A instances
    for (const inst of instancesA) {
      await engine.startTask(yawlCase.id, inst.id);
      await engine.completeTask(yawlCase.id, inst.id);
    }

    // Merge NOT enabled yet (B not complete)
    let mergeItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'merge'
    );
    expect(mergeItems.length).toBe(0);

    // Complete B instances
    for (const inst of instancesB) {
      await engine.startTask(yawlCase.id, inst.id);
      await engine.completeTask(yawlCase.id, inst.id);
    }

    // Assert: NOW merge enabled (both barriers satisfied)
    mergeItems = yawlCase.getEnabledWorkItems();
    expect(mergeItems.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(mergeItems[0].id)).toBe('merge');
  });
});

describe('WP15 Stress Tests - Dynamic Instance Addition', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Add 500 instances dynamically during execution', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp15-500-dynamic' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        creationType: 'dynamic',
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'end' });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act: Start with 100 instances
    const { case: yawlCase } = await engine.createCase('wp15-500-dynamic');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      instanceCount: 100,
    });

    let instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(100);

    // Start 50 instances (not complete)
    for (let i = 0; i < 50; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
    }

    // Dynamically add 400 more instances
    const { duration: addDuration } = await measureTime(async () => {
      for (let i = 0; i < 400; i++) {
        await engine.addMIInstance(yawlCase.id, 'process', { id: 100 + i });
      }
    });

    // Assert: 500 total instances
    instances = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'process'
    );
    expect(instances.length).toBe(500);

    // Assert: Addition performance
    expect(addDuration).toBeLessThan(2000); // Add 400 instances < 2s

    console.log(`Add 400 instances: ${addDuration.toFixed(2)}ms`);
  });

  test('Concurrent add/complete operations - race condition safe', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp15-race' });

    workflow.addTask({ id: 'process', kind: 'multiple' });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act: Initial 50 instances
    const { case: yawlCase } = await engine.createCase('wp15-race', {
      instanceCount: 50,
    });

    const instances = yawlCase.getEnabledWorkItems();

    // Concurrent operations: complete 25, add 25 new
    const operations = [];

    // Complete first 25
    for (let i = 0; i < 25; i++) {
      operations.push(
        (async () => {
          await engine.startTask(yawlCase.id, instances[i].id);
          await engine.completeTask(yawlCase.id, instances[i].id);
        })()
      );
    }

    // Add 25 new instances concurrently
    for (let i = 0; i < 25; i++) {
      operations.push(
        engine.addMIInstance(yawlCase.id, 'process', { id: 50 + i })
      );
    }

    // Wait for all operations
    await Promise.all(operations);

    // Assert: 50 instances (25 completed, 25 original pending, 25 new)
    const remaining = yawlCase.getEnabledWorkItems();
    expect(remaining.length).toBe(50);

    // Complete remaining
    for (const inst of remaining) {
      await engine.startTask(yawlCase.id, inst.id);
      await engine.completeTask(yawlCase.id, inst.id);
    }

    expect(yawlCase.status).toBe('completed');
  });
});

describe('Cancellation Propagation Stress', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Cancel region with 500 MI instances < 1 second', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'cancel-500' });

    workflow.addTask({ id: 'split' });
    workflow.addTask({
      id: 'processA',
      kind: 'multiple',
      cancellationRegion: 'region1',
      multipleInstance: {
        minimum: 250,
        maximum: 250,
        creationType: 'static',
      },
    });
    workflow.addTask({
      id: 'processB',
      kind: 'multiple',
      cancellationRegion: 'region1',
      multipleInstance: {
        minimum: 250,
        maximum: 250,
        creationType: 'static',
      },
    });
    workflow.addTask({ id: 'safe' }); // Not in region

    workflow.addFlow(sequence('split', 'processA'));
    workflow.addFlow(sequence('split', 'processB'));
    workflow.addFlow(sequence('split', 'safe'));
    workflow.setStart('split');
    workflow.setEnd(['processA', 'processB', 'safe']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('cancel-500');

    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id);

    // Assert: 500 instances + 1 safe task
    let instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(501);

    // Cancel region
    const { duration, result } = await measureTime(async () => {
      return engine.cancelRegion(yawlCase.id, 'region1', 'Cancelled');
    });

    const { cancelled } = result;

    // Assert: 500 cancelled, 1 safe task remains
    expect(cancelled.length).toBe(500);
    expect(duration).toBeLessThan(1000);

    const remaining = yawlCase.getEnabledWorkItems();
    expect(remaining.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(remaining[0].id)).toBe('safe');

    console.log(`Cancel 500 instances: ${duration.toFixed(2)}ms`);
  });

  test('Cascading cancellation through nested MI', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'cascade-cancel' });

    workflow.addTask({ id: 'parent', kind: 'multiple', cancellationRegion: 'all' });
    workflow.addTask({ id: 'child', kind: 'multiple', cancellationRegion: 'all' });
    workflow.addFlow(sequence('parent', 'child'));
    workflow.setStart('parent');
    workflow.setEnd(['child']);

    engine.registerWorkflow(workflow);

    // Act: 10 parent instances, each spawns 10 child instances = 100 total
    const { case: yawlCase } = await engine.createCase('cascade-cancel', {
      parentCount: 10,
      childCount: 10,
    });

    // Complete parents to spawn children
    const parents = yawlCase.getEnabledWorkItems();
    for (const parent of parents) {
      await engine.startTask(yawlCase.id, parent.id);
      await engine.completeTask(yawlCase.id, parent.id, {
        childCount: 10,
      });
    }

    // Assert: 100 child instances
    const children = yawlCase.getEnabledWorkItems();
    expect(children.length).toBe(100);

    // Cancel all
    const { cancelled } = await engine.cancelRegion(
      yawlCase.id,
      'all',
      'Cascading cancel'
    );

    // Assert: All 100 cancelled
    expect(cancelled.length).toBe(100);
    expect(yawlCase.getEnabledWorkItems().length).toBe(0);
  });
});

describe('Receipt Performance Under Load', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Receipt generation for 100 MI instances < 500ms', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'receipt-100' });

    workflow.addTask({ id: 'process', kind: 'multiple' });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase, receipt: createReceipt } = await engine.createCase(
      'receipt-100',
      { instanceCount: 100 }
    );

    expect(createReceipt.valid).toBe(true);

    const instances = yawlCase.getEnabledWorkItems();

    // Complete all instances and measure receipt generation
    const { duration } = await measureTime(async () => {
      for (const inst of instances) {
        await engine.startTask(yawlCase.id, inst.id);
        await engine.completeTask(yawlCase.id, inst.id);
      }
    });

    // Assert: Receipt generation performant
    expect(duration).toBeLessThan(500);

    // Verify all receipts valid
    const receipts = yawlCase.receipts;
    expect(receipts.length).toBeGreaterThan(100);

    for (const receipt of receipts) {
      expect(receipt.valid).toBe(true);
    }

    console.log(`Receipt gen for 100 instances: ${duration.toFixed(2)}ms`);
  });

  test('Receipt chain verification scales linearly', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'receipt-chain' });

    workflow.addTask({ id: 'process', kind: 'multiple' });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    const sizes = [10, 50, 100];
    const timings = [];

    for (const size of sizes) {
      // Act
      const { case: yawlCase } = await engine.createCase('receipt-chain', {
        instanceCount: size,
      });

      const instances = yawlCase.getEnabledWorkItems();
      for (const inst of instances) {
        await engine.startTask(yawlCase.id, inst.id);
        await engine.completeTask(yawlCase.id, inst.id);
      }

      // Measure chain verification
      const receipts = yawlCase.receipts;
      const { duration } = await measureTime(async () => {
        for (let i = 1; i < receipts.length; i++) {
          await receipts[i].verifyChain(receipts[i - 1]);
        }
      });

      timings.push({ size, duration });
    }

    // Assert: Linear scaling (within 2x tolerance)
    const ratio1 = timings[1].duration / timings[0].duration;
    const ratio2 = timings[2].duration / timings[1].duration;

    expect(ratio1).toBeLessThan(10); // 50 vs 10 < 10x slower
    expect(ratio2).toBeLessThan(5); // 100 vs 50 < 5x slower

    console.log('Receipt chain timings:', timings);
  });
});
