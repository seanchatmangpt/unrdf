
import { createTestWorkflow, createTestEngine, measureTime } from './test-utils.mjs';

/**
 * YAWL Workflow Patterns Test Suite
 *
 * Comprehensive tests for Van der Aalst's 20 Core Workflow Patterns
 * with KGC-4D time-travel and receipt verification
 *
 * @see https://www.workflowpatterns.com/
 * @author UNRDF Team
 *
 * Test Coverage:
 * - WP1-WP7: Core control flow patterns
 * - Control Flow: Cycles, nested conditionals, deferred choice
 * - Resources: Allocation, prioritization, exhaustion, roles
 * - Cancellation: Single, region, timeout, circuit breaker
 * - Time-Travel: Checkpoints, replay, hash verification
 * - Receipts: Hashes, SPARQL, actor/timestamp, chain verification
 * - Integration: Full lifecycle, error paths, resource contention
 */



    test('completeTask latency < 100ms', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'perf-complete-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      const { case: yawlCase } = await engine.createCase('perf-complete-workflow');
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);

      // Act & Assert
      const { duration } = await measureTime(async () => {
        return engine.completeTask(yawlCase.id, workItemA.id, { result: 'done' });
      });

      expect(duration).toBeLessThan(100);
    }, 5000);

    test('time-travel reconstruction < 100ms', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'perf-timetravel-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      const { case: yawlCase } = await engine.createCase('perf-timetravel-workflow');
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

      await engine.checkpoint('test');

      // Act & Assert
      const { duration } = await measureTime(async () => {
        return engine.reconstructCase(yawlCase.id, BigInt(Date.now()) * 1000000n);
      });

      expect(duration).toBeLessThan(100);
    }, 5000);
  });
});

// =============================================================================
// ADDITIONAL PATTERN TESTS (WP8-WP20)
// =============================================================================

describe('Additional Workflow Patterns (WP8-WP20)', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  // ===========================================================================
  // WP8: Multi-Merge
  // ===========================================================================
  test('WP8: Multi-Merge - Multiple tokens merge without synchronization', async () => {
    const workflow = createTestWorkflow({ id: 'multi-merge-workflow' });
    workflow.addTask({ id: 'A', splitType: SPLIT_TYPE.AND });
    workflow.addTask({ id: 'B' });
    workflow.addTask({ id: 'C' });
    workflow.addTask({ id: 'D', joinType: JOIN_TYPE.XOR }); // Multi-merge

    workflow.addFlow(sequence('A', 'B'));
    workflow.addFlow(sequence('A', 'C'));
    workflow.addFlow(sequence('B', 'D'));
    workflow.addFlow(sequence('C', 'D'));
    workflow.setStart('A');
    workflow.setEnd(['D']);

    engine.registerWorkflow(workflow);

    const { case: yawlCase } = await engine.createCase('multi-merge-workflow');

    // Complete A to spawn B and C
    const workItemA = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, workItemA.id);
    await engine.completeTask(yawlCase.id, workItemA.id);

    // Complete B - D should be enabled (XOR-join)
    const workItemB = yawlCase.getEnabledWorkItems().find(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B'
    );
    await engine.startTask(yawlCase.id, workItemB.id);
    const { downstreamEnabled } = await engine.completeTask(yawlCase.id, workItemB.id);

    expect(downstreamEnabled.length).toBe(1);
    expect(downstreamEnabled[0].taskId).toBe('D');
  }, 5000);

  // ===========================================================================
  // WP9: Structured Discriminator
  // ===========================================================================
  test('WP9: Structured Discriminator - First of N branches triggers downstream', async () => {
    const workflow = createTestWorkflow({ id: 'discriminator-workflow' });
    workflow.addTask({ id: 'A', splitType: SPLIT_TYPE.AND });
    workflow.addTask({ id: 'B' });
    workflow.addTask({ id: 'C' });
    workflow.addTask({ id: 'D', joinType: JOIN_TYPE.XOR }); // Discriminator

    workflow.addFlow(sequence('A', 'B'));
    workflow.addFlow(sequence('A', 'C'));
    workflow.addFlow(sequence('B', 'D'));
    workflow.addFlow(sequence('C', 'D'));
    workflow.setStart('A');
    workflow.setEnd(['D']);

    engine.registerWorkflow(workflow);

    const { case: yawlCase } = await engine.createCase('discriminator-workflow');

    const workItemA = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, workItemA.id);
    await engine.completeTask(yawlCase.id, workItemA.id);

    // Complete just B - D should be enabled immediately
    const workItemB = yawlCase.getEnabledWorkItems().find(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B'
    );
    await engine.startTask(yawlCase.id, workItemB.id);
    const { downstreamEnabled } = await engine.completeTask(yawlCase.id, workItemB.id);

    expect(downstreamEnabled.length).toBe(1);
  }, 5000);

  // ===========================================================================
  // WP11: Implicit Termination
  // ===========================================================================
  test('WP11: Implicit Termination - Case completes when no more work', async () => {
    const workflow = createTestWorkflow({ id: 'implicit-term-workflow' });
    workflow.addTask({ id: 'A' });
    workflow.setStart('A');
    workflow.setEnd(['A']);

    engine.registerWorkflow(workflow);

    const { case: yawlCase } = await engine.createCase('implicit-term-workflow');

    const workItemA = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, workItemA.id);
    await engine.completeTask(yawlCase.id, workItemA.id);

    // Case should be complete (no more work)
    expect(yawlCase.status).toBe('completed');
    expect(yawlCase.getEnabledWorkItems().length).toBe(0);
  }, 5000);

  // ===========================================================================
  // WP19: Cancel Task
  // ===========================================================================
  test('WP19: Cancel Task - Single task cancellation', async () => {
    const workflow = createTestWorkflow({ id: 'cancel-task-workflow' });
    workflow.addTask({ id: 'A' });
    workflow.setStart('A');
    workflow.setEnd(['A']);

    engine.registerWorkflow(workflow);

    const { case: yawlCase } = await engine.createCase('cancel-task-workflow');

    const workItemA = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, workItemA.id);

    const { task, receipt } = await engine.cancelTask(
      yawlCase.id,
      workItemA.id,
      'User cancelled'
    );

    expect(task.status).toBe(TaskStatus.CANCELLED);
    expect(receipt.valid).toBe(true);
    expect(receipt.action).toBe('cancel');
  }, 5000);

  // ===========================================================================
  // WP20: Cancel Case
  // ===========================================================================
  test('WP20: Cancel Case - All tasks in region cancelled', async () => {
    const workflow = createTestWorkflow({ id: 'cancel-case-workflow' });
    workflow.addTask({ id: 'A', splitType: SPLIT_TYPE.AND, cancellationRegion: 'all' });
    workflow.addTask({ id: 'B', cancellationRegion: 'all' });
    workflow.addTask({ id: 'C', cancellationRegion: 'all' });

    workflow.addFlow(sequence('A', 'B'));
    workflow.addFlow(sequence('A', 'C'));
    workflow.setStart('A');
    workflow.setEnd(['B', 'C']);

    engine.registerWorkflow(workflow);

    const { case: yawlCase } = await engine.createCase('cancel-case-workflow');

    const workItemA = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, workItemA.id);
    await engine.completeTask(yawlCase.id, workItemA.id);

    expect(yawlCase.getEnabledWorkItems().length).toBe(2);

    // Cancel all region
    const { cancelled } = await engine.cancelRegion(yawlCase.id, 'all', 'Case cancelled');

    expect(cancelled.length).toBe(2);
    expect(yawlCase.getEnabledWorkItems().length).toBe(0);
  }, 5000);
});