
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


describe('Cancellation Tests', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  // ===========================================================================
  // Cancel Single Work Item
  // ===========================================================================
  describe('Cancel Single Work Item', () => {
    test('Cancelling one task does not affect siblings', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'cancel-single-workflow' });
      workflow.addTask({ id: 'A', splitType: SPLIT_TYPE.AND });
      workflow.addTask({ id: 'B' });
      workflow.addTask({ id: 'C' });

      workflow.addFlow(sequence('A', 'B'));
      workflow.addFlow(sequence('A', 'C'));
      workflow.setStart('A');
      workflow.setEnd(['B', 'C']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('cancel-single-workflow');

      // Complete A to spawn B and C
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

      // Both B and C should be enabled
      const enabledBC = yawlCase.getEnabledWorkItems();
      expect(enabledBC.length).toBe(2);

      // Cancel only B
      const workItemB = enabledBC.find(w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B');
      const { task: cancelledTask, receipt } = await engine.cancelTask(
        yawlCase.id,
        workItemB.id,
        'User requested cancellation'
      );

      // Assert: B cancelled, C still enabled
      expect(cancelledTask.status).toBe(TaskStatus.CANCELLED);
      expect(receipt.valid).toBe(true);

      const stillEnabled = yawlCase.getEnabledWorkItems();
      expect(stillEnabled.length).toBe(1);
      expect(yawlCase.getTaskDefIdForWorkItem(stillEnabled[0].id)).toBe('C');
    }, 5000);
  });

  // ===========================================================================
  // Cancel Region
  // ===========================================================================
  describe('Cancel Region', () => {
    test('Cancels all tasks in region', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'cancel-region-workflow' });
      workflow.addTask({ id: 'A', splitType: SPLIT_TYPE.AND });
      workflow.addTask({ id: 'B', cancellationRegion: 'region1' });
      workflow.addTask({ id: 'C', cancellationRegion: 'region1' });
      workflow.addTask({ id: 'D' }); // Not in region

      workflow.addFlow(sequence('A', 'B'));
      workflow.addFlow(sequence('A', 'C'));
      workflow.addFlow(sequence('A', 'D'));
      workflow.setStart('A');
      workflow.setEnd(['B', 'C', 'D']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('cancel-region-workflow');

      // Complete A to spawn B, C, D
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

      expect(yawlCase.getEnabledWorkItems().length).toBe(3);

      // Cancel region1
      const { cancelled, receipts } = await engine.cancelRegion(
        yawlCase.id,
        'region1',
        'Region cancelled'
      );

      // Assert: B and C cancelled, D still enabled
      expect(cancelled.length).toBe(2);
      expect(receipts.length).toBe(2);

      const stillEnabled = yawlCase.getEnabledWorkItems();
      expect(stillEnabled.length).toBe(1);
      expect(yawlCase.getTaskDefIdForWorkItem(stillEnabled[0].id)).toBe('D');
    }, 5000);
  });

  // ===========================================================================
  // Timeout Cancellation
  // ===========================================================================
  describe('Timeout Cancellation', () => {
    test('Auto-cancels task on timeout', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'timeout-workflow' });
      workflow.addTask({ id: 'A', timeout: 100 }); // 100ms timeout
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('timeout-workflow');
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);

      // Simulate timeout
      const { task, receipt } = await engine.timeoutWorkItem(yawlCase.id, workItemA.id);

      // Assert
      expect(task.status).toBe(TaskStatus.TIMEOUT);
      expect(receipt.action).toBe('timeout');
      expect(receipt.valid).toBe(true);
    }, 5000);
  });

  // ===========================================================================
  // Circuit Breaker
  // ===========================================================================
  describe('Circuit Breaker', () => {
    test('Disabling circuit breaker cancels pending tasks', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'circuit-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.addTask({ id: 'B' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.setStart('A');
      workflow.setEnd(['B']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('circuit-workflow');

      // Complete A to enable B
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

      // B should be enabled
      const workItemB = yawlCase.getEnabledWorkItems()[0];
      expect(yawlCase.getTaskDefIdForWorkItem(workItemB.id)).toBe('B');

      // Disable circuit breaker for B
      const { cancelled } = await engine.setCircuitBreaker(yawlCase.id, 'B', false);

      // Assert: B should be cancelled
      expect(cancelled.length).toBe(1);
      expect(cancelled[0].status).toBe(TaskStatus.CANCELLED);
      expect(yawlCase.getEnabledWorkItems().length).toBe(0);
    }, 5000);
  });
});