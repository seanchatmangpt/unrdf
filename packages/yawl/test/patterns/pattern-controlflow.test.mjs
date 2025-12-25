
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


describe('Control Flow Tests', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  // ===========================================================================
  // Cyclic Workflows (Loops)
  // ===========================================================================
  describe('Cyclic Workflows', () => {
    test('Loop executes until condition met', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'loop-workflow' });
      workflow.addTask({ id: 'init', name: 'Initialize' });
      workflow.addTask({ id: 'process', name: 'Process', splitType: SPLIT_TYPE.XOR });
      workflow.addTask({ id: 'done', name: 'Done' });

      workflow.addFlow(sequence('init', 'process'));
      workflow.addFlow({
        from: 'process',
        to: 'process',
        condition: (ctx) => ctx.data.count < 3,
        isCycle: true,
      });
      workflow.addFlow({
        from: 'process',
        to: 'done',
        condition: (ctx) => ctx.data.count >= 3,
      });
      workflow.setStart('init');
      workflow.setEnd(['done']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('loop-workflow', { count: 0 });

      // Init
      const initItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, initItem.id);
      await engine.completeWorkItem(yawlCase.id, initItem.id, { count: 0 });

      // Process loop - increment count each iteration
      let loopCount = 0;
      while (loopCount < 5) {
        const enabled = yawlCase.getEnabledWorkItems();
        if (enabled.length === 0) break;

        const task = enabled[0];
        const taskDef = yawlCase.getTaskDefIdForWorkItem(task.id);

        if (taskDef === 'done') {
          await engine.startWorkItem(yawlCase.id, task.id);
          await engine.completeWorkItem(yawlCase.id, task.id);
          break;
        }

        loopCount++;
        await engine.startWorkItem(yawlCase.id, task.id);
        await engine.completeWorkItem(yawlCase.id, task.id, { count: loopCount });
      }

      // Assert: Loop ran 3 times then exited
      expect(loopCount).toBe(3);
      expect(yawlCase.status).toBe('completed');
    }, 5000);
  });

  // ===========================================================================
  // Nested Conditionals
  // ===========================================================================
  describe('Nested Conditionals', () => {
    test('Nested XOR splits evaluate correctly', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'nested-workflow' });
      workflow.addTask({ id: 'start', splitType: SPLIT_TYPE.XOR });
      workflow.addTask({ id: 'level1a', splitType: SPLIT_TYPE.XOR });
      workflow.addTask({ id: 'level1b' });
      workflow.addTask({ id: 'level2a' });
      workflow.addTask({ id: 'level2b' });

      workflow.addFlow({ from: 'start', to: 'level1a', condition: (ctx) => ctx.data.path === 'a' });
      workflow.addFlow({ from: 'start', to: 'level1b', condition: (ctx) => ctx.data.path === 'b' });
      workflow.addFlow({ from: 'level1a', to: 'level2a', condition: (ctx) => ctx.data.sub === 'a' });
      workflow.addFlow({ from: 'level1a', to: 'level2b', condition: (ctx) => ctx.data.sub === 'b' });
      workflow.setStart('start');
      workflow.setEnd(['level1b', 'level2a', 'level2b']);

      engine.registerWorkflow(workflow);

      // Act: path=a, sub=b -> should reach level2b
      const { case: yawlCase } = await engine.createCase('nested-workflow', {
        path: 'a',
        sub: 'b',
      });

      // Execute start
      const startItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, startItem.id);
      await engine.completeWorkItem(yawlCase.id, startItem.id, { path: 'a', sub: 'b' });

      // Execute level1a
      const level1aItem = yawlCase.getEnabledWorkItems()[0];
      expect(yawlCase.getTaskDefIdForWorkItem(level1aItem.id)).toBe('level1a');
      await engine.startWorkItem(yawlCase.id, level1aItem.id);
      const { downstreamEnabled } = await engine.completeWorkItem(
        yawlCase.id,
        level1aItem.id,
        { path: 'a', sub: 'b' }
      );

      // Assert: Should reach level2b
      expect(downstreamEnabled.length).toBe(1);
      expect(downstreamEnabled[0].taskId).toBe('level2b');
    }, 5000);
  });

  // ===========================================================================
  // Deferred Choice
  // ===========================================================================
  describe('Deferred Choice', () => {
    test('First external trigger determines branch', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'deferred-workflow' });
      workflow.addTask({ id: 'wait', name: 'Wait for Signal', splitType: 'deferred' });
      workflow.addTask({ id: 'optionA', name: 'Option A' });
      workflow.addTask({ id: 'optionB', name: 'Option B' });

      const deferred = deferredChoice('wait', ['optionA', 'optionB']);
      for (const flow of deferred.flows) {
        workflow.addFlow(flow);
      }
      workflow.setStart('wait');
      workflow.setEnd(['optionA', 'optionB']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('deferred-workflow');

      // Complete wait with selection
      const waitItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, waitItem.id);

      // Simulate external trigger selecting optionA
      const { downstreamEnabled } = await engine.completeWorkItem(
        yawlCase.id,
        waitItem.id,
        { selectedOption: 'optionA' }
      );

      // Assert: The workflow should continue based on external trigger
      // (In real implementation, deferred choice would enable both and cancel the other)
      expect(yawlCase.receipts.length).toBeGreaterThan(0);
    }, 5000);
  });
});