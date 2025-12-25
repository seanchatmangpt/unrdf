/**
 * YAWL Workflow Patterns Test Suite - Van der Aalst WP1-WP7
 *
 * Core control flow patterns:
 * - WP1: Sequence
 * - WP2: Parallel Split (AND-split)
 * - WP3: Synchronization (AND-join)
 * - WP4: Exclusive Choice (XOR-split)
 * - WP5: Simple Merge (XOR-join)
 * - WP6: Multi-choice (OR-split)
 * - WP7: Structured Synchronizing Merge (OR-join)
 *
 * @see https://www.workflowpatterns.com/
 * @author UNRDF Team
 */

import { describe, test, expect, beforeEach, afterEach } from 'vitest';
import { mkdtempSync, rmSync, existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';
import {
  createTestWorkflow,
  createTestEngine,
  YawlWorkflow,
  sequence,
  parallelSplit,
  exclusiveChoice,
  simpleMerge,
  multiChoice,
  structuredSyncMerge,
  SPLIT_TYPE,
  JOIN_TYPE,
} from './test-utils.mjs';

// =============================================================================
// VAN DER AALST'S 20 CORE WORKFLOW PATTERNS
// =============================================================================

describe('Van der Aalst Workflow Patterns', () => {
  let engine;
  let tempDir;

  beforeEach(() => {
    tempDir = mkdtempSync(join(tmpdir(), 'yawl-test-'));
    engine = createTestEngine({ gitPath: tempDir });
  });

  afterEach(() => {
    if (tempDir && existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  });

  // ===========================================================================
  // WP1: Sequence
  // ===========================================================================
  describe('WP1: Sequence', () => {
    test('Task A -> B -> C completes sequentially', async () => {
      // Arrange: Create workflow with sequential tasks
      const workflow = createTestWorkflow({ id: 'sequence-workflow' });
      workflow.addTask({ id: 'A', name: 'Task A' });
      workflow.addTask({ id: 'B', name: 'Task B' });
      workflow.addTask({ id: 'C', name: 'Task C' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.addFlow(sequence('B', 'C'));
      workflow.setStart('A');
      workflow.setEnd(['C']);

      engine.registerWorkflow(workflow);

      // Act: Create case and execute sequence
      const { case: yawlCase, receipt: startReceipt } = await engine.createCase('sequence-workflow');

      // Complete A -> should enable B
      const enabledA = yawlCase.getEnabledWorkItems();
      expect(enabledA.length).toBe(1);

      const workItemA = enabledA[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      const { receipt: completeAReceipt, downstreamEnabled } = await engine.completeTask(
        yawlCase.id,
        workItemA.id,
        { result: 'A done' }
      );

      expect(downstreamEnabled.length).toBe(1);
      expect(downstreamEnabled[0].taskId).toBe('B');

      // Complete B -> should enable C
      const enabledB = yawlCase.getEnabledWorkItems();
      expect(enabledB.length).toBe(1);
      const workItemB = enabledB[0];
      await engine.startTask(yawlCase.id, workItemB.id);
      const { downstreamEnabled: downstreamFromB } = await engine.completeTask(
        yawlCase.id,
        workItemB.id,
        { result: 'B done' }
      );

      expect(downstreamFromB.length).toBe(1);
      expect(downstreamFromB[0].taskId).toBe('C');

      // Complete C -> case should complete
      const enabledC = yawlCase.getEnabledWorkItems();
      expect(enabledC.length).toBe(1);
      const workItemC = enabledC[0];
      await engine.startTask(yawlCase.id, workItemC.id);
      await engine.completeTask(yawlCase.id, workItemC.id, { result: 'C done' });

      // Assert: Case completed
      expect(yawlCase.status).toBe('completed');
      expect(yawlCase.receipts.length).toBeGreaterThan(0);

      // Verify receipt chain
      expect(startReceipt.valid).toBe(true);
      expect(completeAReceipt.valid).toBe(true);
    }, 5000);
  });

  // ===========================================================================
  // WP2: Parallel Split (AND-split)
  // ===========================================================================
  describe('WP2: Parallel Split (AND)', () => {
    test('Task A spawns tasks B, C in parallel', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'parallel-workflow' });
      workflow.addTask({ id: 'A', name: 'Task A', splitType: SPLIT_TYPE.AND });
      workflow.addTask({ id: 'B', name: 'Task B' });
      workflow.addTask({ id: 'C', name: 'Task C' });
      workflow.addTask({ id: 'D', name: 'Task D', joinType: JOIN_TYPE.AND });

      const split = parallelSplit('A', ['B', 'C']);
      for (const flow of split.flows) {
        workflow.addFlow(flow);
      }
      workflow.addFlow(sequence('B', 'D'));
      workflow.addFlow(sequence('C', 'D'));
      workflow.setStart('A');
      workflow.setEnd(['D']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('parallel-workflow');

      // Complete A
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      const { downstreamEnabled } = await engine.completeTask(
        yawlCase.id,
        workItemA.id
      );

      // Assert: Both B and C enabled in parallel
      expect(downstreamEnabled.length).toBe(2);
      expect(downstreamEnabled.map(d => d.taskId).sort()).toEqual(['B', 'C']);

      const enabledTasks = yawlCase.getEnabledWorkItems();
      expect(enabledTasks.length).toBe(2);
    }, 5000);
  });

  // ===========================================================================
  // WP3: Synchronization (AND-join)
  // ===========================================================================
  describe('WP3: Synchronization (AND-join)', () => {
    test('Tasks B, C both complete before D starts', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'sync-workflow' });
      workflow.addTask({ id: 'A', name: 'Task A', splitType: SPLIT_TYPE.AND });
      workflow.addTask({ id: 'B', name: 'Task B' });
      workflow.addTask({ id: 'C', name: 'Task C' });
      workflow.addTask({ id: 'D', name: 'Task D', joinType: JOIN_TYPE.AND });

      workflow.addFlow(sequence('A', 'B'));
      workflow.addFlow(sequence('A', 'C'));
      workflow.addFlow(sequence('B', 'D'));
      workflow.addFlow(sequence('C', 'D'));
      workflow.setStart('A');
      workflow.setEnd(['D']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('sync-workflow');

      // Complete A to spawn B and C
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

      // Complete B only - D should NOT be enabled yet
      const enabledBC = yawlCase.getEnabledWorkItems();
      const workItemB = enabledBC.find(w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B');
      await engine.startTask(yawlCase.id, workItemB.id);
      const { downstreamEnabled: afterB } = await engine.completeTask(
        yawlCase.id,
        workItemB.id
      );

      // D should not be enabled yet (waiting for C)
      expect(afterB.length).toBe(0);

      // Complete C - now D should be enabled
      const workItemC = yawlCase.getEnabledWorkItems().find(
        w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'C'
      );
      await engine.startTask(yawlCase.id, workItemC.id);
      const { downstreamEnabled: afterC } = await engine.completeTask(
        yawlCase.id,
        workItemC.id
      );

      // Assert: D enabled after both B and C complete
      expect(afterC.length).toBe(1);
      expect(afterC[0].taskId).toBe('D');
    }, 5000);
  });

  // ===========================================================================
  // WP4: Exclusive Choice (XOR-split)
  // ===========================================================================
  describe('WP4: Exclusive Choice (XOR-split)', () => {
    test('Task A routes to approve or reject based on condition', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'xor-workflow' });
      workflow.addTask({ id: 'review', name: 'Review Request', splitType: SPLIT_TYPE.XOR });
      workflow.addTask({ id: 'approve', name: 'Approve' });
      workflow.addTask({ id: 'reject', name: 'Reject' });

      const choice = exclusiveChoice('review', [
        { taskId: 'approve', condition: (ctx) => ctx.data.approved === true, priority: 0 },
        { taskId: 'reject', condition: (ctx) => ctx.data.approved === false, priority: 1 },
      ]);
      for (const flow of choice.flows) {
        workflow.addFlow(flow);
      }
      workflow.setStart('review');
      workflow.setEnd(['approve', 'reject']);

      engine.registerWorkflow(workflow);

      // Act: Test approval path
      const { case: case1 } = await engine.createCase('xor-workflow', { approved: true });
      const workItem1 = case1.getEnabledWorkItems()[0];
      await engine.startTask(case1.id, workItem1.id);
      const { downstreamEnabled: enabled1 } = await engine.completeTask(
        case1.id,
        workItem1.id,
        { approved: true }
      );

      expect(enabled1.length).toBe(1);
      expect(enabled1[0].taskId).toBe('approve');

      // Act: Test rejection path
      const { case: case2 } = await engine.createCase('xor-workflow', { approved: false });
      const workItem2 = case2.getEnabledWorkItems()[0];
      await engine.startTask(case2.id, workItem2.id);
      const { downstreamEnabled: enabled2 } = await engine.completeTask(
        case2.id,
        workItem2.id,
        { approved: false }
      );

      expect(enabled2.length).toBe(1);
      expect(enabled2[0].taskId).toBe('reject');
    }, 5000);
  });

  // ===========================================================================
  // WP5: Simple Merge (XOR-join)
  // ===========================================================================
  describe('WP5: Simple Merge (XOR-join)', () => {
    test('B or C complete, then D starts', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'merge-workflow' });
      workflow.addTask({ id: 'start', name: 'Start', splitType: SPLIT_TYPE.XOR });
      workflow.addTask({ id: 'B', name: 'Path B' });
      workflow.addTask({ id: 'C', name: 'Path C' });
      workflow.addTask({ id: 'D', name: 'Merge Point', joinType: JOIN_TYPE.XOR });

      workflow.addFlow({
        from: 'start',
        to: 'B',
        condition: (ctx) => ctx.data.path === 'B',
      });
      workflow.addFlow({
        from: 'start',
        to: 'C',
        condition: (ctx) => ctx.data.path === 'C',
      });
      const merge = simpleMerge(['B', 'C'], 'D');
      for (const flow of merge.flows) {
        workflow.addFlow(flow);
      }
      workflow.setStart('start');
      workflow.setEnd(['D']);

      engine.registerWorkflow(workflow);

      // Act: Take path B
      const { case: yawlCase } = await engine.createCase('merge-workflow', { path: 'B' });
      const workItemStart = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemStart.id);
      await engine.completeTask(yawlCase.id, workItemStart.id, { path: 'B' });

      const workItemB = yawlCase.getEnabledWorkItems()[0];
      expect(yawlCase.getTaskDefIdForWorkItem(workItemB.id)).toBe('B');

      await engine.startTask(yawlCase.id, workItemB.id);
      const { downstreamEnabled } = await engine.completeTask(
        yawlCase.id,
        workItemB.id
      );

      // Assert: D enabled after B completes (XOR-join - any one branch)
      expect(downstreamEnabled.length).toBe(1);
      expect(downstreamEnabled[0].taskId).toBe('D');
    }, 5000);
  });

  // ===========================================================================
  // WP6: Multi-choice (OR-split)
  // ===========================================================================
  describe('WP6: Multi-choice (OR-split)', () => {
    test('Task A may spawn 1+ of {B, C, D} based on conditions', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'or-split-workflow' });
      workflow.addTask({ id: 'A', name: 'Decision', splitType: SPLIT_TYPE.OR });
      workflow.addTask({ id: 'B', name: 'Option B' });
      workflow.addTask({ id: 'C', name: 'Option C' });
      workflow.addTask({ id: 'D', name: 'Option D' });

      const orChoice = multiChoice('A', [
        { taskId: 'B', condition: (ctx) => ctx.data.optionB === true },
        { taskId: 'C', condition: (ctx) => ctx.data.optionC === true },
        { taskId: 'D', condition: (ctx) => ctx.data.optionD === true },
      ]);
      for (const flow of orChoice.flows) {
        workflow.addFlow(flow);
      }
      workflow.setStart('A');
      workflow.setEnd(['B', 'C', 'D']);

      engine.registerWorkflow(workflow);

      // Act: Select B and C (not D)
      const { case: yawlCase } = await engine.createCase('or-split-workflow', {
        optionB: true,
        optionC: true,
        optionD: false,
      });

      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      const { downstreamEnabled } = await engine.completeTask(
        yawlCase.id,
        workItemA.id,
        { optionB: true, optionC: true, optionD: false }
      );

      // Assert: B and C enabled, not D
      expect(downstreamEnabled.length).toBe(2);
      expect(downstreamEnabled.map(d => d.taskId).sort()).toEqual(['B', 'C']);
    }, 5000);
  });

  // ===========================================================================
  // WP7: Structured Synchronizing Merge (OR-join)
  // ===========================================================================
  describe('WP7: Structured Synchronizing Merge (OR-join)', () => {
    test('Sync any combination of B, C that were activated', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'or-join-workflow' });
      workflow.addTask({ id: 'A', name: 'Split', splitType: SPLIT_TYPE.OR });
      workflow.addTask({ id: 'B', name: 'Path B' });
      workflow.addTask({ id: 'C', name: 'Path C' });
      workflow.addTask({ id: 'D', name: 'Sync Merge', joinType: JOIN_TYPE.OR });

      workflow.addFlow({ from: 'A', to: 'B', condition: (ctx) => ctx.data.goB });
      workflow.addFlow({ from: 'A', to: 'C', condition: (ctx) => ctx.data.goC });
      const syncMerge = structuredSyncMerge(['B', 'C'], 'D');
      for (const flow of syncMerge.flows) {
        workflow.addFlow(flow);
      }
      workflow.setStart('A');
      workflow.setEnd(['D']);

      engine.registerWorkflow(workflow);

      // Act: Activate both B and C
      const { case: yawlCase } = await engine.createCase('or-join-workflow', {
        goB: true,
        goC: true,
      });

      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id, { goB: true, goC: true });

      // Complete B
      const enabledBC = yawlCase.getEnabledWorkItems();
      expect(enabledBC.length).toBe(2);

      const workItemB = enabledBC.find(w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B');
      await engine.startTask(yawlCase.id, workItemB.id);
      const { downstreamEnabled: afterB } = await engine.completeTask(
        yawlCase.id,
        workItemB.id
      );

      // D should NOT be enabled yet (C was activated but not complete)
      expect(afterB.length).toBe(0);

      // Complete C
      const workItemC = yawlCase.getEnabledWorkItems().find(
        w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'C'
      );
      await engine.startTask(yawlCase.id, workItemC.id);
      const { downstreamEnabled: afterC } = await engine.completeTask(
        yawlCase.id,
        workItemC.id
      );

      // Assert: D enabled after all activated branches complete
      expect(afterC.length).toBe(1);
      expect(afterC[0].taskId).toBe('D');
    }, 5000);
  });
});
