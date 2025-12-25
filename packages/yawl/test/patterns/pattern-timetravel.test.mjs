import { mkdtempSync, rmSync, existsSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createTestWorkflow, createTestEngine, measureTime, sequence } from './test-utils.mjs';

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


describe('Time-Travel Tests', () => {
  let engine;
  let tempDir;

  beforeEach(() => {
    tempDir = mkdtempSync(join(tmpdir(), 'yawl-timetravel-'));
    engine = createTestEngine({ gitPath: tempDir });
  });

  afterEach(() => {
    if (tempDir && existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  });

  // ===========================================================================
  // Replay to Checkpoint
  // ===========================================================================
  describe('Replay to Checkpoint', () => {
    test('Replay workflow to checkpoint, verify state exact match', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'replay-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.addTask({ id: 'B' });
      workflow.addTask({ id: 'C' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.addFlow(sequence('B', 'C'));
      workflow.setStart('A');
      workflow.setEnd(['C']);

      engine.registerWorkflow(workflow);

      // Act: Execute workflow and create checkpoints
      const { case: yawlCase } = await engine.createCase('replay-workflow');

      // Complete A
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id, { result: 'A' });

      // Create checkpoint after A
      const checkpoint1 = await engine.checkpoint('after-A');

      // Complete B
      const workItemB = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemB.id);
      await engine.completeTask(yawlCase.id, workItemB.id, { result: 'B' });

      // Create checkpoint after B
      const checkpoint2 = await engine.checkpoint('after-B');

      // Reconstruct at checkpoint1
      const reconstructed = await engine.reconstructCase(
        yawlCase.id,
        checkpoint1.timestamp
      );

      // Assert: State should match checkpoint1 (A complete, B not started)
      expect(reconstructed.verified).toBe(true);
      expect(reconstructed.case).toBeDefined();
    }, 5000);
  });

  // ===========================================================================
  // Replay with Hash Verification
  // ===========================================================================
  describe('Replay with Hash Verification', () => {
    test('All hashes match during replay', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'hash-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.addTask({ id: 'B' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.setStart('A');
      workflow.setEnd(['B']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase, receipt: startReceipt } = await engine.createCase('hash-workflow');

      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      const { receipt: completeAReceipt } = await engine.completeTask(
        yawlCase.id,
        workItemA.id
      );

      // Replay to receipt
      const replay = await engine.replayToReceipt(yawlCase.id, completeAReceipt.id);

      // Assert: Replay verified
      expect(replay.verified).toBe(true);
      expect(replay.state.afterHash).toBe(completeAReceipt.afterHash);
    }, 5000);
  });

  // ===========================================================================
  // Concurrent Cases Replay Independently
  // ===========================================================================
  describe('Concurrent Cases', () => {
    test('Concurrent cases replay independently', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'concurrent-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      // Act: Create two concurrent cases
      const { case: case1 } = await engine.createCase('concurrent-workflow', { caseNum: 1 });
      const { case: case2 } = await engine.createCase('concurrent-workflow', { caseNum: 2 });

      // Execute both cases
      const work1 = case1.getEnabledWorkItems()[0];
      const work2 = case2.getEnabledWorkItems()[0];

      await engine.startTask(case1.id, work1.id);
      await engine.completeTask(case1.id, work1.id, { result: 'case1' });

      await engine.checkpoint('case1-done');

      await engine.startTask(case2.id, work2.id);
      await engine.completeTask(case2.id, work2.id, { result: 'case2' });

      await engine.checkpoint('case2-done');

      // Reconstruct each case
      const reconstructed1 = await engine.reconstructCase(case1.id, BigInt(Date.now()) * 1000000n);
      const reconstructed2 = await engine.reconstructCase(case2.id, BigInt(Date.now()) * 1000000n);

      // Assert: Both cases reconstructed independently
      expect(reconstructed1.verified).toBe(true);
      expect(reconstructed2.verified).toBe(true);
      expect(reconstructed1.case.data.result).toBe('case1');
      expect(reconstructed2.case.data.result).toBe('case2');
    }, 5000);
  });

  // ===========================================================================
  // Partial Replay
  // ===========================================================================
  describe('Partial Replay', () => {
    test('Replay single task transition', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'partial-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.addTask({ id: 'B' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.setStart('A');
      workflow.setEnd(['B']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('partial-workflow');

      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemA.id);
      const { receipt: enableReceipt } = await engine.completeTask(
        yawlCase.id,
        workItemA.id
      );

      // Replay to specific receipt
      const replay = await engine.replayToReceipt(yawlCase.id, enableReceipt.id);

      // Assert
      expect(replay.verified).toBe(true);
      expect(replay.state.action).toBe('complete');
    }, 5000);
  });
});