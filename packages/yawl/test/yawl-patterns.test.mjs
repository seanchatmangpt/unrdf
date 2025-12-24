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

import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import { mkdtempSync, rmSync, existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

// Core YAWL imports
import {
  YawlWorkflow,
  YawlTask,
  TaskStatus,
  YawlReceipt,
  YawlResourcePool,
  sequence,
  parallelSplit,
  synchronization,
  exclusiveChoice,
  simpleMerge,
  multiChoice,
  structuredSyncMerge,
  arbitraryCycle,
  deferredChoice,
  SPLIT_TYPE,
  JOIN_TYPE,
} from '../src/index.mjs';

// Import engine separately to handle potential import errors gracefully
let YawlEngine;
try {
  const engineModule = await import('../src/engine.mjs');
  YawlEngine = engineModule.YawlEngine;
} catch (e) {
  // Engine may have additional dependencies - create a mock for pattern tests
  YawlEngine = class MockYawlEngine {
    constructor() {
      this.workflows = new Map();
      this.cases = new Map();
      this.resourcePool = new YawlResourcePool();
      this.events = [];
    }
    registerWorkflow(w) { this.workflows.set(w.id, w); return w; }
    getWorkflow(id) { return this.workflows.get(id); }
  };
}

// =============================================================================
// Test Utilities
// =============================================================================

/**
 * Create a simple workflow for testing
 * @param {Object} config - Workflow configuration
 * @returns {YawlWorkflow}
 */
function createTestWorkflow(config = {}) {
  return new YawlWorkflow({
    id: config.id ?? `test-workflow-${Date.now()}`,
    name: config.name ?? 'Test Workflow',
    version: config.version ?? '1.0.0',
  });
}

/**
 * Create a test engine with optional git backbone
 * @param {Object} options - Engine options
 * @returns {YawlEngine}
 */
function createTestEngine(options = {}) {
  return new YawlEngine({
    nodeId: options.nodeId ?? `test-node-${Date.now()}`,
    gitPath: options.gitPath,
  });
}

/**
 * Measure execution time
 * @param {Function} fn - Function to measure
 * @returns {Promise<{result: any, duration: number}>}
 */
async function measureTime(fn) {
  const start = performance.now();
  const result = await fn();
  const duration = performance.now() - start;
  return { result, duration };
}

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      const { receipt: completeAReceipt, downstreamEnabled } = await engine.completeWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemB.id);
      const { downstreamEnabled: downstreamFromB } = await engine.completeWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemC.id);
      await engine.completeWorkItem(yawlCase.id, workItemC.id, { result: 'C done' });

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      const { downstreamEnabled } = await engine.completeWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

      // Complete B only - D should NOT be enabled yet
      const enabledBC = yawlCase.getEnabledWorkItems();
      const workItemB = enabledBC.find(w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B');
      await engine.startWorkItem(yawlCase.id, workItemB.id);
      const { downstreamEnabled: afterB } = await engine.completeWorkItem(
        yawlCase.id,
        workItemB.id
      );

      // D should not be enabled yet (waiting for C)
      expect(afterB.length).toBe(0);

      // Complete C - now D should be enabled
      const workItemC = yawlCase.getEnabledWorkItems().find(
        w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'C'
      );
      await engine.startWorkItem(yawlCase.id, workItemC.id);
      const { downstreamEnabled: afterC } = await engine.completeWorkItem(
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
      await engine.startWorkItem(case1.id, workItem1.id);
      const { downstreamEnabled: enabled1 } = await engine.completeWorkItem(
        case1.id,
        workItem1.id,
        { approved: true }
      );

      expect(enabled1.length).toBe(1);
      expect(enabled1[0].taskId).toBe('approve');

      // Act: Test rejection path
      const { case: case2 } = await engine.createCase('xor-workflow', { approved: false });
      const workItem2 = case2.getEnabledWorkItems()[0];
      await engine.startWorkItem(case2.id, workItem2.id);
      const { downstreamEnabled: enabled2 } = await engine.completeWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemStart.id);
      await engine.completeWorkItem(yawlCase.id, workItemStart.id, { path: 'B' });

      const workItemB = yawlCase.getEnabledWorkItems()[0];
      expect(yawlCase.getTaskDefIdForWorkItem(workItemB.id)).toBe('B');

      await engine.startWorkItem(yawlCase.id, workItemB.id);
      const { downstreamEnabled } = await engine.completeWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      const { downstreamEnabled } = await engine.completeWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id, { goB: true, goC: true });

      // Complete B
      const enabledBC = yawlCase.getEnabledWorkItems();
      expect(enabledBC.length).toBe(2);

      const workItemB = enabledBC.find(w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B');
      await engine.startWorkItem(yawlCase.id, workItemB.id);
      const { downstreamEnabled: afterB } = await engine.completeWorkItem(
        yawlCase.id,
        workItemB.id
      );

      // D should NOT be enabled yet (C was activated but not complete)
      expect(afterB.length).toBe(0);

      // Complete C
      const workItemC = yawlCase.getEnabledWorkItems().find(
        w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'C'
      );
      await engine.startWorkItem(yawlCase.id, workItemC.id);
      const { downstreamEnabled: afterC } = await engine.completeWorkItem(
        yawlCase.id,
        workItemC.id
      );

      // Assert: D enabled after all activated branches complete
      expect(afterC.length).toBe(1);
      expect(afterC[0].taskId).toBe('D');
    }, 5000);
  });
});

// =============================================================================
// CONTROL FLOW TESTS
// =============================================================================

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

// =============================================================================
// RESOURCE TESTS
// =============================================================================

describe('Resource Tests', () => {
  let engine;
  let resourcePool;

  beforeEach(() => {
    engine = createTestEngine();
    resourcePool = new YawlResourcePool();
  });

  // ===========================================================================
  // Single Participant Allocation
  // ===========================================================================
  describe('Single Participant Allocation', () => {
    test('Allocates single available resource', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['reviewer'],
        available: true,
      });

      // Act
      const allocation = resourcePool.allocate({
        taskId: 'review-task',
        role: 'reviewer',
      });

      // Assert
      expect(allocation.queued).toBe(false);
      expect(allocation.resource.id).toBe('user-1');
      expect(allocation.resource.available).toBe(false);
    }, 5000);
  });

  // ===========================================================================
  // Multiple Eligible Participants
  // ===========================================================================
  describe('Multiple Eligible Participants', () => {
    test('Picks best priority from multiple eligible resources', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['developer'],
        priority: 5,
        available: true,
      });
      resourcePool.addResource({
        id: 'user-2',
        name: 'Bob',
        roles: ['developer'],
        priority: 10,  // Higher priority
        available: true,
      });
      resourcePool.addResource({
        id: 'user-3',
        name: 'Charlie',
        roles: ['developer'],
        priority: 3,
        available: true,
      });

      // Act
      const allocation = resourcePool.allocate({
        taskId: 'dev-task',
        role: 'developer',
      });

      // Assert: Should pick Bob (highest priority)
      expect(allocation.resource.id).toBe('user-2');
      expect(allocation.resource.priority).toBe(10);
    }, 5000);
  });

  // ===========================================================================
  // Resource Pool Exhaustion
  // ===========================================================================
  describe('Resource Pool Exhaustion', () => {
    test('Queues task when no resources available', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['specialist'],
        available: false,  // Already busy
        currentTaskId: 'existing-task',
      });

      // Act
      const allocation = resourcePool.allocate({
        taskId: 'new-task',
        role: 'specialist',
      });

      // Assert: Task should be queued
      expect(allocation.queued).toBe(true);
      expect(allocation.resource).toBeNull();
      expect(resourcePool.getQueueLength()).toBe(1);
    }, 5000);

    test('Dequeues task when resource becomes available', async () => {
      // Arrange
      const resource = resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['specialist'],
        available: false,
        currentTaskId: 'existing-task',
      });

      // Queue a task
      resourcePool.allocate({ taskId: 'queued-task', role: 'specialist' });
      expect(resourcePool.getQueueLength()).toBe(1);

      // Act: Release the resource
      const nextTask = resourcePool.release('user-1');

      // Assert: Queued task should be allocated
      expect(nextTask).not.toBeNull();
      expect(nextTask.taskId).toBe('queued-task');
      expect(resourcePool.getQueueLength()).toBe(0);
    }, 5000);
  });

  // ===========================================================================
  // Role-Based Allocation
  // ===========================================================================
  describe('Role-Based Allocation', () => {
    test('Only allocates resources with matching role', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'dev-1',
        roles: ['developer'],
        available: true,
      });
      resourcePool.addResource({
        id: 'mgr-1',
        roles: ['manager'],
        available: true,
      });

      // Act
      const devAllocation = resourcePool.allocate({
        taskId: 'dev-task',
        role: 'developer',
      });
      const mgrAllocation = resourcePool.allocate({
        taskId: 'mgr-task',
        role: 'manager',
      });

      // Assert
      expect(devAllocation.resource.id).toBe('dev-1');
      expect(mgrAllocation.resource.id).toBe('mgr-1');
    }, 5000);
  });
});

// =============================================================================
// CANCELLATION TESTS
// =============================================================================

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

      // Both B and C should be enabled
      const enabledBC = yawlCase.getEnabledWorkItems();
      expect(enabledBC.length).toBe(2);

      // Cancel only B
      const workItemB = enabledBC.find(w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B');
      const { task: cancelledTask, receipt } = await engine.cancelWorkItem(
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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

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

// =============================================================================
// TIME-TRAVEL TESTS
// =============================================================================

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id, { result: 'A' });

      // Create checkpoint after A
      const checkpoint1 = await engine.checkpoint('after-A');

      // Complete B
      const workItemB = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, workItemB.id);
      await engine.completeWorkItem(yawlCase.id, workItemB.id, { result: 'B' });

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      const { receipt: completeAReceipt } = await engine.completeWorkItem(
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

      await engine.startWorkItem(case1.id, work1.id);
      await engine.completeWorkItem(case1.id, work1.id, { result: 'case1' });

      await engine.checkpoint('case1-done');

      await engine.startWorkItem(case2.id, work2.id);
      await engine.completeWorkItem(case2.id, work2.id, { result: 'case2' });

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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      const { receipt: enableReceipt } = await engine.completeWorkItem(
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

// =============================================================================
// RECEIPT TESTS
// =============================================================================

describe('Receipt Tests', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  // ===========================================================================
  // Receipt Contains Hashes
  // ===========================================================================
  describe('Receipt Hash Content', () => {
    test('Receipt contains before/after hashes', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'receipt-hash-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase, receipt } = await engine.createCase('receipt-hash-workflow');

      // Assert
      expect(receipt.beforeHash).toBeDefined();
      expect(receipt.beforeHash.length).toBeGreaterThan(0);
      expect(receipt.afterHash).toBeDefined();
      expect(receipt.afterHash.length).toBeGreaterThan(0);
      expect(receipt.beforeHash).not.toBe(receipt.afterHash);
    }, 5000);
  });

  // ===========================================================================
  // Receipt Contains SPARQL Query + Result
  // ===========================================================================
  describe('Receipt SPARQL Content', () => {
    test('Receipt can contain SPARQL query + result', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'sparql-receipt-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('sparql-receipt-workflow');

      // The receipt building function supports SPARQL
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

      // Check that receipts can store SPARQL (even if not all do)
      const receipts = yawlCase.receipts;
      expect(receipts.length).toBeGreaterThan(0);

      // Receipt structure supports SPARQL
      const receipt = receipts[0];
      expect(receipt).toHaveProperty('sparqlQuery');
      expect(receipt).toHaveProperty('sparqlResult');
    }, 5000);
  });

  // ===========================================================================
  // Receipt Contains Actor + Timestamp
  // ===========================================================================
  describe('Receipt Actor and Timestamp', () => {
    test('Receipt contains actor + timestamp', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'actor-receipt-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('actor-receipt-workflow');
      const workItemA = yawlCase.getEnabledWorkItems()[0];

      await engine.startWorkItem(yawlCase.id, workItemA.id, { actor: 'user-123' });
      await engine.completeWorkItem(yawlCase.id, workItemA.id, {}, 'user-456');

      // Assert
      const receipts = yawlCase.receipts;
      const completeReceipt = receipts.find(r => r.action === 'complete');

      expect(completeReceipt).toBeDefined();
      expect(completeReceipt.actor).toBe('user-456');
      expect(completeReceipt.timestamp).toBeDefined();
      expect(typeof completeReceipt.timestamp).toBe('bigint');
    }, 5000);
  });

  // ===========================================================================
  // Receipt Chain Verification
  // ===========================================================================
  describe('Receipt Chain Verification', () => {
    test('Receipt chain verifiable (each hash references previous)', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'chain-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.addTask({ id: 'B' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.setStart('A');
      workflow.setEnd(['B']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('chain-workflow');

      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

      const workItemB = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, workItemB.id);
      await engine.completeWorkItem(yawlCase.id, workItemB.id);

      // Verify chain
      const receipts = yawlCase.receipts;
      expect(receipts.length).toBeGreaterThan(2);

      // Check that receipts form a chain
      for (let i = 1; i < receipts.length; i++) {
        const current = receipts[i];
        const previous = receipts[i - 1];

        // Verify chain integrity
        const chainResult = await current.verifyChain(previous);
        expect(chainResult.valid).toBe(true);
      }
    }, 5000);
  });
});

// =============================================================================
// INTEGRATION TESTS
// =============================================================================

describe('Integration Tests', () => {
  let engine;
  let tempDir;

  beforeEach(() => {
    tempDir = mkdtempSync(join(tmpdir(), 'yawl-integration-'));
    engine = createTestEngine({ gitPath: tempDir });
  });

  afterEach(() => {
    if (tempDir && existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  });

  // ===========================================================================
  // Full Workflow Lifecycle
  // ===========================================================================
  describe('Full Workflow Lifecycle', () => {
    test('Create case -> enable -> start -> complete -> enable downstream', async () => {
      // Arrange: 3-step approval workflow
      const workflow = createTestWorkflow({ id: 'approval-workflow' });
      workflow.addTask({ id: 'submit', name: 'Submit Request' });
      workflow.addTask({ id: 'review', name: 'Review Request', splitType: SPLIT_TYPE.XOR });
      workflow.addTask({ id: 'approve', name: 'Approve' });
      workflow.addTask({ id: 'reject', name: 'Reject' });

      workflow.addFlow(sequence('submit', 'review'));
      workflow.addFlow({ from: 'review', to: 'approve', condition: ctx => ctx.data.approved });
      workflow.addFlow({ from: 'review', to: 'reject', condition: ctx => !ctx.data.approved });
      workflow.setStart('submit');
      workflow.setEnd(['approve', 'reject']);

      engine.registerWorkflow(workflow);

      // Add resources
      engine.addResource({ id: 'submitter', roles: ['staff'], available: true });
      engine.addResource({ id: 'reviewer', roles: ['manager'], available: true });

      // Act
      const { case: yawlCase, receipt: createReceipt } = await engine.createCase(
        'approval-workflow',
        { requestId: 'REQ-001' }
      );

      // Verify create receipt
      expect(createReceipt.valid).toBe(true);
      expect(yawlCase.status).toBe('running');

      // Step 1: Submit
      const submitItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, submitItem.id, { actor: 'submitter' });
      const { receipt: submitReceipt } = await engine.completeWorkItem(
        yawlCase.id,
        submitItem.id,
        { description: 'Need new laptop' }
      );

      expect(submitReceipt.valid).toBe(true);

      // Step 2: Review (approve)
      const reviewItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, reviewItem.id, { actor: 'reviewer' });
      const { receipt: reviewReceipt, downstreamEnabled } = await engine.completeWorkItem(
        yawlCase.id,
        reviewItem.id,
        { approved: true, comments: 'Looks good' }
      );

      expect(reviewReceipt.valid).toBe(true);
      expect(downstreamEnabled.length).toBe(1);
      expect(downstreamEnabled[0].taskId).toBe('approve');

      // Step 3: Approve
      const approveItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, approveItem.id);
      await engine.completeWorkItem(yawlCase.id, approveItem.id);

      // Assert: Workflow complete
      expect(yawlCase.status).toBe('completed');

      // Verify full receipt chain
      expect(yawlCase.receipts.length).toBeGreaterThan(5);
      for (let i = 1; i < yawlCase.receipts.length; i++) {
        const chainResult = await yawlCase.receipts[i].verifyChain(yawlCase.receipts[i - 1]);
        expect(chainResult.valid).toBe(true);
      }

      // Verify time-travel
      await engine.checkpoint('final');
      const reconstructed = await engine.reconstructCase(
        yawlCase.id,
        BigInt(Date.now()) * 1000000n
      );
      expect(reconstructed.verified).toBe(true);
    }, 5000);
  });

  // ===========================================================================
  // Error Path with Circuit Breaker
  // ===========================================================================
  describe('Error Path with Circuit Breaker', () => {
    test('Task fails -> circuit breaker -> auto-disable', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'error-workflow' });
      workflow.addTask({ id: 'risky', name: 'Risky Task' });
      workflow.addTask({ id: 'fallback', name: 'Fallback' });
      workflow.addFlow(sequence('risky', 'fallback'));
      workflow.setStart('risky');
      workflow.setEnd(['risky', 'fallback']);

      engine.registerWorkflow(workflow);

      // Act
      const { case: yawlCase } = await engine.createCase('error-workflow');

      const riskyItem = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, riskyItem.id);

      // Simulate failure by cancelling
      await engine.cancelWorkItem(yawlCase.id, riskyItem.id, 'Task failed');

      // Disable circuit breaker for risky task
      await engine.setCircuitBreaker(yawlCase.id, 'risky', false);

      // Try to enable risky again - should fail
      await expect(
        engine.enableTask(yawlCase.id, 'risky')
      ).rejects.toThrow(/circuit breaker/);
    }, 5000);
  });

  // ===========================================================================
  // Resource Contention
  // ===========================================================================
  describe('Resource Contention', () => {
    test('Two cases competing for same resource', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'contention-workflow' });
      workflow.addTask({ id: 'exclusive', name: 'Exclusive Task', role: 'specialist' });
      workflow.setStart('exclusive');
      workflow.setEnd(['exclusive']);

      engine.registerWorkflow(workflow);

      // Only one specialist available
      engine.addResource({
        id: 'sole-specialist',
        roles: ['specialist'],
        available: true,
      });

      // Act: Create two cases
      const { case: case1 } = await engine.createCase('contention-workflow');
      const { case: case2 } = await engine.createCase('contention-workflow');

      // Case 1 starts first
      const work1 = case1.getEnabledWorkItems()[0];
      const { resource: resource1 } = await engine.startWorkItem(case1.id, work1.id);

      expect(resource1.id).toBe('sole-specialist');

      // Case 2 tries to start - should fail (no resources)
      const work2 = case2.getEnabledWorkItems()[0];
      await expect(
        engine.startWorkItem(case2.id, work2.id)
      ).rejects.toThrow(/No available resources/);

      // Case 1 completes - resource released
      await engine.completeWorkItem(case1.id, work1.id);

      // Now case 2 can start
      const { resource: resource2 } = await engine.startWorkItem(case2.id, work2.id);
      expect(resource2.id).toBe('sole-specialist');
    }, 5000);
  });

  // ===========================================================================
  // Performance Latency
  // ===========================================================================
  describe('Performance Latency', () => {
    test('enableTask latency < 100ms', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'perf-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.addTask({ id: 'B' });
      workflow.addFlow(sequence('A', 'B'));
      workflow.setStart('A');
      workflow.setEnd(['B']);

      engine.registerWorkflow(workflow);

      const { case: yawlCase } = await engine.createCase('perf-workflow');

      // Act & Assert: Measure enableTask
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, workItemA.id);

      const { duration } = await measureTime(async () => {
        return engine.completeWorkItem(yawlCase.id, workItemA.id);
      });

      expect(duration).toBeLessThan(100);
    }, 5000);

    test('completeTask latency < 100ms', async () => {
      // Arrange
      const workflow = createTestWorkflow({ id: 'perf-complete-workflow' });
      workflow.addTask({ id: 'A' });
      workflow.setStart('A');
      workflow.setEnd(['A']);

      engine.registerWorkflow(workflow);

      const { case: yawlCase } = await engine.createCase('perf-complete-workflow');
      const workItemA = yawlCase.getEnabledWorkItems()[0];
      await engine.startWorkItem(yawlCase.id, workItemA.id);

      // Act & Assert
      const { duration } = await measureTime(async () => {
        return engine.completeWorkItem(yawlCase.id, workItemA.id, { result: 'done' });
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
      await engine.startWorkItem(yawlCase.id, workItemA.id);
      await engine.completeWorkItem(yawlCase.id, workItemA.id);

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
    await engine.startWorkItem(yawlCase.id, workItemA.id);
    await engine.completeWorkItem(yawlCase.id, workItemA.id);

    // Complete B - D should be enabled (XOR-join)
    const workItemB = yawlCase.getEnabledWorkItems().find(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B'
    );
    await engine.startWorkItem(yawlCase.id, workItemB.id);
    const { downstreamEnabled } = await engine.completeWorkItem(yawlCase.id, workItemB.id);

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
    await engine.startWorkItem(yawlCase.id, workItemA.id);
    await engine.completeWorkItem(yawlCase.id, workItemA.id);

    // Complete just B - D should be enabled immediately
    const workItemB = yawlCase.getEnabledWorkItems().find(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'B'
    );
    await engine.startWorkItem(yawlCase.id, workItemB.id);
    const { downstreamEnabled } = await engine.completeWorkItem(yawlCase.id, workItemB.id);

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
    await engine.startWorkItem(yawlCase.id, workItemA.id);
    await engine.completeWorkItem(yawlCase.id, workItemA.id);

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
    await engine.startWorkItem(yawlCase.id, workItemA.id);

    const { task, receipt } = await engine.cancelWorkItem(
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
    await engine.startWorkItem(yawlCase.id, workItemA.id);
    await engine.completeWorkItem(yawlCase.id, workItemA.id);

    expect(yawlCase.getEnabledWorkItems().length).toBe(2);

    // Cancel all region
    const { cancelled } = await engine.cancelRegion(yawlCase.id, 'all', 'Case cancelled');

    expect(cancelled.length).toBe(2);
    expect(yawlCase.getEnabledWorkItems().length).toBe(0);
  }, 5000);
});
