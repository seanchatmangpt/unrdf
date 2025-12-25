
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
