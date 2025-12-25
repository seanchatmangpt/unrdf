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
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

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

      await engine.startTask(yawlCase.id, workItemA.id, { actor: 'user-123' });
      await engine.completeTask(yawlCase.id, workItemA.id, {}, 'user-456');

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
      await engine.startTask(yawlCase.id, workItemA.id);
      await engine.completeTask(yawlCase.id, workItemA.id);

      const workItemB = yawlCase.getEnabledWorkItems()[0];
      await engine.startTask(yawlCase.id, workItemB.id);
      await engine.completeTask(yawlCase.id, workItemB.id);

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