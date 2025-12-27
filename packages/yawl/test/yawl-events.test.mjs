/**
 * YAWL Events - Integration Tests
 * Tests event sourcing, time-travel, and receipt verification
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KGCStore, now, toISO } from '@unrdf/kgc-4d';
import {
  YAWL_EVENT_TYPES,
  YAWL_NS,
  YAWL_PREDICATES,
  appendWorkflowEvent,
  reconstructCase,
  createWorkflowReceipt,
  verifyWorkflowReceipt,
  getWorkflowAuditTrail,
  createCase,
  enableTask,
  startWorkItem,
  completeWorkItem,
  recordControlFlowEvaluation,
} from '../src/events/yawl-events.mjs';
import { createTestWorkflow, createTestCase } from './test-helpers.mjs';

describe('YAWL Events', () => {
  /** @type {import('@unrdf/kgc-4d').KGCStore} */
  let store;

  beforeEach(() => {
    store = new KGCStore({ nodeId: 'test-node' });
  });

  describe('Event Types', () => {
    it('should define all required event types', () => {
      expect(YAWL_EVENT_TYPES).toBeDefined();
      expect(YAWL_EVENT_TYPES.CASE_CREATED).toBe('YAWL_CASE_CREATED');
      expect(YAWL_EVENT_TYPES.TASK_ENABLED).toBe('YAWL_TASK_ENABLED');
      expect(YAWL_EVENT_TYPES.TASK_STARTED).toBe('YAWL_TASK_STARTED');
      expect(YAWL_EVENT_TYPES.TASK_COMPLETED).toBe('YAWL_TASK_COMPLETED');
      expect(YAWL_EVENT_TYPES.TASK_CANCELLED).toBe('YAWL_TASK_CANCELLED');
      expect(YAWL_EVENT_TYPES.WORK_ITEM_CREATED).toBe('YAWL_WORK_ITEM_CREATED');
      expect(YAWL_EVENT_TYPES.CONTROL_FLOW_EVALUATED).toBe('YAWL_CONTROL_FLOW_EVALUATED');
    });

    it('should have frozen event types', () => {
      expect(Object.isFrozen(YAWL_EVENT_TYPES)).toBe(true);
    });

    it('should define YAWL namespace and predicates', () => {
      expect(YAWL_NS).toBe('http://yawl.io/');
      expect(YAWL_PREDICATES.CASE_ID).toBe('http://yawl.io/caseId');
      expect(YAWL_PREDICATES.TASK_ID).toBe('http://yawl.io/taskId');
      expect(YAWL_PREDICATES.WORK_ITEM_ID).toBe('http://yawl.io/workItemId');
    });
  });

  describe('createWorkflowReceipt', () => {
    it('should create receipt with BLAKE3 hashes', async () => {
      const receipt = await createWorkflowReceipt({
        beforeState: { empty: true },
        afterState: { caseId: 'test-123', state: 'active' },
        decision: { action: 'create_case' },
        justification: { reasoning: 'Test case creation' },
      });

      expect(receipt).toBeDefined();
      expect(receipt.beforeHash).toHaveLength(64);
      expect(receipt.afterHash).toHaveLength(64);
      expect(receipt.hash).toHaveLength(64);
      expect(receipt.t_ns).toBeDefined();
      expect(receipt.timestamp_iso).toBeDefined();
      expect(receipt.justification.reasoning).toBe('Test case creation');
    });

    it('should produce different hashes for different states', async () => {
      const receipt1 = await createWorkflowReceipt({
        beforeState: { state: 'A' },
        afterState: { state: 'B' },
        decision: { action: 'transition' },
      });

      const receipt2 = await createWorkflowReceipt({
        beforeState: { state: 'A' },
        afterState: { state: 'C' },
        decision: { action: 'transition' },
      });

      expect(receipt1.beforeHash).toBe(receipt2.beforeHash); // Same before state
      expect(receipt1.afterHash).not.toBe(receipt2.afterHash); // Different after state
    });
  });

  describe('verifyWorkflowReceipt', () => {
    it('should verify valid receipt', async () => {
      const beforeState = { state: 'enabled' };
      const afterState = { state: 'started' };
      const decision = { action: 'start_task' };

      const receipt = await createWorkflowReceipt({
        beforeState,
        afterState,
        decision,
      });

      const verification = await verifyWorkflowReceipt(
        receipt,
        beforeState,
        afterState,
        decision
      );

      expect(verification.valid).toBe(true);
      expect(verification.verified.beforeHash).toBe(true);
      expect(verification.verified.afterHash).toBe(true);
      expect(verification.verified.decisionHash).toBe(true);
    });

    it('should reject tampered receipt', async () => {
      const beforeState = { state: 'enabled' };
      const afterState = { state: 'started' };
      const decision = { action: 'start_task' };

      const receipt = await createWorkflowReceipt({
        beforeState,
        afterState,
        decision,
      });

      // Tamper with the after state
      const verification = await verifyWorkflowReceipt(
        receipt,
        beforeState,
        { state: 'completed' }, // Different!
        decision
      );

      expect(verification.valid).toBe(false);
      expect(verification.verified.afterHash).toBe(false);
    });
  });

  describe('createCase', () => {
    it('should create a new workflow case', async () => {
      const result = await createCase(store, 'approval-workflow');

      expect(result.caseId).toBeDefined();
      expect(result.caseId).toMatch(
        /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i
      );
      expect(result.specId).toBe('approval-workflow');
      expect(result.state).toBe('active');
      expect(result.receipt).toBeDefined();
      expect(result.receipt.hash).toHaveLength(64);
      expect(result.eventReceipt).toBeDefined();
      expect(result.eventReceipt.eventType).toBe('YAWL_CASE_CREATED');
    });

    it('should store event in KGC-4D store', async () => {
      const result = await createCase(store, 'test-workflow');

      expect(store.getEventCount()).toBe(1);
      expect(result.eventReceipt.event_count).toBe(1);
    });
  });

  describe('enableTask', () => {
    it('should enable a task and create work item', async () => {
      const caseResult = await createCase(store, 'approval-workflow');
      const taskResult = await enableTask(store, caseResult.caseId, 'Review');

      expect(taskResult.workItemId).toBeDefined();
      expect(taskResult.taskId).toBe('Review');
      expect(taskResult.caseId).toBe(caseResult.caseId);
      expect(taskResult.state).toBe('enabled');
      expect(taskResult.receipt).toBeDefined();
      expect(taskResult.eventReceipt.eventType).toBe('YAWL_TASK_ENABLED');
    });
  });

  describe('startWorkItem', () => {
    it('should start a work item', async () => {
      const caseResult = await createCase(store, 'approval-workflow');
      const enableResult = await enableTask(store, caseResult.caseId, 'Review');
      const startResult = await startWorkItem(
        store,
        enableResult.workItemId,
        caseResult.caseId
      );

      expect(startResult.workItemId).toBe(enableResult.workItemId);
      expect(startResult.state).toBe('started');
      expect(startResult.startedAt).toBeDefined();
      expect(startResult.receipt).toBeDefined();
    });
  });

  describe('completeWorkItem', () => {
    it('should complete a work item with result', async () => {
      const caseResult = await createCase(store, 'approval-workflow');
      const enableResult = await enableTask(store, caseResult.caseId, 'Review');
      await startWorkItem(store, enableResult.workItemId, caseResult.caseId);
      const completeResult = await completeWorkItem(
        store,
        enableResult.workItemId,
        caseResult.caseId,
        { approved: true, comments: 'Looks good' }
      );

      expect(completeResult.workItemId).toBe(enableResult.workItemId);
      expect(completeResult.state).toBe('completed');
      expect(completeResult.result).toEqual({ approved: true, comments: 'Looks good' });
      expect(completeResult.receipt).toBeDefined();
    });
  });

  describe('recordControlFlowEvaluation', () => {
    it('should record control flow decision with SPARQL', async () => {
      const caseResult = await createCase(store, 'approval-workflow');
      const sparqlQuery = `
        ASK WHERE {
          ?case <http://yawl.io/hasManager> ?manager .
        }
      `;

      const result = await recordControlFlowEvaluation(
        store,
        caseResult.caseId,
        'ManagerApproval',
        true,
        sparqlQuery
      );

      expect(result.caseId).toBe(caseResult.caseId);
      expect(result.taskId).toBe('ManagerApproval');
      expect(result.result).toBe(true);
      expect(result.sparqlQuery).toBe(sparqlQuery);
      expect(result.receipt.justification.sparqlQuery).toBe(sparqlQuery);
    });
  });

  describe('getWorkflowAuditTrail', () => {
    it('should return complete audit trail for case', async () => {
      // Create case and execute workflow
      const caseResult = await createCase(store, 'approval-workflow');
      const enableResult = await enableTask(store, caseResult.caseId, 'Review');
      await startWorkItem(store, enableResult.workItemId, caseResult.caseId);
      await completeWorkItem(
        store,
        enableResult.workItemId,
        caseResult.caseId,
        { approved: true }
      );

      // Get audit trail
      const audit = await getWorkflowAuditTrail(store, caseResult.caseId);

      expect(audit.caseId).toBe(caseResult.caseId);
      expect(audit.events).toHaveLength(4);
      expect(audit.events[0].type).toBe('YAWL_CASE_CREATED');
      expect(audit.events[1].type).toBe('YAWL_TASK_ENABLED');
      expect(audit.events[2].type).toBe('YAWL_TASK_STARTED');
      expect(audit.events[3].type).toBe('YAWL_TASK_COMPLETED');
      expect(audit.eventCount).toBe(4);
      expect(audit.auditHash).toHaveLength(64);
      expect(audit.reproducible).toBe(true);
      expect(audit.verifiable).toBe(true);
    });

    it('should include receipts in audit trail', async () => {
      const caseResult = await createCase(store, 'test-workflow');
      const audit = await getWorkflowAuditTrail(store, caseResult.caseId);

      expect(audit.receipts.length).toBeGreaterThan(0);
      expect(audit.receipts[0].beforeHash).toHaveLength(64);
    });

    it('should include SPARQL queries in audit trail', async () => {
      const caseResult = await createCase(store, 'test-workflow');
      const sparqlQuery = 'ASK WHERE { ?s ?p ?o }';

      await recordControlFlowEvaluation(
        store,
        caseResult.caseId,
        'TestTask',
        true,
        sparqlQuery
      );

      const audit = await getWorkflowAuditTrail(store, caseResult.caseId);

      expect(audit.sparqlQueries.length).toBe(1);
      expect(audit.sparqlQueries[0].taskId).toBe('TestTask');
      expect(audit.sparqlQueries[0].query).toBe(sparqlQuery);
    });
  });

  describe('reconstructCase (Time Travel)', () => {
    it('should reconstruct case at specific time', async () => {
      // Create case
      const caseResult = await createCase(store, 'approval-workflow');

      // Enable task and record time
      const enableResult = await enableTask(store, caseResult.caseId, 'Review');
      const time_T = now();

      // Wait a tiny bit and start task (after time_T)
      await new Promise((r) => setTimeout(r, 1));
      await startWorkItem(store, enableResult.workItemId, caseResult.caseId);

      // Reconstruct at time_T (before start)
      const reconstructed = await reconstructCase(
        store,
        null, // No git backbone needed for this test
        caseResult.caseId,
        time_T
      );

      expect(reconstructed.caseId).toBe(caseResult.caseId);
      expect(reconstructed.state).toBe('active');
      expect(reconstructed.specId).toBe('approval-workflow');
      expect(reconstructed.workItems).toHaveLength(1);
      expect(reconstructed.workItems[0].state).toBe('enabled'); // Not started yet!
      expect(reconstructed.stateHash).toHaveLength(64);
      expect(reconstructed.verified).toBe(true);
    });

    it('should reconstruct case with completed tasks', async () => {
      // Full workflow execution
      const caseResult = await createCase(store, 'approval-workflow');
      const enableResult = await enableTask(store, caseResult.caseId, 'Review');
      await startWorkItem(store, enableResult.workItemId, caseResult.caseId);
      await completeWorkItem(
        store,
        enableResult.workItemId,
        caseResult.caseId,
        { approved: true }
      );

      const time_T = now();

      // Reconstruct at final time
      const reconstructed = await reconstructCase(
        store,
        null,
        caseResult.caseId,
        time_T
      );

      expect(reconstructed.workItems).toHaveLength(1);
      expect(reconstructed.workItems[0].state).toBe('completed');
      expect(reconstructed.workItems[0].result).toEqual({ approved: true });
      expect(reconstructed.eventCount).toBe(4);
    });

    it('should return empty state before case creation', async () => {
      const time_before = now();
      await new Promise((r) => setTimeout(r, 1));
      const caseResult = await createCase(store, 'approval-workflow');

      const reconstructed = await reconstructCase(
        store,
        null,
        caseResult.caseId,
        time_before
      );

      expect(reconstructed.state).toBeNull();
      expect(reconstructed.eventCount).toBe(0);
    });

    it('should produce same hash for same state', async () => {
      const caseResult = await createCase(store, 'test-workflow');
      const enableResult = await enableTask(store, caseResult.caseId, 'Task1');
      const time_T = now();

      const reconstructed1 = await reconstructCase(
        store,
        null,
        caseResult.caseId,
        time_T
      );

      const reconstructed2 = await reconstructCase(
        store,
        null,
        caseResult.caseId,
        time_T
      );

      // Same state should produce same hash (deterministic)
      expect(reconstructed1.stateHash).toBe(reconstructed2.stateHash);
    });
  });

  describe('appendWorkflowEvent', () => {
    it('should validate event type', async () => {
      await expect(
        appendWorkflowEvent(store, 'INVALID_TYPE', { caseId: 'test' })
      ).rejects.toThrow('Invalid YAWL event type');
    });

    it('should accept both short and full event type names', async () => {
      const receipt = await createWorkflowReceipt({
        beforeState: {},
        afterState: { caseId: 'test' },
        decision: { action: 'create' },
      });

      // Short form
      const result1 = await appendWorkflowEvent(store, 'CASE_CREATED', {
        caseId: crypto.randomUUID(),
        specId: 'test',
        timestamp: toISO(now()),
        receipt,
      });

      expect(result1.eventType).toBe('YAWL_CASE_CREATED');

      // Full form
      const result2 = await appendWorkflowEvent(store, YAWL_EVENT_TYPES.CASE_CREATED, {
        caseId: crypto.randomUUID(),
        specId: 'test',
        timestamp: toISO(now()),
        receipt,
      });

      expect(result2.eventType).toBe('YAWL_CASE_CREATED');
    });

    it('should require caseId for all events', async () => {
      const receipt = await createWorkflowReceipt({
        beforeState: {},
        afterState: {},
        decision: {},
      });

      await expect(
        appendWorkflowEvent(store, 'TASK_STARTED', {
          workItemId: crypto.randomUUID(),
          startedAt: toISO(now()),
          receipt,
        })
      ).rejects.toThrow('caseId is required');
    });
  });

  describe('Concurrent Cases', () => {
    it('should support multiple concurrent cases', async () => {
      // Create two cases
      const case1 = await createCase(store, 'workflow-A');
      const case2 = await createCase(store, 'workflow-B');

      // Enable tasks in both
      const task1 = await enableTask(store, case1.caseId, 'TaskA');
      const task2 = await enableTask(store, case2.caseId, 'TaskB');

      // Complete task in case 1
      await startWorkItem(store, task1.workItemId, case1.caseId);
      await completeWorkItem(store, task1.workItemId, case1.caseId, { result: 1 });

      // Get audit trails - should be isolated
      const audit1 = await getWorkflowAuditTrail(store, case1.caseId);
      const audit2 = await getWorkflowAuditTrail(store, case2.caseId);

      expect(audit1.eventCount).toBe(4); // Created + Enabled + Started + Completed
      expect(audit2.eventCount).toBe(2); // Created + Enabled

      // Verify case isolation
      const events1Types = audit1.events.map((e) => e.type);
      const events2Types = audit2.events.map((e) => e.type);

      expect(events1Types).toContain('YAWL_TASK_COMPLETED');
      expect(events2Types).not.toContain('YAWL_TASK_COMPLETED');
    });

    it('should reconstruct each case independently', async () => {
      const case1 = await createCase(store, 'workflow-A');
      const case2 = await createCase(store, 'workflow-B');

      await enableTask(store, case1.caseId, 'Task1');
      await enableTask(store, case2.caseId, 'Task2');

      const time_T = now();

      const reconstructed1 = await reconstructCase(store, null, case1.caseId, time_T);
      const reconstructed2 = await reconstructCase(store, null, case2.caseId, time_T);

      expect(reconstructed1.workItems[0].taskId).toBe('Task1');
      expect(reconstructed2.workItems[0].taskId).toBe('Task2');

      // Different cases should have different state hashes
      expect(reconstructed1.stateHash).not.toBe(reconstructed2.stateHash);
    });
  });
});
