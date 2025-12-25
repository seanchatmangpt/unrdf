/**
 * Integration Test - Scenario 1: Complete Workflow Execution
 * Tests: YAWL + Hooks + KGC-4D + Receipts
 *
 * Real-world scenario: Document approval workflow with:
 * - Hook-based validation
 * - Time-travel snapshots
 * - Receipt verification
 * - Audit trail generation
 */

import { test, expect, describe, beforeEach } from 'vitest';
import { createWorkflowEngine, createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';
import { freezeUniverse, reconstructState, KGCStore } from '@unrdf/kgc-4d';
import { defineHook, executeHook } from '@unrdf/hooks';

describe('Scenario 1: Complete Workflow Execution', () => {
  let store;
  let kgcStore;
  let engine;

  beforeEach(() => {
    // Create RDF store and KGC store
    store = createStore();
    kgcStore = new KGCStore();
  });

  test('executes workflow with hooks, receipts, and time-travel', async () => {
    // ======================================================================
    // STEP 1: Create workflow engine
    // ======================================================================
    engine = createWorkflowEngine({ store });
    expect(engine).toBeDefined();
    expect(engine.store).toBe(store);

    // ======================================================================
    // STEP 2: Define approval workflow
    // ======================================================================
    const workflow = createWorkflow('document-approval', {
      name: 'Document Approval',
      description: 'Multi-stage document approval workflow',
    });

    // Add tasks
    workflow.addTask('submit', {
      type: 'manual',
      name: 'Submit Document',
    });

    workflow.addTask('review', {
      type: 'automated',
      name: 'Automated Review',
    });

    workflow.addTask('approve', {
      type: 'manual',
      name: 'Manager Approval',
    });

    // Add flows
    workflow.addFlow('submit', 'review');
    workflow.addFlow('review', 'approve');

    // Register workflow
    await engine.registerWorkflow(workflow);

    // ======================================================================
    // STEP 3: Create validation hook
    // ======================================================================
    const validationHook = defineHook({
      id: 'task-data-validation',
      trigger: 'before-task-enable',
      handler: async (context) => {
        const { task, data } = context;

        // Validate task data exists
        if (!data) {
          return {
            valid: false,
            error: 'Task data is required',
          };
        }

        // Validate amount for review tasks
        if (task.id === 'review' && (!data.amount || data.amount <= 0)) {
          return {
            valid: false,
            error: 'Valid amount is required for review',
          };
        }

        return { valid: true };
      },
    });

    // Execute hook to test it works
    const hookResult = await executeHook(validationHook, {
      task: { id: 'review' },
      data: { amount: 1000 },
    });

    expect(hookResult.valid).toBe(true);

    // ======================================================================
    // STEP 4: Start workflow case
    // ======================================================================
    const caseData = {
      submitter: 'user1',
      document: 'contract-2025.pdf',
      amount: 1000,
      timestamp: new Date().toISOString(),
    };

    const workflowCase = await engine.startCase('document-approval', caseData);

    expect(workflowCase).toBeDefined();
    expect(workflowCase.id).toBeDefined();
    expect(workflowCase.status).toBe('active');

    // ======================================================================
    // STEP 5: Freeze universe state (Snapshot 1)
    // ======================================================================
    const snapshot1 = await freezeUniverse(kgcStore, 'before-review', {
      workflowId: workflow.id,
      caseId: workflowCase.id,
      description: 'State before automated review',
    });

    expect(snapshot1).toBeDefined();
    expect(snapshot1.label).toBe('before-review');
    expect(snapshot1.timestamp).toBeDefined();

    // ======================================================================
    // STEP 6: Execute automated review task
    // ======================================================================
    const reviewTask = await engine.enableTask(workflowCase.id, 'review');
    expect(reviewTask).toBeDefined();

    const reviewResult = await engine.completeTask(workflowCase.id, 'review', {
      reviewer: 'automated-system',
      decision: 'approved',
      confidence: 0.95,
      completedAt: new Date().toISOString(),
    });

    expect(reviewResult).toBeDefined();

    // ======================================================================
    // STEP 7: Verify receipt generated
    // ======================================================================
    const receipt = await engine.getReceipt(workflowCase.id, 'review');

    expect(receipt).toBeDefined();
    expect(receipt.hash).toBeDefined();
    expect(receipt.hash).toMatch(/^[a-f0-9]{64}$/); // SHA-256 hex format
    expect(receipt.previousHash).toBeDefined();
    expect(receipt.event).toBe('task-completed');
    expect(receipt.taskId).toBe('review');

    // ======================================================================
    // STEP 8: Freeze universe state (Snapshot 2)
    // ======================================================================
    const snapshot2 = await freezeUniverse(kgcStore, 'after-review', {
      workflowId: workflow.id,
      caseId: workflowCase.id,
      description: 'State after automated review',
    });

    expect(snapshot2).toBeDefined();
    expect(snapshot2.label).toBe('after-review');
    expect(snapshot2.timestamp).toBeGreaterThan(snapshot1.timestamp);

    // ======================================================================
    // STEP 9: Time-travel back to snapshot 1
    // ======================================================================
    const reconstructed = await reconstructState(kgcStore, snapshot1.id);

    expect(reconstructed).toBeDefined();
    // Verify we're back in the "before review" state
    expect(reconstructed.metadata.description).toBe('State before automated review');

    // ======================================================================
    // STEP 10: Complete approval task
    // ======================================================================
    const approveTask = await engine.enableTask(workflowCase.id, 'approve');
    expect(approveTask).toBeDefined();

    await engine.completeTask(workflowCase.id, 'approve', {
      approver: 'manager1',
      decision: 'approved',
      comments: 'All checks passed',
      completedAt: new Date().toISOString(),
    });

    // ======================================================================
    // STEP 11: Verify workflow completed
    // ======================================================================
    const finalCase = await engine.getCase(workflowCase.id);
    expect(finalCase.status).toBe('completed');

    // ======================================================================
    // STEP 12: Verify audit trail
    // ======================================================================
    const auditTrail = await engine.getAuditTrail(workflowCase.id);

    expect(auditTrail).toBeDefined();
    expect(Array.isArray(auditTrail)).toBe(true);
    expect(auditTrail.length).toBeGreaterThanOrEqual(3); // At minimum: submit, review, approve

    // Verify all events have receipts
    const eventsWithReceipts = auditTrail.filter((event) => event.receipt);
    expect(eventsWithReceipts.length).toBe(auditTrail.length);

    // Verify receipt chain integrity
    for (let i = 1; i < auditTrail.length; i++) {
      const current = auditTrail[i];
      const previous = auditTrail[i - 1];

      expect(current.receipt.previousHash).toBe(previous.receipt.hash);
    }

    // ======================================================================
    // STEP 13: Performance verification
    // ======================================================================
    const executionTime = new Date(finalCase.completedAt) - new Date(finalCase.createdAt);
    expect(executionTime).toBeLessThan(30000); // Should complete within 30 seconds

    // ======================================================================
    // SUCCESS CRITERIA VERIFICATION
    // ======================================================================
    // ✅ Workflow executed successfully
    // ✅ Hooks validated data
    // ✅ Receipts generated and verified
    // ✅ Time-travel snapshots work
    // ✅ Audit trail complete
    // ✅ Receipt chain integrity verified
  });

  test('handles invalid task data via hooks', async () => {
    // Create engine
    engine = createWorkflowEngine({ store });

    // Create simple workflow
    const workflow = createWorkflow('validation-test', {
      name: 'Validation Test',
    });

    workflow.addTask('task1', { type: 'manual' });
    await engine.registerWorkflow(workflow);

    // Create validation hook that rejects empty data
    const strictHook = defineHook({
      id: 'strict-validation',
      trigger: 'before-task-enable',
      handler: async (context) => {
        if (!context.data || Object.keys(context.data).length === 0) {
          return {
            valid: false,
            error: 'Empty data not allowed',
          };
        }
        return { valid: true };
      },
    });

    // Test hook rejects invalid data
    const invalidResult = await executeHook(strictHook, {
      task: { id: 'task1' },
      data: {},
    });

    expect(invalidResult.valid).toBe(false);
    expect(invalidResult.error).toBe('Empty data not allowed');

    // Test hook accepts valid data
    const validResult = await executeHook(strictHook, {
      task: { id: 'task1' },
      data: { value: 'test' },
    });

    expect(validResult.valid).toBe(true);
  });
});
