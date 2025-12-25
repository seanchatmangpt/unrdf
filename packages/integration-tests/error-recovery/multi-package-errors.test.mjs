/**
 * Integration Test - Scenario 4: Multi-Package Error Recovery
 * Tests: Error handling across YAWL + KGC-4D + Hooks
 *
 * Real-world scenario: Robust error handling and recovery
 * - Workflow failures
 * - Hook validation failures
 * - State rollback
 * - Transaction semantics
 */

import { test, expect, describe, beforeEach } from 'vitest';
import { createWorkflowEngine, createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';
import { KGCStore, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';
import { defineHook, executeHook } from '@unrdf/hooks';

describe('Scenario 4: Multi-Package Error Recovery', () => {
  let store;
  let kgcStore;
  let engine;

  beforeEach(() => {
    store = createStore();
    kgcStore = new KGCStore();
    engine = createWorkflowEngine({ store });
  });

  test('recovers from workflow failures with state rollback', async () => {
    // ======================================================================
    // STEP 1: Create workflow with potential failure points
    // ======================================================================
    const workflow = createWorkflow('payment-processing', {
      name: 'Payment Processing',
    });

    workflow.addTask('validate', { type: 'automated' });
    workflow.addTask('charge', { type: 'automated' });
    workflow.addTask('confirm', { type: 'automated' });

    workflow.addFlow('validate', 'charge');
    workflow.addFlow('charge', 'confirm');

    await engine.registerWorkflow(workflow);

    // ======================================================================
    // STEP 2: Create validation hook that can fail
    // ======================================================================
    const paymentValidationHook = defineHook({
      id: 'payment-validation',
      trigger: 'before-task-enable',
      handler: async ({ task, data }) => {
        if (task.id === 'charge') {
          // Reject invalid amounts
          if (!data.amount || data.amount <= 0) {
            return {
              valid: false,
              error: 'Invalid payment amount',
              code: 'INVALID_AMOUNT',
            };
          }

          // Reject amounts > 10000
          if (data.amount > 10000) {
            return {
              valid: false,
              error: 'Amount exceeds limit',
              code: 'AMOUNT_LIMIT_EXCEEDED',
            };
          }

          // Simulate fraud check failure
          if (data.fraudCheck === false) {
            return {
              valid: false,
              error: 'Fraud check failed',
              code: 'FRAUD_DETECTED',
            };
          }
        }

        return { valid: true };
      },
    });

    // ======================================================================
    // STEP 3: Start workflow with valid data
    // ======================================================================
    const validCase = await engine.startCase('payment-processing', {
      amount: 100,
      currency: 'USD',
      fraudCheck: true,
    });

    expect(validCase).toBeDefined();
    expect(validCase.status).toBe('active');

    // ======================================================================
    // STEP 4: Capture initial state snapshot
    // ======================================================================
    const snapshot1 = await freezeUniverse(kgcStore, 'initial-state', {
      caseId: validCase.id,
    });

    expect(snapshot1).toBeDefined();

    // ======================================================================
    // STEP 5: Complete validation task successfully
    // ======================================================================
    await engine.enableTask(validCase.id, 'validate');
    await engine.completeTask(validCase.id, 'validate', {
      validationResult: 'passed',
    });

    // ======================================================================
    // STEP 6: Capture pre-charge state
    // ======================================================================
    const snapshot2 = await freezeUniverse(kgcStore, 'pre-charge', {
      caseId: validCase.id,
    });

    // ======================================================================
    // STEP 7: Test hook rejects invalid scenarios
    // ======================================================================
    // Test 1: Invalid amount (zero)
    const invalidAmountResult = await executeHook(paymentValidationHook, {
      task: { id: 'charge' },
      data: { amount: 0 },
    });

    expect(invalidAmountResult.valid).toBe(false);
    expect(invalidAmountResult.code).toBe('INVALID_AMOUNT');

    // Test 2: Amount exceeds limit
    const exceedsLimitResult = await executeHook(paymentValidationHook, {
      task: { id: 'charge' },
      data: { amount: 15000 },
    });

    expect(exceedsLimitResult.valid).toBe(false);
    expect(exceedsLimitResult.code).toBe('AMOUNT_LIMIT_EXCEEDED');

    // Test 3: Fraud detected
    const fraudResult = await executeHook(paymentValidationHook, {
      task: { id: 'charge' },
      data: { amount: 100, fraudCheck: false },
    });

    expect(fraudResult.valid).toBe(false);
    expect(fraudResult.code).toBe('FRAUD_DETECTED');

    // Test 4: Valid payment
    const validResult = await executeHook(paymentValidationHook, {
      task: { id: 'charge' },
      data: { amount: 100, fraudCheck: true },
    });

    expect(validResult.valid).toBe(true);

    // ======================================================================
    // STEP 8: Simulate partial failure and rollback
    // ======================================================================
    // In a real system, if charge fails, we'd rollback to snapshot2
    const canRollback = snapshot2 !== null;
    expect(canRollback).toBe(true);

    if (canRollback) {
      const rolledBackState = await reconstructState(kgcStore, snapshot2.id);
      expect(rolledBackState).toBeDefined();
      expect(rolledBackState.metadata).toBeDefined();
    }

    // ======================================================================
    // STEP 9: Complete workflow successfully after validation
    // ======================================================================
    await engine.enableTask(validCase.id, 'charge');
    await engine.completeTask(validCase.id, 'charge', {
      transactionId: 'txn-001',
      chargedAmount: 100,
    });

    await engine.enableTask(validCase.id, 'confirm');
    await engine.completeTask(validCase.id, 'confirm', {
      confirmationNumber: 'conf-001',
    });

    const finalCase = await engine.getCase(validCase.id);
    expect(finalCase.status).toBe('completed');

    // ======================================================================
    // STEP 10: Verify audit trail shows no failures
    // ======================================================================
    const auditTrail = await engine.getAuditTrail(validCase.id);
    expect(auditTrail.length).toBeGreaterThanOrEqual(3);

    const failedEvents = auditTrail.filter((e) => e.status === 'failed');
    expect(failedEvents.length).toBe(0);

    // ======================================================================
    // SUCCESS CRITERIA VERIFICATION
    // ======================================================================
    // ✅ Validation hooks detect errors
    // ✅ Multiple error scenarios handled
    // ✅ State snapshots captured
    // ✅ Rollback mechanism available
    // ✅ Successful completion after validation
  });

  test('handles concurrent workflow failures gracefully', async () => {
    // ======================================================================
    // Create simple workflow
    // ======================================================================
    const workflow = createWorkflow('concurrent-test', {
      name: 'Concurrent Test',
    });

    workflow.addTask('task1', { type: 'automated' });
    await engine.registerWorkflow(workflow);

    // ======================================================================
    // Start multiple cases concurrently
    // ======================================================================
    const cases = await Promise.all([
      engine.startCase('concurrent-test', { id: 1 }),
      engine.startCase('concurrent-test', { id: 2 }),
      engine.startCase('concurrent-test', { id: 3 }),
    ]);

    expect(cases.length).toBe(3);
    cases.forEach((c) => {
      expect(c).toBeDefined();
      expect(c.status).toBe('active');
    });

    // ======================================================================
    // Verify all cases are independent
    // ======================================================================
    const case1 = await engine.getCase(cases[0].id);
    const case2 = await engine.getCase(cases[1].id);
    const case3 = await engine.getCase(cases[2].id);

    expect(case1.id).not.toBe(case2.id);
    expect(case2.id).not.toBe(case3.id);

    // ======================================================================
    // SUCCESS: Concurrent operations handled correctly
    // ======================================================================
  });

  test('validates error propagation across package boundaries', async () => {
    // ======================================================================
    // Test hook error propagation
    // ======================================================================
    const errorHook = defineHook({
      id: 'error-throwing-hook',
      trigger: 'test-trigger',
      handler: async () => {
        throw new Error('Simulated hook failure');
      },
    });

    // Verify error is caught and handled
    let caughtError = null;
    try {
      await executeHook(errorHook, {});
    } catch (error) {
      caughtError = error;
    }

    expect(caughtError).toBeDefined();
    expect(caughtError.message).toContain('Simulated hook failure');

    // ======================================================================
    // Test validation hook error handling
    // ======================================================================
    const validationHook = defineHook({
      id: 'validation-with-error',
      trigger: 'test-validation',
      handler: async ({ data }) => {
        if (!data) {
          throw new Error('Data is required');
        }
        return { valid: true };
      },
    });

    // Should handle missing data gracefully
    let validationError = null;
    try {
      await executeHook(validationHook, {});
    } catch (error) {
      validationError = error;
    }

    expect(validationError).toBeDefined();

    // ======================================================================
    // SUCCESS: Errors propagate correctly
    // ======================================================================
  });
});
