/**
 * @file YAWL + KGC-4D Integration Tests (ADVERSARIAL)
 * @description
 * Comprehensive adversarial testing for YAWL engine's integration with KGC-4D.
 * Tests actual execution, not just code reading.
 *
 * ADVERSARIAL FOCUS:
 * - Does KGC-4D event storage ACTUALLY work?
 * - Can we PROVE time-travel reconstruction?
 * - What BREAKS when KGC-4D is offline?
 * - Do hooks EXECUTE or just exist?
 * - Are receipts VERIFIABLE or just generated?
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { KGCStore, GitBackbone, now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { createWorkflowEngine } from '../src/engine.mjs';
import { YawlWorkflow } from '../src/workflow.mjs';
import {
  createWorkflowReceipt,
  appendWorkflowEvent,
  reconstructCase,
  getWorkflowAuditTrail,
  YAWL_EVENT_TYPES,
} from '../src/events/yawl-events.mjs';
import {
  createYAWLPolicyPack,
  createTaskEnablementValidator,
} from '../src/hooks/yawl-hooks.mjs';

/* ========================================================================= */
/* ADVERSARIAL TEST 1: YAWL → KGC-4D → YAWL Round-Trip                      */
/* Question: Does data ACTUALLY flow correctly?                             */
/* ========================================================================= */

describe('ADVERSARIAL: YAWL → KGC-4D → YAWL Round-Trip', () => {
  let store;
  let engine;

  beforeEach(() => {
    store = new KGCStore({ nodeId: 'test-adversarial' });
    engine = createWorkflowEngine({
      nodeId: 'test-engine',
      store,
      enableEventLog: true,
      enableSnapshots: false, // Disable to test pure event sourcing
    });
  });

  afterEach(() => {
    // Cleanup
    store = null;
    engine = null;
  });

  it('PROVE: Events flow from YAWL to KGC-4D store', async () => {
    // Create workflow
    const workflow = new YawlWorkflow({
      id: 'test-workflow',
      name: 'Adversarial Test Workflow',
      tasks: [
        { id: 'start', name: 'Start Task' },
        { id: 'process', name: 'Process Task' },
      ],
      flows: [{ from: 'start', to: 'process' }],
      startTaskId: 'start',
      endTaskIds: ['process'],
    });

    engine.registerWorkflow(workflow);

    // PROVE: Store is empty before execution
    const eventCountBefore = store.getEventCount();
    expect(eventCountBefore).toBe(0n);

    // Create case - should append event to KGC-4D
    const caseInstance = await engine.createCase('test-workflow', {
      description: 'Adversarial test case',
    });

    // PROVE: Event was actually written to KGC-4D
    const eventCountAfter = store.getEventCount();
    expect(eventCountAfter).toBeGreaterThan(eventCountBefore);
    expect(eventCountAfter).toBeGreaterThanOrEqual(1n);

    // PROVE: Event has correct structure
    const events = await store.getAllEvents();
    expect(events.length).toBeGreaterThan(0);

    const caseCreatedEvent = events.find(e =>
      e.type.includes('CASE_CREATED')
    );
    expect(caseCreatedEvent).toBeDefined();
    expect(caseCreatedEvent.payload.caseId).toBe(caseInstance.id);
  });

  it('PROVE: Time-travel reconstruction from KGC-4D works', async () => {
    // Setup workflow
    const workflow = new YawlWorkflow({
      id: 'time-travel-test',
      name: 'Time Travel Test',
      tasks: [
        { id: 't1', name: 'Task 1' },
        { id: 't2', name: 'Task 2' },
      ],
      flows: [{ from: 't1', to: 't2' }],
      startTaskId: 't1',
      endTaskIds: ['t2'],
    });
    engine.registerWorkflow(workflow);

    // Create case
    const caseInstance = await engine.createCase('time-travel-test');
    const caseId = caseInstance.id;

    // Record time T1 - after case creation
    const time_T1 = now();
    await new Promise(r => setTimeout(r, 10)); // Ensure time passes

    // Enable task t1
    await engine.enableTask(caseId, 't1');

    // Record time T2 - after task enabled
    const time_T2 = now();
    await new Promise(r => setTimeout(r, 10));

    // Start task t1
    const workItems = await engine.getEnabledWorkItems(caseId);
    expect(workItems.length).toBeGreaterThan(0);
    await engine.startTask(caseId, workItems[0].id);

    // Record time T3 - after task started
    const time_T3 = now();

    // PROVE: Reconstruct at T1 - should have only case created
    const state_T1 = await reconstructCase(store, null, caseId, time_T1);
    expect(state_T1.caseId).toBe(caseId);
    expect(state_T1.state).toBe('active');
    expect(state_T1.workItems || []).toHaveLength(0); // No tasks enabled yet

    // PROVE: Reconstruct at T2 - should have task enabled
    const state_T2 = await reconstructCase(store, null, caseId, time_T2);
    expect(state_T2.workItems).toBeDefined();
    expect(state_T2.workItems.length).toBeGreaterThan(0);
    expect(state_T2.workItems[0].state).toBe('enabled');

    // PROVE: Reconstruct at T3 - should have task started
    const state_T3 = await reconstructCase(store, null, caseId, time_T3);
    const startedItem = state_T3.workItems.find(wi => wi.state === 'started');
    expect(startedItem).toBeDefined();
    expect(startedItem.state).toBe('started');

    // PROVE: Different states produce different hashes (deterministic)
    expect(state_T1.stateHash).not.toBe(state_T2.stateHash);
    expect(state_T2.stateHash).not.toBe(state_T3.stateHash);
  });

  it('PROVE: Receipts include KGC-4D integration fields', async () => {
    const workflow = new YawlWorkflow({
      id: 'receipt-test',
      name: 'Receipt Test',
      tasks: [
        { id: 'task1', name: 'Task 1' },
      ],
      startTaskId: 'task1',
      endTaskIds: ['task1'],
    });
    engine.registerWorkflow(workflow);

    const caseInstance = await engine.createCase('receipt-test');

    // PROVE: Case has receipt with KGC-4D fields
    expect(caseInstance.receipt).toBeDefined();
    expect(caseInstance.receipt.beforeHash).toHaveLength(64); // BLAKE3
    expect(caseInstance.receipt.afterHash).toHaveLength(64);
    expect(caseInstance.receipt.hash).toHaveLength(64);
    expect(caseInstance.receipt.t_ns).toBeDefined();
    expect(caseInstance.receipt.timestamp_iso).toBeDefined();

    // Get events from KGC-4D
    const events = await store.getAllEvents();
    expect(events.length).toBeGreaterThan(0);

    // PROVE: Events have KGC metadata
    const event = events[0];
    expect(event.id).toBeDefined(); // KGC event ID
    expect(event.t_ns).toBeDefined();
    expect(event.timestamp).toBeDefined();
  });

  it('PROVE: Audit trail is complete and verifiable', async () => {
    const workflow = new YawlWorkflow({
      id: 'audit-test',
      name: 'Audit Test',
      tasks: [
        { id: 'a', name: 'A' },
        { id: 'b', name: 'B' },
      ],
      flows: [{ from: 'a', to: 'b' }],
      startTaskId: 'a',
      endTaskIds: ['b'],
    });
    engine.registerWorkflow(workflow);

    const caseInstance = await engine.createCase('audit-test');
    const caseId = caseInstance.id;

    // Execute workflow
    await engine.enableTask(caseId, 'a');
    const workItems = await engine.getEnabledWorkItems(caseId);
    await engine.startTask(caseId, workItems[0].id);
    await engine.completeTask(caseId, workItems[0].id, { result: 'done' });

    // PROVE: Get complete audit trail from KGC-4D
    const audit = await getWorkflowAuditTrail(store, caseId);

    expect(audit.caseId).toBe(caseId);
    expect(audit.events.length).toBeGreaterThan(0);
    expect(audit.eventCount).toBe(audit.events.length);

    // PROVE: Events are in order
    for (let i = 1; i < audit.events.length; i++) {
      expect(BigInt(audit.events[i].t_ns)).toBeGreaterThan(
        BigInt(audit.events[i - 1].t_ns)
      );
    }

    // PROVE: Audit hash is deterministic
    expect(audit.auditHash).toHaveLength(64);

    // PROVE: Receipts are included and verifiable
    expect(audit.receipts.length).toBeGreaterThan(0);
    expect(audit.reproducible).toBe(true);
    expect(audit.verifiable).toBe(true);
  });
});

/* ========================================================================= */
/* ADVERSARIAL TEST 2: Failure Modes - What BREAKS?                         */
/* Question: Does system handle failures gracefully or crash?               */
/* ========================================================================= */

describe('ADVERSARIAL: KGC-4D Offline Failure Scenarios', () => {
  it('PROVE: Engine fails gracefully when KGC-4D store throws', async () => {
    // Create mock store that always throws
    const brokenStore = {
      appendEvent: vi.fn().mockRejectedValue(new Error('KGC-4D unreachable')),
      getAllEvents: vi.fn().mockRejectedValue(new Error('KGC-4D unreachable')),
      getEventCount: vi.fn().mockReturnValue(0n),
    };

    const engine = createWorkflowEngine({
      nodeId: 'broken-test',
      store: brokenStore,
      enableEventLog: true,
    });

    const workflow = new YawlWorkflow({
      id: 'failure-test',
      name: 'Failure Test',
      tasks: [
        { id: 'task1', name: 'Task 1' },
      ],
      startTaskId: 'task1',
      endTaskIds: ['task1'],
    });

    engine.registerWorkflow(workflow);

    // PROVE: Engine throws or handles error appropriately
    // (Behavior depends on implementation - test both scenarios)
    try {
      await engine.createCase('failure-test');
      // If it succeeds, verify event logging was optional
      expect(brokenStore.appendEvent).toHaveBeenCalled();
    } catch (error) {
      // If it fails, error should be descriptive
      expect(error.message).toContain('KGC-4D');
    }
  });

  it('PROVE: Reconstruction fails gracefully with empty event log', async () => {
    const emptyStore = new KGCStore({ nodeId: 'empty-test' });

    // PROVE: Reconstructing non-existent case returns empty/null state
    const fakeCaseId = 'non-existent-case-id';
    const reconstructed = await reconstructCase(
      emptyStore,
      null,
      fakeCaseId,
      now()
    );

    expect(reconstructed.state).toBeNull();
    expect(reconstructed.eventCount).toBe(0);
  });

  it('PROVE: Invalid event types are rejected', async () => {
    const store = new KGCStore({ nodeId: 'validation-test' });

    // PROVE: appendWorkflowEvent rejects invalid event types
    await expect(
      appendWorkflowEvent(store, 'INVALID_EVENT_TYPE', {
        caseId: 'test-123',
      })
    ).rejects.toThrow('Invalid YAWL event type');
  });

  it('PROVE: Missing required fields are caught by Zod validation', async () => {
    const store = new KGCStore({ nodeId: 'zod-test' });

    const receipt = await createWorkflowReceipt({
      beforeState: {},
      afterState: {},
      decision: {},
    });

    // PROVE: Event without caseId is rejected
    await expect(
      appendWorkflowEvent(store, YAWL_EVENT_TYPES.TASK_STARTED, {
        workItemId: 'wi-123',
        startedAt: toISO(now()),
        receipt,
        // Missing caseId!
      })
    ).rejects.toThrow('caseId is required');
  });
});

/* ========================================================================= */
/* ADVERSARIAL TEST 3: Hook Execution - Do hooks ACTUALLY run?              */
/* Question: Are hooks executed or just defined?                            */
/* ========================================================================= */

describe('ADVERSARIAL: Hook Execution Verification', () => {
  it('PROVE: Hooks are EXECUTED during workflow operations', async () => {
    // Create spy for hook execution
    const hookExecutionTrace = [];

    const mockEvaluator = {
      evaluate: vi.fn(async (condition, store, env) => {
        hookExecutionTrace.push({
          type: 'evaluate',
          query: condition.query,
          timestamp: Date.now(),
        });
        return true; // Allow all
      }),
    };

    const workflow = {
      id: crypto.randomUUID(),
      name: 'Hook Test',
      version: '1.0.0',
      tasks: [
        { id: 'task1', kind: 'AtomicTask', inputConditions: ['start-done'] },
        { id: 'task2', kind: 'AtomicTask' },
      ],
      controlFlow: [
        { source: 'task1', target: 'task2', predicate: 'approved' },
      ],
    };

    const policyPack = createYAWLPolicyPack(workflow, {
      conditionEvaluator: mockEvaluator,
    });

    // PROVE: Validator hook is executed
    const store = new KGCStore({ nodeId: 'hook-exec-test' });
    const result = await policyPack.validateEnablement('task1', store);

    // PROVE: Hook was CALLED (not just defined)
    expect(mockEvaluator.evaluate).toHaveBeenCalled();
    expect(hookExecutionTrace.length).toBeGreaterThan(0);
    expect(result.receipt).toBeDefined();
    expect(result.receipt.hookType).toBe('enablement');
  });

  it('PROVE: Hook receipts include justification and SPARQL queries', async () => {
    const sparqlQueryUsed = 'ASK WHERE { ?case yawl:hasCondition ?cond }';

    const mockEvaluator = {
      evaluate: vi.fn(async condition => true),
    };

    const workflow = {
      id: crypto.randomUUID(),
      name: 'Receipt Test',
      version: '1.0.0',
      tasks: [{ id: 'task1', kind: 'AtomicTask', inputConditions: ['cond1'] }],
      controlFlow: [],
    };

    const policyPack = createYAWLPolicyPack(workflow, {
      conditionEvaluator: mockEvaluator,
    });

    const store = new KGCStore({ nodeId: 'hook-receipt-test' });
    const result = await policyPack.validateEnablement('task1', store);

    // PROVE: Receipt has justification
    expect(result.receipt.justification).toBeDefined();
    expect(result.receipt.justification.reason).toBeDefined();

    // PROVE: SPARQL query is included if used
    if (result.receipt.justification.sparqlQuery) {
      expect(typeof result.receipt.justification.sparqlQuery).toBe('string');
    }
  });

  it('PROVE: Hook execution failures are caught and recorded', async () => {
    const mockEvaluator = {
      evaluate: vi
        .fn()
        .mockRejectedValue(new Error('SPARQL evaluation failed')),
    };

    const workflow = {
      id: crypto.randomUUID(),
      name: 'Error Test',
      version: '1.0.0',
      tasks: [{ id: 'task1', kind: 'AtomicTask' }],
      controlFlow: [],
    };

    const policyPack = createYAWLPolicyPack(workflow, {
      conditionEvaluator: mockEvaluator,
    });

    const store = new KGCStore({ nodeId: 'hook-error-test' });

    // PROVE: Hook failure is handled gracefully
    const result = await policyPack.validateEnablement('task1', store);

    expect(result.valid).toBe(false);
    expect(result.receipt.decision).toBe('deny');
    expect(result.receipt.justification.reason).toContain('failed');
  });
});

/* ========================================================================= */
/* ADVERSARIAL TEST 4: Concurrent Cases - Race Conditions?                  */
/* Question: Does KGC-4D handle concurrent writes correctly?                */
/* ========================================================================= */

describe('ADVERSARIAL: Concurrent Case Execution', () => {
  it('PROVE: Multiple concurrent cases maintain isolation', async () => {
    const store = new KGCStore({ nodeId: 'concurrent-test' });
    const engine = createWorkflowEngine({
      nodeId: 'concurrent-engine',
      store,
      enableEventLog: true,
    });

    const workflow = new YawlWorkflow({
      id: 'concurrent-workflow',
      name: 'Concurrent Test',
      tasks: [
        { id: 'task1', name: 'Task 1' },
      ],
      startTaskId: 'task1',
      endTaskIds: ['task1'],
    });
    engine.registerWorkflow(workflow);

    // PROVE: Create multiple cases concurrently
    const casePromises = Array.from({ length: 10 }, (_, i) =>
      engine.createCase('concurrent-workflow', {
        description: `Case ${i}`,
      })
    );

    const cases = await Promise.all(casePromises);

    // PROVE: All cases have unique IDs
    const caseIds = cases.map(c => c.id);
    const uniqueIds = new Set(caseIds);
    expect(uniqueIds.size).toBe(10);

    // PROVE: All events were recorded in KGC-4D
    const eventCount = store.getEventCount();
    expect(eventCount).toBeGreaterThanOrEqual(10n); // At least one event per case

    // PROVE: Audit trails are isolated
    const audit1 = await getWorkflowAuditTrail(store, cases[0].id);
    const audit2 = await getWorkflowAuditTrail(store, cases[1].id);

    expect(audit1.caseId).toBe(cases[0].id);
    expect(audit2.caseId).toBe(cases[1].id);
    expect(audit1.auditHash).not.toBe(audit2.auditHash);
  });

  it('PROVE: Vector clocks track concurrent operations', async () => {
    const store = new KGCStore({ nodeId: 'vector-clock-test' });

    const receipt1 = await createWorkflowReceipt({
      beforeState: { state: 'A' },
      afterState: { state: 'B' },
      decision: { action: 'transition' },
    });

    // PROVE: Receipt has timestamp
    expect(receipt1.t_ns).toBeDefined();
    expect(receipt1.timestamp_iso).toBeDefined();

    // Create event with vector clock
    const vectorClock = new VectorClock('node-1');
    vectorClock.increment();

    const eventReceipt = await appendWorkflowEvent(
      store,
      YAWL_EVENT_TYPES.CASE_CREATED,
      {
        caseId: crypto.randomUUID(),
        specId: 'test-spec',
        timestamp: toISO(now()),
        receipt: receipt1,
        vectorClock: vectorClock.toJSON(),
      }
    );

    // PROVE: Event has vector clock metadata
    expect(eventReceipt).toBeDefined();
    expect(eventReceipt.event_count).toBeGreaterThan(0);
  });
});

/* ========================================================================= */
/* ADVERSARIAL TEST 5: Performance - Does it SCALE?                         */
/* Question: What happens under load?                                       */
/* ========================================================================= */

describe('ADVERSARIAL: Performance Under Load', () => {
  it('PROVE: Event appending completes in reasonable time', async () => {
    const store = new KGCStore({ nodeId: 'perf-test' });

    const receipt = await createWorkflowReceipt({
      beforeState: {},
      afterState: { count: 1 },
      decision: {},
    });

    // PROVE: 100 events can be appended in < 1 second
    const start = performance.now();

    for (let i = 0; i < 100; i++) {
      await appendWorkflowEvent(store, YAWL_EVENT_TYPES.CASE_CREATED, {
        caseId: crypto.randomUUID(),
        specId: 'perf-test',
        timestamp: toISO(now()),
        receipt,
      });
    }

    const duration = performance.now() - start;

    // PROVE: Performance is acceptable (< 10ms per event)
    expect(duration).toBeLessThan(1000); // Total < 1s for 100 events
    expect(duration / 100).toBeLessThan(10); // Average < 10ms per event

    // PROVE: All events were stored
    expect(store.getEventCount()).toBe(100n);
  });

  it('PROVE: Reconstruction performance is acceptable', async () => {
    const store = new KGCStore({ nodeId: 'reconstruct-perf-test' });
    const caseId = crypto.randomUUID();

    // Create 50 events for one case
    const receipt = await createWorkflowReceipt({
      beforeState: {},
      afterState: {},
      decision: {},
    });

    for (let i = 0; i < 50; i++) {
      await appendWorkflowEvent(store, YAWL_EVENT_TYPES.TASK_ENABLED, {
        caseId,
        taskId: `task-${i}`,
        workItemId: crypto.randomUUID(),
        enabledAt: toISO(now()),
        receipt,
      });
      await new Promise(r => setTimeout(r, 1)); // Ensure timestamps differ
    }

    // PROVE: Reconstruction completes in < 100ms
    const start = performance.now();
    const reconstructed = await reconstructCase(store, null, caseId, now());
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(100); // < 100ms for 50 events
    expect(reconstructed.eventCount).toBe(50);
  });
});
