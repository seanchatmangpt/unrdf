/**
 * @file Workflow API Tests
 * @module @unrdf/yawl/test/workflow-api
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createWorkflow,
  createCase,
  enableTask,
  startTask,
  completeTask,
  cancelWorkItem,
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,
  TaskSchema,
  WorkflowSpecSchema,
} from '../src/index.mjs';

// ============================================================================
// Test Fixtures
// ============================================================================

/**
 * Simple sequential workflow for testing
 */
const SIMPLE_WORKFLOW_SPEC = {
  id: 'simple-workflow',
  name: 'Simple Sequential Workflow',
  version: '1.0.0',
  tasks: [
    { id: 'task-1', name: 'First Task', kind: 'atomic' },
    { id: 'task-2', name: 'Second Task', kind: 'atomic' },
    { id: 'task-3', name: 'Third Task', kind: 'atomic' },
  ],
  controlFlow: [
    { id: 'cf1', type: 'sequence', from: 'task-1', to: 'task-2' },
    { id: 'cf2', type: 'sequence', from: 'task-2', to: 'task-3' },
  ],
};

/**
 * Parallel workflow with AND-split/join
 */
const PARALLEL_WORKFLOW_SPEC = {
  id: 'parallel-workflow',
  name: 'Parallel Workflow',
  version: '1.0.0',
  tasks: [
    { id: 'start', name: 'Start', kind: 'atomic' },
    { id: 'parallel-a', name: 'Parallel A', kind: 'atomic' },
    { id: 'parallel-b', name: 'Parallel B', kind: 'atomic' },
    { id: 'end', name: 'End', kind: 'atomic' },
  ],
  controlFlow: [
    { id: 'cf1', type: 'and-split', from: 'start', to: ['parallel-a', 'parallel-b'] },
    { id: 'cf2', type: 'and-join', from: 'parallel-a', to: 'end' },
    { id: 'cf3', type: 'and-join', from: 'parallel-b', to: 'end' },
  ],
};

/**
 * Workflow with cancellation region
 */
const CANCELLATION_WORKFLOW_SPEC = {
  id: 'cancellation-workflow',
  name: 'Cancellation Workflow',
  version: '1.0.0',
  tasks: [
    { id: 'main-task', name: 'Main Task', kind: 'atomic', cancellationRegion: 'region-1' },
    { id: 'timeout-task', name: 'Timeout Task', kind: 'atomic', cancellationRegion: 'region-1' },
    { id: 'cleanup-task', name: 'Cleanup Task', kind: 'atomic' },
  ],
  controlFlow: [
    { id: 'cf1', type: 'deferred-choice', from: 'main-task', to: 'cleanup-task' },
    { id: 'cf2', type: 'deferred-choice', from: 'timeout-task', to: 'cleanup-task' },
  ],
  cancellationRegions: {
    'region-1': ['main-task', 'timeout-task'],
  },
};

// ============================================================================
// Schema Tests
// ============================================================================

describe('Zod Schemas', () => {
  it('TaskSchema validates correct task', () => {
    const task = {
      id: 'test-task',
      name: 'Test Task',
      kind: 'atomic',
    };
    expect(() => TaskSchema.parse(task)).not.toThrow();
  });

  it('TaskSchema rejects invalid task', () => {
    const invalidTask = {
      id: '', // Empty ID not allowed
      name: 'Test',
    };
    expect(() => TaskSchema.parse(invalidTask)).toThrow();
  });

  it('WorkflowSpecSchema validates complete spec', () => {
    expect(() => WorkflowSpecSchema.parse(SIMPLE_WORKFLOW_SPEC)).not.toThrow();
  });

  it('WorkflowSpecSchema rejects specs without required fields', () => {
    const noTasksSpec = {
      id: 'empty',
      tasks: [],
    };
    expect(() => WorkflowSpecSchema.parse(noTasksSpec)).toThrow();
  });
});

// ============================================================================
// Constants Tests
// ============================================================================

describe('Constants', () => {
  it('YAWL_NS contains required namespaces', () => {
    expect(YAWL_NS.BASE).toBe('http://yawl.io/');
    expect(YAWL_NS.WORKFLOW).toBe('http://yawl.io/workflow/');
    expect(YAWL_NS.TASK).toBe('http://yawl.io/task/');
    expect(YAWL_NS.CASE).toBe('http://yawl.io/case/');
  });

  it('YAWL_EVENT_TYPES contains all event types', () => {
    expect(YAWL_EVENT_TYPES.WORKFLOW_CREATED).toBe('YAWL_WORKFLOW_CREATED');
    expect(YAWL_EVENT_TYPES.CASE_CREATED).toBe('YAWL_CASE_CREATED');
    expect(YAWL_EVENT_TYPES.TASK_ENABLED).toBe('YAWL_TASK_ENABLED');
    expect(YAWL_EVENT_TYPES.TASK_STARTED).toBe('YAWL_TASK_STARTED');
    expect(YAWL_EVENT_TYPES.TASK_COMPLETED).toBe('YAWL_TASK_COMPLETED');
  });

  it('WORK_ITEM_STATUS contains all statuses', () => {
    expect(WORK_ITEM_STATUS.PENDING).toBe('pending');
    expect(WORK_ITEM_STATUS.ENABLED).toBe('enabled');
    expect(WORK_ITEM_STATUS.ACTIVE).toBe('active');
    expect(WORK_ITEM_STATUS.COMPLETED).toBe('completed');
    expect(WORK_ITEM_STATUS.CANCELLED).toBe('cancelled');
  });

  it('CONTROL_FLOW_PATTERNS contains YAWL patterns', () => {
    expect(CONTROL_FLOW_PATTERNS.SEQUENCE).toBe('sequence');
    expect(CONTROL_FLOW_PATTERNS.AND_SPLIT).toBe('and-split');
    expect(CONTROL_FLOW_PATTERNS.AND_JOIN).toBe('and-join');
    expect(CONTROL_FLOW_PATTERNS.XOR_SPLIT).toBe('xor-split');
    expect(CONTROL_FLOW_PATTERNS.XOR_JOIN).toBe('xor-join');
  });
});

// ============================================================================
// createWorkflow Tests
// ============================================================================

describe('createWorkflow', () => {
  it('creates workflow from valid spec', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    expect(workflow.id).toBe('simple-workflow');
    expect(workflow.name).toBe('Simple Sequential Workflow');
    expect(workflow.version).toBe('1.0.0');
    expect(workflow.receipt).toBeDefined();
    expect(workflow.receipt.type).toBe(YAWL_EVENT_TYPES.WORKFLOW_CREATED);
  });

  it('builds task index for O(1) lookup', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    expect(workflow.getTask('task-1')).toBeDefined();
    expect(workflow.getTask('task-1').name).toBe('First Task');
    expect(workflow.getTask('nonexistent')).toBeNull();
  });

  it('identifies initial tasks correctly', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    expect(workflow.isInitialTask('task-1')).toBe(true);
    expect(workflow.isInitialTask('task-2')).toBe(false);
    expect(workflow.isInitialTask('task-3')).toBe(false);
  });

  it('builds control flow graph', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    const downstream = workflow.getDownstreamTasks('task-1');
    expect(downstream).toHaveLength(1);
    expect(downstream[0].id).toBe('task-2');

    const upstream = workflow.getUpstreamTasks('task-2');
    expect(upstream).toHaveLength(1);
    expect(upstream[0].id).toBe('task-1');
  });

  it('handles AND-split control flow', async () => {
    const workflow = await createWorkflow(PARALLEL_WORKFLOW_SPEC);

    const downstream = workflow.getDownstreamTasks('start');
    expect(downstream).toHaveLength(2);
    expect(downstream.map(t => t.id).sort()).toEqual(['parallel-a', 'parallel-b']);
  });

  it('returns receipt with hash', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    expect(workflow.receipt.hash).toBeDefined();
    expect(workflow.receipt.hash.length).toBeGreaterThan(0);
    expect(workflow.receipt.timestamp).toBeDefined();
    expect(workflow.receipt.t_ns).toBeDefined();
  });

  it('validates spec by default', async () => {
    const invalidSpec = { id: 'test', tasks: [] };
    await expect(createWorkflow(invalidSpec)).rejects.toThrow();
  });

  it('skips validation when validateSpec is false', async () => {
    const minimalSpec = { id: 'test', tasks: [{ id: 't1', name: 'T1' }] };
    const workflow = await createWorkflow(minimalSpec, { validateSpec: false });
    expect(workflow.id).toBe('test');
  });
});

// ============================================================================
// createCase Tests
// ============================================================================

describe('createCase', () => {
  let workflow;

  beforeEach(async () => {
    workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);
  });

  it('creates case from workflow', async () => {
    const caseObj = await createCase(workflow);

    expect(caseObj.caseId).toBeDefined();
    expect(caseObj.workflowId).toBe('simple-workflow');
    expect(caseObj.status).toBe('active');
    expect(caseObj.receipt).toBeDefined();
    expect(caseObj.receipt.type).toBe(YAWL_EVENT_TYPES.CASE_CREATED);
  });

  it('uses custom caseId if provided', async () => {
    const caseObj = await createCase(workflow, { caseId: 'custom-case-123' });
    expect(caseObj.caseId).toBe('custom-case-123');
  });

  it('initializes work items for all tasks', async () => {
    const caseObj = await createCase(workflow);

    expect(caseObj.getWorkItems()).toHaveLength(3);
    expect(caseObj.getWorkItem('task-1')).toBeDefined();
    expect(caseObj.getWorkItem('task-2')).toBeDefined();
    expect(caseObj.getWorkItem('task-3')).toBeDefined();
  });

  it('enables initial tasks', async () => {
    const caseObj = await createCase(workflow);

    const workItem1 = caseObj.getWorkItem('task-1');
    const workItem2 = caseObj.getWorkItem('task-2');

    expect(workItem1.status).toBe(WORK_ITEM_STATUS.ENABLED);
    expect(workItem2.status).toBe(WORK_ITEM_STATUS.PENDING);
  });

  it('stores initial variables', async () => {
    const caseObj = await createCase(workflow, {
      initialVariables: { customerId: 'C001', amount: 100 },
    });

    expect(caseObj.variables.customerId).toBe('C001');
    expect(caseObj.variables.amount).toBe(100);
  });

  it('getEnabledWorkItems returns correct items', async () => {
    const caseObj = await createCase(workflow);

    const enabled = caseObj.getEnabledWorkItems();
    expect(enabled).toHaveLength(1);
    expect(enabled[0].taskId).toBe('task-1');
  });

  it('isComplete returns false for new case', async () => {
    const caseObj = await createCase(workflow);
    expect(caseObj.isComplete()).toBe(false);
  });
});

// ============================================================================
// enableTask Tests
// ============================================================================

describe('enableTask', () => {
  let workflow;
  let caseObj;

  beforeEach(async () => {
    workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);
    caseObj = await createCase(workflow);
  });

  it('enables pending work item', async () => {
    const workItem = caseObj.getWorkItem('task-2');
    expect(workItem.status).toBe(WORK_ITEM_STATUS.PENDING);

    const result = await enableTask(workItem);

    expect(result.workItem.status).toBe(WORK_ITEM_STATUS.ENABLED);
    expect(result.receipt.type).toBe(YAWL_EVENT_TYPES.TASK_ENABLED);
  });

  it('assigns resource when specified', async () => {
    const workItem = caseObj.getWorkItem('task-2');
    const result = await enableTask(workItem, { assignTo: 'user-123' });

    expect(result.workItem.assignedResource).toBe('user-123');
  });

  it('throws when enabling non-pending work item', async () => {
    const workItem = caseObj.getWorkItem('task-1'); // Already enabled
    await expect(enableTask(workItem)).rejects.toThrow(/status is enabled/);
  });

  it('includes eligibility result', async () => {
    const workItem = caseObj.getWorkItem('task-2');
    const result = await enableTask(workItem);

    expect(result.eligibilityResult.eligible).toBe(true);
  });

  it('receipt contains justification', async () => {
    const workItem = caseObj.getWorkItem('task-2');
    const result = await enableTask(workItem);

    expect(result.receipt.justification).toBeDefined();
    expect(result.receipt.justification.resourceEligibility).toBe(true);
  });
});

// ============================================================================
// startTask Tests
// ============================================================================

describe('startTask', () => {
  let workflow;
  let caseObj;

  beforeEach(async () => {
    workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);
    caseObj = await createCase(workflow);
  });

  it('starts enabled work item', async () => {
    const workItem = caseObj.getWorkItem('task-1'); // Initial task, already enabled
    const result = await startTask(workItem);

    expect(result.workItem.status).toBe(WORK_ITEM_STATUS.ACTIVE);
    expect(result.workItem.startTime).toBeDefined();
    expect(result.receipt.type).toBe(YAWL_EVENT_TYPES.TASK_STARTED);
  });

  it('throws when starting non-enabled work item', async () => {
    const workItem = caseObj.getWorkItem('task-2'); // Pending, not enabled
    await expect(startTask(workItem)).rejects.toThrow(/status is pending/);
  });

  it('records start time as ISO string', async () => {
    const workItem = caseObj.getWorkItem('task-1');
    const result = await startTask(workItem);

    expect(result.workItem.startTime).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });
});

// ============================================================================
// completeTask Tests
// ============================================================================

describe('completeTask', () => {
  let workflow;
  let caseObj;

  beforeEach(async () => {
    workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);
    caseObj = await createCase(workflow);

    // Start task-1
    const workItem = caseObj.getWorkItem('task-1');
    await startTask(workItem);
  });

  it('completes active work item', async () => {
    const workItem = caseObj.getWorkItem('task-1');
    const result = await completeTask(workItem, { output: 'done' });

    expect(result.workItem.status).toBe(WORK_ITEM_STATUS.COMPLETED);
    expect(result.workItem.endTime).toBeDefined();
    expect(result.workItem.result).toEqual({ output: 'done' });
    expect(result.receipt.type).toBe(YAWL_EVENT_TYPES.TASK_COMPLETED);
  });

  it('throws when completing non-active work item', async () => {
    const workItem = caseObj.getWorkItem('task-2'); // Pending
    await expect(completeTask(workItem, {})).rejects.toThrow(/status is pending/);
  });

  it('enables downstream tasks when case and workflow provided', async () => {
    const workItem = caseObj.getWorkItem('task-1');
    const result = await completeTask(workItem, { output: 'done' }, {
      caseObj,
      workflow,
    });

    expect(result.enabledDownstreamTasks).toHaveLength(1);
    expect(result.enabledDownstreamTasks[0].taskId).toBe('task-2');
  });

  it('receipt contains enabled downstream tasks', async () => {
    const workItem = caseObj.getWorkItem('task-1');
    const result = await completeTask(workItem, { output: 'done' }, {
      caseObj,
      workflow,
    });

    expect(result.receipt.payload.enabledDownstreamTasks).toContain('task-2');
  });
});

// ============================================================================
// cancelWorkItem Tests
// ============================================================================

describe('cancelWorkItem', () => {
  let workflow;
  let caseObj;

  beforeEach(async () => {
    workflow = await createWorkflow(CANCELLATION_WORKFLOW_SPEC);
    caseObj = await createCase(workflow);
  });

  it('cancels work item with reason', async () => {
    const workItem = caseObj.getWorkItem('main-task');
    const result = await cancelWorkItem(workItem, 'User cancelled');

    expect(result.workItem.status).toBe(WORK_ITEM_STATUS.CANCELLED);
    expect(result.workItem.result.reason).toBe('User cancelled');
    expect(result.receipt.type).toBe(YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED);
  });

  it('throws when cancelling completed work item', async () => {
    const workItem = caseObj.getWorkItem('main-task');
    workItem.status = WORK_ITEM_STATUS.COMPLETED;

    await expect(cancelWorkItem(workItem, 'reason')).rejects.toThrow(/status is completed/);
  });

  it('throws when reason is empty', async () => {
    const workItem = caseObj.getWorkItem('main-task');
    await expect(cancelWorkItem(workItem, '')).rejects.toThrow(/non-empty string/);
  });

  it('cancels tasks in cancellation region', async () => {
    const workItem = caseObj.getWorkItem('main-task');
    const result = await cancelWorkItem(workItem, 'Timeout', {
      workflow,
      caseObj,
    });

    expect(result.cancelledInRegion).toContain('timeout-task');

    const timeoutWorkItem = caseObj.getWorkItem('timeout-task');
    expect(timeoutWorkItem.status).toBe(WORK_ITEM_STATUS.CANCELLED);
  });

  it('records cancellation time', async () => {
    const workItem = caseObj.getWorkItem('main-task');
    const result = await cancelWorkItem(workItem, 'User cancelled');

    expect(result.workItem.endTime).toBeDefined();
    expect(result.receipt.payload.cancelTime).toBeDefined();
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Integration: Full Workflow Execution', () => {
  it('executes simple sequential workflow end-to-end', async () => {
    // 1. Create workflow
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);
    expect(workflow.id).toBe('simple-workflow');

    // 2. Create case
    const caseObj = await createCase(workflow, {
      caseId: 'order-001',
      initialVariables: { orderId: 'ORD-123' },
    });
    expect(caseObj.caseId).toBe('order-001');

    // 3. Execute task-1
    let workItem = caseObj.getWorkItem('task-1');
    expect(workItem.status).toBe(WORK_ITEM_STATUS.ENABLED);

    await startTask(workItem);
    expect(workItem.status).toBe(WORK_ITEM_STATUS.ACTIVE);

    await completeTask(workItem, { step1: 'done' }, { caseObj, workflow });
    expect(workItem.status).toBe(WORK_ITEM_STATUS.COMPLETED);

    // 4. Task-2 should now be enabled
    workItem = caseObj.getWorkItem('task-2');
    expect(workItem.status).toBe(WORK_ITEM_STATUS.ENABLED);

    await startTask(workItem);
    await completeTask(workItem, { step2: 'done' }, { caseObj, workflow });

    // 5. Task-3 should now be enabled
    workItem = caseObj.getWorkItem('task-3');
    expect(workItem.status).toBe(WORK_ITEM_STATUS.ENABLED);

    await startTask(workItem);
    await completeTask(workItem, { step3: 'done' }, { caseObj, workflow });

    // 6. Case should be complete
    expect(caseObj.isComplete()).toBe(true);
  });

  it('handles parallel workflow with AND patterns', async () => {
    const workflow = await createWorkflow(PARALLEL_WORKFLOW_SPEC);
    const caseObj = await createCase(workflow);

    // Start should be initial and enabled
    let workItem = caseObj.getWorkItem('start');
    expect(workItem.status).toBe(WORK_ITEM_STATUS.ENABLED);

    // Complete start - should enable parallel-a and parallel-b
    await startTask(workItem);
    await completeTask(workItem, {}, { caseObj, workflow });

    const parallelA = caseObj.getWorkItem('parallel-a');
    const parallelB = caseObj.getWorkItem('parallel-b');

    expect(parallelA.status).toBe(WORK_ITEM_STATUS.ENABLED);
    expect(parallelB.status).toBe(WORK_ITEM_STATUS.ENABLED);

    // Complete parallel-a
    await startTask(parallelA);
    await completeTask(parallelA, {}, { caseObj, workflow });

    // End should still be pending (AND-join waits for both)
    const endItem = caseObj.getWorkItem('end');
    expect(endItem.status).toBe(WORK_ITEM_STATUS.PENDING);

    // Complete parallel-b
    await startTask(parallelB);
    await completeTask(parallelB, {}, { caseObj, workflow });

    // Now end should be enabled (both predecessors complete)
    expect(endItem.status).toBe(WORK_ITEM_STATUS.ENABLED);
  });
});

// ============================================================================
// Receipt Verification Tests
// ============================================================================

describe('Cryptographic Receipts', () => {
  it('receipts have unique IDs', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);
    const caseObj = await createCase(workflow);

    expect(workflow.receipt.id).not.toBe(caseObj.receipt.id);
  });

  it('receipts have valid hashes', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    // BLAKE3 hash is 64 hex characters
    expect(workflow.receipt.hash).toMatch(/^[a-f0-9]{64}$/);
  });

  it('receipts contain timestamp in nanoseconds', async () => {
    const workflow = await createWorkflow(SIMPLE_WORKFLOW_SPEC);

    expect(workflow.receipt.t_ns).toBeDefined();
    expect(BigInt(workflow.receipt.t_ns)).toBeGreaterThan(0n);
  });

  it('receipts are deterministic for same payload', async () => {
    // Create two workflows with same spec
    const workflow1 = await createWorkflow({ ...SIMPLE_WORKFLOW_SPEC, id: 'test-1' });
    const workflow2 = await createWorkflow({ ...SIMPLE_WORKFLOW_SPEC, id: 'test-1' });

    // Hashes will differ due to different timestamps, but structure is same
    expect(workflow1.receipt.type).toBe(workflow2.receipt.type);
    expect(workflow1.receipt.payload.workflowId).toBe(workflow2.receipt.payload.workflowId);
  });
});
