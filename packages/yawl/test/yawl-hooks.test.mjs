/**
 * @file YAWL-Hooks Integration Tests
 * @module yawl/test/yawl-hooks.test
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  createYAWLPolicyPack,
  createTaskEnablementHook,
  createTaskEnablementValidator,
  createTaskCompletionHook,
  createTaskCompletionRouter,
  createResourceAllocationHook,
  createResourceAllocationValidator,
  createCancellationHook,
  createTimeoutHook,
  createCancellationHandler,
  generateEnablementQuery,
  generatePredicateQuery,
  generateResourceCapacityQuery,
  YAWLWorkflowSchema,
  YAWLTaskSchema,
  ControlFlowSchema,
  ResourceConstraintSchema,
  HookReceiptSchema,
} from '../src/hooks/yawl-hooks.mjs';

/* ========================================================================= */
/* Test Fixtures                                                             */
/* ========================================================================= */

const createMockConditionEvaluator = (results = {}) => ({
  evaluate: vi.fn(async (condition, _store, _env) => {
    const query = condition.query || '';
    // Return predefined result or default to true
    for (const [pattern, result] of Object.entries(results)) {
      if (query.includes(pattern)) {
        return result;
      }
    }
    return true;
  }),
});

const createMockStore = () => ({
  getQuads: vi.fn(() => []),
  size: 0,
});

const createMockQuad = (taskId, predicate = 'taskState') => ({
  subject: { value: `http://example.org/task/${taskId}` },
  predicate: { value: `http://yawl.org/${predicate}` },
  object: { value: 'enabled' },
});

const sampleWorkflow = {
  name: 'approval-workflow',
  version: '1.0.0',
  tasks: [
    { id: 'start', kind: 'EmptyTask' },
    {
      id: 'review',
      kind: 'AtomicTask',
      inputConditions: ['start-complete'],
      timeout: 60000,
    },
    {
      id: 'approve',
      kind: 'AtomicTask',
      inputConditions: ['review-complete'],
      cancellationSet: ['reject'],
    },
    {
      id: 'reject',
      kind: 'AtomicTask',
      inputConditions: ['review-complete'],
      cancellationSet: ['approve'],
    },
    {
      id: 'finalize',
      kind: 'AtomicTask',
      inputConditions: ['approve-complete'],
    },
    { id: 'end', kind: 'EmptyTask' },
  ],
  controlFlow: [
    { source: 'start', target: 'review', predicate: 'true', splitType: 'XOR' },
    { source: 'review', target: 'approve', predicate: 'approved', splitType: 'XOR', priority: 60 },
    {
      source: 'review',
      target: 'reject',
      predicate: '!approved',
      splitType: 'XOR',
      priority: 40,
    },
    { source: 'approve', target: 'finalize', predicate: 'true', splitType: 'XOR' },
    { source: 'reject', target: 'end', predicate: 'true', splitType: 'XOR' },
    { source: 'finalize', target: 'end', predicate: 'true', splitType: 'XOR' },
  ],
  resources: [
    { resourceId: 'reviewer', capacity: 5, eligibility: 'ASK { ?user a :Reviewer }' },
    { resourceId: 'approver', capacity: 2 },
  ],
  defaultTimeout: 30000,
  cancellationRegions: {
    'approval-region': ['approve', 'reject'],
  },
};

/* ========================================================================= */
/* Schema Validation Tests                                                   */
/* ========================================================================= */

describe('YAWL Schema Validation', () => {
  describe('YAWLTaskSchema', () => {
    it('should validate a valid atomic task', () => {
      const task = { id: 'task1', kind: 'AtomicTask' };
      const result = YAWLTaskSchema.parse(task);
      expect(result.id).toBe('task1');
      expect(result.kind).toBe('AtomicTask');
    });

    it('should validate a task with all optional fields', () => {
      const task = {
        id: 'task1',
        kind: 'CompositeTask',
        name: 'My Task',
        inputConditions: ['cond1', 'cond2'],
        outputConditions: ['cond3'],
        resourcePattern: 'pattern1',
        cancellationSet: ['task2', 'task3'],
        timeout: 5000,
      };
      const result = YAWLTaskSchema.parse(task);
      expect(result.inputConditions).toEqual(['cond1', 'cond2']);
      expect(result.timeout).toBe(5000);
    });

    it('should reject invalid task kind', () => {
      const task = { id: 'task1', kind: 'InvalidKind' };
      expect(() => YAWLTaskSchema.parse(task)).toThrow();
    });

    it('should reject empty task id', () => {
      const task = { id: '', kind: 'AtomicTask' };
      expect(() => YAWLTaskSchema.parse(task)).toThrow();
    });
  });

  describe('ControlFlowSchema', () => {
    it('should validate a valid control flow edge', () => {
      const edge = { source: 'task1', target: 'task2', predicate: 'approved' };
      const result = ControlFlowSchema.parse(edge);
      expect(result.source).toBe('task1');
      expect(result.target).toBe('task2');
      expect(result.splitType).toBe('XOR'); // default
      expect(result.priority).toBe(50); // default
    });

    it('should validate edge with all options', () => {
      const edge = {
        source: 'task1',
        target: 'task2',
        predicate: '!rejected',
        splitType: 'AND',
        priority: 80,
      };
      const result = ControlFlowSchema.parse(edge);
      expect(result.splitType).toBe('AND');
      expect(result.priority).toBe(80);
    });

    it('should reject invalid split type', () => {
      const edge = { source: 't1', target: 't2', predicate: 'x', splitType: 'INVALID' };
      expect(() => ControlFlowSchema.parse(edge)).toThrow();
    });
  });

  describe('YAWLWorkflowSchema', () => {
    it('should validate the sample workflow', () => {
      const result = YAWLWorkflowSchema.parse(sampleWorkflow);
      expect(result.name).toBe('approval-workflow');
      expect(result.tasks.length).toBe(6);
      expect(result.controlFlow.length).toBe(6);
    });

    it('should add default version if missing', () => {
      const workflow = { name: 'test', tasks: [{ id: 't1', kind: 'AtomicTask' }] };
      const result = YAWLWorkflowSchema.parse(workflow);
      expect(result.version).toBe('1.0.0');
    });

    it('should reject workflow without tasks', () => {
      const workflow = { name: 'test', tasks: [] };
      expect(() => YAWLWorkflowSchema.parse(workflow)).toThrow();
    });
  });

  describe('HookReceiptSchema', () => {
    it('should validate a valid receipt', () => {
      const receipt = {
        receiptId: '550e8400-e29b-41d4-a716-446655440000',
        timestamp: new Date().toISOString(),
        hookType: 'enablement',
        taskId: 'task1',
        workflowId: 'wf1',
        decision: 'allow',
        justification: { reason: 'All conditions met' },
      };
      const result = HookReceiptSchema.parse(receipt);
      expect(result.decision).toBe('allow');
    });

    it('should validate receipt with optional fields', () => {
      const receipt = {
        receiptId: '550e8400-e29b-41d4-a716-446655440000',
        timestamp: new Date().toISOString(),
        hookType: 'completion',
        taskId: 'task1',
        workflowId: 'wf1',
        decision: 'route',
        justification: { reason: 'Routed to next task' },
        enabledTasks: ['task2', 'task3'],
        cancelledTasks: [],
      };
      const result = HookReceiptSchema.parse(receipt);
      expect(result.enabledTasks).toEqual(['task2', 'task3']);
    });
  });
});

/* ========================================================================= */
/* SPARQL Query Generator Tests                                              */
/* ========================================================================= */

describe('SPARQL Query Generators', () => {
  describe('generateEnablementQuery', () => {
    it('should generate query for task with no input conditions', () => {
      const query = generateEnablementQuery('task1', []);
      expect(query).toContain('ASK');
      expect(query).toContain('yawl:taskId "task1"');
    });

    it('should generate query for task with input conditions', () => {
      const query = generateEnablementQuery('task1', ['cond1', 'cond2']);
      expect(query).toContain('ASK');
      expect(query).toContain('yawl:conditionId "cond1"');
      expect(query).toContain('yawl:conditionId "cond2"');
      expect(query).toContain('yawl:satisfied true');
    });
  });

  describe('generatePredicateQuery', () => {
    it('should generate query for positive predicate', () => {
      const query = generatePredicateQuery('approved');
      expect(query).toContain('ASK');
      expect(query).toContain('yawl:name "approved"');
      expect(query).toContain('yawl:value true');
      expect(query).not.toContain('FILTER NOT EXISTS');
    });

    it('should generate query for negated predicate', () => {
      const query = generatePredicateQuery('!approved');
      expect(query).toContain('ASK');
      expect(query).toContain('FILTER NOT EXISTS');
      expect(query).toContain('yawl:name "approved"');
    });
  });

  describe('generateResourceCapacityQuery', () => {
    it('should generate capacity check query', () => {
      const query = generateResourceCapacityQuery('http://example.org/resource/r1', 5);
      expect(query).toContain('ASK');
      expect(query).toContain('SELECT');
      expect(query).toContain('COUNT(?alloc)');
      expect(query).toContain('<http://example.org/resource/r1>');
      expect(query).toContain('HAVING (?allocCount < 5)');
    });
  });
});

/* ========================================================================= */
/* Hook Creation Tests                                                       */
/* ========================================================================= */

describe('Hook Creation', () => {
  const mockEvaluator = createMockConditionEvaluator();
  const task = { id: 'task1', kind: 'AtomicTask', inputConditions: ['cond1'] };
  const workflow = { id: 'wf1', name: 'test', tasks: [task], controlFlow: [] };

  describe('createTaskEnablementHook', () => {
    it('should create a valid hook definition', () => {
      const hook = createTaskEnablementHook(task, workflow, mockEvaluator);
      expect(hook.name).toBe('yawl:enable:task1');
      expect(hook.trigger).toBe('before-add');
      expect(hook.metadata.hookType).toBe('enablement');
      expect(hook.metadata.taskId).toBe('task1');
    });

    it('should pass non-task quads through validation', () => {
      const hook = createTaskEnablementHook(task, workflow, mockEvaluator);
      const nonTaskQuad = { predicate: { value: 'http://example.org/other' } };
      expect(hook.validate(nonTaskQuad)).toBe(true);
    });
  });

  describe('createTaskCompletionHook', () => {
    it('should create a valid hook definition', () => {
      const workflowWithEdges = {
        ...workflow,
        controlFlow: [{ source: 'task1', target: 'task2', predicate: 'done' }],
      };
      const hook = createTaskCompletionHook(task, workflowWithEdges, mockEvaluator);
      expect(hook.name).toBe('yawl:complete:task1');
      expect(hook.trigger).toBe('after-add');
      expect(hook.metadata.outgoingEdges).toBe(1);
    });
  });

  describe('createCancellationHook', () => {
    it('should create a valid cancellation hook', () => {
      const taskWithCancellation = { ...task, cancellationSet: ['task2', 'task3'] };
      const hook = createCancellationHook(taskWithCancellation, workflow);
      expect(hook.name).toBe('yawl:cancel:task1');
      expect(hook.trigger).toBe('on-error');
      expect(hook.metadata.cancellationSet).toEqual(['task2', 'task3']);
    });
  });

  describe('createTimeoutHook', () => {
    it('should create a timeout hook with default timeout', () => {
      const hook = createTimeoutHook(task, { ...workflow, defaultTimeout: 30000 });
      expect(hook.name).toBe('yawl:timeout:task1');
      expect(hook.trigger).toBe('on-timeout');
      expect(hook.metadata.timeout).toBe(30000);
    });

    it('should use task-specific timeout if provided', () => {
      const taskWithTimeout = { ...task, timeout: 5000 };
      const hook = createTimeoutHook(taskWithTimeout, workflow);
      expect(hook.metadata.timeout).toBe(5000);
    });
  });

  describe('createResourceAllocationHook', () => {
    it('should create a resource allocation hook', () => {
      const resource = { resourceId: 'resource1', capacity: 10 };
      const hook = createResourceAllocationHook(resource, workflow, mockEvaluator);
      expect(hook.name).toBe('yawl:allocate:resource1');
      expect(hook.trigger).toBe('before-add');
      expect(hook.metadata.capacity).toBe(10);
    });
  });
});

/* ========================================================================= */
/* Async Validator/Router Tests                                              */
/* ========================================================================= */

describe('Async Validators and Routers', () => {
  describe('createTaskEnablementValidator', () => {
    it('should return valid=true when conditions are satisfied', async () => {
      const evaluator = createMockConditionEvaluator({ satisfied: true });
      const task = { id: 'task1', kind: 'AtomicTask', inputConditions: ['cond1'] };
      const workflow = { id: 'wf1', name: 'test', tasks: [task], controlFlow: [] };

      const validator = createTaskEnablementValidator(task, workflow, evaluator);
      const store = createMockStore();
      const result = await validator(store);

      expect(result.valid).toBe(true);
      expect(result.receipt.decision).toBe('allow');
      expect(result.receipt.hookType).toBe('enablement');
    });

    it('should return valid=false when conditions are not satisfied', async () => {
      const evaluator = createMockConditionEvaluator();
      evaluator.evaluate = vi.fn().mockResolvedValue(false);

      const task = { id: 'task1', kind: 'AtomicTask', inputConditions: ['cond1'] };
      const workflow = { id: 'wf1', name: 'test', tasks: [task], controlFlow: [] };

      const validator = createTaskEnablementValidator(task, workflow, evaluator);
      const store = createMockStore();
      const result = await validator(store);

      expect(result.valid).toBe(false);
      expect(result.receipt.decision).toBe('deny');
    });

    it('should handle evaluation errors gracefully', async () => {
      const evaluator = createMockConditionEvaluator();
      evaluator.evaluate = vi.fn().mockRejectedValue(new Error('Evaluation failed'));

      const task = { id: 'task1', kind: 'AtomicTask' };
      const workflow = { id: 'wf1', name: 'test', tasks: [task], controlFlow: [] };

      const validator = createTaskEnablementValidator(task, workflow, evaluator);
      const store = createMockStore();
      const result = await validator(store);

      expect(result.valid).toBe(false);
      expect(result.receipt.justification.reason).toContain('Evaluation failed');
    });
  });

  describe('createTaskCompletionRouter', () => {
    it('should route to correct task on XOR-split', async () => {
      const evaluator = createMockConditionEvaluator({ approved: true });

      const task = { id: 'review', kind: 'AtomicTask' };
      const workflow = {
        id: 'wf1',
        name: 'test',
        tasks: [task],
        controlFlow: [
          { source: 'review', target: 'approve', predicate: 'approved', splitType: 'XOR' },
          { source: 'review', target: 'reject', predicate: '!approved', splitType: 'XOR' },
        ],
      };

      const router = createTaskCompletionRouter(task, workflow, evaluator);
      const store = createMockStore();
      const result = await router(store);

      expect(result.enabledTasks).toContain('approve');
      expect(result.enabledTasks).not.toContain('reject');
      expect(result.receipt.hookType).toBe('completion');
    });

    it('should enable all tasks on AND-split', async () => {
      const evaluator = createMockConditionEvaluator();

      const task = { id: 'start', kind: 'AtomicTask' };
      const workflow = {
        id: 'wf1',
        name: 'test',
        tasks: [task],
        controlFlow: [
          { source: 'start', target: 'task1', predicate: 'true', splitType: 'AND' },
          { source: 'start', target: 'task2', predicate: 'true', splitType: 'AND' },
        ],
      };

      const router = createTaskCompletionRouter(task, workflow, evaluator);
      const store = createMockStore();
      const result = await router(store);

      expect(result.enabledTasks).toContain('task1');
      expect(result.enabledTasks).toContain('task2');
      expect(result.enabledTasks.length).toBe(2);
    });

    it('should enable multiple matching tasks on OR-split', async () => {
      const evaluator = createMockConditionEvaluator();
      // Both conditions true
      evaluator.evaluate = vi.fn().mockResolvedValue(true);

      const task = { id: 'start', kind: 'AtomicTask' };
      const workflow = {
        id: 'wf1',
        name: 'test',
        tasks: [task],
        controlFlow: [
          { source: 'start', target: 'task1', predicate: 'cond1', splitType: 'OR' },
          { source: 'start', target: 'task2', predicate: 'cond2', splitType: 'OR' },
        ],
      };

      const router = createTaskCompletionRouter(task, workflow, evaluator);
      const store = createMockStore();
      const result = await router(store);

      expect(result.enabledTasks).toContain('task1');
      expect(result.enabledTasks).toContain('task2');
    });
  });

  describe('createResourceAllocationValidator', () => {
    it('should allow allocation when capacity is available', async () => {
      const evaluator = createMockConditionEvaluator();
      evaluator.evaluate = vi.fn().mockResolvedValue(true);

      const resource = { resourceId: 'r1', capacity: 5 };
      const workflow = { id: 'wf1', name: 'test', tasks: [], controlFlow: [] };

      const validator = createResourceAllocationValidator(resource, workflow, evaluator);
      const store = createMockStore();
      const result = await validator(store, { taskId: 'task1' });

      expect(result.valid).toBe(true);
      expect(result.receipt.decision).toBe('allow');
    });

    it('should deny allocation when capacity is exceeded', async () => {
      const evaluator = createMockConditionEvaluator();
      evaluator.evaluate = vi.fn().mockResolvedValue(false);

      const resource = { resourceId: 'r1', capacity: 5 };
      const workflow = { id: 'wf1', name: 'test', tasks: [], controlFlow: [] };

      const validator = createResourceAllocationValidator(resource, workflow, evaluator);
      const store = createMockStore();
      const result = await validator(store, { taskId: 'task1' });

      expect(result.valid).toBe(false);
      expect(result.receipt.decision).toBe('deny');
      expect(result.receipt.justification.reason).toContain('at capacity');
    });

    it('should check eligibility constraints', async () => {
      const evaluator = createMockConditionEvaluator();
      let callCount = 0;
      evaluator.evaluate = vi.fn().mockImplementation(() => {
        callCount++;
        // First call (capacity) succeeds, second call (eligibility) fails
        return Promise.resolve(callCount === 1);
      });

      const resource = {
        resourceId: 'r1',
        capacity: 5,
        eligibility: 'ASK { ?user a :Reviewer }',
      };
      const workflow = { id: 'wf1', name: 'test', tasks: [], controlFlow: [] };

      const validator = createResourceAllocationValidator(resource, workflow, evaluator);
      const store = createMockStore();
      const result = await validator(store, { taskId: 'task1' });

      expect(result.valid).toBe(false);
      expect(result.receipt.justification.reason).toContain('eligibility');
    });
  });
});

/* ========================================================================= */
/* Cancellation Handler Tests                                                */
/* ========================================================================= */

describe('Cancellation Handlers', () => {
  describe('createCancellationHandler', () => {
    it('should return cancelled tasks from cancellation set', () => {
      const task = {
        id: 'approve',
        kind: 'AtomicTask',
        cancellationSet: ['reject', 'notify'],
      };
      const workflow = { id: 'wf1', name: 'test', tasks: [task], controlFlow: [] };

      const handler = createCancellationHandler(task, workflow);
      const result = handler(new Error('Task failed'));

      expect(result.cancelledTasks).toContain('reject');
      expect(result.cancelledTasks).toContain('notify');
      expect(result.receipt.hookType).toBe('cancellation');
    });

    it('should include tasks from cancellation regions', () => {
      const task = { id: 'task1', kind: 'AtomicTask' };
      const workflow = {
        id: 'wf1',
        name: 'test',
        tasks: [task],
        controlFlow: [],
        cancellationRegions: {
          region1: ['task1', 'task2', 'task3'],
        },
      };

      const handler = createCancellationHandler(task, workflow);
      const result = handler('Cancelled by user');

      expect(result.cancelledTasks).toContain('task2');
      expect(result.cancelledTasks).toContain('task3');
      expect(result.cancelledTasks).not.toContain('task1'); // Don't include self
    });

    it('should detect timeout errors in reason', () => {
      const task = { id: 'task1', kind: 'AtomicTask', timeout: 5000 };
      const workflow = {
        id: 'wf1',
        name: 'test',
        tasks: [task],
        controlFlow: [],
        defaultTimeout: 30000,
      };

      const handler = createCancellationHandler(task, workflow);
      const result = handler(new Error('Operation timeout after 5000ms'));

      expect(result.receipt.justification.reason).toContain('timed out');
    });
  });
});

/* ========================================================================= */
/* Policy Pack Builder Tests                                                 */
/* ========================================================================= */

describe('createYAWLPolicyPack', () => {
  it('should create a complete policy pack from workflow', () => {
    const policyPack = createYAWLPolicyPack(sampleWorkflow);

    expect(policyPack.manifest.meta.name).toBe('yawl:approval-workflow');
    expect(policyPack.manifest.meta.version).toBe('1.0.0');
    expect(policyPack.hooks.length).toBeGreaterThan(0);
  });

  it('should generate hooks for all tasks', () => {
    const policyPack = createYAWLPolicyPack(sampleWorkflow);
    const hookNames = policyPack.hooks.map(h => h.name);

    // Should have enablement hooks
    expect(hookNames).toContain('yawl:enable:start');
    expect(hookNames).toContain('yawl:enable:review');
    expect(hookNames).toContain('yawl:enable:approve');

    // Should have cancellation hooks
    expect(hookNames).toContain('yawl:cancel:review');
    expect(hookNames).toContain('yawl:timeout:review');
  });

  it('should generate completion hooks for tasks with outgoing edges', () => {
    const policyPack = createYAWLPolicyPack(sampleWorkflow);
    const hookNames = policyPack.hooks.map(h => h.name);

    expect(hookNames).toContain('yawl:complete:start');
    expect(hookNames).toContain('yawl:complete:review');
    // 'end' has no outgoing edges, so no completion hook
  });

  it('should generate allocation hooks for resources', () => {
    const policyPack = createYAWLPolicyPack(sampleWorkflow);
    const hookNames = policyPack.hooks.map(h => h.name);

    expect(hookNames).toContain('yawl:allocate:reviewer');
    expect(hookNames).toContain('yawl:allocate:approver');
  });

  it('should provide validators when conditionEvaluator is provided', () => {
    const evaluator = createMockConditionEvaluator();
    const policyPack = createYAWLPolicyPack(sampleWorkflow, { conditionEvaluator: evaluator });

    expect(policyPack.getValidator('review')).toBeDefined();
    expect(policyPack.getRouter('review')).toBeDefined();
    expect(policyPack.getAllocator('reviewer')).toBeDefined();
  });

  it('should provide statistics', () => {
    const policyPack = createYAWLPolicyPack(sampleWorkflow);
    const stats = policyPack.getStats();

    expect(stats.workflowName).toBe('approval-workflow');
    expect(stats.taskCount).toBe(6);
    expect(stats.controlFlowEdges).toBe(6);
    expect(stats.resourceCount).toBe(2);
    expect(stats.hookCount).toBeGreaterThan(0);
  });

  describe('Policy Pack API', () => {
    let policyPack;
    let evaluator;

    beforeEach(() => {
      evaluator = createMockConditionEvaluator();
      policyPack = createYAWLPolicyPack(sampleWorkflow, { conditionEvaluator: evaluator });
    });

    it('should validate enablement', async () => {
      evaluator.evaluate = vi.fn().mockResolvedValue(true);
      const store = createMockStore();

      const result = await policyPack.validateEnablement('review', store);
      expect(result.valid).toBe(true);
    });

    it('should route completion', async () => {
      evaluator.evaluate = vi.fn().mockResolvedValue(true);
      const store = createMockStore();

      const result = await policyPack.routeCompletion('review', store);
      expect(result.enabledTasks).toBeDefined();
      expect(result.receipt).toBeDefined();
    });

    it('should validate allocation', async () => {
      evaluator.evaluate = vi.fn().mockResolvedValue(true);
      const store = createMockStore();

      const result = await policyPack.validateAllocation('reviewer', store);
      expect(result.valid).toBe(true);
    });

    it('should handle cancellation', () => {
      const result = policyPack.handleCancellation('approve', new Error('Failed'));
      expect(result.cancelledTasks).toBeDefined();
      expect(result.receipt.hookType).toBe('cancellation');
    });

    it('should throw for unknown task validator', async () => {
      const store = createMockStore();
      await expect(policyPack.validateEnablement('nonexistent', store)).rejects.toThrow();
    });

    it('should return empty result for task with no outgoing edges', async () => {
      const store = createMockStore();
      const result = await policyPack.routeCompletion('end', store);
      expect(result.enabledTasks).toEqual([]);
    });
  });
});

/* ========================================================================= */
/* Integration Tests                                                         */
/* ========================================================================= */

describe('Integration: Complete Workflow Execution', () => {
  it('should simulate approval path through workflow', async () => {
    // Setup evaluator that simulates "approved" state
    const evaluator = createMockConditionEvaluator();
    evaluator.evaluate = vi.fn().mockImplementation(async condition => {
      const query = condition.query || '';
      if (query.includes('approved') && !query.includes('NOT EXISTS')) {
        return true;
      }
      return false;
    });

    const policyPack = createYAWLPolicyPack(sampleWorkflow, { conditionEvaluator: evaluator });
    const store = createMockStore();

    // 1. Enable start task
    const startResult = await policyPack.validateEnablement('start', store);
    expect(startResult.valid).toBe(true);

    // 2. Complete start -> should route to review
    const startRouteResult = await policyPack.routeCompletion('start', store);
    expect(startRouteResult.enabledTasks).toContain('review');

    // 3. Complete review with "approved" -> should route to approve
    const reviewRouteResult = await policyPack.routeCompletion('review', store);
    expect(reviewRouteResult.enabledTasks).toContain('approve');
    expect(reviewRouteResult.enabledTasks).not.toContain('reject');

    // 4. Complete approve -> should route to finalize
    evaluator.evaluate = vi.fn().mockResolvedValue(true);
    const approveRouteResult = await policyPack.routeCompletion('approve', store);
    expect(approveRouteResult.enabledTasks).toContain('finalize');
  });

  it('should simulate rejection path through workflow', async () => {
    // Setup evaluator that simulates "!approved" state
    const evaluator = createMockConditionEvaluator();
    evaluator.evaluate = vi.fn().mockImplementation(async condition => {
      const query = condition.query || '';
      // NOT EXISTS approved -> true (not approved)
      if (query.includes('NOT EXISTS')) {
        return true;
      }
      // approved -> false
      if (query.includes('approved')) {
        return false;
      }
      return true;
    });

    const policyPack = createYAWLPolicyPack(sampleWorkflow, { conditionEvaluator: evaluator });
    const store = createMockStore();

    // Complete review with "!approved" -> should route to reject
    const reviewRouteResult = await policyPack.routeCompletion('review', store);
    expect(reviewRouteResult.enabledTasks).toContain('reject');
    expect(reviewRouteResult.enabledTasks).not.toContain('approve');
  });

  it('should handle cancellation cascading', () => {
    const policyPack = createYAWLPolicyPack(sampleWorkflow);

    // Cancel approve -> should cancel reject (from cancellation set)
    const result = policyPack.handleCancellation('approve', new Error('Cancelled'));
    expect(result.cancelledTasks).toContain('reject');
    expect(result.receipt.decision).toBe('deny');
  });
});
