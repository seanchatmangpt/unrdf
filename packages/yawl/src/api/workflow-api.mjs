/**
 * @file YAWL Workflow API - High-level workflow interface for @unrdf/yawl
 * @module @unrdf/yawl/api/workflow-api
 *
 * @description
 * Implements YAWL (Yet Another Workflow Language) workflow patterns with
 * KGC-4D event sourcing, hooks integration, and cryptographic receipts.
 *
 * All functions are pure (no side effects except event append).
 * Returns receipts with cryptographic justification for all state transitions.
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

// ============================================================================
// YAWL Namespace Constants
// ============================================================================

/**
 * YAWL ontology namespace URIs
 * @constant
 */
export const YAWL_NS = {
  BASE: 'http://yawl.io/',
  WORKFLOW: 'http://yawl.io/workflow/',
  TASK: 'http://yawl.io/task/',
  CASE: 'http://yawl.io/case/',
  WORK_ITEM: 'http://yawl.io/workitem/',
  RESOURCE: 'http://yawl.io/resource/',
  EVENT: 'http://yawl.io/event/',
};

/**
 * YAWL event types for KGC-4D logging
 * @constant
 */
export const YAWL_EVENT_TYPES = {
  WORKFLOW_CREATED: 'YAWL_WORKFLOW_CREATED',
  CASE_CREATED: 'YAWL_CASE_CREATED',
  TASK_ENABLED: 'YAWL_TASK_ENABLED',
  TASK_STARTED: 'YAWL_TASK_STARTED',
  TASK_COMPLETED: 'YAWL_TASK_COMPLETED',
  WORK_ITEM_CANCELLED: 'YAWL_WORK_ITEM_CANCELLED',
  CASE_REPLAYED: 'YAWL_CASE_REPLAYED',
};

/**
 * Work item status enumeration
 * @constant
 */
export const WORK_ITEM_STATUS = {
  PENDING: 'pending',
  ENABLED: 'enabled',
  ACTIVE: 'active',
  COMPLETED: 'completed',
  CANCELLED: 'cancelled',
  SUSPENDED: 'suspended',
};

/**
 * Control flow pattern types
 * @constant
 */
export const CONTROL_FLOW_PATTERNS = {
  SEQUENCE: 'sequence',
  AND_SPLIT: 'and-split',
  AND_JOIN: 'and-join',
  XOR_SPLIT: 'xor-split',
  XOR_JOIN: 'xor-join',
  OR_SPLIT: 'or-split',
  OR_JOIN: 'or-join',
  DEFERRED_CHOICE: 'deferred-choice',
  CANCELLATION_REGION: 'cancellation-region',
};

// ============================================================================
// Zod Schemas
// ============================================================================

/**
 * Schema for task definition
 */
export const TaskSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255),
  type: z.enum(['atomic', 'composite', 'multiple-instance']).default('atomic'),
  description: z.string().max(2000).optional(),
  inputVariables: z.array(z.string()).optional(),
  outputVariables: z.array(z.string()).optional(),
  preConditions: z.array(z.string()).optional(),
  postConditions: z.array(z.string()).optional(),
  timeout: z.number().positive().optional(),
  priority: z.number().int().min(0).max(100).default(50),
  resourcePattern: z.string().optional(),
  cancellationRegion: z.string().optional(),
});

/**
 * Schema for control flow definition
 */
export const ControlFlowSchema = z.object({
  id: z.string().min(1).max(255),
  type: z.enum([
    'sequence',
    'and-split',
    'and-join',
    'xor-split',
    'xor-join',
    'or-split',
    'or-join',
    'deferred-choice',
    'cancellation-region',
  ]),
  from: z.string().min(1),
  to: z.union([z.string(), z.array(z.string())]),
  condition: z.string().optional(),
  weight: z.number().min(0).max(1).optional(),
});

/**
 * Schema for resource definition
 */
export const ResourceSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255),
  type: z.enum(['role', 'participant', 'position', 'capability', 'org-group']),
  qualifications: z.array(z.string()).optional(),
  capabilities: z.array(z.string()).optional(),
  constraints: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for workflow specification
 */
export const WorkflowSpecSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255).optional(),
  version: z.string().regex(/^\d+\.\d+\.\d+$/).optional(),
  description: z.string().max(5000).optional(),
  tasks: z.array(TaskSchema).min(1),
  controlFlow: z.array(ControlFlowSchema).optional(),
  resources: z.array(ResourceSchema).optional(),
  inputVariables: z.array(z.string()).optional(),
  outputVariables: z.array(z.string()).optional(),
  cancellationRegions: z.record(z.string(), z.array(z.string())).optional(),
});

/**
 * Schema for workflow creation options
 */
export const WorkflowOptionsSchema = z.object({
  store: z.any().optional(),
  gitBackbone: z.any().optional(),
  hookRegistry: z.any().optional(),
  policyPacks: z.array(z.any()).optional(),
  validateSpec: z.boolean().default(true),
  createRDF: z.boolean().default(true),
}).optional();

/**
 * Schema for case creation options
 */
export const CaseOptionsSchema = z.object({
  caseId: z.string().optional(),
  initialVariables: z.record(z.string(), z.any()).optional(),
  priority: z.number().int().min(0).max(100).optional(),
  deadline: z.string().datetime().optional(),
  parent: z.string().optional(),
}).optional();

/**
 * Schema for work item
 */
export const WorkItemSchema = z.object({
  id: z.string().min(1),
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  status: z.enum(['pending', 'enabled', 'active', 'completed', 'cancelled', 'suspended']),
  assignedResource: z.string().optional(),
  startTime: z.string().optional(),
  endTime: z.string().optional(),
  variables: z.record(z.string(), z.any()).optional(),
  result: z.any().optional(),
});

/**
 * Schema for enable task options
 */
export const EnableTaskOptionsSchema = z.object({
  assignTo: z.string().optional(),
  priority: z.number().int().min(0).max(100).optional(),
  deadline: z.string().datetime().optional(),
  policyPack: z.any().optional(),
}).optional();

/**
 * Schema for receipt
 */
export const ReceiptSchema = z.object({
  id: z.string().min(1),
  type: z.string().min(1),
  timestamp: z.string(),
  t_ns: z.string(),
  hash: z.string().min(1),
  payload: z.record(z.string(), z.any()),
  justification: z.object({
    policyPackId: z.string().optional(),
    hookResults: z.array(z.any()).optional(),
    conditionsMet: z.array(z.string()).optional(),
    resourceEligibility: z.boolean().optional(),
  }).optional(),
});

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Generate a unique ID using crypto.randomUUID or fallback
 * @returns {string} Unique identifier
 * @private
 */
function generateId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  // Node.js fallback
  try {
    const cryptoModule = require('crypto');
    return cryptoModule.randomUUID();
  } catch {
    return `${Date.now()}-${Math.random().toString(36).slice(2, 11)}`;
  }
}

/**
 * Get current time in nanoseconds as BigInt
 * @returns {bigint} Nanosecond timestamp
 * @private
 */
function now() {
  if (typeof process !== 'undefined' && process.hrtime?.bigint) {
    return process.hrtime.bigint();
  }
  return BigInt(Math.floor(performance.now() * 1_000_000));
}

/**
 * Convert nanosecond BigInt to ISO string
 * @param {bigint} t_ns - Nanosecond timestamp
 * @returns {string} ISO 8601 string
 * @private
 */
function toISO(t_ns) {
  const ms = Number(t_ns / 1_000_000n);
  return new Date(ms).toISOString();
}

/**
 * Create cryptographic hash for receipt
 * @param {Object} payload - Data to hash
 * @returns {Promise<string>} BLAKE3 hash
 * @private
 */
async function createHash(payload) {
  const data = JSON.stringify(payload, (_, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  return blake3(data);
}

/**
 * Create a receipt with cryptographic justification
 * @param {string} type - Receipt type
 * @param {Object} payload - Receipt payload
 * @param {Object} [justification] - Optional justification
 * @returns {Promise<Object>} Receipt object
 * @private
 */
async function createReceipt(type, payload, justification) {
  const t_ns = now();
  const id = generateId();

  const receiptData = {
    id,
    type,
    timestamp: toISO(t_ns),
    t_ns: t_ns.toString(),
    payload,
    justification,
  };

  const hash = await createHash(receiptData);

  return ReceiptSchema.parse({
    ...receiptData,
    hash,
  });
}

// ============================================================================
// Core API Functions
// ============================================================================

/**
 * Create a new YAWL workflow from specification.
 *
 * Creates a workflow object with methods for case management, task enablement,
 * and control flow evaluation. Optionally creates RDF representation in store.
 *
 * @param {Object} spec - Workflow specification
 * @param {string} spec.id - Unique workflow identifier
 * @param {Array<Object>} spec.tasks - Task definitions
 * @param {Array<Object>} [spec.controlFlow] - Control flow definitions
 * @param {Array<Object>} [spec.resources] - Resource definitions
 * @param {Object} [options] - Creation options
 * @param {Object} [options.store] - KGC-4D store for RDF representation
 * @param {Object} [options.gitBackbone] - Git backbone for snapshots
 * @param {Object} [options.hookRegistry] - Hook registry for policy execution
 * @param {boolean} [options.validateSpec=true] - Validate spec structure
 * @param {boolean} [options.createRDF=true] - Create RDF representation
 * @returns {Promise<Object>} Workflow object with methods
 *
 * @example
 * const workflow = await createWorkflow({
 *   id: 'order-processing',
 *   tasks: [
 *     { id: 'receive-order', name: 'Receive Order' },
 *     { id: 'process-payment', name: 'Process Payment' },
 *     { id: 'ship-order', name: 'Ship Order' },
 *   ],
 *   controlFlow: [
 *     { id: 'cf1', type: 'sequence', from: 'receive-order', to: 'process-payment' },
 *     { id: 'cf2', type: 'sequence', from: 'process-payment', to: 'ship-order' },
 *   ],
 * });
 */
export async function createWorkflow(spec, options = {}) {
  // Validate options
  const validOptions = WorkflowOptionsSchema.parse(options);

  // Validate spec structure if requested
  let validSpec;
  if (validOptions?.validateSpec !== false) {
    validSpec = WorkflowSpecSchema.parse(spec);
  } else {
    validSpec = spec;
  }

  const t_ns = now();
  const workflowId = validSpec.id;

  // Build task index for O(1) lookup
  const taskIndex = new Map();
  for (const task of validSpec.tasks) {
    taskIndex.set(task.id, { ...task, _validated: true });
  }

  // Build control flow graph
  const controlFlowGraph = buildControlFlowGraph(validSpec.controlFlow || []);

  // Find initial tasks (tasks with no incoming edges)
  const initialTasks = findInitialTasks(validSpec.tasks, controlFlowGraph);

  // Build cancellation regions index
  const cancellationRegions = validSpec.cancellationRegions || {};

  // Create RDF representation if store provided and createRDF is true
  let rdfDeltas = [];
  if (validOptions?.store && validOptions?.createRDF !== false) {
    rdfDeltas = createWorkflowRDF(validSpec, validOptions.store);
  }

  // Append creation event to KGC-4D store if available
  let receipt = null;
  if (validOptions?.store?.appendEvent) {
    const { receipt: eventReceipt } = await validOptions.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.WORKFLOW_CREATED,
        payload: {
          workflowId,
          taskCount: validSpec.tasks.length,
          controlFlowCount: validSpec.controlFlow?.length || 0,
          resourceCount: validSpec.resources?.length || 0,
        },
      },
      rdfDeltas
    );
    receipt = await createReceipt(YAWL_EVENT_TYPES.WORKFLOW_CREATED, {
      workflowId,
      eventId: eventReceipt.id,
    });
  } else {
    receipt = await createReceipt(YAWL_EVENT_TYPES.WORKFLOW_CREATED, {
      workflowId,
    });
  }

  // Return workflow object with methods
  return {
    id: workflowId,
    name: validSpec.name || workflowId,
    version: validSpec.version || '1.0.0',
    spec: validSpec,
    taskIndex,
    controlFlowGraph,
    initialTasks,
    cancellationRegions,
    createdAt: toISO(t_ns),
    t_ns: t_ns.toString(),
    receipt,
    _store: validOptions?.store,
    _gitBackbone: validOptions?.gitBackbone,
    _hookRegistry: validOptions?.hookRegistry,
    _policyPacks: validOptions?.policyPacks || [],

    /**
     * Get task definition by ID
     * @param {string} taskId - Task identifier
     * @returns {Object|null} Task definition or null
     */
    getTask(taskId) {
      return taskIndex.get(taskId) || null;
    },

    /**
     * Get all tasks in the workflow
     * @returns {Array<Object>} Array of task definitions
     */
    getTasks() {
      return Array.from(taskIndex.values());
    },

    /**
     * Get downstream tasks from a given task
     * @param {string} taskId - Source task identifier
     * @returns {Array<Object>} Array of downstream task definitions
     */
    getDownstreamTasks(taskId) {
      const edges = controlFlowGraph.outgoing.get(taskId) || [];
      return edges
        .map((edge) => taskIndex.get(edge.to))
        .filter((task) => task !== undefined);
    },

    /**
     * Get upstream tasks to a given task
     * @param {string} taskId - Target task identifier
     * @returns {Array<Object>} Array of upstream task definitions
     */
    getUpstreamTasks(taskId) {
      const edges = controlFlowGraph.incoming.get(taskId) || [];
      return edges
        .map((edge) => taskIndex.get(edge.from))
        .filter((task) => task !== undefined);
    },

    /**
     * Check if task is initial (no incoming edges)
     * @param {string} taskId - Task identifier
     * @returns {boolean} True if initial task
     */
    isInitialTask(taskId) {
      return initialTasks.includes(taskId);
    },

    /**
     * Get tasks in cancellation region
     * @param {string} regionId - Cancellation region identifier
     * @returns {Array<string>} Array of task IDs in region
     */
    getCancellationRegion(regionId) {
      return cancellationRegions[regionId] || [];
    },
  };
}

/**
 * Create a new case (workflow instance) from a workflow.
 *
 * Instantiates workflow case with initial work items for enabled tasks.
 * Creates immutable event in KGC-4D and returns case object with status.
 *
 * @param {Object} workflow - Workflow object from createWorkflow
 * @param {Object} [options] - Case creation options
 * @param {string} [options.caseId] - Custom case ID (auto-generated if not provided)
 * @param {Object} [options.initialVariables] - Initial case variables
 * @param {number} [options.priority] - Case priority (0-100)
 * @param {string} [options.deadline] - ISO 8601 deadline
 * @param {string} [options.parent] - Parent case ID for sub-workflows
 * @returns {Promise<Object>} Case object with caseId, status, workItems
 *
 * @example
 * const caseObj = await createCase(workflow, {
 *   caseId: 'order-12345',
 *   initialVariables: { customerId: 'C001', amount: 150.00 },
 *   priority: 75,
 * });
 */
export async function createCase(workflow, options = {}) {
  // Validate options
  const validOptions = CaseOptionsSchema.parse(options);

  const t_ns = now();
  const caseId = validOptions?.caseId || generateId();

  // Initialize work items for all tasks
  const workItems = new Map();
  for (const task of workflow.getTasks()) {
    const workItemId = `${caseId}-${task.id}`;
    const isInitial = workflow.isInitialTask(task.id);

    const workItem = WorkItemSchema.parse({
      id: workItemId,
      caseId,
      taskId: task.id,
      status: isInitial ? WORK_ITEM_STATUS.ENABLED : WORK_ITEM_STATUS.PENDING,
      variables: validOptions?.initialVariables || {},
    });

    workItems.set(task.id, workItem);
  }

  // Get initially enabled work items
  const enabledWorkItems = Array.from(workItems.values()).filter(
    (wi) => wi.status === WORK_ITEM_STATUS.ENABLED
  );

  // Create RDF representation if store available
  let rdfDeltas = [];
  if (workflow._store) {
    rdfDeltas = createCaseRDF(caseId, workflow.id, workItems, workflow._store);
  }

  // Append creation event to KGC-4D store
  let receipt = null;
  if (workflow._store?.appendEvent) {
    const { receipt: eventReceipt } = await workflow._store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.CASE_CREATED,
        payload: {
          caseId,
          workflowId: workflow.id,
          workItemCount: workItems.size,
          enabledTaskIds: enabledWorkItems.map((wi) => wi.taskId),
          initialVariables: validOptions?.initialVariables || {},
        },
      },
      rdfDeltas
    );
    receipt = await createReceipt(YAWL_EVENT_TYPES.CASE_CREATED, {
      caseId,
      workflowId: workflow.id,
      eventId: eventReceipt.id,
    });
  } else {
    receipt = await createReceipt(YAWL_EVENT_TYPES.CASE_CREATED, {
      caseId,
      workflowId: workflow.id,
    });
  }

  // Return case object
  return {
    caseId,
    workflowId: workflow.id,
    status: 'active',
    workItems,
    variables: validOptions?.initialVariables || {},
    priority: validOptions?.priority || 50,
    deadline: validOptions?.deadline,
    parent: validOptions?.parent,
    createdAt: toISO(t_ns),
    t_ns: t_ns.toString(),
    receipt,
    _workflow: workflow,

    /**
     * Get work item by task ID
     * @param {string} taskId - Task identifier
     * @returns {Object|null} Work item or null
     */
    getWorkItem(taskId) {
      return workItems.get(taskId) || null;
    },

    /**
     * Get all work items
     * @returns {Array<Object>} Array of work items
     */
    getWorkItems() {
      return Array.from(workItems.values());
    },

    /**
     * Get work items by status
     * @param {string} status - Status to filter by
     * @returns {Array<Object>} Array of matching work items
     */
    getWorkItemsByStatus(status) {
      return Array.from(workItems.values()).filter((wi) => wi.status === status);
    },

    /**
     * Check if case is complete
     * @returns {boolean} True if all tasks completed or cancelled
     */
    isComplete() {
      return Array.from(workItems.values()).every(
        (wi) =>
          wi.status === WORK_ITEM_STATUS.COMPLETED ||
          wi.status === WORK_ITEM_STATUS.CANCELLED
      );
    },

    /**
     * Get enabled work items
     * @returns {Array<Object>} Array of enabled work items
     */
    getEnabledWorkItems() {
      return Array.from(workItems.values()).filter(
        (wi) => wi.status === WORK_ITEM_STATUS.ENABLED
      );
    },

    /**
     * Get active work items
     * @returns {Array<Object>} Array of active work items
     */
    getActiveWorkItems() {
      return Array.from(workItems.values()).filter(
        (wi) => wi.status === WORK_ITEM_STATUS.ACTIVE
      );
    },
  };
}

/**
 * Enable a work item, transitioning it to enabled state.
 *
 * Checks resource eligibility via policy packs before enabling.
 * Creates YAWL admission event with justification.
 *
 * @param {Object} workItem - Work item to enable
 * @param {Object} [options] - Enable options
 * @param {string} [options.assignTo] - Resource to assign to
 * @param {number} [options.priority] - Task priority override
 * @param {string} [options.deadline] - Task deadline
 * @param {Object} [options.policyPack] - Policy pack for eligibility check
 * @returns {Promise<Object>} Receipt with justification
 *
 * @throws {Error} If work item is not in pending state
 * @throws {Error} If resource eligibility check fails
 *
 * @example
 * const receipt = await enableTask(workItem, {
 *   assignTo: 'user-123',
 *   priority: 80,
 * });
 */
export async function enableTask(workItem, options = {}) {
  // Validate work item
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Validate options
  const validOptions = EnableTaskOptionsSchema.parse(options);

  // Check current status
  if (validWorkItem.status !== WORK_ITEM_STATUS.PENDING) {
    throw new Error(
      `Cannot enable work item ${validWorkItem.id}: status is ${validWorkItem.status}, expected ${WORK_ITEM_STATUS.PENDING}`
    );
  }

  const t_ns = now();

  // Check resource eligibility via policy pack if provided
  let eligibilityResult = { eligible: true, reasons: [] };
  let hookResults = [];

  if (validOptions?.policyPack && validOptions.policyPack.getHooks) {
    const hooks = validOptions.policyPack.getHooks();
    for (const hook of hooks) {
      if (hook.validate) {
        try {
          const result = hook.validate({
            workItem: validWorkItem,
            resource: validOptions.assignTo,
          });
          hookResults.push({
            hookName: hook.name || hook.meta?.name,
            passed: result,
          });
          if (!result) {
            eligibilityResult.eligible = false;
            eligibilityResult.reasons.push(
              `Hook ${hook.name || 'unknown'} rejected eligibility`
            );
          }
        } catch (error) {
          hookResults.push({
            hookName: hook.name || hook.meta?.name,
            passed: false,
            error: error.message,
          });
          eligibilityResult.eligible = false;
          eligibilityResult.reasons.push(error.message);
        }
      }
    }
  }

  // Fail if not eligible
  if (!eligibilityResult.eligible) {
    throw new Error(
      `Resource eligibility check failed: ${eligibilityResult.reasons.join(', ')}`
    );
  }

  // Update work item status (mutate original for in-place updates)
  workItem.status = WORK_ITEM_STATUS.ENABLED;
  workItem.assignedResource = validOptions?.assignTo;
  if (validOptions?.priority !== undefined) {
    workItem.priority = validOptions.priority;
  }

  // Create receipt with justification
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.TASK_ENABLED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      assignedResource: validOptions?.assignTo,
    },
    {
      policyPackId: validOptions?.policyPack?.manifest?.id,
      hookResults,
      resourceEligibility: eligibilityResult.eligible,
      conditionsMet: ['resource_eligibility'],
    }
  );

  return {
    workItem,
    receipt,
    eligibilityResult,
  };
}

/**
 * Start a work item, transitioning it from enabled to active state.
 *
 * Executes pre-execution hooks if hook registry is available.
 * Records start time and creates event receipt.
 *
 * @param {Object} workItem - Work item to start
 * @param {Object} [options] - Start options
 * @param {Object} [options.hookRegistry] - Hook registry for pre-execution hooks
 * @param {Object} [options.store] - KGC-4D store for event logging
 * @returns {Promise<Object>} Receipt with work item
 *
 * @throws {Error} If work item is not in enabled state
 *
 * @example
 * const receipt = await startTask(workItem);
 */
export async function startTask(workItem, options = {}) {
  // Validate work item (creates a validated copy for checks)
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Check current status
  if (validWorkItem.status !== WORK_ITEM_STATUS.ENABLED) {
    throw new Error(
      `Cannot start work item ${validWorkItem.id}: status is ${validWorkItem.status}, expected ${WORK_ITEM_STATUS.ENABLED}`
    );
  }

  const t_ns = now();
  const startTime = toISO(t_ns);

  // Execute pre-execution hooks if registry provided
  let hookResults = [];
  if (options.hookRegistry) {
    const preHooks = options.hookRegistry.getHooksByTrigger?.('pre-execution') || [];
    for (const hook of preHooks) {
      try {
        if (hook.execute) {
          const result = await hook.execute({
            workItem: validWorkItem,
            trigger: 'pre-execution',
          });
          hookResults.push({
            hookName: hook.name || hook.meta?.name,
            result,
          });
        }
      } catch (error) {
        hookResults.push({
          hookName: hook.name || hook.meta?.name,
          error: error.message,
        });
      }
    }
  }

  // Update work item status (mutate original for in-place updates)
  workItem.status = WORK_ITEM_STATUS.ACTIVE;
  workItem.startTime = startTime;

  // Append event to KGC-4D store if available
  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.TASK_STARTED,
        payload: {
          workItemId: validWorkItem.id,
          taskId: validWorkItem.taskId,
          caseId: validWorkItem.caseId,
          startTime,
        },
      },
      []
    );
    eventReceipt = receipt;
  }

  // Create receipt
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.TASK_STARTED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      startTime,
      eventId: eventReceipt?.id,
    },
    {
      hookResults,
    }
  );

  return {
    workItem,
    receipt,
  };
}

/**
 * Complete a work item, transitioning it from active to completed state.
 *
 * Evaluates control flow conditions to determine downstream tasks to enable.
 * Creates completion event with result and enables downstream tasks.
 *
 * @param {Object} workItem - Work item to complete
 * @param {unknown} result - Task result data
 * @param {Object} [options] - Complete options
 * @param {Object} [options.caseObj] - Case object for downstream task enablement
 * @param {Object} [options.workflow] - Workflow object for control flow evaluation
 * @param {Object} [options.store] - KGC-4D store for event logging
 * @returns {Promise<Object>} Receipt with downstream enabled tasks
 *
 * @throws {Error} If work item is not in active state
 *
 * @example
 * const receipt = await completeTask(workItem, { orderId: 'ORD-123' });
 */
export async function completeTask(workItem, result, options = {}) {
  // Validate work item (creates a validated copy for checks)
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Check current status
  if (validWorkItem.status !== WORK_ITEM_STATUS.ACTIVE) {
    throw new Error(
      `Cannot complete work item ${validWorkItem.id}: status is ${validWorkItem.status}, expected ${WORK_ITEM_STATUS.ACTIVE}`
    );
  }

  const t_ns = now();
  const endTime = toISO(t_ns);

  // Update work item status (mutate original for in-place updates)
  workItem.status = WORK_ITEM_STATUS.COMPLETED;
  workItem.endTime = endTime;
  workItem.result = result;

  // Evaluate control flow to find downstream tasks
  let enabledDownstreamTasks = [];
  if (options.caseObj && options.workflow) {
    enabledDownstreamTasks = await evaluateControlFlowAndEnable(
      workItem,
      result,
      options.caseObj,
      options.workflow
    );
  }

  // Append event to KGC-4D store if available
  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.TASK_COMPLETED,
        payload: {
          workItemId: workItem.id,
          taskId: workItem.taskId,
          caseId: workItem.caseId,
          endTime,
          result,
          enabledDownstreamTasks: enabledDownstreamTasks.map((t) => t.taskId),
        },
      },
      []
    );
    eventReceipt = receipt;
  }

  // Create receipt
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.TASK_COMPLETED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      endTime,
      enabledDownstreamTasks: enabledDownstreamTasks.map((t) => t.taskId),
      eventId: eventReceipt?.id,
    },
    {
      conditionsMet: enabledDownstreamTasks.map((t) => `flow_to_${t.taskId}`),
    }
  );

  return {
    workItem,
    enabledDownstreamTasks,
    receipt,
  };
}

/**
 * Cancel a work item, aborting it with a reason.
 *
 * Triggers cancellation region logic if the task is in a cancellation region.
 * Logs cancellation receipt and updates work item status.
 *
 * @param {Object} workItem - Work item to cancel
 * @param {string} reason - Reason for cancellation
 * @param {Object} [options] - Cancel options
 * @param {Object} [options.caseObj] - Case object for cancellation region handling
 * @param {Object} [options.workflow] - Workflow for cancellation region lookup
 * @param {Object} [options.store] - KGC-4D store for event logging
 * @returns {Promise<Object>} Receipt with cancellation details
 *
 * @throws {Error} If work item is already completed or cancelled
 *
 * @example
 * const receipt = await cancelWorkItem(workItem, 'Customer cancelled order');
 */
export async function cancelWorkItem(workItem, reason, options = {}) {
  // Validate work item
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Validate reason
  if (!reason || typeof reason !== 'string') {
    throw new TypeError('Cancel reason must be a non-empty string');
  }

  // Check current status - can't cancel completed or already cancelled
  if (
    validWorkItem.status === WORK_ITEM_STATUS.COMPLETED ||
    validWorkItem.status === WORK_ITEM_STATUS.CANCELLED
  ) {
    throw new Error(
      `Cannot cancel work item ${validWorkItem.id}: status is ${validWorkItem.status}`
    );
  }

  const t_ns = now();
  const cancelTime = toISO(t_ns);

  // Update work item status (mutate original for in-place updates)
  const previousStatus = workItem.status;
  workItem.status = WORK_ITEM_STATUS.CANCELLED;
  workItem.endTime = cancelTime;
  workItem.result = { cancelled: true, reason };

  // Handle cancellation region - cancel all tasks in the region
  let cancelledInRegion = [];
  if (options.workflow && options.caseObj) {
    // Find cancellation region for this task
    const task = options.workflow.getTask(workItem.taskId);
    if (task?.cancellationRegion) {
      const regionTasks = options.workflow.getCancellationRegion(
        task.cancellationRegion
      );
      for (const taskId of regionTasks) {
        if (taskId !== workItem.taskId) {
          const regionWorkItem = options.caseObj.getWorkItem(taskId);
          if (
            regionWorkItem &&
            regionWorkItem.status !== WORK_ITEM_STATUS.COMPLETED &&
            regionWorkItem.status !== WORK_ITEM_STATUS.CANCELLED
          ) {
            regionWorkItem.status = WORK_ITEM_STATUS.CANCELLED;
            regionWorkItem.endTime = cancelTime;
            regionWorkItem.result = {
              cancelled: true,
              reason: `Cancelled by region trigger: ${reason}`,
            };
            cancelledInRegion.push(taskId);
          }
        }
      }
    }
  }

  // Append event to KGC-4D store if available
  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED,
        payload: {
          workItemId: workItem.id,
          taskId: workItem.taskId,
          caseId: workItem.caseId,
          previousStatus,
          reason,
          cancelledInRegion,
          cancelTime,
        },
      },
      []
    );
    eventReceipt = receipt;
  }

  // Create receipt
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      previousStatus,
      reason,
      cancelledInRegion,
      cancelTime,
      eventId: eventReceipt?.id,
    }
  );

  return {
    workItem,
    cancelledInRegion,
    receipt,
  };
}

/**
 * Replay a case to reconstruct its state at a specific time.
 *
 * Uses KGC-4D reconstructState() to get case state at target time.
 * Returns historical case with all work item history.
 * Fully deterministic - same input always produces same output.
 *
 * @param {string} caseId - Case identifier to replay
 * @param {string|bigint} targetTime - Target time (ISO string or nanoseconds)
 * @param {Object} options - Replay options
 * @param {Object} options.store - KGC-4D store with event history
 * @param {Object} options.gitBackbone - Git backbone for snapshot access
 * @param {Function} [options.reconstructState] - Custom reconstructState function
 * @returns {Promise<Object>} Historical case with work item history
 *
 * @throws {Error} If case not found in event history
 * @throws {Error} If target time is before case creation
 *
 * @example
 * const historicalCase = await replayCase('order-12345', '2025-01-15T10:30:00Z', {
 *   store,
 *   gitBackbone,
 * });
 */
export async function replayCase(caseId, targetTime, options = {}) {
  // Validate inputs
  if (!caseId || typeof caseId !== 'string') {
    throw new TypeError('caseId must be a non-empty string');
  }

  if (!options.store) {
    throw new Error('replayCase requires a store option');
  }

  if (!options.gitBackbone) {
    throw new Error('replayCase requires a gitBackbone option');
  }

  // Convert target time to BigInt if ISO string
  let targetTimeNs;
  if (typeof targetTime === 'string') {
    // Parse ISO date to nanoseconds
    const ms = new Date(targetTime).getTime();
    if (isNaN(ms)) {
      throw new Error(`Invalid target time: ${targetTime}`);
    }
    targetTimeNs = BigInt(ms) * 1_000_000n;
  } else if (typeof targetTime === 'bigint') {
    targetTimeNs = targetTime;
  } else {
    throw new TypeError('targetTime must be an ISO string or BigInt nanoseconds');
  }

  const t_ns = now();

  // Use custom reconstructState or import from kgc-4d
  let reconstructState = options.reconstructState;
  if (!reconstructState) {
    // Dynamic import for reconstructState
    try {
      const kgc4d = await import('@unrdf/kgc-4d');
      reconstructState = kgc4d.reconstructState;
    } catch {
      throw new Error(
        'replayCase requires @unrdf/kgc-4d reconstructState or options.reconstructState'
      );
    }
  }

  // Reconstruct state at target time
  const historicalStore = await reconstructState(
    options.store,
    options.gitBackbone,
    targetTimeNs
  );

  // Query historical store for case events
  const caseEvents = await queryCaseEvents(caseId, historicalStore);

  if (caseEvents.length === 0) {
    throw new Error(`Case ${caseId} not found in event history at time ${targetTime}`);
  }

  // Reconstruct work item states from events
  const workItemHistory = reconstructWorkItemHistory(caseEvents);

  // Get case creation event for metadata
  const creationEvent = caseEvents.find(
    (e) => e.type === YAWL_EVENT_TYPES.CASE_CREATED
  );

  // Create receipt for replay
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.CASE_REPLAYED,
    {
      caseId,
      targetTime: targetTime.toString(),
      eventCount: caseEvents.length,
      reconstructedAt: toISO(t_ns),
    }
  );

  // Return historical case object
  return {
    caseId,
    workflowId: creationEvent?.payload?.workflowId,
    status: determineHistoricalCaseStatus(workItemHistory),
    workItemHistory,
    events: caseEvents,
    targetTime: targetTime.toString(),
    reconstructedAt: toISO(t_ns),
    receipt,
    _historicalStore: historicalStore,

    /**
     * Get work item state at target time
     * @param {string} taskId - Task identifier
     * @returns {Object|null} Work item state or null
     */
    getWorkItemAtTime(taskId) {
      return workItemHistory.get(taskId) || null;
    },

    /**
     * Get all work items at target time
     * @returns {Array<Object>} Array of work item states
     */
    getWorkItemsAtTime() {
      return Array.from(workItemHistory.values());
    },

    /**
     * Get events for a specific task
     * @param {string} taskId - Task identifier
     * @returns {Array<Object>} Events for the task
     */
    getTaskEvents(taskId) {
      return caseEvents.filter((e) => e.payload?.taskId === taskId);
    },
  };
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Build control flow graph from control flow definitions
 * @param {Array<Object>} controlFlow - Control flow definitions
 * @returns {Object} Graph with incoming and outgoing edge maps
 * @private
 */
function buildControlFlowGraph(controlFlow) {
  const incoming = new Map();
  const outgoing = new Map();

  for (const cf of controlFlow) {
    // Handle 'to' as string or array
    const targets = Array.isArray(cf.to) ? cf.to : [cf.to];

    // Add outgoing edges from source
    if (!outgoing.has(cf.from)) {
      outgoing.set(cf.from, []);
    }
    for (const target of targets) {
      outgoing.get(cf.from).push({
        id: cf.id,
        type: cf.type,
        from: cf.from,
        to: target,
        condition: cf.condition,
        weight: cf.weight,
      });

      // Add incoming edges to target
      if (!incoming.has(target)) {
        incoming.set(target, []);
      }
      incoming.get(target).push({
        id: cf.id,
        type: cf.type,
        from: cf.from,
        to: target,
        condition: cf.condition,
        weight: cf.weight,
      });
    }
  }

  return { incoming, outgoing };
}

/**
 * Find initial tasks (tasks with no incoming edges)
 * @param {Array<Object>} tasks - Task definitions
 * @param {Object} controlFlowGraph - Control flow graph
 * @returns {Array<string>} Array of initial task IDs
 * @private
 */
function findInitialTasks(tasks, controlFlowGraph) {
  const initialTasks = [];
  for (const task of tasks) {
    const incoming = controlFlowGraph.incoming.get(task.id) || [];
    if (incoming.length === 0) {
      initialTasks.push(task.id);
    }
  }

  // If no initial tasks found (circular workflow), use first task
  if (initialTasks.length === 0 && tasks.length > 0) {
    initialTasks.push(tasks[0].id);
  }

  return initialTasks;
}

/**
 * Create RDF representation of workflow
 * @param {Object} spec - Workflow specification
 * @param {Object} store - RDF store
 * @returns {Array<Object>} RDF deltas
 * @private
 */
function createWorkflowRDF(spec, store) {
  const deltas = [];
  const df = store._store?.dataFactory || {
    namedNode: (v) => ({ termType: 'NamedNode', value: v }),
    literal: (v) => ({ termType: 'Literal', value: v }),
  };

  const workflowUri = `${YAWL_NS.WORKFLOW}${spec.id}`;
  const workflowNode = df.namedNode(workflowUri);

  // Add workflow type
  deltas.push({
    type: 'add',
    subject: workflowNode,
    predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: df.namedNode(`${YAWL_NS.BASE}Workflow`),
  });

  // Add workflow name
  if (spec.name) {
    deltas.push({
      type: 'add',
      subject: workflowNode,
      predicate: df.namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      object: df.literal(spec.name),
    });
  }

  // Add tasks
  for (const task of spec.tasks) {
    const taskUri = `${workflowUri}/task/${task.id}`;
    const taskNode = df.namedNode(taskUri);

    deltas.push({
      type: 'add',
      subject: workflowNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}hasTask`),
      object: taskNode,
    });

    deltas.push({
      type: 'add',
      subject: taskNode,
      predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: df.namedNode(`${YAWL_NS.BASE}Task`),
    });

    deltas.push({
      type: 'add',
      subject: taskNode,
      predicate: df.namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      object: df.literal(task.name),
    });
  }

  return deltas;
}

/**
 * Create RDF representation of case
 * @param {string} caseId - Case identifier
 * @param {string} workflowId - Workflow identifier
 * @param {Map<string, Object>} workItems - Work items map
 * @param {Object} store - RDF store
 * @returns {Array<Object>} RDF deltas
 * @private
 */
function createCaseRDF(caseId, workflowId, workItems, store) {
  const deltas = [];
  const df = store._store?.dataFactory || {
    namedNode: (v) => ({ termType: 'NamedNode', value: v }),
    literal: (v) => ({ termType: 'Literal', value: v }),
  };

  const caseUri = `${YAWL_NS.CASE}${caseId}`;
  const caseNode = df.namedNode(caseUri);
  const workflowUri = `${YAWL_NS.WORKFLOW}${workflowId}`;

  // Add case type
  deltas.push({
    type: 'add',
    subject: caseNode,
    predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: df.namedNode(`${YAWL_NS.BASE}Case`),
  });

  // Link to workflow
  deltas.push({
    type: 'add',
    subject: caseNode,
    predicate: df.namedNode(`${YAWL_NS.BASE}instanceOf`),
    object: df.namedNode(workflowUri),
  });

  // Add work items
  for (const [taskId, workItem] of workItems) {
    const workItemUri = `${YAWL_NS.WORK_ITEM}${workItem.id}`;
    const workItemNode = df.namedNode(workItemUri);

    deltas.push({
      type: 'add',
      subject: caseNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}hasWorkItem`),
      object: workItemNode,
    });

    deltas.push({
      type: 'add',
      subject: workItemNode,
      predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: df.namedNode(`${YAWL_NS.BASE}WorkItem`),
    });

    deltas.push({
      type: 'add',
      subject: workItemNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}forTask`),
      object: df.namedNode(`${YAWL_NS.WORKFLOW}${workflowId}/task/${taskId}`),
    });

    deltas.push({
      type: 'add',
      subject: workItemNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}status`),
      object: df.literal(workItem.status),
    });
  }

  return deltas;
}

/**
 * Evaluate control flow conditions and enable downstream tasks
 * @param {Object} workItem - Completed work item
 * @param {unknown} result - Task result
 * @param {Object} caseObj - Case object
 * @param {Object} workflow - Workflow object
 * @returns {Promise<Array<Object>>} Enabled downstream work items
 * @private
 */
async function evaluateControlFlowAndEnable(workItem, result, caseObj, workflow) {
  const enabledWorkItems = [];
  const edges = workflow.controlFlowGraph.outgoing.get(workItem.taskId) || [];

  for (const edge of edges) {
    // Evaluate condition if present
    let conditionMet = true;
    if (edge.condition) {
      conditionMet = evaluateCondition(edge.condition, result, caseObj.variables);
    }

    if (conditionMet) {
      const downstreamWorkItem = caseObj.getWorkItem(edge.to);
      if (downstreamWorkItem) {
        // Check if all join conditions are met (for AND-join)
        if (edge.type === CONTROL_FLOW_PATTERNS.AND_JOIN) {
          const allPredecessorsComplete = checkAllPredecessorsComplete(
            edge.to,
            workflow,
            caseObj
          );
          if (!allPredecessorsComplete) {
            continue;
          }
        }

        // Enable the downstream work item
        if (downstreamWorkItem.status === WORK_ITEM_STATUS.PENDING) {
          downstreamWorkItem.status = WORK_ITEM_STATUS.ENABLED;
          enabledWorkItems.push(downstreamWorkItem);
        }
      }
    }
  }

  return enabledWorkItems;
}

/**
 * Evaluate a condition expression
 * @param {string} condition - Condition expression
 * @param {unknown} result - Task result
 * @param {Object} variables - Case variables
 * @returns {boolean} Condition result
 * @private
 */
function evaluateCondition(condition, result, variables) {
  // Simple condition evaluation - supports basic operators
  // In production, would use a proper expression evaluator
  try {
    // Create a safe evaluation context
    const context = {
      result,
      ...variables,
    };

    // Very basic condition parsing - supports: ==, !=, >, <, >=, <=, &&, ||
    // Replace variable references with context values
    let evalCondition = condition;
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`\\b${key}\\b`, 'g');
      if (typeof value === 'string') {
        evalCondition = evalCondition.replace(regex, `"${value}"`);
      } else if (value === null || value === undefined) {
        evalCondition = evalCondition.replace(regex, 'null');
      } else if (typeof value === 'object') {
        evalCondition = evalCondition.replace(regex, JSON.stringify(value));
      } else {
        evalCondition = evalCondition.replace(regex, String(value));
      }
    }

    // Simple evaluation for common patterns
    if (evalCondition === 'true') return true;
    if (evalCondition === 'false') return false;

    // For safety, only evaluate if it matches safe patterns
    const safePattern =
      /^[\s\d\w"'.\-+*/%<>=!&|()[\],{}:]+$/;
    if (!safePattern.test(evalCondition)) {
      console.warn(`Unsafe condition pattern: ${condition}`);
      return true; // Default to true for unsafe patterns
    }

    // Use Function constructor for sandboxed evaluation
    // eslint-disable-next-line no-new-func
    const evaluator = new Function('return ' + evalCondition);
    return Boolean(evaluator());
  } catch {
    // Default to true on evaluation error
    return true;
  }
}

/**
 * Check if all predecessors of a task are complete (for AND-join)
 * @param {string} taskId - Task to check
 * @param {Object} workflow - Workflow object
 * @param {Object} caseObj - Case object
 * @returns {boolean} True if all predecessors complete
 * @private
 */
function checkAllPredecessorsComplete(taskId, workflow, caseObj) {
  const incomingEdges = workflow.controlFlowGraph.incoming.get(taskId) || [];

  for (const edge of incomingEdges) {
    const predecessorWorkItem = caseObj.getWorkItem(edge.from);
    if (
      predecessorWorkItem &&
      predecessorWorkItem.status !== WORK_ITEM_STATUS.COMPLETED
    ) {
      return false;
    }
  }

  return true;
}

/**
 * Query case events from historical store
 * @param {string} caseId - Case identifier
 * @param {Object} store - Historical store
 * @returns {Promise<Array<Object>>} Case events
 * @private
 */
async function queryCaseEvents(caseId, store) {
  const events = [];

  // Query for events with this caseId in payload
  // Using SPARQL if available, otherwise pattern matching
  if (store.query) {
    try {
      const sparql = `
        PREFIX kgc: <http://kgc.io/>
        SELECT ?event ?type ?payload ?t_ns WHERE {
          ?event kgc:type ?type ;
                 kgc:payload ?payload ;
                 kgc:t_ns ?t_ns .
          FILTER(CONTAINS(?payload, "${caseId}"))
        }
        ORDER BY ?t_ns
      `;
      const results = await store.query(sparql);

      for (const row of results) {
        try {
          const payload = JSON.parse(row.payload?.value || '{}');
          if (payload.caseId === caseId) {
            events.push({
              id: row.event?.value,
              type: row.type?.value,
              payload,
              t_ns: row.t_ns?.value,
            });
          }
        } catch {
          // Skip malformed events
        }
      }
    } catch {
      // Fallback to pattern matching if SPARQL fails
    }
  }

  return events;
}

/**
 * Reconstruct work item history from events
 * @param {Array<Object>} events - Case events
 * @returns {Map<string, Object>} Work item history
 * @private
 */
function reconstructWorkItemHistory(events) {
  const workItemHistory = new Map();

  for (const event of events) {
    const taskId = event.payload?.taskId;
    if (!taskId) continue;

    // Get or create work item state
    let workItem = workItemHistory.get(taskId);
    if (!workItem) {
      workItem = {
        taskId,
        caseId: event.payload.caseId,
        events: [],
        status: WORK_ITEM_STATUS.PENDING,
      };
      workItemHistory.set(taskId, workItem);
    }

    // Add event to history
    workItem.events.push(event);

    // Update status based on event type
    switch (event.type) {
      case YAWL_EVENT_TYPES.TASK_ENABLED:
        workItem.status = WORK_ITEM_STATUS.ENABLED;
        break;
      case YAWL_EVENT_TYPES.TASK_STARTED:
        workItem.status = WORK_ITEM_STATUS.ACTIVE;
        workItem.startTime = event.payload.startTime;
        break;
      case YAWL_EVENT_TYPES.TASK_COMPLETED:
        workItem.status = WORK_ITEM_STATUS.COMPLETED;
        workItem.endTime = event.payload.endTime;
        workItem.result = event.payload.result;
        break;
      case YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED:
        workItem.status = WORK_ITEM_STATUS.CANCELLED;
        workItem.endTime = event.payload.cancelTime;
        workItem.cancelReason = event.payload.reason;
        break;
    }
  }

  return workItemHistory;
}

/**
 * Determine historical case status from work item history
 * @param {Map<string, Object>} workItemHistory - Work item history
 * @returns {string} Case status
 * @private
 */
function determineHistoricalCaseStatus(workItemHistory) {
  const workItems = Array.from(workItemHistory.values());

  if (workItems.length === 0) {
    return 'unknown';
  }

  const allComplete = workItems.every(
    (wi) =>
      wi.status === WORK_ITEM_STATUS.COMPLETED ||
      wi.status === WORK_ITEM_STATUS.CANCELLED
  );

  if (allComplete) {
    const anyCompleted = workItems.some(
      (wi) => wi.status === WORK_ITEM_STATUS.COMPLETED
    );
    return anyCompleted ? 'completed' : 'cancelled';
  }

  const anyActive = workItems.some(
    (wi) => wi.status === WORK_ITEM_STATUS.ACTIVE
  );

  return anyActive ? 'active' : 'pending';
}

// ============================================================================
// Module Exports
// ============================================================================

export default {
  // Core API
  createWorkflow,
  createCase,
  enableTask,
  startTask,
  completeTask,
  cancelWorkItem,
  replayCase,

  // Constants
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,

  // Schemas
  TaskSchema,
  ControlFlowSchema,
  ResourceSchema,
  WorkflowSpecSchema,
  WorkflowOptionsSchema,
  CaseOptionsSchema,
  WorkItemSchema,
  EnableTaskOptionsSchema,
  ReceiptSchema,
};
