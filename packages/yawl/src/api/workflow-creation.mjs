/**
 * @file YAWL Workflow Creation - Workflow specification and creation
 * @module @unrdf/yawl/api/workflow-creation
 *
 * @description
 * Handles workflow creation, validation, and control flow graph construction.
 * Pure functions with no side effects except event append.
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { randomUUID } from 'crypto';

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
 */
export function generateId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  // Node.js ESM fallback
  try {
    return randomUUID();
  } catch {
    return `${Date.now()}-${Math.random().toString(36).slice(2, 11)}`;
  }
}

/**
 * Get current time in nanoseconds as BigInt
 * @returns {bigint} Nanosecond timestamp
 */
export function now() {
  if (typeof process !== 'undefined' && process.hrtime?.bigint) {
    return process.hrtime.bigint();
  }
  return BigInt(Math.floor(performance.now() * 1_000_000));
}

/**
 * Convert nanosecond BigInt to ISO string
 * @param {bigint} t_ns - Nanosecond timestamp
 * @returns {string} ISO 8601 string
 */
export function toISO(t_ns) {
  const ms = Number(t_ns / 1_000_000n);
  return new Date(ms).toISOString();
}

/**
 * Create cryptographic hash for receipt
 * @param {Object} payload - Data to hash
 * @returns {Promise<string>} BLAKE3 hash
 */
export async function createHash(payload) {
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
 */
export async function createReceipt(type, payload, justification) {
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
// Core Creation Functions
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

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Build control flow graph from control flow definitions
 * @param {Array<Object>} controlFlow - Control flow definitions
 * @returns {Object} Graph with incoming and outgoing edge maps
 */
export function buildControlFlowGraph(controlFlow) {
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
 */
export function findInitialTasks(tasks, controlFlowGraph) {
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
 */
export function createWorkflowRDF(spec, store) {
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
