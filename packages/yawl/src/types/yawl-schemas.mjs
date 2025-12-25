/**
 * @file YAWL Zod Validation Schemas
 * @module @unrdf/yawl/schemas
 *
 * @description
 * Comprehensive Zod schemas for validating all YAWL workflow entities.
 * Includes validation for:
 * - Entity structure (Case, Task, WorkItem, ControlFlow, Resource, Receipt)
 * - State transitions (enable -> start -> complete)
 * - Resource eligibility and capacity
 * - Control flow integrity (no cycles except self-loops)
 *
 * @see ./yawl-types.mjs for JSDoc type definitions
 */

import { z } from 'zod';
import {
  CASE_STATUSES,
  WORK_ITEM_STATUSES,
  TASK_KINDS,
  SPLIT_TYPES,
  JOIN_TYPES,
  RESOURCE_TYPES,
  RECEIPT_DECISIONS,
  CASE_STATUS_TRANSITIONS,
  WORK_ITEM_STATUS_TRANSITIONS,
} from './yawl-types.mjs';

// ============================================================================
// Base Schemas (Enumerations)
// ============================================================================

/**
 * Case status enumeration schema
 *
 * Valid values: inactive, active, completed, suspended, cancelled, failed
 */
export const CaseStatusSchema = z.enum(
  /** @type {[string, ...string[]]} */ (CASE_STATUSES)
);

/**
 * Work item status enumeration schema
 *
 * Valid values: enabled, started, completed, suspended, failed, cancelled
 */
export const WorkItemStatusSchema = z.enum(
  /** @type {[string, ...string[]]} */ (WORK_ITEM_STATUSES)
);

/**
 * Task kind enumeration schema
 *
 * Valid values: atomic, composite, multiple, cancellation
 */
export const TaskKindSchema = z.enum(
  /** @type {[string, ...string[]]} */ (TASK_KINDS)
);

/**
 * Split type enumeration schema
 *
 * Valid values: AND, XOR, OR, none
 */
export const SplitTypeSchema = z.enum(
  /** @type {[string, ...string[]]} */ (SPLIT_TYPES)
);

/**
 * Join type enumeration schema
 *
 * Valid values: AND, XOR, OR, none
 */
export const JoinTypeSchema = z.enum(
  /** @type {[string, ...string[]]} */ (JOIN_TYPES)
);

/**
 * Resource type enumeration schema
 *
 * Valid values: human, automated, role, position, capability
 */
export const ResourceTypeSchema = z.enum(
  /** @type {[string, ...string[]]} */ (RESOURCE_TYPES)
);

/**
 * Receipt decision enumeration schema
 *
 * Valid values: enable, start, complete, suspend, resume, cancel, fail, skip, delegate, escalate
 */
export const ReceiptDecisionSchema = z.enum(
  /** @type {[string, ...string[]]} */ (RECEIPT_DECISIONS)
);

// ============================================================================
// Supporting Schemas
// ============================================================================

/**
 * Timer configuration schema for tasks
 */
export const TaskTimerSchema = z.object({
  type: z.enum(['duration', 'date', 'cycle']),
  value: z.string().min(1).max(100),
  action: z.enum(['enable', 'start', 'complete']).optional(),
});

/**
 * Multiple instance configuration schema
 */
export const MultipleInstanceConfigSchema = z.object({
  minimum: z.number().int().nonnegative().optional(),
  maximum: z.number().int().positive().optional(),
  threshold: z.number().int().positive().optional(),
  creationType: z.enum(['static', 'dynamic']).optional(),
  splitQuery: z.string().min(1).optional(),
  instanceSync: z.enum(['AND', 'XOR', 'OR']).optional(),
}).refine(
  data => {
    if (data.minimum !== undefined && data.maximum !== undefined) {
      return data.minimum <= data.maximum;
    }
    return true;
  },
  { message: 'minimum must be less than or equal to maximum' }
);

/**
 * Work item timer schema
 */
export const WorkItemTimerSchema = z.object({
  id: z.string().min(1).max(100),
  type: z.enum(['duration', 'date']),
  value: z.string().min(1).max(100),
  action: z.enum(['escalate', 'reassign', 'cancel']),
  expiresAt: z.coerce.date(),
});

/**
 * Receipt state snapshot schema
 */
export const ReceiptStateSchema = z.object({
  caseStatus: CaseStatusSchema.optional(),
  workItemStatus: WorkItemStatusSchema.optional(),
  enabledTasks: z.array(z.string().min(1)).optional(),
  activeWorkItems: z.array(z.string().uuid()).optional(),
  caseData: z.record(z.unknown()).optional(),
  conditionMarking: z.record(z.number().int().nonnegative()).optional(),
});

/**
 * Data variable definition schema
 */
export const DataVariableSchema = z.object({
  name: z.string().min(1).max(100).regex(
    /^[a-zA-Z_][a-zA-Z0-9_]*$/,
    'Variable name must be a valid identifier'
  ),
  type: z.enum(['string', 'number', 'boolean', 'object', 'array']),
  defaultValue: z.unknown().optional(),
  isInput: z.boolean().optional(),
  isOutput: z.boolean().optional(),
  documentation: z.string().max(500).optional(),
});

/**
 * Condition schema (Petri net place)
 */
export const ConditionSchema = z.object({
  id: z.string().min(1).max(100),
  name: z.string().min(1).max(200),
  type: z.enum(['input', 'output', 'internal', 'implicit']),
  tokenCount: z.number().int().nonnegative().optional(),
  isMarked: z.boolean().optional(),
  documentation: z.string().max(1000).optional(),
});

// ============================================================================
// Core Entity Schemas
// ============================================================================

/**
 * Case schema - Workflow instance container
 *
 * A Case represents a running instance of a workflow specification.
 * It contains the active work items and maintains case-level data.
 */
export const CaseSchema = z.object({
  /** Unique case identifier (UUID) */
  id: z.string().uuid({ message: 'Case ID must be a valid UUID' }),

  /** Reference to the workflow specification */
  specId: z.string().min(1).max(100),

  /** Current case status */
  status: CaseStatusSchema,

  /** Active work items in this case */
  workItems: z.array(z.lazy(() => WorkItemSchema)).default([]),

  /** Parent case ID for sub-nets */
  parentCaseId: z.string().uuid().optional(),

  /** Case-level variable bindings */
  caseData: z.record(z.unknown()).optional(),

  /** Case creation timestamp */
  createdAt: z.coerce.date(),

  /** Last modification timestamp */
  modifiedAt: z.coerce.date(),

  /** Resource ID that launched this case */
  initiator: z.string().min(1).max(100).optional(),

  /** Optimistic locking version */
  version: z.number().int().nonnegative().optional(),
});

/**
 * Task schema - Atomic unit of work
 *
 * A Task is a building block of YAWL nets. It represents a piece of work
 * with configurable split/join patterns and resource allocation.
 */
export const TaskSchema = z.object({
  /** Unique task identifier */
  id: z.string().min(1).max(100),

  /** Human-readable task name */
  name: z.string().min(1).max(200),

  /** Task type */
  kind: TaskKindSchema,

  /** Outgoing flow semantics */
  split: SplitTypeSchema.default('none'),

  /** Incoming flow semantics */
  join: JoinTypeSchema.default('none'),

  /** Condition IDs that feed into this task */
  inputConditions: z.array(z.string().min(1)).optional(),

  /** Condition IDs produced by this task */
  outputConditions: z.array(z.string().min(1)).optional(),

  /** Resource IDs eligible to execute this task */
  resources: z.array(z.string().min(1)).default([]),

  /** Sub-net specification ID (for composite tasks) */
  subNetId: z.string().min(1).max(100).optional(),

  /** Timer configuration for auto-enablement */
  timer: TaskTimerSchema.optional(),

  /** Multiple instance configuration */
  multipleInstance: MultipleInstanceConfigSchema.optional(),

  /** Task IDs cancelled when this completes */
  cancellationSet: z.array(z.string().min(1)).optional(),

  /** Data mappings from case to task */
  inputMappings: z.record(z.unknown()).optional(),

  /** Data mappings from task to case */
  outputMappings: z.record(z.unknown()).optional(),

  /** Task documentation */
  documentation: z.string().max(2000).optional(),

  /** Task priority (0-100) */
  priority: z.number().int().min(0).max(100).optional(),
}).refine(
  data => {
    // Composite tasks must have subNetId
    if (data.kind === 'composite' && !data.subNetId) {
      return false;
    }
    return true;
  },
  { message: 'Composite tasks must specify a subNetId' }
).refine(
  data => {
    // Multiple instance tasks must have multipleInstance config
    if (data.kind === 'multiple' && !data.multipleInstance) {
      return false;
    }
    return true;
  },
  { message: 'Multiple instance tasks must have multipleInstance configuration' }
);

/**
 * WorkItem schema - Task execution instance
 *
 * A WorkItem is created when a task becomes enabled. It tracks the
 * lifecycle of a specific task execution assigned to a resource.
 */
export const WorkItemSchema = z.object({
  /** Unique work item identifier */
  id: z.string().uuid({ message: 'WorkItem ID must be a valid UUID' }),

  /** Reference to the task specification */
  taskId: z.string().min(1).max(100),

  /** Reference to the containing case */
  caseId: z.string().uuid({ message: 'Case ID must be a valid UUID' }),

  /** Current work item status */
  status: WorkItemStatusSchema,

  /** Resource ID currently responsible */
  owner: z.string().min(1).max(100).optional(),

  /** Resource ID(s) work was offered to */
  offeredTo: z.string().min(1).max(100).optional(),

  /** Work item data payload */
  data: z.record(z.unknown()).optional(),

  /** Work item creation timestamp */
  createdAt: z.coerce.date(),

  /** When execution started */
  startedAt: z.coerce.date().optional(),

  /** When execution completed */
  completedAt: z.coerce.date().optional(),

  /** Completion result for split routing */
  result: z.string().min(1).max(100).optional(),

  /** Reason for failure */
  failureReason: z.string().max(1000).optional(),

  /** Instance index for MI tasks */
  instanceNumber: z.number().int().nonnegative().optional(),

  /** Parent work item for MI tasks */
  parentWorkItemId: z.string().uuid().optional(),

  /** Active timers for this work item */
  timers: z.array(WorkItemTimerSchema).optional(),
}).refine(
  data => {
    // Started items must have startedAt
    if (data.status === 'started' && !data.startedAt) {
      return false;
    }
    return true;
  },
  { message: 'Started work items must have startedAt timestamp' }
).refine(
  data => {
    // Completed items must have completedAt
    if (data.status === 'completed' && !data.completedAt) {
      return false;
    }
    return true;
  },
  { message: 'Completed work items must have completedAt timestamp' }
).refine(
  data => {
    // Failed items should have failureReason
    if (data.status === 'failed' && !data.failureReason) {
      return false;
    }
    return true;
  },
  { message: 'Failed work items must have failureReason' }
);

/**
 * ControlFlow schema - Routing between tasks
 *
 * Control flows connect tasks to conditions and conditions to tasks,
 * defining the execution order and routing logic.
 */
export const ControlFlowSchema = z.object({
  /** Unique control flow identifier */
  id: z.string().min(1).max(100),

  /** Source task or condition ID */
  source: z.string().min(1).max(100),

  /** Target task or condition ID */
  target: z.string().min(1).max(100),

  /** Flow direction type */
  flowType: z.enum(['task-to-condition', 'condition-to-task']).optional(),

  /** Boolean expression for conditional flows */
  predicate: z.string().max(1000).optional(),

  /** Evaluation order for XOR splits */
  order: z.number().int().nonnegative().optional(),

  /** True if this is the default flow */
  isDefault: z.boolean().optional(),

  /** Flow documentation */
  documentation: z.string().max(1000).optional(),
}).refine(
  data => {
    // Source and target must be different (no direct self-loops)
    // Note: Self-loops in YAWL go through conditions
    return data.source !== data.target;
  },
  { message: 'Control flow source and target must be different' }
);

/**
 * Resource schema - Participant/tool allocation
 *
 * Resources represent human participants, automated services, or
 * abstract roles that can execute work items.
 */
export const ResourceSchema = z.object({
  /** Unique resource identifier */
  id: z.string().min(1).max(100),

  /** Human-readable resource name */
  name: z.string().min(1).max(200),

  /** Resource classification */
  type: ResourceTypeSchema,

  /** Maximum concurrent work items */
  capacity: z.number().int().positive().default(1),

  /** Current assigned work items */
  currentLoad: z.number().int().nonnegative().default(0),

  /** Whether resource is available */
  isAvailable: z.boolean().default(true),

  /** Roles this resource can fill */
  roles: z.array(z.string().min(1).max(100)).optional(),

  /** Skills/capabilities */
  capabilities: z.array(z.string().min(1).max(100)).optional(),

  /** Supervisor resource ID */
  supervisor: z.string().min(1).max(100).optional(),

  /** Organizational position */
  position: z.string().min(1).max(100).optional(),

  /** Additional attributes */
  attributes: z.record(z.unknown()).optional(),

  /** Last activity timestamp */
  lastActive: z.coerce.date().optional(),

  /** Contact email */
  email: z.string().email().optional(),
}).refine(
  data => {
    // Current load cannot exceed capacity
    return data.currentLoad <= data.capacity;
  },
  { message: 'Resource currentLoad cannot exceed capacity' }
);

/**
 * Receipt schema - Cryptographic proof of decision
 *
 * Receipts provide an immutable audit trail of workflow decisions
 * with cryptographic verification for tamper-evidence.
 */
export const ReceiptSchema = z.object({
  /** Unique receipt identifier */
  id: z.string().uuid({ message: 'Receipt ID must be a valid UUID' }),

  /** The decision/action taken */
  decision: ReceiptDecisionSchema,

  /** Reason for the decision */
  justification: z.string().min(1).max(2000),

  /** State before transition */
  beforeState: ReceiptStateSchema,

  /** State after transition */
  afterState: ReceiptStateSchema,

  /** When the decision was made */
  timestamp: z.coerce.date(),

  /** SHA-256 hash of receipt contents */
  hash: z.string().length(64).regex(
    /^[a-f0-9]+$/,
    'Hash must be a valid SHA-256 hex string'
  ),

  /** Resource ID that made the decision */
  actor: z.string().min(1).max(100).optional(),

  /** Related case ID */
  caseId: z.string().uuid().optional(),

  /** Related work item ID */
  workItemId: z.string().uuid().optional(),

  /** Related task ID */
  taskId: z.string().min(1).max(100).optional(),

  /** Chain to previous receipt */
  previousReceiptId: z.string().uuid().optional(),

  /** Additional context */
  metadata: z.record(z.unknown()).optional(),
});

/**
 * YAWL Net Specification schema
 */
export const YawlNetSpecSchema = z.object({
  /** Unique specification identifier */
  id: z.string().min(1).max(100),

  /** Human-readable specification name */
  name: z.string().min(1).max(200),

  /** Semantic version string */
  version: z.string().regex(/^\d+\.\d+\.\d+$/).optional(),

  /** Specification description */
  description: z.string().max(2000).optional(),

  /** Tasks in this net */
  tasks: z.array(TaskSchema),

  /** Conditions in this net */
  conditions: z.array(ConditionSchema),

  /** Control flows connecting elements */
  controlFlows: z.array(ControlFlowSchema),

  /** ID of the input condition */
  inputConditionId: z.string().min(1).max(100),

  /** ID of the output condition */
  outputConditionId: z.string().min(1).max(100),

  /** Resources authorized for this net */
  resources: z.array(ResourceSchema).optional(),

  /** Net-level variables */
  dataVariables: z.record(DataVariableSchema).optional(),

  /** Specification author */
  author: z.string().min(1).max(100).optional(),

  /** Creation date */
  createdAt: z.coerce.date().optional(),

  /** Last modification date */
  modifiedAt: z.coerce.date().optional(),

  /** Classification tags */
  tags: z.array(z.string().min(1).max(50)).max(20).optional(),
});

// ============================================================================
// State Transition Validation
// ============================================================================

/**
 * Schema for validating case status transitions
 */
export const CaseTransitionSchema = z.object({
  from: CaseStatusSchema,
  to: CaseStatusSchema,
}).refine(
  data => {
    const allowed = CASE_STATUS_TRANSITIONS[data.from];
    return allowed && allowed.includes(data.to);
  },
  data => ({
    message: `Invalid case transition: ${data.from} -> ${data.to}. Allowed: ${CASE_STATUS_TRANSITIONS[data.from]?.join(', ') || 'none'}`,
  })
);

/**
 * Schema for validating work item status transitions
 */
export const WorkItemTransitionSchema = z.object({
  from: WorkItemStatusSchema,
  to: WorkItemStatusSchema,
}).refine(
  data => {
    const allowed = WORK_ITEM_STATUS_TRANSITIONS[data.from];
    return allowed && allowed.includes(data.to);
  },
  data => ({
    message: `Invalid work item transition: ${data.from} -> ${data.to}. Allowed: ${WORK_ITEM_STATUS_TRANSITIONS[data.from]?.join(', ') || 'none'}`,
  })
);

// ============================================================================
// Resource Eligibility Validation
// ============================================================================

/**
 * Schema for validating resource eligibility for a task
 */
export const ResourceEligibilitySchema = z.object({
  resource: ResourceSchema,
  task: TaskSchema,
}).refine(
  data => {
    // Resource must be available
    if (!data.resource.isAvailable) {
      return false;
    }
    return true;
  },
  { message: 'Resource is not available' }
).refine(
  data => {
    // Resource must have capacity
    if (data.resource.currentLoad >= data.resource.capacity) {
      return false;
    }
    return true;
  },
  { message: 'Resource has no remaining capacity' }
).refine(
  data => {
    // If task specifies resources, check membership
    if (data.task.resources.length > 0) {
      return data.task.resources.includes(data.resource.id);
    }
    return true;
  },
  { message: 'Resource is not in the task\'s authorized resource list' }
);

// ============================================================================
// Control Flow Validation (Cycle Detection)
// ============================================================================

/**
 * Validate that a set of control flows has no cycles (except self-loops through conditions)
 *
 * In YAWL, self-loops are allowed but must go through a condition.
 * Direct task-to-task cycles are not allowed.
 *
 * @param {Array<{source: string, target: string}>} flows - Control flows to validate
 * @returns {boolean} True if no invalid cycles
 */
export function validateNoCycles(flows) {
  // Build adjacency list
  /** @type {Map<string, string[]>} */
  const graph = new Map();

  for (const flow of flows) {
    if (!graph.has(flow.source)) {
      graph.set(flow.source, []);
    }
    graph.get(flow.source)?.push(flow.target);
  }

  // Track visited and recursion stack for cycle detection
  /** @type {Set<string>} */
  const visited = new Set();
  /** @type {Set<string>} */
  const recStack = new Set();

  /**
   * DFS helper for cycle detection
   * @param {string} node
   * @returns {boolean}
   */
  function hasCycle(node) {
    if (recStack.has(node)) {
      return true; // Back edge found
    }
    if (visited.has(node)) {
      return false; // Already processed
    }

    visited.add(node);
    recStack.add(node);

    const neighbors = graph.get(node) || [];
    for (const neighbor of neighbors) {
      if (hasCycle(neighbor)) {
        return true;
      }
    }

    recStack.delete(node);
    return false;
  }

  // Check all nodes
  for (const node of graph.keys()) {
    if (hasCycle(node)) {
      return false;
    }
  }

  return true;
}

/**
 * Schema for validating a complete workflow net for cycles
 */
export const NoCyclesSchema = z.array(ControlFlowSchema).refine(
  flows => validateNoCycles(flows),
  { message: 'Control flows contain an invalid cycle' }
);

// ============================================================================
// Validation Functions
// ============================================================================

/**
 * Validate a Case
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof CaseSchema>>}
 */
export function validateCase(data) {
  try {
    const validated = CaseSchema.parse(data);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a Task
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof TaskSchema>>}
 */
export function validateTask(data) {
  try {
    const validated = TaskSchema.parse(data);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a WorkItem
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof WorkItemSchema>>}
 */
export function validateWorkItem(data) {
  try {
    const validated = WorkItemSchema.parse(data);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a ControlFlow
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof ControlFlowSchema>>}
 */
export function validateControlFlow(data) {
  try {
    const validated = ControlFlowSchema.parse(data);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a Resource
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof ResourceSchema>>}
 */
export function validateResource(data) {
  try {
    const validated = ResourceSchema.parse(data);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a Receipt
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof ReceiptSchema>>}
 */
export function validateReceipt(data) {
  try {
    const validated = ReceiptSchema.parse(data);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a state transition for a Case
 * @param {string} from - Current status
 * @param {string} to - Target status
 * @returns {import('./yawl-types.mjs').ValidationResult<{from: string, to: string}>}
 */
export function validateCaseTransition(from, to) {
  try {
    const validated = CaseTransitionSchema.parse({ from, to });
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a state transition for a WorkItem
 * @param {string} from - Current status
 * @param {string} to - Target status
 * @returns {import('./yawl-types.mjs').ValidationResult<{from: string, to: string}>}
 */
export function validateWorkItemTransition(from, to) {
  try {
    const validated = WorkItemTransitionSchema.parse({ from, to });
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate resource eligibility for a task
 * @param {unknown} resource - Resource to check
 * @param {unknown} task - Task to check against
 * @returns {import('./yawl-types.mjs').ValidationResult<{resource: any, task: any}>}
 */
export function validateResourceEligibility(resource, task) {
  try {
    const validated = ResourceEligibilitySchema.parse({ resource, task });
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a YAWL Net Specification
 * @param {unknown} data - Data to validate
 * @returns {import('./yawl-types.mjs').ValidationResult<import('zod').infer<typeof YawlNetSpecSchema>>}
 */
export function validateYawlNetSpec(data) {
  try {
    const validated = YawlNetSpecSchema.parse(data);

    // Additional validation: check for cycles in control flows
    if (!validateNoCycles(validated.controlFlows)) {
      return {
        success: false,
        data: null,
        errors: [{
          path: 'controlFlows',
          message: 'Control flows contain an invalid cycle',
          code: 'custom',
        }],
      };
    }

    // Validate input/output conditions exist
    const conditionIds = new Set(validated.conditions.map(c => c.id));
    if (!conditionIds.has(validated.inputConditionId)) {
      return {
        success: false,
        data: null,
        errors: [{
          path: 'inputConditionId',
          message: `Input condition '${validated.inputConditionId}' not found in conditions`,
          code: 'custom',
        }],
      };
    }
    if (!conditionIds.has(validated.outputConditionId)) {
      return {
        success: false,
        data: null,
        errors: [{
          path: 'outputConditionId',
          message: `Output condition '${validated.outputConditionId}' not found in conditions`,
          code: 'custom',
        }],
      };
    }

    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.errors.map(err => ({
          path: err.path.join('.'),
          message: err.message,
          code: err.code,
          received: /** @type {any} */ (err).received,
          expected: /** @type {any} */ (err).expected,
        })),
      };
    }
    throw error;
  }
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create a new Case with defaults
 * @param {Partial<import('zod').infer<typeof CaseSchema>> & {id: string, specId: string}} input
 * @returns {import('zod').infer<typeof CaseSchema>}
 */
export function createCase(input) {
  const now = new Date();
  const caseData = {
    status: 'inactive',
    workItems: [],
    createdAt: now,
    modifiedAt: now,
    version: 0,
    ...input,
  };
  return CaseSchema.parse(caseData);
}

/**
 * Create a new WorkItem with defaults
 * @param {Partial<import('zod').infer<typeof WorkItemSchema>> & {id: string, taskId: string, caseId: string}} input
 * @returns {import('zod').infer<typeof WorkItemSchema>}
 */
export function createWorkItem(input) {
  const workItemData = {
    status: 'enabled',
    createdAt: new Date(),
    ...input,
  };
  return WorkItemSchema.parse(workItemData);
}

/**
 * Create a new Resource with defaults
 * @param {Partial<import('zod').infer<typeof ResourceSchema>> & {id: string, name: string, type: string}} input
 * @returns {import('zod').infer<typeof ResourceSchema>}
 */
export function createResource(input) {
  const resourceData = {
    capacity: 1,
    currentLoad: 0,
    isAvailable: true,
    ...input,
  };
  return ResourceSchema.parse(resourceData);
}

// ============================================================================
// Type Exports (for JSDoc consumers)
// ============================================================================

/**
 * @typedef {import('zod').infer<typeof CaseSchema>} CaseData
 * @typedef {import('zod').infer<typeof TaskSchema>} TaskData
 * @typedef {import('zod').infer<typeof WorkItemSchema>} WorkItemData
 * @typedef {import('zod').infer<typeof ControlFlowSchema>} ControlFlowData
 * @typedef {import('zod').infer<typeof ResourceSchema>} ResourceData
 * @typedef {import('zod').infer<typeof ReceiptSchema>} ReceiptData
 * @typedef {import('zod').infer<typeof YawlNetSpecSchema>} YawlNetSpecData
 */
