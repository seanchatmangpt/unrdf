/**
 * @file YAWL (Yet Another Workflow Language) Core Type Definitions
 * @module @unrdf/yawl/types
 *
 * @description
 * This module defines the core type system for YAWL workflow execution.
 * YAWL is based on colored Petri nets and provides expressive workflow
 * patterns including advanced split/join semantics, cancellation regions,
 * and multiple-instance tasks.
 *
 * YAWL Semantics Overview:
 * - Case: A running instance of a workflow specification (process instance)
 * - Task: An atomic unit of work in the workflow (activity node)
 * - WorkItem: A specific execution instance of a task assigned to a resource
 * - ControlFlow: Routing logic between tasks (arcs with predicates)
 * - Resource: A participant or service capable of executing work items
 * - Receipt: Cryptographic proof of workflow state transitions
 *
 * @see https://yawlfoundation.github.io/
 */

// ============================================================================
// Status Enumerations
// ============================================================================

/**
 * Valid status values for a Case (workflow instance)
 *
 * Lifecycle: inactive -> active -> completed | suspended | cancelled | failed
 *
 * @typedef {'inactive' | 'active' | 'completed' | 'suspended' | 'cancelled' | 'failed'} CaseStatus
 */

/**
 * Valid status values for a WorkItem
 *
 * Lifecycle: enabled -> started -> completed | failed | cancelled
 * Alternative: enabled -> suspended -> resumed -> started -> ...
 *
 * @typedef {'enabled' | 'started' | 'completed' | 'suspended' | 'failed' | 'cancelled'} WorkItemStatus
 */

/**
 * Task kinds in YAWL
 *
 * - atomic: Simple indivisible task
 * - composite: Contains a sub-net (sub-workflow)
 * - multiple: Multiple instance task (parallel execution)
 * - cancellation: Task that triggers cancellation of other tasks
 *
 * @typedef {'atomic' | 'composite' | 'multiple' | 'cancellation'} TaskKind
 */

/**
 * Split semantics for outgoing flows from a task
 *
 * - AND: All outgoing flows fire (parallel split)
 * - XOR: Exactly one outgoing flow fires (exclusive choice)
 * - OR: One or more outgoing flows fire based on predicates (inclusive choice)
 * - none: Single outgoing flow (sequence)
 *
 * @typedef {'AND' | 'XOR' | 'OR' | 'none'} SplitType
 */

/**
 * Join semantics for incoming flows to a task
 *
 * - AND: Wait for all incoming flows (synchronization)
 * - XOR: Fire on first incoming flow (simple merge)
 * - OR: Wait for all active incoming flows (synchronizing merge)
 * - none: Single incoming flow (sequence)
 *
 * @typedef {'AND' | 'XOR' | 'OR' | 'none'} JoinType
 */

/**
 * Resource types in YAWL
 *
 * - human: Human participant requiring manual work item allocation
 * - automated: System/service resource for automatic task execution
 * - role: Abstract role that multiple resources can fill
 * - position: Organizational position (e.g., "Manager")
 * - capability: Skill-based allocation (e.g., "JavaDeveloper")
 *
 * @typedef {'human' | 'automated' | 'role' | 'position' | 'capability'} ResourceType
 */

/**
 * Receipt decision types
 *
 * - enable: Task was enabled for execution
 * - start: Work item execution began
 * - complete: Work item completed successfully
 * - suspend: Execution suspended
 * - resume: Execution resumed from suspension
 * - cancel: Execution cancelled
 * - fail: Execution failed with error
 * - skip: Task was skipped (empty completion)
 * - delegate: Work item delegated to another resource
 * - escalate: Work item escalated to supervisor
 *
 * @typedef {'enable' | 'start' | 'complete' | 'suspend' | 'resume' | 'cancel' | 'fail' | 'skip' | 'delegate' | 'escalate'} ReceiptDecision
 */

// ============================================================================
// Core Entity Types
// ============================================================================

/**
 * Case - A running instance of a workflow specification
 *
 * In YAWL, a Case represents a single execution of a workflow (called a "net"
 * in YAWL terminology). Each case maintains its own state, including the
 * current marking (tokens in conditions) and case-level data.
 *
 * Cases can be:
 * - Root cases: Top-level workflow instances
 * - Sub-cases: Created when a composite task executes its sub-net
 *
 * @typedef {Object} Case
 * @property {string} id - Unique case identifier (UUID)
 * @property {string} specId - Reference to the workflow specification ID
 * @property {CaseStatus} status - Current case status
 * @property {WorkItem[]} workItems - Active work items in this case
 * @property {string} [parentCaseId] - Parent case ID for sub-nets
 * @property {Record<string, unknown>} [caseData] - Case-level variable bindings
 * @property {Date} createdAt - Case creation timestamp
 * @property {Date} modifiedAt - Last modification timestamp
 * @property {string} [initiator] - Resource ID that launched this case
 * @property {number} [version] - Optimistic locking version
 */

/**
 * Task - An atomic unit of work in the workflow specification
 *
 * Tasks are the building blocks of YAWL nets. Each task represents
 * a piece of work that must be performed. Tasks are connected by
 * conditions (places in Petri net terms) through control flows.
 *
 * YAWL tasks support:
 * - Split/join patterns: AND, XOR, OR, or none
 * - Multiple instance patterns: Static and dynamic
 * - Cancellation regions: Tasks that can cancel other tasks
 * - Timer triggers: Automatic enablement based on time
 *
 * @typedef {Object} Task
 * @property {string} id - Unique task identifier
 * @property {string} name - Human-readable task name
 * @property {TaskKind} kind - Task type (atomic, composite, multiple, cancellation)
 * @property {SplitType} [split] - Outgoing flow semantics (default: 'none')
 * @property {JoinType} [join] - Incoming flow semantics (default: 'none')
 * @property {string[]} inputConditions - Condition IDs that feed into this task
 * @property {string[]} outputConditions - Condition IDs produced by this task
 * @property {string[]} resources - Resource IDs eligible to execute this task
 * @property {string} [subNetId] - Sub-net specification ID (for composite tasks)
 * @property {TaskTimer} [timer] - Timer configuration for auto-enablement
 * @property {MultipleInstanceConfig} [multipleInstance] - MI configuration
 * @property {string[]} [cancellationSet] - Task IDs cancelled when this completes
 * @property {Record<string, unknown>} [inputMappings] - Data mappings from case to task
 * @property {Record<string, unknown>} [outputMappings] - Data mappings from task to case
 * @property {string} [documentation] - Task documentation/description
 * @property {number} [priority] - Task priority (0-100, higher = more urgent)
 */

/**
 * Timer configuration for automatic task enablement
 *
 * @typedef {Object} TaskTimer
 * @property {'duration' | 'date' | 'cycle'} type - Timer type
 * @property {string} value - ISO 8601 duration, date, or cron expression
 * @property {'enable' | 'start' | 'complete'} [action] - Action on trigger
 */

/**
 * Multiple instance task configuration
 *
 * YAWL supports sophisticated multiple instance patterns where a task
 * can spawn multiple concurrent instances based on data.
 *
 * @typedef {Object} MultipleInstanceConfig
 * @property {number} [minimum] - Minimum instances required
 * @property {number} [maximum] - Maximum instances allowed
 * @property {number} [threshold] - Instances needed to proceed (for MI completion)
 * @property {'static' | 'dynamic'} [creationType] - How instances are created
 * @property {string} [splitQuery] - Expression to determine instance data
 * @property {'AND' | 'XOR' | 'OR'} [instanceSync] - How instances sync on completion
 */

/**
 * WorkItem - An execution instance of a task assigned to a resource
 *
 * WorkItems are created when a task becomes enabled (its input conditions
 * are satisfied). They represent the "work" that needs to be done and
 * track the lifecycle of that specific execution.
 *
 * WorkItem lifecycle:
 * 1. Enabled: Created when task input conditions are met
 * 2. Offered: Optionally shown to potential resources
 * 3. Allocated: Assigned to a specific resource
 * 4. Started: Execution has begun
 * 5. Completed/Failed/Cancelled: Terminal states
 *
 * @typedef {Object} WorkItem
 * @property {string} id - Unique work item identifier (UUID)
 * @property {string} taskId - Reference to the task specification
 * @property {string} caseId - Reference to the containing case
 * @property {WorkItemStatus} status - Current work item status
 * @property {string} [owner] - Resource ID currently responsible
 * @property {string} [offeredTo] - Resource ID(s) work was offered to
 * @property {Record<string, unknown>} [data] - Work item data payload
 * @property {Date} createdAt - Work item creation timestamp
 * @property {Date} [startedAt] - When execution started
 * @property {Date} [completedAt] - When execution completed
 * @property {string} [result] - Completion result (for XOR/OR splits)
 * @property {string} [failureReason] - Reason for failure (if failed)
 * @property {number} [instanceNumber] - Instance index for MI tasks
 * @property {string} [parentWorkItemId] - Parent work item for MI tasks
 * @property {WorkItemTimer[]} [timers] - Active timers for this work item
 */

/**
 * Timer attached to a work item
 *
 * @typedef {Object} WorkItemTimer
 * @property {string} id - Timer identifier
 * @property {'duration' | 'date'} type - Timer type
 * @property {string} value - ISO 8601 duration or date
 * @property {'escalate' | 'reassign' | 'cancel'} action - Action on expiry
 * @property {Date} expiresAt - When the timer fires
 */

/**
 * ControlFlow - Routing connection between tasks and conditions
 *
 * Control flows define the execution order and routing logic in YAWL.
 * They connect tasks to conditions and conditions to tasks, forming
 * the structure of the workflow net.
 *
 * For XOR/OR splits, predicates determine which flows are activated.
 * The order property determines evaluation order for XOR splits.
 *
 * @typedef {Object} ControlFlow
 * @property {string} id - Unique control flow identifier
 * @property {string} source - Source task or condition ID
 * @property {string} target - Target task or condition ID
 * @property {'task-to-condition' | 'condition-to-task'} [flowType] - Flow direction type
 * @property {string} [predicate] - Boolean expression for conditional flows
 * @property {number} [order] - Evaluation order for XOR splits (lower = first)
 * @property {boolean} [isDefault] - True if this is the default flow (XOR fallback)
 * @property {string} [documentation] - Flow documentation
 */

/**
 * Resource - A participant or service capable of executing work items
 *
 * Resources in YAWL can be:
 * - Human participants: Require manual allocation and execution
 * - Automated services: Execute tasks programmatically
 * - Abstract roles: Filled by multiple concrete resources
 *
 * Resource allocation patterns:
 * - Direct: Specific resource is assigned
 * - Role-based: Any resource with matching role
 * - Capability-based: Any resource with required skills
 * - Organizational: Based on org structure (position, supervisor)
 *
 * @typedef {Object} Resource
 * @property {string} id - Unique resource identifier
 * @property {string} name - Human-readable resource name
 * @property {ResourceType} type - Resource classification
 * @property {number} [capacity] - Maximum concurrent work items (default: 1)
 * @property {number} [currentLoad] - Current assigned work items
 * @property {boolean} [isAvailable] - Whether resource is available
 * @property {string[]} [roles] - Roles this resource can fill
 * @property {string[]} [capabilities] - Skills/capabilities this resource has
 * @property {string} [supervisor] - Supervisor resource ID (for escalation)
 * @property {string} [position] - Organizational position
 * @property {Record<string, unknown>} [attributes] - Additional resource attributes
 * @property {Date} [lastActive] - Last activity timestamp
 * @property {string} [email] - Contact email (for notifications)
 */

/**
 * Receipt - Cryptographic proof of a workflow state transition
 *
 * Receipts provide an immutable audit trail of all workflow decisions.
 * Each receipt captures the before/after state of a transition along
 * with cryptographic verification (hash) for tamper-evidence.
 *
 * Receipts enable:
 * - Audit trails: Complete history of workflow execution
 * - Compliance: Proof of who did what and when
 * - Recovery: Replay workflow state from receipts
 * - Verification: Cryptographic proof of state integrity
 *
 * @typedef {Object} Receipt
 * @property {string} id - Unique receipt identifier (UUID)
 * @property {ReceiptDecision} decision - The decision/action taken
 * @property {string} justification - Reason for the decision
 * @property {ReceiptState} beforeState - State before transition
 * @property {ReceiptState} afterState - State after transition
 * @property {Date} timestamp - When the decision was made
 * @property {string} hash - SHA-256 hash of receipt contents
 * @property {string} [actor] - Resource ID that made the decision
 * @property {string} [caseId] - Related case ID
 * @property {string} [workItemId] - Related work item ID
 * @property {string} [taskId] - Related task ID
 * @property {string} [previousReceiptId] - Chain to previous receipt
 * @property {Record<string, unknown>} [metadata] - Additional context
 */

/**
 * Captured state for receipt before/after snapshots
 *
 * @typedef {Object} ReceiptState
 * @property {CaseStatus} [caseStatus] - Case status at snapshot
 * @property {WorkItemStatus} [workItemStatus] - Work item status at snapshot
 * @property {string[]} [enabledTasks] - Currently enabled tasks
 * @property {string[]} [activeWorkItems] - Currently active work item IDs
 * @property {Record<string, unknown>} [caseData] - Case data at snapshot
 * @property {Record<string, number>} [conditionMarking] - Token counts per condition
 */

// ============================================================================
// Condition Types (Petri Net Places)
// ============================================================================

/**
 * Condition - A place in the Petri net representing workflow state
 *
 * Conditions hold tokens that control task enablement. Tasks fire
 * when their input conditions have sufficient tokens (based on join type).
 *
 * Special conditions:
 * - Input condition: Entry point of a net (receives initial token)
 * - Output condition: Exit point of a net (indicates completion)
 * - Implicit conditions: Between tasks with same split/join
 *
 * @typedef {Object} Condition
 * @property {string} id - Unique condition identifier
 * @property {string} name - Human-readable condition name
 * @property {'input' | 'output' | 'internal' | 'implicit'} type - Condition type
 * @property {number} [tokenCount] - Current token count (runtime)
 * @property {boolean} [isMarked] - Whether condition has tokens
 * @property {string} [documentation] - Condition documentation
 */

// ============================================================================
// Workflow Specification Types
// ============================================================================

/**
 * YAWL Net Specification - The workflow definition
 *
 * A specification defines the structure of a workflow: its tasks,
 * conditions, control flows, and data bindings. Cases are created
 * by instantiating a specification.
 *
 * @typedef {Object} YawlNetSpec
 * @property {string} id - Unique specification identifier
 * @property {string} name - Human-readable specification name
 * @property {string} [version] - Semantic version string
 * @property {string} [description] - Specification description
 * @property {Task[]} tasks - Tasks in this net
 * @property {Condition[]} conditions - Conditions in this net
 * @property {ControlFlow[]} controlFlows - Control flows connecting elements
 * @property {string} inputConditionId - ID of the input condition
 * @property {string} outputConditionId - ID of the output condition
 * @property {Resource[]} [resources] - Resources authorized for this net
 * @property {Record<string, DataVariable>} [dataVariables] - Net-level variables
 * @property {string} [author] - Specification author
 * @property {Date} [createdAt] - Specification creation date
 * @property {Date} [modifiedAt] - Last modification date
 * @property {string[]} [tags] - Classification tags
 */

/**
 * Data variable definition for case-level data
 *
 * @typedef {Object} DataVariable
 * @property {string} name - Variable name
 * @property {'string' | 'number' | 'boolean' | 'object' | 'array'} type - Variable type
 * @property {unknown} [defaultValue] - Default value if not set
 * @property {boolean} [isInput] - Whether this is an input parameter
 * @property {boolean} [isOutput] - Whether this is an output parameter
 * @property {string} [documentation] - Variable documentation
 */

// ============================================================================
// State Transition Types
// ============================================================================

/**
 * Valid state transitions for CaseStatus
 *
 * @type {Record<CaseStatus, CaseStatus[]>}
 */
export const CASE_STATUS_TRANSITIONS = Object.freeze({
  inactive: ['active'],
  active: ['completed', 'suspended', 'cancelled', 'failed'],
  suspended: ['active', 'cancelled'],
  completed: [], // terminal
  cancelled: [], // terminal
  failed: [], // terminal
});

/**
 * Valid state transitions for WorkItemStatus
 *
 * @type {Record<WorkItemStatus, WorkItemStatus[]>}
 */
export const WORK_ITEM_STATUS_TRANSITIONS = Object.freeze({
  enabled: ['started', 'suspended', 'cancelled'],
  started: ['completed', 'failed', 'suspended', 'cancelled'],
  suspended: ['enabled', 'started', 'cancelled'],
  completed: [], // terminal
  failed: [], // terminal
  cancelled: [], // terminal
});

// ============================================================================
// Utility Types
// ============================================================================

/**
 * Validation result type for schema validation
 *
 * @template T
 * @typedef {Object} ValidationResult
 * @property {boolean} success - Whether validation passed
 * @property {T | null} data - Validated data if successful
 * @property {ValidationError[]} errors - Validation errors if failed
 */

/**
 * Validation error detail
 *
 * @typedef {Object} ValidationError
 * @property {string} path - Path to the invalid field
 * @property {string} message - Error message
 * @property {string} code - Error code
 * @property {unknown} [received] - Actual value received
 * @property {string} [expected] - Expected value/type
 */

/**
 * Workflow event for pub/sub patterns
 *
 * @typedef {Object} WorkflowEvent
 * @property {string} id - Unique event identifier
 * @property {'case' | 'task' | 'workitem' | 'resource'} entityType - Entity type
 * @property {string} entityId - Entity identifier
 * @property {string} eventType - Specific event type
 * @property {Record<string, unknown>} [payload] - Event payload
 * @property {Date} timestamp - Event timestamp
 * @property {string} [correlationId] - Correlation ID for tracing
 */

// ============================================================================
// Type Guards (Runtime Checks)
// ============================================================================

/**
 * Check if a value is a valid CaseStatus
 * @param {unknown} value - Value to check
 * @returns {value is CaseStatus}
 */
export function isCaseStatus(value) {
  return ['inactive', 'active', 'completed', 'suspended', 'cancelled', 'failed'].includes(
    /** @type {string} */ (value)
  );
}

/**
 * Check if a value is a valid WorkItemStatus
 * @param {unknown} value - Value to check
 * @returns {value is WorkItemStatus}
 */
export function isWorkItemStatus(value) {
  return ['enabled', 'started', 'completed', 'suspended', 'failed', 'cancelled'].includes(
    /** @type {string} */ (value)
  );
}

/**
 * Check if a value is a valid TaskKind
 * @param {unknown} value - Value to check
 * @returns {value is TaskKind}
 */
export function isTaskKind(value) {
  return ['atomic', 'composite', 'multiple', 'cancellation'].includes(
    /** @type {string} */ (value)
  );
}

/**
 * Check if a value is a valid ResourceType
 * @param {unknown} value - Value to check
 * @returns {value is ResourceType}
 */
export function isResourceType(value) {
  return ['human', 'automated', 'role', 'position', 'capability'].includes(
    /** @type {string} */ (value)
  );
}

/**
 * Check if a status transition is valid for a Case
 * @param {CaseStatus} from - Current status
 * @param {CaseStatus} to - Target status
 * @returns {boolean}
 */
export function isValidCaseTransition(from, to) {
  const allowed = CASE_STATUS_TRANSITIONS[from];
  return allowed ? allowed.includes(to) : false;
}

/**
 * Check if a status transition is valid for a WorkItem
 * @param {WorkItemStatus} from - Current status
 * @param {WorkItemStatus} to - Target status
 * @returns {boolean}
 */
export function isValidWorkItemTransition(from, to) {
  const allowed = WORK_ITEM_STATUS_TRANSITIONS[from];
  return allowed ? allowed.includes(to) : false;
}

// ============================================================================
// Constants
// ============================================================================

/**
 * All valid case status values
 * @type {readonly CaseStatus[]}
 */
export const CASE_STATUSES = Object.freeze([
  'inactive',
  'active',
  'completed',
  'suspended',
  'cancelled',
  'failed',
]);

/**
 * All valid work item status values
 * @type {readonly WorkItemStatus[]}
 */
export const WORK_ITEM_STATUSES = Object.freeze([
  'enabled',
  'started',
  'completed',
  'suspended',
  'failed',
  'cancelled',
]);

/**
 * All valid task kinds
 * @type {readonly TaskKind[]}
 */
export const TASK_KINDS = Object.freeze(['atomic', 'composite', 'multiple', 'cancellation']);

/**
 * All valid split types
 * @type {readonly SplitType[]}
 */
export const SPLIT_TYPES = Object.freeze(['AND', 'XOR', 'OR', 'none']);

/**
 * All valid join types
 * @type {readonly JoinType[]}
 */
export const JOIN_TYPES = Object.freeze(['AND', 'XOR', 'OR', 'none']);

/**
 * All valid resource types
 * @type {readonly ResourceType[]}
 */
export const RESOURCE_TYPES = Object.freeze([
  'human',
  'automated',
  'role',
  'position',
  'capability',
]);

/**
 * All valid receipt decision types
 * @type {readonly ReceiptDecision[]}
 */
export const RECEIPT_DECISIONS = Object.freeze([
  'enable',
  'start',
  'complete',
  'suspend',
  'resume',
  'cancel',
  'fail',
  'skip',
  'delegate',
  'escalate',
]);
