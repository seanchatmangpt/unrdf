/**
 * @unrdf/yawl - YAWL Workflow API Type Definitions
 *
 * High-level workflow interface implementing YAWL patterns with KGC-4D event sourcing,
 * hooks integration, and cryptographic receipts.
 *
 * @packageDocumentation
 */

import type { z } from 'zod';

// ============================================================================
// Core Types
// ============================================================================

/**
 * Workflow task definition
 */
export interface Task {
  /** Unique task identifier */
  id: string;
  /** Task kind (atomic, composite, or multiple) */
  kind: 'atomic' | 'composite' | 'multiple';
  /** Human-readable task name */
  name?: string;
  /** Split behavior for control flow */
  split?: 'and' | 'or' | 'xor';
  /** Join behavior for control flow */
  join?: 'and' | 'or' | 'xor';
  /** Input conditions (task dependencies) */
  inputConditions?: string[];
  /** Output conditions (tasks that follow) */
  outputConditions?: string[];
  /** Decomposition to nested workflow */
  decomposesTo?: string;
  /** Multiple instance configuration */
  multipleInstance?: {
    minimum: number;
    maximum: number;
    threshold: number;
    creationType: 'static' | 'dynamic';
  };
  /** Timer configuration */
  timer?: {
    expiresAt: string;
    onExpiry: 'cancel' | 'complete' | 'notify';
  };
  /** Resource requirements */
  resources?: {
    role?: string[];
    capability?: string[];
  };
}

/**
 * Control flow edge between tasks
 */
export interface ControlFlow {
  /** Source task ID */
  from: string;
  /** Target task ID */
  to: string;
  /** Flow condition (SPARQL predicate) */
  condition?: string;
  /** Flow priority for OR-split */
  priority?: number;
  /** Whether this is the default flow */
  isDefault?: boolean;
}

/**
 * Resource allocation specification
 */
export interface Resource {
  /** Resource identifier */
  id: string;
  /** Resource type */
  type: 'participant' | 'role' | 'tool';
  /** Display name */
  name?: string;
  /** Capabilities */
  capabilities?: string[];
  /** Availability constraints */
  availability?: {
    from: string;
    to: string;
  };
}

/**
 * Workflow specification
 */
export interface WorkflowSpec {
  /** Unique workflow identifier */
  id: string;
  /** Human-readable workflow name */
  name: string;
  /** Task definitions */
  tasks: Task[];
  /** Control flow edges (optional - auto-generated from task conditions if omitted) */
  flow?: ControlFlow[];
  /** Resource pool */
  resources?: Resource[];
  /** Cancellation regions for exception handling */
  cancellationRegions?: Array<{
    id: string;
    tasks: string[];
  }>;
  /** Workflow-level data schema */
  dataSchema?: Record<string, unknown>;
}

/**
 * Workflow options for creation
 */
export interface WorkflowOptions {
  /** RDF graph store */
  store: any;
  /** Enable hooks integration */
  enableHooks?: boolean;
  /** Hook policy pack */
  policyPack?: any;
  /** Enable cryptographic receipts */
  enableReceipts?: boolean;
}

/**
 * Case creation options
 */
export interface CaseOptions {
  /** Initial case data */
  initialData?: Record<string, unknown>;
  /** Case metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Work item representing task instance
 */
export interface WorkItem {
  /** Work item ID (format: caseId-taskId-instanceNum) */
  id: string;
  /** Case ID this work item belongs to */
  caseId: string;
  /** Task ID */
  taskId: string;
  /** Work item status */
  status: 'enabled' | 'fired' | 'executing' | 'completed' | 'cancelled' | 'failed';
  /** Creation timestamp (ISO 8601) */
  createdAt: string;
  /** Start timestamp (ISO 8601) */
  startedAt?: string;
  /** Completion timestamp (ISO 8601) */
  completedAt?: string;
  /** Work item data */
  data?: Record<string, unknown>;
  /** Allocated resources */
  allocatedResources?: string[];
}

/**
 * Task enablement options
 */
export interface EnableTaskOptions {
  /** Work item data */
  data?: Record<string, unknown>;
  /** Resource allocation */
  allocatedResources?: string[];
}

/**
 * Cryptographic receipt for workflow operation
 */
export interface Receipt {
  /** Receipt ID */
  id: string;
  /** Operation type */
  operation: string;
  /** Event ID */
  eventId: string;
  /** Timestamp (ISO 8601) */
  timestamp: string;
  /** Cryptographic hash of operation */
  hash: string;
  /** Previous receipt hash (for chain) */
  previousHash?: string;
  /** Receipt decision (approved/rejected) */
  decision: 'approved' | 'rejected';
  /** Justification message */
  justification: string;
  /** Operation metadata */
  metadata?: Record<string, unknown>;
}

// ============================================================================
// Constants
// ============================================================================

/**
 * YAWL namespace URI
 */
export const YAWL_NS: string;

/**
 * YAWL event types for KGC-4D event sourcing
 */
export const YAWL_EVENT_TYPES: {
  WORKFLOW_CREATED: string;
  CASE_CREATED: string;
  TASK_ENABLED: string;
  TASK_STARTED: string;
  TASK_COMPLETED: string;
  TASK_CANCELLED: string;
  CONTROL_FLOW_EVALUATED: string;
};

/**
 * Work item status values
 */
export const WORK_ITEM_STATUS: {
  ENABLED: 'enabled';
  FIRED: 'fired';
  EXECUTING: 'executing';
  COMPLETED: 'completed';
  CANCELLED: 'cancelled';
  FAILED: 'failed';
};

/**
 * Van der Aalst's workflow control flow patterns
 */
export const CONTROL_FLOW_PATTERNS: {
  SEQUENCE: 'sequence';
  PARALLEL_SPLIT: 'parallel_split';
  SYNCHRONIZATION: 'synchronization';
  EXCLUSIVE_CHOICE: 'exclusive_choice';
  SIMPLE_MERGE: 'simple_merge';
  MULTI_CHOICE: 'multi_choice';
  STRUCTURED_SYNC_MERGE: 'structured_sync_merge';
  DEFERRED_CHOICE: 'deferred_choice';
  ARBITRARY_CYCLE: 'arbitrary_cycle';
};

// ============================================================================
// Zod Schemas (for runtime validation)
// ============================================================================

export const TaskSchema: z.ZodType<Task>;
export const ControlFlowSchema: z.ZodType<ControlFlow>;
export const ResourceSchema: z.ZodType<Resource>;
export const WorkflowSpecSchema: z.ZodType<WorkflowSpec>;
export const WorkflowOptionsSchema: z.ZodType<WorkflowOptions>;
export const CaseOptionsSchema: z.ZodType<CaseOptions>;
export const WorkItemSchema: z.ZodType<WorkItem>;
export const EnableTaskOptionsSchema: z.ZodType<EnableTaskOptions>;
export const ReceiptSchema: z.ZodType<Receipt>;

// ============================================================================
// Core API Functions
// ============================================================================

/**
 * Create a new YAWL workflow specification
 *
 * This creates the workflow definition in the RDF store and returns a receipt
 * with cryptographic proof. The workflow can then be instantiated as cases.
 *
 * @param spec - Workflow specification with tasks and control flow
 * @param options - Workflow options including RDF store
 * @returns Promise resolving to workflow creation receipt
 *
 * @example
 * ```javascript
 * import { createWorkflow, createStore } from '@unrdf/yawl';
 *
 * const store = createStore();
 * const receipt = await createWorkflow({
 *   id: 'purchase-order',
 *   name: 'Purchase Order Workflow',
 *   tasks: [
 *     { id: 'submit', kind: 'atomic', name: 'Submit Order' },
 *     { id: 'approve', kind: 'atomic', name: 'Approve Order', split: 'xor' },
 *     { id: 'fulfill', kind: 'atomic', name: 'Fulfill Order' },
 *     { id: 'reject', kind: 'atomic', name: 'Reject Order' }
 *   ],
 *   flow: [
 *     { from: 'submit', to: 'approve' },
 *     { from: 'approve', to: 'fulfill', condition: 'approved = true' },
 *     { from: 'approve', to: 'reject', condition: 'approved = false', isDefault: true }
 *   ]
 * }, { store });
 *
 * console.log('Workflow created:', receipt.id);
 * ```
 *
 * @throws {ValidationError} If workflow specification is invalid
 * @throws {StoreError} If RDF store operation fails
 */
export function createWorkflow(
  spec: WorkflowSpec,
  options: WorkflowOptions
): Promise<Receipt>;

/**
 * Create a new workflow case (instance)
 *
 * Instantiates a workflow specification as a runnable case with initial data.
 * Returns a receipt containing the case ID for tracking execution.
 *
 * @param workflowId - Workflow specification ID
 * @param store - RDF graph store
 * @param options - Case creation options with initial data
 * @returns Promise resolving to case creation receipt with case ID
 *
 * @example
 * ```javascript
 * const caseReceipt = await createCase(
 *   'purchase-order',
 *   store,
 *   {
 *     initialData: {
 *       orderId: 'PO-12345',
 *       amount: 1500.00,
 *       requestedBy: 'user@example.com'
 *     }
 *   }
 * );
 *
 * const caseId = caseReceipt.metadata.caseId;
 * console.log('Case started:', caseId);
 * ```
 *
 * @throws {ValidationError} If workflow ID or options are invalid
 * @throws {NotFoundError} If workflow specification doesn't exist
 */
export function createCase(
  workflowId: string,
  store: any,
  options?: CaseOptions
): Promise<Receipt>;

/**
 * Enable a task in a workflow case
 *
 * Marks a task as enabled (ready to execute) based on control flow evaluation.
 * Typically called automatically by the engine, but can be called manually for
 * deferred choice patterns or external task triggering.
 *
 * @param caseId - Workflow case ID
 * @param taskId - Task ID to enable
 * @param store - RDF graph store
 * @param options - Task enablement options with data
 * @returns Promise resolving to task enablement receipt
 *
 * @example
 * ```javascript
 * const enableReceipt = await enableTask(
 *   caseId,
 *   'approve',
 *   store,
 *   {
 *     data: { submittedAt: new Date().toISOString() },
 *     allocatedResources: ['manager-role']
 *   }
 * );
 * ```
 *
 * @throws {ValidationError} If case or task ID is invalid
 * @throws {StateError} If task cannot be enabled (e.g., dependencies not met)
 */
export function enableTask(
  caseId: string,
  taskId: string,
  store: any,
  options?: EnableTaskOptions
): Promise<Receipt>;

/**
 * Start execution of an enabled work item
 *
 * Transitions a work item from "enabled" to "executing" state. Required before
 * a task can be completed.
 *
 * @param caseId - Workflow case ID
 * @param workItemId - Work item ID to start
 * @param store - RDF graph store
 * @returns Promise resolving to task start receipt
 *
 * @example
 * ```javascript
 * // Work item ID format: caseId-taskId-instanceNum
 * const workItemId = `${caseId}-approve-1`;
 * const startReceipt = await startTask(caseId, workItemId, store);
 * ```
 *
 * @throws {ValidationError} If IDs are invalid
 * @throws {StateError} If work item is not in "enabled" state
 */
export function startTask(
  caseId: string,
  workItemId: string,
  store: any
): Promise<Receipt>;

/**
 * Complete a started work item
 *
 * Marks a work item as completed and triggers control flow evaluation to
 * enable downstream tasks. This is the primary way to advance workflow execution.
 *
 * @param caseId - Workflow case ID
 * @param workItemId - Work item ID to complete
 * @param store - RDF graph store
 * @param outputData - Task output data (merged with case data)
 * @returns Promise resolving to task completion receipt
 *
 * @example
 * ```javascript
 * const completeReceipt = await completeTask(
 *   caseId,
 *   workItemId,
 *   store,
 *   {
 *     approved: true,
 *     approvedBy: 'manager@example.com',
 *     approvedAt: new Date().toISOString()
 *   }
 * );
 *
 * // Check which tasks were enabled next
 * console.log('Next tasks:', completeReceipt.metadata.enabledTasks);
 * ```
 *
 * @throws {ValidationError} If IDs or output data are invalid
 * @throws {StateError} If work item is not in "executing" state
 */
export function completeTask(
  caseId: string,
  workItemId: string,
  store: any,
  outputData?: Record<string, unknown>
): Promise<Receipt>;

/**
 * Cancel a work item
 *
 * Cancels an enabled or executing work item. Used for exception handling,
 * cancellation regions, or external termination.
 *
 * @param caseId - Workflow case ID
 * @param workItemId - Work item ID to cancel
 * @param store - RDF graph store
 * @param reason - Cancellation reason
 * @returns Promise resolving to cancellation receipt
 *
 * @example
 * ```javascript
 * const cancelReceipt = await cancelWorkItem(
 *   caseId,
 *   workItemId,
 *   store,
 *   'Order cancelled by customer'
 * );
 * ```
 *
 * @throws {ValidationError} If IDs are invalid
 * @throws {StateError} If work item cannot be cancelled
 */
export function cancelWorkItem(
  caseId: string,
  workItemId: string,
  store: any,
  reason?: string
): Promise<Receipt>;

/**
 * Replay a workflow case to reconstruct state
 *
 * Uses KGC-4D time-travel to replay all events for a case and reconstruct
 * the complete state at any point in time. Essential for audit trails,
 * debugging, and state recovery.
 *
 * @param caseId - Workflow case ID to replay
 * @param store - RDF graph store
 * @param upToTimestamp - Optional timestamp to replay up to (ISO 8601)
 * @returns Promise resolving to reconstructed case state
 *
 * @example
 * ```javascript
 * // Replay entire case history
 * const caseState = await replayCase(caseId, store);
 * console.log('Case status:', caseState.status);
 * console.log('Work items:', caseState.workItems);
 *
 * // Replay up to specific point in time
 * const historicalState = await replayCase(
 *   caseId,
 *   store,
 *   '2025-01-15T10:30:00Z'
 * );
 * ```
 *
 * @throws {ValidationError} If case ID or timestamp is invalid
 * @throws {NotFoundError} If case doesn't exist
 */
export function replayCase(
  caseId: string,
  store: any,
  upToTimestamp?: string
): Promise<{
  caseId: string;
  workflowId: string;
  status: string;
  workItems: WorkItem[];
  data: Record<string, unknown>;
  events: Array<{
    id: string;
    type: string;
    timestamp: string;
    data: Record<string, unknown>;
  }>;
}>;

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Generate a unique identifier
 *
 * @param prefix - Optional prefix for the ID
 * @returns Unique identifier string
 */
export function generateId(prefix?: string): string;

/**
 * Get current timestamp in ISO 8601 format
 *
 * @returns ISO 8601 timestamp string
 */
export function now(): string;

/**
 * Convert Date to ISO 8601 string
 *
 * @param date - Date object
 * @returns ISO 8601 timestamp string
 */
export function toISO(date: Date): string;

/**
 * Create cryptographic hash of data
 *
 * @param data - Data to hash
 * @returns SHA-256 hash as hex string
 */
export function createHash(data: unknown): Promise<string>;

/**
 * Create a receipt for an operation
 *
 * @param operation - Operation name
 * @param eventId - Event ID
 * @param decision - Approval decision
 * @param justification - Justification message
 * @param metadata - Additional metadata
 * @returns Receipt object
 */
export function createReceipt(
  operation: string,
  eventId: string,
  decision: 'approved' | 'rejected',
  justification: string,
  metadata?: Record<string, unknown>
): Promise<Receipt>;
