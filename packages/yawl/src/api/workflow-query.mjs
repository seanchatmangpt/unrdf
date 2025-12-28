/**
 * @file YAWL Workflow Query - Case creation and query operations
 * @module @unrdf/yawl/api/workflow-query
 *
 * @description
 * Handles case creation and query operations for workflows and cases.
 * Pure functions with no side effects except event append.
 */

import { z } from 'zod';
import { WorkflowError } from '../../errors.mjs';
import {
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  generateId,
  now,
  toISO,
  createReceipt,
} from './workflow-creation.mjs';
import { WorkItemSchema } from './workflow-execution.mjs';

// ============================================================================
// Zod Schemas
// ============================================================================

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

// ============================================================================
// Core Query Functions
// ============================================================================

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

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Create RDF representation of case
 * @param {string} caseId - Case identifier
 * @param {string} workflowId - Workflow identifier
 * @param {Map<string, Object>} workItems - Work items map
 * @param {Object} store - RDF store
 * @returns {Array<Object>} RDF deltas
 */
export function createCaseRDF(caseId, workflowId, workItems, store) {
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
