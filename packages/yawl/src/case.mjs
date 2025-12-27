/**
 * @file YAWL Case - Runtime instance of a workflow with Petri net semantics
 * @module @unrdf/yawl/case
 *
 * @description
 * Case lifecycle management for YAWL workflow instances.
 * Implements Petri net marking (token placement) for workflow state tracking.
 * Integrates with KGC-4D for event sourcing and RDF for semantic representation.
 *
 * @example
 * import { createCase, Case, CaseStatus } from '@unrdf/yawl/case';
 * import { YawlWorkflow } from '@unrdf/yawl/workflow';
 *
 * const workflow = new YawlWorkflow({ id: 'expense-approval' });
 * workflow.addTask({ id: 'submit', name: 'Submit Request' });
 * workflow.addTask({ id: 'approve', name: 'Approve Request' });
 * workflow.setStart('submit');
 * workflow.setEnd(['approve']);
 *
 * const caseInstance = createCase(workflow);
 * console.log(caseInstance.getStatus()); // 'running'
 * console.log(caseInstance.getEnabledWorkItems()); // [{ submit work item }]
 */

import { CaseCore, CaseStatus, CaseDataSchema } from './case-core.mjs';
import { CaseLifecycleMixin } from './case-lifecycle.mjs';
import { CaseStateMixin } from './case-state.mjs';
import { caseToRDF, caseFromRDF } from './case-rdf.mjs';

// =============================================================================
// Composed Case Class
// =============================================================================

/**
 * Represents a runtime instance of a workflow with Petri net semantics.
 *
 * Implements token-based state tracking where:
 * - Conditions (places) hold tokens
 * - Tasks (transitions) consume and produce tokens
 * - Join semantics determine when tasks can fire
 *
 * @class
 */
export class Case extends CaseCore {
  constructor(data, workflow) {
    super(data, workflow);
    // Initialize marking after construction
    this._initializeMarking();
  }
}

// Apply mixins to Case prototype
Object.assign(Case.prototype, CaseLifecycleMixin);
Object.assign(Case.prototype, CaseStateMixin);

/**
 * Create Case from JSON
 * @param {Object} json - JSON representation
 * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
 * @returns {Promise<Case>} Restored case
 */
Case.fromJSON = async function(json, workflow) {
  const { YawlTask } = await import('./task.mjs');

  const caseInstance = new Case({
    id: json.id,
    workflowId: json.workflowId,
    status: json.status,
    createdAt: json.createdAt ? BigInt(json.createdAt) : undefined,
    startedAt: json.startedAt ? BigInt(json.startedAt) : undefined,
    completedAt: json.completedAt ? BigInt(json.completedAt) : undefined,
    data: json.data,
  }, workflow);

  // Restore work items
  for (const wiJson of json.workItems || []) {
    const task = YawlTask.fromJSON(wiJson);
    caseInstance.workItems.set(task.id, task);

    const taskDefId = caseInstance.getTaskDefIdForWorkItem(task.id);
    if (!caseInstance.workItemsByTask.has(taskDefId)) {
      caseInstance.workItemsByTask.set(taskDefId, new Set());
    }
    caseInstance.workItemsByTask.get(taskDefId).add(task.id);
  }

  // Restore completed/activated tasks
  caseInstance.completedTasks = new Set(json.completedTasks || []);
  caseInstance.activatedTasks = new Set(json.activatedTasks || []);

  // Restore circuit breakers
  caseInstance.circuitBreakers = new Map(Object.entries(json.circuitBreakers || {}));

  // Restore marking
  caseInstance._marking = new Map(Object.entries(json.marking || {}));

  // Restore event log
  caseInstance.eventLog = json.eventLog || [];

  return caseInstance;
};

// Backwards compatibility alias
export { Case as YawlCase };

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Options for creating a new case
 * @typedef {Object} CreateCaseOptions
 * @property {string} [caseId] - Custom case ID (auto-generated if not provided)
 * @property {Object} [initialData={}] - Initial case variables
 * @property {boolean} [autoStart=true] - Automatically start the case
 */

/**
 * Create a new case instance for a workflow.
 *
 * Initializes work items for all tasks and auto-enables initial tasks.
 *
 * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
 * @param {CreateCaseOptions} [options={}] - Creation options
 * @returns {Promise<Case>} Created and optionally started case
 *
 * @example
 * import { createCase } from '@unrdf/yawl/case';
 *
 * const workflow = new YawlWorkflow({ id: 'expense-approval' });
 * // ... configure workflow ...
 *
 * const caseInstance = await createCase(workflow, {
 *   caseId: 'expense-001',
 *   initialData: { amount: 500, submitter: 'john@example.com' },
 *   autoStart: true
 * });
 */
export async function createCase(workflow, options = {}) {
  const {
    caseId = `case-${workflow.id}-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`,
    initialData = {},
    autoStart = true,
  } = options;

  // Validate workflow
  const validation = workflow.validate();
  if (!validation.valid) {
    throw new Error(`Invalid workflow: ${validation.errors.join(', ')}`);
  }

  const caseInstance = new Case({
    id: caseId,
    workflowId: workflow.id,
    data: initialData,
  }, workflow);

  if (autoStart) {
    await caseInstance.start();
  }

  return caseInstance;
}

// =============================================================================
// Re-exports
// =============================================================================

// Export enums and schemas
export { CaseStatus, CaseDataSchema };

// Export RDF functions
export { caseToRDF, caseFromRDF };

// =============================================================================
// Default Export
// =============================================================================

export default {
  // Classes
  Case,
  YawlCase: Case,

  // Enums
  CaseStatus,

  // Factory
  createCase,

  // RDF Integration
  caseToRDF,
  caseFromRDF,
};
