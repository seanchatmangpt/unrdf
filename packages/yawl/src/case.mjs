/**
 * @file YAWL Case - Runtime instance of a workflow with Petri net semantics
 * @module @unrdf/yawl/case
 *
 * @description
 * Barrel export for YAWL case functionality. Combines:
 * - case-core.mjs: Core Case class with state management
 * - case-lifecycle.mjs: Petri net marking and task lifecycle
 * - case-rdf.mjs: RDF serialization/deserialization
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
 * const caseInstance = await createCase(workflow);
 * console.log(caseInstance.getStatus()); // 'running'
 * console.log(caseInstance.getEnabledWorkItems()); // [{ submit work item }]
 */

// Core class and enums
import { Case as CaseClass, YawlCase, CaseStatus } from './case-core.mjs';

// Install lifecycle methods on Case prototype
import { installLifecycleMethods } from './case-lifecycle.mjs';
installLifecycleMethods(CaseClass);

// Re-export
export { CaseClass as Case, YawlCase, CaseStatus };

// RDF integration
import { caseToRDF, caseFromRDF } from './case-rdf.mjs';
export { caseToRDF, caseFromRDF };

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

  const caseInstance = new CaseClass({
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
// Default Export
// =============================================================================

export default {
  // Classes
  Case: CaseClass,
  YawlCase: CaseClass,

  // Enums
  CaseStatus,

  // Factory
  createCase,

  // RDF Integration
  caseToRDF,
  caseFromRDF,
};
