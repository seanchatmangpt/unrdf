/**
 * @file YAWL Workflow - Complete workflow definition management (Barrel Export)
 * @module @unrdf/yawl/workflow
 *
 * @description
 * Unified export for YAWL workflow functionality:
 * - Workflow class for workflow structure and queries
 * - Factory function for creating validated workflows
 * - Comprehensive validation logic (control flow, split/join, reachability)
 * - RDF integration for serialization/deserialization
 * - Zod schemas for type-safe validation
 *
 * This module provides backward-compatible exports with the original monolithic
 * workflow.mjs while delegating to modular implementations.
 *
 * @example
 * import { Workflow, createWorkflow, workflowToRDF, workflowFromRDF } from '@unrdf/yawl/workflow';
 *
 * const workflow = createWorkflow({
 *   id: 'expense-approval',
 *   name: 'Expense Approval Process',
 *   tasks: [
 *     { id: 'submit', name: 'Submit Expense' },
 *     { id: 'review', name: 'Review', splitType: 'xor' },
 *     { id: 'approve', name: 'Approve' },
 *     { id: 'reject', name: 'Reject' },
 *   ],
 *   flows: [
 *     { from: 'submit', to: 'review' },
 *     { from: 'review', to: 'approve', condition: ctx => ctx.amount < 1000 },
 *     { from: 'review', to: 'reject', priority: 0 },
 *   ],
 * });
 */

// Core workflow class
export { Workflow, default as WorkflowClass } from './workflow-class.mjs';

// Validation schemas
export { TaskDefSchema, FlowDefSchema, WorkflowSpecSchema } from './schemas.mjs';

// Validation logic
export { validateWorkflow } from './validation.mjs';

// RDF integration
export { workflowToRDF, workflowFromRDF } from './rdf.mjs';

// Re-export split/join constants from patterns
import { SPLIT_TYPE, JOIN_TYPE } from '../patterns.mjs';
export { SPLIT_TYPE, JOIN_TYPE };

// =============================================================================
// Factory Function
// =============================================================================

import { Workflow } from './workflow-class.mjs';

/**
 * Create a new workflow from specification
 *
 * Factory function that validates the specification, builds task index,
 * constructs control flow graph, and returns a Workflow instance.
 *
 * @param {Object} spec - Workflow specification
 * @param {string} spec.id - Unique workflow identifier
 * @param {string} [spec.name] - Human-readable name
 * @param {string} [spec.version='1.0.0'] - Semantic version
 * @param {Array<Object>} spec.tasks - Task definitions
 * @param {Array<Object>} [spec.flows=[]] - Flow definitions
 * @param {Object} [options={}] - Creation options
 * @param {boolean} [options.validate=true] - Run validation after creation
 * @param {boolean} [options.strict=false] - Throw on validation errors
 * @returns {Workflow} Workflow instance
 *
 * @example
 * const workflow = createWorkflow({
 *   id: 'expense-approval',
 *   name: 'Expense Approval Process',
 *   tasks: [
 *     { id: 'submit', name: 'Submit Expense' },
 *     { id: 'review', name: 'Review Expense', splitType: 'xor' },
 *     { id: 'approve', name: 'Approve' },
 *     { id: 'reject', name: 'Reject' },
 *   ],
 *   flows: [
 *     { from: 'submit', to: 'review' },
 *     { from: 'review', to: 'approve', condition: ctx => ctx.amount < 1000 },
 *     { from: 'review', to: 'reject', isDefault: true },
 *   ],
 * }, { validate: true, strict: true });
 */
export function createWorkflow(spec, options = {}) {
  const { validate = true, strict = false } = options;

  // Create workflow instance
  const workflow = new Workflow(spec);

  // Run validation if requested
  if (validate) {
    const result = workflow.validate();

    if (!result.valid && strict) {
      throw new Error(`Workflow validation failed:\n${result.errors.join('\n')}`);
    }

    // Attach validation result for inspection
    workflow._validationResult = result;
  }

  return workflow;
}

// =============================================================================
// Legacy Alias (for backward compatibility)
// =============================================================================

/**
 * Legacy YawlWorkflow class (alias for Workflow)
 * @deprecated Use Workflow instead
 */
export const YawlWorkflow = Workflow;

// =============================================================================
// Default Export
// =============================================================================

import { workflowToRDF as toRDF, workflowFromRDF as fromRDF } from './rdf.mjs';
import { WorkflowSpecSchema as WFSpecSchema, TaskDefSchema as TDefSchema, FlowDefSchema as FDefSchema } from './schemas.mjs';
import { validateWorkflow as validateWF } from './validation.mjs';

export default {
  // Main class
  Workflow,
  YawlWorkflow, // Legacy alias

  // Factory
  createWorkflow,

  // RDF integration
  workflowToRDF: toRDF,
  workflowFromRDF: fromRDF,

  // Schemas
  WorkflowSpecSchema: WFSpecSchema,
  TaskDefSchema: TDefSchema,
  FlowDefSchema: FDefSchema,

  // Split/Join constants
  SPLIT_TYPE,
  JOIN_TYPE,

  // Validation
  validateWorkflow: validateWF,
};
