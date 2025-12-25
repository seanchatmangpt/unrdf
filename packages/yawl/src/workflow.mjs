/**
 * @file YAWL Workflow - Complete workflow definition management
 * @module @unrdf/yawl/workflow
 *
 * @description
 * Complete workflow definition management with:
 * - Workflow class for workflow structure and queries
 * - Factory function for creating validated workflows
 * - Comprehensive validation logic (control flow, split/join, reachability)
 * - RDF integration for serialization/deserialization
 * - Zod schemas for type-safe validation
 *
 * This module assembles the complete Workflow class from modular components.
 *
 * @example
 * import { Workflow, createWorkflow, workflowToRDF, workflowFromRDF } from '@unrdf/yawl';
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

import { Workflow as WorkflowCore, WorkflowSpecSchema, TaskDefSchema, FlowDefSchema } from './workflow-core.mjs';
import * as ValidationMethods from './workflow-validation.mjs';
import * as PatternMethods from './workflow-patterns.mjs';
import { workflowToRDF as _workflowToRDF, workflowFromRDF as _workflowFromRDF } from './workflow-rdf.mjs';
import { SPLIT_TYPE, JOIN_TYPE } from './patterns.mjs';

// =============================================================================
// Assemble Complete Workflow Class
// =============================================================================

/**
 * Complete Workflow class with all methods
 */
export class Workflow extends WorkflowCore {
  constructor(spec) {
    super(spec);
  }
}

// Add validation methods to prototype
Object.assign(Workflow.prototype, {
  validate: ValidationMethods.validate,
  isValid: ValidationMethods.isValid,
  _validateBasicStructure: ValidationMethods._validateBasicStructure,
  _validateControlFlowIntegrity: ValidationMethods._validateControlFlowIntegrity,
  _validateSplitJoinConsistency: ValidationMethods._validateSplitJoinConsistency,
  _validateMatchingSplitJoin: ValidationMethods._validateMatchingSplitJoin,
  _findConvergencePoint: ValidationMethods._findConvergencePoint,
  _validateReachability: ValidationMethods._validateReachability,
  _validateCancellationRegions: ValidationMethods._validateCancellationRegions,
  _validateNoCycles: ValidationMethods._validateNoCycles,
});

// Add pattern methods to prototype
Object.assign(Workflow.prototype, {
  evaluateDownstream: PatternMethods.evaluateDownstream,
  canEnable: PatternMethods.canEnable,
});

// =============================================================================
// Factory Function
// =============================================================================

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
// RDF Integration
// =============================================================================

/**
 * Serialize workflow to RDF representation in store
 *
 * Creates YAWL RDF representation using yawl-ontology predicates and classes.
 *
 * @param {Workflow} workflow - Workflow to serialize
 * @param {Object} store - RDF store (from @unrdf/oxigraph)
 * @param {Object} [options={}] - Serialization options
 * @param {string} [options.graph] - Named graph URI (defaults to spec-based)
 * @returns {Object} Result with specUri and quad count
 *
 * @example
 * import { createStore } from '@unrdf/oxigraph';
 * import { createWorkflow, workflowToRDF } from '@unrdf/yawl';
 *
 * const store = createStore();
 * const workflow = createWorkflow({ ... });
 * const { specUri, quadCount } = workflowToRDF(workflow, store);
 */
export function workflowToRDF(workflow, store, options = {}) {
  return _workflowToRDF(workflow, store, options);
}

/**
 * Load workflow from RDF store
 *
 * Reconstructs a Workflow instance from its RDF representation.
 *
 * @param {Object} store - RDF store (from @unrdf/oxigraph)
 * @param {string} workflowId - Workflow specification ID
 * @param {Object} [options={}] - Loading options
 * @param {string} [options.graph] - Named graph URI to query
 * @returns {Promise<Workflow|null>} Workflow instance or null if not found
 *
 * @example
 * import { createStore } from '@unrdf/oxigraph';
 * import { workflowFromRDF } from '@unrdf/yawl';
 *
 * const workflow = await workflowFromRDF(store, 'expense-approval');
 * if (workflow) {
 *   console.log(`Loaded workflow with ${workflow.getTasks().length} tasks`);
 * }
 */
export async function workflowFromRDF(store, workflowId, options = {}) {
  return _workflowFromRDF(store, workflowId, options, Workflow);
}

// =============================================================================
// Legacy Export (for backward compatibility)
// =============================================================================

/**
 * Legacy YawlWorkflow class (alias for Workflow)
 * @deprecated Use Workflow instead
 */
export const YawlWorkflow = Workflow;

// =============================================================================
// Module Exports
// =============================================================================

export {
  // Schemas (re-exported from workflow-core)
  WorkflowSpecSchema,
  TaskDefSchema,
  FlowDefSchema,
  // Constants (re-exported from patterns)
  SPLIT_TYPE,
  JOIN_TYPE,
};

export default {
  // Main class
  Workflow,
  YawlWorkflow, // Legacy alias

  // Factory
  createWorkflow,

  // RDF integration
  workflowToRDF,
  workflowFromRDF,

  // Schemas
  WorkflowSpecSchema,
  TaskDefSchema,
  FlowDefSchema,

  // Split/Join constants
  SPLIT_TYPE,
  JOIN_TYPE,
};
