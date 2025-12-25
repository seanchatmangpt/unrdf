/**
 * @file YAWL Workflow - Workflow definition management (Barrel Export)
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
 * This module aggregates functionality from:
 * - workflow-schemas.mjs: Zod validation schemas
 * - workflow-core.mjs: Workflow class definition
 * - workflow-execution.mjs: Validation and control flow evaluation
 * - workflow-rdf.mjs: RDF serialization/deserialization
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

// =============================================================================
// Import All Modules
// =============================================================================

// Schemas
export {
  TaskDefSchema,
  FlowDefSchema,
  WorkflowSpecSchema,
} from './workflow-schemas.mjs';

// Core Workflow class
export { Workflow } from './workflow-core.mjs';

// Execution: validation and control flow (adds methods to Workflow.prototype)
export { createWorkflow } from './workflow-execution.mjs';

// RDF integration
export { workflowToRDF, workflowFromRDF } from './workflow-rdf.mjs';

// Re-export SPLIT_TYPE and JOIN_TYPE from patterns for convenience
export { SPLIT_TYPE, JOIN_TYPE } from './patterns.mjs';

// =============================================================================
// Legacy Export (for backward compatibility)
// =============================================================================

import { Workflow } from './workflow-core.mjs';

/**
 * Legacy YawlWorkflow class (alias for Workflow)
 * @deprecated Use Workflow instead
 */
export const YawlWorkflow = Workflow;

// =============================================================================
// Default Export
// =============================================================================

import { createWorkflow } from './workflow-execution.mjs';
import { workflowToRDF, workflowFromRDF } from './workflow-rdf.mjs';
import {
  TaskDefSchema,
  FlowDefSchema,
  WorkflowSpecSchema,
} from './workflow-schemas.mjs';
import { SPLIT_TYPE, JOIN_TYPE } from './patterns.mjs';

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

  // Split/Join constants (re-exported for convenience)
  SPLIT_TYPE,
  JOIN_TYPE,
};
