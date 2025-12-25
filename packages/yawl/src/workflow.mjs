/**
 * @file YAWL Workflow - Workflow definition management (DEPRECATED)
 * @module @unrdf/yawl/workflow
 *
 * @deprecated This file has been split into modular components in /workflow directory.
 * Use `import { ... } from '@unrdf/yawl/workflow'` which now points to the modular index.
 * This file remains for backward compatibility but re-exports from the new structure.
 *
 * @description
 * Complete workflow definition management with:
 * - Workflow class for workflow structure and queries
 * - Factory function for creating validated workflows
 * - Comprehensive validation logic (control flow, split/join, reachability)
 * - RDF integration for serialization/deserialization
 * - Zod schemas for type-safe validation
 *
 * **NEW MODULAR STRUCTURE** (since refactor):
 * - workflow/schemas.mjs - Zod validation schemas
 * - workflow/workflow-class.mjs - Core Workflow class
 * - workflow/validation.mjs - Validation logic
 * - workflow/control-flow.mjs - Control flow evaluation
 * - workflow/mutations.mjs - Workflow mutation operations
 * - workflow/serialization.mjs - JSON serialization
 * - workflow/rdf.mjs - RDF integration
 * - workflow/index.mjs - Barrel exports (recommended import)
 *
 * @example
 * // RECOMMENDED: Import from modular structure
 * import { Workflow, createWorkflow, workflowToRDF, workflowFromRDF } from '@unrdf/yawl/workflow';
 *
 * // DEPRECATED: Direct import from this file (still works)
 * import { Workflow, createWorkflow } from '@unrdf/yawl/src/workflow.mjs';
 */

// =============================================================================
// RE-EXPORTS FROM MODULAR STRUCTURE (Backward Compatibility)
// =============================================================================

// Core exports
export {
  Workflow,
  YawlWorkflow,
  createWorkflow,
  TaskDefSchema,
  FlowDefSchema,
  WorkflowSpecSchema,
  validateWorkflow,
  workflowToRDF,
  workflowFromRDF,
  SPLIT_TYPE,
  JOIN_TYPE,
} from './workflow/index.mjs';

// Default export
export { default } from './workflow/index.mjs';
