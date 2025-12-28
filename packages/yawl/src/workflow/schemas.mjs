/**
 * @file YAWL Workflow Schemas - Zod validation schemas for workflow definitions
 * @module @unrdf/yawl/workflow/schemas
 *
 * @description
 * Centralized Zod schemas for type-safe workflow validation:
 * - TaskDefSchema: Task definition validation
 * - FlowDefSchema: Control flow validation
 * - WorkflowSpecSchema: Complete workflow specification
 *
 * All schemas enforce YAWL semantics and constraints.
 *
 * @example
 * import { WorkflowSpecSchema } from '@unrdf/yawl/workflow/schemas';
import { WorkflowError } from '../../errors.mjs';
 *
 * const validated = WorkflowSpecSchema.parse({
 *   id: 'my-workflow',
 *   tasks: [{ id: 'task1', name: 'Task 1' }],
 *   flows: [{ from: 'task1', to: 'task2' }],
 * });
 */

import { z } from 'zod';

// =============================================================================
// Zod Schemas for Workflow Validation
// =============================================================================

/**
 * Task definition schema for workflow tasks
 *
 * Validates task structure including:
 * - Task identity (id, name)
 * - Task kind (atomic, composite, multiple, cancellation)
 * - Split/join semantics (sequence, and, xor, or)
 * - Cancellation configuration
 * - Resource assignments
 * - Execution constraints (timeout, priority)
 *
 * @constant
 * @type {z.ZodObject}
 */
export const TaskDefSchema = z.object({
  /** Unique task identifier within the workflow */
  id: z.string().min(1).max(100),
  /** Human-readable task name */
  name: z.string().min(1).max(200).optional(),
  /** Task kind: atomic, composite, multiple, cancellation */
  kind: z.enum(['atomic', 'composite', 'multiple', 'cancellation']).default('atomic'),
  /** Outgoing flow semantics */
  splitType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  /** Incoming flow semantics */
  joinType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  /** Task IDs in this task's cancellation region */
  cancellationRegion: z.string().optional(),
  /** Tasks cancelled when this task completes */
  cancellationSet: z.array(z.string()).optional(),
  /** Condition function for task enablement */
  condition: z.function().optional(),
  /** Timeout in milliseconds */
  timeout: z.number().positive().optional(),
  /** Assigned resource pattern */
  resource: z.string().optional(),
  /** Required role */
  role: z.string().optional(),
  /** Sub-workflow ID for composite tasks */
  subNetId: z.string().optional(),
  /** Task priority (0-100) */
  priority: z.number().int().min(0).max(100).optional(),
  /** Task documentation */
  documentation: z.string().max(2000).optional(),
});

/**
 * Flow definition schema for control flow connections
 *
 * Validates flow structure including:
 * - Source and target tasks
 * - Conditional routing (XOR/OR splits)
 * - Priority ordering
 * - Default flow designation
 *
 * @constant
 * @type {z.ZodObject}
 */
export const FlowDefSchema = z.object({
  /** Source task ID */
  from: z.string().min(1),
  /** Target task ID */
  to: z.string().min(1),
  /** Condition function for conditional flows */
  condition: z.function().optional(),
  /** Evaluation priority for XOR/OR splits (higher = first) */
  priority: z.number().default(0),
  /** Whether this is the default flow */
  isDefault: z.boolean().optional(),
  /** Flow documentation */
  documentation: z.string().max(1000).optional(),
});

/**
 * Complete workflow specification schema
 *
 * Validates entire workflow structure including:
 * - Workflow metadata (id, name, version, description)
 * - Task and flow definitions
 * - Start/end task designations
 * - Cancellation regions
 * - Authorship and timestamps
 *
 * @constant
 * @type {z.ZodObject}
 */
export const WorkflowSpecSchema = z.object({
  /** Unique workflow identifier */
  id: z.string().min(1).max(100),
  /** Human-readable workflow name */
  name: z.string().min(1).max(200).optional(),
  /** Semantic version string */
  version: z.string().regex(/^\d+\.\d+\.\d+$/).default('1.0.0'),
  /** Workflow description */
  description: z.string().max(5000).optional(),
  /** Task definitions */
  tasks: z.array(TaskDefSchema).default([]),
  /** Flow definitions */
  flows: z.array(FlowDefSchema).optional().default([]),
  /** Starting task ID (auto-detected if not specified) */
  startTaskId: z.string().optional(),
  /** Ending task IDs (auto-detected if not specified) */
  endTaskIds: z.array(z.string()).optional().default([]),
  /** Cancellation regions mapping region ID to task IDs */
  cancellationRegions: z.record(z.string(), z.array(z.string())).optional().default({}),
  /** Workflow author */
  author: z.string().max(100).optional(),
  /** Creation timestamp */
  createdAt: z.date().optional(),
  /** Modification timestamp */
  modifiedAt: z.date().optional(),
});

/**
 * Validation result type
 *
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {string[]} errors - List of validation errors
 * @property {string[]} warnings - List of validation warnings
 */

// =============================================================================
// Module Exports
// =============================================================================

export default {
  TaskDefSchema,
  FlowDefSchema,
  WorkflowSpecSchema,
};
