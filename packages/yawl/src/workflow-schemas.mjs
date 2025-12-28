/**
 * @file YAWL Workflow Schemas - Zod validation schemas
 * @module @unrdf/yawl/workflow-schemas
 *
 * @description
 * Type-safe validation schemas for YAWL workflow specifications using Zod.
 * Defines schemas for tasks, flows, and complete workflow specifications.
 *
 * @example
 * import { TaskDefSchema, FlowDefSchema, WorkflowSpecSchema } from '@unrdf/yawl/workflow-schemas';
import { WorkflowError } from '../errors.mjs';
 *
 * const taskDef = TaskDefSchema.parse({
 *   id: 'review',
 *   name: 'Review Task',
 *   splitType: 'xor',
 * });
 */

import { z } from 'zod';

// =============================================================================
// Zod Schemas for Workflow Validation
// =============================================================================

/**
 * Task definition schema for workflow tasks
 *
 * Validates individual task specifications including control flow semantics,
 * resource assignments, and cancellation regions.
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
 * Validates control flow arcs between tasks, including conditions,
 * priorities, and default flow designation.
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
 * Validates entire workflow definitions including tasks, flows,
 * start/end designations, and cancellation regions.
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
  /** Task definitions (can be added after creation via addTask()) */
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
