/**
 * @file YAWL Workflow - Validation schemas and logic
 * @module @unrdf/yawl/workflow/validation
 *
 * @description
 * Zod schemas for workflow specifications and validation methods.
 * Provides comprehensive validation for control flow, split/join patterns,
 * reachability, cancellation regions, and cycle detection.
 */

import { z } from 'zod';

// =============================================================================
// Zod Schemas for Workflow Validation
// =============================================================================

/**
 * Task definition schema for workflow tasks
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
  /** Cancellation region ID */
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
  tasks: z.array(TaskDefSchema).min(1),
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
// Validation Methods (to be used by Workflow class)
// =============================================================================

/**
 * Validate basic workflow structure
 * @param {Object} workflow - Workflow instance with _tasks, _startTaskId, _endTaskIds
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 */
export function validateBasicStructure(workflow, errors, warnings) {
  // Must have at least one task
  if (workflow._tasks.size === 0) {
    errors.push('Workflow has no tasks');
  }

  // Must have start task
  if (!workflow._startTaskId) {
    errors.push('Workflow has no start task');
  } else if (!workflow._tasks.has(workflow._startTaskId)) {
    errors.push(`Start task '${workflow._startTaskId}' not found in workflow`);
  }

  // Should have at least one end task
  if (workflow._endTaskIds.length === 0) {
    warnings.push('Workflow has no designated end tasks');
  }

  // End tasks must exist
  for (const endTaskId of workflow._endTaskIds) {
    if (!workflow._tasks.has(endTaskId)) {
      errors.push(`End task '${endTaskId}' not found in workflow`);
    }
  }
}

/**
 * Validate control flow integrity
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 */
export function validateControlFlowIntegrity(workflow, errors, warnings) {
  // All flows must reference existing tasks
  for (const flow of workflow._flows) {
    if (!workflow._tasks.has(flow.from)) {
      errors.push(`Flow references non-existent source task '${flow.from}'`);
    }
    if (!workflow._tasks.has(flow.to)) {
      errors.push(`Flow references non-existent target task '${flow.to}'`);
    }

    // Check for self-loops (usually invalid in YAWL)
    if (flow.from === flow.to) {
      warnings.push(`Flow from '${flow.from}' to itself detected (self-loop)`);
    }
  }

  // Check for duplicate flows
  const flowSet = new Set();
  for (const flow of workflow._flows) {
    const key = `${flow.from}->${flow.to}`;
    if (flowSet.has(key)) {
      warnings.push(`Duplicate flow from '${flow.from}' to '${flow.to}'`);
    }
    flowSet.add(key);
  }
}

/**
 * Validate split/join type consistency
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 */
export function validateSplitJoinConsistency(workflow, errors, warnings) {
  const SPLIT_TYPE = {
    SEQUENCE: 'sequence',
    AND: 'and',
    XOR: 'xor',
    OR: 'or',
  };

  const JOIN_TYPE = {
    SEQUENCE: 'sequence',
    AND: 'and',
    XOR: 'xor',
    OR: 'or',
  };

  for (const [taskId, task] of workflow._tasks) {
    const outgoing = workflow._outgoingFlows.get(taskId) ?? [];
    const incoming = workflow._incomingFlows.get(taskId) ?? [];

    // Validate split type matches outgoing flow count
    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    if (splitType === SPLIT_TYPE.SEQUENCE && outgoing.length > 1) {
      errors.push(
        `Task '${taskId}' has sequence split but ${outgoing.length} outgoing flows`
      );
    }
    if (
      (splitType === SPLIT_TYPE.AND ||
        splitType === SPLIT_TYPE.XOR ||
        splitType === SPLIT_TYPE.OR) &&
      outgoing.length < 2
    ) {
      warnings.push(
        `Task '${taskId}' has ${splitType} split but only ${outgoing.length} outgoing flow(s)`
      );
    }

    // Validate join type matches incoming flow count
    const joinType = task.joinType ?? JOIN_TYPE.SEQUENCE;
    if (joinType === JOIN_TYPE.SEQUENCE && incoming.length > 1) {
      errors.push(
        `Task '${taskId}' has sequence join but ${incoming.length} incoming flows`
      );
    }
    if (
      (joinType === JOIN_TYPE.AND ||
        joinType === JOIN_TYPE.XOR ||
        joinType === JOIN_TYPE.OR) &&
      incoming.length < 2
    ) {
      warnings.push(
        `Task '${taskId}' has ${joinType} join but only ${incoming.length} incoming flow(s)`
      );
    }

    // XOR split should have at least one flow with condition or default
    if (splitType === SPLIT_TYPE.XOR && outgoing.length > 1) {
      const hasConditions = outgoing.some(f => f.condition || f.isDefault);
      if (!hasConditions) {
        warnings.push(
          `Task '${taskId}' has XOR split but no conditions or default flow defined`
        );
      }
    }
  }
}

/**
 * Validate all tasks are reachable from start
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 */
export function validateReachability(workflow, errors, warnings) {
  if (!workflow._startTaskId) return;

  const visited = new Set();
  const queue = [workflow._startTaskId];

  while (queue.length > 0) {
    const taskId = queue.shift();
    if (visited.has(taskId)) continue;
    visited.add(taskId);

    const outgoing = workflow._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) {
      if (!visited.has(flow.to)) {
        queue.push(flow.to);
      }
    }
  }

  // Check for unreachable tasks
  for (const [taskId] of workflow._tasks) {
    if (!visited.has(taskId)) {
      errors.push(`Task '${taskId}' is not reachable from start task`);
    }
  }
}

/**
 * Validate cancellation regions
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 */
export function validateCancellationRegions(workflow, errors, warnings) {
  for (const [regionId, taskIds] of workflow._regionToTasks) {
    // All tasks in region must exist
    for (const taskId of taskIds) {
      if (!workflow._tasks.has(taskId)) {
        errors.push(
          `Cancellation region '${regionId}' references non-existent task '${taskId}'`
        );
      }
    }

    // Region should have at least 2 tasks
    if (taskIds.length < 2) {
      warnings.push(`Cancellation region '${regionId}' has only ${taskIds.length} task(s)`);
    }
  }

  // Check cancellation sets reference valid tasks
  for (const [taskId, task] of workflow._tasks) {
    if (task.cancellationSet) {
      for (const cancelTaskId of task.cancellationSet) {
        if (!workflow._tasks.has(cancelTaskId)) {
          errors.push(
            `Task '${taskId}' cancellation set references non-existent task '${cancelTaskId}'`
          );
        }
      }
    }
  }
}

/**
 * Validate no invalid cycles exist
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 */
export function validateNoCycles(workflow, errors, warnings) {
  // Detect cycles using DFS
  const visited = new Set();
  const recStack = new Set();
  const cycles = [];

  const hasCycle = (taskId, path = []) => {
    if (recStack.has(taskId)) {
      const cycleStart = path.indexOf(taskId);
      cycles.push(path.slice(cycleStart).concat(taskId));
      return true;
    }
    if (visited.has(taskId)) {
      return false;
    }

    visited.add(taskId);
    recStack.add(taskId);
    path.push(taskId);

    const outgoing = workflow._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) {
      hasCycle(flow.to, path);
    }

    path.pop();
    recStack.delete(taskId);
    return false;
  };

  for (const [taskId] of workflow._tasks) {
    if (!visited.has(taskId)) {
      hasCycle(taskId, []);
    }
  }

  // In YAWL, some cycles are valid (loops), but we warn about them
  for (const cycle of cycles) {
    warnings.push(`Cycle detected: ${cycle.join(' -> ')}`);
  }
}
