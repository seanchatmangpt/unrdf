/**
 * @file YAWL Workflow Validation - Comprehensive workflow integrity validation
 * @module @unrdf/yawl/workflow/validation
 *
 * @description
 * Extracted validation logic for Workflow instances:
 * - Basic structure validation (tasks, flows, start/end)
 * - Control flow integrity (flow references, duplicates)
 * - Split/join consistency (pattern matching)
 * - Reachability analysis (all tasks reachable from start)
 * - Cancellation region validation
 * - Cycle detection (with warnings for loops)
 *
 * @example
 * import { validateWorkflow } from '@unrdf/yawl/workflow/validation';
 *
 * const result = validateWorkflow(workflowInstance);
 * if (!result.valid) {
 *   console.error('Validation errors:', result.errors);
 * }
 */

import { SPLIT_TYPE, JOIN_TYPE } from '../patterns.mjs';

/**
 * Validate workflow structure and integrity
 *
 * Performs comprehensive validation including:
 * - Basic structure (tasks, start/end)
 * - Control flow integrity
 * - Split/join consistency
 * - Reachability
 * - Cancellation regions
 * - Cycle detection
 *
 * @param {Object} workflow - Workflow instance to validate
 * @returns {ValidationResult} Validation result with errors and warnings
 *
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {string[]} errors - List of validation errors
 * @property {string[]} warnings - List of validation warnings
 */
export function validateWorkflow(workflow) {
  const errors = [];
  const warnings = [];

  // Basic structure validation
  validateBasicStructure(workflow, errors, warnings);

  // Control flow validation
  validateControlFlowIntegrity(workflow, errors, warnings);

  // Split/Join consistency
  validateSplitJoinConsistency(workflow, errors, warnings);

  // Reachability
  validateReachability(workflow, errors, warnings);

  // Cancellation regions
  validateCancellationRegions(workflow, errors, warnings);

  // Cycle detection
  validateNoCycles(workflow, errors, warnings);

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Validate basic workflow structure
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
function validateBasicStructure(workflow, errors, warnings) {
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
 * @private
 */
function validateControlFlowIntegrity(workflow, errors, warnings) {
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
 * @private
 */
function validateSplitJoinConsistency(workflow, errors, warnings) {
  for (const [taskId, task] of workflow._tasks) {
    const outgoing = workflow._outgoingFlows.get(taskId) ?? [];
    const incoming = workflow._incomingFlows.get(taskId) ?? [];

    // Validate split type matches outgoing flow count
    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    if (splitType === SPLIT_TYPE.SEQUENCE && outgoing.length > 1) {
      errors.push(`Task '${taskId}' has sequence split but ${outgoing.length} outgoing flows`);
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
      errors.push(`Task '${taskId}' has sequence join but ${incoming.length} incoming flows`);
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
      const hasConditions = outgoing.some((f) => f.condition || f.isDefault);
      if (!hasConditions) {
        warnings.push(
          `Task '${taskId}' has XOR split but no conditions or default flow defined`
        );
      }
    }
  }

  // Check matching split-join patterns (AND-AND, XOR-XOR, OR-OR)
  validateMatchingSplitJoin(workflow, errors, warnings);
}

/**
 * Validate matching split-join patterns
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
function validateMatchingSplitJoin(workflow, errors, warnings) {
  // Find all split points and their corresponding join points
  for (const [taskId, task] of workflow._tasks) {
    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    if (
      splitType === SPLIT_TYPE.AND ||
      splitType === SPLIT_TYPE.XOR ||
      splitType === SPLIT_TYPE.OR
    ) {
      // Find the convergence point
      const convergence = findConvergencePoint(workflow, taskId);
      if (convergence) {
        const joinTask = workflow._tasks.get(convergence);
        if (joinTask) {
          const joinType = joinTask.joinType ?? JOIN_TYPE.SEQUENCE;
          // Check for matching types
          if (splitType === SPLIT_TYPE.AND && joinType !== JOIN_TYPE.AND) {
            warnings.push(
              `AND-split at '${taskId}' converges at '${convergence}' with ${joinType}-join (expected AND-join)`
            );
          }
          // XOR split can merge with XOR or sequence
          // OR split should merge with OR join
          if (splitType === SPLIT_TYPE.OR && joinType !== JOIN_TYPE.OR) {
            warnings.push(
              `OR-split at '${taskId}' converges at '${convergence}' with ${joinType}-join (expected OR-join)`
            );
          }
        }
      }
    }
  }
}

/**
 * Find the convergence point for a split
 * @param {Object} workflow - Workflow instance
 * @param {string} splitTaskId - Split task ID
 * @returns {string|null} Convergence task ID or null
 * @private
 */
function findConvergencePoint(workflow, splitTaskId) {
  const outgoing = workflow._outgoingFlows.get(splitTaskId) ?? [];
  if (outgoing.length < 2) return null;

  // Simple heuristic: find first common descendant
  const visited = new Map();
  const queue = outgoing.map((f) => ({ taskId: f.to, path: new Set([splitTaskId, f.to]) }));

  while (queue.length > 0) {
    const { taskId, path } = queue.shift();

    if (visited.has(taskId)) {
      const existingPaths = visited.get(taskId);
      existingPaths.push(path);

      // Check if all outgoing branches reach this point
      if (existingPaths.length >= 2) {
        return taskId;
      }
    } else {
      visited.set(taskId, [path]);
    }

    const nextFlows = workflow._outgoingFlows.get(taskId) ?? [];
    for (const flow of nextFlows) {
      if (!path.has(flow.to)) {
        const newPath = new Set(path);
        newPath.add(flow.to);
        queue.push({ taskId: flow.to, path: newPath });
      }
    }
  }

  return null;
}

/**
 * Validate all tasks are reachable from start
 * @param {Object} workflow - Workflow instance
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
function validateReachability(workflow, errors, warnings) {
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
 * @private
 */
function validateCancellationRegions(workflow, errors, warnings) {
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
 * @private
 */
function validateNoCycles(workflow, errors, warnings) {
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

// =============================================================================
// Module Exports
// =============================================================================

export default {
  validateWorkflow,
};
