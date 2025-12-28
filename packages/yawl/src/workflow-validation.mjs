/**
 * @file YAWL Workflow - Validation methods
 * @module @unrdf/yawl/workflow-validation
 *
 * @description
 * Comprehensive validation logic for workflow integrity including:
 * - Basic structure validation
 * - Control flow integrity
 * - Split/join consistency
 * - Reachability analysis
 * - Cancellation region validation
 * - Cycle detection
 */

import { SPLIT_TYPE, JOIN_TYPE } from './patterns.mjs';
import { WorkflowError } from '../errors.mjs';

/**
 * Validate workflow structure and integrity
 * @returns {ValidationResult} Validation result with errors and warnings
 *
 * @example
 * const result = workflow.validate();
 * if (!result.valid) {
 *   console.error('Validation errors:', result.errors);
 * }
 */
export function validate() {
  const errors = [];
  const warnings = [];

  // Basic structure validation
  this._validateBasicStructure(errors, warnings);

  // Control flow validation
  this._validateControlFlowIntegrity(errors, warnings);

  // Split/Join consistency
  this._validateSplitJoinConsistency(errors, warnings);

  // Reachability
  this._validateReachability(errors, warnings);

  // Cancellation regions
  this._validateCancellationRegions(errors, warnings);

  // Cycle detection
  this._validateNoCycles(errors, warnings);

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Quick check if workflow is valid
 * @returns {boolean} True if workflow is valid
 */
export function isValid() {
  return this.validate().valid;
}

/**
 * Validate basic structure
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateBasicStructure(errors, warnings) {
  // Must have at least one task
  if (this._tasks.size === 0) {
    errors.push('Workflow has no tasks');
  }

  // Must have start task
  if (!this._startTaskId) {
    errors.push('Workflow has no start task');
  } else if (!this._tasks.has(this._startTaskId)) {
    errors.push(`Start task '${this._startTaskId}' not found in workflow`);
  }

  // Should have at least one end task
  if (this._endTaskIds.length === 0) {
    warnings.push('Workflow has no designated end tasks');
  }

  // End tasks must exist
  for (const endTaskId of this._endTaskIds) {
    if (!this._tasks.has(endTaskId)) {
      errors.push(`End task '${endTaskId}' not found in workflow`);
    }
  }
}

/**
 * Validate control flow integrity
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateControlFlowIntegrity(errors, warnings) {
  // All flows must reference existing tasks
  for (const flow of this._flows) {
    if (!this._tasks.has(flow.from)) {
      errors.push(`Flow references non-existent source task '${flow.from}'`);
    }
    if (!this._tasks.has(flow.to)) {
      errors.push(`Flow references non-existent target task '${flow.to}'`);
    }

    // Check for self-loops (usually invalid in YAWL)
    if (flow.from === flow.to) {
      warnings.push(`Flow from '${flow.from}' to itself detected (self-loop)`);
    }
  }

  // Check for duplicate flows
  const flowSet = new Set();
  for (const flow of this._flows) {
    const key = `${flow.from}->${flow.to}`;
    if (flowSet.has(key)) {
      warnings.push(`Duplicate flow from '${flow.from}' to '${flow.to}'`);
    }
    flowSet.add(key);
  }
}

/**
 * Validate split/join type consistency
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateSplitJoinConsistency(errors, warnings) {
  // Track split-join pairs for matching
  const splitJoinPairs = new Map();

  for (const [taskId, task] of this._tasks) {
    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    const incoming = this._incomingFlows.get(taskId) ?? [];

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

  // Check matching split-join patterns (AND-AND, XOR-XOR, OR-OR)
  this._validateMatchingSplitJoin(errors, warnings);
}

/**
 * Validate matching split-join patterns
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateMatchingSplitJoin(errors, warnings) {
  // Find all split points and their corresponding join points
  for (const [taskId, task] of this._tasks) {
    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    if (
      splitType === SPLIT_TYPE.AND ||
      splitType === SPLIT_TYPE.XOR ||
      splitType === SPLIT_TYPE.OR
    ) {
      // Find the convergence point
      const convergence = this._findConvergencePoint(taskId);
      if (convergence) {
        const joinTask = this._tasks.get(convergence);
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
 * @param {string} splitTaskId - Split task ID
 * @returns {string|null} Convergence task ID or null
 * @private
 */
export function _findConvergencePoint(splitTaskId) {
  const outgoing = this._outgoingFlows.get(splitTaskId) ?? [];
  if (outgoing.length < 2) return null;

  // Simple heuristic: find first common descendant
  const visited = new Map();
  const queue = outgoing.map(f => ({ taskId: f.to, path: new Set([splitTaskId, f.to]) }));

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

    const nextFlows = this._outgoingFlows.get(taskId) ?? [];
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
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateReachability(errors, warnings) {
  if (!this._startTaskId) return;

  const visited = new Set();
  const queue = [this._startTaskId];

  while (queue.length > 0) {
    const taskId = queue.shift();
    if (visited.has(taskId)) continue;
    visited.add(taskId);

    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) {
      if (!visited.has(flow.to)) {
        queue.push(flow.to);
      }
    }
  }

  // Check for unreachable tasks
  for (const [taskId] of this._tasks) {
    if (!visited.has(taskId)) {
      errors.push(`Task '${taskId}' is not reachable from start task`);
    }
  }
}

/**
 * Validate cancellation regions
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateCancellationRegions(errors, warnings) {
  for (const [regionId, taskIds] of this._regionToTasks) {
    // All tasks in region must exist
    for (const taskId of taskIds) {
      if (!this._tasks.has(taskId)) {
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
  for (const [taskId, task] of this._tasks) {
    if (task.cancellationSet) {
      for (const cancelTaskId of task.cancellationSet) {
        if (!this._tasks.has(cancelTaskId)) {
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
 * @param {string[]} errors - Error array to populate
 * @param {string[]} warnings - Warning array to populate
 * @private
 */
export function _validateNoCycles(errors, warnings) {
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

    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) {
      hasCycle(flow.to, path);
    }

    path.pop();
    recStack.delete(taskId);
    return false;
  };

  for (const [taskId] of this._tasks) {
    if (!visited.has(taskId)) {
      hasCycle(taskId, []);
    }
  }

  // In YAWL, some cycles are valid (loops), but we warn about them
  for (const cycle of cycles) {
    warnings.push(`Cycle detected: ${cycle.join(' -> ')}`);
  }
}
