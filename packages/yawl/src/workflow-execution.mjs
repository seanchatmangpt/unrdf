/**
 * @file YAWL Workflow Execution - Validation and control flow evaluation
 * @module @unrdf/yawl/workflow-execution
 *
 * @description
 * Validation logic and control flow evaluation for YAWL workflows.
 * Methods are added to Workflow.prototype to access private properties.
 *
 * @example
 * import { createWorkflow } from '@unrdf/yawl/workflow-execution';
 *
 * const workflow = createWorkflow({
 *   id: 'expense-approval',
 *   tasks: [...],
 *   flows: [...],
 * }, { validate: true, strict: true });
 */

import { Workflow } from './workflow-core.mjs';
import { SPLIT_TYPE, JOIN_TYPE } from './patterns.mjs';

// =============================================================================
// Validation Methods (added to Workflow.prototype)
// =============================================================================

/**
 * Validate workflow structure and integrity
 * @returns {ValidationResult} Validation result with errors and warnings
 */
Workflow.prototype.validate = function() {
  const errors = [];
  const warnings = [];
  this._validateBasicStructure(errors, warnings);
  this._validateControlFlowIntegrity(errors, warnings);
  this._validateSplitJoinConsistency(errors, warnings);
  this._validateReachability(errors, warnings);
  this._validateCancellationRegions(errors, warnings);
  this._validateNoCycles(errors, warnings);
  return { valid: errors.length === 0, errors, warnings };
};

/** Quick check if workflow is valid */
Workflow.prototype.isValid = function() { return this.validate().valid; };

/** Validate basic structure */
Workflow.prototype._validateBasicStructure = function(errors, warnings) {
  if (this._tasks.size === 0) errors.push('Workflow has no tasks');
  if (!this._startTaskId) errors.push('Workflow has no start task');
  else if (!this._tasks.has(this._startTaskId)) errors.push(`Start task '${this._startTaskId}' not found in workflow`);
  if (this._endTaskIds.length === 0) warnings.push('Workflow has no designated end tasks');
  for (const endTaskId of this._endTaskIds) {
    if (!this._tasks.has(endTaskId)) errors.push(`End task '${endTaskId}' not found in workflow`);
  }
};

/** Validate control flow integrity */
Workflow.prototype._validateControlFlowIntegrity = function(errors, warnings) {
  for (const flow of this._flows) {
    if (!this._tasks.has(flow.from)) errors.push(`Flow references non-existent source task '${flow.from}'`);
    if (!this._tasks.has(flow.to)) errors.push(`Flow references non-existent target task '${flow.to}'`);
    if (flow.from === flow.to) warnings.push(`Flow from '${flow.from}' to itself detected (self-loop)`);
  }
  const flowSet = new Set();
  for (const flow of this._flows) {
    const key = `${flow.from}->${flow.to}`;
    if (flowSet.has(key)) warnings.push(`Duplicate flow from '${flow.from}' to '${flow.to}'`);
    flowSet.add(key);
  }
};

/** Validate split/join type consistency */
Workflow.prototype._validateSplitJoinConsistency = function(errors, warnings) {
  for (const [taskId, task] of this._tasks) {
    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    const incoming = this._incomingFlows.get(taskId) ?? [];
    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    const joinType = task.joinType ?? JOIN_TYPE.SEQUENCE;
    if (splitType === SPLIT_TYPE.SEQUENCE && outgoing.length > 1) {
      errors.push(`Task '${taskId}' has sequence split but ${outgoing.length} outgoing flows`);
    }
    if ((splitType === SPLIT_TYPE.AND || splitType === SPLIT_TYPE.XOR || splitType === SPLIT_TYPE.OR) && outgoing.length < 2) {
      warnings.push(`Task '${taskId}' has ${splitType} split but only ${outgoing.length} outgoing flow(s)`);
    }
    if (joinType === JOIN_TYPE.SEQUENCE && incoming.length > 1) {
      errors.push(`Task '${taskId}' has sequence join but ${incoming.length} incoming flows`);
    }
    if ((joinType === JOIN_TYPE.AND || joinType === JOIN_TYPE.XOR || joinType === JOIN_TYPE.OR) && incoming.length < 2) {
      warnings.push(`Task '${taskId}' has ${joinType} join but only ${incoming.length} incoming flow(s)`);
    }
    if (splitType === SPLIT_TYPE.XOR && outgoing.length > 1) {
      const hasConditions = outgoing.some(f => f.condition || f.isDefault);
      if (!hasConditions) warnings.push(`Task '${taskId}' has XOR split but no conditions or default flow defined`);
    }
  }
  this._validateMatchingSplitJoin(errors, warnings);
};

/** Validate matching split-join patterns */
Workflow.prototype._validateMatchingSplitJoin = function(errors, warnings) {
  for (const [taskId, task] of this._tasks) {
    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    if (splitType === SPLIT_TYPE.AND || splitType === SPLIT_TYPE.XOR || splitType === SPLIT_TYPE.OR) {
      const convergence = this._findConvergencePoint(taskId);
      if (convergence) {
        const joinTask = this._tasks.get(convergence);
        if (joinTask) {
          const joinType = joinTask.joinType ?? JOIN_TYPE.SEQUENCE;
          if (splitType === SPLIT_TYPE.AND && joinType !== JOIN_TYPE.AND) {
            warnings.push(`AND-split at '${taskId}' converges at '${convergence}' with ${joinType}-join (expected AND-join)`);
          }
          if (splitType === SPLIT_TYPE.OR && joinType !== JOIN_TYPE.OR) {
            warnings.push(`OR-split at '${taskId}' converges at '${convergence}' with ${joinType}-join (expected OR-join)`);
          }
        }
      }
    }
  }
};

/**
 * Find the convergence point for a split
 * @param {string} splitTaskId - Split task ID
 * @returns {string|null} Convergence task ID or null
 * @private
 */
Workflow.prototype._findConvergencePoint = function(splitTaskId) {
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
};

/** Validate all tasks are reachable from start */
Workflow.prototype._validateReachability = function(errors, warnings) {
  if (!this._startTaskId) return;
  const visited = new Set();
  const queue = [this._startTaskId];
  while (queue.length > 0) {
    const taskId = queue.shift();
    if (visited.has(taskId)) continue;
    visited.add(taskId);
    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) {
      if (!visited.has(flow.to)) queue.push(flow.to);
    }
  }
  for (const [taskId] of this._tasks) {
    if (!visited.has(taskId)) errors.push(`Task '${taskId}' is not reachable from start task`);
  }
};

/** Validate cancellation regions */
Workflow.prototype._validateCancellationRegions = function(errors, warnings) {
  for (const [regionId, taskIds] of this._regionToTasks) {
    for (const taskId of taskIds) {
      if (!this._tasks.has(taskId)) errors.push(`Cancellation region '${regionId}' references non-existent task '${taskId}'`);
    }
    if (taskIds.length < 2) warnings.push(`Cancellation region '${regionId}' has only ${taskIds.length} task(s)`);
  }
  for (const [taskId, task] of this._tasks) {
    if (task.cancellationSet) {
      for (const cancelTaskId of task.cancellationSet) {
        if (!this._tasks.has(cancelTaskId)) errors.push(`Task '${taskId}' cancellation set references non-existent task '${cancelTaskId}'`);
      }
    }
  }
};

/** Validate no invalid cycles exist */
Workflow.prototype._validateNoCycles = function(errors, warnings) {
  const visited = new Set();
  const recStack = new Set();
  const cycles = [];
  const hasCycle = (taskId, path = []) => {
    if (recStack.has(taskId)) {
      const cycleStart = path.indexOf(taskId);
      cycles.push(path.slice(cycleStart).concat(taskId));
      return true;
    }
    if (visited.has(taskId)) return false;
    visited.add(taskId);
    recStack.add(taskId);
    path.push(taskId);
    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) hasCycle(flow.to, path);
    path.pop();
    recStack.delete(taskId);
    return false;
  };
  for (const [taskId] of this._tasks) {
    if (!visited.has(taskId)) hasCycle(taskId, []);
  }
  for (const cycle of cycles) warnings.push(`Cycle detected: ${cycle.join(' -> ')}`);
};

// =============================================================================
// Control Flow Evaluation Methods (added to Workflow.prototype)
// =============================================================================

/**
 * Evaluate which downstream tasks should be enabled after task completion
 * @param {string} completedTaskId - Task that completed
 * @param {Object} [context={}] - Evaluation context (data, case state)
 * @returns {string[]} Task IDs to enable
 *
 * @example
 * const toEnable = workflow.evaluateDownstream('review', { amount: 500 });
 * // Returns ['approve'] if amount < 1000
 */
Workflow.prototype.evaluateDownstream = function(completedTaskId, context = {}) {
  const taskDef = this._tasks.get(completedTaskId);
  if (!taskDef) return [];

  const outFlows = this._outgoingFlows.get(completedTaskId) ?? [];
  if (outFlows.length === 0) return [];

  const splitType = taskDef.splitType ?? SPLIT_TYPE.SEQUENCE;
  const toEnable = [];

  // Sort flows by priority (higher first)
  const sortedFlows = [...outFlows].sort(
    (a, b) => (b.priority ?? 0) - (a.priority ?? 0)
  );

  switch (splitType) {
    case SPLIT_TYPE.SEQUENCE:
      // Enable the single outgoing task
      if (sortedFlows.length > 0) {
        toEnable.push(sortedFlows[0].to);
      }
      break;

    case SPLIT_TYPE.AND:
      // Enable all outgoing tasks (parallel split)
      for (const flow of sortedFlows) {
        toEnable.push(flow.to);
      }
      break;

    case SPLIT_TYPE.XOR:
      // Enable first matching condition (exclusive choice)
      for (const flow of sortedFlows) {
        if (!flow.condition) {
          if (flow.isDefault) {
            // Save default for fallback
            continue;
          }
          toEnable.push(flow.to);
          break;
        }
        try {
          if (flow.condition(context)) {
            toEnable.push(flow.to);
            break;
          }
        } catch {
          // Skip flow on condition error
        }
      }
      // Use default flow if no match
      if (toEnable.length === 0) {
        const defaultFlow = sortedFlows.find(f => f.isDefault);
        if (defaultFlow) {
          toEnable.push(defaultFlow.to);
        } else if (sortedFlows.length > 0) {
          // Fallback to last flow if no default
          toEnable.push(sortedFlows[sortedFlows.length - 1].to);
        }
      }
      break;

    case SPLIT_TYPE.OR:
      // Enable all matching conditions (multi-choice)
      for (const flow of sortedFlows) {
        if (!flow.condition) {
          toEnable.push(flow.to);
        } else {
          try {
            if (flow.condition(context)) {
              toEnable.push(flow.to);
            }
          } catch {
            // Skip flow on condition error
          }
        }
      }
      // Must enable at least one
      if (toEnable.length === 0 && sortedFlows.length > 0) {
        toEnable.push(sortedFlows[0].to);
      }
      break;

    default:
      // Unknown split type - enable all
      for (const flow of sortedFlows) {
        toEnable.push(flow.to);
      }
  }

  return toEnable;
};

/**
 * Check if a task can be enabled based on join semantics
 * @param {string} taskId - Task to check
 * @param {Set<string>} completedTasks - Set of completed task IDs
 * @param {Set<string>} [activatedTasks=new Set()] - Set of tasks activated by OR-split
 * @returns {boolean} True if task can be enabled
 *
 * @example
 * const completed = new Set(['task-a', 'task-b']);
 * if (workflow.canEnable('merge-task', completed)) {
 *   // Enable the merge task
 * }
 */
Workflow.prototype.canEnable = function(taskId, completedTasks, activatedTasks = new Set()) {
  const taskDef = this._tasks.get(taskId);
  if (!taskDef) return false;

  const inFlows = this._incomingFlows.get(taskId) ?? [];
  if (inFlows.length === 0) return true; // Start task

  const joinType = taskDef.joinType ?? JOIN_TYPE.SEQUENCE;
  const incomingTaskIds = inFlows.map(f => f.from);

  switch (joinType) {
    case JOIN_TYPE.SEQUENCE:
      // Single incoming must be complete
      return incomingTaskIds.some(id => completedTasks.has(id));

    case JOIN_TYPE.AND:
      // All incoming must be complete (synchronization)
      return incomingTaskIds.every(id => completedTasks.has(id));

    case JOIN_TYPE.XOR:
      // Any one incoming complete (simple merge)
      return incomingTaskIds.some(id => completedTasks.has(id));

    case JOIN_TYPE.OR:
      // All ACTIVATED incoming must be complete (structured sync merge)
      const activated = incomingTaskIds.filter(id => activatedTasks.has(id));
      if (activated.length === 0) {
        // If none activated, any one complete suffices
        return incomingTaskIds.some(id => completedTasks.has(id));
      }
      return activated.every(id => completedTasks.has(id));

    default:
      return incomingTaskIds.some(id => completedTasks.has(id));
  }
};

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
// Module Exports
// =============================================================================

export default {
  createWorkflow,
};
