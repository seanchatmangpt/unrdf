/**
 * @file YAWL Workflow API - Task Execution and Control Flow
 * @module @unrdf/yawl/api/workflow-api-execution
 *
 * @description
 * Implements task lifecycle management (enable, start, complete, cancel) and
 * control flow evaluation. All state transitions create cryptographic receipts.
 */

import {
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,
  WorkItemSchema,
  EnableTaskOptionsSchema,
  now,
  toISO,
  createReceipt,
} from './workflow-api-validation.mjs';

// ============================================================================
// Task Lifecycle Functions
// ============================================================================

/**
 * Enable a work item, transitioning it to enabled state.
 * Checks resource eligibility via policy packs before enabling.
 * @param {Object} workItem - Work item to enable
 * @param {Object} [options] - Enable options
 * @returns {Promise<Object>} Receipt with justification
 */
export async function enableTask(workItem, options = {}) {
  // Validate work item
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Validate options
  const validOptions = EnableTaskOptionsSchema.parse(options);

  // Check current status
  if (validWorkItem.status !== WORK_ITEM_STATUS.PENDING) {
    throw new Error(
      `Cannot enable work item ${validWorkItem.id}: status is ${validWorkItem.status}, expected ${WORK_ITEM_STATUS.PENDING}`
    );
  }

  const t_ns = now();

  // Check resource eligibility via policy pack if provided
  let eligibilityResult = { eligible: true, reasons: [] };
  let hookResults = [];

  if (validOptions?.policyPack && validOptions.policyPack.getHooks) {
    const hooks = validOptions.policyPack.getHooks();
    for (const hook of hooks) {
      if (hook.validate) {
        try {
          const result = hook.validate({
            workItem: validWorkItem,
            resource: validOptions.assignTo,
          });
          hookResults.push({
            hookName: hook.name || hook.meta?.name,
            passed: result,
          });
          if (!result) {
            eligibilityResult.eligible = false;
            eligibilityResult.reasons.push(
              `Hook ${hook.name || 'unknown'} rejected eligibility`
            );
          }
        } catch (error) {
          hookResults.push({
            hookName: hook.name || hook.meta?.name,
            passed: false,
            error: error.message,
          });
          eligibilityResult.eligible = false;
          eligibilityResult.reasons.push(error.message);
        }
      }
    }
  }

  // Fail if not eligible
  if (!eligibilityResult.eligible) {
    throw new Error(
      `Resource eligibility check failed: ${eligibilityResult.reasons.join(', ')}`
    );
  }

  // Update work item status (mutate original for in-place updates)
  workItem.status = WORK_ITEM_STATUS.ENABLED;
  workItem.assignedResource = validOptions?.assignTo;
  if (validOptions?.priority !== undefined) {
    workItem.priority = validOptions.priority;
  }

  const receipt = await createReceipt(YAWL_EVENT_TYPES.TASK_ENABLED, {
    workItemId: workItem.id, taskId: workItem.taskId, caseId: workItem.caseId,
    assignedResource: validOptions?.assignTo
  }, {
    policyPackId: validOptions?.policyPack?.manifest?.id, hookResults,
    resourceEligibility: eligibilityResult.eligible, conditionsMet: ['resource_eligibility']
  });

  return { workItem, receipt, eligibilityResult };
}

/**
 * Start a work item, transitioning it from enabled to active state.
 * @param {Object} workItem - Work item to start
 * @param {Object} [options] - Start options
 * @returns {Promise<Object>} Receipt with work item
 */
export async function startTask(workItem, options = {}) {
  // Validate work item (creates a validated copy for checks)
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Check current status
  if (validWorkItem.status !== WORK_ITEM_STATUS.ENABLED) {
    throw new Error(
      `Cannot start work item ${validWorkItem.id}: status is ${validWorkItem.status}, expected ${WORK_ITEM_STATUS.ENABLED}`
    );
  }

  const t_ns = now();
  const startTime = toISO(t_ns);

  // Execute pre-execution hooks if registry provided
  let hookResults = [];
  if (options.hookRegistry) {
    const preHooks = options.hookRegistry.getHooksByTrigger?.('pre-execution') || [];
    for (const hook of preHooks) {
      try {
        if (hook.execute) {
          const result = await hook.execute({
            workItem: validWorkItem,
            trigger: 'pre-execution',
          });
          hookResults.push({
            hookName: hook.name || hook.meta?.name,
            result,
          });
        }
      } catch (error) {
        hookResults.push({
          hookName: hook.name || hook.meta?.name,
          error: error.message,
        });
      }
    }
  }

  // Update work item status (mutate original for in-place updates)
  workItem.status = WORK_ITEM_STATUS.ACTIVE;
  workItem.startTime = startTime;

  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent({
      type: YAWL_EVENT_TYPES.TASK_STARTED,
      payload: { workItemId: validWorkItem.id, taskId: validWorkItem.taskId, caseId: validWorkItem.caseId, startTime }
    }, []);
    eventReceipt = receipt;
  }

  const receipt = await createReceipt(YAWL_EVENT_TYPES.TASK_STARTED, {
    workItemId: workItem.id, taskId: workItem.taskId, caseId: workItem.caseId,
    startTime, eventId: eventReceipt?.id
  }, { hookResults });

  return { workItem, receipt };
}

/**
 * Complete a work item, transitioning it from active to completed state.
 * @param {Object} workItem - Work item to complete
 * @param {unknown} result - Task result data
 * @param {Object} [options] - Complete options
 * @returns {Promise<Object>} Receipt with downstream enabled tasks
 */
export async function completeTask(workItem, result, options = {}) {
  // Validate work item (creates a validated copy for checks)
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Check current status
  if (validWorkItem.status !== WORK_ITEM_STATUS.ACTIVE) {
    throw new Error(
      `Cannot complete work item ${validWorkItem.id}: status is ${validWorkItem.status}, expected ${WORK_ITEM_STATUS.ACTIVE}`
    );
  }

  const t_ns = now();
  const endTime = toISO(t_ns);

  // Update work item status (mutate original for in-place updates)
  workItem.status = WORK_ITEM_STATUS.COMPLETED;
  workItem.endTime = endTime;
  workItem.result = result;

  // Evaluate control flow to find downstream tasks
  let enabledDownstreamTasks = [];
  if (options.caseObj && options.workflow) {
    enabledDownstreamTasks = await evaluateControlFlowAndEnable(
      workItem,
      result,
      options.caseObj,
      options.workflow
    );
  }

  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent({
      type: YAWL_EVENT_TYPES.TASK_COMPLETED,
      payload: { workItemId: workItem.id, taskId: workItem.taskId, caseId: workItem.caseId,
        endTime, result, enabledDownstreamTasks: enabledDownstreamTasks.map((t) => t.taskId) }
    }, []);
    eventReceipt = receipt;
  }

  const receipt = await createReceipt(YAWL_EVENT_TYPES.TASK_COMPLETED, {
    workItemId: workItem.id, taskId: workItem.taskId, caseId: workItem.caseId, endTime,
    enabledDownstreamTasks: enabledDownstreamTasks.map((t) => t.taskId), eventId: eventReceipt?.id
  }, { conditionsMet: enabledDownstreamTasks.map((t) => `flow_to_${t.taskId}`) });

  return { workItem, enabledDownstreamTasks, receipt };
}

/**
 * Cancel a work item, aborting it with a reason.
 * @param {Object} workItem - Work item to cancel
 * @param {string} reason - Reason for cancellation
 * @param {Object} [options] - Cancel options
 * @returns {Promise<Object>} Receipt with cancellation details
 */
export async function cancelWorkItem(workItem, reason, options = {}) {
  // Validate work item
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Validate reason
  if (!reason || typeof reason !== 'string') {
    throw new TypeError('Cancel reason must be a non-empty string');
  }

  if (validWorkItem.status === WORK_ITEM_STATUS.COMPLETED || validWorkItem.status === WORK_ITEM_STATUS.CANCELLED) {
    throw new Error(`Cannot cancel work item ${validWorkItem.id}: status is ${validWorkItem.status}`);
  }

  const t_ns = now();
  const cancelTime = toISO(t_ns);

  // Update work item status (mutate original for in-place updates)
  const previousStatus = workItem.status;
  workItem.status = WORK_ITEM_STATUS.CANCELLED;
  workItem.endTime = cancelTime;
  workItem.result = { cancelled: true, reason };

  // Handle cancellation region - cancel all tasks in the region
  let cancelledInRegion = [];
  if (options.workflow && options.caseObj) {
    // Find cancellation region for this task
    const task = options.workflow.getTask(workItem.taskId);
    if (task?.cancellationRegion) {
      const regionTasks = options.workflow.getCancellationRegion(
        task.cancellationRegion
      );
      for (const taskId of regionTasks) {
        if (taskId !== workItem.taskId) {
          const regionWorkItem = options.caseObj.getWorkItem(taskId);
          if (regionWorkItem && regionWorkItem.status !== WORK_ITEM_STATUS.COMPLETED &&
              regionWorkItem.status !== WORK_ITEM_STATUS.CANCELLED) {
            regionWorkItem.status = WORK_ITEM_STATUS.CANCELLED;
            regionWorkItem.endTime = cancelTime;
            regionWorkItem.result = { cancelled: true, reason: `Cancelled by region trigger: ${reason}` };
            cancelledInRegion.push(taskId);
          }
        }
      }
    }
  }

  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent({
      type: YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED,
      payload: { workItemId: workItem.id, taskId: workItem.taskId, caseId: workItem.caseId,
        previousStatus, reason, cancelledInRegion, cancelTime }
    }, []);
    eventReceipt = receipt;
  }

  const receipt = await createReceipt(YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED, {
    workItemId: workItem.id, taskId: workItem.taskId, caseId: workItem.caseId,
    previousStatus, reason, cancelledInRegion, cancelTime, eventId: eventReceipt?.id
  });

  return { workItem, cancelledInRegion, receipt };
}

// ============================================================================
// Control Flow Evaluation Helpers
// ============================================================================

/**
 * Evaluate control flow conditions and enable downstream tasks
 * @param {Object} workItem - Completed work item
 * @param {unknown} result - Task result
 * @param {Object} caseObj - Case object
 * @param {Object} workflow - Workflow object
 * @returns {Promise<Array<Object>>} Enabled downstream work items
 */
export async function evaluateControlFlowAndEnable(workItem, result, caseObj, workflow) {
  const enabledWorkItems = [];
  const edges = workflow.controlFlowGraph.outgoing.get(workItem.taskId) || [];

  for (const edge of edges) {
    // Evaluate condition if present
    let conditionMet = true;
    if (edge.condition) {
      conditionMet = evaluateCondition(edge.condition, result, caseObj.variables);
    }

    if (conditionMet) {
      const downstreamWorkItem = caseObj.getWorkItem(edge.to);
      if (downstreamWorkItem) {
        // Check if all join conditions are met (for AND-join)
        if (edge.type === CONTROL_FLOW_PATTERNS.AND_JOIN) {
          const allPredecessorsComplete = checkAllPredecessorsComplete(
            edge.to,
            workflow,
            caseObj
          );
          if (!allPredecessorsComplete) {
            continue;
          }
        }

        // Enable the downstream work item
        if (downstreamWorkItem.status === WORK_ITEM_STATUS.PENDING) {
          downstreamWorkItem.status = WORK_ITEM_STATUS.ENABLED;
          enabledWorkItems.push(downstreamWorkItem);
        }
      }
    }
  }

  return enabledWorkItems;
}

/**
 * Evaluate a condition expression
 * @param {string} condition - Condition expression
 * @param {unknown} result - Task result
 * @param {Object} variables - Case variables
 * @returns {boolean} Condition result
 */
export function evaluateCondition(condition, result, variables) {
  // Simple condition evaluation - supports basic operators
  // In production, would use a proper expression evaluator
  try {
    // Create a safe evaluation context
    const context = {
      result,
      ...variables,
    };

    // Very basic condition parsing - supports: ==, !=, >, <, >=, <=, &&, ||
    // Replace variable references with context values
    let evalCondition = condition;
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`\\b${key}\\b`, 'g');
      if (typeof value === 'string') {
        evalCondition = evalCondition.replace(regex, `"${value}"`);
      } else if (value === null || value === undefined) {
        evalCondition = evalCondition.replace(regex, 'null');
      } else if (typeof value === 'object') {
        evalCondition = evalCondition.replace(regex, JSON.stringify(value));
      } else {
        evalCondition = evalCondition.replace(regex, String(value));
      }
    }

    // Simple evaluation for common patterns
    if (evalCondition === 'true') return true;
    if (evalCondition === 'false') return false;

    // For safety, only evaluate if it matches safe patterns
    const safePattern =
      /^[\s\d\w"'.\-+*/%<>=!&|()[\],{}:]+$/;
    if (!safePattern.test(evalCondition)) {
      console.warn(`Unsafe condition pattern: ${condition}`);
      return true; // Default to true for unsafe patterns
    }

    // Use Function constructor for sandboxed evaluation
     
    const evaluator = new Function('return ' + evalCondition);
    return Boolean(evaluator());
  } catch {
    // Default to true on evaluation error
    return true;
  }
}

/**
 * Check if all predecessors of a task are complete (for AND-join)
 * @param {string} taskId - Task to check
 * @param {Object} workflow - Workflow object
 * @param {Object} caseObj - Case object
 * @returns {boolean} True if all predecessors complete
 */
export function checkAllPredecessorsComplete(taskId, workflow, caseObj) {
  const incomingEdges = workflow.controlFlowGraph.incoming.get(taskId) || [];

  for (const edge of incomingEdges) {
    const predecessorWorkItem = caseObj.getWorkItem(edge.from);
    if (
      predecessorWorkItem &&
      predecessorWorkItem.status !== WORK_ITEM_STATUS.COMPLETED
    ) {
      return false;
    }
  }

  return true;
}
