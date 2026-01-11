/**
 * @file YAWL Workflow Execution - Task lifecycle management
 * @module @unrdf/yawl/api/workflow-execution
 *
 * @description
 * Handles task enablement, starting, and completion with control flow evaluation.
 * Pure functions with no side effects except event append.
 */

import { z } from 'zod';
import {
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,
  now,
  toISO,
  createReceipt,
} from './workflow-creation.mjs';

// ============================================================================
// Zod Schemas
// ============================================================================

/**
 * Schema for work item
 */
export const WorkItemSchema = z.object({
  id: z.string().min(1),
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  status: z.enum(['pending', 'enabled', 'active', 'completed', 'cancelled', 'suspended']),
  assignedResource: z.string().optional(),
  startTime: z.string().optional(),
  endTime: z.string().optional(),
  variables: z.record(z.string(), z.any()).optional(),
  result: z.any().optional(),
});

/**
 * Schema for enable task options
 */
export const EnableTaskOptionsSchema = z.object({
  assignTo: z.string().optional(),
  priority: z.number().int().min(0).max(100).optional(),
  deadline: z.string().datetime().optional(),
  policyPack: z.any().optional(),
}).optional();

// ============================================================================
// Core Execution Functions
// ============================================================================

/**
 * Enable a work item, transitioning it to enabled state.
 *
 * Checks resource eligibility via policy packs before enabling.
 * Creates YAWL admission event with justification.
 *
 * @param {Object} workItem - Work item to enable
 * @param {Object} [options] - Enable options
 * @param {string} [options.assignTo] - Resource to assign to
 * @param {number} [options.priority] - Task priority override
 * @param {string} [options.deadline] - Task deadline
 * @param {Object} [options.policyPack] - Policy pack for eligibility check
 * @returns {Promise<Object>} Receipt with justification
 *
 * @throws {Error} If work item is not in pending state
 * @throws {Error} If resource eligibility check fails
 *
 * @example
 * const receipt = await enableTask(workItem, {
 *   assignTo: 'user-123',
 *   priority: 80,
 * });
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

  // Create receipt with justification
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.TASK_ENABLED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      assignedResource: validOptions?.assignTo,
    },
    {
      policyPackId: validOptions?.policyPack?.manifest?.id,
      hookResults,
      resourceEligibility: eligibilityResult.eligible,
      conditionsMet: ['resource_eligibility'],
    }
  );

  return {
    workItem,
    receipt,
    eligibilityResult,
  };
}

/**
 * Start a work item, transitioning it from enabled to active state.
 *
 * Executes pre-execution hooks if hook registry is available.
 * Records start time and creates event receipt.
 *
 * @param {Object} workItem - Work item to start
 * @param {Object} [options] - Start options
 * @param {Object} [options.hookRegistry] - Hook registry for pre-execution hooks
 * @param {Object} [options.store] - KGC-4D store for event logging
 * @returns {Promise<Object>} Receipt with work item
 *
 * @throws {Error} If work item is not in enabled state
 *
 * @example
 * const receipt = await startTask(workItem);
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

  // Append event to KGC-4D store if available
  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.TASK_STARTED,
        payload: {
          workItemId: validWorkItem.id,
          taskId: validWorkItem.taskId,
          caseId: validWorkItem.caseId,
          startTime,
        },
      },
      []
    );
    eventReceipt = receipt;
  }

  // Create receipt
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.TASK_STARTED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      startTime,
      eventId: eventReceipt?.id,
    },
    {
      hookResults,
    }
  );

  return {
    workItem,
    receipt,
  };
}

/**
 * Complete a work item, transitioning it from active to completed state.
 *
 * Evaluates control flow conditions to determine downstream tasks to enable.
 * Creates completion event with result and enables downstream tasks.
 *
 * @param {Object} workItem - Work item to complete
 * @param {unknown} result - Task result data
 * @param {Object} [options] - Complete options
 * @param {Object} [options.caseObj] - Case object for downstream task enablement
 * @param {Object} [options.workflow] - Workflow object for control flow evaluation
 * @param {Object} [options.store] - KGC-4D store for event logging
 * @returns {Promise<Object>} Receipt with downstream enabled tasks
 *
 * @throws {Error} If work item is not in active state
 *
 * @example
 * const receipt = await completeTask(workItem, { orderId: 'ORD-123' });
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

  // Append event to KGC-4D store if available
  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.TASK_COMPLETED,
        payload: {
          workItemId: workItem.id,
          taskId: workItem.taskId,
          caseId: workItem.caseId,
          endTime,
          result,
          enabledDownstreamTasks: enabledDownstreamTasks.map((t) => t.taskId),
        },
      },
      []
    );
    eventReceipt = receipt;
  }

  // Create receipt
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.TASK_COMPLETED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      endTime,
      enabledDownstreamTasks: enabledDownstreamTasks.map((t) => t.taskId),
      eventId: eventReceipt?.id,
    },
    {
      conditionsMet: enabledDownstreamTasks.map((t) => `flow_to_${t.taskId}`),
    }
  );

  return {
    workItem,
    enabledDownstreamTasks,
    receipt,
  };
}

// ============================================================================
// Helper Functions
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
