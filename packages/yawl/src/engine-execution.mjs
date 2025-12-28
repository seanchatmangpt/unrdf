/**
 * @file YAWL Engine Execution - Task execution and work item management
 * @module @unrdf/yawl/engine-execution
 *
 * @description
 * Task execution functionality:
 * - Task enabling and starting
 * - Task completion and cancellation
 * - Timeout handling
 * - Circuit breaker integration
 * - Downstream task propagation
 */

import { now, toISO } from '@unrdf/kgc-4d';
import { TaskStatus, TaskStatus_RUNNING } from './task.mjs';
import { CaseStatus } from './case.mjs';
import { buildReceipt } from './receipt.mjs';
import { ENGINE_EVENTS } from './engine-constants.mjs';
import { appendEvent } from './engine-core.mjs';
import { EngineError } from '../errors.mjs';
import {
  isCircuitOpen,
  recordCircuitFailure,
  resetCircuitBreaker,
  logTaskEvent,
} from './engine-hooks.mjs';

// =============================================================================
// Task Execution
// =============================================================================

export async function enableTask(engine, caseId, taskId, actor) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');

  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  // Check circuit breaker
  const breakerKey = `${yawlCase.workflowId}:${taskId}`;
  if (isCircuitOpen(engine, breakerKey)) {
    throw new Error(`Circuit breaker open for task ${taskId}`);
  }

  // Run pre-enablement hook if policy pack exists
  const policyPack = engine._policyPacks.get(yawlCase.workflowId);
  if (policyPack && policyPack.getValidator) {
    const validator = policyPack.getValidator(taskId);
    if (validator) {
      const validation = await validator(engine.store, { caseId, actor });
      if (!validation.valid) {
        throw new Error(`Task enablement denied: ${validation.receipt?.justification?.reason || 'Unknown'}`);
      }
    }
  }

  const result = await yawlCase.enableTask(taskId, actor);

  appendEvent(engine, {
    type: 'TASK_ENABLED',
    caseId,
    taskId,
    workItemId: result.task.id,
    actor,
  });

  if (engine.enableEventLog) {
    await logTaskEvent(engine, YAWL_EVENT_TYPES.TASK_ENABLED, {
      caseId,
      taskId,
      workItemId: result.task.id,
      enabledAt: toISO(result.task.enabledAt),
    });
  }

  engine.emit(ENGINE_EVENTS.TASK_ENABLED, {
    caseId,
    taskId,
    workItemId: result.task.id,
    actor,
  });

  engine._stats.tasksEnabled++;

  return result;
}

export async function startTask(engine, caseId, workItemId, options = {}) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');

  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const task = yawlCase.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // Try to allocate resource if role specified
  let allocatedResource;
  if (task.role || options.resourceId) {
    const allocation = engine.resourcePool.allocate({
      taskId: workItemId,
      role: task.role,
      preferredResourceId: options.resourceId,
    });

    if (allocation.queued) {
      throw new Error(`No available resources for role ${task.role}`);
    }

    allocatedResource = allocation.resource;

    engine.emit(ENGINE_EVENTS.RESOURCE_ALLOCATED, {
      caseId,
      workItemId,
      resourceId: allocatedResource.id,
      role: task.role,
    });
  }

  const result = await yawlCase.startTask(
    workItemId,
    allocatedResource?.id ?? options.resourceId,
    options.actor
  );

  appendEvent(engine, {
    type: 'TASK_STARTED',
    caseId,
    workItemId,
    resourceId: allocatedResource?.id ?? options.resourceId,
    actor: options.actor,
  });

  if (engine.enableEventLog) {
    await logTaskEvent(engine, YAWL_EVENT_TYPES.TASK_STARTED, {
      workItemId,
      startedAt: toISO(result.task.startedAt),
    }, caseId);
  }

  engine.emit(ENGINE_EVENTS.TASK_STARTED, {
    caseId,
    workItemId,
    resourceId: allocatedResource?.id ?? options.resourceId,
    actor: options.actor,
  });

  engine._stats.tasksStarted++;

  return { ...result, resource: allocatedResource };
}

export async function completeTask(engine, caseId, workItemId, output = {}, actor) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');

  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const task = yawlCase.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // Verify task is active
  if (task.status !== TaskStatus_RUNNING) {
    throw new Error(`Task ${workItemId} is not running (status: ${task.status})`);
  }

  // Release resource if allocated
  if (task.assignedResource) {
    const nextFromQueue = engine.resourcePool.release(task.assignedResource);

    engine.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
      caseId,
      workItemId,
      resourceId: task.assignedResource,
    });

    if (nextFromQueue) {
      appendEvent(engine, {
        type: 'RESOURCE_REALLOCATED',
        resourceId: nextFromQueue.resource.id,
        fromWorkItemId: workItemId,
        toTaskId: nextFromQueue.taskId,
      });
    }
  }

  // Run post-completion hook if policy pack exists
  const policyPack = engine._policyPacks.get(yawlCase.workflowId);
  const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

  let hookRouting = null;
  if (policyPack && policyPack.getRouter) {
    const router = policyPack.getRouter(taskDefId);
    if (router) {
      hookRouting = await router(engine.store, {
        caseId,
        actor,
        output,
        env: output,
      });
    }
  }

  const result = await yawlCase.completeTask(workItemId, output, actor);

  // Reset circuit breaker on success
  const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
  resetCircuitBreaker(engine, breakerKey);

  appendEvent(engine, {
    type: 'TASK_COMPLETED',
    caseId,
    workItemId,
    output,
    actor,
    downstreamEnabled: result.downstreamEnabled.map(d => d.taskId),
  });

  if (engine.enableEventLog) {
    await logTaskEvent(engine, YAWL_EVENT_TYPES.TASK_COMPLETED, {
      workItemId,
      completedAt: toISO(result.task.completedAt),
      result: output,
    }, caseId);
  }

  engine.emit(ENGINE_EVENTS.TASK_COMPLETED, {
    caseId,
    workItemId,
    taskId: taskDefId,
    output,
    actor,
    downstreamEnabled: result.downstreamEnabled,
    hookReceipt: hookRouting?.receipt,
  });

  engine._stats.tasksCompleted++;

  // Emit events for downstream enabled tasks
  for (const downstream of result.downstreamEnabled) {
    engine.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId: downstream.taskId,
      workItemId: downstream.workItemId,
    });
    engine._stats.tasksEnabled++;
  }

  // Check if case completed
  if (yawlCase.status === CaseStatus.COMPLETED) {
    appendEvent(engine, {
      type: 'CASE_COMPLETED',
      caseId,
    });

    engine.emit(ENGINE_EVENTS.CASE_COMPLETED, {
      caseId,
      workflowId: yawlCase.workflowId,
    });

    engine._stats.casesCompleted++;
  }

  return result;
}

export async function cancelTask(engine, caseId, workItemId, reason, actor) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');

  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const task = yawlCase.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // Release resource if allocated
  if (task.assignedResource) {
    engine.resourcePool.release(task.assignedResource);

    engine.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
      caseId,
      workItemId,
      resourceId: task.assignedResource,
    });
  }

  const result = await yawlCase.cancelTask(workItemId, reason, actor);

  // Handle cancellation propagation if policy pack exists
  const policyPack = engine._policyPacks.get(yawlCase.workflowId);
  const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

  if (policyPack && policyPack.getCancellationHandler) {
    const handler = policyPack.getCancellationHandler(taskDefId);
    if (handler) {
      handler(reason, { caseId, actor });
    }
  }

  appendEvent(engine, {
    type: 'TASK_CANCELLED',
    caseId,
    workItemId,
    reason,
    actor,
  });

  if (engine.enableEventLog) {
    await logTaskEvent(engine, YAWL_EVENT_TYPES.TASK_CANCELLED, {
      workItemId,
      cancelledAt: toISO(now()),
      reason: reason || 'No reason provided',
    }, caseId);
  }

  engine.emit(ENGINE_EVENTS.TASK_CANCELLED, {
    caseId,
    workItemId,
    taskId: taskDefId,
    reason,
    actor,
  });

  engine._stats.tasksCancelled++;

  return result;
}

export async function cancelRegion(engine, caseId, regionId, reason, actor) {
  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const result = await yawlCase.cancelRegion(regionId, reason, actor);

  appendEvent(engine, {
    type: 'REGION_CANCELLED',
    caseId,
    regionId,
    cancelledCount: result.cancelled.length,
    reason,
    actor,
  });

  for (const task of result.cancelled) {
    engine.emit(ENGINE_EVENTS.TASK_CANCELLED, {
      caseId,
      workItemId: task.id,
      reason: `Region ${regionId} cancelled: ${reason || 'No reason'}`,
      actor,
    });
    engine._stats.tasksCancelled++;
  }

  return result;
}

/**
 * Set circuit breaker state for a task
 * @param {Object} engine - Engine instance
 * @param {string} caseId - Case ID
 * @param {string} taskId - Task definition ID
 * @param {boolean} enabled - Enable or disable
 * @returns {Promise<{cancelled: Array}>}
 */
export async function setCircuitBreaker(engine, caseId, taskId, enabled) {
  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const result = await yawlCase.setCircuitBreaker(taskId, enabled);

  appendEvent(engine, {
    type: 'CIRCUIT_BREAKER_SET',
    caseId,
    taskId,
    enabled,
    cancelledCount: result.cancelled.length,
  });

  if (!enabled) {
    engine.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
      caseId,
      taskId,
      cancelledCount: result.cancelled.length,
    });
  } else {
    engine.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, {
      caseId,
      taskId,
    });
  }

  return result;
}

/**
 * Handle timeout for a work item
 * @param {Object} engine - Engine instance
 * @param {string} caseId - Case ID
 * @param {string} workItemId - Work item ID
 * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
 */
export async function timeoutTask(engine, caseId, workItemId) {
  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const task = yawlCase.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // Release resource if allocated
  if (task.assignedResource) {
    engine.resourcePool.release(task.assignedResource);
  }

  const beforeState = yawlCase.getState();
  task.timedOut();
  const afterState = yawlCase.getState();

  const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);
  const previousReceipt = yawlCase.receipts[yawlCase.receipts.length - 1];

  const receipt = await buildReceipt({
    caseId,
    taskId: taskDefId,
    action: 'timeout',
    beforeState,
    afterState,
    previousReceipt,
  });

  yawlCase.receipts.push(receipt);

  // Increment circuit breaker failure count
  const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
  recordCircuitFailure(engine, breakerKey);

  appendEvent(engine, {
    type: 'TASK_TIMEOUT',
    caseId,
    workItemId,
  });

  engine.emit(ENGINE_EVENTS.TASK_TIMEOUT, {
    caseId,
    workItemId,
    taskId: taskDefId,
  });

  engine._stats.tasksTimedOut++;

  return { task, receipt };
}
