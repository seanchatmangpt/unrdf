/**
 * @file YAWL Engine - Main workflow execution engine (modular version)
 * @module @unrdf/yawl/engine
 *
 * @description
 * Modular engine composed from subsystems:
 * - engine-core.mjs (core class, workflows, resources)
 * - engine-events.mjs (event subscription and emission)
 * - engine-hooks.mjs (policy pack integration)
 * - engine-health.mjs (health checks, circuit breakers)
 * - engine-snapshots.mjs (time-travel, checkpoints)
 * - engine-queries.mjs (case queries and search)
 */

import { toISO, now } from '@unrdf/kgc-4d';
import { randomUUID } from 'crypto';
import { YawlCase, CaseStatus } from './case.mjs';
import { TaskStatus } from './task.mjs';
import { buildReceipt } from './receipt.mjs';
import { YAWL_EVENT_TYPES } from './events/yawl-events.mjs';

// Import core and mixins
import {
  EngineCore,
  YAWL_NS,
  YAWL_GRAPHS,
  ENGINE_EVENTS,
  HealthStatus,
} from './engine-core.mjs';
import { withEvents } from './engine-events.mjs';
import { withHooks } from './engine-hooks.mjs';
import { withHealth } from './engine-health.mjs';
import { withSnapshots } from './engine-snapshots.mjs';
import { withQueries } from './engine-queries.mjs';

// =============================================================================
// Compose WorkflowEngine from Mixins
// =============================================================================

/**
 * Complete workflow engine with all capabilities
 *
 * Composed from:
 * - EngineCore (base class, configuration, workflows, resources)
 * - withEvents (event subscription, emission, logging)
 * - withHooks (policy packs, hook execution)
 * - withHealth (health checks, circuit breakers, statistics)
 * - withSnapshots (time-travel, checkpoints, replay)
 * - withQueries (case queries, search, aggregations)
 * - TaskExecution (this class - case and task lifecycle methods)
 */
class WorkflowEngine extends withQueries(
  withSnapshots(
    withHealth(
      withHooks(
        withEvents(EngineCore)
      )
    )
  )
) {
  // ===========================================================================
  // Case Management
  // ===========================================================================

  /**
   * Create a new case for a workflow
   * @param {string} workflowId - Workflow ID
   * @param {Object} [initialData={}] - Initial case data
   * @param {Object} [options={}] - Additional options
   * @returns {Promise<{case: YawlCase, receipt: YawlReceipt}>}
   * @throws {Error} If workflow not found or max cases exceeded
   */
  async createCase(workflowId, initialData = {}, options = {}) {
    const workflow = this.workflows.get(workflowId);
    if (!workflow) {
      throw new Error(`Workflow ${workflowId} not found`);
    }

    // Check capacity
    if (this.cases.size >= this.maxConcurrentCases) {
      throw new Error(`Maximum concurrent cases (${this.maxConcurrentCases}) exceeded`);
    }

    const caseId = options.caseId || randomUUID();
    const yawlCase = new YawlCase(
      { id: caseId, workflowId, data: initialData },
      workflow
    );

    this.cases.set(caseId, yawlCase);
    this._stats.casesCreated++;

    // Log to KGC-4D if enabled
    if (this.enableEventLog) {
      await this._logCaseEvent(YAWL_EVENT_TYPES.CASE_CREATED, {
        caseId: yawlCase.id,
        specId: workflowId,
        timestamp: toISO(now()),
      });
    }

    this._appendEvent({
      type: 'CASE_CREATED',
      caseId,
      workflowId,
      data: initialData,
    });

    this.emit(ENGINE_EVENTS.CASE_CREATED, {
      caseId,
      workflowId,
      data: initialData,
    });

    // Start the case (enable start task)
    const startResult = await yawlCase.start();

    this._appendEvent({
      type: 'CASE_STARTED',
      caseId,
      taskId: workflow.startTaskId,
      workItemId: startResult.task.id,
    });

    this.emit(ENGINE_EVENTS.CASE_STARTED, {
      caseId,
      workflowId,
      taskId: workflow.startTaskId,
      workItemId: startResult.task.id,
    });

    this.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId: workflow.startTaskId,
      workItemId: startResult.task.id,
    });

    this._stats.tasksEnabled++;

    return { case: yawlCase, receipt: startResult.receipt };
  }

  // ===========================================================================
  // Task Execution
  // ===========================================================================

  /**
   * Enable a task in a case
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task definition ID
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
   * @throws {Error} If case/task not found or circuit breaker open
   */
  async enableTask(caseId, taskId, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    // Check circuit breaker
    const breakerKey = `${yawlCase.workflowId}:${taskId}`;
    if (this._isCircuitOpen(breakerKey)) {
      throw new Error(`Circuit breaker open for task ${taskId}`);
    }

    // Run pre-enablement hook if policy pack exists
    const validation = await this._executeValidationHook(
      yawlCase.workflowId,
      taskId,
      { caseId, actor }
    );

    if (!validation.valid) {
      throw new Error(`Task enablement denied: ${validation.receipt?.justification?.reason || 'Unknown'}`);
    }

    const result = await yawlCase.enableTask(taskId, actor);

    this._appendEvent({
      type: 'TASK_ENABLED',
      caseId,
      taskId,
      workItemId: result.task.id,
      actor,
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_ENABLED, {
        caseId,
        taskId,
        workItemId: result.task.id,
        enabledAt: toISO(result.task.enabledAt),
      });
    }

    this.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId,
      workItemId: result.task.id,
      actor,
    });

    this._stats.tasksEnabled++;

    return result;
  }

  /**
   * Start a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @param {Object} [options={}] - Options
   * @param {string} [options.resourceId] - Resource to allocate
   * @param {string} [options.actor] - Actor
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt, resource?: Object}>}
   */
  async startTask(caseId, workItemId, options = {}) {
    const yawlCase = this.cases.get(caseId);
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
      const allocation = this.resourcePool.allocate({
        taskId: workItemId,
        role: task.role,
        preferredResourceId: options.resourceId,
      });

      if (allocation.queued) {
        throw new Error(`No available resources for role ${task.role}`);
      }

      allocatedResource = allocation.resource;

      this.emit(ENGINE_EVENTS.RESOURCE_ALLOCATED, {
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

    this._appendEvent({
      type: 'TASK_STARTED',
      caseId,
      workItemId,
      resourceId: allocatedResource?.id ?? options.resourceId,
      actor: options.actor,
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_STARTED, {
        workItemId,
        startedAt: toISO(result.task.startedAt),
      }, caseId);
    }

    this.emit(ENGINE_EVENTS.TASK_STARTED, {
      caseId,
      workItemId,
      resourceId: allocatedResource?.id ?? options.resourceId,
      actor: options.actor,
    });

    this._stats.tasksStarted++;

    return { ...result, resource: allocatedResource };
  }

  /**
   * Complete a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID (or taskId for convenience)
   * @param {Object} [output={}] - Task output
   * @param {string} [actor] - Actor
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt, downstreamEnabled: Array}>}
   */
  async completeTask(caseId, workItemId, output = {}, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Verify task is active
    if (task.status !== TaskStatus.RUNNING) {
      throw new Error(`Task ${workItemId} is not running (status: ${task.status})`);
    }

    // Release resource if allocated
    if (task.assignedResource) {
      const nextFromQueue = this.resourcePool.release(task.assignedResource);

      this.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
        caseId,
        workItemId,
        resourceId: task.assignedResource,
      });

      if (nextFromQueue) {
        this._appendEvent({
          type: 'RESOURCE_REALLOCATED',
          resourceId: nextFromQueue.resource.id,
          fromWorkItemId: workItemId,
          toTaskId: nextFromQueue.taskId,
        });
      }
    }

    // Get task definition ID
    const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

    // Run post-completion hook if policy pack exists
    const hookRouting = await this._executeRoutingHook(
      yawlCase.workflowId,
      taskDefId,
      { caseId, actor, output, env: output }
    );

    const result = await yawlCase.completeTask(workItemId, output, actor);

    // Reset circuit breaker on success
    const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
    this._resetCircuitBreaker(breakerKey);

    this._appendEvent({
      type: 'TASK_COMPLETED',
      caseId,
      workItemId,
      output,
      actor,
      downstreamEnabled: result.downstreamEnabled.map(d => d.taskId),
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_COMPLETED, {
        workItemId,
        completedAt: toISO(result.task.completedAt),
        result: output,
      }, caseId);
    }

    this.emit(ENGINE_EVENTS.TASK_COMPLETED, {
      caseId,
      workItemId,
      taskId: taskDefId,
      output,
      actor,
      downstreamEnabled: result.downstreamEnabled,
      hookReceipt: hookRouting?.receipt,
    });

    this._stats.tasksCompleted++;

    // Emit events for downstream enabled tasks
    for (const downstream of result.downstreamEnabled) {
      this.emit(ENGINE_EVENTS.TASK_ENABLED, {
        caseId,
        taskId: downstream.taskId,
        workItemId: downstream.workItemId,
      });
      this._stats.tasksEnabled++;
    }

    // Check if case completed
    if (yawlCase.status === CaseStatus.COMPLETED) {
      this._appendEvent({
        type: 'CASE_COMPLETED',
        caseId,
      });

      this.emit(ENGINE_EVENTS.CASE_COMPLETED, {
        caseId,
        workflowId: yawlCase.workflowId,
      });

      this._stats.casesCompleted++;
    }

    return result;
  }

  /**
   * Cancel a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
   */
  async cancelTask(caseId, workItemId, reason, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Release resource if allocated
    if (task.assignedResource) {
      this.resourcePool.release(task.assignedResource);

      this.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
        caseId,
        workItemId,
        resourceId: task.assignedResource,
      });
    }

    const result = await yawlCase.cancelTask(workItemId, reason, actor);

    // Get task definition ID
    const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

    // Handle cancellation propagation if policy pack exists
    this._executeCancellationHook(
      yawlCase.workflowId,
      taskDefId,
      reason,
      { caseId, actor }
    );

    this._appendEvent({
      type: 'TASK_CANCELLED',
      caseId,
      workItemId,
      reason,
      actor,
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_CANCELLED, {
        workItemId,
        cancelledAt: toISO(now()),
        reason: reason || 'No reason provided',
      }, caseId);
    }

    this.emit(ENGINE_EVENTS.TASK_CANCELLED, {
      caseId,
      workItemId,
      taskId: taskDefId,
      reason,
      actor,
    });

    this._stats.tasksCancelled++;

    return result;
  }

  /**
   * Cancel all tasks in a cancellation region
   * @param {string} caseId - Case ID
   * @param {string} regionId - Cancellation region ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor
   * @returns {Promise<{cancelled: Array, receipts: Array}>}
   */
  async cancelRegion(caseId, regionId, reason, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const result = await yawlCase.cancelRegion(regionId, reason, actor);

    this._appendEvent({
      type: 'REGION_CANCELLED',
      caseId,
      regionId,
      cancelledCount: result.cancelled.length,
      reason,
      actor,
    });

    for (const task of result.cancelled) {
      this.emit(ENGINE_EVENTS.TASK_CANCELLED, {
        caseId,
        workItemId: task.id,
        reason: `Region ${regionId} cancelled: ${reason || 'No reason'}`,
        actor,
      });
      this._stats.tasksCancelled++;
    }

    return result;
  }

  /**
   * Set circuit breaker state for a task
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task definition ID
   * @param {boolean} enabled - Enable or disable
   * @returns {Promise<{cancelled: Array}>}
   */
  async setCircuitBreaker(caseId, taskId, enabled) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const result = await yawlCase.setCircuitBreaker(taskId, enabled);

    this._appendEvent({
      type: 'CIRCUIT_BREAKER_SET',
      caseId,
      taskId,
      enabled,
      cancelledCount: result.cancelled.length,
    });

    if (!enabled) {
      this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
        caseId,
        taskId,
        cancelledCount: result.cancelled.length,
      });
    } else {
      this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, {
        caseId,
        taskId,
      });
    }

    return result;
  }

  /**
   * Handle timeout for a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
   */
  async timeoutTask(caseId, workItemId) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Release resource if allocated
    if (task.assignedResource) {
      this.resourcePool.release(task.assignedResource);
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
    this._recordCircuitFailure(breakerKey);

    this._appendEvent({
      type: 'TASK_TIMEOUT',
      caseId,
      workItemId,
    });

    this.emit(ENGINE_EVENTS.TASK_TIMEOUT, {
      caseId,
      workItemId,
      taskId: taskDefId,
    });

    this._stats.tasksTimedOut++;

    return { task, receipt };
  }

  // ===========================================================================
  // Backward Compatibility Aliases
  // ===========================================================================

  /**
   * Alias for startTask (backward compatibility)
   * @deprecated Use startTask instead
   */
  async startWorkItem(caseId, workItemId, options) {
    return this.startTask(caseId, workItemId, options);
  }

  /**
   * Alias for completeTask (backward compatibility)
   * @deprecated Use completeTask instead
   */
  async completeWorkItem(caseId, workItemId, output, actor) {
    return this.completeTask(caseId, workItemId, output, actor);
  }

  /**
   * Alias for cancelTask (backward compatibility)
   * @deprecated Use cancelTask instead
   */
  async cancelWorkItem(caseId, workItemId, reason, actor) {
    return this.cancelTask(caseId, workItemId, reason, actor);
  }
}

// =============================================================================
// Backward Compatibility Alias
// =============================================================================

/**
 * Alias for WorkflowEngine for backward compatibility
 * @deprecated Use WorkflowEngine instead
 */
export const YawlEngine = WorkflowEngine;

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a new workflow engine instance
 *
 * @param {Object} [config={}] - Engine configuration
 * @returns {WorkflowEngine} Configured workflow engine
 *
 * @example
 * ```javascript
 * // Basic usage
 * const engine = createWorkflowEngine();
 *
 * // With configuration
 * const engine = createWorkflowEngine({
 *   nodeId: 'production-node-1',
 *   enableEventLog: true,
 *   enableSnapshots: true,
 *   gitPath: '/var/lib/yawl/snapshots',
 *   maxConcurrentCases: 5000,
 * });
 * ```
 */
export function createWorkflowEngine(config = {}) {
  return new WorkflowEngine(config);
}

// =============================================================================
// Exports
// =============================================================================

export {
  WorkflowEngine,
  YAWL_NS,
  YAWL_GRAPHS,
  ENGINE_EVENTS,
  HealthStatus,
};

export default {
  WorkflowEngine,
  YawlEngine,
  createWorkflowEngine,
  ENGINE_EVENTS,
  YAWL_NS,
  YAWL_GRAPHS,
  HealthStatus,
};
