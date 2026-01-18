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
    if (task.status !== TaskStatus.ACTIVE) {
      throw new Error(`Task ${workItemId} is not active (status: ${task.status})`);
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

  // ===========================================================================
  // Enhanced Case Lifecycle (Java YAWL compatibility)
  // ===========================================================================

  /**
   * Launch a new case (13-step full sequence)
   * Java YAWL compatibility method - complete case initialization
   * @param {string} specId - Workflow specification ID
   * @param {Object} [inputData={}] - Initial case data
   * @param {Object} [caseParams={}] - Case parameters
   * @param {string} [caseParams.caseId] - Custom case ID
   * @param {string} [caseParams.launchedBy] - Actor launching the case
   * @returns {Promise<{case: YawlCase, receipt: YawlReceipt, caseId: string}>}
   */
  async launchCase(specId, inputData = {}, caseParams = {}) {
    const result = await this.createCase(specId, inputData, {
      caseId: caseParams.caseId,
      actor: caseParams.launchedBy,
    });

    return {
      case: result.case,
      receipt: result.receipt,
      caseId: result.case.id,
    };
  }

  /**
   * Suspend a case - pause all enabled tasks
   * @param {string} caseId - Case ID to suspend
   * @param {string} [reason] - Suspension reason
   * @param {string} [actor] - Actor performing suspension
   * @returns {Promise<{case: YawlCase, suspendedTasks: Array, receipt: YawlReceipt}>}
   */
  async suspendCase(caseId, reason, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    if (yawlCase.status !== CaseStatus.RUNNING) {
      throw new Error(`Cannot suspend case: status is ${yawlCase.status}`);
    }

    const beforeState = yawlCase.getState();

    // Store enabled work items before suspension
    const enabledWorkItems = yawlCase.getEnabledWorkItems();
    const suspendedTasks = [];

    // Mark case as suspended
    yawlCase.status = CaseStatus.SUSPENDED;

    const afterState = yawlCase.getState();
    const previousReceipt = yawlCase.receipts[yawlCase.receipts.length - 1];

    const receipt = await buildReceipt({
      caseId,
      taskId: 'case-suspend',
      action: 'suspend',
      actor,
      beforeState,
      afterState,
      previousReceipt,
      output: { reason, enabledCount: enabledWorkItems.length },
    });

    yawlCase.receipts.push(receipt);

    this._appendEvent({
      type: 'CASE_SUSPENDED',
      caseId,
      reason,
      actor,
      suspendedCount: enabledWorkItems.length,
    });

    if (this.enableEventLog) {
      await this._logCaseEvent(YAWL_EVENT_TYPES.CASE_SUSPENDED, {
        caseId,
        suspendedAt: toISO(now()),
        reason: reason || 'No reason provided',
      });
    }

    this.emit(ENGINE_EVENTS.CASE_SUSPENDED, {
      caseId,
      workflowId: yawlCase.workflowId,
      reason,
      actor,
      suspendedTasks: enabledWorkItems.map(wi => wi.id),
    });

    return { case: yawlCase, suspendedTasks: enabledWorkItems, receipt };
  }

  /**
   * Resume a suspended case - re-enable suspended tasks
   * @param {string} caseId - Case ID to resume
   * @param {string} [actor] - Actor performing resumption
   * @returns {Promise<{case: YawlCase, resumedTasks: Array, receipt: YawlReceipt}>}
   */
  async resumeCase(caseId, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    if (yawlCase.status !== CaseStatus.SUSPENDED) {
      throw new Error(`Cannot resume case: status is ${yawlCase.status}`);
    }

    const beforeState = yawlCase.getState();

    // Get enabled tasks (they remain enabled during suspension)
    const enabledWorkItems = yawlCase.getEnabledWorkItems();

    // Resume case
    yawlCase.status = CaseStatus.RUNNING;

    const afterState = yawlCase.getState();
    const previousReceipt = yawlCase.receipts[yawlCase.receipts.length - 1];

    const receipt = await buildReceipt({
      caseId,
      taskId: 'case-resume',
      action: 'resume',
      actor,
      beforeState,
      afterState,
      previousReceipt,
      output: { resumedCount: enabledWorkItems.length },
    });

    yawlCase.receipts.push(receipt);

    this._appendEvent({
      type: 'CASE_RESUMED',
      caseId,
      actor,
      resumedCount: enabledWorkItems.length,
    });

    if (this.enableEventLog) {
      await this._logCaseEvent(YAWL_EVENT_TYPES.CASE_RESUMED, {
        caseId,
        resumedAt: toISO(now()),
      });
    }

    this.emit(ENGINE_EVENTS.CASE_RESUMED, {
      caseId,
      workflowId: yawlCase.workflowId,
      actor,
      resumedTasks: enabledWorkItems.map(wi => wi.id),
    });

    return { case: yawlCase, resumedTasks: enabledWorkItems, receipt };
  }

  /**
   * Cancel a case with cancellation region handling
   * @param {string} caseId - Case ID to cancel
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor performing cancellation
   * @returns {Promise<{case: YawlCase, cancelledTasks: Array, receipts: Array}>}
   */
  async cancelCase(caseId, reason, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    if ([CaseStatus.COMPLETED, CaseStatus.CANCELLED].includes(yawlCase.status)) {
      throw new Error(`Cannot cancel case: status is ${yawlCase.status}`);
    }

    const beforeState = yawlCase.getState();

    // Cancel all active and enabled work items
    const cancelledTasks = [];
    const receipts = [];

    for (const [workItemId, task] of yawlCase.workItems) {
      if (!task.isTerminal()) {
        const result = await yawlCase.cancelTask(workItemId, reason, actor);
        cancelledTasks.push(result.task);
        receipts.push(result.receipt);
      }
    }

    // Mark case as cancelled
    yawlCase.status = CaseStatus.CANCELLED;
    yawlCase.completedAt = now();

    const afterState = yawlCase.getState();
    const previousReceipt = yawlCase.receipts[yawlCase.receipts.length - 1];

    const caseReceipt = await buildReceipt({
      caseId,
      taskId: 'case-cancel',
      action: 'cancel',
      actor,
      beforeState,
      afterState,
      previousReceipt,
      output: { reason, cancelledCount: cancelledTasks.length },
    });

    yawlCase.receipts.push(caseReceipt);

    this._appendEvent({
      type: 'CASE_CANCELLED',
      caseId,
      reason,
      actor,
      cancelledCount: cancelledTasks.length,
    });

    if (this.enableEventLog) {
      await this._logCaseEvent(YAWL_EVENT_TYPES.CASE_CANCELLED, {
        caseId,
        cancelledAt: toISO(now()),
        reason: reason || 'No reason provided',
      });
    }

    this.emit(ENGINE_EVENTS.CASE_CANCELLED, {
      caseId,
      workflowId: yawlCase.workflowId,
      reason,
      actor,
      cancelledCount: cancelledTasks.length,
    });

    this._stats.casesCancelled++;

    return { case: yawlCase, cancelledTasks, receipts: [...receipts, caseReceipt] };
  }

  // ===========================================================================
  // Observer Registration (Java YAWL interface)
  // ===========================================================================

  /**
   * Register an observer for workflow events
   * Async notification using Promises instead of callbacks
   * @param {string} eventType - Event type from ENGINE_EVENTS
   * @param {Function} callback - Async callback (event) => Promise<void>
   * @returns {Function} Unsubscribe function
   *
   * @example
   * ```javascript
   * const unsubscribe = engine.registerObserver('case:started', async (event) => {
   *   await notifyExternalSystem(event.caseId);
   *   console.log('Case started:', event.caseId);
   * });
   * ```
   */
  registerObserver(eventType, callback) {
    if (typeof callback !== 'function') {
      throw new TypeError('Observer callback must be a function');
    }

    // Wrap callback to handle async execution
    const asyncHandler = async (event) => {
      try {
        await Promise.resolve(callback(event));
      } catch (error) {
        console.error(`Observer error for ${eventType}:`, error);
        this.emit(ENGINE_EVENTS.OBSERVER_ERROR, {
          eventType,
          error: error.message,
          event,
        });
      }
    };

    // Use existing event system
    return this.on(eventType, asyncHandler);
  }

  /**
   * Get all registered observers
   * @returns {Map<string, number>} Map of event types to observer counts
   */
  getObservers() {
    const observers = new Map();
    for (const [eventType, handlers] of this._eventHandlers) {
      observers.set(eventType, handlers.size);
    }
    return observers;
  }

  // ===========================================================================
  // Timer Management
  // ===========================================================================

  /**
   * Register a timer for a task
   * @param {string} taskId - Task work item ID
   * @param {number} duration - Duration in milliseconds
   * @param {Function} onExpiry - Callback on timer expiry
   * @returns {Promise<{timerId: string, receipt: YawlReceipt}>}
   */
  async registerTimer(taskId, duration, onExpiry) {
    if (!this._timers) {
      this._timers = new Map();
    }

    const timerId = randomUUID();
    const expiryTime = now() + BigInt(duration) * 1000000n; // Convert ms to ns

    const timer = {
      id: timerId,
      taskId,
      duration,
      expiryTime,
      onExpiry,
      handle: null,
    };

    // Set timeout
    timer.handle = setTimeout(async () => {
      try {
        await onExpiry({ taskId, timerId, expiryTime });
        this._timers.delete(timerId);
        this.emit(ENGINE_EVENTS.TIMER_EXPIRED, { taskId, timerId });
      } catch (error) {
        console.error(`Timer ${timerId} expiry error:`, error);
      }
    }, duration);

    this._timers.set(timerId, timer);

    const receipt = await buildReceipt({
      caseId: 'timer-registry',
      taskId,
      action: 'register-timer',
      beforeState: { timers: this._timers.size - 1 },
      afterState: { timers: this._timers.size },
      previousReceipt: null,
      output: { timerId, duration, expiryTime: expiryTime.toString() },
    });

    this._appendEvent({
      type: 'TIMER_REGISTERED',
      taskId,
      timerId,
      duration,
      expiryTime: expiryTime.toString(),
    });

    this.emit(ENGINE_EVENTS.TIMER_REGISTERED, { taskId, timerId, duration });

    return { timerId, receipt };
  }

  /**
   * Cancel a registered timer
   * @param {string} timerId - Timer ID to cancel
   * @returns {Promise<{cancelled: boolean, receipt: YawlReceipt}>}
   */
  async cancelTimer(timerId) {
    if (!this._timers) {
      this._timers = new Map();
    }

    const timer = this._timers.get(timerId);
    if (!timer) {
      return { cancelled: false, receipt: null };
    }

    clearTimeout(timer.handle);
    this._timers.delete(timerId);

    const receipt = await buildReceipt({
      caseId: 'timer-registry',
      taskId: timer.taskId,
      action: 'cancel-timer',
      beforeState: { timers: this._timers.size + 1 },
      afterState: { timers: this._timers.size },
      previousReceipt: null,
      output: { timerId, cancelled: true },
    });

    this._appendEvent({
      type: 'TIMER_CANCELLED',
      taskId: timer.taskId,
      timerId,
    });

    this.emit(ENGINE_EVENTS.TIMER_CANCELLED, { taskId: timer.taskId, timerId });

    return { cancelled: true, receipt };
  }

  /**
   * Get all active timers
   * @returns {Array} Array of timer info
   */
  getActiveTimers() {
    if (!this._timers) {
      return [];
    }

    return [...this._timers.values()].map(t => ({
      id: t.id,
      taskId: t.taskId,
      duration: t.duration,
      expiryTime: t.expiryTime.toString(),
    }));
  }

  // ===========================================================================
  // Bulk Operations
  // ===========================================================================

  /**
   * Launch multiple cases in parallel
   * @param {Array<{specId: string, inputData: Object, caseParams: Object}>} specs - Case specifications
   * @returns {Promise<{cases: Array, receipts: Array, merkleRoot: string}>}
   */
  async launchCases(specs) {
    const results = await Promise.all(
      specs.map(spec => this.launchCase(spec.specId, spec.inputData, spec.caseParams))
    );

    // Build Merkle tree for batch receipt
    const receipts = results.map(r => r.receipt);
    const merkleRoot = this._buildMerkleRoot(receipts);

    this._appendEvent({
      type: 'BULK_LAUNCH',
      count: results.length,
      merkleRoot,
    });

    this.emit(ENGINE_EVENTS.BULK_LAUNCH, {
      count: results.length,
      caseIds: results.map(r => r.caseId),
      merkleRoot,
    });

    return {
      cases: results.map(r => r.case),
      receipts,
      merkleRoot,
    };
  }

  /**
   * Cancel multiple cases in bulk
   * @param {string[]} caseIds - Case IDs to cancel
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor performing cancellation
   * @returns {Promise<{cancelled: Array, receipts: Array, merkleRoot: string}>}
   */
  async cancelCases(caseIds, reason, actor) {
    const results = await Promise.all(
      caseIds.map(caseId =>
        this.cancelCase(caseId, reason, actor).catch(error => ({
          error: error.message,
          caseId,
        }))
      )
    );

    const successful = results.filter(r => !r.error);
    const failed = results.filter(r => r.error);

    // Collect all receipts
    const allReceipts = successful.flatMap(r => r.receipts);
    const merkleRoot = this._buildMerkleRoot(allReceipts);

    this._appendEvent({
      type: 'BULK_CANCEL',
      count: caseIds.length,
      successful: successful.length,
      failed: failed.length,
      merkleRoot,
    });

    this.emit(ENGINE_EVENTS.BULK_CANCEL, {
      count: caseIds.length,
      successful: successful.length,
      failed: failed.length,
      caseIds,
      merkleRoot,
    });

    return {
      cancelled: successful,
      receipts: allReceipts,
      merkleRoot,
      failed,
    };
  }

  /**
   * Build Merkle root from receipts
   * @param {Array} receipts - Receipt array
   * @returns {string} Merkle root hash
   * @private
   */
  _buildMerkleRoot(receipts) {
    if (receipts.length === 0) {
      return '0'.repeat(64);
    }

    const crypto = require('crypto');
    const hashes = receipts.map(r => r.chainHash || r.id);

    while (hashes.length > 1) {
      const newHashes = [];
      for (let i = 0; i < hashes.length; i += 2) {
        const left = hashes[i];
        const right = hashes[i + 1] || left;
        const combined = crypto.createHash('sha256').update(left + right).digest('hex');
        newHashes.push(combined);
      }
      hashes.length = 0;
      hashes.push(...newHashes);
    }

    return hashes[0];
  }

  // ===========================================================================
  // Enhanced Query API
  // ===========================================================================

  /**
   * Get running cases (active cases)
   * @returns {YawlCase[]} Array of running cases
   */
  getRunningCases() {
    return this.getActiveCases();
  }

  /**
   * Get complete case state (tokens, items, data)
   * @param {string} caseId - Case ID
   * @returns {Object} Complete case state
   */
  getCaseState(caseId) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    return {
      caseId,
      workflowId: yawlCase.workflowId,
      status: yawlCase.status,
      data: yawlCase.data,
      tokens: Object.fromEntries(yawlCase._marking),
      workItems: yawlCase.getWorkItems().map(wi => ({
        id: wi.id,
        name: wi.name,
        status: wi.status,
        enabledAt: wi.enabledAt?.toString(),
        startedAt: wi.startedAt?.toString(),
        completedAt: wi.completedAt?.toString(),
      })),
      completedTasks: [...yawlCase.completedTasks],
      activatedTasks: [...yawlCase.activatedTasks],
      receiptCount: yawlCase.receipts.length,
    };
  }

  /**
   * Get task execution history from receipts
   * @param {string} caseId - Case ID
   * @returns {Array} Task history audit trail
   */
  getTaskHistory(caseId) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    return yawlCase.receipts.map(receipt => ({
      id: receipt.id,
      taskId: receipt.taskId,
      action: receipt.action,
      actor: receipt.actor,
      timestamp: receipt.timestamp?.toString(),
      beforeHash: receipt.beforeHash,
      afterHash: receipt.afterHash,
      chainHash: receipt.chainHash,
      output: receipt.output,
    }));
  }

  /**
   * Get performance metrics
   * @returns {Object} Performance metrics
   */
  getPerformanceMetrics() {
    const uptime = now() - this._stats.startedAt;
    const uptimeMs = Number(uptime / 1000000n);

    return {
      uptime: uptimeMs,
      casesCreated: this._stats.casesCreated,
      casesCompleted: this._stats.casesCompleted,
      casesFailed: this._stats.casesFailed,
      casesCancelled: this._stats.casesCancelled,
      tasksEnabled: this._stats.tasksEnabled,
      tasksStarted: this._stats.tasksStarted,
      tasksCompleted: this._stats.tasksCompleted,
      tasksFailed: this._stats.tasksFailed,
      tasksCancelled: this._stats.tasksCancelled,
      tasksTimedOut: this._stats.tasksTimedOut,
      activeCases: this.cases.size,
      throughput: {
        casesPerSecond: (this._stats.casesCompleted / (uptimeMs / 1000)).toFixed(2),
        tasksPerSecond: (this._stats.tasksCompleted / (uptimeMs / 1000)).toFixed(2),
      },
      queueDepth: {
        enabledTasks: this.getRunningCases().reduce((sum, c) => sum + c.getEnabledWorkItems().length, 0),
        activeTasks: this.getRunningCases().reduce((sum, c) => sum + c.getActiveWorkItems().length, 0),
      },
    };
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
