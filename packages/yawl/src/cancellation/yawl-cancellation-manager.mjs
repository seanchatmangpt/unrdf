/**
 * @file YAWL Cancellation Manager - Work Item Orchestration
 * @module yawl/cancellation/manager
 *
 * @description
 * Orchestrates work items, cancellation regions, circuit breakers, and timeouts.
 * Coordinates all cancellation semantics including propagation and hooks.
 */

import { randomUUID } from 'crypto';
import {
  CancellationRegionManager,
} from './yawl-cancellation-regions.mjs';
import {
  TaskCircuitBreaker,
  CancellationReceiptLogger,
} from './yawl-cancellation-core.mjs';

/**
 * @typedef {Object} CancellationConfig
 * @property {number} [defaultTimeout=30000]
 * @property {number} [maxTimeout=300000]
 * @property {number} [circuitBreakerThreshold=3]
 * @property {number} [circuitBreakerReset=60000]
 * @property {Function} [onCancellation]
 * @property {Function} [onTimeout]
 * @property {Function} [onCircuitOpen]
 */

/**
 * Main YAWL Cancellation Manager
 */
export class YawlCancellationManager {
  /**
   * @param {CancellationConfig} [config]
   */
  constructor(config = {}) {
    this.config = {
      defaultTimeout: config.defaultTimeout ?? 30000,
      maxTimeout: config.maxTimeout ?? 300000,
      circuitBreakerThreshold: config.circuitBreakerThreshold ?? 3,
      circuitBreakerReset: config.circuitBreakerReset ?? 60000,
      onCancellation: config.onCancellation ?? null,
      onTimeout: config.onTimeout ?? null,
      onCircuitOpen: config.onCircuitOpen ?? null,
    };

    /** @type {Map<string, import('./yawl-cancellation-core.mjs').WorkItem>} */
    this.workItems = new Map();

    /** @type {Map<string, TaskCircuitBreaker>} */
    this.circuitBreakers = new Map();

    /** @type {Map<string, Set<string>>} */
    this.workItemsByTask = new Map();

    /** @type {Map<string, Set<string>>} */
    this.workItemsByCase = new Map();

    /** @type {Map<string, NodeJS.Timeout>} */
    this.timeoutHandles = new Map();

    /** @type {Map<string, string[]>} */
    this.taskDependencies = new Map();

    this.regionManager = new CancellationRegionManager();
    this.receiptLogger = new CancellationReceiptLogger();

    /** @type {Function[]} */
    this.cancellationHooks = [];
  }

  // --------------------------------------------------------------------------
  // WORK ITEM MANAGEMENT
  // --------------------------------------------------------------------------

  /**
   * @param {Object} options
   * @returns {import('./yawl-cancellation-core.mjs').WorkItem}
   */
  createWorkItem(options) {
    if (!options.taskId || typeof options.taskId !== 'string') {
      throw new Error('taskId is required and must be a string');
    }
    if (!options.caseId || typeof options.caseId !== 'string') {
      throw new Error('caseId is required and must be a string');
    }

    const timeoutMs = Math.min(
      options.timeoutMs ?? this.config.defaultTimeout,
      this.config.maxTimeout
    );

    const workItem = {
      id: randomUUID(),
      taskId: options.taskId,
      caseId: options.caseId,
      regionId: options.regionId,
      state: 'pending',
      createdAt: new Date(),
      startedAt: undefined,
      completedAt: undefined,
      cancelledAt: undefined,
      cancellationReason: undefined,
      timeoutMs,
      retryCount: 0,
      metadata: options.metadata,
    };

    this.workItems.set(workItem.id, workItem);

    if (!this.workItemsByTask.has(workItem.taskId)) {
      this.workItemsByTask.set(workItem.taskId, new Set());
    }
    this.workItemsByTask.get(workItem.taskId).add(workItem.id);

    if (!this.workItemsByCase.has(workItem.caseId)) {
      this.workItemsByCase.set(workItem.caseId, new Set());
    }
    this.workItemsByCase.get(workItem.caseId).add(workItem.id);

    return workItem;
  }

  /**
   *
   */
  enableWorkItem(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'pending') return null;

    const breaker = this._getOrCreateCircuitBreaker(workItem.taskId);
    if (!breaker.allowExecution()) {
      this.cancelWorkItem(workItemId, 'task_disabled');
      return null;
    }

    workItem.state = 'enabled';
    return workItem;
  }

  /**
   *
   */
  startExecution(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'enabled') return null;

    workItem.state = 'executing';
    workItem.startedAt = new Date();

    this._startTimeoutEnforcement(workItemId);

    return workItem;
  }

  /**
   *
   */
  completeWorkItem(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'executing') return null;

    this._clearTimeout(workItemId);

    const breaker = this.circuitBreakers.get(workItem.taskId);
    if (breaker) {
      breaker.recordSuccess();
    }

    workItem.state = 'completed';
    workItem.completedAt = new Date();

    return workItem;
  }

  /**
   *
   */
  recordFailure(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) return { circuitOpened: false };

    this._clearTimeout(workItemId);

    workItem.state = 'failed';
    workItem.completedAt = new Date();

    const breaker = this._getOrCreateCircuitBreaker(workItem.taskId);
    const circuitOpened = breaker.recordFailure();

    if (circuitOpened) {
      this.receiptLogger.logCircuitBreakerOpen(workItem.taskId, breaker.failureCount);

      this._cancelAllWorkItemsForTask(workItem.taskId, 'circuit_breaker');

      if (this.config.onCircuitOpen) {
        try {
          this.config.onCircuitOpen({
            taskId: workItem.taskId,
            failureCount: breaker.failureCount,
            timestamp: new Date(),
          });
        } catch {
          // Ignore
        }
      }
    }

    return { circuitOpened, breakerState: breaker.getState() };
  }

  // --------------------------------------------------------------------------
  // CANCELLATION
  // --------------------------------------------------------------------------

  /**
   *
   */
  cancelWorkItem(workItemId, reason) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) {
      return { success: false, cancelled: [], reason: 'work_item_not_found' };
    }

    if (workItem.state === 'cancelled' || workItem.state === 'completed') {
      return { success: false, cancelled: [], reason: 'already_terminal' };
    }

    this._clearTimeout(workItemId);

    workItem.state = 'cancelled';
    workItem.cancelledAt = new Date();
    workItem.cancellationReason = reason;

    this.receiptLogger.logCancelledWorkItem(workItemId, reason, workItem.regionId);

    this._invokeCancellationHooks(workItem, reason);

    if (this.config.onCancellation) {
      try {
        this.config.onCancellation({
          workItemId,
          taskId: workItem.taskId,
          reason,
          timestamp: new Date(),
        });
      } catch {
        // Ignore
      }
    }

    const cancelled = [workItemId];

    if (workItem.regionId && reason !== 'region_cancelled') {
      const regionCancelled = this._cancelRegion(workItem.regionId, workItemId, reason);
      cancelled.push(...regionCancelled);
    }

    const propagated = this._propagateCancellation(workItem.taskId, reason);
    cancelled.push(...propagated);

    if (propagated.length > 0) {
      this.receiptLogger.logCancellationPropagated(workItemId, propagated, reason);
    }

    return { success: true, cancelled, reason };
  }

  /**
   *
   */
  _cancelRegion(regionId, sourceWorkItemId, reason = 'region_cancelled') {
    const cancelled = [];
    const region = this.regionManager.getRegion(regionId);
    if (!region || !region.active) return cancelled;

    const tasksInRegion = this.regionManager.getAllTasksInRegion(regionId);

    for (const taskId of tasksInRegion) {
      const workItemIds = this.workItemsByTask.get(taskId);
      if (!workItemIds) continue;

      for (const wiId of workItemIds) {
        if (wiId === sourceWorkItemId) continue;

        const wi = this.workItems.get(wiId);
        if (wi && wi.state !== 'cancelled' && wi.state !== 'completed') {
          this._clearTimeout(wiId);
          wi.state = 'cancelled';
          wi.cancelledAt = new Date();
          wi.cancellationReason = 'region_cancelled';
          this.receiptLogger.logCancelledWorkItem(wiId, 'region_cancelled', regionId);
          this._invokeCancellationHooks(wi, 'region_cancelled');
          cancelled.push(wiId);
        }
      }
    }

    this.regionManager.deactivateRegion(regionId);
    this.receiptLogger.logRegionCancelled(regionId, reason, cancelled);

    return cancelled;
  }

  /**
   *
   */
  _propagateCancellation(taskId, reason) {
    const cancelled = [];
    const downstreamTasks = this.taskDependencies.get(taskId);
    if (!downstreamTasks) return cancelled;

    for (const downstreamTaskId of downstreamTasks) {
      const workItemIds = this.workItemsByTask.get(downstreamTaskId);
      if (!workItemIds) continue;

      for (const wiId of workItemIds) {
        const wi = this.workItems.get(wiId);
        if (wi && (wi.state === 'pending' || wi.state === 'enabled')) {
          this._clearTimeout(wiId);
          wi.state = 'cancelled';
          wi.cancelledAt = new Date();
          wi.cancellationReason = 'dependency_failed';
          this.receiptLogger.logCancelledWorkItem(wiId, 'dependency_failed', wi.regionId);
          this._invokeCancellationHooks(wi, 'dependency_failed');
          cancelled.push(wiId);
        }
      }
    }

    return cancelled;
  }

  /**
   *
   */
  _cancelAllWorkItemsForTask(taskId, reason) {
    const workItemIds = this.workItemsByTask.get(taskId);
    if (!workItemIds) return;

    for (const wiId of workItemIds) {
      const wi = this.workItems.get(wiId);
      if (wi && wi.state !== 'cancelled' && wi.state !== 'completed') {
        this.cancelWorkItem(wiId, reason);
      }
    }
  }

  // --------------------------------------------------------------------------
  // TIMEOUT ENFORCEMENT
  // --------------------------------------------------------------------------

  /**
   *
   */
  _startTimeoutEnforcement(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) return;

    const handle = setTimeout(() => {
      this._handleTimeout(workItemId);
    }, workItem.timeoutMs);

    this.timeoutHandles.set(workItemId, handle);
  }

  /**
   *
   */
  _handleTimeout(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'executing') return;

    const durationMs = Date.now() - workItem.startedAt.getTime();

    this.receiptLogger.logTimeoutOccurred(workItemId, workItem.taskId, durationMs, workItem.timeoutMs);

    if (this.config.onTimeout) {
      try {
        this.config.onTimeout({
          workItemId,
          taskId: workItem.taskId,
          durationMs,
          timeoutMs: workItem.timeoutMs,
          timestamp: new Date(),
        });
      } catch {
        // Ignore
      }
    }

    this.cancelWorkItem(workItemId, 'timeout');
  }

  /**
   *
   */
  _clearTimeout(workItemId) {
    const handle = this.timeoutHandles.get(workItemId);
    if (handle) {
      clearTimeout(handle);
      this.timeoutHandles.delete(workItemId);
    }
  }

  // --------------------------------------------------------------------------
  // CIRCUIT BREAKER MANAGEMENT
  // --------------------------------------------------------------------------

  /**
   *
   */
  _getOrCreateCircuitBreaker(taskId) {
    if (!this.circuitBreakers.has(taskId)) {
      const breaker = new TaskCircuitBreaker({
        taskId,
        failureThreshold: this.config.circuitBreakerThreshold,
        resetTimeout: this.config.circuitBreakerReset,
        onStateChange: event => {
          if (event.to === 'closed') {
            this.receiptLogger.logCircuitBreakerClosed(event.taskId);
          }
        },
      });
      this.circuitBreakers.set(taskId, breaker);
    }
    return this.circuitBreakers.get(taskId);
  }

  /**
   *
   */
  enableTask(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    if (breaker) {
      breaker.reset();
      this.receiptLogger.log('TASK_ENABLED', {
        taskId,
        enabledAt: new Date().toISOString(),
      });
    }
  }

  /**
   *
   */
  isTaskEnabled(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    if (!breaker) return true;
    return breaker.allowExecution();
  }

  /**
   *
   */
  getCircuitBreakerState(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    return breaker ? breaker.getState() : null;
  }

  // --------------------------------------------------------------------------
  // CANCELLATION HOOKS
  // --------------------------------------------------------------------------

  /**
   *
   */
  registerCancellationHook(hook) {
    if (typeof hook === 'function') {
      this.cancellationHooks.push(hook);
    }
  }

  /**
   *
   */
  _invokeCancellationHooks(workItem, reason) {
    for (const hook of this.cancellationHooks) {
      try {
        hook(workItem, reason);
      } catch {
        // Ignore
      }
    }
  }

  // --------------------------------------------------------------------------
  // TASK DEPENDENCIES
  // --------------------------------------------------------------------------

  /**
   *
   */
  setTaskDependencies(taskId, downstreamTaskIds) {
    this.taskDependencies.set(taskId, downstreamTaskIds);
  }

  /**
   *
   */
  getTaskDependencies(taskId) {
    return this.taskDependencies.get(taskId) || [];
  }

  // --------------------------------------------------------------------------
  // QUERYING & TIME-TRAVEL
  // --------------------------------------------------------------------------

  /**
   *
   */
  getWorkItem(workItemId) {
    return this.workItems.get(workItemId);
  }

  /**
   *
   */
  getWorkItemsForTask(taskId) {
    const ids = this.workItemsByTask.get(taskId);
    if (!ids) return [];
    return Array.from(ids)
      .map(id => this.workItems.get(id))
      .filter(wi => wi !== undefined);
  }

  /**
   *
   */
  getWorkItemsForCase(caseId) {
    const ids = this.workItemsByCase.get(caseId);
    if (!ids) return [];
    return Array.from(ids)
      .map(id => this.workItems.get(id))
      .filter(wi => wi !== undefined);
  }

  /**
   *
   */
  getStateAtTime(timestamp) {
    const receipts = this.receiptLogger.getReceiptsAtTime(timestamp);

    const cancelledWorkItems = new Set();
    const openCircuitBreakers = new Set();
    const cancelledRegions = new Set();

    for (const receipt of receipts) {
      switch (receipt.type) {
        case 'CANCELLED_WORK_ITEM':
          cancelledWorkItems.add(receipt.payload.workItemId);
          break;
        case 'CIRCUIT_BREAKER_OPEN':
          openCircuitBreakers.add(receipt.payload.taskId);
          break;
        case 'CIRCUIT_BREAKER_CLOSED':
          openCircuitBreakers.delete(receipt.payload.taskId);
          break;
        case 'REGION_CANCELLED':
          cancelledRegions.add(receipt.payload.regionId);
          break;
      }
    }

    return {
      timestamp,
      cancelledWorkItems: Array.from(cancelledWorkItems),
      openCircuitBreakers: Array.from(openCircuitBreakers),
      cancelledRegions: Array.from(cancelledRegions),
      receiptCount: receipts.length,
    };
  }

  // --------------------------------------------------------------------------
  // STATISTICS
  // --------------------------------------------------------------------------

  /**
   *
   */
  getStats() {
    const workItemsByState = {};
    for (const wi of this.workItems.values()) {
      workItemsByState[wi.state] = (workItemsByState[wi.state] || 0) + 1;
    }

    const circuitBreakerStats = {};
    for (const [taskId, breaker] of this.circuitBreakers) {
      circuitBreakerStats[taskId] = breaker.getState();
    }

    return {
      workItems: {
        total: this.workItems.size,
        byState: workItemsByState,
      },
      circuitBreakers: {
        total: this.circuitBreakers.size,
        states: circuitBreakerStats,
      },
      regions: {
        total: this.regionManager.regions.size,
      },
      receipts: {
        total: this.receiptLogger.receipts.length,
      },
      activeTimeouts: this.timeoutHandles.size,
    };
  }

  // --------------------------------------------------------------------------
  // CLEANUP
  // --------------------------------------------------------------------------

  /**
   *
   */
  terminate(reason = 'workflow_terminated') {
    for (const [id, workItem] of this.workItems) {
      if (workItem.state === 'executing' || workItem.state === 'enabled' || workItem.state === 'pending') {
        this.cancelWorkItem(id, reason);
      }
    }

    for (const [id, handle] of this.timeoutHandles) {
      clearTimeout(handle);
    }
    this.timeoutHandles.clear();
  }

  /**
   *
   */
  export() {
    return {
      workItems: Array.from(this.workItems.values()).map(wi => ({
        ...wi,
        createdAt: wi.createdAt.toISOString(),
        startedAt: wi.startedAt?.toISOString(),
        completedAt: wi.completedAt?.toISOString(),
        cancelledAt: wi.cancelledAt?.toISOString(),
      })),
      regions: this.regionManager.export(),
      receipts: this.receiptLogger.export(),
      dependencies: Object.fromEntries(this.taskDependencies),
      circuitBreakers: Object.fromEntries(
        Array.from(this.circuitBreakers.entries()).map(([k, v]) => [k, v.getState()])
      ),
    };
  }

  /**
   *
   */
  clear() {
    this.terminate();
    this.workItems.clear();
    this.workItemsByTask.clear();
    this.workItemsByCase.clear();
    this.circuitBreakers.clear();
    this.taskDependencies.clear();
    this.receiptLogger.clear();
    this.regionManager.regions.clear();
    this.regionManager.taskToRegions.clear();
  }
}
