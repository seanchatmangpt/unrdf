/**
 * @file YAWL Cancellation Manager
 * @module yawl/cancellation/manager
 *
 * @description
 * Main YAWL Cancellation Manager - coordinates cancellation regions,
 * circuit breakers, and timeout enforcement.
 */

import { randomUUID } from 'crypto';
import { TaskCircuitBreaker } from './circuit-breaker.mjs';
import { CancellationRegionManager } from './region-manager.mjs';
import { CancellationReceiptLogger } from './receipt-logger.mjs';

/**
 * @typedef {Object} CancellationConfig
 * @property {number} [defaultTimeout=30000] - Default timeout in ms
 * @property {number} [maxTimeout=300000] - Maximum timeout in ms
 * @property {number} [circuitBreakerThreshold=3] - Failures before circuit opens
 * @property {number} [circuitBreakerReset=60000] - Time before half-open
 * @property {Function} [onCancellation] - Callback when work item cancelled
 * @property {Function} [onTimeout] - Callback when timeout occurs
 * @property {Function} [onCircuitOpen] - Callback when circuit breaker opens
 */

/**
 * Main YAWL Cancellation Manager
 * Coordinates cancellation regions, circuit breakers, and timeout enforcement
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

    /** @type {Map<string, import('./schemas.mjs').WorkItem>} */
    this.workItems = new Map();

    /** @type {Map<string, TaskCircuitBreaker>} Task ID -> circuit breaker */
    this.circuitBreakers = new Map();

    /** @type {Map<string, Set<string>>} Task ID -> Work item IDs */
    this.workItemsByTask = new Map();

    /** @type {Map<string, Set<string>>} Case ID -> Work item IDs */
    this.workItemsByCase = new Map();

    /** @type {Map<string, NodeJS.Timeout>} Work item ID -> timeout handle */
    this.timeoutHandles = new Map();

    /** @type {Map<string, string[]>} Task ID -> downstream task IDs */
    this.taskDependencies = new Map();

    this.regionManager = new CancellationRegionManager();
    this.receiptLogger = new CancellationReceiptLogger();

    /** @type {Function[]} Cancellation hooks */
    this.cancellationHooks = [];
  }

  // --------------------------------------------------------------------------
  // WORK ITEM MANAGEMENT
  // --------------------------------------------------------------------------

  /**
   * Create a new work item
   * @param {Object} options
   * @param {string} options.taskId
   * @param {string} options.caseId
   * @param {string} [options.regionId]
   * @param {number} [options.timeoutMs]
   * @param {Object} [options.metadata]
   * @returns {import('./schemas.mjs').WorkItem}
   */
  createWorkItem(options) {
    this._validateWorkItemOptions(options);

    const workItem = this._buildWorkItem(options);
    this.workItems.set(workItem.id, workItem);
    this._indexWorkItem(workItem);

    return workItem;
  }

  /**
   * @private
   */
  _validateWorkItemOptions(options) {
    if (!options.taskId || typeof options.taskId !== 'string') {
      throw new Error('taskId is required and must be a string');
    }
    if (!options.caseId || typeof options.caseId !== 'string') {
      throw new Error('caseId is required and must be a string');
    }
  }

  /**
   * @private
   */
  _buildWorkItem(options) {
    const timeoutMs = Math.min(
      options.timeoutMs ?? this.config.defaultTimeout,
      this.config.maxTimeout
    );

    return {
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
  }

  /**
   * @private
   */
  _indexWorkItem(workItem) {
    if (!this.workItemsByTask.has(workItem.taskId)) {
      this.workItemsByTask.set(workItem.taskId, new Set());
    }
    this.workItemsByTask.get(workItem.taskId).add(workItem.id);

    if (!this.workItemsByCase.has(workItem.caseId)) {
      this.workItemsByCase.set(workItem.caseId, new Set());
    }
    this.workItemsByCase.get(workItem.caseId).add(workItem.id);
  }

  /**
   * Enable a work item and start timeout enforcement
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
   * Start executing a work item with timeout enforcement
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
   * Complete a work item successfully
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
   * Record a work item failure (for circuit breaker)
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
      this._handleCircuitOpen(workItem.taskId, breaker);
    }

    return { circuitOpened, breakerState: breaker.getState() };
  }

  /**
   * @private
   */
  _handleCircuitOpen(taskId, breaker) {
    this.receiptLogger.logCircuitBreakerOpen(taskId, breaker.failureCount);
    this._cancelAllWorkItemsForTask(taskId, 'circuit_breaker');
    this._invokeCircuitOpenCallback(taskId, breaker);
  }

  /**
   * @private
   */
  _invokeCircuitOpenCallback(taskId, breaker) {
    if (this.config.onCircuitOpen) {
      try {
        this.config.onCircuitOpen({
          taskId,
          failureCount: breaker.failureCount,
          timestamp: new Date(),
        });
      } catch {
        // Ignore callback errors
      }
    }
  }

  // --------------------------------------------------------------------------
  // CANCELLATION
  // --------------------------------------------------------------------------

  /**
   * Cancel a work item
   */
  cancelWorkItem(workItemId, reason) {
    const validation = this._validateCancellation(workItemId);
    if (!validation.valid) {
      return { success: false, cancelled: [], reason: validation.reason };
    }

    const workItem = validation.workItem;
    this._updateWorkItemCancelled(workItem, reason);
    this._notifyCancellation(workItem, reason);

    const cancelled = [workItemId];
    const regionCancelled = this._handleRegionCancellation(workItem, workItemId, reason);
    cancelled.push(...regionCancelled);

    const propagated = this._propagateCancellation(workItem.taskId, reason);
    cancelled.push(...propagated);

    if (propagated.length > 0) {
      this.receiptLogger.logCancellationPropagated(workItemId, propagated, reason);
    }

    return { success: true, cancelled, reason };
  }

  /**
   * @private
   */
  _validateCancellation(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) {
      return { valid: false, reason: 'work_item_not_found' };
    }

    if (workItem.state === 'cancelled' || workItem.state === 'completed') {
      return { valid: false, reason: 'already_terminal' };
    }

    return { valid: true, workItem };
  }

  /**
   * @private
   */
  _updateWorkItemCancelled(workItem, reason) {
    this._clearTimeout(workItem.id);
    workItem.state = 'cancelled';
    workItem.cancelledAt = new Date();
    workItem.cancellationReason = reason;
  }

  /**
   * @private
   */
  _notifyCancellation(workItem, reason) {
    this.receiptLogger.logCancelledWorkItem(workItem.id, reason, workItem.regionId);
    this._invokeCancellationHooks(workItem, reason);
    this._invokeCancellationCallback(workItem, reason);
  }

  /**
   * @private
   */
  _invokeCancellationCallback(workItem, reason) {
    if (this.config.onCancellation) {
      try {
        this.config.onCancellation({
          workItemId: workItem.id,
          taskId: workItem.taskId,
          reason,
          timestamp: new Date(),
        });
      } catch {
        // Ignore callback errors
      }
    }
  }

  /**
   * @private
   */
  _handleRegionCancellation(workItem, sourceWorkItemId, reason) {
    if (workItem.regionId && reason !== 'region_cancelled') {
      return this._cancelRegion(workItem.regionId, sourceWorkItemId, reason);
    }
    return [];
  }

  /**
   * @private
   */
  _cancelRegion(regionId, sourceWorkItemId, reason = 'region_cancelled') {
    const cancelled = [];
    const region = this.regionManager.getRegion(regionId);
    if (!region || !region.active) return cancelled;

    const tasksInRegion = this.regionManager.getAllTasksInRegion(regionId);
    this._cancelTasksInRegion(tasksInRegion, sourceWorkItemId, regionId, cancelled);
    this._deactivateRegionAndLog(regionId, reason, cancelled);

    return cancelled;
  }

  /**
   * @private
   */
  _cancelTasksInRegion(taskIds, sourceWorkItemId, regionId, cancelled) {
    for (const taskId of taskIds) {
      const workItemIds = this.workItemsByTask.get(taskId);
      if (!workItemIds) continue;

      for (const wiId of workItemIds) {
        if (wiId === sourceWorkItemId) continue;
        this._cancelRegionWorkItem(wiId, regionId, cancelled);
      }
    }
  }

  /**
   * @private
   */
  _cancelRegionWorkItem(workItemId, regionId, cancelled) {
    const wi = this.workItems.get(workItemId);
    if (wi && wi.state !== 'cancelled' && wi.state !== 'completed') {
      this._clearTimeout(workItemId);
      wi.state = 'cancelled';
      wi.cancelledAt = new Date();
      wi.cancellationReason = 'region_cancelled';
      this.receiptLogger.logCancelledWorkItem(workItemId, 'region_cancelled', regionId);
      this._invokeCancellationHooks(wi, 'region_cancelled');
      cancelled.push(workItemId);
    }
  }

  /**
   * @private
   */
  _deactivateRegionAndLog(regionId, reason, cancelled) {
    this.regionManager.deactivateRegion(regionId);
    this.receiptLogger.logRegionCancelled(regionId, reason, cancelled);
  }

  /**
   * @private
   */
  _propagateCancellation(taskId, reason) {
    const cancelled = [];
    const downstreamTasks = this.taskDependencies.get(taskId);
    if (!downstreamTasks) return cancelled;

    for (const downstreamTaskId of downstreamTasks) {
      this._cancelDownstreamWorkItems(downstreamTaskId, cancelled);
    }

    return cancelled;
  }

  /**
   * @private
   */
  _cancelDownstreamWorkItems(taskId, cancelled) {
    const workItemIds = this.workItemsByTask.get(taskId);
    if (!workItemIds) return;

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

  /**
   * @private
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
   * @private
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
   * @private
   */
  _handleTimeout(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'executing') return;

    const durationMs = Date.now() - workItem.startedAt.getTime();
    this._logTimeout(workItem, durationMs);
    this._invokeTimeoutCallback(workItem, durationMs);
    this.cancelWorkItem(workItemId, 'timeout');
  }

  /**
   * @private
   */
  _logTimeout(workItem, durationMs) {
    this.receiptLogger.logTimeoutOccurred(
      workItem.id,
      workItem.taskId,
      durationMs,
      workItem.timeoutMs
    );
  }

  /**
   * @private
   */
  _invokeTimeoutCallback(workItem, durationMs) {
    if (this.config.onTimeout) {
      try {
        this.config.onTimeout({
          workItemId: workItem.id,
          taskId: workItem.taskId,
          durationMs,
          timeoutMs: workItem.timeoutMs,
          timestamp: new Date(),
        });
      } catch {
        // Ignore callback errors
      }
    }
  }

  /**
   * @private
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
   * @private
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

  isTaskEnabled(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    if (!breaker) return true;
    return breaker.allowExecution();
  }

  getCircuitBreakerState(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    return breaker ? breaker.getState() : null;
  }

  // --------------------------------------------------------------------------
  // CANCELLATION HOOKS
  // --------------------------------------------------------------------------

  registerCancellationHook(hook) {
    if (typeof hook === 'function') {
      this.cancellationHooks.push(hook);
    }
  }

  /**
   * @private
   */
  _invokeCancellationHooks(workItem, reason) {
    for (const hook of this.cancellationHooks) {
      try {
        hook(workItem, reason);
      } catch {
        // Ignore hook errors
      }
    }
  }

  // --------------------------------------------------------------------------
  // TASK DEPENDENCIES
  // --------------------------------------------------------------------------

  setTaskDependencies(taskId, downstreamTaskIds) {
    this.taskDependencies.set(taskId, downstreamTaskIds);
  }

  getTaskDependencies(taskId) {
    return this.taskDependencies.get(taskId) || [];
  }

  // --------------------------------------------------------------------------
  // QUERYING & TIME-TRAVEL
  // --------------------------------------------------------------------------

  getWorkItem(workItemId) {
    return this.workItems.get(workItemId);
  }

  getWorkItemsForTask(taskId) {
    const ids = this.workItemsByTask.get(taskId);
    if (!ids) return [];
    return Array.from(ids)
      .map(id => this.workItems.get(id))
      .filter(wi => wi !== undefined);
  }

  getWorkItemsForCase(caseId) {
    const ids = this.workItemsByCase.get(caseId);
    if (!ids) return [];
    return Array.from(ids)
      .map(id => this.workItems.get(id))
      .filter(wi => wi !== undefined);
  }

  getStateAtTime(timestamp) {
    const receipts = this.receiptLogger.getReceiptsAtTime(timestamp);
    const state = this._buildEmptyState();
    this._processReceipts(receipts, state);

    return {
      timestamp,
      cancelledWorkItems: Array.from(state.cancelledWorkItems),
      openCircuitBreakers: Array.from(state.openCircuitBreakers),
      cancelledRegions: Array.from(state.cancelledRegions),
      receiptCount: receipts.length,
    };
  }

  /**
   * @private
   */
  _buildEmptyState() {
    return {
      cancelledWorkItems: new Set(),
      openCircuitBreakers: new Set(),
      cancelledRegions: new Set(),
    };
  }

  /**
   * @private
   */
  _processReceipts(receipts, state) {
    for (const receipt of receipts) {
      switch (receipt.type) {
        case 'CANCELLED_WORK_ITEM':
          state.cancelledWorkItems.add(receipt.payload.workItemId);
          break;
        case 'CIRCUIT_BREAKER_OPEN':
          state.openCircuitBreakers.add(receipt.payload.taskId);
          break;
        case 'CIRCUIT_BREAKER_CLOSED':
          state.openCircuitBreakers.delete(receipt.payload.taskId);
          break;
        case 'REGION_CANCELLED':
          state.cancelledRegions.add(receipt.payload.regionId);
          break;
      }
    }
  }

  // --------------------------------------------------------------------------
  // STATISTICS
  // --------------------------------------------------------------------------

  getStats() {
    const workItemsByState = this._computeWorkItemsByState();
    const circuitBreakerStats = this._computeCircuitBreakerStats();

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

  /**
   * @private
   */
  _computeWorkItemsByState() {
    const byState = {};
    for (const wi of this.workItems.values()) {
      byState[wi.state] = (byState[wi.state] || 0) + 1;
    }
    return byState;
  }

  /**
   * @private
   */
  _computeCircuitBreakerStats() {
    const stats = {};
    for (const [taskId, breaker] of this.circuitBreakers) {
      stats[taskId] = breaker.getState();
    }
    return stats;
  }

  // --------------------------------------------------------------------------
  // CLEANUP
  // --------------------------------------------------------------------------

  terminate(reason = 'workflow_terminated') {
    for (const [id, workItem] of this.workItems) {
      if (workItem.state === 'executing' || workItem.state === 'enabled' || workItem.state === 'pending') {
        this.cancelWorkItem(id, reason);
      }
    }

    for (const handle of this.timeoutHandles.values()) {
      clearTimeout(handle);
    }
    this.timeoutHandles.clear();
  }

  export() {
    return {
      workItems: this._exportWorkItems(),
      regions: this.regionManager.export(),
      receipts: this.receiptLogger.export(),
      dependencies: Object.fromEntries(this.taskDependencies),
      circuitBreakers: this._exportCircuitBreakers(),
    };
  }

  /**
   * @private
   */
  _exportWorkItems() {
    return Array.from(this.workItems.values()).map(wi => ({
      ...wi,
      createdAt: wi.createdAt.toISOString(),
      startedAt: wi.startedAt?.toISOString(),
      completedAt: wi.completedAt?.toISOString(),
      cancelledAt: wi.cancelledAt?.toISOString(),
    }));
  }

  /**
   * @private
   */
  _exportCircuitBreakers() {
    return Object.fromEntries(
      Array.from(this.circuitBreakers.entries()).map(([k, v]) => [k, v.getState()])
    );
  }

  clear() {
    this.terminate();
    this.workItems.clear();
    this.workItemsByTask.clear();
    this.workItemsByCase.clear();
    this.circuitBreakers.clear();
    this.taskDependencies.clear();
    this.receiptLogger.receipts = [];
    this.receiptLogger.receiptsByWorkItem.clear();
    this.receiptLogger.receiptsByTask.clear();
    this.regionManager.regions.clear();
    this.regionManager.taskToRegions.clear();
  }
}

/**
 * Factory function to create a cancellation manager
 */
export function createCancellationManager(config = {}) {
  return new YawlCancellationManager(config);
}
