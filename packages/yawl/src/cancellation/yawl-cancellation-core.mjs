/**
 * @file YAWL Cancellation Core - Work Items & Circuit Breakers
 * @module yawl/cancellation/core
 *
 * @description
 * Core cancellation infrastructure including:
 * - Work item lifecycle management
 * - Circuit breakers for task-level failure management
 * - Cancellation receipt logging for auditability
 * - Timeout enforcement
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for cancellation reason
 */
export const CancellationReasonSchema = z.enum([
  'timeout',
  'manual',
  'circuit_breaker',
  'parent_cancelled',
  'region_cancelled',
  'task_disabled',
  'dependency_failed',
  'workflow_terminated',
]);

/**
 * Schema for work item state
 */
export const WorkItemStateSchema = z.enum([
  'pending',
  'enabled',
  'executing',
  'completed',
  'cancelled',
  'failed',
]);

/**
 * Schema for work item
 */
export const WorkItemSchema = z.object({
  id: z.string().uuid(),
  taskId: z.string().min(1),
  caseId: z.string().uuid(),
  regionId: z.string().uuid().optional(),
  state: WorkItemStateSchema,
  createdAt: z.coerce.date(),
  startedAt: z.coerce.date().optional(),
  completedAt: z.coerce.date().optional(),
  cancelledAt: z.coerce.date().optional(),
  cancellationReason: CancellationReasonSchema.optional(),
  timeoutMs: z.number().int().positive().max(300000).default(30000),
  retryCount: z.number().int().nonnegative().default(0),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for circuit breaker state
 */
export const CircuitBreakerStateSchema = z.enum(['closed', 'open', 'half_open']);

/**
 * Valid receipt types
 * @type {readonly string[]}
 */
export const VALID_RECEIPT_TYPES = Object.freeze([
  'CANCELLED_WORK_ITEM',
  'TIMEOUT_OCCURRED',
  'CIRCUIT_BREAKER_OPEN',
  'CIRCUIT_BREAKER_CLOSED',
  'REGION_CANCELLED',
  'TASK_DISABLED',
  'TASK_ENABLED',
  'CANCELLATION_PROPAGATED',
]);

/**
 * Schema for cancellation receipt
 */
export const CancellationReceiptSchema = z.object({
  id: z.string(),
  type: z.string(),
  timestamp: z.any(),
  payload: z.record(z.string(), z.any()),
});

/**
 * @typedef {z.infer<typeof WorkItemSchema>} WorkItem
 * @typedef {z.infer<typeof CancellationReceiptSchema>} CancellationReceipt
 * @typedef {z.infer<typeof CancellationReasonSchema>} CancellationReason
 */

/**
 * Create a validated receipt object
 * @param {string} type
 * @param {Object} payload
 * @returns {Object}
 */
export function createReceipt(type, payload) {
  if (!VALID_RECEIPT_TYPES.includes(type)) {
    throw new Error(`Invalid receipt type: ${type}`);
  }
  return {
    id: randomUUID(),
    type,
    timestamp: new Date(),
    payload,
  };
}

// ============================================================================
// CIRCUIT BREAKER
// ============================================================================

/**
 * Circuit breaker for task-level failure management
 */
export class TaskCircuitBreaker {
  /**
   * @param {Object} config
   * @param {number} [config.failureThreshold=3]
   * @param {number} [config.resetTimeout=60000]
   * @param {number} [config.halfOpenMaxCalls=1]
   * @param {Function} [config.onStateChange]
   */
  constructor(config = {}) {
    this.taskId = config.taskId || 'unknown';
    this.failureThreshold = config.failureThreshold ?? 3;
    this.resetTimeout = config.resetTimeout ?? 60000;
    this.halfOpenMaxCalls = config.halfOpenMaxCalls ?? 1;
    this.onStateChange = config.onStateChange ?? null;

    this.state = 'closed';
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.lastFailureTime = null;
    this.lastStateChange = Date.now();
    this.disabledAt = null;
  }

  /**
   *
   */
  recordSuccess() {
    if (this.state === 'half_open') {
      this.successCount++;
      if (this.successCount >= 1) {
        this._transition('closed');
        this.failureCount = 0;
        this.successCount = 0;
        this.halfOpenCalls = 0;
      }
    } else if (this.state === 'closed') {
      this.failureCount = 0;
    }
  }

  /**
   * @returns {boolean} True if circuit tripped
   */
  recordFailure() {
    this.failureCount++;
    this.lastFailureTime = Date.now();

    if (this.state === 'half_open') {
      this._transition('open');
      return true;
    }

    if (this.state === 'closed' && this.failureCount >= this.failureThreshold) {
      this._transition('open');
      this.disabledAt = new Date();
      return true;
    }

    return false;
  }

  /**
   * @returns {boolean} True if execution allowed
   */
  allowExecution() {
    this._checkTransition();

    if (this.state === 'open') {
      return false;
    }

    if (this.state === 'half_open') {
      if (this.halfOpenCalls >= this.halfOpenMaxCalls) {
        return false;
      }
      this.halfOpenCalls++;
    }

    return true;
  }

  /**
   *
   */
  reset() {
    this._transition('closed');
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.disabledAt = null;
  }

  /**
   *
   */
  getState() {
    return {
      taskId: this.taskId,
      state: this.state,
      failureCount: this.failureCount,
      failureThreshold: this.failureThreshold,
      lastFailureTime: this.lastFailureTime,
      lastStateChange: this.lastStateChange,
      disabledAt: this.disabledAt,
      isOpen: this.state === 'open',
    };
  }

  /**
   *
   */
  _checkTransition() {
    if (this.state === 'open') {
      const elapsed = Date.now() - this.lastStateChange;
      if (elapsed >= this.resetTimeout) {
        this._transition('half_open');
      }
    }
  }

  /**
   *
   */
  _transition(newState) {
    const previousState = this.state;
    this.state = newState;
    this.lastStateChange = Date.now();

    if (this.onStateChange) {
      try {
        this.onStateChange({
          taskId: this.taskId,
          from: previousState,
          to: newState,
          timestamp: new Date(),
          failureCount: this.failureCount,
        });
      } catch {
        // Ignore callback errors
      }
    }
  }
}

// ============================================================================
// RECEIPT LOGGER
// ============================================================================

/**
 * Logs cancellation receipts for auditability and time-travel
 */
export class CancellationReceiptLogger {
  /**
   *
   */
  constructor() {
    /** @type {CancellationReceipt[]} */
    this.receipts = [];
    /** @type {Map<string, CancellationReceipt[]>} */
    this.receiptsByWorkItem = new Map();
    /** @type {Map<string, CancellationReceipt[]>} */
    this.receiptsByTask = new Map();
  }

  /**
   * @param {string} type
   * @param {Object} payload
   * @returns {CancellationReceipt}
   */
  log(type, payload) {
    const receipt = createReceipt(type, payload);

    this.receipts.push(receipt);

    if (payload.workItemId) {
      if (!this.receiptsByWorkItem.has(payload.workItemId)) {
        this.receiptsByWorkItem.set(payload.workItemId, []);
      }
      this.receiptsByWorkItem.get(payload.workItemId).push(receipt);
    }

    if (payload.taskId) {
      if (!this.receiptsByTask.has(payload.taskId)) {
        this.receiptsByTask.set(payload.taskId, []);
      }
      this.receiptsByTask.get(payload.taskId).push(receipt);
    }

    return receipt;
  }

  /**
   *
   */
  logCancelledWorkItem(workItemId, reason, regionId) {
    return this.log('CANCELLED_WORK_ITEM', {
      workItemId,
      reason,
      cancelledAt: new Date().toISOString(),
      region: regionId,
    });
  }

  /**
   *
   */
  logTimeoutOccurred(workItemId, taskId, durationMs, timeoutMs) {
    return this.log('TIMEOUT_OCCURRED', {
      workItemId,
      taskId,
      durationMs,
      timeoutMs,
      occurredAt: new Date().toISOString(),
    });
  }

  /**
   *
   */
  logCircuitBreakerOpen(taskId, failureCount) {
    return this.log('CIRCUIT_BREAKER_OPEN', {
      taskId,
      failureCount,
      disabledAt: new Date().toISOString(),
    });
  }

  /**
   *
   */
  logCircuitBreakerClosed(taskId) {
    return this.log('CIRCUIT_BREAKER_CLOSED', {
      taskId,
      enabledAt: new Date().toISOString(),
    });
  }

  /**
   *
   */
  logRegionCancelled(regionId, reason, affectedWorkItems) {
    return this.log('REGION_CANCELLED', {
      regionId,
      reason,
      affectedWorkItems,
      cancelledAt: new Date().toISOString(),
    });
  }

  /**
   *
   */
  logCancellationPropagated(sourceWorkItemId, propagatedTo, reason) {
    return this.log('CANCELLATION_PROPAGATED', {
      sourceWorkItemId,
      propagatedTo,
      reason,
      propagatedAt: new Date().toISOString(),
    });
  }

  /**
   *
   */
  getReceiptsForWorkItem(workItemId) {
    return this.receiptsByWorkItem.get(workItemId) || [];
  }

  /**
   *
   */
  getReceiptsForTask(taskId) {
    return this.receiptsByTask.get(taskId) || [];
  }

  /**
   *
   */
  getAllReceipts() {
    return [...this.receipts];
  }

  /**
   *
   */
  getReceiptsAtTime(timestamp) {
    const targetTime = timestamp.getTime();
    return this.receipts.filter(r => new Date(r.timestamp).getTime() <= targetTime);
  }

  /**
   *
   */
  getReceiptsInRange(from, to) {
    const fromTime = from.getTime();
    const toTime = to.getTime();
    return this.receipts.filter(r => {
      const time = new Date(r.timestamp).getTime();
      return time >= fromTime && time <= toTime;
    });
  }

  /**
   *
   */
  export() {
    return this.receipts.map(r => ({
      ...r,
      timestamp: r.timestamp.toISOString(),
    }));
  }

  /**
   *
   */
  import(data) {
    this.receipts = [];
    this.receiptsByWorkItem.clear();
    this.receiptsByTask.clear();

    for (const receipt of data) {
      const parsed = {
        id: receipt.id,
        type: receipt.type,
        timestamp: new Date(receipt.timestamp),
        payload: receipt.payload,
      };
      this.receipts.push(parsed);

      if (parsed.payload.workItemId) {
        if (!this.receiptsByWorkItem.has(parsed.payload.workItemId)) {
          this.receiptsByWorkItem.set(parsed.payload.workItemId, []);
        }
        this.receiptsByWorkItem.get(parsed.payload.workItemId).push(parsed);
      }

      if (parsed.payload.taskId) {
        if (!this.receiptsByTask.has(parsed.payload.taskId)) {
          this.receiptsByTask.set(parsed.payload.taskId, []);
        }
        this.receiptsByTask.get(parsed.payload.taskId).push(parsed);
      }
    }
  }

  /**
   *
   */
  clear() {
    this.receipts = [];
    this.receiptsByWorkItem.clear();
    this.receiptsByTask.clear();
  }
}
