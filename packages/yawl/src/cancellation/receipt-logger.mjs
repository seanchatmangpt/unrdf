/**
 * @file Cancellation Receipt Logger
 * @module yawl/cancellation/receipt-logger
 *
 * @description
 * Logs cancellation receipts for auditability and time-travel.
 */

import { createReceipt } from './schemas.mjs';

/**
 * Logs cancellation receipts for auditability and time-travel
 */
export class CancellationReceiptLogger {
  constructor() {
    /** @type {import('./schemas.mjs').CancellationReceipt[]} */
    this.receipts = [];
    /** @type {Map<string, import('./schemas.mjs').CancellationReceipt[]>} Work item ID -> receipts */
    this.receiptsByWorkItem = new Map();
    /** @type {Map<string, import('./schemas.mjs').CancellationReceipt[]>} Task ID -> receipts */
    this.receiptsByTask = new Map();
  }

  /**
   * Log a cancellation receipt
   * @param {string} type - Receipt type
   * @param {Object} payload - Receipt payload
   * @returns {import('./schemas.mjs').CancellationReceipt}
   */
  log(type, payload) {
    const receipt = createReceipt(type, payload);
    this.receipts.push(receipt);
    this._indexReceipt(receipt, payload);
    return receipt;
  }

  /**
   * Index receipt by work item and task
   * @param {import('./schemas.mjs').CancellationReceipt} receipt
   * @param {Object} payload
   * @private
   */
  _indexReceipt(receipt, payload) {
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
  }

  /**
   * Log CANCELLED_WORK_ITEM receipt
   * @param {string} workItemId
   * @param {string} reason
   * @param {string} [regionId]
   * @returns {import('./schemas.mjs').CancellationReceipt}
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
   * Log TIMEOUT_OCCURRED receipt
   * @param {string} workItemId
   * @param {string} taskId
   * @param {number} durationMs
   * @param {number} timeoutMs
   * @returns {import('./schemas.mjs').CancellationReceipt}
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
   * Log CIRCUIT_BREAKER_OPEN receipt
   * @param {string} taskId
   * @param {number} failureCount
   * @returns {import('./schemas.mjs').CancellationReceipt}
   */
  logCircuitBreakerOpen(taskId, failureCount) {
    return this.log('CIRCUIT_BREAKER_OPEN', {
      taskId,
      failureCount,
      disabledAt: new Date().toISOString(),
    });
  }

  /**
   * Log CIRCUIT_BREAKER_CLOSED receipt
   * @param {string} taskId
   * @returns {import('./schemas.mjs').CancellationReceipt}
   */
  logCircuitBreakerClosed(taskId) {
    return this.log('CIRCUIT_BREAKER_CLOSED', {
      taskId,
      enabledAt: new Date().toISOString(),
    });
  }

  /**
   * Log REGION_CANCELLED receipt
   * @param {string} regionId
   * @param {string} reason
   * @param {string[]} affectedWorkItems
   * @returns {import('./schemas.mjs').CancellationReceipt}
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
   * Log CANCELLATION_PROPAGATED receipt
   * @param {string} sourceWorkItemId
   * @param {string[]} propagatedTo
   * @param {string} reason
   * @returns {import('./schemas.mjs').CancellationReceipt}
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
   * Get receipts for a work item
   * @param {string} workItemId
   * @returns {import('./schemas.mjs').CancellationReceipt[]}
   */
  getReceiptsForWorkItem(workItemId) {
    return this.receiptsByWorkItem.get(workItemId) || [];
  }

  /**
   * Get receipts for a task
   * @param {string} taskId
   * @returns {import('./schemas.mjs').CancellationReceipt[]}
   */
  getReceiptsForTask(taskId) {
    return this.receiptsByTask.get(taskId) || [];
  }

  /**
   * Get all receipts
   * @returns {import('./schemas.mjs').CancellationReceipt[]}
   */
  getAllReceipts() {
    return [...this.receipts];
  }

  /**
   * Get receipts at a specific point in time
   * @param {Date} timestamp
   * @returns {import('./schemas.mjs').CancellationReceipt[]}
   */
  getReceiptsAtTime(timestamp) {
    const targetTime = timestamp.getTime();
    return this.receipts.filter(r => new Date(r.timestamp).getTime() <= targetTime);
  }

  /**
   * Get receipts in a time range
   * @param {Date} from
   * @param {Date} to
   * @returns {import('./schemas.mjs').CancellationReceipt[]}
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
   * Export receipts for serialization
   * @returns {import('./schemas.mjs').CancellationReceipt[]}
   */
  export() {
    return this.receipts.map(r => ({
      ...r,
      timestamp: r.timestamp.toISOString(),
    }));
  }

  /**
   * Import receipts from serialized data
   * @param {Object[]} data
   */
  import(data) {
    this.receipts = [];
    this.receiptsByWorkItem.clear();
    this.receiptsByTask.clear();

    for (const receipt of data) {
      this._importReceipt(receipt);
    }
  }

  /**
   * Import single receipt
   * @param {Object} receipt
   * @private
   */
  _importReceipt(receipt) {
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
