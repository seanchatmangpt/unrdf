/**
 * @file Receipt Tracker
 * @module @unrdf/event-automation/receipt-tracker
 * @description Manages receipt generation and tracking for delta operations
 */

import { ReceiptSchema } from './schemas.mjs';
import { sha256 } from 'hash-wasm';

/**
 * Receipt tracker for managing receipts
 */
export class ReceiptTracker {
  /**
   * Creates a new receipt tracker
   * @param {Object} [options] - Tracker options
   * @param {Object} [options.logger] - Logger instance
   * @param {number} [options.maxReceipts=10000] - Maximum receipts to keep
   */
  constructor(options = {}) {
    this.logger = options.logger || console;
    this.maxReceipts = options.maxReceipts || 10000;
    this.receipts = new Map();
    this.deltaToReceipts = new Map();
    this.receiptCounter = 0;
    this.metrics = {
      totalCreated: 0,
      totalVerified: 0,
      totalInvalid: 0,
    };
  }

  /**
   * Create a receipt for a delta
   * @param {Object} delta - Delta to create receipt for
   * @param {Object} [options] - Receipt options
   * @param {string} [options.operation='process'] - Operation type
   * @param {string} [options.entityType='Delta'] - Entity type
   * @returns {Promise<Object>} Created receipt
   */
  async createReceipt(delta, options = {}) {
    const startTime = performance.now();

    try {
      const receiptId = `receipt-${delta.id}-${Date.now()}-${this.receiptCounter++}`;
      const operation = options.operation || 'process';
      const entityType = options.entityType || 'Delta';

      // Generate receipt hash
      const hash = await this._generateReceiptHash({
        deltaId: delta.id,
        operation,
        entityType,
        timestamp: Date.now(),
      });

      const receipt = {
        id: receiptId,
        deltaId: delta.id,
        operation,
        entityType,
        timestamp: Date.now(),
        hash,
        metadata: {
          duration: performance.now() - startTime,
          operationCount: delta.operations?.length || 0,
          ...options.metadata,
        },
      };

      // Validate receipt
      const validated = ReceiptSchema.parse(receipt);

      // Store receipt
      this.receipts.set(receiptId, validated);

      // Map delta to receipt
      if (!this.deltaToReceipts.has(delta.id)) {
        this.deltaToReceipts.set(delta.id, []);
      }
      this.deltaToReceipts.get(delta.id).push(receiptId);

      // Enforce max receipts limit
      this._enforceMaxReceipts();

      // Update metrics
      this.metrics.totalCreated++;

      return validated;
    } catch (error) {
      this.logger.error('Receipt creation failed:', {
        deltaId: delta?.id,
        error: error.message,
      });
      throw error;
    }
  }

  /**
   * Verify a receipt
   * @param {Object} receipt - Receipt to verify
   * @returns {Promise<boolean>} Whether receipt is valid
   */
  async verifyReceipt(receipt) {
    try {
      // Validate structure
      ReceiptSchema.parse(receipt);

      // Verify hash
      const expectedHash = await this._generateReceiptHash({
        deltaId: receipt.deltaId,
        operation: receipt.operation,
        entityType: receipt.entityType,
        timestamp: receipt.timestamp,
      });

      const isValid = expectedHash === receipt.hash;

      if (isValid) {
        this.metrics.totalVerified++;
      } else {
        this.metrics.totalInvalid++;
      }

      return isValid;
    } catch (error) {
      this.logger.error('Receipt verification failed:', {
        receiptId: receipt?.id,
        error: error.message,
      });
      this.metrics.totalInvalid++;
      return false;
    }
  }

  /**
   * Get receipt by ID
   * @param {string} receiptId - Receipt ID
   * @returns {Object|undefined} Receipt or undefined
   */
  getReceipt(receiptId) {
    return this.receipts.get(receiptId);
  }

  /**
   * Get all receipts for a delta
   * @param {string} deltaId - Delta ID
   * @returns {Array<Object>} Receipts for delta
   */
  getReceiptsForDelta(deltaId) {
    const receiptIds = this.deltaToReceipts.get(deltaId) || [];
    return receiptIds.map((id) => this.receipts.get(id)).filter(Boolean);
  }

  /**
   * Get all receipts
   * @returns {Array<Object>} All receipts
   */
  getAllReceipts() {
    return Array.from(this.receipts.values());
  }

  /**
   * Generate hash for receipt
   * @private
   * @param {Object} data - Receipt data
   * @returns {Promise<string>} Receipt hash
   */
  async _generateReceiptHash(data) {
    const content = JSON.stringify({
      deltaId: data.deltaId,
      operation: data.operation,
      entityType: data.entityType,
      timestamp: data.timestamp,
    });
    return sha256(content);
  }

  /**
   * Enforce maximum receipts limit
   * @private
   */
  _enforceMaxReceipts() {
    if (this.receipts.size > this.maxReceipts) {
      // Remove oldest receipts (FIFO)
      const toRemove = this.receipts.size - this.maxReceipts;
      const entries = Array.from(this.receipts.entries());

      for (let i = 0; i < toRemove; i++) {
        const [receiptId, receipt] = entries[i];
        this.receipts.delete(receiptId);

        // Clean up delta mapping
        const deltaReceipts = this.deltaToReceipts.get(receipt.deltaId);
        if (deltaReceipts) {
          const index = deltaReceipts.indexOf(receiptId);
          if (index > -1) {
            deltaReceipts.splice(index, 1);
          }
          if (deltaReceipts.length === 0) {
            this.deltaToReceipts.delete(receipt.deltaId);
          }
        }
      }
    }
  }

  /**
   * Get tracker metrics
   * @returns {Object} Tracker metrics
   */
  getMetrics() {
    return {
      totalReceipts: this.receipts.size,
      totalCreated: this.metrics.totalCreated,
      totalVerified: this.metrics.totalVerified,
      totalInvalid: this.metrics.totalInvalid,
      totalDeltas: this.deltaToReceipts.size,
    };
  }

  /**
   * Reset tracker state
   */
  reset() {
    this.receipts.clear();
    this.deltaToReceipts.clear();
    this.receiptCounter = 0;
    this.metrics = {
      totalCreated: 0,
      totalVerified: 0,
      totalInvalid: 0,
    };
  }
}

/**
 * Create a receipt tracker instance
 * @param {Object} [options] - Tracker options
 * @returns {ReceiptTracker} Receipt tracker instance
 */
export function createReceiptTracker(options = {}) {
  return new ReceiptTracker(options);
}
