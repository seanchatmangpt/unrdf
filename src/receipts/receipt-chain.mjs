/**
 * @fileoverview Receipt Chain - Link receipts across epochs
 *
 * **Purpose**: Maintain cryptographic chain of receipts
 * - beforeHash → afterHash linkage
 * - Verifies temporal ordering
 * - Detects tampering (broken chain)
 * - Enables audit trail reconstruction
 *
 * **Invariants**:
 * - Chain[i].beforeHash === Chain[i-1].receiptHash
 * - Chain is append-only (no modifications)
 * - Epochs are monotonically increasing
 *
 * @module receipts/receipt-chain
 */

import { Receipt } from './receipt.mjs';

/**
 * Receipt Chain - Ordered sequence of linked receipts
 *
 * @example
 * const chain = new ReceiptChain();
 * await chain.append(receipt1);
 * await chain.append(receipt2); // receipt2.beforeHash === receipt1.receiptHash
 */
export class ReceiptChain {
  /**
   * Create a new receipt chain
   */
  constructor() {
    /** @type {Receipt[]} */
    this.receipts = [];

    /** @type {Map<string, Receipt>} */
    this.receiptsByHash = new Map();

    /** @type {Map<string, Receipt>} */
    this.receiptsByEpoch = new Map();
  }

  /**
   * Append a receipt to the chain
   *
   * **Validation**:
   * - If chain non-empty: receipt.beforeHash must match last receipt hash
   * - If chain empty: receipt.beforeHash should be null
   * - Receipt hash must be valid (verify())
   *
   * @param {Receipt} receipt - Receipt to append
   * @throws {Error} If chain linkage is broken
   */
  async append(receipt) {
    // Verify receipt hash
    const isValid = await receipt.verify();
    if (!isValid) {
      throw new Error('Receipt hash verification failed');
    }

    // Check linkage
    if (this.receipts.length === 0) {
      // First receipt - beforeHash should be null
      if (receipt.beforeHash !== null) {
        throw new Error(
          `First receipt must have beforeHash=null, got ${receipt.beforeHash}`
        );
      }
    } else {
      // Subsequent receipt - beforeHash must match last receipt
      const lastReceipt = this.receipts[this.receipts.length - 1];
      if (receipt.beforeHash !== lastReceipt.receiptHash) {
        throw new Error(
          `Receipt chain broken: expected beforeHash=${lastReceipt.receiptHash}, got ${receipt.beforeHash}`
        );
      }
    }

    // Check epoch ordering (monotonic increase)
    if (this.receipts.length > 0) {
      const lastEpoch = this.receipts[this.receipts.length - 1].epoch;
      if (receipt.epoch <= lastEpoch) {
        throw new Error(
          `Epoch must increase: last=${lastEpoch}, current=${receipt.epoch}`
        );
      }
    }

    // Append to chain
    this.receipts.push(receipt);
    this.receiptsByHash.set(receipt.receiptHash, receipt);
    this.receiptsByEpoch.set(receipt.epoch, receipt);
  }

  /**
   * Verify entire chain integrity
   *
   * **Checks**:
   * - All receipts have valid hashes
   * - All links are correct (beforeHash → receiptHash)
   * - Epochs are monotonically increasing
   *
   * @returns {Promise<{valid: boolean, errors: string[]}>}
   */
  async verify() {
    const errors = [];

    for (let i = 0; i < this.receipts.length; i++) {
      const receipt = this.receipts[i];

      // Verify hash
      const isValid = await receipt.verify();
      if (!isValid) {
        errors.push(`Receipt ${i} (${receipt.epoch}): invalid hash`);
      }

      // Verify linkage
      if (i === 0) {
        if (receipt.beforeHash !== null) {
          errors.push(`Receipt ${i} (${receipt.epoch}): first receipt must have beforeHash=null`);
        }
      } else {
        const prevReceipt = this.receipts[i - 1];
        if (receipt.beforeHash !== prevReceipt.receiptHash) {
          errors.push(
            `Receipt ${i} (${receipt.epoch}): broken chain link (expected ${prevReceipt.receiptHash}, got ${receipt.beforeHash})`
          );
        }
      }

      // Verify epoch ordering
      if (i > 0) {
        const prevEpoch = this.receipts[i - 1].epoch;
        if (receipt.epoch <= prevEpoch) {
          errors.push(
            `Receipt ${i} (${receipt.epoch}): epoch not increasing (previous: ${prevEpoch})`
          );
        }
      }
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }

  /**
   * Get receipt by hash
   *
   * @param {string} hash - Receipt hash
   * @returns {Receipt|undefined}
   */
  getByHash(hash) {
    return this.receiptsByHash.get(hash);
  }

  /**
   * Get receipt by epoch
   *
   * @param {string} epoch - Epoch string (τ_YYYY_MM_DD_HHMM_nnn)
   * @returns {Receipt|undefined}
   */
  getByEpoch(epoch) {
    return this.receiptsByEpoch.get(epoch);
  }

  /**
   * Get all receipts in chronological order
   *
   * @returns {Receipt[]}
   */
  getAll() {
    return [...this.receipts];
  }

  /**
   * Get receipts in a time range
   *
   * @param {string} startEpoch - Start epoch (inclusive)
   * @param {string} endEpoch - End epoch (inclusive)
   * @returns {Receipt[]}
   */
  getRange(startEpoch, endEpoch) {
    return this.receipts.filter(r => r.epoch >= startEpoch && r.epoch <= endEpoch);
  }

  /**
   * Get chain length
   *
   * @returns {number}
   */
  get length() {
    return this.receipts.length;
  }

  /**
   * Get last receipt in chain
   *
   * @returns {Receipt|undefined}
   */
  getLast() {
    return this.receipts[this.receipts.length - 1];
  }

  /**
   * Export chain to JSON-LD
   *
   * @returns {object} JSON-LD representation
   */
  toJSONLD() {
    return {
      '@context': {
        unrdf: 'https://unrdf.org/vocab#',
        xsd: 'http://www.w3.org/2001/XMLSchema#',
      },
      '@type': 'unrdf:ReceiptChain',
      'unrdf:length': this.receipts.length,
      'unrdf:receipts': this.receipts.map(r => r.toJSONLD()),
    };
  }

  /**
   * Import chain from JSON-LD
   *
   * @param {object} jsonld - JSON-LD representation
   * @returns {Promise<ReceiptChain>}
   */
  static async fromJSONLD(jsonld) {
    const chain = new ReceiptChain();

    for (const receiptJson of jsonld['unrdf:receipts']) {
      const receipt = Receipt.fromJSONLD(receiptJson);
      await chain.append(receipt);
    }

    return chain;
  }
}
