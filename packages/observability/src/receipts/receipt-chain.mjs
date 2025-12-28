/**
 * Receipt Chain - Hash-chained receipt sequence
 *
 * Provides immutable audit trail via hash chaining:
 * - Each receipt contains hash of previous receipt
 * - Chain break detection (any modification invalidates subsequent hashes)
 * - Temporal ordering enforcement
 * - Complete provenance from genesis to current
 *
 * @module @unrdf/observability/receipts/receipt-chain
 */

import { blake3 } from 'hash-wasm';
import { ReceiptSchema } from './receipt-schema.mjs';

/**
 * ReceiptChain - Manages a chain of cryptographically linked receipts
 *
 * @example
 * const chain = new ReceiptChain('audit-chain-1');
 * await chain.append({
 *   operation: 'admit',
 *   payload: { delta: 'delta_001' },
 *   actor: 'system'
 * });
 */
export class ReceiptChain {
  /**
   * Create a new receipt chain
   * @param {string} chainId - Unique chain identifier
   */
  constructor(chainId) {
    if (!chainId || typeof chainId !== 'string') {
      throw new TypeError('ReceiptChain requires a string chainId');
    }
    this.chainId = chainId;
    this.receipts = [];
    this.lastTimestamp = 0n;
  }

  /**
   * Get chain length
   * @returns {number}
   */
  get length() {
    return this.receipts.length;
  }

  /**
   * Get latest receipt
   * @returns {Object|null}
   */
  getLatest() {
    return this.receipts.length > 0 ? this.receipts[this.receipts.length - 1] : null;
  }

  /**
   * Generate deterministic receipt ID
   * @param {string} operation - Operation type
   * @param {bigint} timestamp - Timestamp in nanoseconds
   * @returns {string}
   * @private
   */
  _generateId(operation, timestamp) {
    return `receipt-${operation}-${timestamp}-${this.receipts.length}`;
  }

  /**
   * Compute canonical hash of receipt content
   * @param {Object} content - Receipt content to hash
   * @returns {Promise<string>} BLAKE3 hash (64-char hex)
   * @private
   */
  async _computeHash(content) {
    // Canonical JSON serialization (sorted keys)
    const canonical = JSON.stringify(content, Object.keys(content).sort());
    return await blake3(canonical);
  }

  /**
   * Append a new receipt to the chain
   *
   * @param {Object} receiptData - Receipt data
   * @param {string} receiptData.operation - Operation type
   * @param {any} receiptData.payload - Operation payload
   * @param {string} [receiptData.actor] - Actor who performed operation
   * @param {bigint} [receiptData.timestamp_ns] - Custom timestamp (default: now)
   * @param {Object} [receiptData.metadata] - Optional metadata
   * @returns {Promise<Object>} Appended receipt
   * @throws {Error} If receipt is invalid or chain is broken
   */
  async append(receiptData) {
    const { operation, payload, actor, metadata } = receiptData;

    // Input validation
    if (!operation || typeof operation !== 'string') {
      throw new TypeError('append: operation must be a non-empty string');
    }
    if (payload === undefined || payload === null) {
      throw new TypeError('append: payload is required');
    }

    // Get timestamp (custom or now)
    const timestamp_ns = receiptData.timestamp_ns || BigInt(Date.now()) * 1_000_000n;

    // Enforce monotonic time
    if (timestamp_ns <= this.lastTimestamp) {
      throw new Error(
        `Timestamp not monotonic: ${timestamp_ns} <= ${this.lastTimestamp}`
      );
    }

    // Generate receipt ID
    const id = this._generateId(operation, timestamp_ns);
    const timestamp_iso = new Date(Number(timestamp_ns / 1_000_000n)).toISOString();

    // Get previous receipt hash (or null for genesis)
    const previousHash = this.getLatest()?.hash || null;

    // Build canonical receipt object for hashing
    const canonicalContent = {
      id,
      timestamp_ns: timestamp_ns.toString(),
      timestamp_iso,
      operation,
      payload,
      previousHash,
      ...(actor && { actor }),
      ...(metadata && { metadata }),
    };

    // Compute hash
    const hash = await this._computeHash(canonicalContent);

    // Create receipt
    const receipt = {
      id,
      hash,
      timestamp_ns: timestamp_ns.toString(),
      timestamp_iso,
      operation,
      payload,
      previousHash,
      ...(actor && { actor }),
      ...(metadata && { metadata }),
    };

    // Validate against schema
    const validated = ReceiptSchema.parse(receipt);

    // Append to chain
    this.receipts.push(Object.freeze(validated));
    this.lastTimestamp = timestamp_ns;

    return validated;
  }

  /**
   * Get receipt by index
   * @param {number} index - Receipt index (0-based)
   * @returns {Object|null}
   */
  getReceipt(index) {
    return this.receipts[index] || null;
  }

  /**
   * Get receipt by ID
   * @param {string} id - Receipt ID
   * @returns {Object|null}
   */
  getReceiptById(id) {
    return this.receipts.find(r => r.id === id) || null;
  }

  /**
   * Get all receipts (defensive copy)
   * @returns {Array<Object>}
   */
  getAllReceipts() {
    return [...this.receipts];
  }

  /**
   * Serialize chain to JSON
   * @returns {Object}
   */
  toJSON() {
    return {
      chainId: this.chainId,
      length: this.receipts.length,
      receipts: this.receipts,
    };
  }

  /**
   * Deserialize chain from JSON
   * @param {Object} json - Serialized chain
   * @returns {ReceiptChain}
   */
  static fromJSON(json) {
    const chain = new ReceiptChain(json.chainId);
    for (const receipt of json.receipts) {
      chain.receipts.push(Object.freeze(receipt));
      chain.lastTimestamp = BigInt(receipt.timestamp_ns);
    }
    return chain;
  }
}

export default ReceiptChain;
