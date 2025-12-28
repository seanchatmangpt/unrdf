/**
 * Receipt Chain with Merkle Verification for Tamper-Proof Audit
 *
 * Implements cryptographic receipt chains where:
 * - Each receipt contains: before hash, after hash, delta hash
 * - Chain integrity: r_i.before = r_{i-1}.after
 * - Merkle tree for batch verification
 * - Tamper detection via chain verification
 *
 * @module receipts
 */

import { createHash } from 'node:crypto';
import { z } from 'zod';

// ============================================================================
// Schemas
// ============================================================================

/**
 * Receipt schema - cryptographic proof of state transition
 * @typedef {Object} Receipt
 * @property {string} before - Hash of previous state A_{τ-1}
 * @property {string} after - Hash of current state A_τ
 * @property {string} delta - Hash of delta operation ΔO
 * @property {number} timestamp - Unix timestamp of receipt creation
 * @property {string} [id] - Optional receipt identifier
 */
export const ReceiptSchema = z.object({
  before: z.string().length(64), // SHA-256 hex = 64 chars
  after: z.string().length(64),
  delta: z.string().length(64),
  timestamp: z.number().int().positive(),
  id: z.string().optional(),
});

/**
 * Receipt chain schema
 * @typedef {Object} ReceiptChainData
 * @property {Receipt[]} receipts - Array of receipts in chain
 * @property {string} merkleRoot - Merkle root of entire chain
 */
export const ReceiptChainSchema = z.object({
  receipts: z.array(ReceiptSchema),
  merkleRoot: z.string().length(64),
});

// ============================================================================
// Pure Hash Functions
// ============================================================================

/**
 * Compute SHA-256 hash of input
 * @param {string | object} input - Data to hash (objects are JSON stringified)
 * @returns {string} Hex-encoded hash (64 chars)
 */
export function hash(input) {
  const data = typeof input === 'string' ? input : JSON.stringify(input);
  return createHash('sha256').update(data).digest('hex');
}

/**
 * Hash a receipt (excluding id field for determinism)
 * @param {Receipt} receipt - Receipt to hash
 * @returns {string} Hash of receipt
 */
export function hashReceipt(receipt) {
  const { before, after, delta, timestamp } = receipt;
  return hash({ before, after, delta, timestamp });
}

/**
 * Compute Merkle parent hash from two child hashes
 * @param {string} left - Left child hash
 * @param {string} right - Right child hash
 * @returns {string} Parent hash
 */
export function merkleParent(left, right) {
  return hash(left + right);
}

// ============================================================================
// Receipt Class
// ============================================================================

/**
 * Receipt - cryptographic proof of state transition
 */
export class Receipt {
  /**
   * Create a receipt
   * @param {string} before - Hash of previous state
   * @param {string} after - Hash of current state
   * @param {string} delta - Hash of delta operation
   * @param {Object} [options] - Optional parameters
   * @param {number} [options.timestamp] - Unix timestamp (defaults to now)
   * @param {string} [options.id] - Receipt identifier
   */
  constructor(before, after, delta, options = {}) {
    this.before = before;
    this.after = after;
    this.delta = delta;
    this.timestamp = options.timestamp ?? Date.now();
    if (options.id) {
      this.id = options.id;
    }

    // Validate on construction
    ReceiptSchema.parse(this);
  }

  /**
   * Get receipt as plain object
   * @returns {Receipt} Plain object
   */
  toObject() {
    const obj = {
      before: this.before,
      after: this.after,
      delta: this.delta,
      timestamp: this.timestamp,
    };
    if (this.id) {
      obj.id = this.id;
    }
    return obj;
  }

  /**
   * Compute hash of this receipt
   * @returns {string} Receipt hash
   */
  hash() {
    return hashReceipt(this.toObject());
  }

  /**
   * Create receipt from plain object
   * @param {Receipt} obj - Plain object
   * @returns {Receipt} Receipt instance
   */
  static fromObject(obj) {
    return new Receipt(obj.before, obj.after, obj.delta, {
      timestamp: obj.timestamp,
      id: obj.id,
    });
  }
}

// ============================================================================
// Merkle Tree Construction
// ============================================================================

/**
 * Build Merkle tree from leaf hashes
 * @param {string[]} leaves - Array of leaf hashes
 * @returns {string[][]} Array of tree levels (leaves at index 0, root at last)
 */
export function buildMerkleTree(leaves) {
  if (leaves.length === 0) {
    throw new Error('Cannot build Merkle tree from empty leaves');
  }

  const tree = [leaves];
  let currentLevel = leaves;

  while (currentLevel.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = i + 1 < currentLevel.length
        ? currentLevel[i + 1]
        : currentLevel[i]; // Duplicate last node if odd count
      nextLevel.push(merkleParent(left, right));
    }
    tree.push(nextLevel);
    currentLevel = nextLevel;
  }

  return tree;
}

/**
 * Get Merkle root from tree
 * @param {string[][]} tree - Merkle tree
 * @returns {string} Root hash
 */
export function getMerkleRoot(tree) {
  return tree[tree.length - 1][0];
}

/**
 * Compute Merkle root directly from leaves
 * @param {string[]} leaves - Array of leaf hashes
 * @returns {string} Root hash
 */
export function computeMerkleRoot(leaves) {
  const tree = buildMerkleTree(leaves);
  return getMerkleRoot(tree);
}

// ============================================================================
// Receipt Chain Class
// ============================================================================

/**
 * ReceiptChain - tamper-proof chain of receipts with Merkle verification
 */
export class ReceiptChain {
  /**
   * Create a receipt chain
   * @param {Receipt[]} [initialReceipts] - Initial receipts (optional)
   */
  constructor(initialReceipts = []) {
    /** @type {Receipt[]} */
    this.receipts = initialReceipts.map(r =>
      r instanceof Receipt ? r : Receipt.fromObject(r)
    );

    // Validate chain integrity on construction
    if (this.receipts.length > 0) {
      this._validateChainIntegrity();
    }
  }

  /**
   * Validate chain integrity (internal)
   * @private
   * @throws {Error} If chain integrity is broken
   */
  _validateChainIntegrity() {
    for (let i = 1; i < this.receipts.length; i++) {
      const prev = this.receipts[i - 1];
      const curr = this.receipts[i];
      if (curr.before !== prev.after) {
        throw new Error(
          `Chain integrity broken at index ${i}: ` +
          `r[${i}].before (${curr.before.slice(0, 8)}...) !== ` +
          `r[${i-1}].after (${prev.after.slice(0, 8)}...)`
        );
      }
    }
  }

  /**
   * Add receipt to chain
   * @param {Receipt} receipt - Receipt to add
   * @throws {Error} If receipt breaks chain integrity
   */
  add(receipt) {
    const r = receipt instanceof Receipt ? receipt : Receipt.fromObject(receipt);

    // Validate chain integrity
    if (this.receipts.length > 0) {
      const last = this.receipts[this.receipts.length - 1];
      if (r.before !== last.after) {
        throw new Error(
          `Cannot add receipt: before hash (${r.before.slice(0, 8)}...) ` +
          `does not match previous after hash (${last.after.slice(0, 8)}...)`
        );
      }
    }

    this.receipts.push(r);
  }

  /**
   * Get number of receipts in chain
   * @returns {number} Receipt count
   */
  get length() {
    return this.receipts.length;
  }

  /**
   * Get receipt at index
   * @param {number} index - Receipt index
   * @returns {Receipt} Receipt
   */
  get(index) {
    return this.receipts[index];
  }

  /**
   * Verify chain integrity
   * @returns {boolean} True if chain is valid, false otherwise
   */
  verify() {
    try {
      this._validateChainIntegrity();
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Compute Merkle root of entire chain
   * @returns {string} Merkle root hash
   */
  getMerkleRoot() {
    if (this.receipts.length === 0) {
      throw new Error('Cannot compute Merkle root of empty chain');
    }
    const leaves = this.receipts.map(r => r.hash());
    return computeMerkleRoot(leaves);
  }

  /**
   * Anchor a receipt to the chain's Merkle root
   * @param {Receipt} receipt - Receipt to anchor
   * @returns {string} Merkle root including this receipt
   */
  anchor(receipt) {
    const tempChain = new ReceiptChain([...this.receipts, receipt]);
    return tempChain.getMerkleRoot();
  }

  /**
   * Get chain as plain object
   * @returns {ReceiptChainData} Plain object
   */
  toObject() {
    return {
      receipts: this.receipts.map(r => r.toObject()),
      merkleRoot: this.length > 0 ? this.getMerkleRoot() : '',
    };
  }

  /**
   * Create chain from plain object
   * @param {ReceiptChainData} obj - Plain object
   * @returns {ReceiptChain} Chain instance
   */
  static fromObject(obj) {
    return new ReceiptChain(obj.receipts);
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Create a genesis receipt (first in chain)
 * @param {string} initialState - Hash of initial state
 * @param {string} delta - Hash of genesis delta
 * @param {Object} [options] - Optional parameters
 * @returns {Receipt} Genesis receipt
 */
export function createGenesisReceipt(initialState, delta, options = {}) {
  // Genesis receipt: before = after = initial state hash
  return new Receipt(initialState, initialState, delta, options);
}

/**
 * Create a state transition receipt
 * @param {string} beforeState - Hash of previous state
 * @param {string} afterState - Hash of new state
 * @param {string} delta - Hash of delta operation
 * @param {Object} [options] - Optional parameters
 * @returns {Receipt} State transition receipt
 */
export function createTransitionReceipt(beforeState, afterState, delta, options = {}) {
  return new Receipt(beforeState, afterState, delta, options);
}

/**
 * Verify a receipt chain and detect tampering
 * @param {ReceiptChain | Receipt[]} chain - Chain to verify
 * @returns {{ valid: boolean, error?: string, merkleRoot?: string }} Verification result
 */
export function verifyChain(chain) {
  try {
    const rc = chain instanceof ReceiptChain
      ? chain
      : new ReceiptChain(chain);

    const valid = rc.verify();
    if (!valid) {
      return { valid: false, error: 'Chain integrity check failed' };
    }

    const merkleRoot = rc.length > 0 ? rc.getMerkleRoot() : null;
    return { valid: true, merkleRoot };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}
