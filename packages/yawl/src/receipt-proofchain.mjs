/**
 * YAWL Receipt ProofChain - Merkle Tree and Proof Generation
 *
 * Manages chains of receipts with Merkle root computation and proof verification.
 *
 * @module @unrdf/yawl/receipt-proofchain
 */

import { blake3 } from 'hash-wasm';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { BLAKE3_HEX_LENGTH } from './receipt-core.mjs';
import { verifyReceipt, verifyChainLink } from './receipt-verification.mjs';
import { ReceiptError } from '../errors.mjs';

// =============================================================================
// ProofChain Class
// =============================================================================

/**
 * ProofChain - Manages a chain of receipts with Merkle root computation
 *
 * Provides append, verification, and Merkle root computation for
 * a sequence of cryptographic receipts.
 *
 * @example
 * const chain = new ProofChain('node-1');
 *
 * // Append receipts
 * await chain.append(await generateReceipt(event1, null));
 * await chain.append(await generateReceipt(event2, chain.getLatest()));
 *
 * // Verify entire chain
 * const result = await chain.verify();
 * console.log(result.valid); // true
 *
 * // Get Merkle root
 * const root = await chain.getMerkleRoot();
 * console.log(root); // 64-char hex hash
 */
export class ProofChain {
  /**
   * Create a new proof chain
   *
   * @param {string} nodeId - Node identifier for vector clock
   */
  constructor(nodeId) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new TypeError('ProofChain requires a string nodeId');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {import('./receipt-core.mjs').Receipt[]} */
    this.receipts = [];

    /** @type {VectorClock} */
    this.vectorClock = new VectorClock(nodeId);

    /** @type {string|null} */
    this._merkleRoot = null;

    /** @type {boolean} */
    this._dirty = false;
  }

  /**
   * Get the number of receipts in the chain
   * @returns {number}
   */
  get length() {
    return this.receipts.length;
  }

  /**
   * Get the latest receipt in the chain
   * @returns {import('./receipt-core.mjs').Receipt|null}
   */
  getLatest() {
    return this.receipts.length > 0 ? this.receipts[this.receipts.length - 1] : null;
  }

  /**
   * Append a receipt to the chain
   *
   * Validates that the receipt properly chains to the previous receipt.
   *
   * @param {import('./receipt-core.mjs').Receipt} receipt - Receipt to append
   * @returns {Promise<void>}
   * @throws {Error} If receipt doesn't chain properly
   */
  async append(receipt) {
    // Validate receipt independently
    const verifyResult = await verifyReceipt(receipt);
    if (!verifyResult.valid) {
      throw new Error(`Cannot append invalid receipt: ${verifyResult.error}`);
    }

    // If not first receipt, validate chain link
    if (this.receipts.length > 0) {
      const latest = this.getLatest();
      if (receipt.previousReceiptHash !== latest.receiptHash) {
        throw new Error(
          `Chain broken: expected previousReceiptHash ${latest.receiptHash}, got ${receipt.previousReceiptHash}`
        );
      }
      if (receipt.t_ns <= latest.t_ns) {
        throw new Error('Temporal ordering violated: receipt timestamp must be after latest');
      }
    } else {
      // First receipt must have null previousReceiptHash
      if (receipt.previousReceiptHash !== null) {
        throw new Error('Genesis receipt must have null previousReceiptHash');
      }
    }

    // Increment vector clock
    this.vectorClock.increment();

    // Add receipt
    this.receipts.push(receipt);
    this._dirty = true;
    this._merkleRoot = null;
  }

  /**
   * Verify the entire chain
   *
   * Validates all receipts and chain links.
   *
   * @returns {Promise<import('./receipt-core.mjs').VerificationResult>}
   */
  async verify() {
    if (this.receipts.length === 0) {
      return { valid: true };
    }

    // Verify first receipt (genesis)
    const firstResult = await verifyReceipt(this.receipts[0]);
    if (!firstResult.valid) {
      return {
        valid: false,
        error: `Receipt 0 invalid: ${firstResult.error}`,
      };
    }

    if (this.receipts[0].previousReceiptHash !== null) {
      return {
        valid: false,
        error: 'Genesis receipt must have null previousReceiptHash',
      };
    }

    // Verify chain links
    for (let i = 1; i < this.receipts.length; i++) {
      const linkResult = await verifyChainLink(this.receipts[i], this.receipts[i - 1]);
      if (!linkResult.valid) {
        return {
          valid: false,
          error: `Chain link ${i - 1} -> ${i} invalid: ${linkResult.error}`,
        };
      }
    }

    return { valid: true };
  }

  /**
   * Compute the Merkle root of all receipt hashes
   *
   * Uses a binary Merkle tree with BLAKE3 for internal nodes.
   *
   * @returns {Promise<string>} 64-character hex Merkle root
   */
  async getMerkleRoot() {
    if (this._merkleRoot && !this._dirty) {
      return this._merkleRoot;
    }

    if (this.receipts.length === 0) {
      // Empty tree has zero hash
      this._merkleRoot = '0'.repeat(BLAKE3_HEX_LENGTH);
      this._dirty = false;
      return this._merkleRoot;
    }

    // Build Merkle tree from receipt hashes
    let level = this.receipts.map((r) => r.receiptHash);

    while (level.length > 1) {
      const nextLevel = [];
      for (let i = 0; i < level.length; i += 2) {
        if (i + 1 < level.length) {
          // Hash pair of nodes
          const combined = `${level[i]}:${level[i + 1]}`;
          nextLevel.push(await blake3(combined));
        } else {
          // Odd node promoted to next level
          nextLevel.push(level[i]);
        }
      }
      level = nextLevel;
    }

    this._merkleRoot = level[0];
    this._dirty = false;
    return this._merkleRoot;
  }

  /**
   * Get receipt by index
   *
   * @param {number} index - Receipt index
   * @returns {import('./receipt-core.mjs').Receipt|undefined}
   */
  getReceipt(index) {
    return this.receipts[index];
  }

  /**
   * Get receipt by ID
   *
   * @param {string} id - Receipt UUID
   * @returns {import('./receipt-core.mjs').Receipt|undefined}
   */
  getReceiptById(id) {
    return this.receipts.find((r) => r.id === id);
  }

  /**
   * Get receipts for a specific case
   *
   * @param {string} caseId - Case ID
   * @returns {import('./receipt-core.mjs').Receipt[]}
   */
  getReceiptsForCase(caseId) {
    return this.receipts.filter((r) => r.caseId === caseId);
  }

  /**
   * Get receipts for a specific task
   *
   * @param {string} taskId - Task ID
   * @returns {import('./receipt-core.mjs').Receipt[]}
   */
  getReceiptsForTask(taskId) {
    return this.receipts.filter((r) => r.taskId === taskId);
  }

  /**
   * Get the proof path for a receipt (Merkle proof)
   *
   * @param {number} index - Receipt index
   * @returns {Promise<Array<{hash: string, position: string}>>} Array of sibling hashes for proof
   */
  async getMerkleProof(index) {
    if (index < 0 || index >= this.receipts.length) {
      throw new RangeError(`Index ${index} out of range [0, ${this.receipts.length})`);
    }

    const proof = [];
    let level = this.receipts.map((r) => r.receiptHash);
    let idx = index;

    while (level.length > 1) {
      // Get sibling index
      const siblingIdx = idx % 2 === 0 ? idx + 1 : idx - 1;

      if (siblingIdx < level.length) {
        proof.push({
          hash: level[siblingIdx],
          position: idx % 2 === 0 ? 'right' : 'left',
        });
      }

      // Build next level
      const nextLevel = [];
      for (let i = 0; i < level.length; i += 2) {
        if (i + 1 < level.length) {
          const combined = `${level[i]}:${level[i + 1]}`;
          nextLevel.push(await blake3(combined));
        } else {
          nextLevel.push(level[i]);
        }
      }

      level = nextLevel;
      idx = Math.floor(idx / 2);
    }

    return proof;
  }

  /**
   * Verify a Merkle proof
   *
   * @param {string} receiptHash - Hash to verify
   * @param {Array<{hash: string, position: string}>} proof - Merkle proof path
   * @param {string} merkleRoot - Expected Merkle root
   * @returns {Promise<boolean>} Whether proof is valid
   */
  async verifyMerkleProof(receiptHash, proof, merkleRoot) {
    let currentHash = receiptHash;

    for (const step of proof) {
      const combined =
        step.position === 'right'
          ? `${currentHash}:${step.hash}`
          : `${step.hash}:${currentHash}`;
      currentHash = await blake3(combined);
    }

    return currentHash === merkleRoot;
  }

  /**
   * Serialize chain to JSON
   *
   * @returns {Object} JSON-serializable chain data
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      vectorClock: this.vectorClock.toJSON(),
      receipts: this.receipts.map((r) => ({
        ...r,
        t_ns: r.t_ns.toString(),
      })),
      merkleRoot: this._merkleRoot,
    };
  }

  /**
   * Create chain from JSON
   *
   * @param {Object} json - Serialized chain data
   * @returns {ProofChain}
   */
  static fromJSON(json) {
    const chain = new ProofChain(json.nodeId);
    chain.vectorClock = VectorClock.fromJSON(json.vectorClock);
    chain.receipts = json.receipts.map((r) => ({
      ...r,
      t_ns: BigInt(r.t_ns),
    }));
    chain._merkleRoot = json.merkleRoot;
    chain._dirty = false;
    return chain;
  }

  /**
   * Export chain as audit trail
   *
   * @returns {Promise<Object>} Complete audit trail with verification
   */
  async exportAuditTrail() {
    const verifyResult = await this.verify();
    const merkleRoot = await this.getMerkleRoot();

    return {
      nodeId: this.nodeId,
      receiptCount: this.receipts.length,
      firstReceiptTime: this.receipts[0]?.timestamp_iso || null,
      lastReceiptTime: this.receipts[this.receipts.length - 1]?.timestamp_iso || null,
      merkleRoot,
      chainValid: verifyResult.valid,
      validationError: verifyResult.error || null,
      vectorClock: this.vectorClock.toJSON(),
      receipts: this.receipts.map((r) => ({
        id: r.id,
        eventType: r.eventType,
        timestamp_iso: r.timestamp_iso,
        caseId: r.caseId,
        taskId: r.taskId,
        receiptHash: r.receiptHash,
        decision: r.payload.decision,
      })),
      exportedAt: toISO(now()),
    };
  }
}
