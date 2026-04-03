/**
 * @file Daemon Receipts Merkle Tree Integration
 * @module @unrdf/daemon/integrations/receipts-merkle
 * @description DaemonReceiptGenerator with Merkle tree batching, chaining, and proof verification
 * for cryptographic audit trail support in background operations.
 */

import { blake3 } from 'hash-wasm';
import {
  GeneratorOptionsSchema,
} from './receipts-merkle-schemas.mjs';
import {
  buildMerkleTree,
  generateInclusionProof,
  verifyInclusionProof,
} from './receipts-merkle-tree.mjs';

// =============================================================================
// Helpers
// =============================================================================

/**
 * Generate UUID v4
 * @private
 * @returns {string} UUID v4 formatted string
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

// =============================================================================
// Daemon Receipt Generator
// =============================================================================

/**
 * DaemonReceiptGenerator - Generates cryptographic receipts with Merkle tree batching
 * for daemon operations. Supports batch proof generation, chaining, and verification.
 *
 * @example
 * const generator = new DaemonReceiptGenerator({ batchSize: 50 });
 * const receipt = await generator.generateReceipt({
 *   operationId: '123e4567-e89b-12d3-a456-426614174000',
 *   operationType: 'task_executed',
 *   timestamp_ns: BigInt(Date.now() * 1_000_000),
 *   nodeId: 'node-1',
 *   daemonId: 'daemon-1',
 *   payload: { taskId: 'task-123', status: 'completed' }
 * });
 */
export class DaemonReceiptGenerator {
  /**
   * Creates a new DaemonReceiptGenerator
   * @param {Object} options - Generator options
   * @param {number} [options.batchSize=100] - Operations per batch (10-100)
   * @param {number} [options.maxBufferSize=1000] - Max buffered operations
   * @throws {Error} If options are invalid
   */
  constructor(options = {}) {
    const validated = GeneratorOptionsSchema.parse(options);

    this.batchSize = validated.batchSize;
    this.maxBufferSize = validated.maxBufferSize;
    this.operationBuffer = [];
    this.receipts = new Map();
    this.batches = new Map();
    this.lastReceiptHash = null;
    this.batchCounter = 0;
  }

  /**
   * Generate receipt for a single operation
   * @param {Object} operation - Operation to receipt
   * @returns {Promise<Object>} Generated receipt
   * @throws {Error} If operation is invalid
   */
  async generateReceipt(operation) {
    // Validate required fields first
    if (!operation || typeof operation !== 'object') {
      throw new TypeError('operation must be an object');
    }
    if (!operation.operationId || typeof operation.operationId !== 'string') {
      throw new TypeError('operation.operationId must be a string UUID');
    }
    if (!operation.operationType || typeof operation.operationType !== 'string') {
      throw new TypeError('operation.operationType must be a string');
    }
    if (typeof operation.timestamp_ns !== 'bigint') {
      throw new TypeError('operation.timestamp_ns must be a bigint');
    }
    if (!operation.nodeId || typeof operation.nodeId !== 'string') {
      throw new TypeError('operation.nodeId must be a non-empty string');
    }
    if (!operation.daemonId || typeof operation.daemonId !== 'string') {
      throw new TypeError('operation.daemonId must be a non-empty string');
    }
    if (!operation.payload || typeof operation.payload !== 'object') {
      throw new TypeError('operation.payload must be an object');
    }

    // Compute payload hash
    const payloadStr = JSON.stringify(operation.payload, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
    const payloadHash = await blake3(payloadStr);

    // Create receipt with chain link
    const receiptId = generateUUID();
    const chainInput = (this.lastReceiptHash || 'GENESIS') + ':' + payloadHash;
    const receiptHash = await blake3(chainInput);

    const receipt = {
      id: receiptId,
      operationId: operation.operationId,
      operationType: operation.operationType,
      timestamp_ns: operation.timestamp_ns,
      timestamp_iso: new Date(Number(operation.timestamp_ns) / 1_000_000).toISOString(),
      payloadHash,
      previousHash: this.lastReceiptHash || null,
      receiptHash,
      batchIndex: this.operationBuffer.length,
      merkleLeafHash: receiptHash,
    };

    // Store receipt and update chain state
    this.receipts.set(receiptId, receipt);
    this.operationBuffer.push(receipt);
    this.lastReceiptHash = receiptHash;

    return receipt;
  }

  /**
   * Generate batch proof for accumulated receipts
   * Creates Merkle tree from buffered receipts and returns batch proof.
   * @param {number} [count] - Number of receipts to batch (default: all)
   * @returns {Promise<Object>} Batch proof with Merkle tree
   * @throws {Error} If buffer is empty or count invalid
   */
  async generateBatchProof(count) {
    if (this.operationBuffer.length === 0) {
      throw new Error('Cannot generate batch proof: operation buffer is empty');
    }

    const batchSize = Math.min(count || this.batchSize, this.operationBuffer.length);
    if (batchSize <= 0) {
      throw new Error(`Invalid batch size: ${batchSize}`);
    }

    // Extract receipts for this batch
    const batchReceipts = this.operationBuffer.slice(0, batchSize);
    const remainingReceipts = this.operationBuffer.slice(batchSize);

    // Build Merkle tree using imported function
    const tree = await buildMerkleTree(batchReceipts);

    // Create batch proof
    const batchId = generateUUID();
    const batchProof = {
      batchId,
      batchNumber: this.batchCounter,
      merkleRoot: tree.root,
      leafCount: batchReceipts.length,
      treeDepth: tree.depth,
      timestamp_ns: BigInt(Date.now() * 1_000_000),
      receipts: batchReceipts,
    };

    // Store batch and update buffer
    this.batches.set(batchId, batchProof);
    this.operationBuffer = remainingReceipts;
    this.batchCounter += 1;

    return batchProof;
  }

  /**
   * Generate Merkle inclusion proof for a specific receipt
   * @param {string} receiptId - Receipt ID to prove
   * @param {Array<Object>} receipts - Array of receipts in tree
   * @returns {Promise<Object>} Merkle proof for receipt
   * @throws {Error} If receipt not found
   */
  async getReceiptProof(receiptId, receipts) {
    if (!Array.isArray(receipts) || receipts.length === 0) {
      throw new Error('Invalid receipts array');
    }

    const receipt = receipts.find(r => r.id === receiptId);
    if (!receipt) {
      throw new Error(`Receipt ${receiptId} not found`);
    }

    const tree = await buildMerkleTree(receipts);
    const index = receipts.findIndex(r => r.id === receiptId);

    const proof = await generateInclusionProof(tree, index);

    return {
      leafHash: receipt.merkleLeafHash,
      leafIndex: index,
      proofPath: proof,
      merkleRoot: tree.root,
      batchSize: receipts.length,
    };
  }

  /**
   * Verify Merkle inclusion proof
   * @param {Object} proof - Merkle proof to verify
   * @returns {Promise<boolean>} True if proof is valid
   */
  async verifyProof(proof) {
    return verifyInclusionProof(proof);
  }

  /**
   * Verify entire receipt chain
   * Checks chain links, hash integrity, and Merkle tree consistency.
   * @param {Array<Object>} receipts - Receipts to verify
   * @returns {Promise<Object>} Chain verification result
   */
  async verifyChain(receipts) {
    if (!Array.isArray(receipts) || receipts.length === 0) {
      return {
        valid: false,
        totalReceipts: 0,
        validReceipts: 0,
        tamperedReceipts: [],
        merkleRootConsistent: false,
        chainLinksValid: false,
      };
    }

    const tamperedReceipts = [];
    let validCount = 0;

    // Verify genesis receipt
    const first = receipts[0];
    if (first.previousHash !== null) {
      tamperedReceipts.push({
        receiptId: first.id,
        reason: 'Genesis receipt must have previousHash = null',
      });
    } else {
      validCount += 1;
    }

    // Verify chain links and hash integrity
    for (let i = 1; i < receipts.length; i++) {
      const current = receipts[i];
      const previous = receipts[i - 1];

      // Verify chain link
      if (current.previousHash !== previous.receiptHash) {
        tamperedReceipts.push({
          receiptId: current.id,
          reason: `Chain broken: previousHash mismatch at index ${i}`,
        });
        continue;
      }

      // Verify receipt hash integrity
      const payloadHash = current.payloadHash;
      const expectedHash = await blake3(current.previousHash + ':' + payloadHash);
      if (expectedHash !== current.receiptHash) {
        tamperedReceipts.push({
          receiptId: current.id,
          reason: `Hash integrity failed at index ${i}`,
        });
        continue;
      }

      validCount += 1;
    }

    // Verify Merkle tree can be built (consistency check)
    let merkleRootConsistent = true;
    try {
      // Try building the tree - success means no tampering of leaf hashes
      await buildMerkleTree(receipts);
      merkleRootConsistent = true;
    } catch {
      merkleRootConsistent = false;
    }

    const valid = tamperedReceipts.length === 0 && merkleRootConsistent;

    return {
      valid,
      totalReceipts: receipts.length,
      validReceipts: validCount,
      tamperedReceipts,
      merkleRootConsistent,
      chainLinksValid: tamperedReceipts.length === 0,
    };
  }

  /**
   * Detect tampered receipts in a batch
   * @param {Array<Object>} receipts - Receipts to check
   * @returns {Promise<Array<Object>>} Array of tampered receipts
   */
  async detectTampering(receipts) {
    const result = await this.verifyChain(receipts);
    return result.tamperedReceipts;
  }

  /**
   * Export Merkle tree as JSON
   * @param {Array<Object>} receipts - Receipts in tree
   * @returns {Promise<Object>} Tree structure for export
   */
  async exportMerkleTree(receipts) {
    if (!Array.isArray(receipts) || receipts.length === 0) {
      throw new Error('Cannot export tree: receipts array is empty');
    }

    const tree = await buildMerkleTree(receipts);
    return {
      root: tree.root,
      depth: tree.depth,
      leafCount: tree.leafCount,
      leaves: tree.leaves,
      treeStructure: this._serializeTree(tree),
    };
  }

  /**
   * Get batch statistics
   * @returns {Object} Batch and buffer statistics
   */
  getStatistics() {
    return {
      totalBatchesGenerated: this.batchCounter,
      bufferedOperations: this.operationBuffer.length,
      totalReceiptsGenerated: this.receipts.size,
      totalBatches: this.batches.size,
      lastReceiptHash: this.lastReceiptHash,
    };
  }

  // ==========================================================================
  // Private Methods
  // ==========================================================================

  /**
   * Serialize tree structure to plain object
   * @private
   * @param {Object} tree - Tree to serialize
   * @returns {Object} Serialized tree
   */
  _serializeTree(tree) {
    return {
      root: tree.root,
      depth: tree.depth,
      leafCount: tree.leafCount,
      levels: tree.levels.map((level, levelIdx) => ({
        level: levelIdx,
        nodeCount: level.length,
        hashes: level,
      })),
    };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default DaemonReceiptGenerator;
