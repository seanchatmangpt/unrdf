/**
 * Merkle Proof Generator - Efficient batch anchoring using Merkle trees
 *
 * Provides Merkle tree construction, proof generation, and verification
 * for efficient batch anchoring of receipt hashes on blockchain.
 *
 * @module @unrdf/blockchain/merkle
 */

import { MerkleTree } from 'merkletreejs';
import { sha256 } from '@noble/hashes/sha256';
import { bytesToHex, hexToBytes } from '@noble/hashes/utils';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

const MerkleProofSchema = z.object({
  /** Leaf hash (receipt hash) */
  leaf: z.string(),
  /** Merkle proof path */
  proof: z.array(z.string()),
  /** Leaf index in tree */
  index: z.number(),
  /** Root hash */
  root: z.string(),
});

const MerkleTreeInfoSchema = z.object({
  /** Root hash */
  root: z.string(),
  /** Total number of leaves */
  leafCount: z.number(),
  /** Tree depth */
  depth: z.number(),
  /** All leaf hashes */
  leaves: z.array(z.string()),
});

// =============================================================================
// Merkle Proof Generator
// =============================================================================

/**
 * MerkleProofGenerator - Build and verify Merkle trees for receipt batches
 *
 * @class
 * @example
 * ```javascript
 * const generator = new MerkleProofGenerator();
 *
 * // Add receipts
 * const receipts = [receipt1, receipt2, receipt3];
 * receipts.forEach(r => generator.addReceipt(r));
 *
 * // Build tree
 * generator.buildTree();
 *
 * // Get root for anchoring
 * const root = generator.getRoot();
 *
 * // Generate proof for specific receipt
 * const proof = generator.generateProof(receipt1);
 *
 * // Verify proof
 * const isValid = generator.verifyProof(proof);
 * ```
 */
export class MerkleProofGenerator {
  constructor() {
    /** @type {Array<string>} */
    this.receiptHashes = [];

    /** @type {Map<string, Object>} */
    this.receiptMap = new Map();

    /** @type {MerkleTree|null} */
    this.tree = null;

    /** @type {string|null} */
    this.root = null;
  }

  /**
   * Add a receipt to the tree
   *
   * @param {Object} receipt - YAWL receipt object
   */
  addReceipt(receipt) {
    const hash = receipt.hash || this._computeReceiptHash(receipt);
    this.receiptHashes.push(hash);
    this.receiptMap.set(hash, receipt);
  }

  /**
   * Add multiple receipts at once
   *
   * @param {Array<Object>} receipts - Array of YAWL receipts
   */
  addReceipts(receipts) {
    receipts.forEach(r => this.addReceipt(r));
  }

  /**
   * Build the Merkle tree from added receipts
   *
   * @returns {string} Root hash
   */
  buildTree() {
    if (this.receiptHashes.length === 0) {
      throw new Error('No receipts added to build tree');
    }

    // Convert hashes to buffers for MerkleTree
    const leaves = this.receiptHashes.map(hash => {
      const bytes = hexToBytes(hash.startsWith('0x') ? hash.slice(2) : hash);
      return Buffer.from(bytes);
    });

    // Build tree using SHA256
    this.tree = new MerkleTree(leaves, sha256, {
      sortPairs: true,
      hashLeaves: false, // Already hashed
    });

    // Get root
    const rootBuffer = this.tree.getRoot();
    this.root = '0x' + rootBuffer.toString('hex');

    return this.root;
  }

  /**
   * Get the Merkle root hash
   *
   * @returns {string} Root hash
   */
  getRoot() {
    if (!this.root) {
      throw new Error('Tree not built. Call buildTree() first.');
    }
    return this.root;
  }

  /**
   * Generate Merkle proof for a specific receipt
   *
   * @param {Object} receipt - Receipt to generate proof for
   * @returns {Object} Merkle proof
   */
  generateProof(receipt) {
    if (!this.tree) {
      throw new Error('Tree not built. Call buildTree() first.');
    }

    const hash = receipt.hash || this._computeReceiptHash(receipt);
    const hashBytes = hexToBytes(hash.startsWith('0x') ? hash.slice(2) : hash);
    const leaf = Buffer.from(hashBytes);

    // Get proof
    const proofBuffers = this.tree.getProof(leaf);
    const proof = proofBuffers.map(p => '0x' + p.data.toString('hex'));

    // Get leaf index
    const index = this.receiptHashes.indexOf(hash);

    const merkleProof = {
      leaf: '0x' + hash.replace('0x', ''),
      proof,
      index,
      root: this.root,
    };

    return MerkleProofSchema.parse(merkleProof);
  }

  /**
   * Verify a Merkle proof
   *
   * @param {Object} merkleProof - Proof to verify
   * @returns {boolean} Whether proof is valid
   */
  verifyProof(merkleProof) {
    if (!this.tree) {
      throw new Error('Tree not built. Call buildTree() first.');
    }

    const leafBytes = hexToBytes(merkleProof.leaf.replace('0x', ''));
    const leaf = Buffer.from(leafBytes);

    const proofBuffers = merkleProof.proof.map(p => {
      const bytes = hexToBytes(p.replace('0x', ''));
      return { data: Buffer.from(bytes) };
    });

    const rootBytes = hexToBytes(merkleProof.root.replace('0x', ''));
    const root = Buffer.from(rootBytes);

    return this.tree.verify(proofBuffers, leaf, root);
  }

  /**
   * Get tree information
   *
   * @returns {Object} Tree info
   */
  getTreeInfo() {
    if (!this.tree) {
      throw new Error('Tree not built. Call buildTree() first.');
    }

    const depth = this.tree.getDepth();
    const leafCount = this.receiptHashes.length;
    const leaves = this.receiptHashes.map(h => '0x' + h.replace('0x', ''));

    const info = {
      root: this.root,
      leafCount,
      depth,
      leaves,
    };

    return MerkleTreeInfoSchema.parse(info);
  }

  /**
   * Get all proofs for all receipts
   *
   * @returns {Array<Object>} Array of proofs
   */
  getAllProofs() {
    if (!this.tree) {
      throw new Error('Tree not built. Call buildTree() first.');
    }

    return Array.from(this.receiptMap.values()).map(receipt =>
      this.generateProof(receipt)
    );
  }

  /**
   * Export tree to JSON for storage
   *
   * @returns {Object} Serializable tree data
   */
  export() {
    if (!this.tree) {
      throw new Error('Tree not built. Call buildTree() first.');
    }

    return {
      root: this.root,
      receipts: Array.from(this.receiptMap.entries()).map(([hash, receipt]) => ({
        hash,
        receipt,
      })),
      treeInfo: this.getTreeInfo(),
    };
  }

  /**
   * Import tree from JSON
   *
   * @param {Object} data - Exported tree data
   */
  import(data) {
    this.receiptHashes = [];
    this.receiptMap = new Map();

    data.receipts.forEach(({ hash, receipt }) => {
      this.receiptHashes.push(hash);
      this.receiptMap.set(hash, receipt);
    });

    this.buildTree();

    if (this.root !== data.root) {
      throw new Error('Imported tree root mismatch');
    }
  }

  /**
   * Compute SHA256 hash of receipt (fallback)
   *
   * @private
   * @param {Object} receipt - Receipt object
   * @returns {string} Hex hash
   */
  _computeReceiptHash(receipt) {
    const data = JSON.stringify(receipt);
    const hash = sha256(new TextEncoder().encode(data));
    return bytesToHex(hash);
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Calculate gas savings using Merkle anchoring vs individual anchoring
 *
 * @param {number} receiptCount - Number of receipts
 * @param {Object} gasPrice - Current gas price data
 * @returns {Object} Savings analysis
 */
export function calculateGasSavings(receiptCount, gasPrice) {
  // Individual anchoring: ~50k gas per receipt
  const individualGas = BigInt(receiptCount) * 50000n;

  // Merkle anchoring: ~60k gas total (fixed)
  const merkleGas = 60000n;

  // Savings
  const savedGas = individualGas - merkleGas;
  const savedCostWei = savedGas * BigInt(gasPrice);

  return {
    receiptCount,
    individualGas: individualGas.toString(),
    merkleGas: merkleGas.toString(),
    savedGas: savedGas.toString(),
    savedCostWei: savedCostWei.toString(),
    savingsPercentage: Number((savedGas * 100n) / individualGas),
  };
}

// =============================================================================
// Exports
// =============================================================================

export default MerkleProofGenerator;
export { MerkleProofSchema, MerkleTreeInfoSchema };
