/**
 * Merkle Tree - Efficient batch verification for receipts
 *
 * Builds binary Merkle tree over receipt hashes:
 * - O(log n) proof size for n receipts
 * - Single root hash for batch verification
 * - Efficient proof generation and verification
 *
 * @module @unrdf/observability/receipts/merkle-tree
 */

import { blake3 } from 'hash-wasm';
import { MerkleProofSchema } from './receipt-schema.mjs';

/**
 * MerkleTree - Binary Merkle tree for receipt batching
 *
 * @example
 * const tree = new MerkleTree();
 * tree.addReceipt(receipt1);
 * tree.addReceipt(receipt2);
 * const root = await tree.buildTree();
 * const proof = await tree.generateProof(receipt1.id);
 */
export class MerkleTree {
  /**
   * Create a new Merkle tree
   */
  constructor() {
    this.receipts = [];
    this.leaves = [];
    this.tree = [];
    this.root = null;
  }

  /**
   * Add receipt to tree (before building)
   * @param {Object} receipt - Receipt to add
   */
  addReceipt(receipt) {
    if (!receipt || !receipt.id || !receipt.hash) {
      throw new TypeError('addReceipt: receipt must have id and hash');
    }
    this.receipts.push(receipt);
    this.root = null; // Invalidate tree
  }

  /**
   * Build Merkle tree from receipts
   * @returns {Promise<string>} Merkle root hash (64-char hex)
   */
  async buildTree() {
    if (this.receipts.length === 0) {
      throw new Error('Cannot build tree: no receipts added');
    }

    // Initialize leaves from receipt hashes
    this.leaves = this.receipts.map(r => r.hash);
    this.tree = [this.leaves];

    // Build tree levels
    let currentLevel = this.leaves;
    while (currentLevel.length > 1) {
      const nextLevel = [];
      let i = 0;
      while (i < currentLevel.length) {
        if (i + 1 < currentLevel.length) {
          // Hash pair
          const left = currentLevel[i];
          const right = currentLevel[i + 1];
          const combined = left + ':' + right;
          const pairHash = await blake3(combined);
          nextLevel.push(pairHash);
          i += 2;
        } else {
          // Odd node promoted to next level
          nextLevel.push(currentLevel[i]);
          i += 1;
        }
      }
      this.tree.push(nextLevel);
      currentLevel = nextLevel;
    }

    this.root = currentLevel[0];
    return this.root;
  }

  /**
   * Get Merkle root
   * @returns {string|null}
   */
  getRoot() {
    return this.root;
  }

  /**
   * Generate Merkle proof for a receipt
   *
   * @param {string} receiptId - Receipt ID to prove
   * @returns {Promise<Object>} Merkle proof
   * @throws {Error} If receipt not found or tree not built
   */
  async generateProof(receiptId) {
    if (!this.root) {
      throw new Error('Tree not built: call buildTree() first');
    }

    const index = this.receipts.findIndex(r => r.id === receiptId);
    if (index === -1) {
      throw new Error('Receipt not found: ' + receiptId);
    }

    const receipt = this.receipts[index];
    const siblings = [];

    let idx = index;
    let level = 0;
    while (level < this.tree.length - 1) {
      const levelData = this.tree[level];
      const siblingIdx = idx % 2 === 0 ? idx + 1 : idx - 1;

      if (siblingIdx < levelData.length) {
        siblings.push({
          hash: levelData[siblingIdx],
          position: idx % 2 === 0 ? 'right' : 'left',
        });
      }

      idx = Math.floor(idx / 2);
      level += 1;
    }

    const proof = {
      receiptId: receipt.id,
      receiptHash: receipt.hash,
      root: this.root,
      siblings,
      index,
    };

    return MerkleProofSchema.parse(proof);
  }

  /**
   * Verify a Merkle proof
   *
   * @param {Object} proof - Merkle proof to verify
   * @returns {Promise<boolean>} Whether proof is valid
   */
  async verifyProof(proof) {
    try {
      MerkleProofSchema.parse(proof);
    } catch (err) {
      return false;
    }

    let currentHash = proof.receiptHash;

    let i = 0;
    while (i < proof.siblings.length) {
      const sibling = proof.siblings[i];
      const combined =
        sibling.position === 'right'
          ? currentHash + ':' + sibling.hash
          : sibling.hash + ':' + currentHash;
      currentHash = await blake3(combined);
      i += 1;
    }

    return currentHash === proof.root;
  }

  /**
   * Get tree info
   * @returns {Object} Tree metadata
   */
  getTreeInfo() {
    return {
      receiptCount: this.receipts.length,
      depth: this.tree.length - 1,
      root: this.root,
      leafCount: this.leaves.length,
    };
  }
}

export default MerkleTree;
