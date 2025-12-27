/**
 * V6 Merkle Tree - Standardized receipt merkle tree implementation
 *
 * Provides BLAKE3-based merkle tree construction, proof generation,
 * and verification for receipt batching and anchoring.
 *
 * @module @unrdf/v6-core/receipts/merkle/tree
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

// =============================================================================
// Constants
// =============================================================================

/** BLAKE3 hash length in hex characters */
export const HASH_LENGTH = 64;

// =============================================================================
// Schemas
// =============================================================================

/**
 * Merkle proof schema
 */
export const MerkleProofSchema = z.object({
  /** Leaf hash (receipt hash) */
  leaf: z.string().length(HASH_LENGTH),
  /** Proof path: array of sibling hashes with positions */
  proof: z.array(z.object({
    hash: z.string().length(HASH_LENGTH),
    position: z.enum(['left', 'right']),
  })),
  /** Merkle root */
  root: z.string().length(HASH_LENGTH),
  /** Leaf index in tree */
  index: z.number().int().nonnegative(),
});

/**
 * Tree info schema
 */
export const TreeInfoSchema = z.object({
  root: z.string().length(HASH_LENGTH),
  leafCount: z.number().int().nonnegative(),
  depth: z.number().int().nonnegative(),
  leaves: z.array(z.string().length(HASH_LENGTH)),
});

// =============================================================================
// Merkle Tree Builder
// =============================================================================

/**
 * Build a merkle tree from receipt hashes
 *
 * Uses BLAKE3 for all internal node hashing.
 * Leaf nodes are receipt hashes (no additional hashing).
 * Internal nodes: BLAKE3(left_hash + ":" + right_hash)
 *
 * @param {Array<Object>} receipts - Array of receipts with hash field
 * @returns {Promise<Object>} Tree structure with root and levels
 *
 * @example
 * const tree = await buildMerkleTree([receipt1, receipt2, receipt3]);
 * console.log(tree.root); // Merkle root hash
 * console.log(tree.depth); // Tree depth
 */
export async function buildMerkleTree(receipts) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new TypeError('buildMerkleTree: receipts must be a non-empty array');
  }

  // Extract receipt hashes (leaf nodes)
  const leaves = receipts.map((r, idx) => {
    if (!r.hash || typeof r.hash !== 'string' || r.hash.length !== HASH_LENGTH) {
      throw new Error(`Receipt at index ${idx} has invalid hash (expected ${HASH_LENGTH} hex chars)`);
    }
    return r.hash;
  });

  // Build tree levels bottom-up
  const levels = [leaves];
  let currentLevel = leaves;

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      if (i + 1 < currentLevel.length) {
        // Hash pair of sibling nodes
        const combined = currentLevel[i] + ':' + currentLevel[i + 1];
        const parentHash = await blake3(combined);
        nextLevel.push(parentHash);
      } else {
        // Odd node promoted to next level (no sibling)
        nextLevel.push(currentLevel[i]);
      }
    }

    levels.push(nextLevel);
    currentLevel = nextLevel;
  }

  const root = currentLevel[0];
  const depth = levels.length - 1;

  return {
    root,
    depth,
    leafCount: leaves.length,
    leaves,
    levels,
  };
}

/**
 * Get the merkle root from a tree
 *
 * @param {Object} tree - Tree structure from buildMerkleTree
 * @returns {string} Merkle root hash
 */
export function getMerkleRoot(tree) {
  if (!tree || !tree.root) {
    throw new TypeError('getMerkleRoot: tree must have a root field');
  }
  return tree.root;
}

/**
 * Get proof path for a specific receipt
 *
 * Generates a merkle inclusion proof for the receipt at the given index.
 * Proof consists of sibling hashes at each level from leaf to root.
 *
 * @param {Object} tree - Tree structure from buildMerkleTree
 * @param {string} receiptId - Receipt ID to generate proof for
 * @param {Array<Object>} receipts - Original receipts array
 * @returns {Promise<Object>} Merkle proof
 *
 * @example
 * const proof = await getProofPath(tree, 'receipt-123', receipts);
 * const isValid = await verifyInclusion(tree.root, receipt, proof);
 */
export async function getProofPath(tree, receiptId, receipts) {
  // Find receipt index
  const index = receipts.findIndex(r => r.id === receiptId);
  if (index === -1) {
    throw new Error(`Receipt ${receiptId} not found in receipts array`);
  }

  const receipt = receipts[index];
  const leaf = receipt.hash;

  // Build proof by traversing tree levels
  const proof = [];
  let currentIndex = index;

  // Iterate through levels (excluding root level)
  for (let levelIdx = 0; levelIdx < tree.levels.length - 1; levelIdx++) {
    const level = tree.levels[levelIdx];

    // Find sibling index
    const siblingIndex = currentIndex % 2 === 0 ? currentIndex + 1 : currentIndex - 1;

    if (siblingIndex < level.length) {
      proof.push({
        hash: level[siblingIndex],
        position: currentIndex % 2 === 0 ? 'right' : 'left',
      });
    }

    // Move to parent index in next level
    currentIndex = Math.floor(currentIndex / 2);
  }

  return MerkleProofSchema.parse({
    leaf,
    proof,
    root: tree.root,
    index,
  });
}

/**
 * Verify merkle inclusion proof
 *
 * Recomputes the merkle root from the leaf hash and proof path.
 * Returns true if computed root matches expected root.
 *
 * @param {string} root - Expected merkle root
 * @param {Object} receipt - Receipt to verify
 * @param {Object} proof - Merkle proof from getProofPath
 * @returns {Promise<boolean>} True if receipt is in tree
 *
 * @example
 * const isValid = await verifyInclusion(root, receipt, proof);
 * console.log(isValid); // true if receipt is in the merkle tree
 */
export async function verifyInclusion(root, receipt, proof) {
  if (!root || typeof root !== 'string' || root.length !== HASH_LENGTH) {
    throw new TypeError('verifyInclusion: root must be a 64-char hex string');
  }
  if (!receipt || !receipt.hash) {
    throw new TypeError('verifyInclusion: receipt must have a hash field');
  }
  if (!proof || !Array.isArray(proof.proof)) {
    throw new TypeError('verifyInclusion: proof must have a proof array');
  }

  // Start with leaf hash
  let currentHash = receipt.hash;

  // Apply proof steps
  for (const step of proof.proof) {
    const combined = step.position === 'right'
      ? currentHash + ':' + step.hash
      : step.hash + ':' + currentHash;
    currentHash = await blake3(combined);
  }

  // Compare computed root with expected root
  return currentHash === root;
}

/**
 * Get tree information summary
 *
 * @param {Object} tree - Tree structure from buildMerkleTree
 * @returns {Object} Tree info
 */
export function getTreeInfo(tree) {
  return TreeInfoSchema.parse({
    root: tree.root,
    leafCount: tree.leafCount,
    depth: tree.depth,
    leaves: tree.leaves,
  });
}

// =============================================================================
// Exports
// =============================================================================

export default {
  buildMerkleTree,
  getMerkleRoot,
  getProofPath,
  verifyInclusion,
  getTreeInfo,
};
