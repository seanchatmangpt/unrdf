/**
 * KGC Receipts - Merkle Tree Batcher
 * Implements Merkle tree construction for batch receipt verification
 *
 * @module @unrdf/receipts/merkle-batcher
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Merkle Node Schema
 */
const MerkleNodeSchema = z.object({
  hash: z.string().regex(/^[a-f0-9]{64}$/),     // BLAKE3 hash (64 hex chars)
  left: z.any().optional(),                      // Left child (recursive)
  right: z.any().optional(),                     // Right child (recursive)
  data: z.any().optional(),                      // Leaf data (if leaf node)
  index: z.number().int().nonnegative(),         // Node index
});

/**
 * Merkle Proof Schema
 */
const MerkleProofSchema = z.object({
  leaf: z.string(),                              // Leaf hash
  index: z.number().int().nonnegative(),         // Leaf index
  proof: z.array(z.object({                      // Proof path
    hash: z.string(),                            // Sibling hash
    position: z.enum(['left', 'right']),         // Sibling position
  })),
  root: z.string(),                              // Merkle root
});

/**
 * Compute Leaf Hash
 * Hashes leaf data using BLAKE3
 *
 * @param {*} data - Leaf data (will be JSON serialized)
 * @returns {Promise<string>} BLAKE3 hash
 *
 * @example
 * const hash = await computeLeafHash({ subject: 'ex:s1', ... });
 * console.assert(hash.length === 64);
 */
async function computeLeafHash(data) {
  const serialized = JSON.stringify(data, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  return blake3(serialized);
}

/**
 * Compute Parent Hash
 * Combines two child hashes to create parent hash
 *
 * @param {string} leftHash - Left child hash
 * @param {string} rightHash - Right child hash
 * @returns {Promise<string>} Parent hash
 *
 * @example
 * const parent = await computeParentHash(hashA, hashB);
 */
async function computeParentHash(leftHash, rightHash) {
  return blake3(leftHash + rightHash);
}

/**
 * Build Merkle Tree
 * Constructs complete Merkle tree from array of data
 *
 * @param {Array<*>} data - Array of leaf data
 * @returns {Promise<Object>} Merkle tree root node
 * @throws {Error} If data is empty or invalid
 *
 * @example
 * import { buildMerkleTree } from './merkle-batcher.mjs';
 * const tree = await buildMerkleTree([
 *   { subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' },
 *   { subject: 'ex:s2', predicate: 'ex:p2', object: 'ex:o2' },
 * ]);
 * console.log('Merkle root:', tree.hash);
 */
export async function buildMerkleTree(data) {
  if (!Array.isArray(data) || data.length === 0) {
    throw new TypeError('buildMerkleTree: data must be non-empty array');
  }

  // Build leaf nodes
  const leaves = await Promise.all(
    data.map(async (item, index) => ({
      hash: await computeLeafHash(item),
      data: item,
      index,
    }))
  );

  // Build tree bottom-up
  let currentLevel = leaves;

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1];

      if (right) {
        // Two children - compute parent
        const parentHash = await computeParentHash(left.hash, right.hash);
        nextLevel.push({
          hash: parentHash,
          left,
          right,
          index: Math.floor(i / 2),
        });
      } else {
        // Odd number - promote single node (or duplicate for complete binary tree)
        // Using duplication for complete binary tree
        const parentHash = await computeParentHash(left.hash, left.hash);
        nextLevel.push({
          hash: parentHash,
          left,
          right: left, // Duplicate
          index: Math.floor(i / 2),
        });
      }
    }

    currentLevel = nextLevel;
  }

  // Return root
  return currentLevel[0];
}

/**
 * Generate Merkle Proof
 * Creates proof path from leaf to root
 *
 * @param {Object} tree - Merkle tree root
 * @param {number} leafIndex - Index of leaf to prove
 * @returns {Object} Merkle proof
 * @throws {Error} If leaf index invalid
 *
 * @example
 * const proof = generateMerkleProof(tree, 1);
 * console.log('Proof path length:', proof.proof.length);
 */
export function generateMerkleProof(tree, leafIndex) {
  if (typeof leafIndex !== 'number' || leafIndex < 0) {
    throw new TypeError('generateMerkleProof: leafIndex must be non-negative number');
  }

  const proof = [];
  let currentNode = tree;
  let currentIndex = leafIndex;

  // Find leaf
  let leaf = null;
  function findLeaf(node) {
    if (node.data !== undefined && node.index === leafIndex) {
      leaf = node;
      return true;
    }
    if (node.left && findLeaf(node.left)) return true;
    if (node.right && findLeaf(node.right)) return true;
    return false;
  }

  findLeaf(tree);

  if (!leaf) {
    throw new Error(`generateMerkleProof: Leaf at index ${leafIndex} not found`);
  }

  // Build proof path
  function buildProof(node, targetIndex) {
    if (node.data !== undefined) {
      // Reached leaf
      return node.index === targetIndex;
    }

    const leftMatch = node.left && buildProof(node.left, targetIndex);
    const rightMatch = node.right && buildProof(node.right, targetIndex);

    if (leftMatch) {
      // Target in left subtree - add right sibling to proof
      if (node.right && node.right !== node.left) {
        proof.push({
          hash: node.right.hash,
          position: 'right',
        });
      }
      return true;
    }

    if (rightMatch) {
      // Target in right subtree - add left sibling to proof
      if (node.left && node.left !== node.right) {
        proof.push({
          hash: node.left.hash,
          position: 'left',
        });
      }
      return true;
    }

    return false;
  }

  buildProof(tree, leafIndex);

  const result = {
    leaf: leaf.hash,
    index: leafIndex,
    proof,
    root: tree.hash,
  };

  // Validate proof schema
  MerkleProofSchema.parse(result);

  return result;
}

/**
 * Verify Merkle Proof
 * Verifies a Merkle proof against a root hash
 *
 * @param {Object} proof - Merkle proof
 * @param {string} leafHash - Leaf hash to verify
 * @returns {Promise<boolean>} True if proof is valid
 *
 * @example
 * const valid = await verifyMerkleProof(proof, leafHash);
 * console.log('Proof valid:', valid);
 */
export async function verifyMerkleProof(proof, leafHash) {
  // Validate proof schema
  try {
    MerkleProofSchema.parse(proof);
  } catch (err) {
    return false;
  }

  // Verify leaf hash matches
  if (proof.leaf !== leafHash) {
    return false;
  }

  // Compute root by following proof path
  let currentHash = leafHash;

  for (const step of proof.proof) {
    if (step.position === 'left') {
      // Sibling is on left
      currentHash = await computeParentHash(step.hash, currentHash);
    } else {
      // Sibling is on right
      currentHash = await computeParentHash(currentHash, step.hash);
    }
  }

  // Compare computed root with proof root
  return currentHash === proof.root;
}

/**
 * Batch Operations with Merkle Tree
 * Creates batch receipt with Merkle tree for verification
 *
 * @param {Array<Object>} operations - Array of operations
 * @returns {Promise<Object>} Batch with Merkle root and tree
 *
 * @example
 * import { batchWithMerkleTree } from './merkle-batcher.mjs';
 * const batch = await batchWithMerkleTree(operations);
 * console.log('Merkle root:', batch.merkleRoot);
 * console.log('Tree depth:', calculateDepth(batch.tree));
 */
export async function batchWithMerkleTree(operations) {
  if (!Array.isArray(operations) || operations.length === 0) {
    throw new TypeError('batchWithMerkleTree: operations must be non-empty array');
  }

  // Build Merkle tree
  const tree = await buildMerkleTree(operations);

  return {
    operations,
    merkleRoot: tree.hash,
    tree,
    batchSize: operations.length,
  };
}

/**
 * Verify Operation in Batch
 * Verifies single operation is included in batch using Merkle proof
 *
 * @param {Object} operation - Operation to verify
 * @param {number} index - Index of operation in batch
 * @param {Object} batch - Batch with Merkle tree
 * @returns {Promise<boolean>} True if operation is valid member of batch
 *
 * @example
 * const valid = await verifyOperationInBatch(operation, 2, batch);
 * console.log('Operation verified:', valid);
 */
export async function verifyOperationInBatch(operation, index, batch) {
  if (!batch || !batch.tree) {
    throw new TypeError('verifyOperationInBatch: batch must have tree');
  }

  // Generate proof for this operation
  const proof = generateMerkleProof(batch.tree, index);

  // Compute leaf hash
  const leafHash = await computeLeafHash(operation);

  // Verify proof
  return verifyMerkleProof(proof, leafHash);
}

/**
 * Get Merkle Root
 * Extracts Merkle root hash from tree
 *
 * @param {Object} tree - Merkle tree
 * @returns {string} Root hash
 *
 * @example
 * const root = getMerkleRoot(tree);
 * console.assert(root.length === 64);
 */
export function getMerkleRoot(tree) {
  if (!tree || !tree.hash) {
    throw new TypeError('getMerkleRoot: tree must have hash property');
  }

  return tree.hash;
}

/**
 * Calculate Tree Depth
 * Computes depth of Merkle tree
 *
 * @param {Object} tree - Merkle tree
 * @returns {number} Tree depth (0 for single leaf)
 *
 * @example
 * const depth = calculateTreeDepth(tree);
 * console.log('Tree depth:', depth);
 */
export function calculateTreeDepth(tree) {
  if (!tree) return 0;

  if (tree.data !== undefined) {
    // Leaf node
    return 0;
  }

  const leftDepth = tree.left ? calculateTreeDepth(tree.left) : 0;
  const rightDepth = tree.right ? calculateTreeDepth(tree.right) : 0;

  return 1 + Math.max(leftDepth, rightDepth);
}

/**
 * Get Leaf Count
 * Counts number of leaves in tree
 *
 * @param {Object} tree - Merkle tree
 * @returns {number} Number of leaves
 *
 * @example
 * const count = getLeafCount(tree);
 * console.log('Leaf count:', count);
 */
export function getLeafCount(tree) {
  if (!tree) return 0;

  if (tree.data !== undefined) {
    // Leaf node
    return 1;
  }

  const leftCount = tree.left ? getLeafCount(tree.left) : 0;
  const rightCount = tree.right && tree.right !== tree.left
    ? getLeafCount(tree.right)
    : 0;

  return leftCount + rightCount;
}
