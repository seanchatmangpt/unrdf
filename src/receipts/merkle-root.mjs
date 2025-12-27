/**
 * @fileoverview Merkle Root - Batch multiple receipts into a Merkle tree
 *
 * **Purpose**: Efficiently batch N receipts into a single Merkle root
 * - Enables efficient proof of inclusion (log N)
 * - Reduces storage for large receipt sets
 * - Cryptographically binds receipts together
 *
 * **Algorithm**: Standard binary Merkle tree
 * 1. Hash all receipt hashes (leaves)
 * 2. Pair and hash recursively until root
 * 3. Handle odd counts by duplicating last node
 *
 * @module receipts/merkle-root
 */

import { blake3 } from 'hash-wasm';

/**
 * Compute Merkle root from receipt hashes
 *
 * **Algorithm**:
 * - Leaves: receipt hashes
 * - Internal nodes: hash(left + right)
 * - Odd counts: duplicate last node
 *
 * @param {string[]} receiptHashes - Receipt hashes (leaves)
 * @returns {Promise<string>} Merkle root hash
 *
 * @example
 * const root = await computeMerkleRoot([hash1, hash2, hash3, hash4]);
 */
export async function computeMerkleRoot(receiptHashes) {
  if (receiptHashes.length === 0) {
    throw new Error('Cannot compute Merkle root of empty array');
  }

  if (receiptHashes.length === 1) {
    return receiptHashes[0];
  }

  // Build tree bottom-up
  let currentLevel = [...receiptHashes];

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1] || currentLevel[i]; // Duplicate if odd

      // Hash concatenation: hash(left + right)
      const combined = left + right;
      const parentHash = await blake3(combined);
      nextLevel.push(parentHash);
    }

    currentLevel = nextLevel;
  }

  return currentLevel[0];
}

/**
 * Generate Merkle proof for a receipt at given index
 *
 * **Proof structure**: Array of sibling hashes from leaf to root
 * - Proves receipt is included in tree
 * - Verification: recompute root using proof path
 *
 * @param {string[]} receiptHashes - All receipt hashes
 * @param {number} index - Index of receipt to prove
 * @returns {Promise<{root: string, proof: Array<{hash: string, position: 'left'|'right'}>}>}
 *
 * @example
 * const { root, proof } = await generateMerkleProof([h1, h2, h3, h4], 2);
 * // proof = [{hash: h3, position: 'right'}, {hash: h(h1,h2), position: 'left'}]
 */
export async function generateMerkleProof(receiptHashes, index) {
  if (index < 0 || index >= receiptHashes.length) {
    throw new Error(`Index ${index} out of bounds [0, ${receiptHashes.length})`);
  }

  const proof = [];
  let currentLevel = [...receiptHashes];
  let currentIndex = index;

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1] || currentLevel[i];

      // If current index is in this pair, record sibling
      if (i === currentIndex || i + 1 === currentIndex) {
        if (currentIndex === i) {
          // Left child - sibling is right
          proof.push({ hash: right, position: 'right' });
        } else {
          // Right child - sibling is left
          proof.push({ hash: left, position: 'left' });
        }
      }

      const combined = left + right;
      const parentHash = await blake3(combined);
      nextLevel.push(parentHash);
    }

    // Update index for next level
    currentIndex = Math.floor(currentIndex / 2);
    currentLevel = nextLevel;
  }

  return {
    root: currentLevel[0],
    proof,
  };
}

/**
 * Verify Merkle proof for a receipt hash
 *
 * @param {string} receiptHash - Receipt hash (leaf)
 * @param {string} merkleRoot - Expected Merkle root
 * @param {Array<{hash: string, position: 'left'|'right'}>} proof - Merkle proof
 * @returns {Promise<boolean>} True if proof is valid
 *
 * @example
 * const isValid = await verifyMerkleProof(receiptHash, root, proof);
 */
export async function verifyMerkleProof(receiptHash, merkleRoot, proof) {
  let currentHash = receiptHash;

  for (const { hash: siblingHash, position } of proof) {
    // Combine with sibling in correct order
    const combined =
      position === 'left' ? siblingHash + currentHash : currentHash + siblingHash;

    currentHash = await blake3(combined);
  }

  return currentHash === merkleRoot;
}

/**
 * Batch receipts and compute Merkle root
 *
 * **Use case**: Group multiple receipts under single root hash
 * - Efficient storage (1 root vs N receipts)
 * - Proof of inclusion (log N verification)
 * - Tamper detection (any change invalidates root)
 *
 * @param {import('./receipt.mjs').Receipt[]} receipts - Receipts to batch
 * @returns {Promise<string>} Merkle root hash
 */
export async function batchReceipts(receipts) {
  const receiptHashes = receipts.map(r => r.receiptHash);
  return computeMerkleRoot(receiptHashes);
}
