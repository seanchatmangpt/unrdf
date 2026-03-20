/**
 * @file Receipts Merkle Tree Operations
 * @module @unrdf/daemon/integrations/receipts-merkle-tree
 * @description Merkle tree building and proof operations for receipt batching
 */

import { blake3 } from 'hash-wasm';

/**
 *
 */
export async function buildMerkleTree(receipts) {
  if (receipts.length === 0) {
    throw new Error('Cannot build tree: empty receipts array');
  }
  if (receipts.length === 1) {
    return {
      root: receipts[0].merkleLeafHash,
      depth: 0,
      leafCount: 1,
      leaves: [receipts[0].merkleLeafHash],
      levels: [[receipts[0].merkleLeafHash]],
    };
  }
  const leaves = receipts.map(r => r.merkleLeafHash);
  const levels = [leaves];
  let currentLevel = leaves;
  while (currentLevel.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < currentLevel.length; i += 2) {
      if (i + 1 < currentLevel.length) {
        const combined = currentLevel[i] + ':' + currentLevel[i + 1];
        const parentHash = await blake3(combined);
        nextLevel.push(parentHash);
      } else {
        // CVE-2012-2459 mitigation: Use domain separator for padding nodes
        // to distinguish them from real leaf pairs. Without this tag, an attacker
        // could append a duplicate of the last leaf and get the same root hash.
        const combined = '\x01' + currentLevel[i] + ':' + currentLevel[i];
        const parentHash = await blake3(combined);
        nextLevel.push(parentHash);
      }
    }
    levels.push(nextLevel);
    currentLevel = nextLevel;
  }
  return {
    root: currentLevel[0],
    depth: levels.length - 1,
    leafCount: leaves.length,
    leaves,
    levels,
  };
}

/**
 *
 */
export async function generateInclusionProof(tree, index) {
  const proof = [];
  let currentIndex = index;
  for (let levelIdx = 0; levelIdx < tree.levels.length - 1; levelIdx++) {
    const level = tree.levels[levelIdx];
    const siblingIndex = currentIndex % 2 === 0 ? currentIndex + 1 : currentIndex - 1;
    if (siblingIndex < level.length) {
      proof.push({
        hash: level[siblingIndex],
        position: currentIndex % 2 === 0 ? 'right' : 'left',
      });
    } else {
      // Odd leaf at end of level - sibling is self with domain separator
      proof.push({
        hash: level[currentIndex],
        position: 'pad',
      });
    }
    currentIndex = Math.floor(currentIndex / 2);
  }
  return proof;
}

/**
 *
 */
export async function verifyInclusionProof(proof) {
  try {
    if (!proof || !proof.leafHash || !proof.proofPath || !proof.merkleRoot) {
      return false;
    }
    let currentHash = proof.leafHash;
    for (const step of proof.proofPath) {
      let combined;
      if (step.position === 'pad') {
        // Odd leaf padding - uses domain separator matching buildMerkleTree
        combined = '\x01' + currentHash + ':' + step.hash;
      } else if (step.position === 'right') {
        combined = currentHash + ':' + step.hash;
      } else {
        combined = step.hash + ':' + currentHash;
      }
      currentHash = await blake3(combined);
    }
    return currentHash === proof.merkleRoot;
  } catch {
    return false;
  }
}
