/**
 * @file KGC-4D Merkle Tree Operations
 * @module @unrdf/daemon/integrations/kgc-4d-merkle
 * @description Merkle tree building and proof generation for event sourcing
 */

import { blake3 } from 'hash-wasm';

export async function buildMerkleTree(leaves) {
  if (leaves.length === 0) {
    return await blake3('');
  }
  if (leaves.length === 1) {
    return leaves[0];
  }
  let current = leaves;
  while (current.length > 1) {
    const next = [];
    for (let i = 0; i < current.length; i += 2) {
      const left = current[i];
      const right = i + 1 < current.length ? current[i + 1] : current[i];
      // Use separator to prevent ambiguity between leaf content and combinations
      const combined = left + ':' + right;
      const hash = await blake3(combined);
      next.push(hash);
    }
    current = next;
  }
  // CVE-2012-2459 mitigation: include leaf count in root hash to ensure
  // a tree with N leaves always has a different root than N+1 leaves
  // even if the extra leaf is a duplicate.
  const rawRoot = current[0];
  return await blake3(`${leaves.length}:${rawRoot}`);
}

export async function getMerkleProofPath(leaves, index) {
  if (leaves.length === 0 || index >= leaves.length) {
    return [];
  }
  const proof = [];
  let current = leaves;
  let currentIndex = index;
  while (current.length > 1) {
    const isRight = currentIndex % 2 === 1;
    const siblingIndex = isRight ? currentIndex - 1 : currentIndex + 1;
    const siblingHash = siblingIndex < current.length ? current[siblingIndex] : current[currentIndex];
    proof.push({
      hash: siblingHash,
      position: isRight ? 'left' : 'right',
    });
    const next = [];
    for (let i = 0; i < current.length; i += 2) {
      const left = current[i];
      const right = i + 1 < current.length ? current[i + 1] : current[i];
      const combined = left + ':' + right;
      const hash = await blake3(combined);
      next.push(hash);
    }
    current = next;
    currentIndex = Math.floor(currentIndex / 2);
  }
  return proof;
}
