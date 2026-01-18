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
  let current = leaves;
  while (current.length > 1) {
    const next = [];
    for (let i = 0; i < current.length; i += 2) {
      const left = current[i];
      const right = current[i + 1] || current[i];
      const combined = left + right;
      const hash = await blake3(combined);
      next.push(hash);
    }
    current = next;
  }
  return current[0];
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
      const right = current[i + 1] || current[i];
      const combined = left + right;
      const hash = await blake3(combined);
      next.push(hash);
    }
    current = next;
    currentIndex = Math.floor(currentIndex / 2);
  }
  return proof;
}
