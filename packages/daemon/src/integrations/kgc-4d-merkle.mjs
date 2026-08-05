/**
 * @file KGC-4D Merkle Tree Operations
 * @module @unrdf/daemon/integrations/kgc-4d-merkle
 * @description Merkle tree building, proof generation, and proof verification.
 */

import { blake3 } from 'hash-wasm';

function requireLeaves(leaves) {
  if (!Array.isArray(leaves)) throw new TypeError('leaves must be an array');
  for (const leaf of leaves) {
    if (typeof leaf !== 'string' || leaf.length === 0) {
      throw new TypeError('each leaf must be a non-empty hash string');
    }
  }
}

async function parentHash(left, right) {
  return blake3(`${left}:${right}`);
}

async function bindRoot(rawRoot, leafCount) {
  if (leafCount === 0) return blake3('');
  if (leafCount === 1) return rawRoot;
  return blake3(`${leafCount}:${rawRoot}`);
}

export async function buildMerkleTree(leaves) {
  requireLeaves(leaves);
  if (leaves.length === 0) return bindRoot('', 0);
  if (leaves.length === 1) return bindRoot(leaves[0], 1);

  let current = [...leaves];
  while (current.length > 1) {
    const next = [];
    for (let index = 0; index < current.length; index += 2) {
      const left = current[index];
      const right = index + 1 < current.length ? current[index + 1] : current[index];
      next.push(await parentHash(left, right));
    }
    current = next;
  }

  // CVE-2012-2459 mitigation: bind the number of leaves into every
  // multi-leaf root so duplicate-tail extension cannot preserve identity.
  return bindRoot(current[0], leaves.length);
}

export async function getMerkleProofPath(leaves, index) {
  requireLeaves(leaves);
  if (!Number.isInteger(index) || index < 0) {
    throw new TypeError('index must be a non-negative integer');
  }
  if (index >= leaves.length) return [];

  const proof = [];
  let current = [...leaves];
  let currentIndex = index;
  while (current.length > 1) {
    const isRight = currentIndex % 2 === 1;
    const siblingIndex = isRight ? currentIndex - 1 : currentIndex + 1;
    const siblingHash = siblingIndex < current.length
      ? current[siblingIndex]
      : current[currentIndex];
    proof.push({
      hash: siblingHash,
      position: isRight ? 'left' : 'right',
    });

    const next = [];
    for (let pairIndex = 0; pairIndex < current.length; pairIndex += 2) {
      const left = current[pairIndex];
      const right = pairIndex + 1 < current.length
        ? current[pairIndex + 1]
        : current[pairIndex];
      next.push(await parentHash(left, right));
    }
    current = next;
    currentIndex = Math.floor(currentIndex / 2);
  }
  return proof;
}

/**
 * Verify a proof against the same parent and root domains used by construction.
 * @param {{leafIndex:number, leafCount:number, leafHash:string, proof:Array<{hash:string,position:'left'|'right'}>, merkleRoot:string}} proof
 * @returns {Promise<boolean>}
 */
export async function verifyMerkleProof(proof) {
  if (!proof || typeof proof !== 'object') throw new TypeError('proof must be an object');
  if (!Number.isInteger(proof.leafCount) || proof.leafCount <= 0) {
    throw new TypeError('leafCount must be a positive integer');
  }
  if (!Number.isInteger(proof.leafIndex) || proof.leafIndex < 0 || proof.leafIndex >= proof.leafCount) {
    throw new TypeError('leafIndex must identify a leaf within leafCount');
  }
  if (typeof proof.leafHash !== 'string' || proof.leafHash.length === 0) {
    throw new TypeError('leafHash must be a non-empty string');
  }
  if (!Array.isArray(proof.proof)) throw new TypeError('proof path must be an array');
  if (typeof proof.merkleRoot !== 'string' || proof.merkleRoot.length === 0) {
    throw new TypeError('merkleRoot must be a non-empty string');
  }

  let currentHash = proof.leafHash;
  for (const step of proof.proof) {
    if (!step || typeof step.hash !== 'string' || !['left', 'right'].includes(step.position)) {
      throw new TypeError('each proof step must contain a hash and left/right position');
    }
    currentHash = step.position === 'left'
      ? await parentHash(step.hash, currentHash)
      : await parentHash(currentHash, step.hash);
  }

  return await bindRoot(currentHash, proof.leafCount) === proof.merkleRoot;
}
