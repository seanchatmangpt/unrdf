/**
 * @file KGC-4D Merkle Tree Operations
 * @module @unrdf/daemon/integrations/kgc-4d-merkle
 * @description Merkle tree building, proof generation, and proof verification.
 */

import { blake3 } from 'hash-wasm';

const HASH_PATTERN = /^[a-f0-9]{64}$/i;
const NODE_DOMAIN = 'urn:unrdf:daemon:merkle-node:v1';
const ROOT_DOMAIN = 'urn:unrdf:daemon:merkle-root:v1';

function requireLeaves(leaves) {
  if (!Array.isArray(leaves)) throw new TypeError('leaves must be an array');
  for (const leaf of leaves) {
    if (typeof leaf !== 'string' || !HASH_PATTERN.test(leaf)) {
      throw new TypeError('each leaf must be a 64-character hexadecimal hash');
    }
  }
}

async function parentHash(left, right) {
  return blake3(`${NODE_DOMAIN}:${left}:${right}`);
}

async function bindRoot(rawRoot, leafCount) {
  return blake3(`${ROOT_DOMAIN}:${leafCount}:${rawRoot}`);
}

export async function buildMerkleTree(leaves) {
  requireLeaves(leaves);
  if (leaves.length === 0) return bindRoot('', 0);

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
  return bindRoot(current[0], leaves.length);
}

export async function getMerkleProofPath(leaves, index) {
  requireLeaves(leaves);
  if (!Number.isInteger(index) || index < 0) {
    throw new TypeError('index must be a non-negative integer');
  }
  if (index >= leaves.length) {
    throw new RangeError(`index ${index} is outside a ${leaves.length}-leaf tree`);
  }

  const proof = [];
  let current = [...leaves];
  let currentIndex = index;
  while (current.length > 1) {
    const isRight = currentIndex % 2 === 1;
    const siblingIndex = isRight ? currentIndex - 1 : currentIndex + 1;
    const siblingHash = siblingIndex < current.length ? current[siblingIndex] : current[currentIndex];
    proof.push({ hash: siblingHash, position: isRight ? 'left' : 'right' });

    const next = [];
    for (let pairIndex = 0; pairIndex < current.length; pairIndex += 2) {
      const left = current[pairIndex];
      const right = pairIndex + 1 < current.length ? current[pairIndex + 1] : current[pairIndex];
      next.push(await parentHash(left, right));
    }
    current = next;
    currentIndex = Math.floor(currentIndex / 2);
  }
  return proof;
}

export async function verifyMerkleProof(proof) {
  if (!proof || typeof proof !== 'object') throw new TypeError('proof must be an object');
  if (!Number.isInteger(proof.leafCount) || proof.leafCount <= 0) {
    throw new TypeError('leafCount must be a positive integer');
  }
  if (!Number.isInteger(proof.leafIndex) || proof.leafIndex < 0 || proof.leafIndex >= proof.leafCount) {
    throw new TypeError('leafIndex must identify a leaf within leafCount');
  }
  if (typeof proof.leafHash !== 'string' || !HASH_PATTERN.test(proof.leafHash)) {
    throw new TypeError('leafHash must be a 64-character hexadecimal hash');
  }
  if (!Array.isArray(proof.proof)) throw new TypeError('proof path must be an array');
  if (typeof proof.merkleRoot !== 'string' || !HASH_PATTERN.test(proof.merkleRoot)) {
    throw new TypeError('merkleRoot must be a 64-character hexadecimal hash');
  }

  let currentHash = proof.leafHash;
  let currentIndex = proof.leafIndex;
  let width = proof.leafCount;
  let stepIndex = 0;

  while (width > 1) {
    const step = proof.proof[stepIndex];
    if (!step || typeof step.hash !== 'string' || !HASH_PATTERN.test(step.hash) || !['left', 'right'].includes(step.position)) {
      throw new TypeError('each proof step must contain a hash and left/right position');
    }

    const isRight = currentIndex % 2 === 1;
    const expectedPosition = isRight ? 'left' : 'right';
    if (step.position !== expectedPosition) return false;
    const isDuplicatedTail = !isRight && currentIndex + 1 >= width;
    if (isDuplicatedTail && step.hash !== currentHash) return false;

    currentHash = isRight
      ? await parentHash(step.hash, currentHash)
      : await parentHash(currentHash, step.hash);
    currentIndex = Math.floor(currentIndex / 2);
    width = Math.ceil(width / 2);
    stepIndex += 1;
  }

  if (stepIndex !== proof.proof.length) return false;
  return await bindRoot(currentHash, proof.leafCount) === proof.merkleRoot;
}
