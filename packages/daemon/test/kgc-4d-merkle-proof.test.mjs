import { describe, expect, it } from 'vitest';
import { blake3 } from 'hash-wasm';
import {
  buildMerkleTree,
  getMerkleProofPath,
  verifyMerkleProof,
} from '../src/integrations/kgc-4d-merkle.mjs';
import { DaemonEventStore } from '../src/integrations/kgc-4d-sourcing.mjs';

async function hashes(count) {
  return Promise.all(Array.from({ length: count }, (_, index) => blake3(`leaf-${index}`)));
}

async function proofFor(leaves, leafIndex) {
  return {
    leafIndex,
    leafCount: leaves.length,
    leafHash: leaves[leafIndex],
    proof: await getMerkleProofPath(leaves, leafIndex),
    merkleRoot: await buildMerkleTree(leaves),
  };
}

describe('KGC-4D Merkle proof law', () => {
  it.each([1, 2, 3, 4, 5, 8])('verifies every leaf in a %i-leaf tree', async leafCount => {
    const leaves = await hashes(leafCount);
    for (let leafIndex = 0; leafIndex < leafCount; leafIndex += 1) {
      expect(await verifyMerkleProof(await proofFor(leaves, leafIndex))).toBe(true);
    }
  });

  it('rejects a tampered leaf, path, root, or leaf count', async () => {
    const leaves = await hashes(5);
    const proof = await proofFor(leaves, 2);
    expect(await verifyMerkleProof({ ...proof, leafHash: await blake3('tampered') })).toBe(false);
    expect(await verifyMerkleProof({
      ...proof,
      proof: proof.proof.map((step, index) => index === 0 ? { ...step, hash: '0'.repeat(64) } : step),
    })).toBe(false);
    expect(await verifyMerkleProof({ ...proof, merkleRoot: '0'.repeat(64) })).toBe(false);
    expect(await verifyMerkleProof({ ...proof, leafCount: proof.leafCount + 1 })).toBe(false);
  });

  it('binds duplicate-tail trees to their exact leaf count', async () => {
    const leaves = await hashes(2);
    const twoLeafRoot = await buildMerkleTree(leaves);
    const threeLeafRoot = await buildMerkleTree([...leaves, leaves[1]]);
    expect(twoLeafRoot).not.toBe(threeLeafRoot);
  });

  it('round-trips every event proof through DaemonEventStore', async () => {
    const store = new DaemonEventStore({ logger: { log() {}, error() {} } });
    await store.initialize();
    for (let index = 0; index < 7; index += 1) {
      await store.appendEvent(`operation-${index}`, { index });
    }
    for (let index = 0; index < store.eventLog.length; index += 1) {
      const proof = await store.generateMerkleProof(index);
      expect(proof.leafCount).toBe(store.eventLog.length);
      expect(await store.verifyProof(proof)).toBe(true);
    }
  });
});
