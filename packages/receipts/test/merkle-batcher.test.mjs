/**
 * KGC Receipts - Merkle Batcher Tests - Refactored for speed
 * Tests Merkle tree construction (essential tests only)
 */

import { describe, it, expect } from 'vitest';
import {
  buildMerkleTree,
  generateMerkleProof,
  verifyMerkleProof,
  batchWithMerkleTree,
} from '../src/merkle-batcher.mjs';

describe('Merkle Batcher', () => {
  const sampleData = [
    { subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' },
    { subject: 'ex:s2', predicate: 'ex:p2', object: 'ex:o2' },
    { subject: 'ex:s3', predicate: 'ex:p3', object: 'ex:o3' },
    { subject: 'ex:s4', predicate: 'ex:p4', object: 'ex:o4' },
  ];

  it('builds Merkle tree from data array', async () => {
    const tree = await buildMerkleTree(sampleData);

    expect(tree).toHaveProperty('hash');
    expect(tree.hash).toMatch(/^[a-f0-9]{64}$/);
    expect(tree).toHaveProperty('left');
    expect(tree).toHaveProperty('right');
  });

  it('generates and verifies Merkle proof', async () => {
    const tree = await buildMerkleTree(sampleData);
    const proof = generateMerkleProof(tree, 1);

    const valid = await verifyMerkleProof(proof, proof.leaf);

    expect(valid).toBe(true);
    expect(proof).toHaveProperty('leaf');
    expect(proof).toHaveProperty('root');
  });

  it('creates batch with Merkle tree', async () => {
    const batch = await batchWithMerkleTree(sampleData);

    expect(batch).toHaveProperty('operations');
    expect(batch).toHaveProperty('merkleRoot');
    expect(batch.batchSize).toBe(4);
  });
});
