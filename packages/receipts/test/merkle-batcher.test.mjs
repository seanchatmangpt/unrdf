/**
 * KGC Receipts - Merkle Batcher Tests
 * Tests Merkle tree construction and verification
 */

import { describe, it, expect } from 'vitest';
import {
  buildMerkleTree,
  generateMerkleProof,
  verifyMerkleProof,
  batchWithMerkleTree,
  verifyOperationInBatch,
  getMerkleRoot,
  calculateTreeDepth,
  getLeafCount,
} from '../src/merkle-batcher.mjs';

describe('Merkle Batcher', () => {
  const sampleData = [
    { subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' },
    { subject: 'ex:s2', predicate: 'ex:p2', object: 'ex:o2' },
    { subject: 'ex:s3', predicate: 'ex:p3', object: 'ex:o3' },
    { subject: 'ex:s4', predicate: 'ex:p4', object: 'ex:o4' },
  ];

  describe('buildMerkleTree', () => {
    it('builds tree from data array', async () => {
      const tree = await buildMerkleTree(sampleData);

      expect(tree).toHaveProperty('hash');
      expect(tree.hash).toMatch(/^[a-f0-9]{64}$/);
      expect(tree).toHaveProperty('left');
      expect(tree).toHaveProperty('right');
    });

    it('creates deterministic tree structure', async () => {
      const tree1 = await buildMerkleTree(sampleData);
      const tree2 = await buildMerkleTree(sampleData);

      expect(tree1.hash).toBe(tree2.hash);
    });

    it('creates different roots for different data', async () => {
      const tree1 = await buildMerkleTree(sampleData);
      const tree2 = await buildMerkleTree([...sampleData, { subject: 'ex:s5', predicate: 'ex:p5', object: 'ex:o5' }]);

      expect(tree1.hash).not.toBe(tree2.hash);
    });

    it('handles single element array', async () => {
      const tree = await buildMerkleTree([sampleData[0]]);

      expect(tree.hash).toMatch(/^[a-f0-9]{64}$/);
    });

    it('rejects empty array', async () => {
      await expect(buildMerkleTree([])).rejects.toThrow(/non-empty array/);
    });
  });

  describe('generateMerkleProof', () => {
    it('generates proof for leaf at index 0', async () => {
      const tree = await buildMerkleTree(sampleData);
      const proof = generateMerkleProof(tree, 0);

      expect(proof).toHaveProperty('leaf');
      expect(proof).toHaveProperty('index');
      expect(proof).toHaveProperty('proof');
      expect(proof).toHaveProperty('root');
      expect(proof.index).toBe(0);
      expect(proof.root).toBe(tree.hash);
    });

    it('generates proof for leaf at index 2', async () => {
      const tree = await buildMerkleTree(sampleData);
      const proof = generateMerkleProof(tree, 2);

      expect(proof.index).toBe(2);
      expect(Array.isArray(proof.proof)).toBe(true);
    });

    it('throws error for invalid leaf index', async () => {
      const tree = await buildMerkleTree(sampleData);

      expect(() => {
        generateMerkleProof(tree, 999);
      }).toThrow(/not found/);
    });
  });

  describe('verifyMerkleProof', () => {
    it('verifies valid proof', async () => {
      const tree = await buildMerkleTree(sampleData);
      const proof = generateMerkleProof(tree, 1);

      const valid = await verifyMerkleProof(proof, proof.leaf);

      expect(valid).toBe(true);
    });

    it('rejects tampered leaf hash', async () => {
      const tree = await buildMerkleTree(sampleData);
      const proof = generateMerkleProof(tree, 1);

      const tamperedHash = 'a'.repeat(64);
      const valid = await verifyMerkleProof(proof, tamperedHash);

      expect(valid).toBe(false);
    });

    it('rejects tampered proof path', async () => {
      const tree = await buildMerkleTree(sampleData);
      const proof = generateMerkleProof(tree, 1);

      // Tamper with proof
      const tamperedProof = {
        ...proof,
        proof: [
          ...proof.proof.slice(0, -1),
          { hash: 'b'.repeat(64), position: 'left' },
        ],
      };

      const valid = await verifyMerkleProof(tamperedProof, proof.leaf);

      expect(valid).toBe(false);
    });
  });

  describe('batchWithMerkleTree', () => {
    it('creates batch with Merkle tree', async () => {
      const batch = await batchWithMerkleTree(sampleData);

      expect(batch).toHaveProperty('operations');
      expect(batch).toHaveProperty('merkleRoot');
      expect(batch).toHaveProperty('tree');
      expect(batch).toHaveProperty('batchSize');
      expect(batch.batchSize).toBe(4);
      expect(batch.merkleRoot).toMatch(/^[a-f0-9]{64}$/);
    });

    it('includes all operations', async () => {
      const batch = await batchWithMerkleTree(sampleData);

      expect(batch.operations).toEqual(sampleData);
    });

    it('rejects empty operations', async () => {
      await expect(batchWithMerkleTree([])).rejects.toThrow(/non-empty array/);
    });
  });

  describe('verifyOperationInBatch', () => {
    it('verifies operation is in batch', async () => {
      const batch = await batchWithMerkleTree(sampleData);

      const valid = await verifyOperationInBatch(sampleData[2], 2, batch);

      expect(valid).toBe(true);
    });

    it('rejects operation not in batch', async () => {
      const batch = await batchWithMerkleTree(sampleData);
      const fakeOp = { subject: 'ex:fake', predicate: 'ex:fake', object: 'ex:fake' };

      const valid = await verifyOperationInBatch(fakeOp, 0, batch);

      expect(valid).toBe(false);
    });
  });

  describe('getMerkleRoot', () => {
    it('extracts root hash from tree', async () => {
      const tree = await buildMerkleTree(sampleData);
      const root = getMerkleRoot(tree);

      expect(root).toBe(tree.hash);
      expect(root).toMatch(/^[a-f0-9]{64}$/);
    });
  });

  describe('calculateTreeDepth', () => {
    it('calculates depth correctly for 4 leaves', async () => {
      const tree = await buildMerkleTree(sampleData);
      const depth = calculateTreeDepth(tree);

      // 4 leaves → depth 2 (leaf → parent → root)
      expect(depth).toBe(2);
    });

    it('returns 0 for single leaf', async () => {
      const tree = await buildMerkleTree([sampleData[0]]);
      const depth = calculateTreeDepth(tree);

      expect(depth).toBe(0);
    });
  });

  describe('getLeafCount', () => {
    it('counts leaves correctly', async () => {
      const tree = await buildMerkleTree(sampleData);
      const count = getLeafCount(tree);

      expect(count).toBe(4);
    });

    it('returns 1 for single leaf', async () => {
      const tree = await buildMerkleTree([sampleData[0]]);
      const count = getLeafCount(tree);

      expect(count).toBe(1);
    });
  });

  describe('Merkle Tree Properties', () => {
    it('root changes when leaf changes', async () => {
      const tree1 = await buildMerkleTree(sampleData);
      const modifiedData = [...sampleData];
      modifiedData[2] = { subject: 'ex:modified', predicate: 'ex:p', object: 'ex:o' };
      const tree2 = await buildMerkleTree(modifiedData);

      expect(tree1.hash).not.toBe(tree2.hash);
    });

    it('proof verification is position-sensitive', async () => {
      const tree = await buildMerkleTree(sampleData);
      const proof = generateMerkleProof(tree, 0);

      // Verify original position
      const valid = await verifyMerkleProof(proof, proof.leaf);
      expect(valid).toBe(true);
    });
  });
});
