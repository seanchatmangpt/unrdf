/**
 * @fileoverview Tests for Merkle Proofs
 */

import { describe, it, expect } from 'vitest';
import {
  computeMerkleRoot,
  generateCompactProof,
  verifyCompactProof,
  generateMultiProof,
  verifyMultiProof,
  batchVerify,
  compressProof,
  decompressProof,
  serializeProof,
  deserializeProof,
  getProofSize,
  getProofStats,
} from './merkle-proofs.mjs';

describe('Merkle Proofs', () => {
  describe('computeMerkleRoot', () => {
    it('should compute root for single hash', async () => {
      const root = await computeMerkleRoot(['hash1']);
      expect(root).toBe('hash1');
    });

    it('should compute root for power of 2 hashes', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const root = await computeMerkleRoot(hashes);
      expect(root).toMatch(/^[a-f0-9]+$/);
    });

    it('should compute root for non-power of 2', async () => {
      const hashes = ['h1', 'h2', 'h3'];
      const root = await computeMerkleRoot(hashes);
      expect(root).toMatch(/^[a-f0-9]+$/);
    });

    it('should be deterministic', async () => {
      const hashes = ['h1', 'h2', 'h3'];
      const root1 = await computeMerkleRoot(hashes);
      const root2 = await computeMerkleRoot(hashes);
      expect(root1).toBe(root2);
    });

    it('should throw error for empty array', async () => {
      await expect(computeMerkleRoot([])).rejects.toThrow('empty array');
    });
  });

  describe('Compact Proofs', () => {
    it('should generate and verify proof for first element', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const { root, proof, index } = await generateCompactProof(hashes, 0);

      expect(index).toBe(0);
      expect(Array.isArray(proof)).toBe(true);

      const isValid = await verifyCompactProof(hashes[0], root, proof, index);
      expect(isValid).toBe(true);
    });

    it('should generate and verify proof for middle element', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const { root, proof, index } = await generateCompactProof(hashes, 2);

      const isValid = await verifyCompactProof(hashes[2], root, proof, index);
      expect(isValid).toBe(true);
    });

    it('should generate and verify proof for last element', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const { root, proof, index } = await generateCompactProof(hashes, 3);

      const isValid = await verifyCompactProof(hashes[3], root, proof, index);
      expect(isValid).toBe(true);
    });

    it('should have O(log n) proof size', async () => {
      const hashes = Array.from({ length: 16 }, (_, i) => `hash${i}`);
      const { proof } = await generateCompactProof(hashes, 7);

      // log2(16) = 4
      expect(proof.length).toBe(4);
    });

    it('should reject invalid proof', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const { root, proof } = await generateCompactProof(hashes, 0);

      // Tamper with proof
      proof[0] = 'tampered';

      const isValid = await verifyCompactProof(hashes[0], root, proof, 0);
      expect(isValid).toBe(false);
    });

    it('should reject proof for wrong receipt', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const { root, proof } = await generateCompactProof(hashes, 0);

      const isValid = await verifyCompactProof('wrong_hash', root, proof, 0);
      expect(isValid).toBe(false);
    });

    it('should throw error for out of bounds index', async () => {
      const hashes = ['h1', 'h2'];
      await expect(generateCompactProof(hashes, 5)).rejects.toThrow('out of bounds');
    });
  });

  describe('Multi-Proofs', () => {
    it('should generate and verify multi-proof', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8'];
      const indices = [1, 3, 5];

      const { root, proofs } = await generateMultiProof(hashes, indices);

      const { valid, results } = await verifyMultiProof(hashes, root, proofs);

      expect(valid).toBe(true);
      expect(results.size).toBe(3);
    });

    it('should detect invalid proof in multi-proof', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];
      const { root, proofs } = await generateMultiProof(hashes, [0, 2]);

      // Tamper with one proof
      const proof0 = proofs.get(0);
      proof0[0] = 'tampered';

      const { valid, results } = await verifyMultiProof([hashes[0], hashes[2]], root, proofs);

      expect(valid).toBe(false);
      expect(results.get(0)).toBe(false);
    });

    it('should throw error for empty indices', async () => {
      await expect(generateMultiProof(['h1', 'h2'], [])).rejects.toThrow(
        'at least one index'
      );
    });
  });

  describe('Batch Verify', () => {
    it('should batch verify multiple independent proofs', async () => {
      const batch = [];

      for (let i = 0; i < 3; i++) {
        const hashes = Array.from({ length: 4 }, (_, j) => `chain${i}_hash${j}`);
        const { root, proof } = await generateCompactProof(hashes, 1);
        batch.push({
          receiptHash: hashes[1],
          root,
          proof,
          index: 1,
        });
      }

      const { valid, results } = await batchVerify(batch);

      expect(valid).toBe(true);
      expect(results.length).toBe(3);
      expect(results.every((r) => r)).toBe(true);
    });

    it('should detect invalid proof in batch', async () => {
      const batch = [];

      // Valid proof
      const hashes1 = ['h1', 'h2'];
      const proof1 = await generateCompactProof(hashes1, 0);
      batch.push({
        receiptHash: hashes1[0],
        root: proof1.root,
        proof: proof1.proof,
        index: 0,
      });

      // Invalid proof
      batch.push({
        receiptHash: 'wrong',
        root: proof1.root,
        proof: proof1.proof,
        index: 0,
      });

      const { valid, results } = await batchVerify(batch);

      expect(valid).toBe(false);
      expect(results[0]).toBe(true);
      expect(results[1]).toBe(false);
    });
  });

  describe('Proof Serialization', () => {
    it('should compress and decompress proof', () => {
      const proof = ['hash1', 'hash2', 'hash3'];
      const compressed = compressProof(proof);
      const decompressed = decompressProof(compressed);

      expect(decompressed).toEqual(proof);
    });

    it('should serialize and deserialize proof', () => {
      const proofData = {
        root: 'root_hash',
        proof: ['p1', 'p2'],
        index: 5,
      };

      const serialized = serializeProof(proofData);
      const deserialized = deserializeProof(serialized);

      expect(deserialized).toEqual(proofData);
    });
  });

  describe('Proof Statistics', () => {
    it('should calculate proof size', () => {
      const proof = ['hash1', 'hash2', 'hash3'];
      const size = getProofSize(proof);

      // 3 hashes * 32 bytes = 96 bytes
      expect(size).toBe(96);
    });

    it('should calculate proof stats', async () => {
      const hashes = Array.from({ length: 16 }, (_, i) => `hash${i}`);
      const stats = await getProofStats(hashes);

      expect(stats.treeSize).toBe(16);
      expect(stats.maxDepth).toBe(4); // log2(16) = 4
      expect(stats.maxProofSize).toBe(128); // 4 * 32 bytes
    });
  });

  describe('Integration Tests', () => {
    it('should handle large tree (1000 receipts)', async () => {
      const hashes = Array.from({ length: 1000 }, (_, i) => `receipt_${i}`);

      const { root, proof, index } = await generateCompactProof(hashes, 500);

      // Proof size should be ~10 (log2(1000) ≈ 10)
      expect(proof.length).toBeLessThanOrEqual(10);

      const isValid = await verifyCompactProof(hashes[500], root, proof, index);
      expect(isValid).toBe(true);
    });

    it('should verify all receipts in small tree', async () => {
      const hashes = ['h1', 'h2', 'h3', 'h4'];

      for (let i = 0; i < hashes.length; i++) {
        const { root, proof } = await generateCompactProof(hashes, i);
        const isValid = await verifyCompactProof(hashes[i], root, proof, i);
        expect(isValid).toBe(true);
      }
    });
  });
});
