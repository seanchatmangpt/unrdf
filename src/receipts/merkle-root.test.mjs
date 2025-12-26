/**
 * @fileoverview Tests for Merkle root computation
 */

import { describe, it, expect } from 'vitest';
import {
  computeMerkleRoot,
  generateMerkleProof,
  verifyMerkleProof,
  batchReceipts,
} from './merkle-root.mjs';
import { Receipt } from './receipt.mjs';

describe('Merkle Root', () => {
  describe('computeMerkleRoot()', () => {
    it('should handle single hash', async () => {
      const root = await computeMerkleRoot(['hash1']);
      expect(root).toBe('hash1');
    });

    it('should compute root for 2 hashes', async () => {
      const root = await computeMerkleRoot(['hash1', 'hash2']);
      expect(root).toBeDefined();
      expect(typeof root).toBe('string');
    });

    it('should compute root for 4 hashes (perfect binary tree)', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const root = await computeMerkleRoot(hashes);

      expect(root).toBeDefined();
      expect(typeof root).toBe('string');
    });

    it('should handle odd number of hashes', async () => {
      const hashes = ['hash1', 'hash2', 'hash3'];
      const root = await computeMerkleRoot(hashes);

      expect(root).toBeDefined();
    });

    it('should compute deterministic root', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];

      const root1 = await computeMerkleRoot(hashes);
      const root2 = await computeMerkleRoot(hashes);

      expect(root1).toBe(root2);
    });

    it('should produce different roots for different orders', async () => {
      const hashes1 = ['hash1', 'hash2', 'hash3', 'hash4'];
      const hashes2 = ['hash4', 'hash3', 'hash2', 'hash1'];

      const root1 = await computeMerkleRoot(hashes1);
      const root2 = await computeMerkleRoot(hashes2);

      expect(root1).not.toBe(root2);
    });

    it('should reject empty array', async () => {
      await expect(computeMerkleRoot([])).rejects.toThrow('Cannot compute Merkle root of empty array');
    });
  });

  describe('generateMerkleProof()', () => {
    it('should generate proof for first element', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const { root, proof } = await generateMerkleProof(hashes, 0);

      expect(root).toBeDefined();
      expect(proof).toBeInstanceOf(Array);
      expect(proof.length).toBeGreaterThan(0);
    });

    it('should generate proof for last element', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const { root, proof } = await generateMerkleProof(hashes, 3);

      expect(root).toBeDefined();
      expect(proof).toBeInstanceOf(Array);
    });

    it('should generate proof for middle element', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const { root, proof } = await generateMerkleProof(hashes, 2);

      expect(root).toBeDefined();
      expect(proof).toBeInstanceOf(Array);
    });

    it('should match computed root', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const expectedRoot = await computeMerkleRoot(hashes);
      const { root } = await generateMerkleProof(hashes, 0);

      expect(root).toBe(expectedRoot);
    });

    it('should reject out-of-bounds index', async () => {
      const hashes = ['hash1', 'hash2'];
      await expect(generateMerkleProof(hashes, 5)).rejects.toThrow('out of bounds');
      await expect(generateMerkleProof(hashes, -1)).rejects.toThrow('out of bounds');
    });
  });

  describe('verifyMerkleProof()', () => {
    it('should verify valid proof', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const { root, proof } = await generateMerkleProof(hashes, 2);

      const isValid = await verifyMerkleProof('hash3', root, proof);
      expect(isValid).toBe(true);
    });

    it('should verify all proofs in tree', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];

      for (let i = 0; i < hashes.length; i++) {
        const { root, proof } = await generateMerkleProof(hashes, i);
        const isValid = await verifyMerkleProof(hashes[i], root, proof);
        expect(isValid).toBe(true);
      }
    });

    it('should reject invalid proof', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const { root, proof } = await generateMerkleProof(hashes, 0);

      // Verify with wrong hash
      const isValid = await verifyMerkleProof('wrong_hash', root, proof);
      expect(isValid).toBe(false);
    });

    it('should reject tampered proof', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const { root, proof } = await generateMerkleProof(hashes, 0);

      // Tamper with proof
      proof[0].hash = 'tampered';

      const isValid = await verifyMerkleProof('hash1', root, proof);
      expect(isValid).toBe(false);
    });
  });

  describe('batchReceipts()', () => {
    it('should batch multiple receipts', async () => {
      const receipts = await Promise.all([
        Receipt.create({
          inputHashes: { ontologyReleases: ['h1'], deltaCapsule: 'd1' },
          decision: 'allow',
          outputHash: 'o1',
          toolchainVersion: { node: '18.19.0', packages: {} },
        }),
        Receipt.create({
          inputHashes: { ontologyReleases: ['h2'], deltaCapsule: 'd2' },
          decision: 'allow',
          outputHash: 'o2',
          toolchainVersion: { node: '18.19.0', packages: {} },
        }),
        Receipt.create({
          inputHashes: { ontologyReleases: ['h3'], deltaCapsule: 'd3' },
          decision: 'deny',
          outputHash: 'o3',
          toolchainVersion: { node: '18.19.0', packages: {} },
        }),
      ]);

      const merkleRoot = await batchReceipts(receipts);

      expect(merkleRoot).toBeDefined();
      expect(typeof merkleRoot).toBe('string');
    });

    it('should produce deterministic batch root', async () => {
      const receipts = await Promise.all([
        Receipt.create({
          inputHashes: { ontologyReleases: ['h1'], deltaCapsule: 'd1' },
          decision: 'allow',
          outputHash: 'o1',
          toolchainVersion: { node: '18.19.0', packages: {} },
          timestamp: new Date('2025-01-01T00:00:00Z'),
        }),
        Receipt.create({
          inputHashes: { ontologyReleases: ['h2'], deltaCapsule: 'd2' },
          decision: 'allow',
          outputHash: 'o2',
          toolchainVersion: { node: '18.19.0', packages: {} },
          timestamp: new Date('2025-01-01T00:00:01Z'),
        }),
      ]);

      const root1 = await batchReceipts(receipts);
      const root2 = await batchReceipts(receipts);

      expect(root1).toBe(root2);
    });
  });
});
