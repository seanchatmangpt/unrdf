/**
 * @fileoverview Tests for Timestamp
 */

import { describe, it, expect } from 'vitest';
import {
  generateTimestamp,
  verifyTimestamp,
  batchTimestamp,
  verifyBatchTimestamp,
  getTimestampAge,
  getTimestampAgeHuman,
} from './timestamp.mjs';

describe('Timestamp', () => {
  describe('generateTimestamp', () => {
    it('should generate local timestamp', async () => {
      const receiptHash = 'test_hash_123';
      const proof = await generateTimestamp(receiptHash, { method: 'local' });

      expect(proof.receiptHash).toBe(receiptHash);
      expect(proof.authority).toBe('local');
      expect(proof.method).toBe('local');
      expect(proof.timestamp).toMatch(/^\d{4}-\d{2}-\d{2}T/);
      expect(proof.anchorHash).toMatch(/^[a-f0-9]+$/);
    });

    it('should generate TSA timestamp', async () => {
      const receiptHash = 'test_hash_456';
      const proof = await generateTimestamp(receiptHash, {
        method: 'tsa',
        authority: 'https://tsa.example.com',
      });

      expect(proof.method).toBe('tsa');
      expect(proof.authority).toBe('https://tsa.example.com');
      expect(proof.tsaResponse).toBeDefined();
      expect(proof.tsaResponse.status).toBe('granted');
    });

    it('should generate blockchain timestamp', async () => {
      const receiptHash = 'test_hash_789';
      const proof = await generateTimestamp(receiptHash, {
        method: 'blockchain',
        authority: 'bitcoin-testnet',
      });

      expect(proof.method).toBe('blockchain');
      expect(proof.authority).toBe('bitcoin-testnet');
      expect(proof.blockHeight).toBeGreaterThan(0);
      expect(proof.transaction).toBeDefined();
      expect(proof.transaction.confirmations).toBe(6);
    });

    it('should default to local method', async () => {
      const proof = await generateTimestamp('hash');
      expect(proof.method).toBe('local');
    });

    it('should throw error for unknown method', async () => {
      await expect(
        generateTimestamp('hash', { method: 'unknown' })
      ).rejects.toThrow('Unknown timestamp method');
    });
  });

  describe('verifyTimestamp', () => {
    it('should verify valid local timestamp', async () => {
      const proof = await generateTimestamp('hash123', { method: 'local' });
      const result = await verifyTimestamp(proof);

      expect(result.valid).toBe(true);
    });

    it('should verify valid TSA timestamp', async () => {
      const proof = await generateTimestamp('hash456', {
        method: 'tsa',
        authority: 'tsa.example.com',
      });
      const result = await verifyTimestamp(proof);

      expect(result.valid).toBe(true);
    });

    it('should verify valid blockchain timestamp', async () => {
      const proof = await generateTimestamp('hash789', {
        method: 'blockchain',
        authority: 'ethereum-testnet',
      });
      const result = await verifyTimestamp(proof);

      expect(result.valid).toBe(true);
    });

    it('should reject timestamp with tampered hash', async () => {
      const proof = await generateTimestamp('hash', { method: 'local' });
      proof.anchorHash = 'tampered';

      const result = await verifyTimestamp(proof);
      expect(result.valid).toBe(false);
    });

    it('should reject incomplete proof', async () => {
      const proof = {
        receiptHash: 'hash',
        timestamp: new Date().toISOString(),
        // Missing authority and proof
      };

      const result = await verifyTimestamp(proof);
      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Invalid proof structure');
    });

    it('should check timestamp age', async () => {
      const oldTimestamp = new Date(Date.now() - 2 * 24 * 60 * 60 * 1000); // 2 days ago
      const proof = await generateTimestamp('hash', { method: 'local' });
      proof.timestamp = oldTimestamp.toISOString();
      proof.anchorHash = await import('hash-wasm').then((m) =>
        m.blake3(`hash:${oldTimestamp.toISOString()}:local`)
      );

      const maxAge = 24 * 60 * 60 * 1000; // 1 day
      const result = await verifyTimestamp(proof, { maxAge });

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('too old');
    });

    it('should check minimum confirmations for blockchain', async () => {
      const proof = await generateTimestamp('hash', {
        method: 'blockchain',
        authority: 'bitcoin',
      });
      proof.transaction.confirmations = 3;

      const result = await verifyTimestamp(proof, { minConfirmations: 6 });

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Insufficient confirmations');
    });
  });

  describe('Batch Timestamp', () => {
    it('should batch timestamp multiple receipts', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const result = await batchTimestamp(hashes, { method: 'local' });

      expect(result.merkleRoot).toBeDefined();
      expect(result.timestamp).toBeDefined();
      expect(result.timestamp.receiptHash).toBe(result.merkleRoot);
      expect(result.proofs.size).toBe(4);
    });

    it('should verify batched timestamp', async () => {
      const hashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const batch = await batchTimestamp(hashes, { method: 'blockchain' });

      // Verify second receipt
      const result = await verifyBatchTimestamp(
        hashes[1],
        batch.merkleRoot,
        batch.proofs.get(1),
        1,
        batch.timestamp
      );

      expect(result.valid).toBe(true);
    });

    it('should reject batch timestamp with wrong merkle proof', async () => {
      const hashes = ['hash1', 'hash2', 'hash3'];
      const batch = await batchTimestamp(hashes, { method: 'local' });

      // Use wrong proof
      const result = await verifyBatchTimestamp(
        hashes[0],
        batch.merkleRoot,
        batch.proofs.get(1), // Wrong proof
        0,
        batch.timestamp
      );

      expect(result.valid).toBe(false);
    });

    it('should reject batch with tampered timestamp', async () => {
      const hashes = ['hash1', 'hash2'];
      const batch = await batchTimestamp(hashes, { method: 'local' });

      // Tamper timestamp's anchor hash (which will fail first)
      const originalHash = batch.timestamp.anchorHash;
      batch.timestamp.anchorHash = 'tampered';

      const result = await verifyBatchTimestamp(
        hashes[0],
        batch.merkleRoot,
        batch.proofs.get(0),
        0,
        batch.timestamp
      );

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Anchor hash mismatch');
    });
  });

  describe('Timestamp Age', () => {
    it('should calculate timestamp age', async () => {
      const proof = await generateTimestamp('hash', { method: 'local' });
      const age = getTimestampAge(proof);

      expect(age).toBeGreaterThanOrEqual(0);
      expect(age).toBeLessThan(1000); // Should be < 1 second
    });

    it('should format age in human-readable format', async () => {
      const proof = await generateTimestamp('hash', { method: 'local' });

      // Recent timestamp
      const age1 = getTimestampAgeHuman(proof);
      expect(age1).toMatch(/second/);

      // Old timestamp (mock)
      proof.timestamp = new Date(Date.now() - 2 * 24 * 60 * 60 * 1000).toISOString();
      const age2 = getTimestampAgeHuman(proof);
      expect(age2).toBe('2 days');

      // Hours
      proof.timestamp = new Date(Date.now() - 3 * 60 * 60 * 1000).toISOString();
      const age3 = getTimestampAgeHuman(proof);
      expect(age3).toBe('3 hours');
    });
  });

  describe('Integration Tests', () => {
    it('should create full timestamp proof lifecycle', async () => {
      const receiptHash = 'receipt_xyz_123';

      // 1. Generate timestamp
      const proof = await generateTimestamp(receiptHash, {
        method: 'blockchain',
        authority: 'bitcoin-testnet',
      });

      // 2. Verify immediately
      const result1 = await verifyTimestamp(proof);
      expect(result1.valid).toBe(true);

      // 3. Check age
      const age = getTimestampAge(proof);
      expect(age).toBeLessThan(1000);

      // 4. Verify with constraints
      const result2 = await verifyTimestamp(proof, {
        maxAge: 10000,
        minConfirmations: 6,
      });
      expect(result2.valid).toBe(true);
    });

    it('should handle all three timestamp methods', async () => {
      const hash = 'test_hash';

      const localProof = await generateTimestamp(hash, { method: 'local' });
      const tsaProof = await generateTimestamp(hash, { method: 'tsa', authority: 'tsa.com' });
      const blockchainProof = await generateTimestamp(hash, {
        method: 'blockchain',
        authority: 'ethereum',
      });

      expect((await verifyTimestamp(localProof)).valid).toBe(true);
      expect((await verifyTimestamp(tsaProof)).valid).toBe(true);
      expect((await verifyTimestamp(blockchainProof)).valid).toBe(true);
    });

    it('should batch timestamp large set of receipts', async () => {
      const hashes = Array.from({ length: 100 }, (_, i) => `receipt_${i}`);
      const batch = await batchTimestamp(hashes, { method: 'local' });

      // Verify random receipts
      const indicesToVerify = [0, 25, 50, 75, 99];

      for (const idx of indicesToVerify) {
        const result = await verifyBatchTimestamp(
          hashes[idx],
          batch.merkleRoot,
          batch.proofs.get(idx),
          idx,
          batch.timestamp
        );
        expect(result.valid).toBe(true);
      }
    });
  });
});
