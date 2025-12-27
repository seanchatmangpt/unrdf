/**
 * @fileoverview Tests for Zero-Knowledge Proofs
 */

import { describe, it, expect } from 'vitest';
import {
  generateZKProof,
  verifyZKProof,
  proveReceiptMembership,
  verifyReceiptMembership,
  generateRangeProof,
  verifyRangeProof,
  generateAggregateProof,
  verifyAggregateProof,
} from './zk-proofs.mjs';

describe('ZK Proofs', () => {
  describe('generateZKProof', () => {
    it('should generate valid ZK proof for receipt chain', async () => {
      const hashes = ['hash1', 'hash2', 'hash3'];
      const proof = await generateZKProof(hashes);

      expect(proof).toHaveProperty('commitment');
      expect(proof).toHaveProperty('challenge');
      expect(proof).toHaveProperty('response');
      expect(proof).toHaveProperty('nonce');
      expect(proof.commitment).toMatch(/^[a-f0-9]+$/);
    });

    it('should generate different proofs for same data (nonce randomness)', async () => {
      const hashes = ['hash1', 'hash2'];
      const proof1 = await generateZKProof(hashes);
      const proof2 = await generateZKProof(hashes);

      expect(proof1.commitment).not.toBe(proof2.commitment);
      expect(proof1.nonce).not.toBe(proof2.nonce);
    });

    it('should throw error for empty array', async () => {
      await expect(generateZKProof([])).rejects.toThrow('non-empty array');
    });

    it('should throw error for non-array input', async () => {
      await expect(generateZKProof('not-array')).rejects.toThrow();
    });
  });

  describe('verifyZKProof', () => {
    it('should verify valid proof', async () => {
      const hashes = ['hash1', 'hash2', 'hash3'];
      const proof = await generateZKProof(hashes);
      const isValid = await verifyZKProof(proof);

      expect(isValid).toBe(true);
    });

    it('should reject tampered commitment', async () => {
      const hashes = ['hash1', 'hash2'];
      const proof = await generateZKProof(hashes);
      proof.commitment = 'tampered';

      const isValid = await verifyZKProof(proof);
      expect(isValid).toBe(false);
    });

    it('should reject tampered challenge', async () => {
      const hashes = ['hash1', 'hash2'];
      const proof = await generateZKProof(hashes);
      proof.challenge = 'tampered';

      const isValid = await verifyZKProof(proof);
      expect(isValid).toBe(false);
    });
  });

  describe('Receipt Membership Proofs', () => {
    it('should prove receipt membership', async () => {
      const allHashes = ['hash1', 'hash2', 'hash3', 'hash4'];
      const targetHash = 'hash2';

      const { proof, index } = await proveReceiptMembership(targetHash, allHashes);

      expect(index).toBe(1);
      expect(proof).toHaveProperty('positionCommitment');
      expect(proof).toHaveProperty('challenge');
      expect(proof).toHaveProperty('response');

      const isValid = await verifyReceiptMembership(proof, targetHash);
      expect(isValid).toBe(true);
    });

    it('should throw error if receipt not in chain', async () => {
      const allHashes = ['hash1', 'hash2'];
      await expect(proveReceiptMembership('hash3', allHashes)).rejects.toThrow(
        'not found in chain'
      );
    });

    it('should reject proof with wrong receipt', async () => {
      const allHashes = ['hash1', 'hash2', 'hash3'];
      const { proof } = await proveReceiptMembership('hash2', allHashes);

      const isValid = await verifyReceiptMembership(proof, 'hash1');
      expect(isValid).toBe(false);
    });
  });

  describe('Range Proofs', () => {
    it('should generate valid range proof', async () => {
      const proof = await generateRangeProof(150, 100, 200);

      expect(proof).toHaveProperty('commitment');
      expect(proof).toHaveProperty('challenge');
      expect(proof).toHaveProperty('response');
      expect(proof.min).toBe(100);
      expect(proof.max).toBe(200);

      const isValid = await verifyRangeProof(proof);
      expect(isValid).toBe(true);
    });

    it('should throw error if count outside range', async () => {
      await expect(generateRangeProof(50, 100, 200)).rejects.toThrow('not in range');
    });

    it('should work at range boundaries', async () => {
      const proofMin = await generateRangeProof(100, 100, 200);
      const proofMax = await generateRangeProof(200, 100, 200);

      expect(await verifyRangeProof(proofMin)).toBe(true);
      expect(await verifyRangeProof(proofMax)).toBe(true);
    });

    it('should reject tampered range proof', async () => {
      const proof = await generateRangeProof(150, 100, 200);
      proof.challenge = 'tampered';

      const isValid = await verifyRangeProof(proof);
      expect(isValid).toBe(false);
    });
  });

  describe('Aggregate Proofs', () => {
    it('should generate aggregate proof for multiple chains', async () => {
      const chains = [
        ['chain1_hash1', 'chain1_hash2'],
        ['chain2_hash1', 'chain2_hash2', 'chain2_hash3'],
        ['chain3_hash1'],
      ];

      const proof = await generateAggregateProof(chains);

      expect(proof).toHaveProperty('commitment');
      expect(proof).toHaveProperty('challenge');
      expect(proof).toHaveProperty('response');
      expect(proof.chainCount).toBe(3);

      const isValid = await verifyAggregateProof(proof);
      expect(isValid).toBe(true);
    });

    it('should throw error for empty chains', async () => {
      await expect(generateAggregateProof([])).rejects.toThrow('non-empty array');
    });

    it('should verify single chain aggregate', async () => {
      const chains = [['hash1', 'hash2']];
      const proof = await generateAggregateProof(chains);

      const isValid = await verifyAggregateProof(proof);
      expect(isValid).toBe(true);
    });

    it('should reject tampered aggregate proof', async () => {
      const chains = [['hash1'], ['hash2']];
      const proof = await generateAggregateProof(chains);
      proof.chainCount = 999;

      const isValid = await verifyAggregateProof(proof);
      expect(isValid).toBe(false);
    });
  });

  describe('Integration Tests', () => {
    it('should prove and verify complex receipt chain', async () => {
      // Generate chain
      const chain = Array.from({ length: 100 }, (_, i) => `receipt_${i}`);

      // ZK proof
      const proof = await generateZKProof(chain);
      expect(await verifyZKProof(proof)).toBe(true);

      // Membership proof
      const { proof: memberProof } = await proveReceiptMembership('receipt_42', chain);
      expect(await verifyReceiptMembership(memberProof, 'receipt_42')).toBe(true);

      // Range proof
      const rangeProof = await generateRangeProof(chain.length, 50, 150);
      expect(await verifyRangeProof(rangeProof)).toBe(true);
    });

    it('should handle edge case: single receipt', async () => {
      const proof = await generateZKProof(['single_hash']);
      expect(await verifyZKProof(proof)).toBe(true);
    });
  });
});
