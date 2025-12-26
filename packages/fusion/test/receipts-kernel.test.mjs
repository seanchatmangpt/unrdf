/**
 * Receipts Kernel Tests
 * Tests unified receipt system with creation, verification, and chaining
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createReceipt,
  verifyReceipt,
  chainReceipts,
  merkleBatch,
  receiptFromFreeze,
  receiptFromAnchor,
  receiptFromHook,
} from '../src/receipts-kernel.mjs';

describe('Receipts Kernel', () => {
  describe('createReceipt', () => {
    it('should create a receipt with required fields', async () => {
      const receipt = await createReceipt('test-event', { value: 42 });

      expect(receipt).toHaveProperty('id');
      expect(receipt).toHaveProperty('hash');
      expect(receipt).toHaveProperty('timestamp');
      expect(receipt).toHaveProperty('timestamp_iso');
      expect(receipt).toHaveProperty('eventType', 'test-event');
      expect(receipt).toHaveProperty('payload');
      expect(receipt.payload).toEqual({ value: 42 });
      expect(receipt).toHaveProperty('receiptType', 'kgc'); // Default
    });

    it('should create deterministic receipts with same input', async () => {
      const oldEnv = process.env.DETERMINISTIC;
      process.env.DETERMINISTIC = '1';

      try {
        const r1 = await createReceipt('test', { value: 42 });
        const r2 = await createReceipt('test', { value: 42 });

        // Hashes should match in deterministic mode
        expect(r1.hash).toBe(r2.hash);
        expect(r1.eventType).toBe(r2.eventType);
      } finally {
        process.env.DETERMINISTIC = oldEnv;
      }
    });

    it('should support blockchain receipt type with SHA256', async () => {
      const receipt = await createReceipt('anchor', { txHash: '0xabc' }, {
        receiptType: 'blockchain',
      });

      expect(receipt.receiptType).toBe('blockchain');
      expect(receipt.hash).toBeTruthy();
      // SHA256 produces 64 char hex string
      expect(receipt.hash.length).toBe(64);
    });

    it('should support KGC receipt type with BLAKE3', async () => {
      const receipt = await createReceipt('snapshot', { universe_hash: 'abc123' }, {
        receiptType: 'kgc',
      });

      expect(receipt.receiptType).toBe('kgc');
      expect(receipt.hash).toBeTruthy();
      // BLAKE3 also produces 64 char hex string
      expect(receipt.hash.length).toBe(64);
    });

    it('should support hook receipt type', async () => {
      const receipt = await createReceipt('validation', { valid: true }, {
        receiptType: 'hook',
      });

      expect(receipt.receiptType).toBe('hook');
      expect(receipt.hash).toBeTruthy();
    });

    it('should accept optional chain reference', async () => {
      const prevHash = 'abc123def456';
      const receipt = await createReceipt('chained-event', { data: 'test' }, {
        chain: prevHash,
      });

      expect(receipt.chain).toBe(prevHash);
    });

    it('should accept optional proof', async () => {
      const proof = { merkleProof: { leaf: '0xabc', proof: [], root: '0xdef' } };
      const receipt = await createReceipt('verified-event', { data: 'test' }, {
        proof,
      });

      expect(receipt.proof).toEqual(proof);
    });

    it('should reject invalid input', async () => {
      await expect(createReceipt('', { value: 42 }))
        .rejects.toThrow('eventType must be a non-empty string');

      await expect(createReceipt('test', null))
        .rejects.toThrow('payload is required');

      await expect(createReceipt('test', { value: 42 }, { timestamp: 123 }))
        .rejects.toThrow('timestamp must be a BigInt');
    });
  });

  describe('verifyReceipt', () => {
    it('should verify a valid receipt', async () => {
      const receipt = await createReceipt('test', { value: 42 });
      const result = await verifyReceipt(receipt);

      expect(result.valid).toBe(true);
      expect(result.receiptId).toBe(receipt.id);
      expect(result.details).toBeTruthy();
    });

    it('should detect tampered payload', async () => {
      const receipt = await createReceipt('test', { value: 42 });

      // Tamper with payload
      const tamperedReceipt = { ...receipt, payload: { value: 99 } };
      const result = await verifyReceipt(tamperedReceipt);

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Hash mismatch');
    });

    it('should detect tampered hash', async () => {
      const receipt = await createReceipt('test', { value: 42 });

      // Tamper with hash
      const tamperedReceipt = { ...receipt, hash: 'fake-hash-000000' };
      const result = await verifyReceipt(tamperedReceipt);

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Hash mismatch');
    });

    it('should reject invalid receipt structure', async () => {
      const result1 = await verifyReceipt(null);
      expect(result1.valid).toBe(false);
      expect(result1.reason).toContain('must be an object');

      const result2 = await verifyReceipt({ id: 'test' });
      expect(result2.valid).toBe(false);
      expect(result2.reason).toContain('Missing required field');
    });

    it('should verify receipt with chain reference', async () => {
      const receipt = await createReceipt('test', { value: 42 }, {
        chain: 'abc123def456',
      });

      const result = await verifyReceipt(receipt);
      expect(result.valid).toBe(true);
      expect(result.details.hasChain).toBe(true);
    });

    it('should verify receipt with proof', async () => {
      const proof = { merkleProof: { leaf: '0xabc', proof: [], root: '0xdef' } };
      const receipt = await createReceipt('test', { value: 42 }, { proof });

      const result = await verifyReceipt(receipt);
      expect(result.valid).toBe(true);
      expect(result.details.hasProof).toBe(true);
    });
  });

  describe('chainReceipts', () => {
    it('should chain multiple receipts into Merkle DAG', async () => {
      const receipts = await Promise.all([
        createReceipt('event-1', { value: 1 }),
        createReceipt('event-2', { value: 2 }),
        createReceipt('event-3', { value: 3 }),
      ]);

      const result = await chainReceipts(receipts);

      expect(result.valid).toBe(true);
      expect(result.root).toBeTruthy();
      expect(result.root.startsWith('0x')).toBe(true);
      expect(result.proofs).toHaveLength(3);
      expect(result.count).toBe(3);

      // Verify each proof has required fields
      result.proofs.forEach((proof, index) => {
        expect(proof.receiptId).toBe(receipts[index].id);
        expect(proof.receiptHash).toBe(receipts[index].hash);
        expect(proof.merkleProof).toBeTruthy();
        expect(proof.index).toBe(index);
      });
    });

    it('should detect invalid receipts in chain', async () => {
      const validReceipt = await createReceipt('valid', { value: 1 });
      const invalidReceipt = { id: 'fake', hash: 'fake', timestamp: '123', eventType: 'fake', payload: {}, receiptType: 'kgc' };

      const result = await chainReceipts([validReceipt, invalidReceipt]);

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('failed verification');
    });

    it('should reject empty receipt array', async () => {
      await expect(chainReceipts([]))
        .rejects.toThrow('must be a non-empty array');
    });

    it('should reject non-array input', async () => {
      await expect(chainReceipts(null))
        .rejects.toThrow('must be a non-empty array');
    });
  });

  describe('merkleBatch', () => {
    it('should batch receipts into Merkle tree', async () => {
      const receipts = await Promise.all([
        createReceipt('batch-1', { value: 1 }),
        createReceipt('batch-2', { value: 2 }),
        createReceipt('batch-3', { value: 3 }),
        createReceipt('batch-4', { value: 4 }),
      ]);

      const result = await merkleBatch(receipts);

      expect(result.root).toBeTruthy();
      expect(result.root.startsWith('0x')).toBe(true);
      expect(result.tree.leafCount).toBe(4);
      expect(result.tree.depth).toBeGreaterThan(0);
      expect(result.tree.leaves).toHaveLength(4);
      expect(result.proofs).toHaveLength(4);

      // Verify proofs match receipts
      result.proofs.forEach((proof, index) => {
        expect(proof.receiptId).toBe(receipts[index].id);
        expect(proof.merkleProof).toBeTruthy();
      });
    });

    it('should handle large batches efficiently', async () => {
      const receipts = await Promise.all(
        Array.from({ length: 100 }, (_, i) =>
          createReceipt(`batch-${i}`, { value: i })
        )
      );

      const result = await merkleBatch(receipts);

      expect(result.root).toBeTruthy();
      expect(result.tree.leafCount).toBe(100);
      expect(result.proofs).toHaveLength(100);
    });

    it('should reject empty array', async () => {
      await expect(merkleBatch([]))
        .rejects.toThrow('must be a non-empty array');
    });
  });

  describe('Utility Functions', () => {
    it('should create receipt from freeze result', async () => {
      const freezeResult = {
        id: 'freeze-123',
        t_ns: '1000000000000000000',
        timestamp_iso: '2025-01-01T00:00:00.000Z',
        universe_hash: 'abc123def456',
        git_ref: 'refs/snapshots/abc123',
        event_count: 42,
      };

      const receipt = await receiptFromFreeze(freezeResult);

      expect(receipt.eventType).toBe('snapshot');
      expect(receipt.receiptType).toBe('kgc');
      expect(receipt.payload.universe_hash).toBe(freezeResult.universe_hash);
      expect(receipt.payload.git_ref).toBe(freezeResult.git_ref);
      expect(receipt.payload.event_count).toBe(42);
    });

    it('should create receipt from anchor result', async () => {
      const anchorResult = {
        txHash: '0xabc123',
        blockNumber: 12345,
        gasUsed: 50000n,
        gasPrice: 20000000000n,
        receiptHash: 'def456',
        timestamp: 1609459200, // Unix timestamp in seconds
      };

      const receipt = await receiptFromAnchor(anchorResult);

      expect(receipt.eventType).toBe('anchor');
      expect(receipt.receiptType).toBe('blockchain');
      expect(receipt.payload.txHash).toBe(anchorResult.txHash);
      expect(receipt.payload.blockNumber).toBe(anchorResult.blockNumber);
    });

    it('should create receipt from hook result', async () => {
      const hookResult = {
        valid: true,
        errors: [],
        transformations: ['normalize-namespace'],
      };

      const receipt = await receiptFromHook('validation-hook-1', hookResult);

      expect(receipt.eventType).toBe('hook-execution');
      expect(receipt.receiptType).toBe('hook');
      expect(receipt.payload.hookId).toBe('validation-hook-1');
      expect(receipt.payload.valid).toBe(true);
    });
  });

  describe('Deterministic Mode', () => {
    beforeEach(() => {
      // Reset deterministic counter between tests
      delete process.env.DETERMINISTIC;
    });

    it('should produce identical hashes in deterministic mode', async () => {
      process.env.DETERMINISTIC = '1';

      const r1 = await createReceipt('test', { value: 42 });
      const r2 = await createReceipt('test', { value: 42 });

      expect(r1.hash).toBe(r2.hash);
      expect(r1.id).toBe(r2.id); // Same timestamp = same ID
    });

    it('should produce different hashes with different payloads', async () => {
      process.env.DETERMINISTIC = '1';

      const r1 = await createReceipt('test', { value: 42 });
      const r2 = await createReceipt('test', { value: 99 });

      expect(r1.hash).not.toBe(r2.hash);
    });

    it('should produce monotonic timestamps', async () => {
      process.env.DETERMINISTIC = '1';

      const r1 = await createReceipt('test', { value: 1 });
      const r2 = await createReceipt('test', { value: 2 });
      const r3 = await createReceipt('test', { value: 3 });

      const t1 = BigInt(r1.timestamp);
      const t2 = BigInt(r2.timestamp);
      const t3 = BigInt(r3.timestamp);

      expect(t2 > t1).toBe(true);
      expect(t3 > t2).toBe(true);
    });
  });

  describe('Integration Tests', () => {
    it('should create, verify, and chain receipts end-to-end', async () => {
      // Create receipts
      const receipts = await Promise.all([
        createReceipt('integration-1', { step: 1 }),
        createReceipt('integration-2', { step: 2 }),
        createReceipt('integration-3', { step: 3 }),
      ]);

      // Verify each receipt
      for (const receipt of receipts) {
        const result = await verifyReceipt(receipt);
        expect(result.valid).toBe(true);
      }

      // Chain receipts
      const chain = await chainReceipts(receipts);
      expect(chain.valid).toBe(true);
      expect(chain.root).toBeTruthy();

      // Batch receipts
      const batch = await merkleBatch(receipts);
      expect(batch.root).toBe(chain.root); // Should match chain result
      expect(batch.proofs).toHaveLength(3);
    });

    it('should handle mixed receipt types', async () => {
      const receipts = await Promise.all([
        createReceipt('kgc-event', { data: 1 }, { receiptType: 'kgc' }),
        createReceipt('blockchain-event', { data: 2 }, { receiptType: 'blockchain' }),
        createReceipt('hook-event', { data: 3 }, { receiptType: 'hook' }),
      ]);

      // Verify all types
      for (const receipt of receipts) {
        const result = await verifyReceipt(receipt);
        expect(result.valid).toBe(true);
      }

      // Chain mixed types
      const chain = await chainReceipts(receipts);
      expect(chain.valid).toBe(true);
      expect(chain.proofs).toHaveLength(3);
    });
  });
});
