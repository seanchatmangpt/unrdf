/**
 * KGC Documentation Receipts Tests
 *
 * Tests receipt infrastructure for dynamic documentation:
 * - Receipt issuance and verification
 * - Chain construction and validation
 * - Merkle batching and proof verification
 * - Manifest generation and aggregation
 * - Deterministic mode
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdir, rm, readFile, readdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createHash } from 'node:crypto';
import {
  issueReceipt,
  verifyReceipt,
  chainReceipts,
  merkleBatch,
  verifyMerkleProof,
  manifestReceipts,
  KGCDocReceiptSchema,
  VerificationResultSchema,
  ReceiptChainSchema,
  MerkleTreeSchema,
  MerkleProofSchema,
  ManifestSchema,
} from '../src/kgc-docs-receipts.mjs';

describe('KGC Documentation Receipts', () => {
  let testDir;

  beforeEach(async () => {
    // Create temporary test directory
    testDir = join(tmpdir(), `test-receipts-${Date.now()}`);
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    // Clean up test directory
    try {
      await rm(testDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('issueReceipt', () => {
    it('should issue an admit receipt for query block', async () => {
      const block = { id: 'query-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const result = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
        policy_id: 'test-policy',
      });

      expect(result.receipt).toBeDefined();
      expect(result.path).toBeDefined();
      expect(result.receipt.id).toMatch(/^receipt-/);
      expect(result.receipt.decision).toBe('allow');
      expect(result.receipt.block_type).toBe('query');
      expect(result.receipt.input_hash).toBe(inputHash);
      expect(result.receipt.output_hash).toBe(outputHash);
      expect(result.receipt.receipt_hash).toBeTruthy();
      expect(result.path).toContain('admits');

      // Verify file was written
      const fileContent = await readFile(result.path, 'utf-8');
      const parsed = JSON.parse(fileContent);
      expect(parsed).toEqual(result.receipt);
    });

    it('should issue a denial receipt for proof block', async () => {
      const block = { id: 'proof-1', type: 'proof' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = ''; // Empty for denials

      const result = await issueReceipt(block, inputHash, outputHash, 'deny', {
        receiptsDir: testDir,
        reason: 'Policy violation: forbidden predicate',
      });

      expect(result.receipt.decision).toBe('deny');
      expect(result.receipt.block_type).toBe('proof');
      expect(result.receipt.output_hash).toBe('');
      expect(result.receipt.reason).toBe('Policy violation: forbidden predicate');
      expect(result.path).toContain('denials');
    });

    it('should support all block types', async () => {
      const blockTypes = ['query', 'proof', 'extract', 'render'];

      for (const type of blockTypes) {
        const block = { id: `${type}-1`, type };
        const inputHash = createHash('sha256').update(`input-${type}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${type}`).digest('hex');

        const result = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        expect(result.receipt.block_type).toBe(type);
      }
    });

    it('should validate receipt with Zod schema', async () => {
      const block = { id: 'validate-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const result = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      // Should not throw
      expect(() => KGCDocReceiptSchema.parse(result.receipt)).not.toThrow();
    });

    it('should compute deterministic receipt hash', async () => {
      const block = { id: 'hash-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const result = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      // Recompute hash manually
      const { receipt_hash, ...receiptData } = result.receipt;
      const canonical = JSON.stringify(
        Object.keys(receiptData).sort().reduce((obj, key) => {
          obj[key] = receiptData[key];
          return obj;
        }, {}),
        null,
        2
      );
      const recomputed = createHash('sha256').update(canonical).digest('hex');

      expect(recomputed).toBe(receipt_hash);
    });

    it('should reject invalid input', async () => {
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      // Missing block ID
      await expect(issueReceipt({ type: 'query' }, inputHash, outputHash, 'allow'))
        .rejects.toThrow('block must have id and type');

      // Missing block type
      await expect(issueReceipt({ id: 'test' }, inputHash, outputHash, 'allow'))
        .rejects.toThrow('block must have id and type');

      // Invalid decision
      await expect(issueReceipt({ id: 'test', type: 'query' }, inputHash, outputHash, 'invalid'))
        .rejects.toThrow('decision must be "allow" or "deny"');

      // Missing inputHash
      await expect(issueReceipt({ id: 'test', type: 'query' }, '', outputHash, 'allow'))
        .rejects.toThrow('inputHash must be a non-empty string');
    });

    it('should support deterministic mode', async () => {
      const oldEnv = process.env.DETERMINISTIC;
      process.env.DETERMINISTIC = '1';

      try {
        const block = { id: 'det-1', type: 'query' };
        const inputHash = createHash('sha256').update('input').digest('hex');
        const outputHash = createHash('sha256').update('output').digest('hex');

        const r1 = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        const r2 = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        // IDs should be sequential
        expect(r1.receipt.id).toMatch(/^receipt-\d{12}$/);
        expect(r2.receipt.id).toMatch(/^receipt-\d{12}$/);

        // Timestamps should be fixed
        expect(r1.receipt.timestamp).toBe(r2.receipt.timestamp);
        expect(r1.receipt.timestamp).toBe('2025-01-01T00:00:00.000Z');
      } finally {
        process.env.DETERMINISTIC = oldEnv;
      }
    });
  });

  describe('verifyReceipt', () => {
    it('should verify a valid receipt', async () => {
      const block = { id: 'verify-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      const result = await verifyReceipt(receipt);

      expect(result.valid).toBe(true);
      expect(result.errors).toEqual([]);
      expect(result.receiptId).toBe(receipt.id);
      expect(result.details).toBeDefined();
    });

    it('should detect tampered receipt hash', async () => {
      const block = { id: 'tamper-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      // Tamper with receipt data
      const tampered = { ...receipt, output_hash: 'fake-hash' };

      const result = await verifyReceipt(tampered);

      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors[0]).toContain('Hash mismatch');
    });

    it('should detect tampered receipt_hash field', async () => {
      const block = { id: 'tamper-2', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      // Tamper with hash field
      const tampered = { ...receipt, receipt_hash: 'fake-hash-000000' };

      const result = await verifyReceipt(tampered);

      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors[0]).toContain('Hash mismatch');
    });

    it('should verify expected output hash', async () => {
      const block = { id: 'output-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      // Verify with correct expected hash
      const result1 = await verifyReceipt(receipt, outputHash);
      expect(result1.valid).toBe(true);

      // Verify with incorrect expected hash
      const wrongHash = createHash('sha256').update('wrong').digest('hex');
      const result2 = await verifyReceipt(receipt, wrongHash);
      expect(result2.valid).toBe(false);
      expect(result2.errors.length).toBeGreaterThan(0);
      expect(result2.errors[0]).toContain('Output hash mismatch');
    });

    it('should reject invalid receipt structure', async () => {
      const result1 = await verifyReceipt(null);
      expect(result1.valid).toBe(false);
      expect(result1.errors).toContain('Receipt must be an object');

      const result2 = await verifyReceipt({ id: 'test' });
      expect(result2.valid).toBe(false);
      expect(result2.errors[0]).toContain('Schema validation failed');
    });

    it('should validate Zod schema', async () => {
      const block = { id: 'schema-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      const result = await verifyReceipt(receipt);

      expect(result.valid).toBe(true);
      expect(() => VerificationResultSchema.parse(result)).not.toThrow();
    });
  });

  describe('chainReceipts', () => {
    it('should chain receipts in chronological order', async () => {
      const receipts = [];

      for (let i = 0; i < 3; i++) {
        const block = { id: `chain-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);

        // Add small delay to ensure different timestamps (unless deterministic)
        if (process.env.DETERMINISTIC !== '1') {
          await new Promise(resolve => setTimeout(resolve, 10));
        }
      }

      const chain = await chainReceipts(receipts);

      expect(chain.valid).toBe(true);
      expect(chain.length).toBe(3);
      expect(chain.receipts).toHaveLength(3);
      expect(Object.keys(chain.parents)).toHaveLength(2); // First receipt has no parent

      // Verify parent pointers
      expect(chain.parents[chain.receipts[1].id]).toBe(chain.receipts[0].receipt_hash);
      expect(chain.parents[chain.receipts[2].id]).toBe(chain.receipts[1].receipt_hash);

      // Validate schema
      expect(() => ReceiptChainSchema.parse(chain)).not.toThrow();
    });

    it('should sort receipts by timestamp', async () => {
      const oldEnv = process.env.DETERMINISTIC;
      delete process.env.DETERMINISTIC; // Ensure real timestamps

      try {
        const receipts = [];

        // Create receipts with delays
        for (let i = 0; i < 3; i++) {
          const block = { id: `sort-${i}`, type: 'query' };
          const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
          const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

          const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
            receiptsDir: testDir,
          });

          receipts.push(receipt);
          await new Promise(resolve => setTimeout(resolve, 10));
        }

        // Shuffle receipts
        const shuffled = [receipts[2], receipts[0], receipts[1]];

        const chain = await chainReceipts(shuffled);

        // Should be sorted chronologically
        expect(chain.receipts[0].id).toBe(receipts[0].id);
        expect(chain.receipts[1].id).toBe(receipts[1].id);
        expect(chain.receipts[2].id).toBe(receipts[2].id);
      } finally {
        process.env.DETERMINISTIC = oldEnv;
      }
    });

    it('should reject empty array', async () => {
      await expect(chainReceipts([]))
        .rejects.toThrow('receipts must be a non-empty array');
    });

    it('should reject non-array input', async () => {
      await expect(chainReceipts(null))
        .rejects.toThrow('receipts must be a non-empty array');
    });
  });

  describe('merkleBatch', () => {
    it('should build Merkle tree from receipts', async () => {
      const receipts = [];

      for (let i = 0; i < 4; i++) {
        const block = { id: `merkle-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      const batch = await merkleBatch(receipts);

      expect(batch.root).toBeTruthy();
      expect(batch.root.length).toBe(64); // SHA-256 hex
      expect(batch.tree.leafCount).toBe(4);
      expect(batch.tree.leaves).toHaveLength(4);
      expect(batch.tree.depth).toBeGreaterThan(0);
      expect(Object.keys(batch.proofs)).toHaveLength(4);

      // Validate schema
      expect(() => MerkleTreeSchema.parse(batch.tree)).not.toThrow();

      // Verify each proof
      for (const receipt of receipts) {
        const proof = batch.proofs[receipt.id];
        expect(proof).toBeDefined();
        expect(proof.receiptId).toBe(receipt.id);
        expect(proof.receiptHash).toBe(receipt.receipt_hash);
        expect(proof.root).toBe(batch.root);
        expect(() => MerkleProofSchema.parse(proof)).not.toThrow();
      }
    });

    it('should produce deterministic tree with sorted leaves', async () => {
      const receipts = [];

      for (let i = 0; i < 3; i++) {
        const block = { id: `det-merkle-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      // Build tree twice (same receipts, different order)
      const batch1 = await merkleBatch(receipts);
      const shuffled = [receipts[2], receipts[0], receipts[1]];
      const batch2 = await merkleBatch(shuffled);

      // Roots should match (deterministic)
      expect(batch1.root).toBe(batch2.root);
      expect(batch1.tree.leafCount).toBe(batch2.tree.leafCount);
    });

    it('should handle large batches', async () => {
      const receipts = [];

      for (let i = 0; i < 100; i++) {
        const block = { id: `large-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      const batch = await merkleBatch(receipts);

      expect(batch.tree.leafCount).toBe(100);
      expect(Object.keys(batch.proofs)).toHaveLength(100);
      expect(batch.tree.depth).toBeGreaterThanOrEqual(6); // log2(100) ≈ 6.64
    });

    it('should reject empty array', async () => {
      await expect(merkleBatch([]))
        .rejects.toThrow('receipts must be a non-empty array');
    });
  });

  describe('verifyMerkleProof', () => {
    it('should verify valid Merkle proof', async () => {
      const receipts = [];

      for (let i = 0; i < 4; i++) {
        const block = { id: `proof-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      const batch = await merkleBatch(receipts);

      // Verify each proof
      for (const receipt of receipts) {
        const proof = batch.proofs[receipt.id];
        const result = await verifyMerkleProof(
          receipt.id,
          receipt.receipt_hash,
          proof,
          batch.root
        );

        expect(result.valid).toBe(true);
        expect(result.recomputedRoot).toBe(batch.root);
        expect(result.receiptId).toBe(receipt.id);
      }
    });

    it('should detect tampered receipt hash', async () => {
      const receipts = [];

      for (let i = 0; i < 4; i++) {
        const block = { id: `tamper-proof-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      const batch = await merkleBatch(receipts);
      const proof = batch.proofs[receipts[0].id];

      // Tamper with receipt hash
      const tamperedHash = 'fake-hash-000000000000000000000000000000000000000000000000000000000000';

      const result = await verifyMerkleProof(
        receipts[0].id,
        tamperedHash,
        proof,
        batch.root
      );

      expect(result.valid).toBe(false);
      expect(result.recomputedRoot).not.toBe(batch.root);
    });

    it('should detect tampered proof', async () => {
      const receipts = [];

      for (let i = 0; i < 4; i++) {
        const block = { id: `tamper-siblings-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      const batch = await merkleBatch(receipts);
      const proof = batch.proofs[receipts[0].id];

      // Tamper with siblings
      const tamperedProof = {
        ...proof,
        siblings: proof.siblings.map(s => ({
          ...s,
          hash: 'fake-hash',
        })),
      };

      const result = await verifyMerkleProof(
        receipts[0].id,
        receipts[0].receipt_hash,
        tamperedProof,
        batch.root
      );

      expect(result.valid).toBe(false);
    });

    it('should handle edge cases in input', async () => {
      // With empty siblings array, hash should equal root for valid proof
      const proof = {
        receiptId: 'test',
        receiptHash: 'abc123',
        siblings: [],
        root: 'abc123', // Same as hash = valid proof
        index: 0,
      };

      const result1 = await verifyMerkleProof('', 'abc123', proof, 'abc123');
      expect(result1.valid).toBe(true); // Empty receiptId is allowed

      const result2 = await verifyMerkleProof('id', '', proof, 'root');
      expect(result2.valid).toBe(false); // Empty hash not allowed

      const result3 = await verifyMerkleProof('id', 'hash', null, 'root');
      expect(result3.valid).toBe(false); // Null proof not allowed

      const result4 = await verifyMerkleProof('id', 'hash', proof, '');
      expect(result4.valid).toBe(false); // Empty root not allowed
    });
  });

  describe('manifestReceipts', () => {
    it('should generate manifest from receipts directory', async () => {
      const receipts = [];

      // Create 3 admits
      for (let i = 0; i < 3; i++) {
        const block = { id: `admit-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      // Create 2 denials
      for (let i = 0; i < 2; i++) {
        const block = { id: `deny-${i}`, type: 'proof' };
        const inputHash = createHash('sha256').update(`deny-input-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, '', 'deny', {
          receiptsDir: testDir,
          reason: 'Test denial',
        });

        receipts.push(receipt);
      }

      const result = await manifestReceipts(testDir);

      expect(result.manifest).toBeDefined();
      expect(result.path).toBe(join(testDir, 'manifest.json'));
      expect(result.manifest.totalAdmits).toBe(3);
      expect(result.manifest.totalDenials).toBe(2);
      expect(result.manifest.receipts).toHaveLength(5);
      expect(result.manifest.merkleRoot).toBeTruthy();
      expect(result.manifest.manifestHash).toBeTruthy();
      expect(result.manifest.generatedAt).toBeTruthy();

      // Validate schema
      expect(() => ManifestSchema.parse(result.manifest)).not.toThrow();

      // Verify file was written
      const fileContent = await readFile(result.path, 'utf-8');
      const parsed = JSON.parse(fileContent);
      expect(parsed).toEqual(result.manifest);
    });

    it('should sort receipts chronologically in manifest', async () => {
      const oldEnv = process.env.DETERMINISTIC;
      delete process.env.DETERMINISTIC;

      try {
        const receipts = [];

        for (let i = 0; i < 3; i++) {
          const block = { id: `sort-manifest-${i}`, type: 'query' };
          const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
          const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

          const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
            receiptsDir: testDir,
          });

          receipts.push(receipt);
          await new Promise(resolve => setTimeout(resolve, 10));
        }

        const result = await manifestReceipts(testDir);

        // Verify chronological order
        const timestamps = result.manifest.receipts.map(r =>
          new Date(r.timestamp).getTime()
        );

        for (let i = 1; i < timestamps.length; i++) {
          expect(timestamps[i]).toBeGreaterThanOrEqual(timestamps[i - 1]);
        }
      } finally {
        process.env.DETERMINISTIC = oldEnv;
      }
    });

    it('should compute deterministic manifest hash', async () => {
      const block = { id: 'manifest-hash', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      const result = await manifestReceipts(testDir);

      // Recompute hash manually
      const { manifestHash, ...manifestData } = result.manifest;
      const canonical = JSON.stringify(
        Object.keys(manifestData).sort().reduce((obj, key) => {
          obj[key] = manifestData[key];
          return obj;
        }, {}),
        null,
        2
      );
      const recomputed = createHash('sha256').update(canonical).digest('hex');

      expect(recomputed).toBe(manifestHash);
    });

    it('should reject empty receipts directory', async () => {
      await expect(manifestReceipts(testDir))
        .rejects.toThrow('no receipts found');
    });

    it('should handle admits-only directory', async () => {
      const block = { id: 'admits-only', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      const result = await manifestReceipts(testDir);

      expect(result.manifest.totalAdmits).toBe(1);
      expect(result.manifest.totalDenials).toBe(0);
    });

    it('should handle denials-only directory', async () => {
      const block = { id: 'denials-only', type: 'proof' };
      const inputHash = createHash('sha256').update('input').digest('hex');

      await issueReceipt(block, inputHash, '', 'deny', {
        receiptsDir: testDir,
        reason: 'Test',
      });

      const result = await manifestReceipts(testDir);

      expect(result.manifest.totalAdmits).toBe(0);
      expect(result.manifest.totalDenials).toBe(1);
    });
  });

  describe('Integration Tests', () => {
    it('should handle complete workflow: issue -> verify -> chain -> batch -> manifest', async () => {
      const receipts = [];

      // Issue 5 receipts (3 admits, 2 denials)
      for (let i = 0; i < 3; i++) {
        const block = { id: `integration-admit-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      for (let i = 0; i < 2; i++) {
        const block = { id: `integration-deny-${i}`, type: 'proof' };
        const inputHash = createHash('sha256').update(`deny-input-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, '', 'deny', {
          receiptsDir: testDir,
          reason: 'Integration test denial',
        });

        receipts.push(receipt);
      }

      // Verify all receipts
      for (const receipt of receipts) {
        const result = await verifyReceipt(receipt);
        expect(result.valid).toBe(true);
      }

      // Chain receipts
      const chain = await chainReceipts(receipts);
      expect(chain.valid).toBe(true);
      expect(chain.length).toBe(5);

      // Batch into Merkle tree
      const batch = await merkleBatch(receipts);
      expect(batch.tree.leafCount).toBe(5);

      // Verify all Merkle proofs
      for (const receipt of receipts) {
        const proof = batch.proofs[receipt.id];
        const result = await verifyMerkleProof(
          receipt.id,
          receipt.receipt_hash,
          proof,
          batch.root
        );
        expect(result.valid).toBe(true);
      }

      // Generate manifest
      const manifest = await manifestReceipts(testDir);
      expect(manifest.manifest.totalAdmits).toBe(3);
      expect(manifest.manifest.totalDenials).toBe(2);
      expect(manifest.manifest.receipts).toHaveLength(5);
    });

    it('should detect tampering at any stage', async () => {
      const receipts = [];

      for (let i = 0; i < 3; i++) {
        const block = { id: `tamper-integration-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      // Tamper with one receipt's data (not the hash)
      const tampered = { ...receipts[1], output_hash: 'fake-hash' };

      // Verification should fail (hash mismatch)
      const verifyResult = await verifyReceipt(tampered);
      expect(verifyResult.valid).toBe(false);
      expect(verifyResult.errors.length).toBeGreaterThan(0);

      // For Merkle proof test, we need to use the ORIGINAL receipt
      // (tampering would be detected at verification stage, not proof stage)
      const batch = await merkleBatch(receipts);
      const proof = batch.proofs[receipts[1].id];

      // Proof should be valid for original receipt
      const validProofResult = await verifyMerkleProof(
        receipts[1].id,
        receipts[1].receipt_hash,
        proof,
        batch.root
      );
      expect(validProofResult.valid).toBe(true);

      // But if we try to use tampered hash with original proof, it should fail
      const tamperedHash = createHash('sha256').update('fake-data').digest('hex');
      const invalidProofResult = await verifyMerkleProof(
        receipts[1].id,
        tamperedHash,
        proof,
        batch.root
      );
      expect(invalidProofResult.valid).toBe(false);
    });
  });

  describe('Deterministic Mode', () => {
    beforeEach(() => {
      delete process.env.DETERMINISTIC;
    });

    it('should produce deterministic UUIDs', async () => {
      process.env.DETERMINISTIC = '1';

      const block = { id: 'det-uuid-1', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const r1 = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      const r2 = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      expect(r1.receipt.id).toMatch(/^receipt-\d{12}$/);
      expect(r2.receipt.id).toMatch(/^receipt-\d{12}$/);
      expect(r1.receipt.id).not.toBe(r2.receipt.id); // Sequential
    });

    it('should produce fixed timestamps', async () => {
      process.env.DETERMINISTIC = '1';

      const block = { id: 'det-timestamp', type: 'query' };
      const inputHash = createHash('sha256').update('input').digest('hex');
      const outputHash = createHash('sha256').update('output').digest('hex');

      const r1 = await issueReceipt(block, inputHash, outputHash, 'allow', {
        receiptsDir: testDir,
      });

      expect(r1.receipt.timestamp).toBe('2025-01-01T00:00:00.000Z');
    });

    it('should produce deterministic Merkle trees', async () => {
      process.env.DETERMINISTIC = '1';

      const receipts = [];

      for (let i = 0; i < 4; i++) {
        const block = { id: `det-merkle-tree-${i}`, type: 'query' };
        const inputHash = createHash('sha256').update(`input-${i}`).digest('hex');
        const outputHash = createHash('sha256').update(`output-${i}`).digest('hex');

        const { receipt } = await issueReceipt(block, inputHash, outputHash, 'allow', {
          receiptsDir: testDir,
        });

        receipts.push(receipt);
      }

      const batch1 = await merkleBatch(receipts);
      const batch2 = await merkleBatch(receipts);

      expect(batch1.root).toBe(batch2.root);
    });
  });
});
