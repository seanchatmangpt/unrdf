/**
 * KGC Receipts - Batch Receipt Generator Tests
 * Tests receipt generation and verification
 */

import { describe, it, expect } from 'vitest';
import {
  generateBatchReceipt,
  verifyBatchReceipt,
  serializeReceipt,
  deserializeReceipt,
  batchMultipleOperations,
} from '../src/batch-receipt-generator.mjs';

describe('Batch Receipt Generator', () => {
  const sampleOperations = [
    {
      type: 'add',
      subject: 'http://example.com/subject1',
      predicate: 'http://example.com/predicate1',
      object: 'value1',
    },
    {
      type: 'delete',
      subject: 'http://example.com/subject2',
      predicate: 'http://example.com/predicate2',
      object: 'value2',
    },
  ];

  describe('generateBatchReceipt', () => {
    it('generates valid receipt with Q* format', async () => {
      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'morphism',
      });

      expect(receipt.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(receipt.Q_RDF).toMatch(/^http:\/\/kgc\.io\/receipts\/[a-f0-9]{16}$/);
      expect(receipt.Q_PROV).toHaveProperty('timestamp');
      expect(receipt.Q_PROV).toHaveProperty('batchSize');
      expect(receipt.Q_PROV).toHaveProperty('contentHash');
    });

    it('includes batch metadata in Q_PROV', async () => {
      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test-operation',
      });

      expect(receipt.Q_PROV.batchSize).toBe(2);
      expect(receipt.Q_PROV.operationType).toBe('test-operation');
      expect(receipt.Q_PROV.universeID).toBe('Q*_0123456789abcdef');
      expect(typeof receipt.Q_PROV.timestamp).toBe('bigint');
    });

    it('computes content hash', async () => {
      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
      });

      expect(receipt.Q_PROV.contentHash).toMatch(/^[a-f0-9]{64}$/);
    });

    it('includes Merkle root when provided', async () => {
      const merkleRoot = 'a'.repeat(64);

      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
        merkleRoot,
      });

      expect(receipt.Q_PROV.merkleRoot).toBe(merkleRoot);
    });

    it('validates universe ID format', async () => {
      await expect(
        generateBatchReceipt({
          universeID: 'invalid-id',
          operations: sampleOperations,
          operationType: 'test',
        })
      ).rejects.toThrow(/universeID must be Q\* identifier/);
    });

    it('rejects empty operations array', async () => {
      await expect(
        generateBatchReceipt({
          universeID: 'Q*_0123456789abcdef',
          operations: [],
          operationType: 'test',
        })
      ).rejects.toThrow(/non-empty array/);
    });
  });

  describe('verifyBatchReceipt', () => {
    it('verifies valid receipt', async () => {
      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
      });

      const result = await verifyBatchReceipt(receipt, sampleOperations);

      expect(result.valid).toBe(true);
      expect(result.receiptID).toBe(receipt.Q_ID);
      expect(result.contentHash).toBe(receipt.Q_PROV.contentHash);
    });

    it('detects content hash mismatch', async () => {
      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
      });

      const tamperedOps = [
        { ...sampleOperations[0], object: 'tampered' },
      ];

      const result = await verifyBatchReceipt(receipt, tamperedOps);

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Batch size mismatch');
    });

    it('detects batch size mismatch', async () => {
      const receipt = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
      });

      const fewerOps = [sampleOperations[0]];

      const result = await verifyBatchReceipt(receipt, fewerOps);

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('Batch size mismatch');
    });
  });

  describe('serializeReceipt / deserializeReceipt', () => {
    it('serializes and deserializes receipt', async () => {
      const original = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
      });

      const json = serializeReceipt(original);
      const deserialized = deserializeReceipt(json);

      expect(deserialized.Q_ID).toBe(original.Q_ID);
      expect(deserialized.Q_PROV.batchSize).toBe(original.Q_PROV.batchSize);
      expect(deserialized.Q_PROV.contentHash).toBe(original.Q_PROV.contentHash);
    });

    it('preserves BigInt timestamp', async () => {
      const original = await generateBatchReceipt({
        universeID: 'Q*_0123456789abcdef',
        operations: sampleOperations,
        operationType: 'test',
      });

      const json = serializeReceipt(original);
      const deserialized = deserializeReceipt(json);

      expect(typeof deserialized.Q_PROV.timestamp).toBe('bigint');
    });
  });

  describe('batchMultipleOperations', () => {
    it('generates receipts for multiple operation groups', async () => {
      const groups = [
        {
          universeID: 'Q*_abc0123456789ab',
          operations: sampleOperations,
          operationType: 'morphism',
        },
        {
          universeID: 'Q*_def0123456789ab',
          operations: [sampleOperations[0]],
          operationType: 'update',
        },
      ];

      const receipts = await batchMultipleOperations(groups);

      expect(receipts.length).toBe(2);
      expect(receipts[0].Q_PROV.universeID).toBe('Q*_abc0123456789ab');
      expect(receipts[1].Q_PROV.universeID).toBe('Q*_def0123456789ab');
    });
  });
});
