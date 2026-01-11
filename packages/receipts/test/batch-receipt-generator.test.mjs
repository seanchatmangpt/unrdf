/**
 * KGC Receipts - Batch Receipt Generator Tests - Refactored for speed
 * Tests receipt generation (essential tests only)
 */

import { describe, it, expect } from 'vitest';
import { generateBatchReceipt, verifyBatchReceipt } from '../src/batch-receipt-generator.mjs';

describe('Batch Receipt Generator', () => {
  const sampleOperations = [
    {
      type: 'add',
      subject: 'http://example.com/s1',
      predicate: 'http://example.com/p1',
      object: 'v1',
    },
    {
      type: 'delete',
      subject: 'http://example.com/s2',
      predicate: 'http://example.com/p2',
      object: 'v2',
    },
  ];

  it('generates valid receipt with Q* format', async () => {
    const receipt = await generateBatchReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: sampleOperations,
      operationType: 'morphism',
    });

    expect(receipt.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
    expect(receipt.Q_PROV).toHaveProperty('timestamp');
    expect(receipt.Q_PROV).toHaveProperty('batchSize');
    expect(receipt.Q_PROV.batchSize).toBe(2);
  });

  it('computes content hash for batch', async () => {
    const receipt = await generateBatchReceipt({
      universeID: 'Q*_0123456789abcdef',
      operations: sampleOperations,
      operationType: 'test',
    });

    expect(receipt.Q_PROV.contentHash).toMatch(/^[a-f0-9]{64}$/);
  });
});
