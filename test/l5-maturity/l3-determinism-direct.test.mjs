/**
 * @file L3 Maturity Tests - Determinism (Direct Imports)
 * @description Validates determinism using direct file imports
 *
 * CRITICAL: Run same operation 100x with same inputs → identical outputs + identical receipts
 */

import { test, describe, expect } from 'vitest';

describe('L3: Determinism - 100x Identical Runs (Direct)', () => {
  test('[L3.2-DIRECT] Receipt generation is deterministic (100x runs)', async () => {
    console.log('[L3.2] Testing receipt determinism (100 iterations)');

    const { Receipt } = await import('../../src/admission/receipts.mjs');

    const config = {
      id: 'urn:receipt:test:determinism',
      decision: 'ALLOW',
      deltaHash: 'determinism-test-hash',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: 1234567890, // Fixed timestamp
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Determinism test',
    };

    const hashes = [];

    for (let i = 0; i < 100; i++) {
      const receipt = new Receipt(config);
      const hash = receipt.getHash();
      hashes.push(hash);
    }

    // All hashes should be identical
    const uniqueHashes = new Set(hashes);
    expect(
      uniqueHashes.size).toBe(
      1);

    const finalHash = Array.from(uniqueHashes)[0];
    console.log(`[L3.2] ✅ 100/100 receipts have identical hash: ${finalHash.slice(0, 16)}...`);
    console.log(`[L3.2] Full hash: ${finalHash}`);
  });

  test('[L3.2-CHAIN] Receipt chain is deterministic (100x runs)', async () => {
    console.log('[L3.2-CHAIN] Testing receipt chain determinism (100 iterations)');

    const { ReceiptChain, Receipt } = await import('../../src/admission/receipts.mjs');

    const chainHashes = [];

    for (let i = 0; i < 100; i++) {
      const chain = new ReceiptChain();

      // Add same receipts in same order
      for (let j = 0; j < 5; j++) {
        const receipt = new Receipt({
          id: `urn:receipt:chain:${j}`,
          decision: 'ALLOW',
          deltaHash: `delta-${j}`,
          beforeHash: '0'.repeat(64),
          afterHash: '1'.repeat(64),
          epoch: j + 1,
          timestamp: 1234567890 + j,
          toolchainVersion: '1.0.0',
          violations: [],
          reason: `Receipt ${j}`,
        });
        chain.addReceipt(receipt);
      }

      // Get final receipt hash (last in chain)
      const lastReceipt = chain.receipts[chain.receipts.length - 1];
      chainHashes.push(lastReceipt.getHash());
    }

    const uniqueHashes = new Set(chainHashes);
    expect(
      uniqueHashes.size).toBe(
      1);

    console.log(`[L3.2-CHAIN] ✅ 100/100 chains have identical final hash: ${Array.from(uniqueHashes)[0].slice(0, 16)}...`);
  });

  test('[L3.2-MERKLE] Merkle batching is deterministic (100x runs)', async () => {
    console.log('[L3.2-MERKLE] Testing merkle batch determinism (100 iterations)');

    const { MerkleBatcher, Receipt } = await import('../../src/admission/receipts.mjs');

    const merkleRoots = [];

    const receipts = [
      {
        id: 'urn:receipt:1',
        decision: 'ALLOW',
        deltaHash: 'delta-1',
        beforeHash: '0'.repeat(64),
        afterHash: '1'.repeat(64),
        epoch: 1,
        timestamp: 1234567890,
        toolchainVersion: '1.0.0',
        violations: [],
        reason: 'Test 1',
      },
      {
        id: 'urn:receipt:2',
        decision: 'ALLOW',
        deltaHash: 'delta-2',
        beforeHash: '1'.repeat(64),
        afterHash: '2'.repeat(64),
        epoch: 2,
        timestamp: 1234567891,
        toolchainVersion: '1.0.0',
        violations: [],
        reason: 'Test 2',
      },
    ];

    for (let i = 0; i < 100; i++) {
      const batcher = new MerkleBatcher();
      const receiptObjects = receipts.map(r => new Receipt(r));
      const root = batcher.computeMerkleRoot(receiptObjects);
      merkleRoots.push(root);
    }

    const uniqueRoots = new Set(merkleRoots);
    expect(
      uniqueRoots.size).toBe(
      1);

    console.log(`[L3.2-MERKLE] ✅ 100/100 merkle batches have identical root: ${Array.from(uniqueRoots)[0].slice(0, 16)}...`);
  });
});

// Export for evidence reporting
export const L3_DIRECT_CRITERIA = {
  level: 'L3',
  name: 'Determinism (Direct Imports)',
  tests: 3,
  iterations: 100,
  target: '100/100 per operation',
  status: 'Using direct file imports to bypass workspace resolution issues',
};
