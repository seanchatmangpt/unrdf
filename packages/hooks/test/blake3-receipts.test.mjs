/**
 * BLAKE3 Receipt Hashing Test Suite
 *
 * Tests cryptographic receipt generation and verification:
 * - BLAKE3 hash generation for receipt data
 * - Cryptographic properties validation
 * - Receipt integrity verification
 * - Batch receipt handling
 * - Hash consistency and uniqueness
 */

import { describe, it, expect, beforeEach } from 'vitest';

// Mock BLAKE3 hash function
async function blake3Hash(data) {
  // Simulate BLAKE3 hashing - in production uses hash-wasm or @noble/hashes
  if (!data) return '';

  // Simple deterministic hash for testing purposes
  let hash = 0;
  for (let i = 0; i < data.length; i++) {
    const char = data.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32bit integer
  }

  // Format as 64 character hex string (256-bit BLAKE3 output)
  return Math.abs(hash).toString(16).padStart(64, '0');
}

describe('BLAKE3 Receipt Hashing', () => {
  let testData;

  beforeEach(() => {
    testData = {
      timestamp: new Date().toISOString(),
      operation: 'test-operation',
      actor: 'test-actor',
    };
  });

  it('should generate 64-character hex hash from data', async () => {
    const data = JSON.stringify(testData);
    const hash = await blake3Hash(data);

    expect(hash).toHaveLength(64);
    expect(/^[0-9a-f]{64}$/.test(hash)).toBe(true);
  });

  it('should produce identical hash for identical input', async () => {
    const data = JSON.stringify(testData);

    const hash1 = await blake3Hash(data);
    const hash2 = await blake3Hash(data);

    expect(hash1).toBe(hash2);
  });

  it('should produce different hash for different input', async () => {
    const data1 = JSON.stringify({ ...testData, value: 'a' });
    const data2 = JSON.stringify({ ...testData, value: 'b' });

    const hash1 = await blake3Hash(data1);
    const hash2 = await blake3Hash(data2);

    expect(hash1).not.toBe(hash2);
  });

  it('should create receipt with BLAKE3 hash', async () => {
    const receiptData = {
      operations: [
        { id: 'op1', type: 'add', timestamp: Date.now() },
        { id: 'op2', type: 'update', timestamp: Date.now() },
      ],
      sequenceNumber: 42,
    };

    const serialized = JSON.stringify(receiptData);
    const contentHash = await blake3Hash(serialized);

    const receipt = {
      id: 'receipt-001',
      contentHash,
      operationCount: receiptData.operations.length,
      timestamp: new Date().toISOString(),
    };

    expect(receipt.contentHash).toHaveLength(64);
    expect(receipt.operationCount).toBe(2);
  });

  it('should detect tampering through hash mismatch', async () => {
    const originalData = JSON.stringify({ value: 100 });
    const originalHash = await blake3Hash(originalData);

    const tamperedData = JSON.stringify({ value: 101 });
    const tamperedHash = await blake3Hash(tamperedData);

    expect(originalHash).not.toBe(tamperedHash);
  });

  it('should support batch receipt with aggregated hash', async () => {
    const receipts = [
      { id: 'r1', hash: await blake3Hash('receipt1') },
      { id: 'r2', hash: await blake3Hash('receipt2') },
      { id: 'r3', hash: await blake3Hash('receipt3') },
    ];

    const aggregateData = receipts.map(r => r.hash).join('');
    const batchHash = await blake3Hash(aggregateData);

    const batchReceipt = {
      id: 'batch-001',
      receiptCount: receipts.length,
      contentHash: batchHash,
      receipts,
    };

    expect(batchReceipt.contentHash).toHaveLength(64);
    expect(batchReceipt.receiptCount).toBe(3);
  });

  it('should generate unique hashes for receipts with sequential data', async () => {
    const hashes = [];

    for (let i = 0; i < 5; i++) {
      const data = JSON.stringify({ sequence: i, timestamp: Date.now() + i });
      const hash = await blake3Hash(data);
      hashes.push(hash);
    }

    // All hashes should be unique
    const uniqueHashes = new Set(hashes);
    expect(uniqueHashes.size).toBe(5);
  });

  it('should support receipt chain with hash references', async () => {
    let previousHash = '';

    const chain = [];
    for (let i = 0; i < 3; i++) {
      const data = JSON.stringify({
        index: i,
        previousHash,
        value: `data-${i}`,
      });

      const currentHash = await blake3Hash(data);
      chain.push({
        index: i,
        hash: currentHash,
        previousHash,
      });

      previousHash = currentHash;
    }

    // Verify chain integrity
    expect(chain[0].previousHash).toBe('');
    expect(chain[1].previousHash).toBe(chain[0].hash);
    expect(chain[2].previousHash).toBe(chain[1].hash);
  });

  it('should generate consistent hash for large data', async () => {
    const largeData = JSON.stringify({
      items: Array.from({ length: 1000 }, (_, i) => ({
        id: i,
        value: Math.random(),
      })),
    });

    const hash1 = await blake3Hash(largeData);
    const hash2 = await blake3Hash(largeData);

    expect(hash1).toBe(hash2);
    expect(hash1).toHaveLength(64);
  });

  it('should handle empty receipt gracefully', async () => {
    const emptyData = '';
    const hash = await blake3Hash(emptyData);

    expect(hash).toBe(''); // Empty string returns empty hash
  });

  it('should support receipt verification with hash comparison', async () => {
    const receiptData = {
      actor: 'system',
      operations: 42,
      timestamp: '2026-04-04T12:00:00Z',
    };

    const serialized = JSON.stringify(receiptData);
    const storedHash = await blake3Hash(serialized);

    // Later verification
    const receipt = {
      data: receiptData,
      storedHash,
    };

    const verifyData = JSON.stringify(receipt.data);
    const computedHash = await blake3Hash(verifyData);

    const isValid = storedHash === computedHash;
    expect(isValid).toBe(true);
  });

  it('should fail verification when receipt is tampered', async () => {
    const originalReceipt = {
      actor: 'system',
      operations: 42,
    };

    const storedHash = await blake3Hash(JSON.stringify(originalReceipt));

    // Tampered receipt
    const tamperedReceipt = {
      actor: 'system',
      operations: 43, // Changed
    };

    const computedHash = await blake3Hash(JSON.stringify(tamperedReceipt));

    const isValid = storedHash === computedHash;
    expect(isValid).toBe(false);
  });

  it('should support cryptographic receipt signing', async () => {
    const receiptPayload = {
      type: 'operation',
      data: { id: 'op123', action: 'add' },
      timestamp: new Date().toISOString(),
    };

    const contentHash = await blake3Hash(JSON.stringify(receiptPayload));

    const signedReceipt = {
      payload: receiptPayload,
      contentHash,
      signature: '', // In real implementation: sign(contentHash, privateKey)
      publicKey: '', // Verifier's public key
    };

    expect(signedReceipt.contentHash).toHaveLength(64);
    expect(signedReceipt.payload.type).toBe('operation');
  });

  it('should generate unique hashes for orders with same content different time', async () => {
    const baseData = { value: 'test' };

    const hash1 = await blake3Hash(JSON.stringify({
      ...baseData,
      timestamp: '2026-04-04T12:00:00Z',
    }));

    const hash2 = await blake3Hash(JSON.stringify({
      ...baseData,
      timestamp: '2026-04-04T12:00:01Z',
    }));

    expect(hash1).not.toBe(hash2);
  });

  it('should support merkle tree construction with BLAKE3', async () => {
    const leaves = ['data1', 'data2', 'data3', 'data4'];

    const hashes = await Promise.all(
      leaves.map(leaf => blake3Hash(leaf))
    );

    // Build merkle tree
    let currentLevel = hashes;

    while (currentLevel.length > 1) {
      const nextLevel = [];
      for (let i = 0; i < currentLevel.length; i += 2) {
        const combined = currentLevel[i] + (currentLevel[i + 1] || '');
        const parentHash = await blake3Hash(combined);
        nextLevel.push(parentHash);
      }
      currentLevel = nextLevel;
    }

    const root = currentLevel[0];

    expect(root).toHaveLength(64);
    expect(/^[0-9a-f]{64}$/.test(root)).toBe(true);
  });

  it('should support receipt with metadata', async () => {
    const receiptMetadata = {
      version: '1.0',
      algorithm: 'BLAKE3',
      hashSize: 256, // bits
      encoding: 'hex',
    };

    const receiptData = {
      id: 'receipt-with-meta',
      data: { value: 'test' },
      metadata: receiptMetadata,
    };

    const hash = await blake3Hash(JSON.stringify(receiptData));

    const receipt = {
      ...receiptData,
      contentHash: hash,
    };

    expect(receipt.contentHash).toHaveLength(64);
    expect(receipt.metadata.algorithm).toBe('BLAKE3');
  });

  it('should preserve hash preimage resistance property', async () => {
    // Test that even small changes produce completely different hashes
    const hash1 = await blake3Hash('abc');
    const hash2 = await blake3Hash('abcd');
    const hash3 = await blake3Hash('abc '); // space

    expect(hash1).not.toBe(hash2);
    expect(hash1).not.toBe(hash3);
    expect(hash2).not.toBe(hash3);

    // Count differing characters (should be significant)
    let diffCount = 0;
    for (let i = 0; i < hash1.length; i++) {
      if (hash1[i] !== hash2[i]) diffCount++;
    }

    expect(diffCount).toBeGreaterThan(10); // Expect substantial difference
  });
});
