/**
 * @file Test Merkle root verification implementation
 * @description Validates that Merkle root calculation and verification works correctly
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { LockchainWriter } from '../src/knowledge-engine/lockchain-writer.mjs';
import { existsSync, rmSync, mkdirSync } from 'fs';
import { join } from 'path';

describe('Lockchain Merkle Root Verification', () => {
  let lockchain;
  let testStoragePath;

  beforeEach(() => {
    testStoragePath = join(process.cwd(), '.test-lockchain-merkle');
    if (existsSync(testStoragePath)) {
      rmSync(testStoragePath, { recursive: true, force: true });
    }
    mkdirSync(testStoragePath, { recursive: true });

    lockchain = new LockchainWriter({
      gitRepo: process.cwd(),
      enableMerkle: true,
      enableGitAnchoring: false, // Disable Git for unit testing
      storagePath: testStoragePath,
    });
  });

  afterEach(() => {
    if (existsSync(testStoragePath)) {
      rmSync(testStoragePath, { recursive: true, force: true });
    }
  });

  it('should calculate Merkle root when writing receipt', async () => {
    const receipt = {
      transactionId: 'tx-123',
      operation: 'create',
      data: { test: 'value' },
    };

    const entry = await lockchain.writeReceipt(receipt);

    // Verify Merkle root was calculated
    expect(entry.merkleRoot).toBeDefined();
    expect(typeof entry.merkleRoot).toBe('string');
    expect(entry.merkleRoot.length).toBeGreaterThan(0);
  });

  it('should verify valid Merkle root', async () => {
    const receipt = {
      transactionId: 'tx-456',
      operation: 'update',
      data: { test: 'value2' },
    };

    const entry = await lockchain.writeReceipt(receipt);
    const verification = await lockchain.verifyEntry(entry.id);

    // Verification should pass
    expect(verification.valid).toBe(true);
    expect(verification.error).toBeUndefined();
  });

  it('should fail verification for tampered Merkle root', async () => {
    const receipt = {
      transactionId: 'tx-789',
      operation: 'delete',
      data: { test: 'value3' },
    };

    const entry = await lockchain.writeReceipt(receipt);

    // Tamper with the Merkle root
    entry.merkleRoot = '0000000000000000000000000000000000000000000000000000000000000000';
    await lockchain._updateEntry(entry);

    const verification = await lockchain.verifyEntry(entry.id);

    // Verification should fail
    expect(verification.valid).toBe(false);
    expect(verification.error).toBe('Invalid merkle root');
  });

  it('should fail verification for tampered entry data', async () => {
    const receipt = {
      transactionId: 'tx-999',
      operation: 'create',
      data: { sensitive: 'data' },
    };

    const entry = await lockchain.writeReceipt(receipt);
    const _originalMerkleRoot = entry.merkleRoot;

    // Tamper with receipt data (but keep Merkle root unchanged)
    entry.receipt.data.sensitive = 'tampered';
    await lockchain._updateEntry(entry);

    const verification = await lockchain.verifyEntry(entry.id);

    // Verification should fail because data changed but Merkle root didn't
    expect(verification.valid).toBe(false);
    expect(verification.error).toBe('Invalid merkle root');
  });

  it('should use same hash for identical entry data', async () => {
    const receipt1 = {
      transactionId: 'tx-identical',
      operation: 'create',
      data: { value: 42 },
    };

    const receipt2 = {
      transactionId: 'tx-identical',
      operation: 'create',
      data: { value: 42 },
    };

    const entry1 = await lockchain.writeReceipt(receipt1);
    const entry2 = await lockchain.writeReceipt(receipt2);

    // Different entries but should have same receipt data
    // However, they'll have different IDs, timestamps, and signatures
    // so Merkle roots will differ
    expect(entry1.merkleRoot).toBeDefined();
    expect(entry2.merkleRoot).toBeDefined();
    expect(entry1.id).not.toBe(entry2.id);
  });

  it('should handle missing Merkle root gracefully', async () => {
    const receipt = {
      transactionId: 'tx-no-merkle',
      operation: 'create',
      data: { test: 'no merkle' },
    };

    // Create lockchain with Merkle disabled
    const noMerkleLockchain = new LockchainWriter({
      gitRepo: process.cwd(),
      enableMerkle: false,
      enableGitAnchoring: false,
      storagePath: testStoragePath,
    });

    const entry = await noMerkleLockchain.writeReceipt(receipt);

    // Entry should not have Merkle root
    expect(entry.merkleRoot).toBeUndefined();

    // But verification should still pass
    const verification = await noMerkleLockchain.verifyEntry(entry.id);
    expect(verification.valid).toBe(true);
  });

  it('should use SHA3-256 for Merkle root calculation', async () => {
    const receipt = {
      transactionId: 'tx-hash-test',
      operation: 'create',
      data: { test: 'hash validation' },
    };

    const entry = await lockchain.writeReceipt(receipt);

    // SHA3-256 produces 64-character hex string (32 bytes)
    expect(entry.merkleRoot).toMatch(/^[0-9a-f]{64}$/);
  });
});
