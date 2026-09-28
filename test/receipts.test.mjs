/**
 * @file Receipt Tests
 * @description Tests for receipt generation, chaining, and merkle batching
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';
import {
  Receipt,
  ReceiptGenerator,
  ReceiptChain,
  MerkleBatcher,
} from '../src/admission/receipts.mjs';

describe('Receipt Tests', () => {
  test('[TEST] Receipt - Generation is deterministic (same input → same hash)', () => {
    console.log('[START] Testing receipt determinism');
    const config = {
      id: 'urn:receipt:1:test',
      decision: 'ALLOW',
      deltaHash: 'abcd1234',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: 1234567890,
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test receipt',
    };

    const receipt1 = new Receipt(config);
    const receipt2 = new Receipt(config);

    const hash1 = receipt1.getHash();
    const hash2 = receipt2.getHash();

    console.log(`[ASSERT] Hash 1: ${hash1.slice(0, 16)}...`);
    console.log(`[ASSERT] Hash 2: ${hash2.slice(0, 16)}...`);
    assert.equal(hash1, hash2, 'Identical receipts should have identical hashes');
    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Decision (allow/deny) is captured correctly', () => {
    console.log('[START] Testing decision capture');
    const allowReceipt = new Receipt({
      id: 'urn:receipt:1:allow',
      decision: 'ALLOW',
      deltaHash: 'test',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Allowed',
    });

    console.log(`[ASSERT] Allow decision: ${allowReceipt.decision}`);
    assert.equal(allowReceipt.decision, 'ALLOW');

    const denyReceipt = new Receipt({
      id: 'urn:receipt:2:deny',
      decision: 'DENY',
      deltaHash: 'test',
      beforeHash: '0'.repeat(64),
      afterHash: '0'.repeat(64),
      epoch: 2,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: ['Test violation'],
      reason: 'Denied',
    });

    console.log(`[ASSERT] Deny decision: ${denyReceipt.decision}`);
    assert.equal(denyReceipt.decision, 'DENY');
    console.log(`[ASSERT] Violations captured: ${denyReceipt.violations.length}`);
    assert.equal(denyReceipt.violations.length, 1);
    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Toolchain versions are included', () => {
    console.log('[START] Testing toolchain version');
    const generator = new ReceiptGenerator({ toolchainVersion: '2.3.4' });

    const result = { decision: 'ALLOW', violations: [], reason: 'Test' };
    const delta = { additions: [], deletions: [] };
    const receipt = generator.generate(result, delta, '0'.repeat(64));

    console.log(`[ASSERT] Toolchain version: ${receipt.toolchainVersion}`);
    assert.equal(receipt.toolchainVersion, '2.3.4');
    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Epochs are monotonically increasing', () => {
    console.log('[START] Testing epoch monotonicity');
    const generator = new ReceiptGenerator();

    const result = { decision: 'ALLOW', violations: [], reason: 'Test' };
    const delta = { additions: [], deletions: [] };

    const receipt1 = generator.generate(result, delta, '0'.repeat(64));
    const receipt2 = generator.generate(result, delta, '0'.repeat(64));
    const receipt3 = generator.generate(result, delta, '0'.repeat(64));

    console.log(`[ASSERT] Epoch 1: ${receipt1.epoch}`);
    console.log(`[ASSERT] Epoch 2: ${receipt2.epoch}`);
    console.log(`[ASSERT] Epoch 3: ${receipt3.epoch}`);

    assert.ok(receipt2.epoch > receipt1.epoch);
    assert.ok(receipt3.epoch > receipt2.epoch);
    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Chaining: beforeHash → afterHash links correctly', () => {
    console.log('[START] Testing receipt chaining');
    const generator = new ReceiptGenerator();
    const chain = new ReceiptChain();

    const result = { decision: 'ALLOW', violations: [], reason: 'Test' };
    const delta1 = {
      additions: [{ subject: 's1', predicate: 'p1', object: 'o1' }],
      deletions: [],
    };
    const delta2 = {
      additions: [{ subject: 's2', predicate: 'p2', object: 'o2' }],
      deletions: [],
    };

    const receipt1 = generator.generate(result, delta1, '0'.repeat(64));
    chain.addReceipt(receipt1);

    console.log(`[ASSERT] Receipt 1 afterHash: ${receipt1.afterHash.slice(0, 16)}...`);

    const receipt2 = generator.generate(result, delta2, receipt1.afterHash);
    chain.addReceipt(receipt2);

    console.log(`[ASSERT] Receipt 2 beforeHash: ${receipt2.beforeHash.slice(0, 16)}...`);
    console.log('[ASSERT] Chain links match');
    assert.equal(receipt2.beforeHash, receipt1.afterHash);

    console.log('[ASSERT] Verifying chain integrity');
    const isValid = chain.verifyChain();
    assert.equal(isValid, true);
    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Merkle root computed over 10 receipts correctly', () => {
    console.log('[START] Testing merkle root computation');
    const generator = new ReceiptGenerator();
    const batcher = new MerkleBatcher();
    const receipts = [];

    const result = { decision: 'ALLOW', violations: [], reason: 'Test' };

    for (let i = 0; i < 10; i++) {
      const delta = {
        additions: [{ subject: `s${i}`, predicate: `p${i}`, object: `o${i}` }],
        deletions: [],
      };
      const receipt = generator.generate(result, delta, '0'.repeat(64));
      receipts.push(receipt);
    }

    console.log(`[ASSERT] Generated ${receipts.length} receipts`);
    const merkleRoot = batcher.computeMerkleRoot(receipts);

    console.log(`[ASSERT] Merkle root: ${merkleRoot.slice(0, 16)}...`);
    assert.ok(merkleRoot.length === 64, 'Merkle root should be 64 character hash');

    console.log('[ASSERT] Merkle root is deterministic');
    const merkleRoot2 = batcher.computeMerkleRoot(receipts);
    assert.equal(merkleRoot, merkleRoot2);

    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Serialization to JSON-LD and back preserves data', () => {
    console.log('[START] Testing JSON-LD serialization');
    const original = new Receipt({
      id: 'urn:receipt:1:test',
      decision: 'ALLOW',
      deltaHash: 'abcd1234',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: 1234567890,
      toolchainVersion: '1.0.0',
      violations: ['violation1', 'violation2'],
      reason: 'Test receipt',
    });

    console.log('[ASSERT] Converting to JSON-LD');
    const jsonld = original.toJSONLD();
    assert.ok(jsonld['@context']);
    assert.equal(jsonld['@type'], 'AdmissionReceipt');

    console.log('[ASSERT] Converting back from JSON-LD');
    const restored = Receipt.fromJSONLD(jsonld);

    assert.equal(restored.id, original.id);
    assert.equal(restored.decision, original.decision);
    assert.equal(restored.deltaHash, original.deltaHash);
    assert.equal(restored.beforeHash, original.beforeHash);
    assert.equal(restored.afterHash, original.afterHash);
    assert.equal(restored.epoch, original.epoch);
    assert.equal(restored.toolchainVersion, original.toolchainVersion);
    assert.deepEqual(restored.violations, original.violations);
    assert.equal(restored.reason, original.reason);

    console.log('[RESULT] pass');
  });

  test('[TEST] ReceiptGenerator - Delta hashing is deterministic', () => {
    console.log('[START] Testing delta hash determinism');
    const generator = new ReceiptGenerator();

    const result = { decision: 'ALLOW', violations: [], reason: 'Test' };
    const delta = {
      additions: [
        { subject: 's2', predicate: 'p2', object: 'o2' },
        { subject: 's1', predicate: 'p1', object: 'o1' },
      ],
      deletions: [],
    };

    const receipt1 = generator.generate(result, delta, '0'.repeat(64));
    const generator2 = new ReceiptGenerator();
    const receipt2 = generator2.generate(result, delta, '0'.repeat(64));

    console.log(`[ASSERT] Delta hash 1: ${receipt1.deltaHash.slice(0, 16)}...`);
    console.log(`[ASSERT] Delta hash 2: ${receipt2.deltaHash.slice(0, 16)}...`);
    assert.equal(receipt1.deltaHash, receipt2.deltaHash);
    console.log('[RESULT] pass');
  });

  test('[TEST] ReceiptChain - Broken chain is detected', () => {
    console.log('[START] Testing broken chain detection');
    const chain = new ReceiptChain();

    const receipt1 = new Receipt({
      id: 'urn:receipt:1',
      decision: 'ALLOW',
      deltaHash: 'test1',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test',
    });

    const receipt2 = new Receipt({
      id: 'urn:receipt:2',
      decision: 'ALLOW',
      deltaHash: 'test2',
      beforeHash: '9'.repeat(64), // Broken link
      afterHash: '2'.repeat(64),
      epoch: 2,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test',
    });

    chain.addReceipt(receipt1);
    chain.addReceipt(receipt2);

    console.log('[ASSERT] Verifying chain');
    const isValid = chain.verifyChain();
    console.log(`[ASSERT] Chain valid: ${isValid}`);
    assert.equal(isValid, false);
    console.log('[RESULT] pass');
  });

  test('[TEST] ReceiptChain - Non-monotonic epochs are detected', () => {
    console.log('[START] Testing epoch monotonicity check');
    const chain = new ReceiptChain();

    const receipt1 = new Receipt({
      id: 'urn:receipt:1',
      decision: 'ALLOW',
      deltaHash: 'test1',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 5,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test',
    });

    const receipt2 = new Receipt({
      id: 'urn:receipt:2',
      decision: 'ALLOW',
      deltaHash: 'test2',
      beforeHash: '1'.repeat(64),
      afterHash: '2'.repeat(64),
      epoch: 3, // Non-monotonic
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test',
    });

    chain.addReceipt(receipt1);
    chain.addReceipt(receipt2);

    console.log('[ASSERT] Verifying chain');
    const isValid = chain.verifyChain();
    console.log(`[ASSERT] Chain valid: ${isValid}`);
    assert.equal(isValid, false);
    console.log('[RESULT] pass');
  });

  test('[TEST] MerkleBatcher - Empty receipts return empty hash', () => {
    console.log('[START] Testing empty batch');
    const batcher = new MerkleBatcher();
    const merkleRoot = batcher.computeMerkleRoot([]);

    console.log(`[ASSERT] Empty merkle root: ${merkleRoot.slice(0, 16)}...`);
    assert.ok(merkleRoot.length === 64);
    console.log('[RESULT] pass');
  });

  test('[TEST] MerkleBatcher - Single receipt returns receipt hash', () => {
    console.log('[START] Testing single receipt batch');
    const batcher = new MerkleBatcher();
    const receipt = new Receipt({
      id: 'urn:receipt:1',
      decision: 'ALLOW',
      deltaHash: 'test',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test',
    });

    const merkleRoot = batcher.computeMerkleRoot([receipt]);
    const receiptHash = receipt.getHash();

    console.log(`[ASSERT] Merkle root: ${merkleRoot.slice(0, 16)}...`);
    console.log(`[ASSERT] Receipt hash: ${receiptHash.slice(0, 16)}...`);
    assert.equal(merkleRoot, receiptHash);
    console.log('[RESULT] pass');
  });

  test('[TEST] MerkleBatcher - Create batch with metadata', () => {
    console.log('[START] Testing batch creation');
    const generator = new ReceiptGenerator();
    const batcher = new MerkleBatcher();
    const receipts = [];

    const result = { decision: 'ALLOW', violations: [], reason: 'Test' };
    for (let i = 0; i < 5; i++) {
      const delta = { additions: [{ subject: `s${i}`, predicate: 'p', object: 'o' }], deletions: [] };
      receipts.push(generator.generate(result, delta, '0'.repeat(64)));
    }

    const batch = batcher.createBatch(receipts);

    console.log(`[ASSERT] Batch count: ${batch.count}`);
    assert.equal(batch.count, 5);
    console.log(`[ASSERT] Batch merkle root: ${batch.merkleRoot.slice(0, 16)}...`);
    assert.ok(batch.merkleRoot);
    console.log(`[ASSERT] Batch timestamp: ${batch.timestamp}`);
    assert.ok(batch.timestamp);
    console.log('[RESULT] pass');
  });

  test('[TEST] Receipt - Immutability is enforced', () => {
    console.log('[START] Testing receipt immutability');
    const receipt = new Receipt({
      id: 'urn:receipt:1',
      decision: 'ALLOW',
      deltaHash: 'test',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: Date.now(),
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Test',
    });

    console.log('[ASSERT] Attempting to modify frozen object');
    assert.throws(() => {
      receipt.decision = 'DENY';
    }, 'Receipt should be immutable');
    console.log('[RESULT] pass');
  });
});

console.log('\n=== Receipt Test Suite Complete ===');
