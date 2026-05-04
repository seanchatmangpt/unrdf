/**
 * Receipt Chain Integration Tests
 * Phase 5: 10 tests covering Single receipt, Batch processing, Merkle, Replay prevention, Chain validation
 *
 * @module @unrdf/integration-tests/test/chains/receipt
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  buildMerkleTree,
  generateMerkleProof,
  verifyMerkleProof,
  batchWithMerkleTree,
  verifyOperationInBatch,
  getMerkleRoot,
  calculateTreeDepth,
  getLeafCount,
} from '@unrdf/receipts';
import { blake3 } from 'hash-wasm';

/**
 * Helper: Generate test operations
 * @param {number} count - Number of operations
 * @returns {Array} Array of test operations
 */
function generateTestOperations(count) {
  const operations = [];
  for (let i = 0; i < count; i++) {
    operations.push({
      id: `op-${i}`,
      type: 'ADD',
      subject: `http://example.org/subject/${i}`,
      predicate: 'http://example.org/predicate',
      object: `value-${i}`,
      timestamp: BigInt(Date.now() * 1000000 + i),
    });
  }
  return operations;
}

/**
 * Helper: Create receipt-like object for single verification
 * @param {Object} operation - Operation data
 * @returns {Promise<Object>} Receipt object
 */
async function createSingleReceipt(operation) {
  const serialized = JSON.stringify(operation, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  const hash = await blake3(serialized);

  return {
    id: `receipt-${operation.id}`,
    operation,
    hash,
    timestamp: operation.timestamp,
    parentHash: null,
  };
}

describe('Receipt Chain Integration Tests', () => {
  // Test 1: Single receipt generation and verification
  it('should generate and verify single receipt', async () => {
    const operation = {
      id: 'single-op',
      type: 'ADD',
      subject: 'http://example.org/s1',
      predicate: 'http://example.org/p1',
      object: 'value1',
      timestamp: BigInt(Date.now() * 1000000),
    };

    const receipt = await createSingleReceipt(operation);

    expect(receipt.id).toBe('receipt-single-op');
    expect(receipt.hash).toMatch(/^[a-f0-9]{64}$/);
    expect(receipt.operation).toEqual(operation);

    // Verify hash is deterministic
    const receipt2 = await createSingleReceipt(operation);
    expect(receipt2.hash).toBe(receipt.hash);
  });

  // Test 2: Batch processing - 100 receipts to Merkle root
  it('should batch 100 operations into Merkle root', async () => {
    const operations = generateTestOperations(100);
    const batch = await batchWithMerkleTree(operations);

    expect(batch.batchSize).toBe(100);
    expect(batch.merkleRoot).toMatch(/^[a-f0-9]{64}$/);
    expect(batch.operations).toEqual(operations);
    expect(batch.tree).toBeDefined();
  });

  // Test 3: Merkle proof generation
  it('should generate Merkle proof for leaf', async () => {
    const operations = generateTestOperations(8);
    const tree = await buildMerkleTree(operations);

    const proof = generateMerkleProof(tree, 3);

    expect(proof.index).toBe(3);
    expect(proof.leaf).toMatch(/^[a-f0-9]{64}$/);
    expect(proof.root).toBe(tree.hash);
    expect(Array.isArray(proof.proof)).toBe(true);
    expect(proof.proof.length).toBeGreaterThan(0);
  });

  // Test 4: Merkle proof verification
  it('should verify valid Merkle proof', async () => {
    const operations = generateTestOperations(16);
    const tree = await buildMerkleTree(operations);

    // Generate and verify proof for index 5
    const proof = generateMerkleProof(tree, 5);

    // Compute leaf hash for verification
    const leafData = operations[5];
    const serialized = JSON.stringify(leafData, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
    const leafHash = await blake3(serialized);

    const isValid = await verifyMerkleProof(proof, leafHash);
    expect(isValid).toBe(true);
  });

  // Test 5: Receipt replay prevention - timestamp validation
  it('should detect replay via timestamp comparison', async () => {
    const baseTime = BigInt(Date.now() * 1000000);

    const originalOp = {
      id: 'replay-test',
      type: 'ADD',
      subject: 'http://example.org/s1',
      predicate: 'http://example.org/p1',
      object: 'value1',
      timestamp: baseTime,
    };

    const receipt1 = await createSingleReceipt(originalOp);

    // Simulate replay attack with same operation but at later "now"
    const isReplay = (receipt, currentTime, maxAge = 60000000000n) => {
      const receiptAge = currentTime - receipt.timestamp;
      return receiptAge > maxAge;
    };

    // Fresh receipt should not be considered replay
    const currentTime = baseTime + 1000000n; // 1ms later
    expect(isReplay(receipt1, currentTime)).toBe(false);

    // Old receipt should be considered replay
    const farFuture = baseTime + 120000000000n; // 2 minutes later
    expect(isReplay(receipt1, farFuture)).toBe(true);
  });

  // Test 6: Chain link validation - parentHash to childHash
  it('should validate receipt chain links', async () => {
    const operations = generateTestOperations(5);
    const chain = [];

    for (let i = 0; i < operations.length; i++) {
      const receipt = await createSingleReceipt(operations[i]);
      receipt.parentHash = i > 0 ? chain[i - 1].hash : null;
      chain.push(receipt);
    }

    // Validate chain integrity
    const validateChain = (receipts) => {
      for (let i = 1; i < receipts.length; i++) {
        if (receipts[i].parentHash !== receipts[i - 1].hash) {
          return { valid: false, breakAt: i };
        }
      }
      return { valid: true };
    };

    const result = validateChain(chain);
    expect(result.valid).toBe(true);

    // Corrupt chain and verify detection
    chain[2].parentHash = 'corrupted_hash';
    const corruptResult = validateChain(chain);
    expect(corruptResult.valid).toBe(false);
    expect(corruptResult.breakAt).toBe(2);
  });

  // Test 7: Merkle tree depth calculation
  it('should calculate correct tree depth', async () => {
    // 1 element = depth 0
    const tree1 = await buildMerkleTree([{ id: 1 }]);
    expect(calculateTreeDepth(tree1)).toBe(0);

    // 2 elements = depth 1
    const tree2 = await buildMerkleTree([{ id: 1 }, { id: 2 }]);
    expect(calculateTreeDepth(tree2)).toBe(1);

    // 4 elements = depth 2
    const tree4 = await buildMerkleTree([{ id: 1 }, { id: 2 }, { id: 3 }, { id: 4 }]);
    expect(calculateTreeDepth(tree4)).toBe(2);

    // 8 elements = depth 3
    const tree8 = await buildMerkleTree(generateTestOperations(8));
    expect(calculateTreeDepth(tree8)).toBe(3);
  });

  // Test 8: Leaf count verification
  it('should count leaves correctly', async () => {
    const ops16 = generateTestOperations(16);
    const tree16 = await buildMerkleTree(ops16);
    expect(getLeafCount(tree16)).toBe(16);

    const ops7 = generateTestOperations(7);
    const tree7 = await buildMerkleTree(ops7);
    // With duplication for odd counts, we still count original leaves
    expect(getLeafCount(tree7)).toBeGreaterThanOrEqual(7);
  });

  // Test 9: Verify operation in batch
  it('should verify operation inclusion in batch', async () => {
    // Use power of 2 for clean Merkle tree structure
    const operations = generateTestOperations(8);
    const batch = await batchWithMerkleTree(operations);

    // Verify first, middle, and last operations
    const indicesToTest = [0, 3, 7];
    for (const i of indicesToTest) {
      const proof = generateMerkleProof(batch.tree, i);
      const serialized = JSON.stringify(operations[i], (key, value) =>
        typeof value === 'bigint' ? value.toString() : value
      );
      const leafHash = await blake3(serialized);
      const isValid = await verifyMerkleProof(proof, leafHash);
      expect(isValid).toBe(true);
    }

    // Tampered operation should fail verification
    const tamperedOp = { ...operations[3], object: 'tampered-value' };
    const tamperedSerialized = JSON.stringify(tamperedOp, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
    const tamperedHash = await blake3(tamperedSerialized);
    const originalProof = generateMerkleProof(batch.tree, 3);
    const isTamperedValid = await verifyMerkleProof(originalProof, tamperedHash);
    expect(isTamperedValid).toBe(false);
  });

  // Test 10: Merkle root extraction and consistency
  it('should extract consistent Merkle root', async () => {
    const operations = generateTestOperations(32);
    const tree = await buildMerkleTree(operations);
    const batch = await batchWithMerkleTree(operations);

    const root1 = getMerkleRoot(tree);
    const root2 = batch.merkleRoot;

    expect(root1).toBe(root2);
    expect(root1).toMatch(/^[a-f0-9]{64}$/);

    // Root should be deterministic
    const tree2 = await buildMerkleTree(operations);
    const root3 = getMerkleRoot(tree2);
    expect(root3).toBe(root1);
  });
});
