/**
 * @file End-to-End Tests - Daemon Receipts Merkle Integration
 * @module @unrdf/daemon/test/e2e-receipts-merkle
 * @description Comprehensive E2E tests for DaemonReceiptGenerator with Merkle tree
 * batching, chaining, proof verification, and tamper detection (15+ tests)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import DaemonReceiptGenerator from '../src/integrations/receipts-merkle.mjs';

// =============================================================================
// Helpers
// =============================================================================

/**
 * Generate UUID v4
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Create a test operation
 */
function createTestOperation(overrides = {}) {
  return {
    operationId: generateUUID(),
    operationType: 'task_executed',
    timestamp_ns: BigInt(Date.now() * 1_000_000),
    nodeId: 'test-node-1',
    daemonId: 'test-daemon-1',
    payload: { taskId: generateUUID(), status: 'completed', duration_ms: 42 },
    ...overrides,
  };
}

/**
 * Create multiple test operations
 */
function createTestOperations(count, overrides = {}) {
  return Array.from({ length: count }, (_, i) =>
    createTestOperation({
      operationId: generateUUID(),
      timestamp_ns: BigInt(Date.now() * 1_000_000 + i * 1000),
      ...overrides,
    })
  );
}

// =============================================================================
// Tests
// =============================================================================

describe('DaemonReceiptGenerator - Merkle Integration', () => {
  describe('initialization', () => {
    it('should create generator with default batch size', () => {
      // Arrange & Act
      const generator = new DaemonReceiptGenerator();

      // Assert
      expect(generator.batchSize).toBe(100);
      expect(generator.maxBufferSize).toBe(1000);
      expect(generator.operationBuffer.length).toBe(0);
      expect(generator.batchCounter).toBe(0);
    });

    it('should create generator with custom batch size', () => {
      // Arrange & Act
      const generator = new DaemonReceiptGenerator({ batchSize: 50 });

      // Assert
      expect(generator.batchSize).toBe(50);
    });

    it('should reject invalid batch size (< 10)', () => {
      // Act & Assert
      expect(() => new DaemonReceiptGenerator({ batchSize: 5 })).toThrow();
    });

    it('should reject invalid batch size (> 100)', () => {
      // Act & Assert
      expect(() => new DaemonReceiptGenerator({ batchSize: 150 })).toThrow();
    });
  });

  describe('single receipt generation', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should generate receipt with valid structure', async () => {
      // Arrange
      const operation = createTestOperation();

      // Act
      const receipt = await generator.generateReceipt(operation);

      // Assert
      expect(receipt.id).toBeDefined();
      expect(receipt.operationId).toBe(operation.operationId);
      expect(receipt.operationType).toBe(operation.operationType);
      expect(receipt.payloadHash).toHaveLength(64);
      expect(receipt.receiptHash).toHaveLength(64);
      expect(receipt.previousHash).toBeNull();
      expect(receipt.merkleLeafHash).toHaveLength(64);
      expect(receipt.batchIndex).toBe(0);
    });

    it('should generate receipt with unique ID', async () => {
      // Arrange
      const op1 = createTestOperation();
      const op2 = createTestOperation();

      // Act
      const receipt1 = await generator.generateReceipt(op1);
      const receipt2 = await generator.generateReceipt(op2);

      // Assert
      expect(receipt1.id).not.toBe(receipt2.id);
    });

    it('should store receipt in generator', async () => {
      // Arrange
      const operation = createTestOperation();

      // Act
      const receipt = await generator.generateReceipt(operation);

      // Assert
      expect(generator.receipts.has(receipt.id)).toBe(true);
      expect(generator.receipts.get(receipt.id)).toEqual(receipt);
    });

    it('should buffer receipt for batch generation', async () => {
      // Arrange
      const operation = createTestOperation();

      // Act
      await generator.generateReceipt(operation);

      // Assert
      expect(generator.operationBuffer.length).toBe(1);
    });
  });

  describe('receipt chaining', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should chain receipts with previousHash', async () => {
      // Arrange
      const op1 = createTestOperation();
      const op2 = createTestOperation();

      // Act
      const receipt1 = await generator.generateReceipt(op1);
      const receipt2 = await generator.generateReceipt(op2);

      // Assert
      expect(receipt1.previousHash).toBeNull();
      expect(receipt2.previousHash).toBe(receipt1.receiptHash);
    });

    it('should maintain chain consistency across 10 receipts', async () => {
      // Arrange
      const operations = createTestOperations(10);

      // Act
      const receipts = [];
      for (const op of operations) {
        const receipt = await generator.generateReceipt(op);
        receipts.push(receipt);
      }

      // Assert
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i].previousHash).toBe(receipts[i - 1].receiptHash);
      }
    });

    it('should update lastReceiptHash after each generation', async () => {
      // Arrange
      const op = createTestOperation();

      // Act
      const receipt = await generator.generateReceipt(op);

      // Assert
      expect(generator.lastReceiptHash).toBe(receipt.receiptHash);
    });
  });

  describe('batch proof generation', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator({ batchSize: 50 });
    });

    it('should generate batch proof from buffered receipts', async () => {
      // Arrange
      const operations = createTestOperations(50);
      for (const op of operations) {
        await generator.generateReceipt(op);
      }

      // Act
      const proof = await generator.generateBatchProof();

      // Assert
      expect(proof.batchId).toBeDefined();
      expect(proof.merkleRoot).toHaveLength(64);
      expect(proof.leafCount).toBe(50);
      expect(proof.receipts.length).toBe(50);
      expect(proof.batchNumber).toBe(0);
    });

    it('should generate multiple batch proofs sequentially', async () => {
      // Arrange
      const operations = createTestOperations(100);
      for (const op of operations) {
        await generator.generateReceipt(op);
      }

      // Act
      const proof1 = await generator.generateBatchProof(50);
      const proof2 = await generator.generateBatchProof(50);

      // Assert
      expect(proof1.batchNumber).toBe(0);
      expect(proof2.batchNumber).toBe(1);
      expect(proof1.batchId).not.toBe(proof2.batchId);
      expect(proof1.merkleRoot).not.toBe(proof2.merkleRoot);
    });

    it('should clear buffer after batch generation', async () => {
      // Arrange
      const operations = createTestOperations(30);
      for (const op of operations) {
        await generator.generateReceipt(op);
      }

      // Act
      await generator.generateBatchProof(20);

      // Assert
      expect(generator.operationBuffer.length).toBe(10);
    });

    it('should throw on empty buffer', async () => {
      // Act & Assert
      await expect(generator.generateBatchProof()).rejects.toThrow('operation buffer is empty');
    });

    it('should support partial batch from buffer', async () => {
      // Arrange
      const operations = createTestOperations(30);
      for (const op of operations) {
        await generator.generateReceipt(op);
      }

      // Act
      const proof = await generator.generateBatchProof(20);

      // Assert
      expect(proof.leafCount).toBe(20);
      expect(generator.operationBuffer.length).toBe(10);
    });
  });

  describe('merkle tree construction', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should build merkle tree with single receipt', async () => {
      // Arrange
      const operations = createTestOperations(1);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const tree = await generator.exportMerkleTree(receipts);

      // Assert
      expect(tree.root).toBeDefined();
      expect(tree.depth).toBe(0);
      expect(tree.leafCount).toBe(1);
      expect(tree.leaves.length).toBe(1);
    });

    it('should build merkle tree with multiple receipts', async () => {
      // Arrange
      const operations = createTestOperations(8);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const tree = await generator.exportMerkleTree(receipts);

      // Assert
      expect(tree.root).toBeDefined();
      expect(tree.depth).toBe(3);
      expect(tree.leafCount).toBe(8);
      expect(tree.leaves.length).toBe(8);
    });

    it('should maintain tree root consistency', async () => {
      // Arrange
      const operations = createTestOperations(16);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const tree1 = await generator.exportMerkleTree(receipts);
      const tree2 = await generator.exportMerkleTree(receipts);

      // Assert
      expect(tree1.root).toBe(tree2.root);
    });
  });

  describe('proof verification', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should verify valid merkle inclusion proof', async () => {
      // Arrange
      const operations = createTestOperations(8);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const proof = await generator.getReceiptProof(receipts[0].id, receipts);
      const isValid = await generator.verifyProof(proof);

      // Assert
      expect(isValid).toBe(true);
    });

    it('should verify inclusion proofs for all receipts in tree', async () => {
      // Arrange
      const operations = createTestOperations(16);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const results = await Promise.all(
        receipts.map(async r => {
          const proof = await generator.getReceiptProof(r.id, receipts);
          return generator.verifyProof(proof);
        })
      );

      // Assert
      expect(results.every(r => r === true)).toBe(true);
    });

    it('should reject invalid merkle proof (tampered hash)', async () => {
      // Arrange
      const operations = createTestOperations(8);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      const proof = await generator.getReceiptProof(receipts[0].id, receipts);
      const tamperedProof = {
        ...proof,
        leafHash: proof.leafHash.substring(0, 63) + 'f',
      };

      // Act
      const isValid = await generator.verifyProof(tamperedProof);

      // Assert
      expect(isValid).toBe(false);
    });

    it('should generate distinct proofs for different receipts', async () => {
      // Arrange
      const operations = createTestOperations(16);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const proof0 = await generator.getReceiptProof(receipts[0].id, receipts);
      const proof1 = await generator.getReceiptProof(receipts[1].id, receipts);

      // Assert
      expect(proof0.leafHash).not.toBe(proof1.leafHash);
      expect(proof0.leafIndex).not.toBe(proof1.leafIndex);
    });
  });

  describe('chain verification', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should verify valid receipt chain', async () => {
      // Arrange
      const operations = createTestOperations(10);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const result = await generator.verifyChain(receipts);

      // Assert
      expect(result.valid).toBe(true);
      expect(result.totalReceipts).toBe(10);
      expect(result.validReceipts).toBe(10);
      expect(result.tamperedReceipts.length).toBe(0);
      expect(result.chainLinksValid).toBe(true);
    });

    it('should detect tampering in chain', async () => {
      // Arrange
      const operations = createTestOperations(10);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Tamper with receipt 5
      receipts[5].previousHash = 'bad_hash_0123456789abcdef0123456789abcdef0123456789abcdef0123456789ab';

      // Act
      const result = await generator.verifyChain(receipts);

      // Assert
      expect(result.valid).toBe(false);
      expect(result.tamperedReceipts.length).toBeGreaterThan(0);
      expect(result.chainLinksValid).toBe(false);
    });

    it('should reject invalid genesis receipt', async () => {
      // Arrange
      const operations = createTestOperations(5);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Tamper with genesis
      receipts[0].previousHash = 'not_null_hash_0123456789abcdef0123456789abcdef0123456789abcdef';

      // Act
      const result = await generator.verifyChain(receipts);

      // Assert
      expect(result.valid).toBe(false);
      expect(result.tamperedReceipts[0].reason).toContain('Genesis');
    });

    it('should detect hash integrity violations', async () => {
      // Arrange
      const operations = createTestOperations(5);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Tamper with hash
      receipts[2].receiptHash = receipts[2].receiptHash.substring(0, 63) + 'f';

      // Act
      const result = await generator.verifyChain(receipts);

      // Assert
      expect(result.valid).toBe(false);
    });
  });

  describe('tamper detection', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should detect modified payload hash', async () => {
      // Arrange
      const operations = createTestOperations(5);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      receipts[2].payloadHash = receipts[2].payloadHash.substring(0, 63) + 'a';

      // Act
      const tampered = await generator.detectTampering(receipts);

      // Assert
      expect(tampered.length).toBeGreaterThan(0);
    });

    it('should identify specific tampered receipts', async () => {
      // Arrange
      const operations = createTestOperations(10);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Tamper with receipts at indices 3 and 7
      receipts[3].previousHash = 'tampered_hash_0123456789abcdef0123456789abcdef0123456789abcdef0';
      receipts[7].receiptHash = 'tampered_hash_0123456789abcdef0123456789abcdef0123456789abcdef1';

      // Act
      const tampered = await generator.detectTampering(receipts);

      // Assert
      expect(tampered.length).toBeGreaterThanOrEqual(2);
      expect(tampered.some(t => t.receiptId === receipts[3].id)).toBe(true);
    });
  });

  describe('performance', () => {
    it('should generate 100 receipts efficiently', async () => {
      // Arrange
      const generator = new DaemonReceiptGenerator({ batchSize: 100 });
      const operations = createTestOperations(100);

      // Act
      const startTime = Date.now();
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }
      const duration = Date.now() - startTime;

      // Assert
      expect(receipts.length).toBe(100);
      expect(duration).toBeLessThan(5000); // 5 second timeout
    });

    it('should generate batch proof for 100 receipts', async () => {
      // Arrange
      const generator = new DaemonReceiptGenerator({ batchSize: 100 });
      const operations = createTestOperations(100);
      for (const op of operations) {
        await generator.generateReceipt(op);
      }

      // Act
      const startTime = Date.now();
      const proof = await generator.generateBatchProof();
      const duration = Date.now() - startTime;

      // Assert
      expect(proof.leafCount).toBe(100);
      expect(duration).toBeLessThan(1000);
    });

    it('should verify chain of 100 receipts', async () => {
      // Arrange
      const generator = new DaemonReceiptGenerator();
      const operations = createTestOperations(100);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const startTime = Date.now();
      const result = await generator.verifyChain(receipts);
      const duration = Date.now() - startTime;

      // Assert
      expect(result.valid).toBe(true);
      expect(duration).toBeLessThan(2000);
    });
  });

  describe('statistics and exports', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator({ batchSize: 50 });
    });

    it('should track statistics', async () => {
      // Arrange
      const operations = createTestOperations(75);
      for (const op of operations) {
        await generator.generateReceipt(op);
      }
      await generator.generateBatchProof(50);

      // Act
      const stats = generator.getStatistics();

      // Assert
      expect(stats.totalReceiptsGenerated).toBe(75);
      expect(stats.bufferedOperations).toBe(25);
      expect(stats.totalBatchesGenerated).toBe(1);
      expect(stats.lastReceiptHash).toBeDefined();
    });

    it('should export merkle tree structure', async () => {
      // Arrange
      const operations = createTestOperations(16);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const tree = await generator.exportMerkleTree(receipts);

      // Assert
      expect(tree.root).toBeDefined();
      expect(tree.treeStructure).toBeDefined();
      expect(tree.treeStructure.levels).toHaveLength(5);
      expect(tree.treeStructure.levels[0].nodeCount).toBe(16);
    });

    it('should throw on export with empty receipts', async () => {
      // Act & Assert
      await expect(generator.exportMerkleTree([])).rejects.toThrow('empty');
    });
  });

  describe('security - CVE-2012-2459 mitigation', () => {
    let generator;

    beforeEach(() => {
      generator = new DaemonReceiptGenerator();
    });

    it('should prevent odd-leaf duplication attack (CVE-2012-2459)', async () => {
      // Arrange - Create 3 receipts (odd number for odd-leaf scenario)
      const operations = createTestOperations(3);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act - Build trees with original receipts and with duplicated last receipt
      const tree1 = await generator.exportMerkleTree(receipts);
      const tree2 = await generator.exportMerkleTree([...receipts, receipts[2]]);

      // Assert - Roots MUST be different (attack prevented)
      expect(tree1.root).not.toBe(tree2.root);
      expect(tree1.leafCount).toBe(3);
      expect(tree2.leafCount).toBe(4);
    });

    it('should handle odd-leaf scenario with 5 receipts', async () => {
      // Arrange
      const operations = createTestOperations(5);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const tree = await generator.exportMerkleTree(receipts);

      // Assert - Tree should be built successfully
      expect(tree.root).toBeDefined();
      expect(tree.leafCount).toBe(5);
      expect(tree.root).toHaveLength(64);
    });

    it('should produce different roots for odd and even receipt counts', async () => {
      // Arrange
      const operations = createTestOperations(7);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act
      const treeOdd = await generator.exportMerkleTree(receipts.slice(0, 7));
      const treeEven = await generator.exportMerkleTree(receipts.slice(0, 6));

      // Assert
      expect(treeOdd.root).not.toBe(treeEven.root);
      expect(treeOdd.leafCount).toBe(7);
      expect(treeEven.leafCount).toBe(6);
    });

    it('should verify proofs correctly for odd-leaf trees', async () => {
      // Arrange - 7 receipts (odd)
      const operations = createTestOperations(7);
      const receipts = [];
      for (const op of operations) {
        receipts.push(await generator.generateReceipt(op));
      }

      // Act - Get proof for last receipt (the odd leaf)
      const proof = await generator.getReceiptProof(receipts[6].id, receipts);
      const isValid = await generator.verifyProof(proof);

      // Assert
      expect(isValid).toBe(true);
      expect(proof.leafIndex).toBe(6);
    });
  });

  describe('integration scenarios', () => {
    it('should support complete workflow: generate -> batch -> verify', async () => {
      // Arrange
      const generator = new DaemonReceiptGenerator({ batchSize: 25 });
      const operations = createTestOperations(50);

      // Act - Generate receipts
      const allReceipts = [];
      for (const op of operations) {
        allReceipts.push(await generator.generateReceipt(op));
      }

      // Generate batches
      const batch1 = await generator.generateBatchProof(25);
      const batch2 = await generator.generateBatchProof(25);

      // Verify individual batch chains (first batch is genesis)
      const result1 = await generator.verifyChain(batch1.receipts);

      // Verify combined chain (batches linked together)
      const combinedReceipts = [...batch1.receipts, ...batch2.receipts];
      const resultCombined = await generator.verifyChain(combinedReceipts);

      // Assert
      expect(batch1.leafCount).toBe(25);
      expect(batch2.leafCount).toBe(25);
      expect(result1.valid).toBe(true);
      expect(resultCombined.valid).toBe(true);
    });

    it('should handle mixed operation types', async () => {
      // Arrange
      const generator = new DaemonReceiptGenerator();

      // Act
      const types = ['task_scheduled', 'task_executed', 'task_failed', 'state_change'];
      const receipts = [];

      for (let i = 0; i < 20; i++) {
        const op = createTestOperation({
          operationType: types[i % types.length],
        });
        receipts.push(await generator.generateReceipt(op));
      }

      // Verify
      const result = await generator.verifyChain(receipts);

      // Assert
      expect(result.valid).toBe(true);
      expect(receipts.length).toBe(20);
    });
  });
});
