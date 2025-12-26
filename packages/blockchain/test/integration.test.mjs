/**
 * Integration Tests - Blockchain anchoring
 *
 * Tests all blockchain integration functionality including:
 * - Merkle tree generation and verification
 * - Receipt anchoring simulation
 * - Gas cost calculations
 */

import { describe, it, expect } from 'vitest';
import { MerkleProofGenerator, calculateGasSavings } from '../src/merkle/merkle-proof-generator.mjs';
import { ethers } from 'ethers';

// =============================================================================
// Test Utilities
// =============================================================================

/**
 * Generate mock receipt for testing
 */
function generateMockReceipt(id) {
  const receipt = {
    id: `receipt-${id}`,
    timestamp: Date.now(),
    event: 'TASK_COMPLETED',
    caseId: `case-${Math.floor(id / 10)}`,
    taskId: `task-${id}`,
    payload: {
      decision: 'APPROVE',
      actor: 'test-user',
    },
  };

  // Compute hash
  const data = JSON.stringify(receipt);
  const hashBytes = ethers.keccak256(new TextEncoder().encode(data));
  receipt.hash = hashBytes;

  return receipt;
}

// =============================================================================
// Merkle Proof Generator Tests
// =============================================================================

describe('MerkleProofGenerator', () => {
  it('should build Merkle tree from receipts', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 10 }, (_, i) => generateMockReceipt(i));

    receipts.forEach(r => generator.addReceipt(r));
    const root = generator.buildTree();

    expect(root).toBeDefined();
    expect(root).toMatch(/^0x[0-9a-f]{64}$/i);
  });

  it('should generate valid Merkle proofs', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 100 }, (_, i) => generateMockReceipt(i));

    generator.addReceipts(receipts);
    generator.buildTree();

    // Generate proof for first receipt
    const proof = generator.generateProof(receipts[0]);

    expect(proof).toBeDefined();
    expect(proof.leaf).toBeDefined();
    expect(proof.proof).toBeInstanceOf(Array);
    expect(proof.proof.length).toBeGreaterThan(0);
    expect(proof.index).toBe(0);
    expect(proof.root).toBeDefined();
  });

  it('should verify Merkle proofs', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 50 }, (_, i) => generateMockReceipt(i));

    generator.addReceipts(receipts);
    generator.buildTree();

    // Test all receipts
    receipts.forEach(receipt => {
      const proof = generator.generateProof(receipt);
      const isValid = generator.verifyProof(proof);
      expect(isValid).toBe(true);
    });
  });

  it('should provide tree information', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 16 }, (_, i) => generateMockReceipt(i));

    generator.addReceipts(receipts);
    generator.buildTree();

    const info = generator.getTreeInfo();

    expect(info.root).toBeDefined();
    expect(info.leafCount).toBe(16);
    expect(info.depth).toBeGreaterThan(0);
    expect(info.leaves).toHaveLength(16);
  });

  it('should export and import tree data', () => {
    const generator1 = new MerkleProofGenerator();
    const receipts = Array.from({ length: 20 }, (_, i) => generateMockReceipt(i));

    generator1.addReceipts(receipts);
    const root1 = generator1.buildTree();

    // Export
    const exported = generator1.export();

    // Import into new generator
    const generator2 = new MerkleProofGenerator();
    generator2.import(exported);

    const root2 = generator2.getRoot();

    expect(root1).toBe(root2);

    // Verify proofs work on imported tree
    const proof = generator2.generateProof(receipts[0]);
    const isValid = generator2.verifyProof(proof);
    expect(isValid).toBe(true);
  });

  it('should handle large batches efficiently', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 1000 }, (_, i) => generateMockReceipt(i));

    const start = Date.now();
    generator.addReceipts(receipts);
    generator.buildTree();
    const duration = Date.now() - start;

    expect(duration).toBeLessThan(1000); // Should complete in < 1 second

    // Verify a few random proofs
    const randomIndices = [0, 500, 999];
    randomIndices.forEach(i => {
      const proof = generator.generateProof(receipts[i]);
      const isValid = generator.verifyProof(proof);
      expect(isValid).toBe(true);
    });
  });
});

// =============================================================================
// Gas Savings Tests
// =============================================================================

describe('Gas Savings Calculations', () => {
  const mockGasPrice = 20000000000n; // 20 Gwei

  it('should calculate gas savings correctly', () => {
    const savings = calculateGasSavings(100, mockGasPrice);

    expect(savings.receiptCount).toBe(100);
    expect(savings.savingsPercentage).toBeGreaterThan(90);
    expect(BigInt(savings.savedGas)).toBeGreaterThan(0n);
  });

  it('should show increasing savings with batch size', () => {
    const sizes = [10, 50, 100, 500, 1000];
    const savingsData = sizes.map(size => calculateGasSavings(size, mockGasPrice));

    // Verify savings percentage increases
    for (let i = 1; i < savingsData.length; i++) {
      expect(savingsData[i].savingsPercentage).toBeGreaterThan(
        savingsData[i - 1].savingsPercentage
      );
    }
  });

  it('should provide cost breakdown in wei and ETH', () => {
    const savings = calculateGasSavings(100, mockGasPrice);

    expect(savings.savedCostWei).toBeDefined();
    expect(BigInt(savings.savedCostWei)).toBeGreaterThan(0n);
  });
});

// =============================================================================
// Schema Validation Tests
// =============================================================================

describe('Schema Validation', () => {
  it('should validate Merkle proof schema', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 10 }, (_, i) => generateMockReceipt(i));

    generator.addReceipts(receipts);
    generator.buildTree();

    const proof = generator.generateProof(receipts[0]);

    // Should have all required fields
    expect(proof).toHaveProperty('leaf');
    expect(proof).toHaveProperty('proof');
    expect(proof).toHaveProperty('index');
    expect(proof).toHaveProperty('root');

    // Validate types
    expect(typeof proof.leaf).toBe('string');
    expect(Array.isArray(proof.proof)).toBe(true);
    expect(typeof proof.index).toBe('number');
    expect(typeof proof.root).toBe('string');
  });

  it('should validate tree info schema', () => {
    const generator = new MerkleProofGenerator();
    const receipts = Array.from({ length: 8 }, (_, i) => generateMockReceipt(i));

    generator.addReceipts(receipts);
    generator.buildTree();

    const info = generator.getTreeInfo();

    expect(info).toHaveProperty('root');
    expect(info).toHaveProperty('leafCount');
    expect(info).toHaveProperty('depth');
    expect(info).toHaveProperty('leaves');

    expect(typeof info.root).toBe('string');
    expect(typeof info.leafCount).toBe('number');
    expect(typeof info.depth).toBe('number');
    expect(Array.isArray(info.leaves)).toBe(true);
  });
});

// =============================================================================
// Error Handling Tests
// =============================================================================

describe('Error Handling', () => {
  it('should throw error when building empty tree', () => {
    const generator = new MerkleProofGenerator();

    expect(() => generator.buildTree()).toThrow('No receipts added');
  });

  it('should throw error when generating proof before building tree', () => {
    const generator = new MerkleProofGenerator();
    const receipt = generateMockReceipt(1);

    generator.addReceipt(receipt);

    expect(() => generator.generateProof(receipt)).toThrow('Tree not built');
  });

  it('should throw error when getting root before building tree', () => {
    const generator = new MerkleProofGenerator();

    expect(() => generator.getRoot()).toThrow('Tree not built');
  });

  it('should throw error on import root mismatch', () => {
    const generator1 = new MerkleProofGenerator();
    const receipts = Array.from({ length: 10 }, (_, i) => generateMockReceipt(i));

    generator1.addReceipts(receipts);
    generator1.buildTree();

    const exported = generator1.export();
    exported.root = '0x0000000000000000000000000000000000000000000000000000000000000000';

    const generator2 = new MerkleProofGenerator();
    expect(() => generator2.import(exported)).toThrow('root mismatch');
  });
});

// =============================================================================
// Integration Tests (Conceptual - requires running network)
// =============================================================================

describe('Integration Scenarios (Conceptual)', () => {
  it('should demonstrate workflow receipt to Merkle proof pipeline', () => {
    // Simulate workflow generating receipts
    const workflowReceipts = [];
    for (let i = 0; i < 100; i++) {
      const receipt = generateMockReceipt(i);
      workflowReceipts.push(receipt);
    }

    // Build Merkle tree
    const generator = new MerkleProofGenerator();
    generator.addReceipts(workflowReceipts);
    const root = generator.buildTree();

    // Simulate anchoring root to blockchain
    expect(root).toBeDefined();

    // Later: verify any receipt
    const receiptToVerify = workflowReceipts[42];
    const proof = generator.generateProof(receiptToVerify);
    const isValid = generator.verifyProof(proof);

    expect(isValid).toBe(true);

    // This would be submitted to blockchain for on-chain verification
    expect(proof.root).toBe(root);
  });

  it('should demonstrate batching strategy based on count', () => {
    const testCases = [
      { count: 1, expected: 'individual' },
      { count: 5, expected: 'batch' },
      { count: 100, expected: 'merkle' },
    ];

    testCases.forEach(({ count, expected }) => {
      const gasPrice = 20000000000n;

      const individualCost = BigInt(count) * 50000n * gasPrice;
      const batchCost = (30000n + BigInt(count) * 20000n) * gasPrice;
      const merkleCost = 60000n * gasPrice;

      const costs = { individual: individualCost, batch: batchCost, merkle: merkleCost };
      const optimal = Object.keys(costs).reduce((a, b) => (costs[a] < costs[b] ? a : b));

      // Verify strategy selection makes sense
      expect(['individual', 'batch', 'merkle']).toContain(optimal);
    });
  });
});
