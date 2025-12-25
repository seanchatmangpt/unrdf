/**
 * @file Blockchain Receipt Performance Benchmarks
 * @module benchmarks/advanced/blockchain-receipt-benchmark
 *
 * @description
 * Benchmarks for cryptographic receipt operations:
 * - Signature generation speed
 * - Verification throughput
 * - Merkle tree construction time
 * - Receipt storage overhead
 */

import { suite, randomString } from '../framework.mjs';
import { createHash, randomBytes } from 'node:crypto';

// =============================================================================
// Simple Cryptographic Receipt Implementation
// =============================================================================

class CryptoReceipt {
  constructor(data) {
    this.data = data;
    this.timestamp = Date.now();
    this.nonce = randomBytes(16).toString('hex');
  }

  /**
   * Generate hash of receipt
   * @returns {string} Hash
   */
  hash() {
    const content = JSON.stringify({
      data: this.data,
      timestamp: this.timestamp,
      nonce: this.nonce
    });

    return createHash('sha256').update(content).digest('hex');
  }

  /**
   * Generate signature (mock)
   * @returns {string} Signature
   */
  sign() {
    const hash = this.hash();
    // In real implementation, would use private key signing
    return createHash('sha256').update(hash + this.nonce).digest('hex');
  }

  /**
   * Verify signature (mock)
   * @param {string} signature - Signature to verify
   * @returns {boolean} Valid
   */
  verify(signature) {
    const expectedSignature = this.sign();
    return signature === expectedSignature;
  }

  /**
   * Serialize to JSON
   * @returns {string} JSON string
   */
  toJSON() {
    return JSON.stringify({
      data: this.data,
      timestamp: this.timestamp,
      nonce: this.nonce,
      hash: this.hash(),
      signature: this.sign()
    });
  }
}

/**
 * Simple Merkle tree implementation
 */
class MerkleTree {
  constructor(leaves) {
    this.leaves = leaves.map(leaf => this.hashLeaf(leaf));
    this.root = this.buildTree(this.leaves);
  }

  hashLeaf(data) {
    return createHash('sha256').update(JSON.stringify(data)).digest('hex');
  }

  hashPair(left, right) {
    return createHash('sha256').update(left + right).digest('hex');
  }

  buildTree(nodes) {
    if (nodes.length === 0) return null;
    if (nodes.length === 1) return nodes[0];

    const parents = [];
    for (let i = 0; i < nodes.length; i += 2) {
      const left = nodes[i];
      const right = i + 1 < nodes.length ? nodes[i + 1] : left;
      parents.push(this.hashPair(left, right));
    }

    return this.buildTree(parents);
  }

  getRoot() {
    return this.root;
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate receipt data
 * @returns {object} Receipt data
 */
function generateReceiptData() {
  return {
    transactionId: randomString(16),
    workflowId: randomString(8),
    caseId: randomString(8),
    taskId: randomString(8),
    status: 'completed',
    timestamp: Date.now(),
    data: {
      result: randomString(32),
      metadata: {
        duration: Math.random() * 1000,
        resourceUsage: Math.random() * 100
      }
    }
  };
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const blockchainReceiptBenchmarks = suite('Blockchain Receipt Performance', {
  'create receipt': {
    fn: () => {
      const data = generateReceiptData();
      return new CryptoReceipt(data);
    },
    iterations: 10000,
    warmup: 1000
  },

  'generate hash': {
    setup: () => {
      const data = generateReceiptData();
      const receipt = new CryptoReceipt(data);
      return { receipt };
    },
    fn: function() {
      return this.receipt.hash();
    },
    iterations: 50000,
    warmup: 5000
  },

  'generate signature': {
    setup: () => {
      const data = generateReceiptData();
      const receipt = new CryptoReceipt(data);
      return { receipt };
    },
    fn: function() {
      return this.receipt.sign();
    },
    iterations: 20000,
    warmup: 2000
  },

  'verify signature': {
    setup: () => {
      const data = generateReceiptData();
      const receipt = new CryptoReceipt(data);
      const signature = receipt.sign();
      return { receipt, signature };
    },
    fn: function() {
      return this.receipt.verify(this.signature);
    },
    iterations: 20000,
    warmup: 2000
  },

  'serialize receipt': {
    setup: () => {
      const data = generateReceiptData();
      const receipt = new CryptoReceipt(data);
      return { receipt };
    },
    fn: function() {
      return this.receipt.toJSON();
    },
    iterations: 10000,
    warmup: 1000
  },

  'create and sign receipt': {
    fn: () => {
      const data = generateReceiptData();
      const receipt = new CryptoReceipt(data);
      const signature = receipt.sign();
      return { receipt, signature };
    },
    iterations: 10000,
    warmup: 1000
  },

  'build merkle tree (10 receipts)': {
    fn: () => {
      const receipts = [];
      for (let i = 0; i < 10; i++) {
        receipts.push(generateReceiptData());
      }
      return new MerkleTree(receipts);
    },
    iterations: 5000,
    warmup: 500
  },

  'build merkle tree (100 receipts)': {
    fn: () => {
      const receipts = [];
      for (let i = 0; i < 100; i++) {
        receipts.push(generateReceiptData());
      }
      return new MerkleTree(receipts);
    },
    iterations: 1000,
    warmup: 100
  },

  'build merkle tree (1000 receipts)': {
    fn: () => {
      const receipts = [];
      for (let i = 0; i < 1000; i++) {
        receipts.push(generateReceiptData());
      }
      return new MerkleTree(receipts);
    },
    iterations: 100,
    warmup: 10
  },

  'batch receipt creation (100)': {
    fn: () => {
      const receipts = [];
      for (let i = 0; i < 100; i++) {
        const data = generateReceiptData();
        const receipt = new CryptoReceipt(data);
        receipt.sign();
        receipts.push(receipt);
      }
      return receipts;
    },
    iterations: 1000,
    warmup: 100
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await blockchainReceiptBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
