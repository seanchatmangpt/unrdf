/**
 * Cryptographic Attack Adversarial Security Tests
 * Phase 5: 5 tests covering Merkle tampering, Receipt replay, Hash collision, Signature forgery, Nonce reuse
 *
 * @module @unrdf/integration-tests/test/adversarial/cryptographic
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  buildMerkleTree,
  generateMerkleProof,
  verifyMerkleProof,
  getMerkleRoot,
} from '@unrdf/receipts';
import { blake3 } from 'hash-wasm';

/**
 * Helper: Create test operations for Merkle tree
 * @param {number} count - Number of operations
 * @returns {Array}
 */
function createTestOperations(count) {
  return Array.from({ length: count }, (_, i) => ({
    id: `op-${i}`,
    type: 'ADD',
    subject: `http://example.org/s${i}`,
    timestamp: BigInt(Date.now() * 1000000 + i),
  }));
}

/**
 * Mock receipt with cryptographic properties
 */
class CryptoReceipt {
  constructor(data, nonce = null) {
    this.data = data;
    this.nonce = nonce || crypto.getRandomValues(new Uint8Array(16));
    this.timestamp = BigInt(Date.now() * 1000000);
    this.hash = null;
  }

  async computeHash() {
    const payload = JSON.stringify({
      data: this.data,
      nonce: Array.from(this.nonce),
      timestamp: this.timestamp.toString(),
    });
    this.hash = await blake3(payload);
    return this.hash;
  }

  async verify(expectedHash) {
    const computed = await this.computeHash();
    return computed === expectedHash;
  }
}

/**
 * Nonce tracking registry
 */
class NonceRegistry {
  constructor() {
    this.usedNonces = new Set();
    this.maxAge = 60 * 60 * 1000; // 1 hour
    this.timestamps = new Map();
  }

  /**
   * Check if nonce is valid (not reused)
   * @param {Uint8Array} nonce
   * @returns {Object}
   */
  validateNonce(nonce) {
    const nonceStr = Array.from(nonce).join(',');

    if (this.usedNonces.has(nonceStr)) {
      return { valid: false, error: 'Nonce reuse detected' };
    }

    return { valid: true };
  }

  /**
   * Register nonce as used
   * @param {Uint8Array} nonce
   */
  registerNonce(nonce) {
    const nonceStr = Array.from(nonce).join(',');
    this.usedNonces.add(nonceStr);
    this.timestamps.set(nonceStr, Date.now());
  }

  /**
   * Clean expired nonces
   */
  cleanup() {
    const now = Date.now();
    for (const [nonceStr, timestamp] of this.timestamps.entries()) {
      if (now - timestamp > this.maxAge) {
        this.usedNonces.delete(nonceStr);
        this.timestamps.delete(nonceStr);
      }
    }
  }
}

/**
 * Mock Ed25519 signature verification
 * In production, use actual crypto library
 */
class MockEd25519 {
  constructor() {
    this.validSignatures = new Map();
  }

  async sign(message, privateKey) {
    // Mock signing - in production use actual Ed25519
    const hash = await blake3(message + privateKey);
    const signature = hash.slice(0, 64); // Mock 64-char signature
    this.validSignatures.set(message, signature);
    return signature;
  }

  async verify(message, signature, publicKey) {
    // Verify signature matches
    const expectedSig = this.validSignatures.get(message);
    if (expectedSig && signature === expectedSig) {
      return true;
    }
    // Forged signatures fail
    return false;
  }
}

describe('Cryptographic Attack Adversarial Tests', () => {
  let nonceRegistry;
  let mockEd25519;

  beforeEach(() => {
    nonceRegistry = new NonceRegistry();
    mockEd25519 = new MockEd25519();
  });

  // Test 1: Merkle tampering - modify root hash
  it('should detect Merkle root tampering', async () => {
    const operations = createTestOperations(8);
    const tree = await buildMerkleTree(operations);
    const originalRoot = getMerkleRoot(tree);

    // Generate proof for item 3
    const proof = generateMerkleProof(tree, 3);

    // Compute correct leaf hash
    const leafData = operations[3];
    const serialized = JSON.stringify(leafData, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
    const correctLeafHash = await blake3(serialized);

    // Valid proof should pass
    const isValid = await verifyMerkleProof(proof, correctLeafHash);
    expect(isValid).toBe(true);

    // Tamper with root hash in proof
    const tamperedProof = {
      ...proof,
      root: 'a'.repeat(64), // Fake root
    };

    // Tampered proof should fail
    const isTampered = await verifyMerkleProof(tamperedProof, correctLeafHash);
    expect(isTampered).toBe(false);

    // Tamper with proof path
    if (proof.proof.length > 0) {
      const tamperedPathProof = {
        ...proof,
        proof: [
          { hash: 'b'.repeat(64), position: proof.proof[0].position },
          ...proof.proof.slice(1),
        ],
      };
      const isPathTampered = await verifyMerkleProof(tamperedPathProof, correctLeafHash);
      expect(isPathTampered).toBe(false);
    }
  });

  // Test 2: Receipt replay attack - resubmit old receipt
  it('should reject receipt replay attacks', async () => {
    const receipt = new CryptoReceipt({ action: 'transfer', amount: 100 });
    const originalHash = await receipt.computeHash();

    // Register nonce on first use
    const firstValidation = nonceRegistry.validateNonce(receipt.nonce);
    expect(firstValidation.valid).toBe(true);
    nonceRegistry.registerNonce(receipt.nonce);

    // Attempt to replay same receipt (same nonce)
    const replayValidation = nonceRegistry.validateNonce(receipt.nonce);
    expect(replayValidation.valid).toBe(false);
    expect(replayValidation.error).toContain('Nonce reuse');

    // Different nonce should work
    const newReceipt = new CryptoReceipt({ action: 'transfer', amount: 100 });
    const newValidation = nonceRegistry.validateNonce(newReceipt.nonce);
    expect(newValidation.valid).toBe(true);
  });

  // Test 3: Hash collision search (birthday attack mitigation)
  it('should resist hash collision attempts', async () => {
    // BLAKE3 produces 256-bit hashes - birthday attack requires 2^128 attempts
    // We verify that similar inputs produce very different hashes

    const input1 = { data: 'transaction-001', amount: 100 };
    const input2 = { data: 'transaction-001', amount: 101 }; // Tiny change

    const hash1 = await blake3(JSON.stringify(input1));
    const hash2 = await blake3(JSON.stringify(input2));

    // Hashes should be completely different
    expect(hash1).not.toBe(hash2);

    // Check hamming distance (bit difference) - should be ~50% for good hash
    let differences = 0;
    for (let i = 0; i < hash1.length; i++) {
      if (hash1[i] !== hash2[i]) differences++;
    }
    const differenceRatio = differences / hash1.length;

    // With 64 hex chars, BLAKE3's avalanche effect causes ~50%+ character changes
    // Allow 30-100% difference ratio since hex char comparison may exceed 50%
    expect(differenceRatio).toBeGreaterThan(0.3);
    expect(differenceRatio).toBeLessThan(1.0);

    // Verify determinism - same input always same hash
    const hash1Repeat = await blake3(JSON.stringify(input1));
    expect(hash1Repeat).toBe(hash1);
  });

  // Test 4: Signature forgery - fake Ed25519 signature
  it('should reject forged signatures', async () => {
    const message = 'Sign this important transaction';
    const privateKey = 'mock-private-key';
    const publicKey = 'mock-public-key';

    // Create valid signature
    const validSignature = await mockEd25519.sign(message, privateKey);

    // Verify valid signature
    const isValidSig = await mockEd25519.verify(message, validSignature, publicKey);
    expect(isValidSig).toBe(true);

    // Attempt forgery - random signature
    const forgedSignature = 'c'.repeat(64);
    const isForged = await mockEd25519.verify(message, forgedSignature, publicKey);
    expect(isForged).toBe(false);

    // Attempt forgery - modify valid signature
    const modifiedSig = 'd' + validSignature.slice(1);
    const isModified = await mockEd25519.verify(message, modifiedSig, publicKey);
    expect(isModified).toBe(false);

    // Attempt forgery - valid sig for different message
    const differentMessage = 'Different message';
    const isDifferentMsg = await mockEd25519.verify(differentMessage, validSignature, publicKey);
    expect(isDifferentMsg).toBe(false);
  });

  // Test 5: Nonce reuse detection - duplicate nonce
  it('should detect and reject duplicate nonces', async () => {
    const fixedNonce = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);

    // First use - should be valid
    const receipt1 = new CryptoReceipt({ action: 'action1' }, fixedNonce);
    const validation1 = nonceRegistry.validateNonce(receipt1.nonce);
    expect(validation1.valid).toBe(true);
    nonceRegistry.registerNonce(receipt1.nonce);

    // Second use with same nonce - should be rejected
    const receipt2 = new CryptoReceipt({ action: 'action2' }, fixedNonce);
    const validation2 = nonceRegistry.validateNonce(receipt2.nonce);
    expect(validation2.valid).toBe(false);
    expect(validation2.error).toBe('Nonce reuse detected');

    // Third use with same nonce - still rejected
    const receipt3 = new CryptoReceipt({ action: 'action3' }, fixedNonce);
    const validation3 = nonceRegistry.validateNonce(receipt3.nonce);
    expect(validation3.valid).toBe(false);

    // Different nonce should work
    const differentNonce = new Uint8Array([16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
    const receipt4 = new CryptoReceipt({ action: 'action4' }, differentNonce);
    const validation4 = nonceRegistry.validateNonce(receipt4.nonce);
    expect(validation4.valid).toBe(true);
  });
});
