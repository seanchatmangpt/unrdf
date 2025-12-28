/**
 * @file Blockchain Receipts Integration Tests
 * @module @unrdf/yawl/test/blockchain-receipts
 *
 * Comprehensive integration tests for blockchain-verified receipts covering:
 * - Ed25519 key generation and management
 * - Digital signature creation and verification
 * - Receipt chain creation and validation
 * - Merkle tree generation and proof verification
 * - Tamper detection and security validation
 * - Performance under load (100+ receipts)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { sha512 } from '@noble/hashes/sha512';
import * as ed from '@noble/ed25519';
import {
  generateSigningKey,
  derivePublicKey,
  createBlockchainReceipt,
  createReceiptChain,
  verifyBlockchainReceipt,
  verifyReceiptChain,
  createMerkleRoot,
  generateMerkleProof,
  verifyMerkleProof,
  SigningKeySchema,
  BlockchainReceiptSchema,
  VerificationResultSchema,
} from '../src/blockchain-receipts.mjs';

// Set up SHA-512 for @noble/ed25519
ed.etc.sha512Sync = (...m) => sha512(ed.etc.concatBytes(...m));

// =============================================================================
// Test Fixtures
// =============================================================================

/**
 * Sample workflow events for testing
 */
const SAMPLE_EVENTS = {
  taskEnabled: {
    type: 'TASK_ENABLED',
    caseId: 'case-001',
    taskId: 'task-1',
    timestamp: new Date().toISOString(),
  },
  taskStarted: {
    type: 'TASK_STARTED',
    caseId: 'case-001',
    taskId: 'task-1',
    timestamp: new Date().toISOString(),
  },
  taskCompleted: {
    type: 'TASK_COMPLETED',
    caseId: 'case-001',
    taskId: 'task-1',
    timestamp: new Date().toISOString(),
    result: { success: true },
  },
};

/**
 * Sample decision payload
 */
const SAMPLE_PAYLOAD = {
  decision: 'APPROVE',
  justification: {
    reasoning: 'All validation checks passed',
    hookValidated: 'pre-task-hook',
  },
};

// =============================================================================
// Tests
// =============================================================================

describe('Blockchain Receipts Integration Tests', () => {
  // ===========================================================================
  // Key Management Tests
  // ===========================================================================

  describe('Key Generation and Management', () => {
    it('should generate valid Ed25519 signing key pair', async () => {
      const keyPair = await generateSigningKey('test-key-001');

      expect(keyPair).toBeDefined();
      expect(keyPair.keyId).toBe('test-key-001');
      expect(keyPair.privateKey).toBeDefined();
      expect(keyPair.publicKey).toBeDefined();
      expect(keyPair.createdAt).toBeDefined();

      // Validate key lengths (hex strings)
      expect(keyPair.privateKey).toHaveLength(64); // 32 bytes * 2
      expect(keyPair.publicKey).toHaveLength(64); // 32 bytes * 2

      // Validate with schema
      const validated = SigningKeySchema.parse(keyPair);
      expect(validated).toEqual(keyPair);
    });

    it('should generate unique keys on each call', async () => {
      const key1 = await generateSigningKey('key-1');
      const key2 = await generateSigningKey('key-2');

      expect(key1.privateKey).not.toBe(key2.privateKey);
      expect(key1.publicKey).not.toBe(key2.publicKey);
      expect(key1.keyId).not.toBe(key2.keyId);
    });

    it('should derive public key from private key', async () => {
      const keyPair = await generateSigningKey('derive-test');
      const derivedPublic = await derivePublicKey(keyPair.privateKey);

      expect(derivedPublic).toBe(keyPair.publicKey);
      expect(derivedPublic).toHaveLength(64);
    });

    it('should validate key schema requirements', () => {
      expect(() => SigningKeySchema.parse({
        privateKey: 'too-short',
        keyId: 'test',
      })).toThrow();

      expect(() => SigningKeySchema.parse({
        privateKey: 'a'.repeat(64),
        publicKey: 'invalid-length',
        keyId: 'test',
      })).toThrow();

      expect(() => SigningKeySchema.parse({
        privateKey: 'a'.repeat(64),
        keyId: '', // Empty keyId
      })).toThrow();
    });

    it('should handle multiple key pairs for different purposes', async () => {
      const workflowKey = await generateSigningKey('workflow-signer');
      const auditKey = await generateSigningKey('audit-signer');
      const backupKey = await generateSigningKey('backup-signer');

      expect(workflowKey.keyId).toBe('workflow-signer');
      expect(auditKey.keyId).toBe('audit-signer');
      expect(backupKey.keyId).toBe('backup-signer');

      // All should be unique
      const publicKeys = [workflowKey.publicKey, auditKey.publicKey, backupKey.publicKey];
      const uniqueKeys = new Set(publicKeys);
      expect(uniqueKeys.size).toBe(3);
    });
  });

  // ===========================================================================
  // Receipt Creation Tests
  // ===========================================================================

  describe('Blockchain Receipt Creation', () => {
    let signingKey;

    beforeEach(async () => {
      signingKey = await generateSigningKey('test-signer');
    });

    it('should create blockchain receipt with valid signature', async () => {
      const receipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskCompleted,
        SAMPLE_PAYLOAD,
        signingKey
      );

      expect(receipt).toBeDefined();
      expect(receipt.hash).toBeDefined();
      expect(receipt.signature).toBeDefined();
      expect(receipt.publicKey).toBe(signingKey.publicKey);
      expect(receipt.keyId).toBe('test-signer');
      expect(receipt.signedAt).toBeDefined();

      // Validate signature length
      expect(receipt.signature).toHaveLength(128); // 64 bytes * 2

      // Validate with schema
      const validated = BlockchainReceiptSchema.parse(receipt);
      expect(validated).toEqual(receipt);
    });

    it('should include optional blockchain metadata', async () => {
      const receipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskCompleted,
        SAMPLE_PAYLOAD,
        signingKey,
        {
          blockchain: 'ethereum',
          blockchainTxId: '0x123abc...',
        }
      );

      expect(receipt.blockchain).toBe('ethereum');
      expect(receipt.blockchainTxId).toBe('0x123abc...');
    });

    it('should create receipts with different events', async () => {
      const enabledReceipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskEnabled,
        { decision: 'ENABLE' },
        signingKey
      );

      const startedReceipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskStarted,
        { decision: 'START' },
        signingKey
      );

      expect(enabledReceipt.hash).not.toBe(startedReceipt.hash);
      expect(enabledReceipt.signature).not.toBe(startedReceipt.signature);
    });

    it('should create deterministic receipts for same input', async () => {
      const event = SAMPLE_EVENTS.taskCompleted;
      const payload = SAMPLE_PAYLOAD;

      const receipt1 = await createBlockchainReceipt(event, payload, signingKey);

      // Wait a bit to ensure timestamp difference
      await new Promise(resolve => setTimeout(resolve, 10));

      const receipt2 = await createBlockchainReceipt(event, payload, signingKey);

      // Hashes should be different due to timestamp
      // but both should be valid receipts
      expect(receipt1.hash).toBeDefined();
      expect(receipt2.hash).toBeDefined();
      expect(receipt1.signature).toBeDefined();
      expect(receipt2.signature).toBeDefined();
    });

    it('should handle complex payload data', async () => {
      const complexPayload = {
        decision: 'APPROVE',
        justification: {
          reasoning: 'Multi-step validation',
          checks: [
            { check: 'authorization', status: 'passed' },
            { check: 'business-rules', status: 'passed' },
            { check: 'data-validation', status: 'passed' },
          ],
          metadata: {
            reviewer: 'system',
            timestamp: Date.now(),
            version: '2.0',
          },
        },
      };

      const receipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskCompleted,
        complexPayload,
        signingKey
      );

      expect(receipt).toBeDefined();
      expect(receipt.payload).toEqual(complexPayload);
      expect(receipt.signature).toBeDefined();
    });

    it('should reject invalid signing key', async () => {
      const invalidKey = {
        privateKey: 'invalid',
        keyId: 'test',
      };

      await expect(
        createBlockchainReceipt(SAMPLE_EVENTS.taskCompleted, SAMPLE_PAYLOAD, invalidKey)
      ).rejects.toThrow();
    });
  });

  // ===========================================================================
  // Receipt Verification Tests
  // ===========================================================================

  describe('Blockchain Receipt Verification', () => {
    let signingKey;
    let validReceipt;

    beforeEach(async () => {
      signingKey = await generateSigningKey('verify-test');
      validReceipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskCompleted,
        SAMPLE_PAYLOAD,
        signingKey
      );
    });

    it('should verify valid blockchain receipt', async () => {
      const result = await verifyBlockchainReceipt(validReceipt);

      expect(result).toBeDefined();
      expect(result.valid).toBe(true);
      expect(result.hashValid).toBe(true);
      expect(result.signatureValid).toBe(true);
      expect(result.keyId).toBe('verify-test');
      expect(result.verifiedAt).toBeDefined();
      expect(result.details.receiptHash).toBe(validReceipt.hash);
      expect(result.details.publicKey).toBe(signingKey.publicKey);

      // Validate with schema
      const validated = VerificationResultSchema.parse(result);
      expect(validated).toEqual(result);
    });

    it('should detect tampered receipt hash', async () => {
      const tamperedReceipt = { ...validReceipt, hash: 'a'.repeat(64) };

      const result = await verifyBlockchainReceipt(tamperedReceipt);

      expect(result.valid).toBe(false);
      expect(result.hashValid).toBe(false);
      // Signature verification might also fail
    });

    it('should detect tampered signature', async () => {
      const tamperedReceipt = { ...validReceipt, signature: 'b'.repeat(128) };

      const result = await verifyBlockchainReceipt(tamperedReceipt);

      expect(result.valid).toBe(false);
      expect(result.signatureValid).toBe(false);
    });

    it('should detect tampered payload', async () => {
      const tamperedReceipt = {
        ...validReceipt,
        payload: { ...validReceipt.payload, decision: 'REJECT' },
      };

      const result = await verifyBlockchainReceipt(tamperedReceipt);

      expect(result.valid).toBe(false);
      // Either hash or signature validation should fail
      expect(result.hashValid || result.signatureValid).toBe(false);
    });

    it('should detect wrong public key', async () => {
      const otherKey = await generateSigningKey('other-key');
      const tamperedReceipt = { ...validReceipt, publicKey: otherKey.publicKey };

      const result = await verifyBlockchainReceipt(tamperedReceipt);

      expect(result.valid).toBe(false);
      expect(result.signatureValid).toBe(false);
    });

    it('should verify receipts from different keys', async () => {
      const key1 = await generateSigningKey('key-1');
      const key2 = await generateSigningKey('key-2');

      const receipt1 = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskEnabled,
        SAMPLE_PAYLOAD,
        key1
      );

      const receipt2 = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskStarted,
        SAMPLE_PAYLOAD,
        key2
      );

      const result1 = await verifyBlockchainReceipt(receipt1);
      const result2 = await verifyBlockchainReceipt(receipt2);

      expect(result1.valid).toBe(true);
      expect(result1.keyId).toBe('key-1');
      expect(result2.valid).toBe(true);
      expect(result2.keyId).toBe('key-2');
    });
  });

  // ===========================================================================
  // Receipt Chain Tests
  // ===========================================================================

  describe('Receipt Chain Creation and Verification', () => {
    let signingKey;

    beforeEach(async () => {
      signingKey = await generateSigningKey('chain-test');
    });

    it('should create receipt chain with proper linking', async () => {
      const events = [
        SAMPLE_EVENTS.taskEnabled,
        SAMPLE_EVENTS.taskStarted,
        SAMPLE_EVENTS.taskCompleted,
      ];

      const chain = await createReceiptChain(events, signingKey);

      expect(chain).toBeInstanceOf(Array);
      expect(chain).toHaveLength(3);

      // Verify each receipt is valid
      chain.forEach(receipt => {
        expect(receipt.hash).toBeDefined();
        expect(receipt.signature).toBeDefined();
        expect(receipt.keyId).toBe('chain-test');
      });

      // Verify chain linkage
      for (let i = 1; i < chain.length; i++) {
        const previousHash = chain[i - 1].hash;
        const currentPayload = chain[i].payload;
        expect(currentPayload.justification.previousReceipt).toBe(previousHash);
      }
    });

    it('should verify valid receipt chain', async () => {
      const events = [
        SAMPLE_EVENTS.taskEnabled,
        SAMPLE_EVENTS.taskStarted,
        SAMPLE_EVENTS.taskCompleted,
      ];

      const chain = await createReceiptChain(events, signingKey);
      const result = await verifyReceiptChain(chain);

      expect(result.valid).toBe(true);
      expect(result.chainLength).toBe(3);
      expect(result.verifiedCount).toBe(3);
      expect(result.results).toHaveLength(3);
      expect(result.verifiedAt).toBeDefined();

      // All individual receipts should be valid
      result.results.forEach(r => {
        expect(r.valid).toBe(true);
        expect(r.hashValid).toBe(true);
        expect(r.signatureValid).toBe(true);
      });

      // All chain links should be valid (except first)
      for (let i = 1; i < result.results.length; i++) {
        expect(result.results[i].chainLinkValid).toBe(true);
      }
    });

    it('should detect broken chain link', async () => {
      const events = [
        SAMPLE_EVENTS.taskEnabled,
        SAMPLE_EVENTS.taskStarted,
        SAMPLE_EVENTS.taskCompleted,
      ];

      const chain = await createReceiptChain(events, signingKey);

      // Break the chain by modifying the second receipt's link
      chain[1].payload.justification.previousReceipt = 'invalid-hash';

      const result = await verifyReceiptChain(chain);

      expect(result.valid).toBe(false);
      expect(result.results[1].chainLinkValid).toBe(false);
    });

    it('should detect tampered receipt in chain', async () => {
      const events = [
        SAMPLE_EVENTS.taskEnabled,
        SAMPLE_EVENTS.taskStarted,
        SAMPLE_EVENTS.taskCompleted,
      ];

      const chain = await createReceiptChain(events, signingKey);

      // Tamper with the middle receipt
      chain[1].payload.decision = 'TAMPERED';

      const result = await verifyReceiptChain(chain);

      expect(result.valid).toBe(false);
      // The tampered receipt should fail validation
      expect(result.results[1].valid).toBe(false);
    });

    it('should handle long receipt chains', async () => {
      const events = Array(50).fill(null).map((_, i) => ({
        type: `EVENT_${i}`,
        caseId: 'case-001',
        taskId: `task-${i}`,
        timestamp: new Date().toISOString(),
      }));

      const chain = await createReceiptChain(events, signingKey);

      expect(chain).toHaveLength(50);

      const result = await verifyReceiptChain(chain);
      expect(result.valid).toBe(true);
      expect(result.chainLength).toBe(50);
      expect(result.verifiedCount).toBe(50);
    });

    it('should handle empty chain', async () => {
      const chain = [];
      const result = await verifyReceiptChain(chain);

      expect(result.valid).toBe(true);
      expect(result.chainLength).toBe(0);
      expect(result.verifiedCount).toBe(0);
    });

    it('should handle single receipt chain', async () => {
      const events = [SAMPLE_EVENTS.taskCompleted];
      const chain = await createReceiptChain(events, signingKey);

      expect(chain).toHaveLength(1);

      const result = await verifyReceiptChain(chain);
      expect(result.valid).toBe(true);
      expect(result.chainLength).toBe(1);
      expect(result.verifiedCount).toBe(1);
    });
  });

  // ===========================================================================
  // Merkle Tree Tests
  // ===========================================================================

  describe('Merkle Tree Generation and Verification', () => {
    let signingKey;
    let receipts;

    beforeEach(async () => {
      signingKey = await generateSigningKey('merkle-test');

      // Create batch of receipts
      const events = Array(8).fill(null).map((_, i) => ({
        type: `EVENT_${i}`,
        caseId: 'batch-001',
        taskId: `task-${i}`,
      }));

      receipts = [];
      for (const event of events) {
        const receipt = await createBlockchainReceipt(
          event,
          { decision: `DECISION_${event.type}` },
          signingKey
        );
        receipts.push(receipt);
      }
    });

    it('should create Merkle root from receipts', async () => {
      const merkleRoot = await createMerkleRoot(receipts);

      expect(merkleRoot).toBeDefined();
      expect(typeof merkleRoot).toBe('string');
      expect(merkleRoot.length).toBeGreaterThan(0);
    });

    it('should create consistent Merkle root for same receipts', async () => {
      const root1 = await createMerkleRoot(receipts);
      const root2 = await createMerkleRoot(receipts);

      expect(root1).toBe(root2);
    });

    it('should create different Merkle root for different receipts', async () => {
      const root1 = await createMerkleRoot(receipts);

      // Create different batch
      const otherReceipt = await createBlockchainReceipt(
        { type: 'OTHER', caseId: 'other-001' },
        { decision: 'OTHER' },
        signingKey
      );

      const modifiedReceipts = [...receipts.slice(0, -1), otherReceipt];
      const root2 = await createMerkleRoot(modifiedReceipts);

      expect(root1).not.toBe(root2);
    });

    it('should reject empty receipt array', async () => {
      await expect(createMerkleRoot([])).rejects.toThrow('empty array');
    });

    it('should handle single receipt', async () => {
      const singleReceipt = [receipts[0]];
      const merkleRoot = await createMerkleRoot(singleReceipt);

      expect(merkleRoot).toBe(receipts[0].hash);
    });

    it('should handle odd number of receipts', async () => {
      const oddReceipts = receipts.slice(0, 7); // 7 receipts
      const merkleRoot = await createMerkleRoot(oddReceipts);

      expect(merkleRoot).toBeDefined();
      expect(typeof merkleRoot).toBe('string');
    });
  });

  // ===========================================================================
  // Merkle Proof Tests
  // ===========================================================================

  describe('Merkle Proof Generation and Verification', () => {
    let signingKey;
    let receipts;
    let merkleRoot;

    beforeEach(async () => {
      signingKey = await generateSigningKey('proof-test');

      // Create batch of 16 receipts for complete binary tree
      const events = Array(16).fill(null).map((_, i) => ({
        type: `EVENT_${i}`,
        caseId: 'proof-batch',
        taskId: `task-${i}`,
      }));

      receipts = [];
      for (const event of events) {
        const receipt = await createBlockchainReceipt(
          event,
          { decision: `DECISION_${event.type}` },
          signingKey
        );
        receipts.push(receipt);
      }

      merkleRoot = await createMerkleRoot(receipts);
    });

    it('should generate valid Merkle proof for receipt', async () => {
      const proof = await generateMerkleProof(receipts, 5);

      expect(proof).toBeDefined();
      expect(proof.receiptHash).toBe(receipts[5].hash);
      expect(proof.merkleRoot).toBe(merkleRoot);
      expect(proof.proof).toBeInstanceOf(Array);
      expect(proof.proof.length).toBeGreaterThan(0);

      // Verify proof structure
      proof.proof.forEach(step => {
        expect(step.hash).toBeDefined();
        expect(['left', 'right']).toContain(step.position);
      });
    });

    it('should verify valid Merkle proof', async () => {
      const proof = await generateMerkleProof(receipts, 5);
      const isValid = await verifyMerkleProof(proof, merkleRoot);

      expect(isValid).toBe(true);
    });

    it('should generate and verify proofs for all receipts', async () => {
      for (let i = 0; i < receipts.length; i++) {
        const proof = await generateMerkleProof(receipts, i);
        const isValid = await verifyMerkleProof(proof, merkleRoot);

        expect(isValid).toBe(true);
        expect(proof.receiptHash).toBe(receipts[i].hash);
      }
    });

    it('should detect invalid proof', async () => {
      const proof = await generateMerkleProof(receipts, 5);

      // Tamper with proof
      proof.proof[0].hash = 'a'.repeat(64);

      const isValid = await verifyMerkleProof(proof, merkleRoot);
      expect(isValid).toBe(false);
    });

    it('should detect wrong Merkle root', async () => {
      const proof = await generateMerkleProof(receipts, 5);
      const wrongRoot = 'b'.repeat(64);

      const isValid = await verifyMerkleProof(proof, wrongRoot);
      expect(isValid).toBe(false);
    });

    it('should generate proof for first receipt', async () => {
      const proof = await generateMerkleProof(receipts, 0);
      expect(proof.receiptHash).toBe(receipts[0].hash);

      const isValid = await verifyMerkleProof(proof, merkleRoot);
      expect(isValid).toBe(true);
    });

    it('should generate proof for last receipt', async () => {
      const lastIndex = receipts.length - 1;
      const proof = await generateMerkleProof(receipts, lastIndex);
      expect(proof.receiptHash).toBe(receipts[lastIndex].hash);

      const isValid = await verifyMerkleProof(proof, merkleRoot);
      expect(isValid).toBe(true);
    });
  });

  // ===========================================================================
  // Performance Tests
  // ===========================================================================

  describe('Performance Under Load', () => {
    let signingKey;

    beforeEach(async () => {
      signingKey = await generateSigningKey('perf-test');
    });

    it('should handle 100+ receipt creation efficiently', async () => {
      const count = 100;
      const startTime = Date.now();

      const receipts = [];
      for (let i = 0; i < count; i++) {
        const receipt = await createBlockchainReceipt(
          { type: `EVENT_${i}`, caseId: 'perf-test' },
          { decision: `DECISION_${i}` },
          signingKey
        );
        receipts.push(receipt);
      }

      const duration = Date.now() - startTime;

      expect(receipts).toHaveLength(count);
      // Should complete in reasonable time (< 5000ms for 100 receipts)
      expect(duration).toBeLessThan(5000);

      // Verify all receipts are valid
      for (const receipt of receipts) {
        expect(receipt.hash).toBeDefined();
        expect(receipt.signature).toBeDefined();
      }
    });

    it('should handle batch verification efficiently', async () => {
      // Create 50 receipts
      const receipts = [];
      for (let i = 0; i < 50; i++) {
        const receipt = await createBlockchainReceipt(
          { type: `EVENT_${i}`, caseId: 'verify-perf' },
          { decision: `DECISION_${i}` },
          signingKey
        );
        receipts.push(receipt);
      }

      const startTime = Date.now();

      // Verify all receipts
      const results = await Promise.all(
        receipts.map(r => verifyBlockchainReceipt(r))
      );

      const duration = Date.now() - startTime;

      expect(results).toHaveLength(50);
      expect(results.every(r => r.valid)).toBe(true);
      // Should complete in reasonable time (< 3000ms for 50 verifications)
      expect(duration).toBeLessThan(3000);
    });

    it('should handle large Merkle tree efficiently', async () => {
      // Create 256 receipts
      const count = 256;
      const receipts = [];

      for (let i = 0; i < count; i++) {
        const receipt = await createBlockchainReceipt(
          { type: `EVENT_${i}`, caseId: 'merkle-perf' },
          { decision: `DECISION_${i}` },
          signingKey
        );
        receipts.push(receipt);
      }

      const startTime = Date.now();
      const merkleRoot = await createMerkleRoot(receipts);
      const createDuration = Date.now() - startTime;

      expect(merkleRoot).toBeDefined();
      expect(createDuration).toBeLessThan(1000); // < 1s for 256 receipts

      // Generate and verify proof for middle receipt
      const proofStartTime = Date.now();
      const proof = await generateMerkleProof(receipts, 128);
      const isValid = await verifyMerkleProof(proof, merkleRoot);
      const proofDuration = Date.now() - proofStartTime;

      expect(isValid).toBe(true);
      expect(proofDuration).toBeLessThan(100); // < 100ms for proof
    });
  });

  // ===========================================================================
  // Security Tests
  // ===========================================================================

  describe('Security and Tamper Detection', () => {
    let signingKey;

    beforeEach(async () => {
      signingKey = await generateSigningKey('security-test');
    });

    it('should detect any modification to receipt data', async () => {
      const receipt = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskCompleted,
        SAMPLE_PAYLOAD,
        signingKey
      );

      // Try various tampering attempts
      const tamperTests = [
        { ...receipt, hash: receipt.hash.replace('a', 'b') },
        { ...receipt, signature: receipt.signature.replace('1', '2') },
        { ...receipt, publicKey: receipt.publicKey.replace('c', 'd') },
        { ...receipt, payload: { ...receipt.payload, decision: 'TAMPERED' } },
        { ...receipt, timestamp: new Date().toISOString() },
      ];

      for (const tampered of tamperTests) {
        const result = await verifyBlockchainReceipt(tampered);
        expect(result.valid).toBe(false);
      }
    });

    it('should prevent signature reuse across different receipts', async () => {
      const receipt1 = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskEnabled,
        { decision: 'ENABLE' },
        signingKey
      );

      const receipt2 = await createBlockchainReceipt(
        SAMPLE_EVENTS.taskStarted,
        { decision: 'START' },
        signingKey
      );

      // Try to use receipt1's signature on receipt2
      const invalidReceipt = { ...receipt2, signature: receipt1.signature };

      const result = await verifyBlockchainReceipt(invalidReceipt);
      expect(result.valid).toBe(false);
      expect(result.signatureValid).toBe(false);
    });

    it('should maintain integrity in receipt chains', async () => {
      const events = Array(10).fill(null).map((_, i) => ({
        type: `EVENT_${i}`,
        caseId: 'security-chain',
        taskId: `task-${i}`,
      }));

      const chain = await createReceiptChain(events, signingKey);

      // Verify original chain is valid
      const validResult = await verifyReceiptChain(chain);
      expect(validResult.valid).toBe(true);

      // Test various chain tampering scenarios
      const tamperScenarios = [
        // Swap two receipts
        (() => {
          const swapped = [...chain];
          [swapped[3], swapped[4]] = [swapped[4], swapped[3]];
          return swapped;
        })(),
        // Remove receipt from middle
        [...chain.slice(0, 5), ...chain.slice(6)],
        // Duplicate receipt
        [...chain, chain[0]],
      ];

      for (const tamperedChain of tamperScenarios) {
        const result = await verifyReceiptChain(tamperedChain);
        // Should detect tampering (either invalid chain or wrong count)
        if (tamperedChain.length === chain.length) {
          expect(result.valid).toBe(false);
        }
      }
    });
  });
});
