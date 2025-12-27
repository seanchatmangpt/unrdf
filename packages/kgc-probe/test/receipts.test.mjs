/**
 * @file Receipt Tests - Observation, Merge, Verification Receipts
 * @description Comprehensive tests for cryptographic receipt system
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  // Constants
  HASH_ALGORITHM,
  RECEIPT_VERSION,
  RECEIPT_TYPES,

  // Hash utilities
  computeHash,
  computeChainHash,
  deterministicSerialize,

  // Merkle
  buildMerkleTree,
  verifyMerkleRoot,

  // Receipt creation
  createObservationReceipt,
  createMergeReceipt,
  createVerificationReceipt,

  // Verification
  verifyObservationReceipt,
  verifyMergeReceipt,
  verifyVerificationReceipt,

  // Confidence
  calculateConfidenceScore,
  getFailedChecks,
  summarizeVerification,

  // Chain builder
  ReceiptChainBuilder,
  createReceiptChainBuilder
} from '../src/receipts/index.mjs';

// ============================================================================
// CONSTANTS TESTS
// ============================================================================

describe('Receipt Constants', () => {
  it('should export HASH_ALGORITHM', () => {
    expect(HASH_ALGORITHM).toBe('sha256');
  });

  it('should export RECEIPT_VERSION', () => {
    expect(RECEIPT_VERSION).toBe('1.0.0');
  });

  it('should export RECEIPT_TYPES', () => {
    expect(RECEIPT_TYPES.OBSERVATION).toBe('probe:observation');
    expect(RECEIPT_TYPES.MERGE).toBe('probe:merge');
    expect(RECEIPT_TYPES.VERIFICATION).toBe('probe:verification');
  });
});

// ============================================================================
// HASH UTILITY TESTS
// ============================================================================

describe('Hash Utilities', () => {
  describe('computeHash', () => {
    it('should compute hash of string', () => {
      const hash = computeHash('test');
      expect(hash).toHaveLength(64); // SHA256 hex
    });

    it('should compute deterministic hash', () => {
      const h1 = computeHash('test');
      const h2 = computeHash('test');
      expect(h1).toBe(h2);
    });

    it('should compute different hashes for different inputs', () => {
      const h1 = computeHash('test1');
      const h2 = computeHash('test2');
      expect(h1).not.toBe(h2);
    });

    it('should hash objects deterministically', () => {
      const h1 = computeHash({ b: 2, a: 1 });
      const h2 = computeHash({ a: 1, b: 2 });
      expect(h1).toBe(h2);
    });
  });

  describe('computeChainHash', () => {
    it('should compute chain hash with previous', () => {
      const prev = computeHash('prev');
      const current = computeHash('current');
      const chain = computeChainHash(prev, current);
      expect(chain).toHaveLength(64);
    });

    it('should handle null previous (genesis)', () => {
      const current = computeHash('current');
      const chain = computeChainHash(null, current);
      expect(chain).toHaveLength(64);
    });

    it('should produce different results for different chains', () => {
      const current = computeHash('current');
      const chain1 = computeChainHash('prev1', current);
      const chain2 = computeChainHash('prev2', current);
      expect(chain1).not.toBe(chain2);
    });
  });

  describe('deterministicSerialize', () => {
    it('should serialize null', () => {
      expect(deterministicSerialize(null)).toBe('null');
    });

    it('should serialize primitives', () => {
      expect(deterministicSerialize('test')).toBe('"test"');
      expect(deterministicSerialize(123)).toBe('123');
      expect(deterministicSerialize(true)).toBe('true');
    });

    it('should serialize bigint', () => {
      expect(deterministicSerialize(BigInt(123))).toBe('123');
    });

    it('should serialize arrays', () => {
      const result = deterministicSerialize([1, 2, 3]);
      expect(result).toBe('[1,2,3]');
    });

    it('should serialize objects with sorted keys', () => {
      const result = deterministicSerialize({ b: 2, a: 1 });
      expect(result).toBe('{"a":1,"b":2}');
    });

    it('should serialize nested objects', () => {
      const result = deterministicSerialize({ z: { b: 2, a: 1 }, a: 0 });
      expect(result).toBe('{"a":0,"z":{"a":1,"b":2}}');
    });
  });
});

// ============================================================================
// MERKLE TREE TESTS
// ============================================================================

describe('Merkle Tree', () => {
  const sampleShards = [
    { agentId: 'agent-1', chainFinalHash: 'hash1', obsCount: 5, domain: 'runtime' },
    { agentId: 'agent-2', chainFinalHash: 'hash2', obsCount: 3, domain: 'network' },
    { agentId: 'agent-3', chainFinalHash: 'hash3', obsCount: 7, domain: 'system' }
  ];

  describe('buildMerkleTree', () => {
    it('should build tree from shards', () => {
      const { root, proofPath } = buildMerkleTree(sampleShards);
      expect(root).toHaveLength(64);
      expect(Array.isArray(proofPath)).toBe(true);
    });

    it('should produce deterministic root', () => {
      const { root: r1 } = buildMerkleTree(sampleShards);
      const { root: r2 } = buildMerkleTree(sampleShards);
      expect(r1).toBe(r2);
    });

    it('should sort by agentId for determinism', () => {
      const unsorted = [
        { agentId: 'c', chainFinalHash: 'h3', obsCount: 1, domain: 'd' },
        { agentId: 'a', chainFinalHash: 'h1', obsCount: 1, domain: 'd' },
        { agentId: 'b', chainFinalHash: 'h2', obsCount: 1, domain: 'd' }
      ];
      const sorted = [
        { agentId: 'a', chainFinalHash: 'h1', obsCount: 1, domain: 'd' },
        { agentId: 'b', chainFinalHash: 'h2', obsCount: 1, domain: 'd' },
        { agentId: 'c', chainFinalHash: 'h3', obsCount: 1, domain: 'd' }
      ];

      const { root: r1 } = buildMerkleTree(unsorted);
      const { root: r2 } = buildMerkleTree(sorted);
      expect(r1).toBe(r2);
    });

    it('should handle empty shards', () => {
      const { root } = buildMerkleTree([]);
      expect(root).toHaveLength(64);
    });

    it('should handle single shard', () => {
      const { root, proofPath } = buildMerkleTree([sampleShards[0]]);
      expect(root).toHaveLength(64);
      expect(proofPath.length).toBe(1);
    });
  });

  describe('verifyMerkleRoot', () => {
    it('should verify correct root', () => {
      const { root } = buildMerkleTree(sampleShards);
      const isValid = verifyMerkleRoot(sampleShards, root);
      expect(isValid).toBe(true);
    });

    it('should reject incorrect root', () => {
      const isValid = verifyMerkleRoot(sampleShards, 'wrongroot');
      expect(isValid).toBe(false);
    });

    it('should reject modified shards', () => {
      const { root } = buildMerkleTree(sampleShards);
      const modified = [...sampleShards];
      modified[0] = { ...modified[0], chainFinalHash: 'tampered' };
      const isValid = verifyMerkleRoot(modified, root);
      expect(isValid).toBe(false);
    });
  });
});

// ============================================================================
// OBSERVATION RECEIPT TESTS
// ============================================================================

describe('Observation Receipt', () => {
  const payload = { key: 'value', timestamp: Date.now() };

  describe('createObservationReceipt', () => {
    it('should create receipt with required fields', () => {
      const receipt = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload,
        domain: 'test'
      });

      expect(receipt.id).toBeDefined();
      expect(receipt.receiptType).toBe(RECEIPT_TYPES.OBSERVATION);
      expect(receipt.agentId).toBe('agent-1');
      expect(receipt.observationIndex).toBe(1);
      expect(receipt.domain).toBe('test');
      expect(receipt.obsHash).toHaveLength(64);
      expect(receipt.payloadHash).toHaveLength(64);
      expect(receipt.receiptHash).toHaveLength(64);
    });

    it('should set prevHash to null for first receipt', () => {
      const receipt = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload,
        domain: 'test'
      });

      expect(receipt.prevHash).toBeNull();
      expect(receipt.previousHash).toBeNull();
    });

    it('should chain to previous receipt', () => {
      const r1 = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload: { a: 1 },
        domain: 'test'
      });

      const r2 = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 2,
        payload: { b: 2 },
        domain: 'test',
        previousReceipt: r1
      });

      expect(r2.prevHash).toBe(r1.obsHash);
      expect(r2.previousHash).toBe(r1.receiptHash);
    });

    it('should include observation with metadata', () => {
      const receipt = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload,
        domain: 'test'
      });

      expect(receipt.observation.payload).toEqual(payload);
      expect(receipt.observation.hash).toBe(receipt.obsHash);
      expect(receipt.observation.metadata.serializationVersion).toBe(RECEIPT_VERSION);
      expect(receipt.observation.metadata.deterministic).toBe(true);
    });

    it('should run determinism checks', () => {
      const receipt = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload,
        domain: 'test'
      });

      expect(receipt.checks).toHaveLength(3);
      expect(receipt.checks.every(c => c.passed)).toBe(true);
    });

    it('should throw for missing agentId', () => {
      expect(() => createObservationReceipt({
        observationIndex: 1,
        payload,
        domain: 'test'
      })).toThrow('agentId is required');
    });

    it('should throw for invalid observationIndex', () => {
      expect(() => createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 0,
        payload,
        domain: 'test'
      })).toThrow('observationIndex must be >= 1');
    });
  });

  describe('verifyObservationReceipt', () => {
    it('should verify valid receipt', () => {
      const receipt = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload,
        domain: 'test'
      });

      const result = verifyObservationReceipt(receipt);
      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    it('should verify chained receipts', () => {
      const r1 = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload: { a: 1 },
        domain: 'test'
      });

      const r2 = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 2,
        payload: { b: 2 },
        domain: 'test',
        previousReceipt: r1
      });

      const result = verifyObservationReceipt(r2, r1);
      expect(result.valid).toBe(true);
    });

    it('should detect tampered payload', () => {
      const receipt = createObservationReceipt({
        agentId: 'agent-1',
        observationIndex: 1,
        payload,
        domain: 'test'
      });

      // Tamper with payload
      receipt.observation.payload = { tampered: true };

      const result = verifyObservationReceipt(receipt);
      expect(result.valid).toBe(false);
      expect(result.errors).toContain('Observation hash mismatch');
    });
  });
});

// ============================================================================
// MERGE RECEIPT TESTS
// ============================================================================

describe('Merge Receipt', () => {
  const shards = [
    { agentId: 'agent-1', chainFinalHash: 'hash1', obsCount: 5, domain: 'runtime' },
    { agentId: 'agent-2', chainFinalHash: 'hash2', obsCount: 3, domain: 'network' }
  ];

  describe('createMergeReceipt', () => {
    it('should create receipt with required fields', () => {
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards
      });

      expect(receipt.id).toBeDefined();
      expect(receipt.receiptType).toBe(RECEIPT_TYPES.MERGE);
      expect(receipt.mergeId).toBe('merge-1');
      expect(receipt.shards).toEqual(shards);
      expect(receipt.merkleRoot).toHaveLength(64);
      expect(receipt.payloadHash).toHaveLength(64);
      expect(receipt.receiptHash).toHaveLength(64);
    });

    it('should include merkle proof', () => {
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards
      });

      expect(Array.isArray(receipt.proofPath)).toBe(true);
    });

    it('should include merge algorithm metadata', () => {
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards
      });

      expect(receipt.mergeAlgorithm.algorithm).toBe('lww-deterministic');
      expect(receipt.mergeAlgorithm.version).toBe('1.0.0');
      expect(receipt.mergeAlgorithm.parameters.hashFunction).toBe(HASH_ALGORITHM);
    });

    it('should handle conflicts', () => {
      const conflicts = [{ type: 'value_conflict', key: 'key1' }];
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards,
        conflicts
      });

      expect(receipt.conflicts).toEqual(conflicts);
    });

    it('should set null conflicts when none', () => {
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards
      });

      expect(receipt.conflicts).toBeNull();
    });

    it('should throw for missing mergeId', () => {
      expect(() => createMergeReceipt({ shards })).toThrow('mergeId is required');
    });

    it('should throw for empty shards', () => {
      expect(() => createMergeReceipt({
        mergeId: 'merge-1',
        shards: []
      })).toThrow('shards is required');
    });
  });

  describe('verifyMergeReceipt', () => {
    it('should verify valid receipt', () => {
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards
      });

      const result = verifyMergeReceipt(receipt);
      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    it('should detect tampered merkle root', () => {
      const receipt = createMergeReceipt({
        mergeId: 'merge-1',
        shards
      });

      receipt.merkleRoot = 'tampered';

      const result = verifyMergeReceipt(receipt);
      expect(result.valid).toBe(false);
      expect(result.errors).toContain('Merkle root mismatch');
    });
  });
});

// ============================================================================
// VERIFICATION RECEIPT TESTS
// ============================================================================

describe('Verification Receipt', () => {
  const verifications = [
    { checkType: 'hash', passed: true, details: 'Hash valid', weight: 2 },
    { checkType: 'chain', passed: true, details: 'Chain valid', weight: 2 },
    { checkType: 'merkle', passed: true, details: 'Merkle valid', weight: 1 }
  ];

  const certificateChain = [
    { step: 'observation', status: 'verified', hash: 'hash1' },
    { step: 'merge', status: 'verified', hash: 'hash2' }
  ];

  describe('createVerificationReceipt', () => {
    it('should create receipt with required fields', () => {
      const receipt = createVerificationReceipt({
        verificationId: 'verify-1',
        mergeReceiptHash: 'merge-hash',
        verifications,
        deterministic: true,
        conflictFree: true,
        certificateChain,
        obsCount: 10,
        agentCount: 5
      });

      expect(receipt.id).toBeDefined();
      expect(receipt.receiptType).toBe(RECEIPT_TYPES.VERIFICATION);
      expect(receipt.verificationId).toBe('verify-1');
      expect(receipt.deterministic).toBe(true);
      expect(receipt.conflictFree).toBe(true);
      expect(receipt.obsCount).toBe(10);
      expect(receipt.agentCount).toBe(5);
    });

    it('should calculate confidence score', () => {
      const receipt = createVerificationReceipt({
        verificationId: 'verify-1',
        mergeReceiptHash: 'merge-hash',
        verifications,
        deterministic: true,
        conflictFree: true,
        certificateChain,
        obsCount: 10,
        agentCount: 5
      });

      expect(receipt.confidenceScore).toBe(1); // All passed
    });

    it('should handle failed checks in confidence', () => {
      const mixedChecks = [
        { checkType: 'a', passed: true, weight: 1 },
        { checkType: 'b', passed: false, weight: 1 }
      ];

      const receipt = createVerificationReceipt({
        verificationId: 'verify-1',
        mergeReceiptHash: 'merge-hash',
        verifications: mixedChecks,
        deterministic: true,
        conflictFree: true,
        certificateChain,
        obsCount: 10,
        agentCount: 5
      });

      expect(receipt.confidenceScore).toBe(0.5);
    });
  });

  describe('verifyVerificationReceipt', () => {
    it('should verify valid receipt', () => {
      const receipt = createVerificationReceipt({
        verificationId: 'verify-1',
        mergeReceiptHash: 'merge-hash',
        verifications,
        deterministic: true,
        conflictFree: true,
        certificateChain,
        obsCount: 10,
        agentCount: 5
      });

      const result = verifyVerificationReceipt(receipt);
      expect(result.valid).toBe(true);
    });
  });

  describe('calculateConfidenceScore', () => {
    it('should return 0 for empty checks', () => {
      expect(calculateConfidenceScore([])).toBe(0);
    });

    it('should return 1 when all pass', () => {
      const checks = [
        { passed: true, weight: 1 },
        { passed: true, weight: 1 }
      ];
      expect(calculateConfidenceScore(checks)).toBe(1);
    });

    it('should weight checks correctly', () => {
      const checks = [
        { passed: true, weight: 3 },
        { passed: false, weight: 1 }
      ];
      expect(calculateConfidenceScore(checks)).toBe(0.75);
    });

    it('should default weight to 1', () => {
      const checks = [
        { passed: true },
        { passed: false }
      ];
      expect(calculateConfidenceScore(checks)).toBe(0.5);
    });
  });

  describe('getFailedChecks', () => {
    it('should return empty array when all pass', () => {
      const checks = [{ passed: true }, { passed: true }];
      expect(getFailedChecks(checks)).toHaveLength(0);
    });

    it('should return failed checks only', () => {
      const checks = [
        { checkType: 'a', passed: true },
        { checkType: 'b', passed: false },
        { checkType: 'c', passed: false }
      ];
      const failed = getFailedChecks(checks);
      expect(failed).toHaveLength(2);
      expect(failed.map(c => c.checkType)).toEqual(['b', 'c']);
    });
  });

  describe('summarizeVerification', () => {
    it('should summarize verification result', () => {
      const receipt = createVerificationReceipt({
        verificationId: 'verify-1',
        mergeReceiptHash: 'merge-hash',
        verifications,
        deterministic: true,
        conflictFree: true,
        certificateChain,
        obsCount: 10,
        agentCount: 5
      });

      const summary = summarizeVerification(receipt);
      expect(summary.verificationId).toBe('verify-1');
      expect(summary.passed).toBe(true);
      expect(summary.deterministic).toBe(true);
      expect(summary.conflictFree).toBe(true);
      expect(summary.confidenceScore).toBe(1);
      expect(summary.totalChecks).toBe(3);
      expect(summary.failedChecks).toBe(0);
    });
  });
});

// ============================================================================
// RECEIPT CHAIN BUILDER TESTS
// ============================================================================

describe('ReceiptChainBuilder', () => {
  let builder;

  beforeEach(() => {
    builder = createReceiptChainBuilder();
  });

  it('should add observations to chain', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    builder.addObservation('agent-1', { b: 2 }, 'test');

    const chain = builder.getChain('agent-1');
    expect(chain).toHaveLength(2);
    expect(chain[0].observationIndex).toBe(1);
    expect(chain[1].observationIndex).toBe(2);
  });

  it('should chain receipts correctly', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    builder.addObservation('agent-1', { b: 2 }, 'test');

    const chain = builder.getChain('agent-1');
    expect(chain[1].prevHash).toBe(chain[0].obsHash);
    expect(chain[1].previousHash).toBe(chain[0].receiptHash);
  });

  it('should manage multiple agents', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    builder.addObservation('agent-2', { b: 2 }, 'test');
    builder.addObservation('agent-1', { c: 3 }, 'test');

    expect(builder.getChain('agent-1')).toHaveLength(2);
    expect(builder.getChain('agent-2')).toHaveLength(1);
  });

  it('should get final hash', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    const receipt = builder.addObservation('agent-1', { b: 2 }, 'test');

    const finalHash = builder.getFinalHash('agent-1');
    expect(finalHash).toBe(receipt.obsHash);
  });

  it('should return null for unknown agent', () => {
    expect(builder.getFinalHash('unknown')).toBeNull();
  });

  it('should list agent IDs', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    builder.addObservation('agent-2', { b: 2 }, 'test');

    const ids = builder.getAgentIds();
    expect(ids).toContain('agent-1');
    expect(ids).toContain('agent-2');
  });

  it('should get shard info', () => {
    builder.addObservation('agent-1', { a: 1 }, 'runtime');
    builder.addObservation('agent-1', { b: 2 }, 'runtime');
    builder.addObservation('agent-2', { c: 3 }, 'network');

    const shards = builder.getShardInfo();
    expect(shards).toHaveLength(2);

    const agent1 = shards.find(s => s.agentId === 'agent-1');
    expect(agent1.obsCount).toBe(2);
    expect(agent1.domain).toBe('runtime');
  });

  it('should verify valid chain', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    builder.addObservation('agent-1', { b: 2 }, 'test');

    const result = builder.verifyChain('agent-1');
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });

  it('should clear all chains', () => {
    builder.addObservation('agent-1', { a: 1 }, 'test');
    builder.addObservation('agent-2', { b: 2 }, 'test');
    builder.clear();

    expect(builder.getAgentIds()).toHaveLength(0);
  });
});
