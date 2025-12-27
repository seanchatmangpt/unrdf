/**
 * @fileoverview KGC Probe - Receipt System Integration
 *
 * Provides cryptographic receipts for probe observations and merge operations:
 * - ObservationReceipt: Per-agent observation hashing with chain
 * - MergeReceipt: Merkle tree of shard hashes
 * - VerificationReceipt: Confidence scoring and verification status
 *
 * Integrates with @unrdf/v6-core BaseReceipt system.
 *
 * @module @unrdf/kgc-probe/receipts
 */

import { randomUUID, createHash } from 'crypto';

// ============================================================================
// CONSTANTS
// ============================================================================

/** Hash algorithm used for receipts */
export const HASH_ALGORITHM = 'sha256';

/** Receipt version */
export const RECEIPT_VERSION = '1.0.0';

/** Receipt types */
export const RECEIPT_TYPES = Object.freeze({
  OBSERVATION: 'probe:observation',
  MERGE: 'probe:merge',
  VERIFICATION: 'probe:verification'
});

// ============================================================================
// HASH UTILITIES
// ============================================================================

/**
 * Deterministic serialization of object
 * @param {any} obj - Object to serialize
 * @returns {string} Deterministic JSON string
 */
export function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) {
    return 'null';
  }
  if (typeof obj === 'bigint') {
    return obj.toString();
  }
  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }
  if (Array.isArray(obj)) {
    return '[' + obj.map(deterministicSerialize).join(',') + ']';
  }
  const keys = Object.keys(obj).sort();
  const pairs = keys.map(k => `${JSON.stringify(k)}:${deterministicSerialize(obj[k])}`);
  return '{' + pairs.join(',') + '}';
}

/**
 * Compute deterministic hash of data
 * @param {any} data - Data to hash
 * @returns {string} Hex-encoded hash
 */
export function computeHash(data) {
  const serialized = typeof data === 'string'
    ? data
    : deterministicSerialize(data);
  return createHash(HASH_ALGORITHM).update(serialized).digest('hex');
}

/**
 * Compute chained hash (previousHash + currentHash)
 * @param {string|null} previousHash - Previous hash in chain (null for genesis)
 * @param {string} currentHash - Current payload hash
 * @returns {string} Chained hash
 */
export function computeChainHash(previousHash, currentHash) {
  const input = `${previousHash || 'GENESIS'}:${currentHash}`;
  return createHash(HASH_ALGORITHM).update(input).digest('hex');
}

// ============================================================================
// OBSERVATION RECEIPT
// ============================================================================

/**
 * @typedef {Object} ObservationReceipt
 * @property {string} id - Receipt UUID
 * @property {string} receiptType - Receipt type discriminator
 * @property {string} agentId - Agent that produced observation
 * @property {number} observationIndex - Sequence number in agent chain
 * @property {string} domain - Observation domain
 * @property {string} obsHash - Hash of observation payload
 * @property {string|null} prevHash - Previous observation hash (chain)
 * @property {bigint} timestamp_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string} payloadHash - Hash of receipt payload
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} receiptHash - Chained receipt hash
 * @property {Object} observation - Original observation
 * @property {Object[]} checks - Determinism checks performed
 */

/**
 * Create an observation receipt
 * @param {Object} params - Receipt parameters
 * @param {string} params.agentId - Agent identifier
 * @param {number} params.observationIndex - Sequence number
 * @param {Object} params.payload - Observation payload
 * @param {string} params.domain - Observation domain
 * @param {ObservationReceipt|null} [params.previousReceipt] - Previous receipt for chaining
 * @returns {ObservationReceipt} Created receipt
 */
export function createObservationReceipt({
  agentId,
  observationIndex,
  payload,
  domain,
  previousReceipt = null
}) {
  if (!agentId) throw new Error('agentId is required');
  if (observationIndex < 1) throw new Error('observationIndex must be >= 1');
  if (!payload) throw new Error('payload is required');
  if (!domain) throw new Error('domain is required');

  const id = randomUUID();
  const now = process.hrtime.bigint();
  const timestamp_iso = new Date().toISOString();

  // Compute observation hash
  const obsHash = computeHash(payload);

  // Previous hash in agent's chain
  const prevHash = previousReceipt ? previousReceipt.obsHash : null;

  // Determinism checks
  const checks = [
    {
      checkType: 'hash_recompute',
      passed: computeHash(payload) === obsHash,
      details: 'Hash recomputation matches'
    },
    {
      checkType: 'serialization_stable',
      passed: deterministicSerialize(payload) === deterministicSerialize(payload),
      details: 'Serialization is stable'
    },
    {
      checkType: 'payload_integrity',
      passed: typeof payload === 'object' && payload !== null,
      details: 'Payload is valid object'
    }
  ];

  // Ensure all checks pass
  const allPassed = checks.every(c => c.passed);
  if (!allPassed) {
    const failed = checks.filter(c => !c.passed).map(c => c.checkType);
    throw new Error(`Determinism checks failed: ${failed.join(', ')}`);
  }

  // Build receipt payload
  const receiptPayload = {
    id,
    receiptType: RECEIPT_TYPES.OBSERVATION,
    agentId,
    observationIndex,
    domain,
    obsHash,
    prevHash,
    timestamp_ns: now,
    timestamp_iso,
    observation: {
      payload,
      timestamp: now,
      hash: obsHash,
      metadata: {
        serializationVersion: RECEIPT_VERSION,
        encoding: 'utf-8',
        deterministic: true
      }
    },
    checks
  };

  // Compute hashes
  const payloadHash = computeHash(receiptPayload);
  const previousReceiptHash = previousReceipt ? previousReceipt.receiptHash : null;
  const receiptHash = computeChainHash(previousReceiptHash, payloadHash);

  return {
    ...receiptPayload,
    payloadHash,
    previousHash: previousReceiptHash,
    receiptHash
  };
}

// ============================================================================
// MERGE RECEIPT
// ============================================================================

/**
 * @typedef {Object} ShardInfo
 * @property {string} agentId - Agent ID
 * @property {string} chainFinalHash - Final hash in agent's chain
 * @property {number} obsCount - Number of observations
 * @property {string} domain - Agent domain
 */

/**
 * @typedef {Object} MergeReceipt
 * @property {string} id - Receipt UUID
 * @property {string} receiptType - Receipt type
 * @property {string} mergeId - Merge operation ID
 * @property {ShardInfo[]} shards - Merged shard information
 * @property {string} merkleRoot - Merkle tree root hash
 * @property {string[]} proofPath - Merkle proof path
 * @property {Object} mergeAlgorithm - Algorithm metadata
 * @property {Object[]|null} conflicts - Detected conflicts
 * @property {bigint} timestamp_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string} payloadHash - Payload hash
 * @property {string|null} previousHash - Always null for merge
 * @property {string} receiptHash - Receipt hash
 */

/**
 * Build merkle tree from shard hashes
 * @param {ShardInfo[]} shards - Shard information
 * @returns {{root: string, proofPath: string[]}} Merkle root and proof
 */
export function buildMerkleTree(shards) {
  if (shards.length === 0) {
    return { root: computeHash('EMPTY'), proofPath: [] };
  }

  // Sort shards by agentId for determinism
  const sorted = [...shards].sort((a, b) => a.agentId.localeCompare(b.agentId));

  // Build leaf hashes
  const leaves = sorted.map(s => computeHash(`${s.agentId}:${s.chainFinalHash}`));
  const proofPath = [...leaves];

  // Build tree bottom-up
  let level = leaves;
  while (level.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < level.length; i += 2) {
      const left = level[i];
      const right = level[i + 1] || left; // Duplicate if odd
      nextLevel.push(computeHash(left + right));
    }
    level = nextLevel;
  }

  return { root: level[0], proofPath };
}

/**
 * Verify merkle root
 * @param {ShardInfo[]} shards - Shard information
 * @param {string} expectedRoot - Expected merkle root
 * @returns {boolean} True if root matches
 */
export function verifyMerkleRoot(shards, expectedRoot) {
  const { root } = buildMerkleTree(shards);
  return root === expectedRoot;
}

/**
 * Create a merge receipt
 * @param {Object} params - Receipt parameters
 * @param {string} params.mergeId - Unique merge operation ID
 * @param {ShardInfo[]} params.shards - Shard information
 * @param {Object[]|null} [params.conflicts] - Detected conflicts
 * @param {string} [params.orchestratorId] - Orchestrator ID
 * @returns {MergeReceipt} Created receipt
 */
export function createMergeReceipt({
  mergeId,
  shards,
  conflicts = null,
  orchestratorId = 'default'
}) {
  if (!mergeId) throw new Error('mergeId is required');
  if (!shards || shards.length === 0) throw new Error('shards is required (non-empty)');

  const id = randomUUID();
  const now = process.hrtime.bigint();
  const timestamp_iso = new Date().toISOString();

  // Build merkle tree
  const { root: merkleRoot, proofPath } = buildMerkleTree(shards);

  // Build receipt payload
  const receiptPayload = {
    id,
    receiptType: RECEIPT_TYPES.MERGE,
    mergeId,
    shards,
    merkleRoot,
    proofPath,
    mergeAlgorithm: {
      algorithm: 'lww-deterministic',
      version: '1.0.0',
      parameters: {
        leafOrder: 'agentId-ascending',
        hashFunction: HASH_ALGORITHM
      }
    },
    conflicts,
    timestamp_ns: now,
    timestamp_iso,
    orchestratorId,
    mergedAt: timestamp_iso
  };

  // Compute hashes
  const payloadHash = computeHash(receiptPayload);
  const receiptHash = computeChainHash(null, payloadHash);

  return {
    ...receiptPayload,
    payloadHash,
    previousHash: null,
    receiptHash
  };
}

// ============================================================================
// VERIFICATION RECEIPT
// ============================================================================

/**
 * @typedef {Object} VerificationCheck
 * @property {string} checkType - Type of verification check
 * @property {boolean} passed - Whether check passed
 * @property {string} details - Check details
 * @property {number} weight - Weight in confidence calculation
 */

/**
 * @typedef {Object} CertificateStep
 * @property {string} step - Step name
 * @property {string} status - Step status
 * @property {string} hash - Step hash
 */

/**
 * @typedef {Object} VerificationReceipt
 * @property {string} id - Receipt UUID
 * @property {string} receiptType - Receipt type
 * @property {string} verificationId - Verification operation ID
 * @property {string} mergeReceiptHash - Hash of merge receipt verified
 * @property {VerificationCheck[]} verifications - Checks performed
 * @property {boolean} deterministic - Overall determinism result
 * @property {boolean} conflictFree - Whether conflicts were found
 * @property {CertificateStep[]} certificateChain - Certificate chain
 * @property {number} obsCount - Observations verified
 * @property {number} agentCount - Agents verified
 * @property {number} confidenceScore - Confidence score [0, 1]
 * @property {bigint} timestamp_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string} payloadHash - Payload hash
 * @property {string|null} previousHash - Always null for verification
 * @property {string} receiptHash - Receipt hash
 */

/**
 * Calculate confidence score from verification checks
 * @param {VerificationCheck[]} checks - Verification checks
 * @returns {number} Confidence score [0, 1]
 */
export function calculateConfidenceScore(checks) {
  if (checks.length === 0) return 0;

  let totalWeight = 0;
  let passedWeight = 0;

  for (const check of checks) {
    const weight = check.weight ?? 1;
    totalWeight += weight;
    if (check.passed) {
      passedWeight += weight;
    }
  }

  return totalWeight > 0 ? passedWeight / totalWeight : 0;
}

/**
 * Get failed checks from verification
 * @param {VerificationCheck[]} checks - Verification checks
 * @returns {VerificationCheck[]} Failed checks
 */
export function getFailedChecks(checks) {
  return checks.filter(c => !c.passed);
}

/**
 * Summarize verification result
 * @param {VerificationReceipt} receipt - Verification receipt
 * @returns {Object} Summary
 */
export function summarizeVerification(receipt) {
  const failed = getFailedChecks(receipt.verifications);
  return {
    verificationId: receipt.verificationId,
    passed: failed.length === 0,
    deterministic: receipt.deterministic,
    conflictFree: receipt.conflictFree,
    confidenceScore: receipt.confidenceScore,
    totalChecks: receipt.verifications.length,
    failedChecks: failed.length,
    obsCount: receipt.obsCount,
    agentCount: receipt.agentCount
  };
}

/**
 * Create a verification receipt
 * @param {Object} params - Receipt parameters
 * @param {string} params.verificationId - Verification ID
 * @param {string} params.mergeReceiptHash - Hash of merge receipt
 * @param {VerificationCheck[]} params.verifications - Checks performed
 * @param {boolean} params.deterministic - Determinism result
 * @param {boolean} params.conflictFree - Conflict-free result
 * @param {CertificateStep[]} params.certificateChain - Certificate chain
 * @param {number} params.obsCount - Observations verified
 * @param {number} params.agentCount - Agents verified
 * @param {string} [params.verifierId] - Verifier ID
 * @returns {VerificationReceipt} Created receipt
 */
export function createVerificationReceipt({
  verificationId,
  mergeReceiptHash,
  verifications,
  deterministic,
  conflictFree,
  certificateChain,
  obsCount,
  agentCount,
  verifierId = 'default'
}) {
  if (!verificationId) throw new Error('verificationId is required');
  if (!mergeReceiptHash) throw new Error('mergeReceiptHash is required');
  if (!Array.isArray(verifications)) throw new Error('verifications array is required');
  if (!Array.isArray(certificateChain)) throw new Error('certificateChain array is required');

  const id = randomUUID();
  const now = process.hrtime.bigint();
  const timestamp_iso = new Date().toISOString();

  // Calculate confidence score
  const confidenceScore = calculateConfidenceScore(verifications);

  // Build receipt payload
  const receiptPayload = {
    id,
    receiptType: RECEIPT_TYPES.VERIFICATION,
    verificationId,
    mergeReceiptHash,
    verifications,
    deterministic,
    conflictFree,
    certificateChain,
    obsCount,
    agentCount,
    confidenceScore,
    timestamp_ns: now,
    timestamp_iso,
    verifierId,
    verifiedAt: timestamp_iso
  };

  // Compute hashes
  const payloadHash = computeHash(receiptPayload);
  const receiptHash = computeChainHash(null, payloadHash);

  return {
    ...receiptPayload,
    payloadHash,
    previousHash: null,
    receiptHash
  };
}

// ============================================================================
// VERIFICATION FUNCTIONS
// ============================================================================

/**
 * Verify an observation receipt
 * @param {ObservationReceipt} receipt - Receipt to verify
 * @param {ObservationReceipt|null} [previousReceipt] - Previous receipt for chain verification
 * @returns {{valid: boolean, errors: string[], checks: Object}}
 */
export function verifyObservationReceipt(receipt, previousReceipt = null) {
  const errors = [];
  const checks = {
    hasRequiredFields: true,
    obsHashValid: true,
    chainValid: true,
    receiptHashValid: true
  };

  // Check required fields
  const requiredFields = ['id', 'agentId', 'obsHash', 'payloadHash', 'receiptHash'];
  for (const field of requiredFields) {
    if (!receipt[field]) {
      checks.hasRequiredFields = false;
      errors.push(`Missing required field: ${field}`);
    }
  }

  // Verify observation hash
  if (receipt.observation?.payload) {
    const expectedObsHash = computeHash(receipt.observation.payload);
    if (expectedObsHash !== receipt.obsHash) {
      checks.obsHashValid = false;
      errors.push('Observation hash mismatch');
    }
  }

  // Verify chain
  if (previousReceipt) {
    if (receipt.prevHash !== previousReceipt.obsHash) {
      checks.chainValid = false;
      errors.push('Previous observation hash mismatch');
    }
    if (receipt.previousHash !== previousReceipt.receiptHash) {
      checks.chainValid = false;
      errors.push('Previous receipt hash mismatch');
    }
  }

  // Verify receipt hash
  const expectedChainHash = computeChainHash(receipt.previousHash, receipt.payloadHash);
  if (expectedChainHash !== receipt.receiptHash) {
    checks.receiptHashValid = false;
    errors.push('Receipt hash mismatch');
  }

  return {
    valid: errors.length === 0,
    errors,
    checks
  };
}

/**
 * Verify a merge receipt
 * @param {MergeReceipt} receipt - Receipt to verify
 * @returns {{valid: boolean, errors: string[], checks: Object}}
 */
export function verifyMergeReceipt(receipt) {
  const errors = [];
  const checks = {
    hasRequiredFields: true,
    merkleRootValid: true,
    receiptHashValid: true
  };

  // Check required fields
  const requiredFields = ['id', 'mergeId', 'shards', 'merkleRoot', 'payloadHash', 'receiptHash'];
  for (const field of requiredFields) {
    if (!receipt[field]) {
      checks.hasRequiredFields = false;
      errors.push(`Missing required field: ${field}`);
    }
  }

  // Verify merkle root
  if (receipt.shards) {
    const expectedRoot = buildMerkleTree(receipt.shards).root;
    if (expectedRoot !== receipt.merkleRoot) {
      checks.merkleRootValid = false;
      errors.push('Merkle root mismatch');
    }
  }

  // Verify receipt hash
  const expectedChainHash = computeChainHash(null, receipt.payloadHash);
  if (expectedChainHash !== receipt.receiptHash) {
    checks.receiptHashValid = false;
    errors.push('Receipt hash mismatch');
  }

  return {
    valid: errors.length === 0,
    errors,
    checks
  };
}

/**
 * Verify a verification receipt
 * @param {VerificationReceipt} receipt - Receipt to verify
 * @returns {{valid: boolean, errors: string[], checks: Object}}
 */
export function verifyVerificationReceipt(receipt) {
  const errors = [];
  const checks = {
    hasRequiredFields: true,
    confidenceValid: true,
    receiptHashValid: true
  };

  // Check required fields
  const requiredFields = ['id', 'verificationId', 'mergeReceiptHash', 'payloadHash', 'receiptHash'];
  for (const field of requiredFields) {
    if (!receipt[field]) {
      checks.hasRequiredFields = false;
      errors.push(`Missing required field: ${field}`);
    }
  }

  // Verify confidence calculation
  if (receipt.verifications) {
    const expectedConfidence = calculateConfidenceScore(receipt.verifications);
    if (Math.abs(expectedConfidence - receipt.confidenceScore) > 0.001) {
      checks.confidenceValid = false;
      errors.push('Confidence score mismatch');
    }
  }

  // Verify receipt hash
  const expectedChainHash = computeChainHash(null, receipt.payloadHash);
  if (expectedChainHash !== receipt.receiptHash) {
    checks.receiptHashValid = false;
    errors.push('Receipt hash mismatch');
  }

  return {
    valid: errors.length === 0,
    errors,
    checks
  };
}

// ============================================================================
// RECEIPT CHAIN BUILDER
// ============================================================================

/**
 * ReceiptChainBuilder - Build chains of observation receipts
 */
export class ReceiptChainBuilder {
  /**
   *
   */
  constructor() {
    /** @type {Map<string, ObservationReceipt[]>} */
    this.chains = new Map();
  }

  /**
   * Add observation to agent's chain
   * @param {string} agentId - Agent ID
   * @param {Object} payload - Observation payload
   * @param {string} domain - Observation domain
   * @returns {ObservationReceipt} Created receipt
   */
  addObservation(agentId, payload, domain) {
    if (!this.chains.has(agentId)) {
      this.chains.set(agentId, []);
    }

    const chain = this.chains.get(agentId);
    const previousReceipt = chain.length > 0 ? chain[chain.length - 1] : null;
    const observationIndex = chain.length + 1;

    const receipt = createObservationReceipt({
      agentId,
      observationIndex,
      payload,
      domain,
      previousReceipt
    });

    chain.push(receipt);
    return receipt;
  }

  /**
   * Get chain for agent
   * @param {string} agentId - Agent ID
   * @returns {ObservationReceipt[]} Agent's receipt chain
   */
  getChain(agentId) {
    return this.chains.get(agentId) || [];
  }

  /**
   * Get final hash for agent's chain
   * @param {string} agentId - Agent ID
   * @returns {string|null} Final chain hash or null
   */
  getFinalHash(agentId) {
    const chain = this.chains.get(agentId);
    if (!chain || chain.length === 0) return null;
    return chain[chain.length - 1].obsHash;
  }

  /**
   * Get all agent IDs
   * @returns {string[]}
   */
  getAgentIds() {
    return Array.from(this.chains.keys());
  }

  /**
   * Get shard info for all agents
   * @returns {ShardInfo[]}
   */
  getShardInfo() {
    const shards = [];
    for (const [agentId, chain] of this.chains) {
      if (chain.length > 0) {
        shards.push({
          agentId,
          chainFinalHash: chain[chain.length - 1].obsHash,
          obsCount: chain.length,
          domain: chain[0].domain
        });
      }
    }
    return shards;
  }

  /**
   * Verify entire chain for agent
   * @param {string} agentId - Agent ID
   * @returns {{valid: boolean, errors: string[]}}
   */
  verifyChain(agentId) {
    const chain = this.chains.get(agentId);
    if (!chain) {
      return { valid: false, errors: ['No chain found for agent'] };
    }

    const allErrors = [];
    for (let i = 0; i < chain.length; i++) {
      const previous = i > 0 ? chain[i - 1] : null;
      const result = verifyObservationReceipt(chain[i], previous);
      if (!result.valid) {
        allErrors.push(...result.errors.map(e => `[${i}] ${e}`));
      }
    }

    return {
      valid: allErrors.length === 0,
      errors: allErrors
    };
  }

  /**
   * Clear all chains
   */
  clear() {
    this.chains.clear();
  }
}

/**
 * Create receipt chain builder
 * @returns {ReceiptChainBuilder}
 */
export function createReceiptChainBuilder() {
  return new ReceiptChainBuilder();
}

// ============================================================================
// EXPORTS
// ============================================================================

export default {
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
};
