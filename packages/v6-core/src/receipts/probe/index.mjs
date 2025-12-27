/**
 * Probe Receipts - Integration module for observation + merge + verification
 *
 * Provides factories and verifiers for probe receipt types.
 * Integrates with v6-core base receipt system.
 *
 * @module @unrdf/v6-core/receipts/probe
 */

import { now, toISO } from '@unrdf/kgc-4d';
import {
  generateUUID,
  computeBlake3,
  computeChainHash,
  deterministicSerialize,
} from '../base-receipt.mjs';

// Probe receipt types
import ProbeObservationReceiptSchema, {
  PROBE_OBSERVATION_TYPE,
  SERIALIZATION_VERSION,
  ENCODING_FORMAT,
  checkHashRecompute,
  checkSerializationStable,
  checkPayloadIntegrity,
  computeObsHash,
} from './observation-receipt.mjs';

import ProbeMergeReceiptSchema, {
  PROBE_MERGE_TYPE,
  MERGE_ALGORITHM,
  MERGE_ALGORITHM_VERSION,
  LEAF_ORDER,
  buildMerkleTree,
  verifyMerkleRoot,
} from './merge-receipt.mjs';

import ProbeVerificationReceiptSchema, {
  PROBE_VERIFICATION_TYPE,
  summarizeVerification,
  getFailedChecks,
  getConfidenceScore,
} from './verification-receipt.mjs';

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * Create a probe observation receipt
 *
 * @param {Object} event - Event data
 * @param {string} event.agentId - Agent/shard identifier
 * @param {number} event.observationIndex - Sequence number in agent's chain
 * @param {any} event.payload - Observation payload
 * @param {string} event.domain - Domain/category
 * @param {Object} [previousReceipt] - Previous observation receipt (for chaining)
 * @returns {Promise<Object>} ProbeObservationReceipt
 *
 * @example
 * const receipt = await createProbeObservationReceipt({
 *   agentId: 'agent-1',
 *   observationIndex: 1,
 *   payload: { timestamp: Date.now(), value: 'data' },
 *   domain: 'network'
 * });
 */
export async function createProbeObservationReceipt(event, previousReceipt = null) {
  if (!event.agentId) throw new Error('agentId is required');
  if (event.observationIndex < 1) throw new Error('observationIndex must be >= 1');
  if (!event.payload) throw new Error('payload is required');
  if (!event.domain) throw new Error('domain is required');

  // Generate receipt ID and timestamp
  const id = generateUUID();
  const t_ns = now();
  const timestamp_iso = toISO(t_ns);

  // Compute observation hash
  const obsHash = await computeObsHash(event.payload);

  // Determine previous hash in agent's chain
  const prevHash = previousReceipt ? previousReceipt.obsHash : null;

  // Build observation record
  const observation = {
    payload: event.payload,
    timestamp: t_ns,
    hash: obsHash,
    metadata: {
      serializationVersion: SERIALIZATION_VERSION,
      encoding: ENCODING_FORMAT,
      deterministic: true,
    },
  };

  // Run determinism checks
  const checks = [];

  // Check 1: Hash recompute
  const checkHash = await checkHashRecompute(event.payload, obsHash);
  checks.push(checkHash);

  // Check 2: Serialization stability
  const checkStability = await checkSerializationStable(event.payload, obsHash);
  checks.push(checkStability);

  // Check 3: Payload integrity
  const checkIntegrity = checkPayloadIntegrity(event.payload);
  checks.push(checkIntegrity);

  // Verify all checks passed
  const allChecksPassed = checks.every((c) => c.passed);
  if (!allChecksPassed) {
    const failedChecks = checks.filter((c) => !c.passed).map((c) => c.checkType);
    throw new Error(`Determinism checks failed: ${failedChecks.join(', ')}`);
  }

  // Build receipt data
  const receiptData = {
    id,
    receiptType: PROBE_OBSERVATION_TYPE,
    t_ns,
    timestamp_iso,
    agentId: event.agentId,
    observationIndex: event.observationIndex,
    domain: event.domain,
    obsHash,
    prevHash,
    observation,
    checks,
    attestation: event.attestation,
  };

  // Compute hashes
  const payloadToHash = { ...receiptData };
  const payloadHash = await computeBlake3(payloadToHash);

  const previousHashForChain = previousReceipt ? previousReceipt.receiptHash : null;
  const receiptHash = await computeChainHash(previousHashForChain, payloadHash);

  // Build final receipt
  const receipt = {
    ...receiptData,
    previousHash: previousHashForChain,
    payloadHash,
    receiptHash,
  };

  // Validate against schema
  ProbeObservationReceiptSchema.parse(receipt);

  return receipt;
}

/**
 * Create a probe merge receipt
 *
 * @param {Object} event - Event data
 * @param {string} event.mergeId - Unique merge operation ID
 * @param {Array} event.shards - Array of shard info objects
 * @param {string} event.shards[].agentId - Agent ID
 * @param {string} event.shards[].chainFinalHash - Final chain hash
 * @param {number} event.shards[].obsCount - Observation count
 * @param {string} event.shards[].domain - Domain
 * @param {Array} [event.conflicts] - Conflicts found (null = none)
 * @param {Function} [hashFunction] - Hash function (default: computeBlake3)
 * @returns {Promise<Object>} ProbeMergeReceipt
 */
export async function createProbeMergeReceipt(event, hashFunction = computeBlake3) {
  if (!event.mergeId) throw new Error('mergeId is required');
  if (!event.shards || event.shards.length === 0) throw new Error('shards is required (non-empty)');

  // Generate receipt ID and timestamp
  const id = generateUUID();
  const t_ns = now();
  const timestamp_iso = toISO(t_ns);

  // Build merkle tree
  const { root: merkleRoot, proofPath } = await buildMerkleTree(event.shards, hashFunction);

  // Build receipt data
  const receiptData = {
    id,
    receiptType: PROBE_MERGE_TYPE,
    t_ns,
    timestamp_iso,
    mergeId: event.mergeId,
    shards: event.shards,
    merkleRoot,
    proofPath,
    mergeAlgorithm: {
      algorithm: MERGE_ALGORITHM,
      version: MERGE_ALGORITHM_VERSION,
      parameters: {
        leafOrder: LEAF_ORDER,
        hashFunction: 'blake3',
      },
    },
    conflicts: event.conflicts || null,
    mergedAt: event.mergedAt,
    orchestratorId: event.orchestratorId,
  };

  // Compute hashes
  const payloadToHash = { ...receiptData };
  const payloadHash = await computeBlake3(payloadToHash);

  // Merge receipt has no previous
  const receiptHash = await computeChainHash(null, payloadHash);

  // Build final receipt
  const receipt = {
    ...receiptData,
    previousHash: null,
    payloadHash,
    receiptHash,
  };

  // Validate against schema
  ProbeMergeReceiptSchema.parse(receipt);

  return receipt;
}

/**
 * Create a probe verification receipt
 *
 * @param {Object} event - Event data
 * @param {string} event.verificationId - Unique verification ID
 * @param {string} event.mergeReceiptHash - Hash of merge receipt being verified
 * @param {Array} event.verifications - Verification checks performed
 * @param {boolean} event.deterministic - Determinism result
 * @param {boolean} event.conflictFree - Conflict-free result
 * @param {Array} event.certificateChain - Certificate chain steps
 * @param {number} event.obsCount - Total observations verified
 * @param {number} event.agentCount - Total agents verified
 * @returns {Promise<Object>} ProbeVerificationReceipt
 */
export async function createProbeVerificationReceipt(event) {
  if (!event.verificationId) throw new Error('verificationId is required');
  if (!event.mergeReceiptHash) throw new Error('mergeReceiptHash is required');
  if (!Array.isArray(event.verifications)) throw new Error('verifications array is required');
  if (!Array.isArray(event.certificateChain)) throw new Error('certificateChain array is required');

  // Generate receipt ID and timestamp
  const id = generateUUID();
  const t_ns = now();
  const timestamp_iso = toISO(t_ns);

  // Build receipt data
  const receiptData = {
    id,
    receiptType: PROBE_VERIFICATION_TYPE,
    t_ns,
    timestamp_iso,
    verificationId: event.verificationId,
    mergeReceiptHash: event.mergeReceiptHash,
    verifications: event.verifications,
    deterministic: event.deterministic,
    conflictFree: event.conflictFree,
    certificateChain: event.certificateChain,
    obsCount: event.obsCount || 0,
    agentCount: event.agentCount || 0,
    verifiedAt: event.verifiedAt,
    verifierId: event.verifierId,
  };

  // Compute hashes
  const payloadToHash = { ...receiptData };
  const payloadHash = await computeBlake3(payloadToHash);

  // Verification receipt has no previous
  const receiptHash = await computeChainHash(null, payloadHash);

  // Build final receipt
  const receipt = {
    ...receiptData,
    previousHash: null,
    payloadHash,
    receiptHash,
  };

  // Validate against schema
  ProbeVerificationReceiptSchema.parse(receipt);

  return receipt;
}

// =============================================================================
// Exports
// =============================================================================

// Schemas
export {
  ProbeObservationReceiptSchema,
  ProbeMergeReceiptSchema,
  ProbeVerificationReceiptSchema,
};

// Constants
export {
  PROBE_OBSERVATION_TYPE,
  PROBE_MERGE_TYPE,
  PROBE_VERIFICATION_TYPE,
  MERGE_ALGORITHM,
  MERGE_ALGORITHM_VERSION,
  LEAF_ORDER,
};

// Utilities
export {
  // Observation
  computeObsHash,
  checkHashRecompute,
  checkSerializationStable,
  checkPayloadIntegrity,
  // Merge
  buildMerkleTree,
  verifyMerkleRoot,
  // Verification
  summarizeVerification,
  getFailedChecks,
  getConfidenceScore,
};

// Factory functions
export {
  createProbeObservationReceipt,
  createProbeMergeReceipt,
  createProbeVerificationReceipt,
};

// Default export
export default {
  createProbeObservationReceipt,
  createProbeMergeReceipt,
  createProbeVerificationReceipt,
  PROBE_OBSERVATION_TYPE,
  PROBE_MERGE_TYPE,
  PROBE_VERIFICATION_TYPE,
};
