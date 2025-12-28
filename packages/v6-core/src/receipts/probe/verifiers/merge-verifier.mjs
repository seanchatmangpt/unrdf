/**
 * Probe Merge Verifier
 *
 * Verifies that merges are deterministic and conflict-free.
 *
 * @module @unrdf/v6-core/receipts/probe/verifiers/merge-verifier
 */

import {
  computeBlake3,
  computeChainHash,
} from '../../base-receipt.mjs';

import {
  verifyMerkleRoot,
} from '../merge-receipt.mjs';

import {
  verifyProbeChains,
} from './chain-verifier.mjs';

// =============================================================================
// Merge Verification
// =============================================================================

/**
 * Verify a probe merge receipt
 *
 * Checks:
 * 1. Schema validity
 * 2. Shard hashes match actual chain final hashes
 * 3. Merkle root correctly computed
 * 4. Conflict status is consistent
 *
 * @param {Object} receipt - ProbeMergeReceipt to verify
 * @param {Object<string, Array>} agentChains - Map of agentId -> chain
 * @returns {Promise<Object>} Merge verification result
 *
 * @example
 * const result = await verifyProbeMerge(receipt, chains);
 * console.log(result.valid); // true if merge is valid
 * console.log(result.conflictFree); // true if no conflicts
 */
export async function verifyProbeMerge(receipt, agentChains) {
  const errors = [];
  const checks = {
    schemaValid: false,
    shardHashesValid: false,
    merkleRootValid: false,
    conflictConsistent: false,
  };

  // Input validation
  if (!receipt || receipt.receiptType !== 'probe-merge') {
    errors.push('Invalid receipt type');
    return { valid: false, checks, errors };
  }

  if (!receipt.shards || receipt.shards.length === 0) {
    errors.push('Receipt has no shards');
    return { valid: false, checks, errors };
  }

  checks.schemaValid = true;

  // 1. Verify shard hashes match actual chains
  const shardErrors = [];

  if (!agentChains) {
    shardErrors.push('No agent chains provided for verification');
  } else {
    for (const shard of receipt.shards) {
      const chain = agentChains[shard.agentId];

      if (!chain) {
        shardErrors.push(`Missing chain for agent ${shard.agentId}`);
        continue;
      }

      const lastReceipt = chain[chain.length - 1];

      // Check final hash
      if (lastReceipt.receiptHash !== shard.chainFinalHash) {
        shardErrors.push(`Chain final hash mismatch for ${shard.agentId}: expected ${shard.chainFinalHash}, got ${lastReceipt.receiptHash}`);
      }

      // Check observation count
      if (chain.length !== shard.obsCount) {
        shardErrors.push(`Observation count mismatch for ${shard.agentId}: expected ${shard.obsCount}, got ${chain.length}`);
      }
    }
  }

  if (shardErrors.length === 0) {
    checks.shardHashesValid = true;
  } else {
    errors.push(...shardErrors);
  }

  // 2. Verify merkle root
  try {
    const merkleVerify = await verifyMerkleRoot(
      receipt.merkleRoot,
      receipt.shards,
      computeBlake3
    );

    checks.merkleRootValid = merkleVerify.valid;

    if (!merkleVerify.valid) {
      errors.push(`Merkle root mismatch: expected ${receipt.merkleRoot}, got ${merkleVerify.computedRoot}`);
    }
  } catch (error) {
    checks.merkleRootValid = false;
    errors.push(`Merkle verification failed: ${error.message}`);
  }

  // 3. Verify conflict status is consistent
  const hasConflicts = receipt.conflicts && receipt.conflicts.length > 0;
  const conflictConsistent = (hasConflicts && errors.length > 0) || (!hasConflicts && errors.length === 0);

  if (!conflictConsistent) {
    errors.push('Conflict status inconsistent with verification results');
  }

  checks.conflictConsistent = conflictConsistent;

  // 4. Verify receipt hashes
  try {
    const payloadToHash = { ...receipt };
    delete payloadToHash.receiptHash;
    delete payloadToHash.payloadHash;

    const computedPayloadHash = await computeBlake3(payloadToHash);

    if (computedPayloadHash !== receipt.payloadHash) {
      errors.push('Payload hash mismatch');
    } else {
      const computedReceiptHash = await computeChainHash(null, receipt.payloadHash);

      if (computedReceiptHash !== receipt.receiptHash) {
        errors.push('Receipt hash mismatch');
      }
    }
  } catch (error) {
    errors.push(`Hash verification failed: ${error.message}`);
  }

  const valid = errors.length === 0;
  const conflictFree = !hasConflicts && valid;

  return {
    valid,
    conflictFree,
    checks,
    errors,
    shardCount: receipt.shards.length,
    merkleRoot: receipt.merkleRoot,
  };
}

/**
 * Verify merge by recomputing from original chains
 *
 * End-to-end verification: verify all chains + verify merge against chains.
 *
 * @param {Object} mergeReceipt - ProbeMergeReceipt
 * @param {Object<string, Array>} agentChains - Map of agentId -> chain
 * @returns {Promise<Object>} Complete verification result
 */
export async function verifyProbeMergeComplete(mergeReceipt, agentChains) {
  // First verify all chains
  const chainResults = await verifyProbeChains(agentChains);

  if (!chainResults.valid) {
    return {
      valid: false,
      chainsValid: false,
      mergeValid: false,
      deterministic: false,
      conflictFree: false,
      errors: chainResults.errors,
    };
  }

  // Then verify merge against chains
  const mergeResults = await verifyProbeMerge(mergeReceipt, agentChains);

  const deterministic = chainResults.valid && mergeResults.valid;
  const conflictFree = mergeResults.conflictFree && deterministic;

  return {
    valid: mergeResults.valid && chainResults.valid,
    chainsValid: chainResults.valid,
    mergeValid: mergeResults.valid,
    deterministic,
    conflictFree,
    chainCount: chainResults.agentCount,
    observationCount: chainResults.totalObservations,
    errors: [...chainResults.errors, ...mergeResults.errors],
    mergeResults,
    chainResults,
  };
}

/**
 * Generate certificate chain linking observations to merge
 *
 * Creates proof path showing how observations are included in merge.
 *
 * @param {Object} mergeReceipt - ProbeMergeReceipt
 * @param {Object<string, Array>} agentChains - Map of agentId -> chain
 * @returns {Array<Object>} Certificate chain steps
 */
export function generateCertificateChain(mergeReceipt, agentChains) {
  const chain = [];

  // Add all observations
  for (const shard of mergeReceipt.shards) {
    const observations = agentChains[shard.agentId] || [];

    for (const obs of observations) {
      chain.push({
        receiptHash: obs.receiptHash,
        receiptType: 'probe-observation',
        relationship: `obs-${obs.observationIndex}-in-chain-${shard.agentId}`,
        context: {
          agentId: shard.agentId,
          observationIndex: obs.observationIndex,
        },
      });
    }
  }

  // Add merge receipt
  chain.push({
    receiptHash: mergeReceipt.receiptHash,
    receiptType: 'probe-merge',
    relationship: 'merge-verified',
    context: {
      mergeId: mergeReceipt.mergeId,
    },
  });

  return chain;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  verifyProbeMerge,
  verifyProbeMergeComplete,
  generateCertificateChain,
};
