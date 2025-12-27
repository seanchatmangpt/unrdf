/**
 * Probe Chain Verifier
 *
 * Verifies per-agent observation chains for integrity and immutability.
 *
 * @module @unrdf/v6-core/receipts/probe/verifiers/chain-verifier
 */

import { verifyProbeObservation } from './observation-verifier.mjs';

// =============================================================================
// Chain Verification
// =============================================================================

/**
 * Verify an agent's observation chain
 *
 * Checks:
 * 1. Genesis observation has no previous
 * 2. Chain links are valid (prevHash chain)
 * 3. All observations verified individually
 * 4. Temporal ordering is monotonic
 *
 * @param {Array<Object>} chain - Array of ProbeObservationReceipt objects
 * @param {string} agentId - Expected agent ID
 * @returns {Promise<Object>} Chain verification result
 *
 * @example
 * const result = await verifyProbeChain(chain, 'agent-1');
 * console.log(result.valid); // true if chain is valid
 * console.log(result.chainFinalHash); // Last receipt's receiptHash
 */
export async function verifyProbeChain(chain, agentId) {
  const errors = [];
  const checks = {
    genesisValid: false,
    chainLinksValid: false,
    observationsValid: false,
    orderingValid: false,
  };

  // Input validation
  if (!chain || !Array.isArray(chain) || chain.length === 0) {
    errors.push('Chain must be a non-empty array');
    return { valid: false, checks, errors };
  }

  // 1. Verify genesis observation
  const first = chain[0];

  if (first.observationIndex !== 1) {
    errors.push('Genesis observation must have observationIndex = 1');
  }

  if (first.prevHash !== null) {
    errors.push('Genesis observation must have prevHash = null');
  }

  if (first.previousHash !== null) {
    errors.push('Genesis observation must have previousHash = null');
  }

  if (first.agentId !== agentId) {
    errors.push(`Agent ID mismatch: expected ${agentId}, got ${first.agentId}`);
  }

  if (errors.length === 0) {
    checks.genesisValid = true;
  } else {
    return { valid: false, checks, errors };
  }

  // 2. Verify chain links (per-agent hash chain)
  const chainLinkErrors = [];

  for (let i = 1; i < chain.length; i++) {
    const current = chain[i];
    const previous = chain[i - 1];

    // Check agent consistency
    if (current.agentId !== agentId) {
      chainLinkErrors.push(`Agent ID mismatch at index ${i}`);
      continue;
    }

    // Check observation index sequence
    if (current.observationIndex !== previous.observationIndex + 1) {
      chainLinkErrors.push(`Index sequence broken at ${i}: expected ${previous.observationIndex + 1}, got ${current.observationIndex}`);
      continue;
    }

    // Check prevHash links to previous obsHash
    if (current.prevHash !== previous.obsHash) {
      chainLinkErrors.push(`Per-agent chain broken at ${i}: prevHash ${current.prevHash} != previous.obsHash ${previous.obsHash}`);
      continue;
    }

    // Check previousHash links to previous receiptHash (v6-core chain)
    if (current.previousHash !== previous.receiptHash) {
      chainLinkErrors.push(`Receipt chain broken at ${i}: previousHash ${current.previousHash} != previous.receiptHash ${previous.receiptHash}`);
      continue;
    }
  }

  if (chainLinkErrors.length === 0) {
    checks.chainLinksValid = true;
  } else {
    errors.push(...chainLinkErrors);
  }

  // 3. Verify all observations
  const obsErrors = [];

  for (let i = 0; i < chain.length; i++) {
    const receipt = chain[i];
    const obsVerification = await verifyProbeObservation(receipt);

    if (!obsVerification.valid) {
      obsErrors.push(`Observation ${i} verification failed: ${obsVerification.errors.join(', ')}`);
    }
  }

  if (obsErrors.length === 0) {
    checks.observationsValid = true;
  } else {
    errors.push(...obsErrors);
  }

  // 4. Verify temporal ordering
  const orderingErrors = [];

  for (let i = 1; i < chain.length; i++) {
    const current = chain[i];
    const previous = chain[i - 1];

    if (current.t_ns <= previous.t_ns) {
      orderingErrors.push(`Temporal violation at index ${i}: ${current.t_ns} <= ${previous.t_ns}`);
    }

    if (current.observation.timestamp <= previous.observation.timestamp) {
      orderingErrors.push(`Observation timestamp violation at index ${i}`);
    }
  }

  if (orderingErrors.length === 0) {
    checks.orderingValid = true;
  } else {
    errors.push(...orderingErrors);
  }

  // Build result
  const valid = errors.length === 0;
  const chainFinalHash = chain[chain.length - 1].receiptHash;

  return {
    valid,
    checks,
    errors,
    chainLength: chain.length,
    chainFinalHash,
    agentId,
  };
}

/**
 * Verify multiple agent chains together
 *
 * @param {Object<string, Array>} chains - Map of agentId -> chain
 * @returns {Promise<Object>} Multi-chain verification result
 *
 * @example
 * const chains = {
 *   'agent-1': [...],
 *   'agent-2': [...],
 * };
 * const result = await verifyProbeChains(chains);
 */
export async function verifyProbeChains(chains) {
  const errors = [];
  const results = {};
  let allValid = true;
  let totalObservations = 0;

  if (!chains || Object.keys(chains).length === 0) {
    errors.push('No chains provided');
    return { valid: false, errors };
  }

  // Verify each chain
  for (const [agentId, chain] of Object.entries(chains)) {
    const result = await verifyProbeChain(chain, agentId);

    results[agentId] = result;
    totalObservations += result.chainLength || 0;

    if (!result.valid) {
      allValid = false;
      errors.push(`Agent ${agentId}: ${result.errors.join('; ')}`);
    }
  }

  return {
    valid: allValid,
    agentCount: Object.keys(chains).length,
    totalObservations,
    results,
    errors,
  };
}

/**
 * Extract shard information from chains
 *
 * Useful for building shard arrays for merge receipts.
 *
 * @param {Object<string, Array>} chains - Map of agentId -> chain
 * @returns {Array<Object>} Array of shard info objects
 *
 * @example
 * const shards = extractShardInfo(chains);
 * // Returns: [
 * //   { agentId: 'agent-1', chainFinalHash: '...', obsCount: 3, domain: 'network' },
 * //   ...
 * // ]
 */
export function extractShardInfo(chains) {
  const shards = [];

  for (const [agentId, chain] of Object.entries(chains)) {
    if (chain.length === 0) continue;

    const lastReceipt = chain[chain.length - 1];
    const domain = lastReceipt.domain || 'unknown';

    shards.push({
      agentId,
      chainFinalHash: lastReceipt.receiptHash,
      obsCount: chain.length,
      domain,
    });
  }

  // Sort by agentId for determinism
  shards.sort((a, b) => a.agentId.localeCompare(b.agentId));

  return shards;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  verifyProbeChain,
  verifyProbeChains,
  extractShardInfo,
};
