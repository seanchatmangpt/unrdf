/**
 * @fileoverview Receipt Chain Data Fixtures
 *
 * Sample observation chains with:
 * - 3 agents x 5 observations each
 * - Expected hashes at each link
 * - Merkle tree examples
 * - Verification proofs
 *
 * @module @unrdf/kgc-probe/test/fixtures/receipt-chain-data
 */

import { FROZEN_TIMESTAMP } from './frozen-environment.mjs';

// ============================================================================
// HASH UTILITIES
// ============================================================================

/**
 * Simple deterministic hash for testing
 * @param {string} data - Data to hash
 * @returns {string} 64-char hex hash
 */
export function simpleHash(data) {
  let hash = 0;
  for (let i = 0; i < data.length; i++) {
    const char = data.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash;
  }
  return Math.abs(hash).toString(16).padStart(64, '0').slice(0, 64);
}

/**
 * Hash observation for chain
 * @param {Object} observation - Observation to hash
 * @returns {string} Hash string
 */
export function hashObservation(observation) {
  const data = JSON.stringify({
    id: observation.id,
    agent: observation.agent,
    timestamp: observation.timestamp,
    kind: observation.kind,
    subject: observation.subject,
    predicate: observation.predicate,
    object: observation.object
  });
  return simpleHash(data);
}

// ============================================================================
// OBSERVATION CHAINS (3 agents x 5 observations)
// ============================================================================

/**
 * Generate observation for chain
 */
function createChainObservation(agent, index, prevHash) {
  const id = `${agent}-obs-${String(index).padStart(3, '0')}`;
  const observation = {
    id,
    agent,
    timestamp: new Date(Date.parse(FROZEN_TIMESTAMP) + index * 1000).toISOString(),
    kind: 'chain_test',
    severity: 'info',
    subject: `chain:${agent}:${index}`,
    predicate: 'chain:sequence',
    object: String(index),
    evidence: {
      query: `chain_${agent}_${index}`,
      result: { index },
      witnesses: []
    },
    metrics: {
      confidence: 0.95,
      coverage: 0.90,
      latency_ms: 10 + index
    },
    tags: ['chain', agent],
    receipt: {
      prev_hash: prevHash,
      self_hash: '', // Will be filled
      chain_position: index
    }
  };

  // Calculate self hash
  observation.receipt.self_hash = hashObservation(observation);

  return observation;
}

/**
 * Agent 1: Completion Agent Chain
 */
export const CHAIN_AGENT_1 = (() => {
  const chain = [];
  let prevHash = '0'.repeat(64); // Genesis

  for (let i = 0; i < 5; i++) {
    const obs = createChainObservation('completion', i, prevHash);
    chain.push(obs);
    prevHash = obs.receipt.self_hash;
  }

  return chain;
})();

/**
 * Agent 2: Consistency Agent Chain
 */
export const CHAIN_AGENT_2 = (() => {
  const chain = [];
  let prevHash = '0'.repeat(64);

  for (let i = 0; i < 5; i++) {
    const obs = createChainObservation('consistency', i, prevHash);
    chain.push(obs);
    prevHash = obs.receipt.self_hash;
  }

  return chain;
})();

/**
 * Agent 3: Conformance Agent Chain
 */
export const CHAIN_AGENT_3 = (() => {
  const chain = [];
  let prevHash = '0'.repeat(64);

  for (let i = 0; i < 5; i++) {
    const obs = createChainObservation('conformance', i, prevHash);
    chain.push(obs);
    prevHash = obs.receipt.self_hash;
  }

  return chain;
})();

/**
 * All chains combined
 */
export const ALL_CHAINS = [CHAIN_AGENT_1, CHAIN_AGENT_2, CHAIN_AGENT_3];

// ============================================================================
// MERKLE TREE IMPLEMENTATION
// ============================================================================

/**
 * Build merkle tree from observations
 * @param {Array} observations - Observations to tree
 * @returns {Object} Merkle tree with root and proofs
 */
export function buildMerkleTree(observations) {
  if (observations.length === 0) {
    return {
      root: '0'.repeat(64),
      leaves: [],
      layers: [],
      proofs: new Map()
    };
  }

  // Create leaf hashes
  const leaves = observations.map(obs => hashObservation(obs));

  // Build tree layers
  const layers = [leaves];
  let currentLayer = leaves;

  while (currentLayer.length > 1) {
    const nextLayer = [];
    for (let i = 0; i < currentLayer.length; i += 2) {
      const left = currentLayer[i];
      const right = currentLayer[i + 1] || left; // Duplicate if odd
      nextLayer.push(simpleHash(left + right));
    }
    layers.push(nextLayer);
    currentLayer = nextLayer;
  }

  const root = currentLayer[0];

  // Generate proofs for each leaf
  const proofs = new Map();
  for (let i = 0; i < leaves.length; i++) {
    proofs.set(i, generateMerkleProof(layers, i));
  }

  return {
    root,
    leaves,
    layers,
    proofs
  };
}

/**
 * Generate merkle proof for leaf at index
 * @param {Array[]} layers - Tree layers
 * @param {number} index - Leaf index
 * @returns {Array} Proof path
 */
function generateMerkleProof(layers, index) {
  const proof = [];
  let currentIndex = index;

  for (let i = 0; i < layers.length - 1; i++) {
    const layer = layers[i];
    const isRight = currentIndex % 2 === 1;
    const siblingIndex = isRight ? currentIndex - 1 : currentIndex + 1;

    if (siblingIndex < layer.length) {
      proof.push({
        hash: layer[siblingIndex],
        position: isRight ? 'left' : 'right'
      });
    }

    currentIndex = Math.floor(currentIndex / 2);
  }

  return proof;
}

/**
 * Verify merkle proof
 * @param {string} leafHash - Hash of leaf
 * @param {Array} proof - Proof path
 * @param {string} root - Expected root
 * @returns {boolean} True if valid
 */
export function verifyMerkleProof(leafHash, proof, root) {
  let currentHash = leafHash;

  for (const { hash, position } of proof) {
    if (position === 'left') {
      currentHash = simpleHash(hash + currentHash);
    } else {
      currentHash = simpleHash(currentHash + hash);
    }
  }

  return currentHash === root;
}

// ============================================================================
// PRECOMPUTED EXAMPLES
// ============================================================================

/**
 * Merkle tree for Agent 1 chain
 */
export const MERKLE_TREE_AGENT_1 = buildMerkleTree(CHAIN_AGENT_1);

/**
 * Merkle tree for Agent 2 chain
 */
export const MERKLE_TREE_AGENT_2 = buildMerkleTree(CHAIN_AGENT_2);

/**
 * Merkle tree for Agent 3 chain
 */
export const MERKLE_TREE_AGENT_3 = buildMerkleTree(CHAIN_AGENT_3);

/**
 * Combined merkle tree (all observations)
 */
export const MERKLE_TREE_COMBINED = buildMerkleTree([
  ...CHAIN_AGENT_1,
  ...CHAIN_AGENT_2,
  ...CHAIN_AGENT_3
]);

/**
 * Expected hash at each chain link
 */
export const EXPECTED_CHAIN_HASHES = {
  agent_1: CHAIN_AGENT_1.map(obs => ({
    position: obs.receipt.chain_position,
    prev_hash: obs.receipt.prev_hash,
    self_hash: obs.receipt.self_hash
  })),
  agent_2: CHAIN_AGENT_2.map(obs => ({
    position: obs.receipt.chain_position,
    prev_hash: obs.receipt.prev_hash,
    self_hash: obs.receipt.self_hash
  })),
  agent_3: CHAIN_AGENT_3.map(obs => ({
    position: obs.receipt.chain_position,
    prev_hash: obs.receipt.prev_hash,
    self_hash: obs.receipt.self_hash
  }))
};

/**
 * Verification test cases
 */
export const VERIFICATION_CASES = [
  {
    name: 'Valid chain - Agent 1',
    chain: CHAIN_AGENT_1,
    expected_valid: true
  },
  {
    name: 'Valid chain - Agent 2',
    chain: CHAIN_AGENT_2,
    expected_valid: true
  },
  {
    name: 'Valid chain - Agent 3',
    chain: CHAIN_AGENT_3,
    expected_valid: true
  },
  {
    name: 'Tampered chain - Modified observation',
    chain: (() => {
      const tampered = JSON.parse(JSON.stringify(CHAIN_AGENT_1));
      tampered[2].subject = 'tampered:subject';
      return tampered;
    })(),
    expected_valid: false
  },
  {
    name: 'Broken chain - Wrong prev_hash',
    chain: (() => {
      const broken = JSON.parse(JSON.stringify(CHAIN_AGENT_2));
      broken[3].receipt.prev_hash = 'x'.repeat(64);
      return broken;
    })(),
    expected_valid: false
  }
];

/**
 * Verify observation chain integrity
 * @param {Array} chain - Chain to verify
 * @returns {Object} Verification result
 */
export function verifyChain(chain) {
  if (chain.length === 0) {
    return { valid: true, errors: [] };
  }

  const errors = [];

  // Check first observation has genesis prev_hash
  if (chain[0].receipt.prev_hash !== '0'.repeat(64)) {
    errors.push('First observation must have genesis prev_hash (all zeros)');
  }

  // Verify chain links
  for (let i = 0; i < chain.length; i++) {
    const obs = chain[i];

    // Recompute hash
    const computedHash = hashObservation(obs);
    if (computedHash !== obs.receipt.self_hash) {
      errors.push(`Observation ${i}: self_hash mismatch`);
    }

    // Verify prev_hash link
    if (i > 0) {
      const prevObs = chain[i - 1];
      if (obs.receipt.prev_hash !== prevObs.receipt.self_hash) {
        errors.push(`Observation ${i}: prev_hash does not match previous self_hash`);
      }
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

export default {
  simpleHash,
  hashObservation,
  buildMerkleTree,
  verifyMerkleProof,
  verifyChain,
  CHAIN_AGENT_1,
  CHAIN_AGENT_2,
  CHAIN_AGENT_3,
  ALL_CHAINS,
  MERKLE_TREE_AGENT_1,
  MERKLE_TREE_AGENT_2,
  MERKLE_TREE_AGENT_3,
  MERKLE_TREE_COMBINED,
  EXPECTED_CHAIN_HASHES,
  VERIFICATION_CASES
};
