/**
 * Probe Merge Receipt - Deterministic merge proof with merkle tree
 *
 * Orchestrator creates one merge receipt per operation, proving that merge
 * was deterministic and conflict-free using a merkle tree over agent hashes.
 *
 * @module @unrdf/v6-core/receipts/probe/merge-receipt
 */

import { z } from 'zod';
import {
  BaseReceiptSchema,
  RECEIPT_TYPES,
  BLAKE3_HEX_LENGTH,
} from '../base-receipt.mjs';

// =============================================================================
// Constants
// =============================================================================

/**
 * Probe merge receipt type discriminator
 * @constant {string}
 */
export const PROBE_MERGE_TYPE = 'probe-merge';

/**
 * Merge algorithm name
 * @constant {string}
 */
export const MERGE_ALGORITHM = 'merkle-tree-deterministic';

/**
 * Merge algorithm version
 * @constant {string}
 */
export const MERGE_ALGORITHM_VERSION = '1.0.0';

/**
 * Leaf ordering for merkle tree
 * @constant {string}
 */
export const LEAF_ORDER = 'sorted-by-agentId';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Shard information schema - per-agent chain reference
 */
export const ShardInfoSchema = z.object({
  /** Agent/shard identifier */
  agentId: z.string().min(1),

  /** Final hash of this agent's observation chain */
  chainFinalHash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Number of observations in this agent's chain */
  obsCount: z.number().int().nonnegative(),

  /** Domain/category this agent observed */
  domain: z.string().min(1),
});

/**
 * Merge algorithm descriptor schema
 */
export const MergeAlgorithmSchema = z.object({
  /** Algorithm name */
  algorithm: z.string(),

  /** Algorithm version */
  version: z.string(),

  /** Algorithm parameters (leaf ordering, hash function, etc.) */
  parameters: z.object({
    leafOrder: z.string().optional(),
    hashFunction: z.string().optional(),
  }).passthrough().optional(),
});

/**
 * Conflict descriptor schema
 */
export const ConflictSchema = z.object({
  /** Type of conflict found */
  type: z.enum(['hash-divergence', 'temporal-anomaly', 'domain-overlap', 'inconsistency']),

  /** Agents involved in conflict */
  agents: z.array(z.string()),

  /** Human-readable description */
  description: z.string(),

  /** Suggested resolution (optional) */
  resolution: z.string().optional(),
});

/**
 * Merkle proof step schema
 */
export const MerkleProofStepSchema = z.object({
  /** Tree level (0 = leaves) */
  level: z.number().int().nonnegative(),

  /** Position in proof (left or right) */
  position: z.enum(['left', 'right']),

  /** Hash at this step */
  hash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Computed parent (optional, for verification) */
  parent: z.string().length(BLAKE3_HEX_LENGTH).optional(),
});

/**
 * Probe merge receipt schema - extends base with merge-specific fields
 */
export const ProbeMergeReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(PROBE_MERGE_TYPE),

  // Merge identification
  /** Unique identifier for this merge operation */
  mergeId: z.string().min(1),

  // Shard information
  /** Array of per-agent chain references */
  shards: z.array(ShardInfoSchema).min(1),

  // Merkle proof
  /** Merkle root hash (root of tree over shard.chainFinalHash) */
  merkleRoot: z.string().length(BLAKE3_HEX_LENGTH),

  /** Path to verify merkle root */
  proofPath: z.array(MerkleProofStepSchema),

  // Algorithm descriptor
  /** Merge algorithm information */
  mergeAlgorithm: MergeAlgorithmSchema,

  // Conflict information
  /**
   * Conflicts found during merge
   * null = no conflicts (deterministic + conflict-free)
   * [] = no conflicts (alternative representation)
   * [...] = conflicts found (merge not deterministic)
   */
  conflicts: z.array(ConflictSchema).nullable(),

  // Audit trail
  /** Timestamp of merge computation */
  mergedAt: z.string().optional(),

  /** Orchestrator identifier (optional) */
  orchestratorId: z.string().optional(),
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} ShardInfo
 * @property {string} agentId - Agent identifier
 * @property {string} chainFinalHash - Final hash of agent's chain
 * @property {number} obsCount - Number of observations
 * @property {string} domain - Domain/category
 */

/**
 * @typedef {Object} MergeAlgorithm
 * @property {string} algorithm - Algorithm name
 * @property {string} version - Version
 * @property {Object} [parameters] - Parameters
 * @property {string} [parameters.leafOrder] - Leaf ordering method
 * @property {string} [parameters.hashFunction] - Hash function used
 */

/**
 * @typedef {Object} Conflict
 * @property {'hash-divergence'|'temporal-anomaly'|'domain-overlap'|'inconsistency'} type
 * @property {string[]} agents - Agents involved
 * @property {string} description - Description
 * @property {string} [resolution] - Suggested resolution
 */

/**
 * @typedef {Object} MerkleProofStep
 * @property {number} level - Tree level
 * @property {'left'|'right'} position - Position
 * @property {string} hash - Hash at this step
 * @property {string} [parent] - Computed parent
 */

/**
 * @typedef {Object} ProbeMergeReceipt
 * @property {string} id - UUID of receipt
 * @property {'probe-merge'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} mergeId - Merge operation ID
 * @property {ShardInfo[]} shards - Per-agent chain references
 * @property {string} merkleRoot - Merkle root hash
 * @property {MerkleProofStep[]} proofPath - Merkle proof path
 * @property {MergeAlgorithm} mergeAlgorithm - Algorithm descriptor
 * @property {Conflict[]|null} conflicts - Conflicts found or null
 * @property {string} [mergedAt] - Merge timestamp
 * @property {string} [orchestratorId] - Orchestrator ID
 */

// =============================================================================
// Merkle Tree Utilities
// =============================================================================

/**
 * Build merkle tree and compute root
 *
 * Deterministically sorts shards and builds tree bottom-up.
 *
 * @param {ShardInfo[]} shards - Array of shard info objects
 * @param {Function} hashFunction - Hash function to use
 * @returns {Promise<{root: string, proofPath: MerkleProofStep[]}>}
 */
export async function buildMerkleTree(shards, hashFunction) {
  if (!shards || shards.length === 0) {
    throw new Error('Cannot build merkle tree with no shards');
  }

  // Sort shards deterministically by agentId
  const sortedShards = [...shards].sort((a, b) => a.agentId.localeCompare(b.agentId));
  const leaves = sortedShards.map((s) => s.chainFinalHash);

  let currentLevel = leaves;
  let level = 0;
  const proofPath = [];

  // Build tree bottom-up
  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1] || left; // Duplicate if odd

      // Record in proof path
      proofPath.push({
        level,
        position: 'left',
        hash: left,
      });

      if (right !== left) {
        proofPath.push({
          level,
          position: 'right',
          hash: right,
        });
      }

      // Compute parent
      const pairInput = left + right;
      const parent = await hashFunction(pairInput);
      nextLevel.push(parent);
    }

    currentLevel = nextLevel;
    level += 1;
  }

  return {
    root: currentLevel[0],
    proofPath,
  };
}

/**
 * Verify merkle root against shards
 *
 * Rebuilds tree and compares computed root with expected.
 *
 * @param {string} expectedRoot - Expected merkle root
 * @param {ShardInfo[]} shards - Shard info array
 * @param {Function} hashFunction - Hash function to use
 * @returns {Promise<{valid: boolean, computedRoot: string}>}
 */
export async function verifyMerkleRoot(expectedRoot, shards, hashFunction) {
  const { root: computedRoot } = await buildMerkleTree(shards, hashFunction);
  return {
    valid: computedRoot === expectedRoot,
    computedRoot,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default ProbeMergeReceiptSchema;
