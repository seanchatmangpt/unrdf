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
/**
 * Build merkle tree and compute root
 *
 * Deterministically sorts shards and builds tree bottom-up.
 *
 * @param {ShardInfo[]} shards - Array of shard info objects
 * @param {Function} hashFunction - Hash function to use
 * @returns {Promise<{root: string, proofPath: MerkleProofStep[]}>}
 */
export function buildMerkleTree(shards: ShardInfo[], hashFunction: Function): Promise<{
    root: string;
    proofPath: MerkleProofStep[];
}>;
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
export function verifyMerkleRoot(expectedRoot: string, shards: ShardInfo[], hashFunction: Function): Promise<{
    valid: boolean;
    computedRoot: string;
}>;
/**
 * Probe merge receipt type discriminator
 * @constant {string}
 */
export const PROBE_MERGE_TYPE: "probe-merge";
/**
 * Merge algorithm name
 * @constant {string}
 */
export const MERGE_ALGORITHM: "merkle-tree-deterministic";
/**
 * Merge algorithm version
 * @constant {string}
 */
export const MERGE_ALGORITHM_VERSION: "1.0.0";
/**
 * Leaf ordering for merkle tree
 * @constant {string}
 */
export const LEAF_ORDER: "sorted-by-agentId";
/**
 * Shard information schema - per-agent chain reference
 */
export const ShardInfoSchema: z.ZodObject<{
    agentId: z.ZodString;
    chainFinalHash: z.ZodString;
    obsCount: z.ZodNumber;
    domain: z.ZodString;
}, z.core.$strip>;
/**
 * Merge algorithm descriptor schema
 */
export const MergeAlgorithmSchema: z.ZodObject<{
    algorithm: z.ZodString;
    version: z.ZodString;
    parameters: z.ZodOptional<z.ZodObject<{
        leafOrder: z.ZodOptional<z.ZodString>;
        hashFunction: z.ZodOptional<z.ZodString>;
    }, z.core.$loose>>;
}, z.core.$strip>;
/**
 * Conflict descriptor schema
 */
export const ConflictSchema: z.ZodObject<{
    type: z.ZodEnum<{
        "hash-divergence": "hash-divergence";
        "temporal-anomaly": "temporal-anomaly";
        "domain-overlap": "domain-overlap";
        inconsistency: "inconsistency";
    }>;
    agents: z.ZodArray<z.ZodString>;
    description: z.ZodString;
    resolution: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Merkle proof step schema
 */
export const MerkleProofStepSchema: z.ZodObject<{
    level: z.ZodNumber;
    position: z.ZodEnum<{
        left: "left";
        right: "right";
    }>;
    hash: z.ZodString;
    parent: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Probe merge receipt schema - extends base with merge-specific fields
 */
export const ProbeMergeReceiptSchema: z.ZodObject<{
    id: z.ZodString;
    t_ns: z.ZodBigInt;
    timestamp_iso: z.ZodString;
    previousHash: z.ZodNullable<z.ZodString>;
    payloadHash: z.ZodString;
    receiptHash: z.ZodString;
    attestation: z.ZodOptional<z.ZodObject<{
        algorithm: z.ZodString;
        publicKey: z.ZodString;
        signature: z.ZodString;
        signer: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
    vectorClock: z.ZodOptional<z.ZodObject<{
        nodeId: z.ZodString;
        counters: z.ZodRecord<z.ZodString, z.ZodString>;
    }, z.core.$loose>>;
    gitRef: z.ZodOptional<z.ZodString>;
    kgcEventId: z.ZodOptional<z.ZodString>;
    receiptType: z.ZodLiteral<"probe-merge">;
    mergeId: z.ZodString;
    shards: z.ZodArray<z.ZodObject<{
        agentId: z.ZodString;
        chainFinalHash: z.ZodString;
        obsCount: z.ZodNumber;
        domain: z.ZodString;
    }, z.core.$strip>>;
    merkleRoot: z.ZodString;
    proofPath: z.ZodArray<z.ZodObject<{
        level: z.ZodNumber;
        position: z.ZodEnum<{
            left: "left";
            right: "right";
        }>;
        hash: z.ZodString;
        parent: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
    mergeAlgorithm: z.ZodObject<{
        algorithm: z.ZodString;
        version: z.ZodString;
        parameters: z.ZodOptional<z.ZodObject<{
            leafOrder: z.ZodOptional<z.ZodString>;
            hashFunction: z.ZodOptional<z.ZodString>;
        }, z.core.$loose>>;
    }, z.core.$strip>;
    conflicts: z.ZodNullable<z.ZodArray<z.ZodObject<{
        type: z.ZodEnum<{
            "hash-divergence": "hash-divergence";
            "temporal-anomaly": "temporal-anomaly";
            "domain-overlap": "domain-overlap";
            inconsistency: "inconsistency";
        }>;
        agents: z.ZodArray<z.ZodString>;
        description: z.ZodString;
        resolution: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>>;
    mergedAt: z.ZodOptional<z.ZodString>;
    orchestratorId: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
export default ProbeMergeReceiptSchema;
export type ShardInfo = {
    /**
     * - Agent identifier
     */
    agentId: string;
    /**
     * - Final hash of agent's chain
     */
    chainFinalHash: string;
    /**
     * - Number of observations
     */
    obsCount: number;
    /**
     * - Domain/category
     */
    domain: string;
};
export type MergeAlgorithm = {
    /**
     * - Algorithm name
     */
    algorithm: string;
    /**
     * - Version
     */
    version: string;
    /**
     * - Parameters
     */
    parameters?: {
        leafOrder?: string;
        hashFunction?: string;
    };
};
export type Conflict = {
    type: "hash-divergence" | "temporal-anomaly" | "domain-overlap" | "inconsistency";
    /**
     * - Agents involved
     */
    agents: string[];
    /**
     * - Description
     */
    description: string;
    /**
     * - Suggested resolution
     */
    resolution?: string;
};
export type MerkleProofStep = {
    /**
     * - Tree level
     */
    level: number;
    /**
     * - Position
     */
    position: "left" | "right";
    /**
     * - Hash at this step
     */
    hash: string;
    /**
     * - Computed parent
     */
    parent?: string;
};
export type ProbeMergeReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type
     */
    receiptType: "probe-merge";
    /**
     * - Nanosecond timestamp
     */
    t_ns: bigint;
    /**
     * - ISO timestamp
     */
    timestamp_iso: string;
    /**
     * - Previous receipt hash
     */
    previousHash: string | null;
    /**
     * - Payload hash
     */
    payloadHash: string;
    /**
     * - Receipt hash
     */
    receiptHash: string;
    /**
     * - Merge operation ID
     */
    mergeId: string;
    /**
     * - Per-agent chain references
     */
    shards: ShardInfo[];
    /**
     * - Merkle root hash
     */
    merkleRoot: string;
    /**
     * - Merkle proof path
     */
    proofPath: MerkleProofStep[];
    /**
     * - Algorithm descriptor
     */
    mergeAlgorithm: MergeAlgorithm;
    /**
     * - Conflicts found or null
     */
    conflicts: Conflict[] | null;
    /**
     * - Merge timestamp
     */
    mergedAt?: string;
    /**
     * - Orchestrator ID
     */
    orchestratorId?: string;
};
import { z } from 'zod';
