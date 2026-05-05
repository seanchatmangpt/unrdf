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
export function createProbeObservationReceipt(event: {
    agentId: string;
    observationIndex: number;
    payload: any;
    domain: string;
}, previousReceipt?: any): Promise<any>;
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
export function createProbeMergeReceipt(event: {
    mergeId: string;
    shards: any[];
}, hashFunction?: Function): Promise<any>;
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
export function createProbeVerificationReceipt(event: {
    verificationId: string;
    mergeReceiptHash: string;
    verifications: any[];
    deterministic: boolean;
    conflictFree: boolean;
    certificateChain: any[];
    obsCount: number;
    agentCount: number;
}): Promise<any>;
declare namespace _default {
    export { createProbeObservationReceipt };
    export { createProbeMergeReceipt };
    export { createProbeVerificationReceipt };
    export { PROBE_OBSERVATION_TYPE };
    export { PROBE_MERGE_TYPE };
    export { PROBE_VERIFICATION_TYPE };
}
export default _default;
import ProbeObservationReceiptSchema from './observation-receipt.mjs';
import ProbeMergeReceiptSchema from './merge-receipt.mjs';
import ProbeVerificationReceiptSchema from './verification-receipt.mjs';
import { PROBE_OBSERVATION_TYPE } from './observation-receipt.mjs';
import { PROBE_MERGE_TYPE } from './merge-receipt.mjs';
import { PROBE_VERIFICATION_TYPE } from './verification-receipt.mjs';
import { MERGE_ALGORITHM } from './merge-receipt.mjs';
import { MERGE_ALGORITHM_VERSION } from './merge-receipt.mjs';
import { LEAF_ORDER } from './merge-receipt.mjs';
import { computeObsHash } from './observation-receipt.mjs';
import { checkHashRecompute } from './observation-receipt.mjs';
import { checkSerializationStable } from './observation-receipt.mjs';
import { checkPayloadIntegrity } from './observation-receipt.mjs';
import { buildMerkleTree } from './merge-receipt.mjs';
import { verifyMerkleRoot } from './merge-receipt.mjs';
import { summarizeVerification } from './verification-receipt.mjs';
import { getFailedChecks } from './verification-receipt.mjs';
import { getConfidenceScore } from './verification-receipt.mjs';
export { ProbeObservationReceiptSchema, ProbeMergeReceiptSchema, ProbeVerificationReceiptSchema, PROBE_OBSERVATION_TYPE, PROBE_MERGE_TYPE, PROBE_VERIFICATION_TYPE, MERGE_ALGORITHM, MERGE_ALGORITHM_VERSION, LEAF_ORDER, computeObsHash, checkHashRecompute, checkSerializationStable, checkPayloadIntegrity, buildMerkleTree, verifyMerkleRoot, summarizeVerification, getFailedChecks, getConfidenceScore };
