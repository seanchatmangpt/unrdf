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
export function verifyProbeMerge(receipt: any, agentChains: {
    [x: string]: any[];
}): Promise<any>;
/**
 * Verify merge by recomputing from original chains
 *
 * End-to-end verification: verify all chains + verify merge against chains.
 *
 * @param {Object} mergeReceipt - ProbeMergeReceipt
 * @param {Object<string, Array>} agentChains - Map of agentId -> chain
 * @returns {Promise<Object>} Complete verification result
 */
export function verifyProbeMergeComplete(mergeReceipt: any, agentChains: {
    [x: string]: any[];
}): Promise<any>;
/**
 * Generate certificate chain linking observations to merge
 *
 * Creates proof path showing how observations are included in merge.
 *
 * @param {Object} mergeReceipt - ProbeMergeReceipt
 * @param {Object<string, Array>} agentChains - Map of agentId -> chain
 * @returns {Array<Object>} Certificate chain steps
 */
export function generateCertificateChain(mergeReceipt: any, agentChains: {
    [x: string]: any[];
}): Array<any>;
declare namespace _default {
    export { verifyProbeMerge };
    export { verifyProbeMergeComplete };
    export { generateCertificateChain };
}
export default _default;
