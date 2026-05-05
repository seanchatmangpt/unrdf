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
export function verifyProbeChain(chain: Array<any>, agentId: string): Promise<any>;
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
export function verifyProbeChains(chains: {
    [x: string]: any[];
}): Promise<any>;
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
export function extractShardInfo(chains: {
    [x: string]: any[];
}): Array<any>;
declare namespace _default {
    export { verifyProbeChain };
    export { verifyProbeChains };
    export { extractShardInfo };
}
export default _default;
