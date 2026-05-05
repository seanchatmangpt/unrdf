/**
 * Anchor merkle root to blockchain (STUBBED)
 *
 * In production, this would submit a transaction to Ethereum or other chain.
 * For now, generates a mock transaction receipt for testing.
 *
 * @param {string} merkleRoot - Merkle root hash to anchor
 * @param {Object} chainConfig - Blockchain configuration
 * @param {Object} [context={}] - Execution context with t_ns for determinism
 * @returns {Promise<Object>} Anchor receipt
 *
 * @example
 * const anchorReceipt = await anchorToChain(merkleRoot, {
 *   network: 'goerli',
 *   contractAddress: '0x...'
 * }, { t_ns: 1234567890000000000n });
 */
export function anchorToChain(merkleRoot: string, chainConfig: any, context?: any): Promise<any>;
/**
 * Verify anchor receipt
 *
 * Checks if the merkle root was anchored on-chain.
 *
 * @param {string} merkleRoot - Merkle root to verify
 * @param {Object} anchorReceipt - Anchor receipt to verify
 * @returns {Promise<boolean>} True if anchor is valid
 *
 * @example
 * const isValid = await verifyAnchor(merkleRoot, anchorReceipt);
 */
export function verifyAnchor(merkleRoot: string, anchorReceipt: any): Promise<boolean>;
/**
 * Create anchor receipt object
 *
 * @param {string} root - Merkle root hash
 * @param {string} txHash - Transaction hash
 * @param {Object} [opts={}] - Optional fields
 * @param {Object} [context={}] - Execution context with t_ns for determinism
 * @returns {Object} Anchor receipt
 */
export function createAnchorReceipt(root: string, txHash: string, opts?: any, context?: any): any;
/**
 * Anchor receipt schema
 */
export const AnchorReceiptSchema: z.ZodObject<{
    merkleRoot: z.ZodString;
    txHash: z.ZodString;
    blockNumber: z.ZodNumber;
    network: z.ZodString;
    timestamp: z.ZodNumber;
    receiptCount: z.ZodNumber;
}, z.core.$strip>;
/**
 * Chain config schema
 */
export const ChainConfigSchema: z.ZodObject<{
    network: z.ZodDefault<z.ZodString>;
    rpcUrl: z.ZodOptional<z.ZodString>;
    contractAddress: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
declare namespace _default {
    export { anchorToChain };
    export { verifyAnchor };
    export { createAnchorReceipt };
}
export default _default;
import { z } from 'zod';
