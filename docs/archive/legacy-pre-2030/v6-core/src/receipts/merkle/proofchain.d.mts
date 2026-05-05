/**
 * Verify entire receipt chain
 *
 * Checks:
 * 1. Each receipt's hash integrity
 * 2. Chain links (previousHash references)
 * 3. Temporal ordering (monotonic timestamps)
 *
 * @param {Array<Object>} receipts - Array of receipts to verify
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyChain([receipt1, receipt2, receipt3]);
 * console.log(result.valid); // true if entire chain is valid
 * console.log(result.tamperedReceipts); // [] if no tampering
 */
export function verifyChain(receipts: Array<any>): Promise<any>;
/**
 * Find tampered receipts in a chain
 *
 * @param {Array<Object>} receipts - Array of receipts to check
 * @returns {Promise<Array<Object>>} Array of tampered receipts with reasons
 */
export function findTamperedReceipts(receipts: Array<any>): Promise<Array<any>>;
/**
 * Reconstruct chain state from receipts
 *
 * Builds a timeline of decisions from receipt chain.
 *
 * @param {Array<Object>} receipts - Array of receipts
 * @returns {Promise<Object>} Reconstructed chain state
 *
 * @example
 * const state = await reconstructChainState(receipts);
 * console.log(state.timeline); // Array of decisions in order
 */
export function reconstructChainState(receipts: Array<any>): Promise<any>;
/**
 * Chain verification result schema
 */
export const ChainVerificationSchema: z.ZodObject<{
    valid: z.ZodBoolean;
    totalReceipts: z.ZodNumber;
    validReceipts: z.ZodNumber;
    tamperedReceipts: z.ZodArray<z.ZodObject<{
        index: z.ZodNumber;
        receiptId: z.ZodString;
        reason: z.ZodString;
    }, z.core.$strip>>;
    errors: z.ZodArray<z.ZodString>;
}, z.core.$strip>;
declare namespace _default {
    export { verifyChain };
    export { findTamperedReceipts };
    export { reconstructChainState };
}
export default _default;
import { z } from 'zod';
