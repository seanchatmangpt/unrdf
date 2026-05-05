/**
 * Wrap a function with deterministic receipt generation
 *
 * **Determinism Guarantee:**
 * - NO Date.now() or Math.random()
 * - All timestamps injected via context
 * - Hashes are deterministic (BLAKE3 with sorted keys)
 * - Same inputs → Same receipt (idempotent)
 *
 * @param {Function} fn - Function to wrap
 * @param {Object} context - Receipt context
 * @param {string} context.operation - Operation name (for receipt tracking)
 * @param {Function} [context.getTimestamp] - Timestamp provider (defaults to kgc-4d now())
 * @param {Object|null} [context.previousReceipt=null] - Previous receipt for chaining
 * @param {string} [context.caseId] - Workflow case ID (for execution receipts)
 * @param {string} [context.taskId] - Task ID (for execution receipts)
 * @returns {Function} Wrapped function that returns {result, receipt}
 *
 * @example
 * // Basic usage with injected timestamp
 * import { withReceipt } from '@unrdf/v6-core/receipts/with-receipt';
 *
 * const processData = withReceipt(
 *   (data) => data.map(x => x * 2),
 *   {
 *     operation: 'processData',
 *     getTimestamp: () => 1704110400000000000n // Fixed for determinism
 *   }
 * );
 *
 * const { result, receipt } = await processData([1, 2, 3]);
 * // result: [2, 4, 6]
 * // receipt: { id, hash, merkle_proof, timestamp_provided, ... }
 *
 * @example
 * // Chained receipts
 * const receipt1 = await processStep1();
 * const receipt2 = await processStep2WithReceipt({
 *   previousReceipt: receipt1.receipt
 * });
 */
export function withReceipt(fn: Function, context?: {
    operation: string;
    getTimestamp?: Function;
    previousReceipt?: any | null;
    caseId?: string;
    taskId?: string;
}): Function;
/**
 * Create a receipt chain from multiple function calls
 *
 * @param {Array<{fn: Function, context: Object}>} steps - Array of functions with contexts
 * @returns {Promise<Array<{result: any, receipt: Object}>>} Results with chained receipts
 *
 * @example
 * const chain = await createReceiptChain([
 *   { fn: step1, context: { operation: 'step1' } },
 *   { fn: step2, context: { operation: 'step2' } },
 *   { fn: step3, context: { operation: 'step3' } },
 * ]);
 *
 * // chain[0].receipt.previousHash === null
 * // chain[1].receipt.previousHash === chain[0].receipt.receiptHash
 * // chain[2].receipt.previousHash === chain[1].receipt.receiptHash
 */
export function createReceiptChain(steps: Array<{
    fn: Function;
    context: any;
}>): Promise<Array<{
    result: any;
    receipt: any;
}>>;
/**
 * Verify idempotency: same inputs → same receipt
 *
 * **Testing Determinism:**
 * Run the wrapped function twice with identical inputs and injected timestamp.
 * Receipt hashes MUST be identical.
 *
 * @param {Function} wrappedFn - Function wrapped with withReceipt
 * @param {Array} args - Arguments to test with
 * @returns {Promise<{idempotent: boolean, receipt1: Object, receipt2: Object}>}
 *
 * @example
 * const wrapped = withReceipt(
 *   (x) => x * 2,
 *   { operation: 'double', getTimestamp: () => 1704110400000000000n }
 * );
 *
 * const check = await verifyIdempotency(wrapped, [5]);
 * assert(check.idempotent === true);
 * assert(check.receipt1.receiptHash === check.receipt2.receiptHash);
 */
export function verifyIdempotency(wrappedFn: Function, args?: any[]): Promise<{
    idempotent: boolean;
    receipt1: any;
    receipt2: any;
}>;
export default withReceipt;
