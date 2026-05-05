/**
 * Check runtime complexity before execution
 *
 * Pre-execution gate - verify compiled query can run within bounds.
 *
 * @param {Object} query - Compiled query from compiler.compileGrammar()
 * @param {Object} store - RDF store (for size estimation)
 * @param {Object} [options] - Runtime options
 * @returns {Object} Check result { allowed, reason, bounds, receipt }
 *
 * @example
 * const check = checkRuntimeComplexity(compiled, store);
 * if (check.allowed) {
 *   const result = await executeQuery(compiled);
 * } else {
 *   console.error('Runtime denied:', check.reason);
 * }
 */
export function checkRuntimeComplexity(query: any, store: any, _options?: {}): any;
/**
 * Wrap function execution with timeout and error handling
 *
 * **Guarantee**: Never hangs indefinitely. Returns timeout receipt after maxMs.
 *
 * @param {Function} fn - Function to execute
 * @param {number} maxMs - Maximum execution time in milliseconds
 * @param {Object} [context] - Execution context for receipts
 * @returns {Promise<Object>} Execution result { success, result, receipt }
 *
 * @example
 * const result = await wrapWithTimeout(
 *   () => store.query(sparql),
 *   5000,
 *   { grammarType: 'sparql', query: sparql }
 * );
 *
 * if (result.timeout) {
 *   console.error('Query timed out:', result.receipt);
 * }
 */
export function wrapWithTimeout(fn: Function, maxMs: number, context?: any): Promise<any>;
/**
 * Emit denial receipt for runtime rejection
 *
 * @param {Object} query - Query that was denied
 * @param {string} reason - Denial reason
 * @param {Object} [details] - Additional details
 * @returns {Object} Denial receipt
 *
 * @example
 * const receipt = emitDenialReceipt(query, 'TIMEOUT', {
 *   timeoutMs: 5000,
 *   actualMs: 6543
 * });
 */
export function emitDenialReceipt(query: any, reason: string, details?: any): any;
/**
 * Execute query with full runtime gating
 *
 * Complete runtime gate: pre-check → timeout-wrapped execution → receipt.
 *
 * @param {Object} compiled - Compiled query from compiler.compileGrammar()
 * @param {Function} executeFn - Execution function (store) => result
 * @param {Object} store - RDF store
 * @param {Object} [options] - Runtime options
 * @returns {Promise<Object>} Execution result with receipt
 *
 * @example
 * const result = await executeWithGate(
 *   compiled,
 *   (store) => store.query(compiled.ast.queryString),
 *   myStore
 * );
 *
 * if (result.success) {
 *   console.log('Result:', result.result);
 * } else {
 *   console.error('Denied:', result.denialReceipt);
 * }
 */
export function executeWithGate(compiled: any, executeFn: Function, store: any, options?: any): Promise<any>;
/**
 * Format runtime bounds for display
 * @param {string} grammarType - Grammar type
 * @returns {Object} Formatted bounds
 */
export function getRuntimeBounds(grammarType: string): any;
export namespace RUNTIME_BOUNDS {
    namespace sparql {
        let maxResults: number;
        let timeoutMs: number;
        let maxMemoryMB: number;
    }
    namespace shacl {
        let maxResults_1: number;
        export { maxResults_1 as maxResults };
        let timeoutMs_1: number;
        export { timeoutMs_1 as timeoutMs };
        let maxMemoryMB_1: number;
        export { maxMemoryMB_1 as maxMemoryMB };
    }
    namespace n3 {
        let maxResults_2: number;
        export { maxResults_2 as maxResults };
        let timeoutMs_2: number;
        export { timeoutMs_2 as timeoutMs };
        let maxMemoryMB_2: number;
        export { maxMemoryMB_2 as maxMemoryMB };
    }
    namespace owl {
        let maxResults_3: number;
        export { maxResults_3 as maxResults };
        let timeoutMs_3: number;
        export { timeoutMs_3 as timeoutMs };
        let maxMemoryMB_3: number;
        export { maxMemoryMB_3 as maxMemoryMB };
    }
    namespace shex {
        let maxResults_4: number;
        export { maxResults_4 as maxResults };
        let timeoutMs_4: number;
        export { timeoutMs_4 as timeoutMs };
        let maxMemoryMB_4: number;
        export { maxMemoryMB_4 as maxMemoryMB };
    }
}
/**
 * Runtime check result schema
 */
export const RuntimeCheckResultSchema: z.ZodObject<{
    allowed: z.ZodBoolean;
    reason: z.ZodOptional<z.ZodString>;
    bounds: z.ZodObject<{
        timeoutMs: z.ZodNumber;
        maxResults: z.ZodOptional<z.ZodNumber>;
    }, z.core.$strip>;
    receipt: z.ZodObject<{
        timestamp: z.ZodString;
        decision: z.ZodEnum<{
            DENY: "DENY";
            ALLOW: "ALLOW";
        }>;
    }, z.core.$strip>;
}, z.core.$strip>;
/**
 * Execution result schema
 */
export const ExecutionResultSchema: z.ZodObject<{
    success: z.ZodBoolean;
    result: z.ZodOptional<z.ZodAny>;
    error: z.ZodOptional<z.ZodString>;
    timeout: z.ZodOptional<z.ZodBoolean>;
    receipt: z.ZodObject<{
        timestamp: z.ZodString;
        executionTimeMs: z.ZodNumber;
        status: z.ZodEnum<{
            ERROR: "ERROR";
            SUCCESS: "SUCCESS";
            TIMEOUT: "TIMEOUT";
        }>;
        merkleProof: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>;
}, z.core.$strip>;
import { z } from 'zod';
