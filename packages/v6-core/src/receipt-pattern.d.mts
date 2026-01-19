/**
 * BLAKE3 Hash Utility
 * Provides deterministic, cryptographically secure hashing using BLAKE3.
 * BLAKE3 is faster than SHA-256 and provides better security properties.
 *
 * @param {string|object} data - Data to hash (will be stringified if object)
 * @returns {Promise<string>} Hex-encoded BLAKE3 hash (64 characters)
 *
 * @example
 * const hash = await blake3Hash('test data');
 * console.log(hash); // 64-character hex string
 */
export function blake3Hash(data: string | object): Promise<string>;
/**
 * Canonicalize object for deterministic hashing
 * Sorts keys lexicographically, removes undefined values
 * Handles non-serializable objects (class instances, functions)
 *
 * @param {any} obj - Object to canonicalize
 * @returns {string} JSON string with sorted keys
 */
export function canonicalize(obj: any): string;
/**
 * Generate deterministic UUID from content hash
 * Creates a valid UUID v4 format from a hash string
 *
 * @param {string} contentHash - Hash to derive UUID from (min 32 hex chars)
 * @returns {string} Valid UUID v4 format string
 */
export function deterministicUUID(contentHash: string): string;
/**
 * Higher-Order Function: Wrap operation with receipt generation
 *
 * @param {Function} fn - Pure function to wrap
 * @param {Object} options - Receipt options
 * @param {string} options.operation - Operation name
 * @param {string} options.profile - Receipt profile type
 * @param {z.ZodSchema} options.inputSchema - Zod schema for input validation
 * @param {z.ZodSchema} options.outputSchema - Zod schema for output validation
 * @returns {Function} Wrapped function that returns { result, receipt }
 *
 * @example
 * const queryWithReceipt = withReceipt(
 *   async (store, sparql) => store.query(sparql),
 *   {
 *     operation: 'sparql-query',
 *     profile: 'query',
 *     inputSchema: z.tuple([StoreSchema, z.string()]),
 *     outputSchema: QueryResultSchema
 *   }
 * );
 *
 * const ctx = createContext({ nodeId: 'node-1' });
 * const { result, receipt } = await queryWithReceipt(ctx, store, 'SELECT * WHERE {?s ?p ?o}');
 */
export function withReceipt(fn: Function, options?: {
    operation: string;
    profile: string;
    inputSchema: z.ZodSchema;
    outputSchema: z.ZodSchema;
}): Function;
/**
 * Create deterministic context
 *
 * @param {Object} options - Context options
 * @param {string} options.nodeId - Node identifier
 * @param {bigint} [options.t_ns] - Timestamp in nanoseconds (default: current time)
 * @param {string} [options.previousReceiptHash] - Previous receipt hash (default: null)
 * @param {string} [options.caseId] - Case ID (optional)
 * @param {string} [options.taskId] - Task ID (optional)
 * @param {string} [options.deltaId] - Delta ID (optional)
 * @returns {Object} Deterministic context
 */
export function createContext(options?: {
    nodeId: string;
    t_ns?: bigint;
    previousReceiptHash?: string;
    caseId?: string;
    taskId?: string;
    deltaId?: string;
}): any;
/**
 * Chain receipts together (merkle tree)
 *
 * @param {Object} receipt1 - First receipt
 * @param {Object} receipt2 - Second receipt
 * @returns {Object} Context for next operation with receipt2 as previous
 */
export function chainReceipts(receipt1: any, receipt2: any): any;
/**
 * Verify receipt chain integrity
 *
 * @param {Array<Object>} receipts - Array of receipts to verify
 * @returns {Object} Verification result
 */
export function verifyReceiptChain(receipts: Array<any>): any;
/**
 * Compose multiple receipted operations
 *
 * @param {...Function} fns - Wrapped functions to compose
 * @returns {Function} Composed function that chains receipts
 *
 * @example
 * const parse = withReceipt(parseRDF, { operation: 'parse', profile: 'parse' });
 * const query = withReceipt(queryStore, { operation: 'query', profile: 'query' });
 *
 * const parseAndQuery = compose(parse, query);
 * const { result, receipts } = await parseAndQuery(ctx, rdfData, sparql);
 */
export function compose(...fns: Function[]): Function;
/**
 * Deterministic Context Schema
 * Injects all non-deterministic values (time, randomness, node ID)
 */
export const DeterministicContextSchema: z.ZodObject<{
    t_ns: z.ZodBigInt;
    timestamp_iso: z.ZodString;
    nodeId: z.ZodString;
    caseId: z.ZodOptional<z.ZodString>;
    taskId: z.ZodOptional<z.ZodString>;
    previousReceiptHash: z.ZodNullable<z.ZodString>;
    deltaId: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Receipt Profile Schema
 * Unified receipt format across all packages
 */
export const ReceiptProfileSchema: z.ZodObject<{
    id: z.ZodString;
    profile: z.ZodEnum<{
        execution: "execution";
        allocation: "allocation";
        compile: "compile";
        verification: "verification";
        workflow: "workflow";
        delta: "delta";
        query: "query";
        store: "store";
        parse: "parse";
        index: "index";
    }>;
    previousReceiptHash: z.ZodNullable<z.ZodString>;
    payloadHash: z.ZodString;
    receiptHash: z.ZodString;
    t_ns: z.ZodBigInt;
    timestamp_iso: z.ZodString;
    context: z.ZodObject<{
        caseId: z.ZodOptional<z.ZodString>;
        taskId: z.ZodOptional<z.ZodString>;
        deltaId: z.ZodOptional<z.ZodString>;
        nodeId: z.ZodString;
    }, z.core.$strip>;
    payload: z.ZodAny;
    kgcEventId: z.ZodOptional<z.ZodString>;
    gitRef: z.ZodOptional<z.ZodString>;
    vectorClock: z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodNumber>>;
}, z.core.$strip>;
import { z } from 'zod';
