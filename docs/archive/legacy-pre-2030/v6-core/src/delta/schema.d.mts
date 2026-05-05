/**
 * Validate Delta
 *
 * @param {any} data - Data to validate
 * @returns {Delta} Validated delta
 * @throws {Error} If validation fails
 */
export function validateDelta(data: any): Delta;
/**
 * Validate Delta Operation
 *
 * @param {any} data - Data to validate
 * @returns {DeltaOperation} Validated operation
 * @throws {Error} If validation fails
 */
export function validateDeltaOperation(data: any): DeltaOperation;
/**
 * Validate Delta Receipt
 *
 * @param {any} data - Data to validate
 * @returns {DeltaReceipt} Validated receipt
 * @throws {Error} If validation fails
 */
export function validateDeltaReceipt(data: any): DeltaReceipt;
/**
 * Validate Delta Conflict
 *
 * @param {any} data - Data to validate
 * @returns {DeltaConflict} Validated conflict
 * @throws {Error} If validation fails
 */
export function validateDeltaConflict(data: any): DeltaConflict;
/**
 * Delta Operation Schema - Individual RDF operations
 *
 * Supports three operation types:
 * - add: Insert new triple
 * - delete: Remove existing triple
 * - update: Atomic replace (delete old + add new)
 *
 * @constant
 * @type {z.ZodDiscriminatedUnion}
 */
export const DeltaOperationSchema: z.ZodDiscriminatedUnion;
/**
 * Delta Source Schema - Tracks delta origin and context
 *
 * @typedef {Object} DeltaSource
 * @property {string} package - Package that proposed delta (e.g., '@unrdf/yawl')
 * @property {string} [actor] - Optional actor/user identifier
 * @property {any} [context] - Optional context metadata
 */
export const DeltaSourceSchema: z.ZodObject<{
    package: z.ZodString;
    actor: z.ZodOptional<z.ZodString>;
    context: z.ZodOptional<z.ZodAny>;
}, z.core.$strip>;
/**
 * Delta Admissibility Schema - Policy enforcement metadata
 *
 * @typedef {Object} DeltaAdmissibility
 * @property {string} [policyId] - Policy identifier for validation
 * @property {string[]} [constraints] - Constraint identifiers to check
 * @property {string[]} [preConditions] - Pre-condition checks required
 */
export const DeltaAdmissibilitySchema: z.ZodOptional<z.ZodObject<{
    policyId: z.ZodOptional<z.ZodString>;
    constraints: z.ZodOptional<z.ZodArray<z.ZodString>>;
    preConditions: z.ZodOptional<z.ZodArray<z.ZodString>>;
}, z.core.$strip>>;
/**
 * Delta Schema - Complete change carrier
 *
 * The ONLY legal way to mutate ontology state in v6.
 * Every mutation must be packaged as a Delta.
 *
 * @typedef {Object} Delta
 * @property {string} id - UUID for delta tracking
 * @property {string} timestamp_iso - ISO 8601 timestamp
 * @property {bigint} t_ns - Nanosecond precision timestamp
 * @property {Array<DeltaOperation>} operations - RDF operations to apply
 * @property {DeltaSource} source - Delta origin metadata
 * @property {DeltaAdmissibility} [admissibility] - Policy enforcement metadata
 *
 * @constant
 * @type {z.ZodObject}
 *
 * @example
 * const delta = {
 *   id: crypto.randomUUID(),
 *   timestamp_iso: new Date().toISOString(),
 *   t_ns: BigInt(Date.now()) * 1_000_000n,
 *   operations: [
 *     { op: 'add', subject: 'http://ex.org/s', predicate: 'http://ex.org/p', object: 'value' }
 *   ],
 *   source: { package: '@unrdf/yawl', actor: 'workflow-engine' }
 * };
 */
export const DeltaSchema: z.ZodObject;
/**
 * Delta Receipt Schema - Result of delta application
 *
 * Every delta produces a receipt indicating success or failure.
 * Receipts are immutable proof of delta processing.
 *
 * @typedef {Object} DeltaReceipt
 * @property {string} deltaId - Reference to delta ID
 * @property {boolean} applied - Whether delta was applied
 * @property {bigint} timestamp_ns - Processing timestamp
 * @property {string} [stateHash] - Post-application state hash (if applied)
 * @property {string} [reason] - Denial reason (if not applied)
 * @property {number} [operationsApplied] - Count of operations applied
 * @property {string[]} [errors] - Error messages (if any)
 *
 * @constant
 * @type {z.ZodObject}
 */
export const DeltaReceiptSchema: z.ZodObject;
/**
 * Delta Conflict Schema - Conflict detection result
 *
 * Tracks conflicts discovered during reconciliation.
 *
 * @typedef {Object} DeltaConflict
 * @property {string} subject - Conflicting triple subject
 * @property {string} predicate - Conflicting triple predicate
 * @property {string} currentObject - Current object value in store
 * @property {string} deltaObject - Proposed object value in delta
 * @property {string} resolution - Conflict resolution strategy applied
 */
export const DeltaConflictSchema: z.ZodObject<{
    subject: z.ZodString;
    predicate: z.ZodString;
    currentObject: z.ZodString;
    deltaObject: z.ZodString;
    resolution: z.ZodEnum<{
        merge: "merge";
        "current-wins": "current-wins";
        "delta-wins": "delta-wins";
        reject: "reject";
    }>;
}, z.core.$strip>;
/**
 * Delta Source Schema - Tracks delta origin and context
 */
export type DeltaSource = {
    /**
     * - Package that proposed delta (e.g., '@unrdf/yawl')
     */
    package: string;
    /**
     * - Optional actor/user identifier
     */
    actor?: string;
    /**
     * - Optional context metadata
     */
    context?: any;
};
/**
 * Delta Admissibility Schema - Policy enforcement metadata
 */
export type DeltaAdmissibility = {
    /**
     * - Policy identifier for validation
     */
    policyId?: string;
    /**
     * - Constraint identifiers to check
     */
    constraints?: string[];
    /**
     * - Pre-condition checks required
     */
    preConditions?: string[];
};
/**
 * Delta Schema - Complete change carrier
 *
 * The ONLY legal way to mutate ontology state in v6.
 * Every mutation must be packaged as a Delta.
 */
export type Delta = {
    /**
     * - UUID for delta tracking
     */
    id: string;
    /**
     * - ISO 8601 timestamp
     */
    timestamp_iso: string;
    /**
     * - Nanosecond precision timestamp
     */
    t_ns: bigint;
    /**
     * - RDF operations to apply
     */
    operations: Array<DeltaOperation>;
    /**
     * - Delta origin metadata
     */
    source: DeltaSource;
    /**
     * - Policy enforcement metadata
     */
    admissibility?: DeltaAdmissibility;
};
/**
 * Delta Receipt Schema - Result of delta application
 *
 * Every delta produces a receipt indicating success or failure.
 * Receipts are immutable proof of delta processing.
 */
export type DeltaReceipt = {
    /**
     * - Reference to delta ID
     */
    deltaId: string;
    /**
     * - Whether delta was applied
     */
    applied: boolean;
    /**
     * - Processing timestamp
     */
    timestamp_ns: bigint;
    /**
     * - Post-application state hash (if applied)
     */
    stateHash?: string;
    /**
     * - Denial reason (if not applied)
     */
    reason?: string;
    /**
     * - Count of operations applied
     */
    operationsApplied?: number;
    /**
     * - Error messages (if any)
     */
    errors?: string[];
};
/**
 * Delta Conflict Schema - Conflict detection result
 *
 * Tracks conflicts discovered during reconciliation.
 */
export type DeltaConflict = {
    /**
     * - Conflicting triple subject
     */
    subject: string;
    /**
     * - Conflicting triple predicate
     */
    predicate: string;
    /**
     * - Current object value in store
     */
    currentObject: string;
    /**
     * - Proposed object value in delta
     */
    deltaObject: string;
    /**
     * - Conflict resolution strategy applied
     */
    resolution: string;
};
import { z } from 'zod';
