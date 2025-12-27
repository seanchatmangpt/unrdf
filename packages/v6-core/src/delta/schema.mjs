/**
 * V6 Delta Contract - Schema Definitions
 *
 * Defines the ONLY legal change carrier in v6 UNRDF.
 * All mutations flow through Δ (Delta) → μ(O ⊔ Δ) → Receipt → Atomic A.
 *
 * @module @unrdf/v6-core/delta/schema
 *
 * INVARIANTS:
 * 1. Δ is the ONLY way to mutate O (Ontology state)
 * 2. All operations are atomic: all-or-none
 * 3. Every Δ produces a receipt (success or denial)
 * 4. No partial applications allowed
 */

import { z } from 'zod';

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
export const DeltaOperationSchema = z.discriminatedUnion('op', [
  z.object({
    op: z.literal('add'),
    subject: z.string().min(1),
    predicate: z.string().min(1),
    object: z.string().min(1),
    graph: z.string().optional(),
  }),
  z.object({
    op: z.literal('delete'),
    subject: z.string().min(1),
    predicate: z.string().min(1),
    object: z.string().min(1),
    graph: z.string().optional(),
  }),
  z.object({
    op: z.literal('update'),
    subject: z.string().min(1),
    predicate: z.string().min(1),
    oldObject: z.string().min(1),
    newObject: z.string().min(1),
    graph: z.string().optional(),
  }),
]);

/**
 * Delta Source Schema - Tracks delta origin and context
 *
 * @typedef {Object} DeltaSource
 * @property {string} package - Package that proposed delta (e.g., '@unrdf/yawl')
 * @property {string} [actor] - Optional actor/user identifier
 * @property {any} [context] - Optional context metadata
 */
export const DeltaSourceSchema = z.object({
  package: z.string().min(1),
  actor: z.string().optional(),
  context: z.any().optional(),
});

/**
 * Delta Admissibility Schema - Policy enforcement metadata
 *
 * @typedef {Object} DeltaAdmissibility
 * @property {string} [policyId] - Policy identifier for validation
 * @property {string[]} [constraints] - Constraint identifiers to check
 * @property {string[]} [preConditions] - Pre-condition checks required
 */
export const DeltaAdmissibilitySchema = z.object({
  policyId: z.string().optional(),
  constraints: z.array(z.string()).optional(),
  preConditions: z.array(z.string()).optional(),
}).optional();

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
export const DeltaSchema = z.object({
  id: z.string().uuid(),
  timestamp_iso: z.string().datetime(),
  t_ns: z.bigint().nonnegative(),
  operations: z.array(DeltaOperationSchema).min(1),
  source: DeltaSourceSchema,
  admissibility: DeltaAdmissibilitySchema,
});

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
export const DeltaReceiptSchema = z.object({
  deltaId: z.string().uuid(),
  applied: z.boolean(),
  timestamp_ns: z.bigint().nonnegative(),
  stateHash: z.string().min(1).optional(),
  reason: z.string().optional(),
  operationsApplied: z.number().int().nonnegative().optional(),
  errors: z.array(z.string()).optional(),
});

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
export const DeltaConflictSchema = z.object({
  subject: z.string().min(1),
  predicate: z.string().min(1),
  currentObject: z.string().min(1),
  deltaObject: z.string().min(1),
  resolution: z.enum(['current-wins', 'delta-wins', 'merge', 'reject']),
});

/**
 * Validate Delta
 *
 * @param {any} data - Data to validate
 * @returns {Delta} Validated delta
 * @throws {Error} If validation fails
 */
export function validateDelta(data) {
  return DeltaSchema.parse(data);
}

/**
 * Validate Delta Operation
 *
 * @param {any} data - Data to validate
 * @returns {DeltaOperation} Validated operation
 * @throws {Error} If validation fails
 */
export function validateDeltaOperation(data) {
  return DeltaOperationSchema.parse(data);
}

/**
 * Validate Delta Receipt
 *
 * @param {any} data - Data to validate
 * @returns {DeltaReceipt} Validated receipt
 * @throws {Error} If validation fails
 */
export function validateDeltaReceipt(data) {
  return DeltaReceiptSchema.parse(data);
}

/**
 * Validate Delta Conflict
 *
 * @param {any} data - Data to validate
 * @returns {DeltaConflict} Validated conflict
 * @throws {Error} If validation fails
 */
export function validateDeltaConflict(data) {
  return DeltaConflictSchema.parse(data);
}
