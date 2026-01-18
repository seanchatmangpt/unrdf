/**
 * @fileoverview Core Types - Zod schemas and type definitions for narrative-state-chain
 *
 * **Purpose**: Runtime validation and type definitions for:
 * - Universe records (schema, reconciliation, invariants, guards)
 * - Scene envelopes (observations, deltas, consequences, receipts)
 * - Guard results (admissibility checks)
 * - Receipts (tamper-proof audit trail)
 * - Bridges (cross-universe type coercion)
 *
 * **Design**: JSDoc for static types, Zod for runtime validation
 *
 * @module narrative-state-chain/types
 */

import { z } from 'zod';

/**
 * Zod schema for Universe metadata
 * @type {z.ZodObject}
 */
export const UniverseMetadataSchema = z.object({
  name: z.string().min(1),
  description: z.string().optional(),
  version: z.string().default('1.0.0'),
  created: z.date().default(() => new Date()),
  updated: z.date().default(() => new Date()),
});

/**
 * Zod schema for Invariant (pure predicate function)
 * @type {z.ZodObject}
 */
export const InvariantSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  description: z.string().optional(),
  predicate: z.unknown(), // Function type - validation skipped
});

/**
 * Zod schema for Guard (authorization check)
 * @type {z.ZodObject}
 */
export const GuardSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  description: z.string().optional(),
  condition: z.unknown(), // Function type - validation skipped
});

/**
 * Zod schema for Universe record
 * @type {z.ZodObject}
 */
export const UniverseSchema = z.object({
  id: z.string().uuid(),
  schema: z.string().min(1), // RDF schema IRI or identifier
  reconcile: z.unknown(), // Function type - validation skipped
  invariants: z.array(InvariantSchema).default([]),
  guards: z.array(GuardSchema).default([]),
  metadata: UniverseMetadataSchema,
});

/**
 * Zod schema for Guard Result
 * @type {z.ZodObject}
 */
export const GuardResultSchema = z.object({
  guardId: z.string(),
  passed: z.boolean(),
  reason: z.string().optional(),
  proof: z.string().optional(), // Hash or signature proving evaluation
  timestamp: z.date().default(() => new Date()),
});

/**
 * Zod schema for Receipt
 * @type {z.ZodObject}
 */
export const ReceiptSchema = z.object({
  sceneId: z.string().uuid(),
  universeId: z.string().uuid(),
  timestamp: z.date(),
  admissibilityChecks: z.array(GuardResultSchema),
  minimalityProof: z.string(), // Hash proving delta is minimal
  forkParents: z.array(z.string()).default([]), // Parent scene IDs for forks
  receiptHash: z.string(), // BLAKE3 hash of entire receipt
  signature: z.string().optional(), // Optional cryptographic signature
  previousReceiptHash: z.union([z.string(), z.null()]).optional(), // Link to previous receipt
}).strict(false); // Allow additional properties

/**
 * Zod schema for Scene envelope
 * @type {z.ZodObject}
 */
export const SceneSchema = z.object({
  id: z.string().uuid(),
  universeId: z.string().uuid(),
  observations: z.array(z.any()), // RDF quads or domain objects
  delta: z.record(z.any()), // State change (JSON-serializable)
  consequences: z.array(z.any()).default([]), // Derived effects from reconciliation
  artifacts: z.record(z.any()).default({}), // Side products (logs, metrics, etc.)
  receipts: z.array(ReceiptSchema).default([]), // Receipt chain
  timestamp: z.date().default(() => new Date()),
  previousSceneId: z.string().uuid().optional(), // Link to previous scene (linear history)
});

/**
 * Zod schema for Bridge (cross-universe connection)
 * @type {z.ZodObject}
 */
export const BridgeSchema = z.object({
  id: z.string().uuid(),
  sourceUniverseId: z.string().uuid(),
  targetUniverseId: z.string().uuid(),
  typeCoercion: z.unknown(), // Type transformation function - validation skipped
  invariantPreservation: z.unknown(), // Verify invariants hold post-coercion - validation skipped
  accessGrants: z.array(z.object({
    agent: z.string(),
    permission: z.enum(['read', 'write', 'execute']),
  })).default([]),
  proof: z.string().optional(), // Formal proof that bridge preserves semantics
  metadata: z.object({
    name: z.string(),
    description: z.string().optional(),
    created: z.date().default(() => new Date()),
  }),
});

/**
 * Type definitions (JSDoc) for IDE support
 */

/**
 * @typedef {Object} UniverseMetadata
 * @property {string} name - Universe name
 * @property {string} [description] - Optional description
 * @property {string} version - Semantic version
 * @property {Date} created - Creation timestamp
 * @property {Date} updated - Last update timestamp
 */

/**
 * @typedef {Object} Invariant
 * @property {string} id - Invariant identifier
 * @property {string} name - Human-readable name
 * @property {string} [description] - Optional description
 * @property {(state: any) => boolean} predicate - Pure predicate function
 */

/**
 * @typedef {Object} Guard
 * @property {string} id - Guard identifier
 * @property {string} name - Human-readable name
 * @property {string} [description] - Optional description
 * @property {(context: {agent: string, action: string, target: any}) => Promise<boolean>} condition - Authorization check
 */

/**
 * @typedef {Object} Universe
 * @property {string} id - UUID identifier
 * @property {string} schema - RDF schema IRI
 * @property {(state: any, observations: any[]) => Promise<{consequences: any[], artifacts: Object, errors: string[]}>} reconcile - Reconciliation function μ
 * @property {Invariant[]} invariants - State invariants
 * @property {Guard[]} guards - Authorization guards
 * @property {UniverseMetadata} metadata - Metadata
 */

/**
 * @typedef {Object} GuardResult
 * @property {string} guardId - Guard that was evaluated
 * @property {boolean} passed - Whether guard passed
 * @property {string} [reason] - Reason for failure
 * @property {string} [proof] - Proof of evaluation
 * @property {Date} timestamp - Evaluation time
 */

/**
 * @typedef {Object} Receipt
 * @property {string} sceneId - Scene this receipt covers
 * @property {string} universeId - Universe context
 * @property {Date} timestamp - Receipt generation time
 * @property {GuardResult[]} admissibilityChecks - Guard evaluation results
 * @property {string} minimalityProof - Proof that delta is minimal
 * @property {string[]} forkParents - Parent scenes (for forks)
 * @property {string} receiptHash - BLAKE3 hash
 * @property {string} [signature] - Optional signature
 */

/**
 * @typedef {Object} Scene
 * @property {string} id - UUID identifier
 * @property {string} universeId - Parent universe
 * @property {any[]} observations - Input observations (RDF quads)
 * @property {Object} delta - State change
 * @property {any[]} consequences - Derived consequences
 * @property {Object} artifacts - Side products
 * @property {Receipt[]} receipts - Receipt chain
 * @property {Date} timestamp - Scene creation time
 * @property {string} [previousSceneId] - Previous scene in chain
 */

/**
 * @typedef {Object} Bridge
 * @property {string} id - UUID identifier
 * @property {string} sourceUniverseId - Source universe
 * @property {string} targetUniverseId - Target universe
 * @property {(value: any) => any} typeCoercion - Type transformation
 * @property {(value: any) => Promise<boolean>} invariantPreservation - Invariant checker
 * @property {Array<{agent: string, permission: 'read'|'write'|'execute'}>} accessGrants - Access control
 * @property {string} [proof] - Formal proof
 * @property {Object} metadata - Bridge metadata
 */

/**
 * Validate Universe at runtime
 *
 * @param {any} universe - Universe object to validate
 * @returns {{success: boolean, data?: Universe, error?: z.ZodError}} Validation result
 *
 * @example
 * const result = validateUniverse(myUniverse);
 * if (result.success) {
 *   console.log('Valid universe:', result.data.id);
 * }
 */
export function validateUniverse(universe) {
  return UniverseSchema.safeParse(universe);
}

/**
 * Validate Scene at runtime
 *
 * @param {any} scene - Scene object to validate
 * @returns {{success: boolean, data?: Scene, error?: z.ZodError}} Validation result
 *
 * @example
 * const result = validateScene(myScene);
 * if (!result.success) {
 *   console.error('Invalid scene:', result.error);
 * }
 */
export function validateScene(scene) {
  return SceneSchema.safeParse(scene);
}

/**
 * Validate Receipt at runtime
 *
 * @param {any} receipt - Receipt object to validate
 * @returns {{success: boolean, data?: Receipt, error?: z.ZodError}} Validation result
 */
export function validateReceipt(receipt) {
  return ReceiptSchema.safeParse(receipt);
}

/**
 * Validate Bridge at runtime
 *
 * @param {any} bridge - Bridge object to validate
 * @returns {{success: boolean, data?: Bridge, error?: z.ZodError}} Validation result
 */
export function validateBridge(bridge) {
  return BridgeSchema.safeParse(bridge);
}
