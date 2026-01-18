/**
 * @file Chatman Equation Zod Schemas
 * @module @unrdf/chatman-equation/schemas
 * @description
 * Zod validation schemas for Chatman Equation framework.
 * Generated from TOML schema definitions.
 *
 * Core equation: μ(O ⊔ Δ) → A
 * - O = Observations (current state)
 * - Δ = Delta (proposed changes)
 * - μ = Closure operator (reconciliation function)
 * - A = Artifacts (results)
 */

import { z } from 'zod';

/**
 * Domain enumeration
 * @constant
 */
export const DomainEnum = z.enum([
  'market',
  'organization',
  'strategy',
  'product',
  'customer',
  'blue_ocean',
]);

/**
 * Observation Schema - Represents current state O
 *
 * @typedef {Object} Observation
 * @property {string} id - UUID identifier
 * @property {string} timestamp - ISO 8601 timestamp
 * @property {string} domain - Domain context
 * @property {Object} state - Domain-specific state
 * @property {Object} [metadata] - Optional metadata
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ObservationSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  domain: DomainEnum,
  state: z.record(z.any()),
  metadata: z.record(z.any()).optional(),
});

/**
 * Closure Operator Type enumeration
 * @constant
 */
export const ClosureOperatorTypeEnum = z.enum([
  'merge',
  'transform',
  'reconcile',
  'compose',
]);

/**
 * Conflict Resolution Strategy enumeration
 * @constant
 */
export const ConflictResolutionEnum = z.enum([
  'delta_wins',
  'current_wins',
  'merge',
  'reject',
]);

/**
 * Closure Operator Schema - Defines μ function
 *
 * @typedef {Object} ClosureOperator
 * @property {string} type - Operator type (merge|transform|reconcile|compose)
 * @property {string} name - Operator identifier
 * @property {string} domain - Applicable domain
 * @property {string[]} [policies] - Policy identifiers to enforce
 * @property {string} [conflict_resolution] - Conflict resolution strategy
 * @property {string[]} [invariants] - Invariants to preserve
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ClosureOperatorSchema = z.object({
  type: ClosureOperatorTypeEnum,
  name: z.string().min(1),
  domain: DomainEnum,
  policies: z.array(z.string()).optional(),
  conflict_resolution: ConflictResolutionEnum.default('delta_wins'),
  invariants: z.array(z.string()).optional(),
});

/**
 * Delta Operation Schema - Individual change operations
 *
 * @typedef {Object} DeltaOperation
 * @property {string} op - Operation type (add|update|delete)
 * @property {string} field - Field being modified
 * @property {any} value - New value
 * @property {string} [reason] - Justification for change
 *
 * @constant
 * @type {z.ZodObject}
 */
export const DeltaOperationSchema = z.object({
  op: z.enum(['add', 'update', 'delete']),
  field: z.string().min(1),
  value: z.any(),
  reason: z.string().optional(),
});

/**
 * Delta Schema - Represents proposed changes Δ
 *
 * @typedef {Object} Delta
 * @property {string} id - UUID identifier
 * @property {string} timestamp - ISO 8601 timestamp
 * @property {string} domain - Domain context
 * @property {DeltaOperation[]} operations - List of operations
 *
 * @constant
 * @type {z.ZodObject}
 */
export const DeltaSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  domain: DomainEnum,
  operations: z.array(DeltaOperationSchema).min(1),
});

/**
 * Artifact Schema - Represents result A of μ(O ⊔ Δ)
 *
 * @typedef {Object} Artifact
 * @property {string} id - UUID identifier
 * @property {string} timestamp - ISO 8601 timestamp
 * @property {string} source_observation - Reference to observation ID
 * @property {string[]} applied_deltas - List of applied delta IDs
 * @property {string} operator - Closure operator used
 * @property {Object} result - Resulting state
 * @property {Object} [proof] - Cryptographic proof
 * @property {Object} [receipt] - Receipt from delta gate
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ArtifactSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  source_observation: z.string().uuid(),
  applied_deltas: z.array(z.string().uuid()).min(1),
  operator: z.string().min(1),
  result: z.record(z.any()),
  proof: z.record(z.any()).optional(),
  receipt: z.record(z.any()).optional(),
});

/**
 * Unification Mapping Schema - Domain-specific configuration
 *
 * @typedef {Object} UnificationMapping
 * @property {string} description - Mapping description
 * @property {string[]} observation_fields - Fields in observation state
 * @property {string[]} delta_fields - Fields in delta operations
 * @property {string[]} artifact_fields - Fields in artifact result
 * @property {string} closure_operator - Operator to use
 * @property {Object} invariants - Domain invariants
 *
 * @constant
 * @type {z.ZodObject}
 */
export const UnificationMappingSchema = z.object({
  description: z.string(),
  observation_fields: z.array(z.string()),
  delta_fields: z.array(z.string()),
  artifact_fields: z.array(z.string()),
  closure_operator: z.string(),
  invariants: z.record(z.string()),
});

/**
 * Complete Example Schema - Full μ(O ⊔ Δ) → A transformation
 *
 * @typedef {Object} ChatmanExample
 * @property {string} description - Example description
 * @property {Observation} observation - Initial observation O
 * @property {Delta} delta - Proposed delta Δ
 * @property {ClosureOperator} closure_operator - Operator μ
 * @property {Artifact} expected_artifact - Expected result A
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ChatmanExampleSchema = z.object({
  description: z.string(),
  observation: ObservationSchema,
  delta: DeltaSchema,
  closure_operator: ClosureOperatorSchema,
  expected_artifact: ArtifactSchema,
});

/**
 * Validate Observation
 *
 * @param {any} data - Data to validate
 * @returns {Observation} Validated observation
 * @throws {Error} If validation fails
 */
export function validateObservation(data) {
  return ObservationSchema.parse(data);
}

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
 * Validate Closure Operator
 *
 * @param {any} data - Data to validate
 * @returns {ClosureOperator} Validated closure operator
 * @throws {Error} If validation fails
 */
export function validateClosureOperator(data) {
  return ClosureOperatorSchema.parse(data);
}

/**
 * Validate Artifact
 *
 * @param {any} data - Data to validate
 * @returns {Artifact} Validated artifact
 * @throws {Error} If validation fails
 */
export function validateArtifact(data) {
  return ArtifactSchema.parse(data);
}

/**
 * Validate Unification Mapping
 *
 * @param {any} data - Data to validate
 * @returns {UnificationMapping} Validated mapping
 * @throws {Error} If validation fails
 */
export function validateUnificationMapping(data) {
  return UnificationMappingSchema.parse(data);
}

/**
 * Validate Complete Example
 *
 * @param {any} data - Data to validate
 * @returns {ChatmanExample} Validated example
 * @throws {Error} If validation fails
 */
export function validateChatmanExample(data) {
  return ChatmanExampleSchema.parse(data);
}

/**
 * Safe validation with result object
 *
 * @param {z.ZodSchema} schema - Zod schema to use
 * @param {any} data - Data to validate
 * @returns {{ success: boolean, data?: any, error?: z.ZodError }} Result object
 */
export function safeValidate(schema, data) {
  return schema.safeParse(data);
}
