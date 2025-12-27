/**
 * KGC Substrate Type Definitions
 *
 * Defines types for StorageSnapshot, QueryPattern, and related substrate primitives.
 * Uses Zod for runtime validation with JSDoc for static type hints.
 */

import { z } from 'zod';

/**
 * Storage Snapshot Schema
 *
 * Represents an immutable snapshot of the KnowledgeStore state at a specific epoch.
 * All hashes are BLAKE3 for deterministic verification.
 *
 * @typedef {Object} StorageSnapshot
 * @property {number} epoch - Sequential epoch number (starts at 0)
 * @property {bigint} timestamp_ns - Nanosecond-precision timestamp
 * @property {string} quads_hash - BLAKE3 hash of canonically sorted quads
 * @property {string} commit_hash - Git blob hash for snapshot storage
 * @property {string} snapshot_id - UUID for snapshot identification
 * @property {number} [quad_count] - Optional quad count for metadata
 */
export const StorageSnapshotSchema = z.object({
  epoch: z.number().int().nonnegative(),
  timestamp_ns: z.bigint().nonnegative(),
  quads_hash: z.string().min(1),
  commit_hash: z.string().min(1),
  snapshot_id: z.string().uuid(),
  quad_count: z.number().int().nonnegative().optional(),
});

/**
 * Query Pattern Schema
 *
 * Triple pattern for querying the KnowledgeStore.
 * Null values act as wildcards (match any).
 *
 * @typedef {Object} QueryPattern
 * @property {Object|null} subject - RDF subject term or null for wildcard
 * @property {Object|null} predicate - RDF predicate term or null for wildcard
 * @property {Object|null} object - RDF object term or null for wildcard
 * @property {Object|null} [graph] - RDF graph term or null for wildcard
 */
export const QueryPatternSchema = z.object({
  subject: z.any().nullable(),
  predicate: z.any().nullable(),
  object: z.any().nullable(),
  graph: z.any().nullable().optional(),
});

/**
 * Triple Entry Schema
 *
 * Represents a single triple entry in the append-only log.
 *
 * @typedef {Object} TripleEntry
 * @property {bigint} index - Sequential index in append-only log
 * @property {bigint} timestamp_ns - Nanosecond-precision timestamp
 * @property {'add'|'delete'} operation - Operation type
 * @property {Object} subject - RDF subject term
 * @property {Object} predicate - RDF predicate term
 * @property {Object} object - RDF object term
 * @property {Object} [graph] - RDF graph term (optional)
 */
export const TripleEntrySchema = z.object({
  index: z.bigint().nonnegative(),
  timestamp_ns: z.bigint().nonnegative(),
  operation: z.enum(['add', 'delete']),
  subject: z.any(),
  predicate: z.any(),
  object: z.any(),
  graph: z.any().optional(),
});

/**
 * State Commitment Schema
 *
 * Represents a cryptographic commitment to the current store state.
 *
 * @typedef {Object} StateCommitment
 * @property {string} state_hash - BLAKE3 hash of canonical store state
 * @property {bigint} log_index - Current append-only log index
 * @property {bigint} timestamp_ns - Nanosecond-precision timestamp
 * @property {number} quad_count - Total number of quads in store
 */
export const StateCommitmentSchema = z.object({
  state_hash: z.string().min(1),
  log_index: z.bigint().nonnegative(),
  timestamp_ns: z.bigint().nonnegative(),
  quad_count: z.number().int().nonnegative(),
});

/**
 * Validate StorageSnapshot
 *
 * @param {any} data - Data to validate
 * @returns {StorageSnapshot} Validated snapshot
 * @throws {Error} If validation fails
 */
export function validateStorageSnapshot(data) {
  return StorageSnapshotSchema.parse(data);
}

/**
 * Validate QueryPattern
 *
 * @param {any} data - Data to validate
 * @returns {QueryPattern} Validated query pattern
 * @throws {Error} If validation fails
 */
export function validateQueryPattern(data) {
  return QueryPatternSchema.parse(data);
}

/**
 * Validate TripleEntry
 *
 * @param {any} data - Data to validate
 * @returns {TripleEntry} Validated triple entry
 * @throws {Error} If validation fails
 */
export function validateTripleEntry(data) {
  return TripleEntrySchema.parse(data);
}

/**
 * Validate StateCommitment
 *
 * @param {any} data - Data to validate
 * @returns {StateCommitment} Validated state commitment
 * @throws {Error} If validation fails
 */
export function validateStateCommitment(data) {
  return StateCommitmentSchema.parse(data);
}
