/**
 * Compile Receipt - V6 AOT Grammar/Doc Generation Receipts
 *
 * For ahead-of-time grammar compilation and document generation with
 * deterministic output verification.
 *
 * @module @unrdf/v6-core/receipts/compile-receipt
 */

import { z } from 'zod';
import { BaseReceiptSchema, RECEIPT_TYPES, BLAKE3_HEX_LENGTH } from './base-receipt.mjs';

// =============================================================================
// Event Types
// =============================================================================

/**
 * Compile event types
 * @readonly
 * @enum {string}
 */
export const COMPILE_EVENT_TYPES = Object.freeze({
  GRAMMAR_COMPILED: 'GRAMMAR_COMPILED',
  DOC_GENERATED: 'DOC_GENERATED',
  SCHEMA_VALIDATED: 'SCHEMA_VALIDATED',
  RULESET_BUILT: 'RULESET_BUILT',
  ONTOLOGY_COMPILED: 'ONTOLOGY_COMPILED',
});

/**
 * Grammar types
 * @readonly
 * @enum {string}
 */
export const GRAMMAR_TYPES = Object.freeze({
  SPARQL: 'SPARQL',
  SHACL: 'SHACL',
  N3: 'N3',
  OWL: 'OWL',
  RDFS: 'RDFS',
  CUSTOM: 'CUSTOM',
});

// =============================================================================
// Schemas
// =============================================================================

/**
 * Event type schema
 */
export const CompileEventTypeSchema = z.enum([
  'GRAMMAR_COMPILED',
  'DOC_GENERATED',
  'SCHEMA_VALIDATED',
  'RULESET_BUILT',
  'ONTOLOGY_COMPILED',
]);

/**
 * Grammar type schema
 */
export const GrammarTypeSchema = z.enum([
  'SPARQL',
  'SHACL',
  'N3',
  'OWL',
  'RDFS',
  'CUSTOM',
]);

/**
 * Compilation metadata schema
 */
export const CompilationMetadataSchema = z.object({
  /** Input file count */
  inputCount: z.number(),
  /** Output file count */
  outputCount: z.number(),
  /** Compilation duration in milliseconds */
  durationMs: z.number().optional(),
  /** Compiler flags/options */
  compilerFlags: z.array(z.string()).optional(),
  /** Warning count */
  warningCount: z.number().optional(),
  /** Error count (0 for successful compilation) */
  errorCount: z.number().optional(),
});

/**
 * Compile payload schema
 */
export const CompilePayloadSchema = z.object({
  /** Compilation result (e.g., 'SUCCESS', 'PARTIAL', 'FAILED') */
  result: z.string(),
  /** Compilation metadata */
  metadata: CompilationMetadataSchema,
  /** Output file paths */
  outputPaths: z.array(z.string()).optional(),
  /** Compilation warnings */
  warnings: z.array(z.string()).optional(),
  /** Additional context */
  context: z.any().optional(),
}).passthrough();

/**
 * Compile receipt schema - extends base with compilation-specific fields
 */
export const CompileReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(RECEIPT_TYPES.COMPILE),

  /** Event type (GRAMMAR_COMPILED, etc.) */
  eventType: CompileEventTypeSchema,

  /** Array of input file hashes */
  inputHashes: z.array(z.string().length(BLAKE3_HEX_LENGTH)),

  /** Output artifact hash */
  outputHash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Compiler version */
  compilerVersion: z.string(),

  /** Grammar/schema type */
  grammarType: GrammarTypeSchema,

  /** Compile payload */
  payload: CompilePayloadSchema,
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} CompilationMetadata
 * @property {number} inputCount - Input file count
 * @property {number} outputCount - Output file count
 * @property {number} [durationMs] - Compilation duration
 * @property {string[]} [compilerFlags] - Compiler flags/options
 * @property {number} [warningCount] - Warning count
 * @property {number} [errorCount] - Error count
 */

/**
 * @typedef {Object} CompilePayload
 * @property {string} result - Compilation result
 * @property {CompilationMetadata} metadata - Compilation metadata
 * @property {string[]} [outputPaths] - Output file paths
 * @property {string[]} [warnings] - Compilation warnings
 * @property {Object} [context] - Additional context
 */

/**
 * @typedef {Object} CompileReceipt
 * @property {string} id - UUID of receipt
 * @property {'compile'} receiptType - Receipt type discriminator
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} eventType - Compile event type
 * @property {string[]} inputHashes - Input file hashes
 * @property {string} outputHash - Output artifact hash
 * @property {string} compilerVersion - Compiler version
 * @property {string} grammarType - Grammar/schema type
 * @property {CompilePayload} payload - Compile payload
 * @property {Object} [attestation] - Signature/attestation
 * @property {Object} [vectorClock] - Vector clock
 * @property {string} [gitRef] - Git reference
 * @property {string} [kgcEventId] - KGC event ID
 */

// =============================================================================
// Exports
// =============================================================================

export default CompileReceiptSchema;
