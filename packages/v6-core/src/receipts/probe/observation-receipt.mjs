/**
 * Probe Observation Receipt - Deterministic observation proof
 *
 * Each observation from a probe agent creates an immutable hash chain entry.
 * Proves observation was made exactly as recorded with deterministic serialization.
 *
 * @module @unrdf/v6-core/receipts/probe/observation-receipt
 */

import { z } from 'zod';
import {
  BaseReceiptSchema,
  RECEIPT_TYPES,
  BLAKE3_HEX_LENGTH,
  deterministicSerialize,
  computeBlake3,
  computeChainHash,
} from '../base-receipt.mjs';

// =============================================================================
// Constants
// =============================================================================

/**
 * Probe observation receipt type discriminator
 * @constant {string}
 */
export const PROBE_OBSERVATION_TYPE = 'probe-observation';

/**
 * Serialization version for determinism
 * @constant {string}
 */
export const SERIALIZATION_VERSION = '1.0';

/**
 * Encoding format for determinism
 * @constant {string}
 */
export const ENCODING_FORMAT = 'json-deterministic';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Observation payload metadata schema
 */
export const ObservationMetadataSchema = z.object({
  /** Serialization version for reproducibility */
  serializationVersion: z.string(),
  /** Encoding format used */
  encoding: z.string(),
  /** Whether this observation was created with deterministic serialization */
  deterministic: z.boolean().default(true),
}).passthrough();

/**
 * Core observation data schema
 */
export const ObservationDataSchema = z.object({
  /** The actual observed data (any structure) */
  payload: z.any(),
  /** Nanosecond timestamp when observed */
  timestamp: z.bigint(),
  /** BLAKE3 hash of payload (deterministically serialized) */
  hash: z.string().length(BLAKE3_HEX_LENGTH),
  /** Metadata about serialization/encoding */
  metadata: ObservationMetadataSchema.optional(),
});

/**
 * Determinism check result schema
 */
export const DeterminismCheckSchema = z.object({
  /** Type of check performed */
  checkType: z.enum(['hash-recompute', 'serialization-stable', 'payload-integrity']),
  /** Value/result of the check */
  checkValue: z.string().optional(),
  /** Whether check passed */
  passed: z.boolean(),
  /** Details about check results */
  details: z.record(z.any()).optional(),
});

/**
 * Probe observation receipt schema - extends base with probe-specific fields
 */
export const ProbeObservationReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(PROBE_OBSERVATION_TYPE),

  // Per-agent chain fields
  /** Agent/shard that made this observation */
  agentId: z.string().min(1),

  /** Sequence number in agent's observation chain (1-indexed) */
  observationIndex: z.number().int().nonnegative().min(1),

  /** Hash of observation payload (deterministically serialized) */
  obsHash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Hash of previous observation in THIS agent's chain (null for genesis) */
  prevHash: z.string().length(BLAKE3_HEX_LENGTH).nullable(),

  /** Domain/category of observation */
  domain: z.string().min(1),

  // Observation data
  /** Complete observation record */
  observation: ObservationDataSchema,

  // Determinism proof
  /** Results of determinism checks */
  checks: z.array(DeterminismCheckSchema),

  // Optional signature
  attestation: z.object({
    algorithm: z.string(),
    publicKey: z.string(),
    signature: z.string(),
    signer: z.string().optional(),
  }).optional(),
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} ObservationMetadata
 * @property {string} serializationVersion - Version for reproducibility
 * @property {string} encoding - Encoding format used
 * @property {boolean} [deterministic=true] - Whether deterministically serialized
 */

/**
 * @typedef {Object} ObservationData
 * @property {any} payload - The actual observed data
 * @property {bigint} timestamp - Nanosecond timestamp
 * @property {string} hash - BLAKE3 hash of payload
 * @property {ObservationMetadata} [metadata] - Serialization metadata
 */

/**
 * @typedef {Object} DeterminismCheck
 * @property {'hash-recompute'|'serialization-stable'|'payload-integrity'} checkType
 * @property {string} [checkValue] - Check result value
 * @property {boolean} passed - Whether check passed
 * @property {Record<string, any>} [details] - Check details
 */

/**
 * @typedef {Object} ProbeObservationReceipt
 * @property {string} id - UUID of receipt
 * @property {'probe-observation'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash (v6-core chain)
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt chain hash
 * @property {string} agentId - Agent ID
 * @property {number} observationIndex - Sequence number
 * @property {string} obsHash - Observation hash
 * @property {string|null} prevHash - Previous observation hash (agent chain)
 * @property {string} domain - Domain/category
 * @property {ObservationData} observation - Observation record
 * @property {DeterminismCheck[]} checks - Determinism checks
 * @property {Object} [attestation] - Signature/attestation
 */

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Compute observation hash from payload
 *
 * Uses deterministic serialization to ensure reproducibility.
 *
 * @param {any} payload - Observation payload
 * @returns {Promise<string>} 64-character BLAKE3 hex hash
 */
export async function computeObsHash(payload) {
  const serialized = deterministicSerialize(payload);
  return computeBlake3(serialized);
}

/**
 * Check if observation hash is reproducible
 *
 * Recomputes hash from payload, compares with expected.
 *
 * @param {any} payload - Observation payload
 * @param {string} expectedHash - Expected hash
 * @returns {Promise<DeterminismCheck>}
 */
export async function checkHashRecompute(payload, expectedHash) {
  const serialized = deterministicSerialize(payload);
  const recomputed = await computeBlake3(serialized);

  return {
    checkType: 'hash-recompute',
    passed: recomputed === expectedHash,
    checkValue: recomputed,
    details: {
      expected: expectedHash,
      computed: recomputed,
    },
  };
}

/**
 * Check if observation serialization is stable
 *
 * Serializes and hashes observation 3 times to ensure consistency.
 *
 * @param {any} payload - Observation payload
 * @param {string} expectedHash - Expected final hash
 * @returns {Promise<DeterminismCheck>}
 */
export async function checkSerializationStable(payload, expectedHash) {
  // Serialize 3 times independently
  const ser1 = deterministicSerialize(payload);
  const ser2 = deterministicSerialize(payload);
  const ser3 = deterministicSerialize(payload);

  // Compute hashes
  const h1 = await computeBlake3(ser1);
  const h2 = await computeBlake3(ser2);
  const h3 = await computeBlake3(ser3);

  const internallyStable = h1 === h2 && h2 === h3;
  const globallyStable = h1 === expectedHash;

  return {
    checkType: 'serialization-stable',
    passed: internallyStable && globallyStable,
    details: {
      internallyStable,
      globallyStable,
      hashes: [h1, h2, h3],
      serializationIdentical: ser1 === ser2 && ser2 === ser3,
    },
  };
}

/**
 * Validate observation payload structure
 *
 * @param {any} payload - Payload to validate
 * @returns {DeterminismCheck}
 */
export function checkPayloadIntegrity(payload) {
  try {
    // Must be serializable
    const serialized = deterministicSerialize(payload);
    const canReparse = JSON.parse(serialized);

    return {
      checkType: 'payload-integrity',
      passed: true,
      details: {
        serializable: true,
        reparseSuccessful: true,
      },
    };
  } catch (error) {
    return {
      checkType: 'payload-integrity',
      passed: false,
      details: {
        serializable: false,
        error: error.message,
      },
    };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default ProbeObservationReceiptSchema;
