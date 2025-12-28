/**
 * Probe Verification Receipt - Comprehensive integrity proof
 *
 * Proves that observations, chains, and merge are deterministic and conflict-free.
 * Links back to original observations via certificate chain.
 *
 * @module @unrdf/v6-core/receipts/probe/verification-receipt
 */

import { z } from 'zod';
import {
  BaseReceiptSchema,
  RECEIPT_TYPES,
  BLAKE3_HEX_LENGTH,
} from '../base-receipt.mjs';

// =============================================================================
// Constants
// =============================================================================

/**
 * Probe verification receipt type discriminator
 * @constant {string}
 */
export const PROBE_VERIFICATION_TYPE = 'probe-verification';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Verification check result schema
 */
export const VerificationCheckSchema = z.object({
  /** Type of check performed */
  checkType: z.enum([
    'observation-hash-recompute',
    'chain-integrity',
    'merkle-root-recompute',
    'temporal-ordering',
    'shard-consistency',
  ]),

  /** Agent ID (null for global checks) */
  agentId: z.string().optional().nullable(),

  /** Whether check passed */
  passed: z.boolean(),

  /** Check-specific details */
  details: z.record(z.any()).optional(),

  /** Error message if check failed */
  errorMessage: z.string().optional(),
});

/**
 * Certificate chain step schema
 *
 * Proves relationship between observations, chains, and merge.
 */
export const CertificateChainStepSchema = z.object({
  /** Receipt hash being verified */
  receiptHash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Type of receipt */
  receiptType: z.enum(['probe-observation', 'probe-merge']),

  /** Relationship to next step */
  relationship: z.string(),

  /** Additional context */
  context: z.object({
    agentId: z.string().optional(),
    observationIndex: z.number().optional(),
    mergeId: z.string().optional(),
  }).optional(),
});

/**
 * Probe verification receipt schema
 */
export const ProbeVerificationReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(PROBE_VERIFICATION_TYPE),

  // Verification identification
  /** Unique verification identifier */
  verificationId: z.string().min(1),

  /** Hash of merge receipt being verified */
  mergeReceiptHash: z.string().length(BLAKE3_HEX_LENGTH),

  // Verification results
  /** Array of all verification checks performed */
  verifications: z.array(VerificationCheckSchema),

  /** Whether all observations reproduced identically */
  deterministic: z.boolean(),

  /** Whether merge is conflict-free */
  conflictFree: z.boolean(),

  // Certificate chain for audit
  /** Complete chain linking back to original observations */
  certificateChain: z.array(CertificateChainStepSchema),

  // Metadata
  /** Total number of observations verified */
  obsCount: z.number().int().nonnegative(),

  /** Number of agents verified */
  agentCount: z.number().int().nonnegative(),

  /** Timestamp of verification */
  verifiedAt: z.string().optional(),

  /** Verifier identifier (optional) */
  verifierId: z.string().optional(),
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} VerificationCheck
 * @property {'observation-hash-recompute'|'chain-integrity'|'merkle-root-recompute'|'temporal-ordering'|'shard-consistency'} checkType
 * @property {string} [agentId] - Agent being verified (null for global)
 * @property {boolean} passed - Whether check passed
 * @property {Record<string, any>} [details] - Check details
 * @property {string} [errorMessage] - Error if failed
 */

/**
 * @typedef {Object} CertificateChainStep
 * @property {string} receiptHash - Receipt hash
 * @property {'probe-observation'|'probe-merge'} receiptType - Receipt type
 * @property {string} relationship - Relationship to next step
 * @property {Object} [context] - Additional context
 * @property {string} [context.agentId] - Agent ID
 * @property {number} [context.observationIndex] - Observation index
 * @property {string} [context.mergeId] - Merge ID
 */

/**
 * @typedef {Object} ProbeVerificationReceipt
 * @property {string} id - UUID of receipt
 * @property {'probe-verification'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} verificationId - Verification ID
 * @property {string} mergeReceiptHash - Merge receipt hash
 * @property {VerificationCheck[]} verifications - Checks performed
 * @property {boolean} deterministic - Determinism result
 * @property {boolean} conflictFree - Conflict-free result
 * @property {CertificateChainStep[]} certificateChain - Audit chain
 * @property {number} obsCount - Total observations
 * @property {number} agentCount - Total agents
 * @property {string} [verifiedAt] - Verification timestamp
 * @property {string} [verifierId] - Verifier ID
 */

// =============================================================================
// Verification Summary Utilities
// =============================================================================

/**
 * Summarize verification results
 *
 * @param {ProbeVerificationReceipt} receipt - Verification receipt
 * @returns {Object} Summary object
 *
 * @example
 * const summary = summarizeVerification(receipt);
 * console.log(summary.passed); // true if all checks passed
 * console.log(summary.passCount); // number of passed checks
 */
export function summarizeVerification(receipt) {
  const totalChecks = receipt.verifications.length;
  const passedChecks = receipt.verifications.filter((c) => c.passed).length;
  const failedChecks = totalChecks - passedChecks;

  return {
    passed: failedChecks === 0,
    totalChecks,
    passedChecks,
    failedChecks,
    deterministic: receipt.deterministic,
    conflictFree: receipt.conflictFree,
    obsCount: receipt.obsCount,
    agentCount: receipt.agentCount,
    chainLength: receipt.certificateChain.length,
  };
}

/**
 * Get failed checks from verification receipt
 *
 * @param {ProbeVerificationReceipt} receipt - Verification receipt
 * @returns {VerificationCheck[]} Array of failed checks
 */
export function getFailedChecks(receipt) {
  return receipt.verifications.filter((c) => !c.passed);
}

/**
 * Get confidence score (0-100)
 *
 * Based on number of passed checks and properties.
 *
 * @param {ProbeVerificationReceipt} receipt - Verification receipt
 * @returns {number} Confidence percentage (0-100)
 */
export function getConfidenceScore(receipt) {
  if (receipt.verifications.length === 0) {
    return 0;
  }

  const passRatio = receipt.verifications.filter((c) => c.passed).length / receipt.verifications.length;
  const baseScore = passRatio * 100;

  // Boost for determinism + conflict-free
  const deterministicBoost = receipt.deterministic ? 0 : -10;
  const conflictFreeBoost = receipt.conflictFree ? 0 : -10;

  // Boost for large observation count (more thorough)
  const obsCountBoost = Math.min((receipt.obsCount / 100) * 5, 5);

  const finalScore = Math.max(0, Math.min(100, baseScore + deterministicBoost + conflictFreeBoost + obsCountBoost));

  return Math.round(finalScore);
}

// =============================================================================
// Exports
// =============================================================================

export default ProbeVerificationReceiptSchema;
