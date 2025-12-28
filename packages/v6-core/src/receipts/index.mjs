/**
 * V6 Unified Receipt System - Main Export Module
 *
 * Provides unified factory functions and validators for all receipt types.
 *
 * @module @unrdf/v6-core/receipts
 */

import { now, toISO } from '@unrdf/kgc-4d';

// Base schemas and utilities
import {
  BaseReceiptSchema,
  RECEIPT_TYPES,
  BLAKE3_HEX_LENGTH,
  generateUUID,
  deterministicSerialize,
  computeBlake3,
  computeChainHash,
  verifyBaseReceipt,
  ReceiptTypeSchema,
  AttestationSchema,
  VectorClockSchema,
} from './base-receipt.mjs';

// Specific receipt schemas
import ExecutionReceiptSchema, {
  EXECUTION_EVENT_TYPES,
  ExecutionEventTypeSchema,
  JustificationSchema,
  ExecutionPayloadSchema,
} from './execution-receipt.mjs';

import AllocationReceiptSchema, {
  ALLOCATION_EVENT_TYPES,
  AllocationEventTypeSchema,
  AllocationPeriodSchema,
  CapacitySchema,
  AllocationPayloadSchema,
} from './allocation-receipt.mjs';

import CompileReceiptSchema, {
  COMPILE_EVENT_TYPES,
  GRAMMAR_TYPES,
  CompileEventTypeSchema,
  GrammarTypeSchema,
  CompilationMetadataSchema,
  CompilePayloadSchema,
} from './compile-receipt.mjs';

import VerificationReceiptSchema, {
  VERIFICATION_EVENT_TYPES,
  VerificationEventTypeSchema,
  MerkleProofStepSchema,
  BlockchainAnchorSchema,
  VerificationPayloadSchema,
} from './verification-receipt.mjs';

// =============================================================================
// Union Schema for All Receipt Types
// =============================================================================

import { z } from 'zod';

/**
 * Unified Receipt Schema - Discriminated union of all receipt types
 *
 * Validates any receipt type based on the `receiptType` discriminator field.
 */
export const ReceiptSchema = z.discriminatedUnion('receiptType', [
  ExecutionReceiptSchema,
  AllocationReceiptSchema,
  CompileReceiptSchema,
  VerificationReceiptSchema,
]);

// =============================================================================
// Unified Receipt Factory
// =============================================================================

/**
 * Create a receipt of the specified type
 *
 * Unified factory function that generates properly typed receipts with
 * cryptographic proofs.
 *
 * @param {'execution'|'allocation'|'compile'|'verification'} type - Receipt type
 * @param {Object} event - Event data specific to receipt type
 * @param {Object|null} [previousReceipt=null] - Previous receipt for chaining
 * @returns {Promise<Object>} Complete receipt with hashes
 *
 * @example
 * // Execution receipt
 * const execReceipt = await createReceipt('execution', {
 *   eventType: 'TASK_COMPLETED',
 *   caseId: 'case-123',
 *   taskId: 'approval',
 *   payload: { decision: 'APPROVE' }
 * });
 *
 * @example
 * // Allocation receipt
 * const allocReceipt = await createReceipt('allocation', {
 *   eventType: 'RESOURCE_ALLOCATED',
 *   resourceId: 'res-456',
 *   poolId: 'pool-789',
 *   allocationPeriod: { start: '2025-01-01', end: '2025-01-02' },
 *   capacity: { total: 100, available: 80, allocated: 20, unit: 'hours' },
 *   payload: { action: 'ALLOCATE' }
 * });
 *
 * @example
 * // Compile receipt
 * const compileReceipt = await createReceipt('compile', {
 *   eventType: 'GRAMMAR_COMPILED',
 *   inputHashes: ['abc...', 'def...'],
 *   outputHash: 'ghi...',
 *   compilerVersion: '1.0.0',
 *   grammarType: 'SPARQL',
 *   payload: { result: 'SUCCESS', metadata: { inputCount: 2, outputCount: 1 } }
 * });
 *
 * @example
 * // Verification receipt
 * const verifyReceipt = await createReceipt('verification', {
 *   eventType: 'MERKLE_PROOF_VERIFIED',
 *   verifiedHash: 'abc...',
 *   merkleRoot: 'def...',
 *   proofPath: [{ hash: 'ghi...', position: 'left' }],
 *   payload: { result: 'VALID', method: 'merkle-tree' }
 * });
 */
export async function createReceipt(type, event, previousReceipt = null) {
  // Validate type
  if (!Object.values(RECEIPT_TYPES).includes(type)) {
    throw new Error(`Invalid receipt type: ${type}. Must be one of: ${Object.values(RECEIPT_TYPES).join(', ')}`);
  }

  // 1. Extract internal fields (not part of payload)
  const { _deterministicId, _timestampProvider, ...eventData } = event;

  // 2. Generate receipt ID and timestamp
  // Support deterministic ID for testing (via event._deterministicId)
  const id = _deterministicId || generateUUID();
  const t_ns = _timestampProvider ? _timestampProvider() : now();
  const timestamp_iso = toISO(t_ns);

  // 3. Build receipt payload (type-specific fields + base fields)
  const receiptData = {
    id,
    receiptType: type,
    t_ns,
    timestamp_iso,
    ...eventData,
    // Preserve optional fields
    attestation: event.attestation,
    vectorClock: event.vectorClock,
    gitRef: event.gitRef,
    kgcEventId: event.kgcEventId,
  };

  // 4. Compute payload hash (exclude hash fields)
  const payloadToHash = { ...receiptData };
  const payloadHash = await computeBlake3(payloadToHash);

  // 5. Get previous receipt hash for chain
  const previousHash = previousReceipt ? previousReceipt.receiptHash : null;

  // 6. Compute chained receipt hash
  const receiptHash = await computeChainHash(previousHash, payloadHash);

  // 7. Build complete receipt
  const receipt = {
    ...receiptData,
    previousHash,
    payloadHash,
    receiptHash,
  };

  // 8. Validate against appropriate schema
  let schema;
  switch (type) {
    case RECEIPT_TYPES.EXECUTION:
      schema = ExecutionReceiptSchema;
      break;
    case RECEIPT_TYPES.ALLOCATION:
      schema = AllocationReceiptSchema;
      break;
    case RECEIPT_TYPES.COMPILE:
      schema = CompileReceiptSchema;
      break;
    case RECEIPT_TYPES.VERIFICATION:
      schema = VerificationReceiptSchema;
      break;
    default:
      throw new Error(`Unsupported receipt type: ${type}`);
  }

  return schema.parse(receipt);
}

// =============================================================================
// Unified Receipt Verifier
// =============================================================================

/**
 * Verify a receipt of any type
 *
 * Validates schema, hashes, and type-specific constraints.
 *
 * @param {Object} receipt - Receipt to verify
 * @returns {Promise<{valid: boolean, error?: string, checks?: Object}>}
 */
export async function verifyReceipt(receipt) {
  // First verify base receipt structure
  const baseVerification = await verifyBaseReceipt(receipt);
  if (!baseVerification.valid) {
    return baseVerification;
  }

  // Then validate against specific schema
  try {
    let schema;
    switch (receipt.receiptType) {
      case RECEIPT_TYPES.EXECUTION:
        schema = ExecutionReceiptSchema;
        break;
      case RECEIPT_TYPES.ALLOCATION:
        schema = AllocationReceiptSchema;
        break;
      case RECEIPT_TYPES.COMPILE:
        schema = CompileReceiptSchema;
        break;
      case RECEIPT_TYPES.VERIFICATION:
        schema = VerificationReceiptSchema;
        break;
      default:
        return {
          valid: false,
          error: `Unknown receipt type: ${receipt.receiptType}`,
        };
    }

    schema.parse(receipt);

    return {
      valid: true,
      checks: baseVerification.checks,
    };
  } catch (error) {
    return {
      valid: false,
      error: `Schema validation failed: ${error.message}`,
      checks: baseVerification.checks,
    };
  }
}

/**
 * Verify chain link between two receipts
 *
 * @param {Object} receipt - Current receipt
 * @param {Object} previousReceipt - Previous receipt
 * @returns {Promise<{valid: boolean, error?: string}>}
 */
export async function verifyChainLink(receipt, previousReceipt) {
  // Verify current receipt links to previous
  if (receipt.previousHash !== previousReceipt.receiptHash) {
    return {
      valid: false,
      error: `Chain broken: expected previousHash ${previousReceipt.receiptHash}, got ${receipt.previousHash}`,
    };
  }

  // Verify temporal ordering
  if (receipt.t_ns <= previousReceipt.t_ns) {
    return {
      valid: false,
      error: 'Temporal ordering violated: receipt timestamp must be after previous receipt',
    };
  }

  return { valid: true };
}

// =============================================================================
// Exports
// =============================================================================

// Factory and verifiers
// Note: createReceipt, verifyReceipt, and verifyChainLink are already exported
// as named exports above (lines 115, 192, 244)

// Base utilities
export {
  RECEIPT_TYPES,
  BLAKE3_HEX_LENGTH,
  generateUUID,
  deterministicSerialize,
  computeBlake3,
  computeChainHash,
  verifyBaseReceipt,
};

// Schemas
// Note: ReceiptSchema is already exported above as const declaration (line 70)
export {
  BaseReceiptSchema,
  ReceiptTypeSchema,
  AttestationSchema,
  VectorClockSchema,
  ExecutionReceiptSchema,
  AllocationReceiptSchema,
  CompileReceiptSchema,
  VerificationReceiptSchema,
};

// Event types
export {
  EXECUTION_EVENT_TYPES,
  ALLOCATION_EVENT_TYPES,
  COMPILE_EVENT_TYPES,
  GRAMMAR_TYPES,
  VERIFICATION_EVENT_TYPES,
};

// Sub-schemas
export {
  ExecutionEventTypeSchema,
  JustificationSchema,
  ExecutionPayloadSchema,
  AllocationEventTypeSchema,
  AllocationPeriodSchema,
  CapacitySchema,
  AllocationPayloadSchema,
  CompileEventTypeSchema,
  GrammarTypeSchema,
  CompilationMetadataSchema,
  CompilePayloadSchema,
  VerificationEventTypeSchema,
  MerkleProofStepSchema,
  BlockchainAnchorSchema,
  VerificationPayloadSchema,
};

// Merkle tree utilities
export { default as MerkleTree } from './merkle/index.mjs';

// P0-001: KGC-4D Receipt Wrapper HOF
export { withReceipt, createReceiptChain, verifyIdempotency } from './with-receipt.mjs';

// Default export
export default {
  createReceipt,
  verifyReceipt,
  verifyChainLink,
  RECEIPT_TYPES,
  EXECUTION_EVENT_TYPES,
  ALLOCATION_EVENT_TYPES,
  COMPILE_EVENT_TYPES,
  GRAMMAR_TYPES,
  VERIFICATION_EVENT_TYPES,
};
