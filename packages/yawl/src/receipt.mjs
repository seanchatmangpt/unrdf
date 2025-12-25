/**
 * YAWL Receipt - BLAKE3 Cryptographic Proof of Workflow Transitions
 *
 * Implements cryptographic receipts for audit trail and verification.
 * Each receipt chains to the previous, creating an immutable proof chain.
 *
 * @module @unrdf/yawl/receipt
 */

// =============================================================================
// Core Exports
// =============================================================================

export {
  // Constants
  BLAKE3_HEX_LENGTH,
  RECEIPT_EVENT_TYPES,

  // Schemas
  JustificationSchema,
  PayloadSchema,
  VectorClockSchema,
  EventTypeSchema,
  ReceiptSchema,
  VerificationResultSchema,

  // Utility Functions
  generateUUID,
  deterministicSerialize,
  computeBlake3,
  computeChainHash,

  // Receipt Generation
  generateReceipt,
} from './receipt-core.mjs';

// =============================================================================
// Verification Exports
// =============================================================================

export {
  verifyReceipt,
  verifyChainLink,
} from './receipt-verification.mjs';

// =============================================================================
// ProofChain Exports
// =============================================================================

export {
  ProofChain,
} from './receipt-proofchain.mjs';

// =============================================================================
// Serialization Exports (Legacy)
// =============================================================================

export {
  YawlReceipt,
  buildReceipt,
} from './receipt-serialization.mjs';

// =============================================================================
// Default Export (Full API)
// =============================================================================

import {
  RECEIPT_EVENT_TYPES,
  ReceiptSchema,
  JustificationSchema,
  PayloadSchema,
  generateReceipt,
} from './receipt-core.mjs';

import {
  verifyReceipt,
  verifyChainLink,
} from './receipt-verification.mjs';

import {
  ProofChain,
} from './receipt-proofchain.mjs';

import {
  YawlReceipt,
  buildReceipt,
} from './receipt-serialization.mjs';

export default {
  // Main API
  generateReceipt,
  verifyReceipt,
  verifyChainLink,
  ProofChain,

  // Legacy API
  YawlReceipt,
  buildReceipt,

  // Constants
  RECEIPT_EVENT_TYPES,

  // Schemas
  ReceiptSchema,
  JustificationSchema,
  PayloadSchema,
};
