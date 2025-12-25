/**
 * YAWL Receipt - BLAKE3 Cryptographic Proof of Workflow Transitions
 *
 * Barrel export for all receipt functionality.
 *
 * @module @unrdf/yawl/receipt
 */

// Core functionality
export {
  BLAKE3_HEX_LENGTH,
  RECEIPT_EVENT_TYPES,
  ReceiptSchema,
  JustificationSchema,
  PayloadSchema,
  VectorClockSchema,
  VerificationResultSchema,
  generateUUID,
  deterministicSerialize,
  computeBlake3,
  computeChainHash,
  generateReceipt,
  verifyReceipt,
  verifyChainLink,
} from './receipt-core.mjs';

// ProofChain
export { ProofChain } from './receipt-chain.mjs';

// Legacy API
export { YawlReceipt, buildReceipt } from './receipt-legacy.mjs';

// Default export
import { generateReceipt, verifyReceipt, verifyChainLink } from './receipt-core.mjs';
import { ProofChain } from './receipt-chain.mjs';
import { YawlReceipt, buildReceipt } from './receipt-legacy.mjs';
import {
  RECEIPT_EVENT_TYPES,
  ReceiptSchema,
  JustificationSchema,
  PayloadSchema,
} from './receipt-core.mjs';

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
