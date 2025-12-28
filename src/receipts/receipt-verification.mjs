/**
 * @fileoverview Receipt Verification - Hash validation and schema verification
 *
 * @module receipts/receipt-verification
 */

import { computeReceiptHash } from './receipt-builder.mjs';
import { UniversalReceiptSchema } from './receipt-schemas.mjs';

// ============================================================================
// Verification Functions
// ============================================================================

/**
 * Verify receipt hash integrity
 *
 * @param {import('./receipt-schemas.mjs').UniversalReceipt} receipt
 * @returns {Promise<boolean>} True if hash is valid
 */
export async function verifyReceiptHash(receipt) {
  const computedHash = await computeReceiptHash(receipt);
  return computedHash === receipt.receiptHash;
}

/**
 * Validate receipt against schema
 *
 * @param {object} receipt
 * @returns {{ valid: boolean, errors: string[] }}
 */
export function validateReceipt(receipt) {
  const result = UniversalReceiptSchema.safeParse(receipt);

  if (result.success) {
    return { valid: true, errors: [] };
  }

  return {
    valid: false,
    errors: result.error.errors.map(e => `${e.path.join('.')}: ${e.message}`),
  };
}
