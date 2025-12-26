/**
 * YAWL Receipt Verification - Chain Validation and Integrity Checks
 *
 * Provides verification functions for cryptographic receipt integrity.
 *
 * @module @unrdf/yawl/receipt-verification
 */

import {
  computeBlake3,
  computeChainHash,
} from './receipt-core.mjs';

// =============================================================================
// Receipt Verification
// =============================================================================

/**
 * Verify a receipt's cryptographic integrity
 *
 * Recomputes hashes and verifies the chain link.
 *
 * @param {import('./receipt-core.mjs').Receipt} receipt - Receipt to verify
 * @returns {Promise<import('./receipt-core.mjs').VerificationResult>} Verification result
 *
 * @example
 * const result = await verifyReceipt(receipt);
 * if (!result.valid) {
 *   console.error('Receipt verification failed:', result.error);
 * }
 */
export async function verifyReceipt(receipt) {
  try {
    // 1. Recompute payload hash
    const payloadToHash = {
      eventType: receipt.eventType,
      caseId: receipt.caseId,
      taskId: receipt.taskId,
      workItemId: receipt.workItemId || null,
      payload: receipt.payload,
      t_ns: receipt.t_ns.toString(),
    };
    const computedPayloadHash = await computeBlake3(payloadToHash);
    const payloadHashValid = computedPayloadHash === receipt.payloadHash;

    // 2. Recompute chain hash
    const computedReceiptHash = await computeChainHash(
      receipt.previousReceiptHash,
      receipt.payloadHash
    );
    const chainHashValid = computedReceiptHash === receipt.receiptHash;

    // 3. Validate timestamp (must be positive and reasonable)
    const timestampValid = receipt.t_ns > 0n;

    // 4. Determine overall validity
    const valid = payloadHashValid && chainHashValid && timestampValid;

    if (!valid) {
      const errors = [];
      if (!payloadHashValid) errors.push('payload hash mismatch');
      if (!chainHashValid) errors.push('chain hash mismatch');
      if (!timestampValid) errors.push('invalid timestamp');

      return {
        valid: false,
        error: `Verification failed: ${errors.join(', ')}`,
        checks: {
          payloadHashValid,
          chainHashValid,
          timestampValid,
        },
      };
    }

    return {
      valid: true,
      checks: {
        payloadHashValid,
        chainHashValid,
        timestampValid,
      },
    };
  } catch (error) {
    return {
      valid: false,
      error: `Verification error: ${error.message}`,
    };
  }
}

/**
 * Verify a receipt chain link
 *
 * @param {import('./receipt-core.mjs').Receipt} current - Current receipt
 * @param {import('./receipt-core.mjs').Receipt} previous - Previous receipt in chain
 * @returns {Promise<import('./receipt-core.mjs').VerificationResult>} Chain verification result
 */
export async function verifyChainLink(current, previous) {
  // Verify current receipt independently
  const currentResult = await verifyReceipt(current);
  if (!currentResult.valid) {
    return currentResult;
  }

  // Verify previous receipt independently
  const previousResult = await verifyReceipt(previous);
  if (!previousResult.valid) {
    return {
      valid: false,
      error: `Previous receipt invalid: ${previousResult.error}`,
    };
  }

  // Verify chain link
  if (current.previousReceiptHash !== previous.receiptHash) {
    return {
      valid: false,
      error: `Chain broken: expected ${previous.receiptHash}, got ${current.previousReceiptHash}`,
    };
  }

  // Verify temporal ordering
  if (current.t_ns <= previous.t_ns) {
    return {
      valid: false,
      error: 'Temporal ordering violated: current receipt timestamp not after previous',
    };
  }

  return { valid: true };
}
