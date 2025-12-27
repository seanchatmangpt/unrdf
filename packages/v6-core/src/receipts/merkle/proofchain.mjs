/**
 * V6 Proof Chain - Receipt chain verification
 *
 * Provides functions to verify receipt chains and detect tampering.
 *
 * @module @unrdf/v6-core/receipts/merkle/proofchain
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Chain verification result schema
 */
export const ChainVerificationSchema = z.object({
  valid: z.boolean(),
  totalReceipts: z.number().int().nonnegative(),
  validReceipts: z.number().int().nonnegative(),
  tamperedReceipts: z.array(z.object({
    index: z.number(),
    receiptId: z.string(),
    reason: z.string(),
  })),
  errors: z.array(z.string()),
});

// =============================================================================
// Chain Verification
// =============================================================================

/**
 * Verify entire receipt chain
 *
 * Checks:
 * 1. Each receipt's hash integrity
 * 2. Chain links (previousHash references)
 * 3. Temporal ordering (monotonic timestamps)
 *
 * @param {Array<Object>} receipts - Array of receipts to verify
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyChain([receipt1, receipt2, receipt3]);
 * console.log(result.valid); // true if entire chain is valid
 * console.log(result.tamperedReceipts); // [] if no tampering
 */
export async function verifyChain(receipts) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    return ChainVerificationSchema.parse({
      valid: false,
      totalReceipts: 0,
      validReceipts: 0,
      tamperedReceipts: [],
      errors: ['receipts must be a non-empty array'],
    });
  }

  const tamperedReceipts = [];
  const errors = [];
  let validCount = 0;

  // Verify first receipt (genesis)
  const first = receipts[0];
  if (first.previousHash !== null) {
    tamperedReceipts.push({
      index: 0,
      receiptId: first.id,
      reason: 'Genesis receipt must have previousHash = null',
    });
  } else {
    const isValid = await verifyReceiptHash(first);
    if (isValid) {
      validCount++;
    } else {
      tamperedReceipts.push({
        index: 0,
        receiptId: first.id,
        reason: 'Hash integrity failed',
      });
    }
  }

  // Verify chain links
  for (let i = 1; i < receipts.length; i++) {
    const current = receipts[i];
    const previous = receipts[i - 1];

    // Check hash integrity
    const hashValid = await verifyReceiptHash(current);
    if (!hashValid) {
      tamperedReceipts.push({
        index: i,
        receiptId: current.id,
        reason: 'Hash integrity failed',
      });
      continue;
    }

    // Check chain link
    if (current.previousHash !== previous.hash) {
      tamperedReceipts.push({
        index: i,
        receiptId: current.id,
        reason: `Chain broken: previousHash (${current.previousHash}) != previous.hash (${previous.hash})`,
      });
      continue;
    }

    // Check temporal ordering
    if (current.timestamp_ns && previous.timestamp_ns) {
      const currTime = BigInt(current.timestamp_ns);
      const prevTime = BigInt(previous.timestamp_ns);
      if (currTime <= prevTime) {
        tamperedReceipts.push({
          index: i,
          receiptId: current.id,
          reason: `Temporal violation: timestamp (${currTime}) <= previous timestamp (${prevTime})`,
        });
        continue;
      }
    }

    validCount++;
  }

  const valid = tamperedReceipts.length === 0 && errors.length === 0;

  return ChainVerificationSchema.parse({
    valid,
    totalReceipts: receipts.length,
    validReceipts: validCount,
    tamperedReceipts,
    errors,
  });
}

/**
 * Find tampered receipts in a chain
 *
 * @param {Array<Object>} receipts - Array of receipts to check
 * @returns {Promise<Array<Object>>} Array of tampered receipts with reasons
 */
export async function findTamperedReceipts(receipts) {
  const result = await verifyChain(receipts);
  return result.tamperedReceipts;
}

/**
 * Reconstruct chain state from receipts
 *
 * Builds a timeline of decisions from receipt chain.
 *
 * @param {Array<Object>} receipts - Array of receipts
 * @returns {Promise<Object>} Reconstructed chain state
 *
 * @example
 * const state = await reconstructChainState(receipts);
 * console.log(state.timeline); // Array of decisions in order
 */
export async function reconstructChainState(receipts) {
  const verifyResult = await verifyChain(receipts);

  const timeline = receipts.map((r, idx) => ({
    index: idx,
    receiptId: r.id,
    timestamp: r.timestamp_iso || new Date(Number(r.timestamp_ns) / 1_000_000).toISOString(),
    eventType: r.payload?.eventType || r.eventType || 'unknown',
    decision: r.payload?.decision,
    actor: r.payload?.actor,
    hash: r.hash,
    previousHash: r.previousHash,
  }));

  return {
    chainValid: verifyResult.valid,
    receiptCount: receipts.length,
    firstReceipt: timeline[0],
    lastReceipt: timeline[timeline.length - 1],
    timeline,
    tamperedReceipts: verifyResult.tamperedReceipts,
  };
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Verify a single receipt's hash integrity
 *
 * Recomputes hash from payload and chain data, compares to stored hash.
 *
 * @private
 * @param {Object} receipt - Receipt to verify
 * @returns {Promise<boolean>} True if hash is valid
 */
async function verifyReceiptHash(receipt) {
  if (!receipt.hash || !receipt.payload) {
    return false;
  }

  try {
    // Recompute payload hash
    const payloadStr = JSON.stringify(receipt.payload);
    const payloadHash = await blake3(payloadStr);

    // Recompute chain hash
    const chainInput = (receipt.previousHash || 'GENESIS') + ':' + payloadHash;
    const recomputedHash = await blake3(chainInput);

    return recomputedHash === receipt.hash;
  } catch {
    return false;
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  verifyChain,
  findTamperedReceipts,
  reconstructChainState,
};
