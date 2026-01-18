/**
 * Verification - Tamper detection and integrity checks
 * @module verify
 */

import { hashCapsule } from './hash.mjs';
import { CapsuleSchema } from './capsule.mjs';
import { fromISO } from '../../../packages/kgc-4d/src/index.mjs';

/**
 * Verify capsule integrity
 * Checks:
 * 1. Schema validation (Zod)
 * 2. Hash correctness (recompute and compare)
 * 3. Parent chain integrity
 * 4. Timestamp validity
 *
 * @param {Object} capsule - Capsule to verify
 * @param {Array<string>} [expectedParents] - Expected parent hashes
 * @returns {Promise<{ ok: boolean, reason?: string }>}
 */
export async function verifyCapsule(capsule, expectedParents = null) {
  try {
    // 1. Schema validation
    CapsuleSchema.parse(capsule);
  } catch (err) {
    return { ok: false, reason: `Schema validation failed: ${err.message}` };
  }

  // 2. Recompute hash
  const recomputedHash = await hashCapsule(capsule);
  if (recomputedHash !== capsule.id) {
    return {
      ok: false,
      reason: `Hash mismatch: expected ${capsule.id}, got ${recomputedHash}`,
    };
  }

  // 3. Verify receipt hash matches capsule ID
  if (capsule.receipt.hash !== capsule.id) {
    return {
      ok: false,
      reason: `Receipt hash mismatch: ${capsule.receipt.hash} !== ${capsule.id}`,
    };
  }

  // 4. Verify parent hashes (if expected parents provided)
  if (expectedParents !== null) {
    const actualParents = capsule.receipt.parents.slice().sort();
    const expectedSorted = expectedParents.slice().sort();

    if (JSON.stringify(actualParents) !== JSON.stringify(expectedSorted)) {
      return {
        ok: false,
        reason: `Parent mismatch: expected [${expectedSorted}], got [${actualParents}]`,
      };
    }
  }

  // 5. Verify timestamp is valid ISO 8601
  try {
    fromISO(capsule.receipt.timestamp);
  } catch (err) {
    return { ok: false, reason: `Invalid timestamp: ${err.message}` };
  }

  // 6. Verify guard limits are reasonable
  const { maxQuads, maxDepth, timeout } = capsule.guard.limits;
  if (maxQuads < 1 || maxDepth < 1 || timeout < 1) {
    return { ok: false, reason: 'Guard limits must be positive integers' };
  }

  return { ok: true };
}

/**
 * Detect tampering by comparing stored vs computed hash
 * @param {Object} capsule - Capsule to check
 * @returns {Promise<boolean>} True if tampered
 */
export async function detectTampering(capsule) {
  const { ok } = await verifyCapsule(capsule);
  return !ok;
}
