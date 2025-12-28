/**
 * Probe Observation Receipt Verifier
 *
 * Verifies that observations were created deterministically.
 *
 * @module @unrdf/v6-core/receipts/probe/verifiers/observation-verifier
 */

import {
  deterministicSerialize,
  computeBlake3,
  computeChainHash,
} from '../../base-receipt.mjs';

import {
  checkHashRecompute,
  checkSerializationStable,
} from '../observation-receipt.mjs';

// =============================================================================
// Main Verification
// =============================================================================

/**
 * Verify a probe observation receipt
 *
 * Checks:
 * 1. Schema validation
 * 2. Observation hash integrity
 * 3. Determinism checks
 * 4. Receipt chain integrity
 *
 * @param {Object} receipt - ProbeObservationReceipt to verify
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyProbeObservation(receipt);
 * console.log(result.valid); // true if all checks pass
 * console.log(result.checks); // Details of each check
 */
export async function verifyProbeObservation(receipt) {
  const errors = [];
  const checks = {
    schemaValid: false,
    hashIntegrityValid: false,
    determinismValid: false,
    chainIntegrityValid: false,
  };

  // 1. Validate schema structure
  if (!receipt.receiptType || receipt.receiptType !== 'probe-observation') {
    errors.push('Invalid receiptType');
    return { valid: false, checks, errors };
  }

  if (!receipt.obsHash || receipt.obsHash.length !== 64) {
    errors.push('Invalid obsHash format');
    return { valid: false, checks, errors };
  }

  if (!receipt.observation || !receipt.observation.payload) {
    errors.push('Missing observation payload');
    return { valid: false, checks, errors };
  }

  checks.schemaValid = true;

  // 2. Verify observation hash integrity
  try {
    const checkResult = await checkHashRecompute(
      receipt.observation.payload,
      receipt.obsHash
    );

    checks.hashIntegrityValid = checkResult.passed;

    if (!checkResult.passed) {
      errors.push(`Observation hash mismatch: expected ${receipt.obsHash}, got ${checkResult.checkValue}`);
    }
  } catch (error) {
    checks.hashIntegrityValid = false;
    errors.push(`Hash recompute failed: ${error.message}`);
  }

  // 3. Verify determinism checks
  try {
    if (!receipt.checks || receipt.checks.length === 0) {
      checks.determinismValid = false;
      errors.push('No determinism checks recorded');
    } else {
      const allChecksPassed = receipt.checks.every((c) => c.passed);

      if (!allChecksPassed) {
        const failedChecks = receipt.checks
          .filter((c) => !c.passed)
          .map((c) => c.checkType);

        checks.determinismValid = false;
        errors.push(`Determinism checks failed: ${failedChecks.join(', ')}`);
      } else {
        checks.determinismValid = true;
      }
    }
  } catch (error) {
    checks.determinismValid = false;
    errors.push(`Determinism validation failed: ${error.message}`);
  }

  // 4. Verify receipt hash chain
  try {
    const payloadToHash = { ...receipt };
    delete payloadToHash.receiptHash;
    delete payloadToHash.payloadHash;

    const computedPayloadHash = await computeBlake3(payloadToHash);

    if (computedPayloadHash !== receipt.payloadHash) {
      errors.push('Payload hash mismatch');
      checks.chainIntegrityValid = false;
    } else {
      // Verify receipt chain hash
      const chainInput = `${receipt.previousHash || 'GENESIS'}:${receipt.payloadHash}`;
      const computedReceiptHash = await computeChainHash(receipt.previousHash, receipt.payloadHash);

      if (computedReceiptHash !== receipt.receiptHash) {
        errors.push(`Receipt hash mismatch: expected ${receipt.receiptHash}, got ${computedReceiptHash}`);
        checks.chainIntegrityValid = false;
      } else {
        checks.chainIntegrityValid = true;
      }
    }
  } catch (error) {
    checks.chainIntegrityValid = false;
    errors.push(`Chain integrity check failed: ${error.message}`);
  }

  const valid = errors.length === 0;

  return {
    valid,
    checks,
    errors,
  };
}

/**
 * Verify observation timestamp is valid
 *
 * @param {Object} receipt - ProbeObservationReceipt
 * @returns {Object} Verification result
 */
export function verifyObservationTimestamp(receipt) {
  const errors = [];

  if (!receipt.t_ns || receipt.t_ns <= 0n) {
    errors.push('Invalid receipt timestamp');
    return { valid: false, errors };
  }

  if (!receipt.observation.timestamp || receipt.observation.timestamp <= 0n) {
    errors.push('Invalid observation timestamp');
    return { valid: false, errors };
  }

  // Observation timestamp should be close to receipt timestamp
  const timeDiff = Math.abs(Number(receipt.t_ns - receipt.observation.timestamp) / 1_000_000); // Convert to ms
  if (timeDiff > 5000) { // Allow 5 second difference
    errors.push(`Large time difference between receipt and observation: ${timeDiff}ms`);
  }

  return {
    valid: errors.length === 0,
    errors,
    timeDiff,
  };
}

/**
 * Verify observation index sequence
 *
 * @param {Array<Object>} receipts - Array of receipts to check
 * @returns {Object} Verification result
 */
export function verifyObservationSequence(receipts) {
  const errors = [];

  if (!receipts || receipts.length === 0) {
    return { valid: true, errors };
  }

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];
    const expectedIndex = i + 1;

    if (receipt.observationIndex !== expectedIndex) {
      errors.push(`Index mismatch at position ${i}: expected ${expectedIndex}, got ${receipt.observationIndex}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
    receiptCount: receipts.length,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default {
  verifyProbeObservation,
  verifyObservationTimestamp,
  verifyObservationSequence,
};
