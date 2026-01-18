/**
 * @fileoverview Determinism Enforcement - Hash Verification
 * @module agent-10/determinism-test
 *
 * Validates that all agent primitives produce identical outputs
 * across multiple runs with identical inputs.
 *
 * Principle: No randomness tolerated in production path.
 *
 * Validation Protocol:
 * 1. Run e2e validation (run1)
 * 2. Run e2e validation (run2)
 * 3. Assert: ALL hashes identical
 * 4. Optional: Run 10 times for statistical confidence
 */

import { strict as assert } from 'node:assert';
import { e2eValidation } from './e2e-test.mjs';

/**
 * Extract all hashes from e2e result
 * @param {object} result - E2E validation result
 * @returns {object} Hash collection
 */
function extractHashes(result) {
  return {
    profileHash: result.profileHash,
    lensHash: result.lensHash,
    capsuleHash: result.capsuleHash,
    impactHash: result.impactHash,
    receiptHash: result.receiptHash,
    facadeHash: result.facadeHash,
    shadowHash: result.shadowHash
  };
}

/**
 * Determinism validation - Two runs
 * @returns {Promise<object>} Determinism result
 */
export async function determinismValidation() {
  // Run 1
  const run1 = await e2eValidation();
  assert(run1.allPassed, 'Run 1 failed');

  const hashes1 = extractHashes(run1);

  // Run 2 (clean state)
  const run2 = await e2eValidation();
  assert(run2.allPassed, 'Run 2 failed');

  const hashes2 = extractHashes(run2);

  // Strict equality check for all hashes
  const errors = [];

  if (hashes1.profileHash !== hashes2.profileHash) {
    errors.push(`Profile hash diverged: ${hashes1.profileHash} !== ${hashes2.profileHash}`);
  }

  if (hashes1.lensHash !== hashes2.lensHash) {
    errors.push(`Lens hash diverged: ${hashes1.lensHash} !== ${hashes2.lensHash}`);
  }

  if (hashes1.capsuleHash !== hashes2.capsuleHash) {
    errors.push(`Capsule hash diverged: ${hashes1.capsuleHash} !== ${hashes2.capsuleHash}`);
  }

  if (hashes1.impactHash !== hashes2.impactHash) {
    errors.push(`Impact hash diverged: ${hashes1.impactHash} !== ${hashes2.impactHash}`);
  }

  if (hashes1.receiptHash !== hashes2.receiptHash) {
    errors.push(`Receipt hash diverged: ${hashes1.receiptHash} !== ${hashes2.receiptHash}`);
  }

  if (hashes1.facadeHash !== hashes2.facadeHash) {
    errors.push(`Facade hash diverged: ${hashes1.facadeHash} !== ${hashes2.facadeHash}`);
  }

  if (hashes1.shadowHash !== hashes2.shadowHash) {
    errors.push(`Shadow hash diverged: ${hashes1.shadowHash} !== ${hashes2.shadowHash}`);
  }

  if (errors.length > 0) {
    throw new Error(`Determinism violation:\n${errors.join('\n')}`);
  }

  return {
    deterministic: true,
    runs: 2,
    hashes: hashes1
  };
}

/**
 * Statistical determinism validation - 10 runs
 * @returns {Promise<object>} Statistical result
 */
export async function statisticalDeterminism() {
  const runs = 10;
  const allHashes = [];

  for (let i = 0; i < runs; i++) {
    const result = await e2eValidation();
    assert(result.allPassed, `Run ${i + 1} failed`);
    allHashes.push(extractHashes(result));
  }

  // Verify all runs have identical hashes
  const baseline = allHashes[0];

  for (let i = 1; i < runs; i++) {
    assert.deepStrictEqual(
      allHashes[i],
      baseline,
      `Run ${i + 1} produced different hashes`
    );
  }

  return {
    deterministic: true,
    runs,
    confidence: '100%',
    baseline
  };
}

/**
 * Quick determinism check (2 runs only)
 * @returns {Promise<boolean>} True if deterministic
 */
export async function quickDeterminismCheck() {
  const result = await determinismValidation();
  return result.deterministic;
}
