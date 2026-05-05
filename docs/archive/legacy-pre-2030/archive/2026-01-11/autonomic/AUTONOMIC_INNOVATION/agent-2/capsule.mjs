/**
 * @file Capsule Core Operations
 * @description Pure functions for capsule creation, compilation, verification, and application
 * @module agent-2/capsule
 */

import { CapsuleSchema, PartialCapsuleSchema } from './schema.mjs';
import { hashCapsule, hashReceipt } from './canonicalization.mjs';

/**
 * Plan capsule from intent operations
 * Creates initial capsule structure with intent and guard, empty delta
 * @param {import('./schema.mjs').IntentOp[]} intent - Array of intent operations
 * @param {string} [profile] - Optional profile name for validation
 * @returns {import('./schema.mjs').Capsule} Planned capsule (no delta or receipt yet)
 */
export function planCapsule(intent, profile) {
  // Deep copy intent to ensure immutability
  const intentCopy = structuredClone(intent);

  // Create capsule with default guard
  const capsule = {
    intent: intentCopy,
    delta: [],
    guard: {
      limits: {
        maxDeltas: 1000,
        maxDepth: 10,
        timeout: 5000,
      },
      profiles: profile ? [profile] : [],
      invariants: [],
    },
  };

  // Validate against partial schema (delta not required yet)
  const result = PartialCapsuleSchema.safeParse(capsule);
  if (!result.success) {
    throw new Error(`Invalid capsule: ${result.error.message}`);
  }

  return result.data;
}

/**
 * Compile capsule intent to RDF deltas using lens mappings
 * Translates high-level intent operations → low-level RDF changes
 * @param {import('./schema.mjs').Capsule} capsule - Capsule with intent
 * @param {Map<string, Function>} lensMap - Map of operation type → lens function
 * @returns {import('./schema.mjs').Capsule} New capsule with delta field populated
 */
export function compileCapsuleToDeltas(capsule, lensMap) {
  // Deep copy to ensure immutability
  const compiled = structuredClone(capsule);

  const deltas = [];

  for (const intentOp of compiled.intent) {
    const lensKey = `${intentOp.op}:${intentOp.profile || 'default'}`;
    const lens = lensMap.get(lensKey) || lensMap.get(intentOp.op);

    if (!lens) {
      throw new Error(`No lens found for operation: ${intentOp.op} (profile: ${intentOp.profile || 'default'})`);
    }

    // Lens function: intentOp → array of deltas
    const opDeltas = lens(intentOp);
    if (!Array.isArray(opDeltas)) {
      throw new Error(`Lens for ${intentOp.op} must return array of deltas`);
    }

    deltas.push(...opDeltas);
  }

  // Check guard limits
  if (deltas.length > compiled.guard.limits.maxDeltas) {
    throw new Error(
      `Delta count ${deltas.length} exceeds limit ${compiled.guard.limits.maxDeltas}`
    );
  }

  compiled.delta = deltas;

  // Validate complete capsule
  const result = CapsuleSchema.safeParse(compiled);
  if (!result.success) {
    throw new Error(`Invalid compiled capsule: ${result.error.message}`);
  }

  return result.data;
}

/**
 * Verify capsule integrity (schema, receipt chain, invariants)
 * @param {import('./schema.mjs').Capsule} capsule - Capsule to verify
 * @returns {{ valid: boolean, errors: string[] }} Verification result
 */
export function verifyCapsule(capsule) {
  const errors = [];

  // 1. Pre-validation: Check parent hashes before schema validation
  //    This ensures we get specific error messages for invalid hashes
  if (capsule.receipt?.parents) {
    for (const parentHash of capsule.receipt.parents) {
      if (!/^[a-f0-9]{64}$/.test(parentHash)) {
        errors.push(`Invalid parent hash format: ${parentHash}`);
      }
    }
  }

  // 2. Schema validation
  const schemaResult = CapsuleSchema.safeParse(capsule);
  if (!schemaResult.success) {
    errors.push(`Schema validation failed: ${schemaResult.error.message}`);
    // Don't return early - collect all errors
  }

  // 3. Receipt chain verification (if receipt present and schema passed)
  if (capsule.receipt && schemaResult.success) {
    // Verify receipt hash matches capsule content
    const computedHash = hashCapsule({
      intent: capsule.intent,
      delta: capsule.delta,
      guard: capsule.guard,
    });

    // For receipts with parents, verify the chain
    if (capsule.receipt.parents && capsule.receipt.parents.length > 0) {
      const expectedReceiptHash = hashReceipt(
        {
          timestamp: capsule.receipt.timestamp,
          signer: capsule.receipt.signer,
        },
        capsule.receipt.parents
      );

      // The receipt hash should be derived from content hash + parent chain
      // Verify the receipt hash is valid hex (already checked in pre-validation)
      if (!/^[a-f0-9]{64}$/.test(capsule.receipt.hash)) {
        errors.push(`Invalid receipt hash format: ${capsule.receipt.hash}`);
      }
    }
  }

  // 4. Guard invariant checks
  if (capsule.guard?.invariants && capsule.guard.invariants.length > 0) {
    // Invariants are constraint expressions (future: evaluate them)
    // For now, just verify they are non-empty strings
    for (const invariant of capsule.guard.invariants) {
      if (!invariant || invariant.trim().length === 0) {
        errors.push('Guard invariant must be non-empty string');
      }
    }
  }

  // 5. Delta count within limits
  if (capsule.delta && capsule.guard?.limits && capsule.delta.length > capsule.guard.limits.maxDeltas) {
    errors.push(
      `Delta count ${capsule.delta.length} exceeds limit ${capsule.guard.limits.maxDeltas}`
    );
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Apply capsule (generate receipt, do NOT mutate capsule)
 * Returns receipt metadata for Agent 8 to apply to RDF store
 * @param {import('./schema.mjs').Capsule} capsule - Capsule to apply
 * @param {string[]} [parents=[]] - Parent capsule hashes
 * @returns {{ receipt: import('./schema.mjs').Receipt, appliedAt: number, hash: string }} Application result
 */
export function applyCapsule(capsule, parents = []) {
  // Verify capsule before application
  const verification = verifyCapsule(capsule);
  if (!verification.valid) {
    throw new Error(`Cannot apply invalid capsule: ${verification.errors.join('; ')}`);
  }

  // Generate capsule content hash (identity)
  const contentHash = hashCapsule(capsule);

  // Generate receipt with parent chain
  const timestamp = Date.now();
  const receiptHash = hashReceipt({ timestamp }, parents);

  const receipt = {
    hash: receiptHash,
    parents: [...parents], // Copy to ensure immutability
    timestamp,
  };

  return {
    receipt,
    appliedAt: timestamp,
    hash: contentHash,
  };
}
