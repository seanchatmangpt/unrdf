/**
 * Hashing - BLAKE3 content-addressing for capsules
 * @module hash
 */

import { blake3 } from '../../../node_modules/.pnpm/hash-wasm@4.12.0/node_modules/hash-wasm/dist/index.esm.js';
import { canonicalizeCapsule } from './canonicalize.mjs';

/**
 * Compute BLAKE3 hash of capsule
 * @param {Object} capsule - Capsule to hash
 * @returns {Promise<string>} BLAKE3 hex digest
 */
export async function hashCapsule(capsule) {
  // 1. Create copy without ID and hash fields to avoid circular dependency
  const capsuleForHashing = {
    version: capsule.version,
    intent: capsule.intent,
    delta: capsule.delta,
    guard: capsule.guard,
    receipt: {
      parents: capsule.receipt.parents,
      timestamp: capsule.receipt.timestamp,
      ...(capsule.receipt.signer && { signer: capsule.receipt.signer }),
    },
  };

  // 2. Canonicalize capsule (deterministic serialization)
  const canonical = canonicalizeCapsule(capsuleForHashing);

  // 3. Compute BLAKE3 hash (via hash-wasm)
  const hash = await blake3(canonical);

  return hash;
}

/**
 * Compute hash with parent chain verification
 * Ensures parent hashes exist and are valid
 *
 * @param {Object} capsule - Capsule to hash
 * @param {Map<string, Object>} parentCapsules - Map of parent hash → capsule
 * @returns {Promise<{ hash: string, parentChain: Array<string> }>}
 */
export async function hashWithParentChain(
  capsule,
  parentCapsules = new Map()
) {
  // 1. Verify all parent hashes exist
  for (const parentHash of capsule.receipt.parents) {
    if (!parentCapsules.has(parentHash)) {
      throw new Error(`Parent capsule not found: ${parentHash}`);
    }

    // 2. Verify parent hash is correct
    const parentCapsule = parentCapsules.get(parentHash);
    const recomputedHash = await hashCapsule(parentCapsule);
    if (recomputedHash !== parentHash) {
      throw new Error(
        `Parent hash mismatch: expected ${parentHash}, got ${recomputedHash}`
      );
    }
  }

  // 3. Compute capsule hash
  const hash = await hashCapsule(capsule);

  // 4. Build full parent chain (recursive)
  const parentChain = [];
  for (const parentHash of capsule.receipt.parents) {
    parentChain.push(parentHash);
    const parent = parentCapsules.get(parentHash);
    if (parent.receipt.parents.length > 0) {
      const { parentChain: grandparents } = await hashWithParentChain(
        parent,
        parentCapsules
      );
      parentChain.push(...grandparents);
    }
  }

  return { hash, parentChain };
}
