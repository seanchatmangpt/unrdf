/**
 * Capsule IR - Public API
 * @module index
 */

// Capsule creation
export {
  createCapsule,
  CapsuleSchema,
  validateCapsule,
  serializeQuad,
  deserializeQuad,
} from './capsule.mjs';

// Planning
export { planCapsule, planCapsuleIdempotent } from './planner.mjs';

// Hashing
export { hashCapsule, hashWithParentChain } from './hash.mjs';

// Canonicalization
export { canonicalizeCapsule, canonicalizeQuads } from './canonicalize.mjs';

// Verification
export { verifyCapsule, detectTampering } from './verify.mjs';
