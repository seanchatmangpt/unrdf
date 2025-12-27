/**
 * @file Agent 2: Capsule IR Primitive - Public API
 * @description Portable, content-addressed change programs with deterministic hashing
 * @module agent-2
 */

// Core operations
export {
  planCapsule,
  compileCapsuleToDeltas,
  verifyCapsule,
  applyCapsule,
} from './capsule.mjs';

// Canonicalization and hashing
export {
  canonicalizeCapsule,
  hashCapsule,
  hashReceipt,
} from './canonicalization.mjs';

// Schemas
export {
  CapsuleSchema,
  IntentOpSchema,
  DeltaOpSchema,
  GuardSchema,
  ReceiptSchema,
  PartialCapsuleSchema,
} from './schema.mjs';
