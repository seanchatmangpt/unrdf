/**
 * @fileoverview Proof Kernel - Capsules, Receipts, and Hash Chains.
 *
 * Provides tamper-evident proof system for agent operations:
 * - Capsules: Encapsulate changes with metadata
 * - Receipts: Form hash chain linking capsules
 * - Tamper Detection: Verify chain integrity
 *
 * @example
 * import {
 *   createCapsule,
 *   generateReceipt,
 *   attachReceipt,
 *   auditChain,
 * } from './agent-5/index.mjs';
 *
 * // Create capsule
 * const capsule = createCapsule(
 *   { action: 'migrate', files: ['foo.mjs'] },
 *   { agentId: 'agent-3', phase: 'migrate' }
 * );
 *
 * // Generate receipt
 * const receipt = generateReceipt(capsule);
 * const capsuleWithReceipt = attachReceipt(capsule, receipt);
 *
 * // Verify chain
 * const audit = auditChain([capsuleWithReceipt]);
 * console.log(audit.valid); // true
 */

// Hash utilities
export {
  sha256,
  sha256Hex,
  sha256Prefixed,
  hashChain,
  isValidSha256Hex,
  isValidSha256Prefixed,
} from './hash.mjs';

// Canonicalization
export {
  canonicalize,
  serializeCanonical,
  deserializeCanonical,
  areCanonicallyEqual,
  sortKeys,
} from './canonicalize.mjs';

// Capsule operations
export {
  createCapsule,
  validateCapsule,
  serializeCapsule,
  deserializeCapsule,
  extractCapsuleContent,
  cloneCapsule,
  attachReceipt,
  getCapsuleSummary,
} from './capsule.mjs';

// Receipt operations
export {
  generateReceipt,
  verifyReceipt,
  verifyChain,
  getReceiptHash,
  createReceiptChain,
  getReceiptSummary,
} from './receipt.mjs';

// Tamper detection
export {
  detectTamper,
  auditChain,
  findFirstTamper,
  verifyChainContinuity,
  generateChainSummary,
} from './tamper-detection.mjs';
