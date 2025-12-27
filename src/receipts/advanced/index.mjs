/**
 * @fileoverview Advanced Receipt Verification - Export all advanced features
 *
 * **Purpose**: Centralized exports for advanced receipt verification
 * - Zero-Knowledge Proofs
 * - Compact Merkle Proofs
 * - Timestamping (TSA + Blockchain)
 *
 * @module receipts/advanced
 */

// Zero-Knowledge Proofs
export {
  generateZKProof,
  verifyZKProof,
  proveReceiptMembership,
  verifyReceiptMembership,
  generateRangeProof,
  verifyRangeProof,
  generateAggregateProof,
  verifyAggregateProof,
} from './zk-proofs.mjs';

// Merkle Proofs
export {
  computeMerkleRoot,
  generateCompactProof,
  verifyCompactProof,
  generateMultiProof,
  verifyMultiProof,
  batchVerify,
  compressProof,
  decompressProof,
  serializeProof,
  deserializeProof,
  getProofSize,
  getProofStats,
  updateProof,
} from './merkle-proofs.mjs';

// Timestamping
export {
  generateTimestamp,
  verifyTimestamp,
  batchTimestamp,
  verifyBatchTimestamp,
  getTimestampAge,
  getTimestampAgeHuman,
} from './timestamp.mjs';
