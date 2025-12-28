/**
 * KGC Receipts - Main Entry Point
 * Batch receipt generation with Merkle tree verification
 *
 * @module @unrdf/receipts
 */

// Batch Receipt Generator
export {
  generateBatchReceipt,
  verifyBatchReceipt,
  serializeReceipt,
  deserializeReceipt,
  batchMultipleOperations,
} from './batch-receipt-generator.mjs';

// Merkle Tree Batcher
export {
  buildMerkleTree,
  generateMerkleProof,
  verifyMerkleProof,
  batchWithMerkleTree,
  verifyOperationInBatch,
  getMerkleRoot,
  calculateTreeDepth,
  getLeafCount,
} from './merkle-batcher.mjs';
