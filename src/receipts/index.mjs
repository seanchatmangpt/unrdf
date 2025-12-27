/**
 * @fileoverview Receipt and Provenance System - Main exports
 *
 * **Purpose**: Universal receipt standard for UNRDF ecosystem
 *
 * This module provides:
 * - Universal receipt schema with type-specific extensions
 * - Receipt builders for all operation types
 * - Append-only ledger for chain storage
 * - Query indexer for fast retrieval
 * - Serialization (JSON-LD, TTL, binary)
 *
 * @module receipts
 */

// Legacy exports (backward compatibility)
export { Receipt } from './receipt.mjs';
export { ReceiptChain } from './receipt-chain.mjs';
export { ReceiptGenerator } from './receipt-generator.mjs';
export {
  computeMerkleRoot,
  generateMerkleProof,
  verifyMerkleProof,
  batchReceipts,
} from './merkle-root.mjs';

// Universal Receipt Standard
export {
  // Constants
  RECEIPT_TYPES,
  DECISION_OUTCOMES,
  SEVERITY_LEVELS,

  // Schema
  UniversalReceiptSchema,

  // Utilities
  generateEpoch,
  generateReceiptId,
  computeReceiptHash,

  // Builder
  ReceiptBuilder,

  // Factories
  createAdmissionReceipt,
  createTestReceipt,
  createBuildReceipt,
  createDeploymentReceipt,
  createProjectionReceipt,
  createQueryReceipt,
  createWorkflowReceipt,

  // Serialization
  receiptToJSONLD,
  receiptToTurtle,
  receiptToBinary,
  receiptFromBinary,

  // Verification
  verifyReceiptHash,
  validateReceipt,
} from './receipt-standard.mjs';

// Receipt Ledger
export {
  // Storage backends
  MemoryStorage,
  FileStorage,

  // Ledger
  ReceiptLedger,

  // Factory functions
  createMemoryLedger,
  createFileLedger,

  // Snapshots
  createChainSnapshot,
  verifyChainSnapshot,
} from './receipt-ledger.mjs';

// Receipt Indexer
export {
  ReceiptQuery,
  ReceiptIndexer,
  createIndexer,
} from './receipt-indexer.mjs';
