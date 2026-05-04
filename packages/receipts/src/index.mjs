/**
 * KGC Receipts - Main Entry Point
 * Batch receipt generation with Merkle tree verification + Post-Quantum Cryptography
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

// Post-Quantum Cryptography
export {
  // Dilithium3
  generateDilithium3KeyPair,
  signDilithium3,
  verifyDilithium3,
  serializeDilithium3Signature,
  deserializeDilithium3Signature,
  getDilithium3SecurityLevel,
} from './dilithium3.mjs';

// Hybrid Signatures (Ed25519 + Dilithium3)
export {
  generateHybridKeyPair,
  signHybrid,
  verifyHybrid,
  serializeHybridSignature,
  deserializeHybridSignature,
  getHybridSecurityLevel,
} from './hybrid-signature.mjs';

// Post-Quantum Receipts
export {
  createPQReceipt,
  verifyPQReceipt,
  batchSignReceipts,
  getPQCapabilities,
} from './pq-signer.mjs';

// Post-Quantum Merkle Trees (XMSS)
export {
  buildPQMerkleTree,
  generatePQMerkleProof,
  verifyPQMerkleProof,
  getPQMerkleTreeInfo,
} from './pq-merkle.mjs';
