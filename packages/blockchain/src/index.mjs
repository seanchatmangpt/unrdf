/**
 * UNRDF Blockchain Integration - Main Entry Point
 *
 * Provides blockchain-backed cryptographic receipts and audit trails.
 * Supports Ethereum anchoring, smart contract interaction, and Merkle proofs.
 *
 * @module @unrdf/blockchain
 */

// Anchoring
export { ReceiptAnchorer, AnchorResultSchema, VerificationResultSchema } from './anchoring/receipt-anchorer.mjs';

// Contracts
export { WorkflowVerifier, ContractStatsSchema, estimateGasCosts } from './contracts/workflow-verifier.mjs';

// Merkle
export {
  MerkleProofGenerator,
  MerkleProofSchema,
  MerkleTreeInfoSchema,
  calculateGasSavings,
} from './merkle/merkle-proof-generator.mjs';

// Re-export for convenience
import { ReceiptAnchorer } from './anchoring/receipt-anchorer.mjs';
import { WorkflowVerifier } from './contracts/workflow-verifier.mjs';
import { MerkleProofGenerator } from './merkle/merkle-proof-generator.mjs';

export default {
  ReceiptAnchorer,
  WorkflowVerifier,
  MerkleProofGenerator,
};
