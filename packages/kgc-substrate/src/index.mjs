/**
 * KGC Substrate - Core KnowledgeStore, Receipt Chain, and Tamper Detection
 *
 * Exports:
 * - KnowledgeStore: Deterministic, hash-stable, immutable append-only log
 * - ReceiptChain: Cryptographic receipt chain with merkle tree chaining
 * - TamperDetector: Tamper detection and verification for receipt chains
 * - Type validators and schemas
 */

export { KnowledgeStore } from './KnowledgeStore.mjs';
export { ReceiptChain } from './ReceiptChain.mjs';
export { TamperDetector } from './TamperDetector.mjs';
export {
  validateStorageSnapshot,
  validateQueryPattern,
  validateTripleEntry,
  validateStateCommitment,
  StorageSnapshotSchema,
  QueryPatternSchema,
  TripleEntrySchema,
  StateCommitmentSchema,
} from './types.mjs';
