/**
 * Receipts System - Unified API
 *
 * Tamper-evident audit trails with:
 * - Hash-chained receipts
 * - Merkle tree batching
 * - Tamper detection
 * - External anchoring
 *
 * @module @unrdf/observability/receipts
 */

import { ReceiptChain as Chain } from './receipt-chain.mjs';
import { MerkleTree as Tree } from './merkle-tree.mjs';
import { TamperDetector as Detector } from './tamper-detection.mjs';
import { ReceiptAnchorer as Anchorer } from './anchor.mjs';

import {
  ReceiptSchema,
  MerkleProofSchema,
  AnchorSchema,
  VerificationResultSchema,
  ChainExportSchema,
} from './receipt-schema.mjs';

export const ReceiptChain = Chain;
export const MerkleTree = Tree;
export const TamperDetector = Detector;
export const ReceiptAnchorer = Anchorer;

export {
  ReceiptSchema,
  MerkleProofSchema,
  AnchorSchema,
  VerificationResultSchema,
  ChainExportSchema,
};

/**
 * Quick start: Create receipt chain with tamper detection
 *
 * @example
 * import { ReceiptChain, TamperDetector } from '@unrdf/observability/receipts';
 *
 * const chain = new ReceiptChain('audit-1');
 * await chain.append({
 *   operation: 'admit',
 *   payload: { delta: 'delta_001' },
 *   actor: 'system'
 * });
 *
 * const detector = new TamperDetector();
 * const result = await detector.verifyChain(chain.getAllReceipts());
 * console.log(result.valid); // true
 */

export default {
  ReceiptChain,
  MerkleTree,
  TamperDetector,
  ReceiptAnchorer,
};
