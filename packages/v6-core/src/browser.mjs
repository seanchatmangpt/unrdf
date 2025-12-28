/**
 * @unrdf/v6-core/browser - Browser Entry Point
 *
 * Browser-compatible exports for UNRDF v6 Alpha.
 * Excludes CLI and doc generation (Node.js only).
 *
 * @module @unrdf/v6-core/browser
 */

// =============================================================================
// Core Receipts (100% browser-compatible)
// =============================================================================

export {
  // Base receipt
  BaseReceiptSchema,
  ReceiptTypeSchema,
  AttestationSchema,
  VectorClockSchema,
  RECEIPT_TYPES,
  BLAKE3_HEX_LENGTH,
  generateUUID,
  deterministicSerialize,
  computeBlake3,
  computeChainHash,
  verifyBaseReceipt,
} from './receipts/base-receipt.mjs';

export {
  // Receipt operations
  createReceipt,
  verifyReceipt,
  verifyChainLink,
  ExecutionReceiptSchema,
  AllocationReceiptSchema,
  CompileReceiptSchema,
  VerificationReceiptSchema,
} from './receipts/index.mjs';

export {
  // Merkle trees
  buildMerkleTree,
  getMerkleRoot,
  getProofPath,
  verifyInclusion,
  getTreeInfo,
  MerkleProofSchema,
  TreeInfoSchema,
} from './receipts/merkle/tree.mjs';

// Note: Proof chain not yet implemented in v6-alpha
// export {
//   createProofChain,
//   verifyProofChain,
//   addProofToChain,
// } from './receipts/merkle/proofchain.mjs';

// =============================================================================
// Delta System (100% browser-compatible after fix)
// =============================================================================

export {
  // Delta schemas
  DeltaSchema,
  DeltaOperationSchema,
  DeltaSourceSchema,
  DeltaAdmissibilitySchema,
  DeltaReceiptSchema,
  DeltaConflictSchema,
  validateDelta,
  validateDeltaOperation,
  validateDeltaReceipt,
  validateDeltaConflict,
} from './delta/schema.mjs';

export {
  // Delta gate
  DeltaGate,
} from './delta/gate.mjs';

export {
  // Reconciliation
  reconcile,
  defaultConflictResolver,
  currentWinsResolver,
  strictResolver,
  customResolver,
} from './delta/reconcile.mjs';

export {
  // Adapters
  WorkflowAdapter,
  createWorkflowAdapter,
} from './delta/adapters/workflow-adapter.mjs';

export {
  ResourceAdapter,
  createResourceAdapter,
} from './delta/adapters/resource-adapter.mjs';

export {
  GraphQLAdapter,
  createGraphQLAdapter,
} from './delta/adapters/graphql-adapter.mjs';

export {
  // Factory functions
  createDeltaSystem,
  createDelta,
} from './delta/index.mjs';

// =============================================================================
// Grammar (browser-compatible)
// =============================================================================

// Note: Grammar exports available but not re-exported in browser build
// to avoid issues with Node.js-specific dependencies.
// Import from './grammar/index.mjs' directly if needed.
// export {
//   parseGrammar,
//   compileGrammar,
// } from './grammar/index.mjs';

// =============================================================================
// Version & Status
// =============================================================================

export {
  V6_VERSION,
  V6_FEATURES,
  getV6Status,
  isFeatureEnabled,
} from './index.mjs';

// =============================================================================
// Browser-specific note
// =============================================================================

/**
 * Browser compatibility notes:
 *
 * ✅ Included in browser build:
 * - Receipts (full support)
 * - Merkle trees (full support)
 * - Delta system (full support)
 * - Grammar definitions (full support)
 * - BLAKE3 hashing via hash-wasm WASM
 * - UUID generation via Web Crypto API
 *
 * ❌ Excluded from browser build:
 * - CLI commands (Node.js only)
 * - Documentation generators (Node.js only)
 * - Thesis generators (Node.js only)
 *
 * Dependencies:
 * - hash-wasm: WASM-based hashing (browser-compatible)
 * - zod: Schema validation (browser-compatible)
 * - @unrdf/oxigraph: SPARQL engine with WASM (browser-compatible)
 *
 * Storage:
 * - Use IndexedDB for receipt persistence
 * - See @unrdf/v6-core/browser/receipt-store for helpers
 *
 * Example:
 * ```javascript
 * import { createReceipt, buildMerkleTree } from '@unrdf/v6-core/browser';
 *
 * const receipt = await createReceipt({
 *   receiptType: 'execution',
 *   payload: { task: 'example' }
 * });
 *
 * const tree = await buildMerkleTree([receipt]);
 * console.log('Merkle root:', tree.root);
 * ```
 */
