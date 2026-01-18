/**
 * Agent 8: Store Adapter & Atomic Apply
 *
 * Main entry point for atomic RDF store operations with capsule delta application.
 *
 * @module agent-8
 *
 * @example
 * import { createAtomicStore, applyCapsule } from '@agent-8';
 *
 * const store = createAtomicStore({ nodeId: 'node-1' });
 * const capsule = {
 *   delta: {
 *     add: [quad1, quad2],
 *     del: []
 *   }
 * };
 *
 * const receipt = await applyCapsule(store, capsule);
 * console.log(receipt.hash); // BLAKE3 hash
 * console.log(receipt.stats.added); // 2
 */

// Store exports
export { AtomicStore, createAtomicStore, UNIVERSE_GRAPH } from './store.mjs';

// Apply exports
export {
  applyCapsule,
  applyBatch,
  validateCapsule,
  CapsuleSchema
} from './apply.mjs';

// Query exports
export {
  queryStore,
  queryUniverse,
  countQuads,
  askQuery,
  selectQuery,
  constructQuery
} from './query.mjs';

// Freeze exports (optional)
export {
  snapshotStore,
  restoreSnapshot,
  snapshotsEnabled,
  listSnapshots
} from './freeze.mjs';

// Utilities exports
export {
  hashReceipt,
  verifyReceipt,
  serializeDelta,
  deserializeDelta,
  generateReceipt,
  quadToNQuad,
  ReceiptSchema
} from './utils.mjs';
