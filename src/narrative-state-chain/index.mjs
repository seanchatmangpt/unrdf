/**
 * @fileoverview Narrative State Chain - Complete implementation
 *
 * **Purpose**: Main entry point for narrative-state-chain system
 *
 * Exports all core modules:
 * - Types and schemas
 * - Store and persistence
 * - Reconciliation engine
 * - Guard enforcement
 * - Receipt generation
 * - Bridge system
 *
 * @module narrative-state-chain
 *
 * @example
 * import { UniverseStore, SceneStore, createAllowAllGuard } from './narrative-state-chain/index.mjs';
 *
 * // Create stores
 * const universeStore = new UniverseStore();
 * const sceneStore = new SceneStore(universeStore);
 *
 * // Create universe
 * const universe = await universeStore.create({
 *   schema: 'http://example.org/schema#',
 *   reconcile: async (state, obs) => ({ consequences: obs, artifacts: {}, errors: [] }),
 *   guards: [createAllowAllGuard()],
 *   metadata: { name: 'MyUniverse' }
 * });
 *
 * // Add scene
 * const scene = await sceneStore.add(
 *   universe.id,
 *   [{ type: 'observation' }],
 *   { property: 'value' },
 *   { agent: 'user@example.com' }
 * );
 */

// Types and schemas
export {
  UniverseMetadataSchema,
  InvariantSchema,
  GuardSchema,
  UniverseSchema,
  GuardResultSchema,
  ReceiptSchema,
  SceneSchema,
  BridgeSchema,
  validateUniverse,
  validateScene,
  validateReceipt,
  validateBridge,
} from './types.mjs';

// Store and persistence
export {
  UniverseStore,
  SceneStore,
} from './store.mjs';

// Reconciliation engine
export {
  reconcile,
  checkInvariants,
  checkMinimality,
  computeStateHash,
  mergeStates,
  validateReconciliationResult,
  createIdentityReconcile,
} from './reconcile.mjs';

// Guard enforcement
export {
  evaluateGuard,
  evaluateAllGuards,
  checkAdmissibility,
  createAllowAllGuard,
  createDenyAllGuard,
  createAgentWhitelistGuard,
  createRateLimitGuard,
  composeGuards,
} from './guards.mjs';

// Receipt generation
export {
  generateReceipt,
  computeMinimalityProof,
  hashReceipt,
  signReceipt,
  verifyReceipt,
  verifyReceiptChain,
  generateMockKeyPair,
  computeReceiptMerkleRoot,
  receiptToJSONLD,
} from './receipts.mjs';

// Bridge system
export {
  Bridge,
  crossUniverseCall,
  createBidirectionalBridge,
  createZodBridge,
  computeBridgeProof,
} from './bridges.mjs';
