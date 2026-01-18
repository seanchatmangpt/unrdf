/**
 * Agent 8: Store Adapter and Atomic Apply
 * Public API exports for atomic RDF capsule application
 */

// Store adapter exports
export { createStoreAdapter, transaction, replayFromReceipt } from './store-adapter.mjs';

// Atomic operations exports
export { atomicApply, verifyAtomicity } from './atomic.mjs';

// Integration layer exports
export { integrateWithKGC4D, integrateWithOxigraph } from './integration.mjs';
