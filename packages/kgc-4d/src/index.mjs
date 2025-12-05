/**
 * KGC 4D Engine - Public API
 * Nanosecond-precision event logging with Git-backed snapshots
 */

// Core 4D Engine
export { KGCStore } from './store.mjs';
export { GitBackbone } from './git.mjs';
export { freezeUniverse, reconstructState, verifyReceipt } from './freeze.mjs';
export { now, toISO, fromISO, addNanoseconds, duration, VectorClock } from './time.mjs';
export { GRAPHS, EVENT_TYPES, PREDICATES } from './constants.mjs';

// Reusable Patterns for Client/Server Applications
export { HookRegistry } from './core/patterns/hook-registry.mjs';
export {
  createDeltaSyncReducer,
  DeltaSyncState,
  DeltaSyncActions,
} from './core/patterns/delta-sync-reducer.mjs';
export { SSEClient } from './core/patterns/sse-client.mjs';
