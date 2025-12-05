/**
 * KGC 4D Client Patterns - Browser-Safe Exports Only
 *
 * This entry point ONLY exports client-safe patterns that don't depend on:
 * - Oxigraph (WASM)
 * - isomorphic-git (Node.js fs)
 * - hash-wasm (Node.js crypto)
 * - @unrdf/core (native modules)
 *
 * Use this in browser/client components instead of the main entry point.
 *
 * @example
 * // In Next.js client components:
 * import { createDeltaSyncReducer } from '@unrdf/kgc-4d/client';
 */

// Reusable Patterns for Client/Server Applications
export { HookRegistry } from './core/patterns/hook-registry.mjs';
export {
  createDeltaSyncReducer,
  DeltaSyncState,
  DeltaSyncActions,
} from './core/patterns/delta-sync-reducer.mjs';
export { SSEClient } from './core/patterns/sse-client.mjs';

// Time utilities (pure JS, no native dependencies)
export { now, toISO, fromISO, addNanoseconds, duration } from './time.mjs';

// Constants (pure JS)
export { GRAPHS, EVENT_TYPES, PREDICATES } from './constants.mjs';
