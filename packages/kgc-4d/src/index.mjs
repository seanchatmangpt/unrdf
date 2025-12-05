/**
 * KGC 4D Engine - Public API
 * Nanosecond-precision event logging with Git-backed snapshots
 */

export { KGCStore } from './store.mjs';
export { GitBackbone } from './git.mjs';
export { freezeUniverse, reconstructState, verifyReceipt } from './freeze.mjs';
export { now, toISO, fromISO, addNanoseconds, duration, VectorClock } from './time.mjs';
export { GRAPHS, EVENT_TYPES, PREDICATES } from './constants.mjs';
