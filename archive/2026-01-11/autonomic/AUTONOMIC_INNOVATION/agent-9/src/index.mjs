/**
 * Agent 9: Shadow Modes and Mismatch Reporting
 * @module @autonomic/agent-9
 *
 * Implement deterministic shadow modes to run legacy and new code in parallel,
 * detect mismatches, and report them with content-addressable hashing.
 *
 * @example
 * import { shadowWrite, partialServe } from '@autonomic/agent-9';
 *
 * // Shadow write with mismatch detection
 * const result = await shadowWrite(
 *   async (x) => legacyAdd(x),
 *   async (x) => facadeAdd(x),
 *   5,
 *   { onMismatch: (report) => console.error('Mismatch:', report.hash) }
 * );
 *
 * // Gradual rollout with partial serve
 * const response = await partialServe(
 *   { facadePercent: 10, strategy: 'hash-based' },
 *   legacyHandler,
 *   facadeHandler,
 *   request
 * );
 */

// Shadow modes
export { shadowWrite } from './shadow-write.mjs';
export { shadowRead } from './shadow-read.mjs';
export { partialServe } from './partial-serve.mjs';

// Reporting
export { createMismatchReport, hashMismatchReport } from './mismatch-report.mjs';
export { canonicalDiff, canonicalSerialize } from './canonical-diff.mjs';
