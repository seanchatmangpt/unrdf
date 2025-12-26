/**
 * Mismatch Report - Content-addressable report generation
 * @module @autonomic/agent-9/mismatch-report
 */

import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';
import { canonicalSerialize, canonicalReplacer } from './canonical-diff.mjs';

/**
 * @typedef {Object} MismatchReport
 * @property {bigint} timestamp - Nanosecond timestamp (NOT part of hash)
 * @property {string} type - Operation type ('write' | 'read')
 * @property {any} input - Canonical input
 * @property {any} legacyOutput - Canonical legacy output
 * @property {any} facadeOutput - Canonical facade output
 * @property {string[]} differencePaths - Array of difference descriptions
 * @property {string} hash - Content hash (excludes timestamp)
 */

/**
 * Create deterministic mismatch report
 * Hash = f(input, legacyOutput, facadeOutput, differencePaths), NOT f(timestamp)
 *
 * @param {Object} params - Report parameters
 * @param {string} params.type - 'write' | 'read'
 * @param {any} params.input - Operation input
 * @param {any} params.legacyOutput - Legacy result
 * @param {any} params.facadeOutput - Facade result
 * @param {Object} params.difference - Canonical diff object with paths
 * @returns {Promise<MismatchReport>}
 *
 * @example
 * const report = await createMismatchReport({
 *   type: 'write',
 *   input: { x: 5 },
 *   legacyOutput: 6,
 *   facadeOutput: 7,
 *   difference: { hasDifference: true, paths: ['root: 6 vs 7'] }
 * });
 * console.log(report.hash); // Deterministic hash
 */
export async function createMismatchReport({
  type,
  input,
  legacyOutput,
  facadeOutput,
  difference,
}) {
  const timestamp = now(); // Nanosecond precision

  // Canonical serialization (deterministic ordering)
  const canonical = {
    type,
    input: canonicalSerialize(input),
    legacyOutput: canonicalSerialize(legacyOutput),
    facadeOutput: canonicalSerialize(facadeOutput),
    differencePaths: difference.paths.slice().sort(), // Lexicographic sort
  };

  // Hash ONLY the canonical content (NOT timestamp)
  const hash = await hashMismatchReport(canonical);

  return {
    timestamp, // For logging/debugging, not part of hash
    ...canonical,
    hash,
  };
}

/**
 * Hash mismatch report content (deterministic)
 * Uses BLAKE3 for fast, cryptographically secure hashing
 *
 * @param {Object} canonical - Canonical report object (no timestamp)
 * @returns {Promise<string>} - Hex hash (64 characters)
 *
 * @example
 * const hash = await hashMismatchReport({
 *   type: 'write',
 *   input: { x: 5 },
 *   legacyOutput: 6,
 *   facadeOutput: 7,
 *   differencePaths: ['root: 6 vs 7']
 * });
 * console.log(hash.length); // 64 (BLAKE3 output)
 */
export async function hashMismatchReport(canonical) {
  // Deterministic JSON serialization
  const json = JSON.stringify(canonical, canonicalReplacer);

  // Content-addressed hash
  return await blake3(json);
}
