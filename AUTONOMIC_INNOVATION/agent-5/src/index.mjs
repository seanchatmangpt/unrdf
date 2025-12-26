/**
 * Commutativity Analysis - Public API
 * Agent 5: Diff as Program - Commutativity & Conflicts
 *
 * @module agent-5
 */

export { canReorder, conflictCertificate } from './commutativity.mjs';
export { findConflicts, minimizeConflict, classifyConflict } from './conflicts.mjs';
export { canonicalize, sortQuads, sha256 } from './canonicalization.mjs';

/**
 * @typedef {import('./types.mjs').Capsule} Capsule
 * @typedef {import('./types.mjs').ReorderResult} ReorderResult
 * @typedef {import('./types.mjs').ConflictCertificate} ConflictCertificate
 * @typedef {import('./types.mjs').Quad} Quad
 * @typedef {import('./types.mjs').ImpactSet} ImpactSet
 */
