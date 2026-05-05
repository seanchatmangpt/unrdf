/**
 * @file Capsule Canonicalization and Deterministic Hashing
 * @description Provides content-addressed identity for Capsules using SHA-256
 * @module agent-2/canonicalization
 */

import { createHash } from 'crypto';

/**
 * Deterministic JSON replacer for consistent encoding
 * Sorts object keys alphabetically for canonical representation
 * @param {string} _key - Object key (unused)
 * @param {*} value - Value to encode
 * @returns {*} Canonicalized value
 */
function canonicalReplacer(_key, value) {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    // Sort object keys alphabetically
    return Object.keys(value)
      .sort()
      .reduce((sorted, k) => {
        sorted[k] = value[k];
        return sorted;
      }, {});
  }
  return value;
}

/**
 * Lexicographic comparator for intent operations
 * Sort order: op → target → value
 * @param {import('./schema.mjs').IntentOp} a - First intent operation
 * @param {import('./schema.mjs').IntentOp} b - Second intent operation
 * @returns {number} Comparison result (-1, 0, 1)
 */
function compareIntentOps(a, b) {
  if (a.op !== b.op) return a.op.localeCompare(b.op);
  if (a.target !== b.target) return a.target.localeCompare(b.target);

  const aVal = String(a.value ?? '');
  const bVal = String(b.value ?? '');
  return aVal.localeCompare(bVal);
}

/**
 * Lexicographic comparator for delta operations
 * Sort order: subject → predicate → object → op
 * @param {import('./schema.mjs').DeltaOp} a - First delta operation
 * @param {import('./schema.mjs').DeltaOp} b - Second delta operation
 * @returns {number} Comparison result (-1, 0, 1)
 */
function compareDeltaOps(a, b) {
  if (a.subject !== b.subject) return a.subject.localeCompare(b.subject);
  if (a.predicate !== b.predicate) return a.predicate.localeCompare(b.predicate);
  if (a.object !== b.object) return a.object.localeCompare(b.object);
  return a.op.localeCompare(b.op);
}

/**
 * Canonicalize capsule to deterministic string representation
 * Same capsule content → always same string (idempotent, deterministic)
 * @param {import('./schema.mjs').Capsule} capsule - Capsule to canonicalize
 * @returns {string} Canonical JSON string
 */
export function canonicalizeCapsule(capsule) {
  // Create deep copy to avoid mutation
  const canonical = {
    intent: [...capsule.intent].sort(compareIntentOps),
    delta: [...capsule.delta].sort(compareDeltaOps),
    guard: {
      limits: capsule.guard?.limits || {},
      profiles: [...(capsule.guard?.profiles || [])].sort(),
      invariants: [...(capsule.guard?.invariants || [])].sort(),
    },
  };

  // Include receipt if present (for verification)
  if (capsule.receipt) {
    canonical.receipt = {
      hash: capsule.receipt.hash,
      parents: [...capsule.receipt.parents].sort(), // Sort parents deterministically
      timestamp: capsule.receipt.timestamp,
      ...(capsule.receipt.signer && { signer: capsule.receipt.signer }),
    };
  }

  // Use canonical replacer for consistent key ordering
  return JSON.stringify(canonical, canonicalReplacer);
}

/**
 * Compute SHA-256 hash of capsule (content-addressed identity)
 * Deterministic: same capsule → always same hash
 * @param {import('./schema.mjs').Capsule} capsule - Capsule to hash
 * @returns {string} 64-character hex SHA-256 hash
 */
export function hashCapsule(capsule) {
  const canonical = canonicalizeCapsule(capsule);
  const hash = createHash('sha256');
  hash.update(canonical, 'utf8');
  return hash.digest('hex');
}

/**
 * Compute receipt hash with parent chain (Merkle DAG)
 * Creates tamper-evident chain: hash depends on all parent hashes
 * @param {Partial<import('./schema.mjs').Receipt>} receipt - Receipt data (without hash field)
 * @param {string[]} [parents=[]] - Parent capsule hashes
 * @returns {string} 64-character hex SHA-256 hash
 */
export function hashReceipt(receipt, parents = []) {
  // Sort parents deterministically for canonical hash
  const sortedParents = [...parents].sort();

  // Create canonical receipt representation
  const canonical = {
    parents: sortedParents,
    timestamp: receipt.timestamp,
    ...(receipt.signer && { signer: receipt.signer }),
  };

  const canonicalStr = JSON.stringify(canonical, canonicalReplacer);

  // Chain hash: hash(receipt data + all parent hashes)
  const hash = createHash('sha256');
  hash.update(canonicalStr, 'utf8');

  // Include parent hashes in chain
  for (const parentHash of sortedParents) {
    hash.update(parentHash, 'utf8');
  }

  return hash.digest('hex');
}
