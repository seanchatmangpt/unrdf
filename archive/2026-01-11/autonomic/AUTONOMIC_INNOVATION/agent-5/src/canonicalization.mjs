/**
 * Deterministic Canonicalization and Hashing
 * @module agent-5/canonicalization
 */

import { createHash } from 'node:crypto';

/**
 * Convert quad to canonical N-Triples string
 * @param {Object} quad - RDF quad
 * @returns {string} - Canonical N-Triples representation
 */
export function quadToNTriples(quad) {
  const s = quad.subject.value;
  const p = quad.predicate.value;
  const o = quad.object.value;
  const g = quad.graph?.value || 'http://kgc.io/Universe';

  return `<${s}> <${p}> "${o}" <${g}> .`;
}

/**
 * Sort quads deterministically by N-Triples representation
 * @param {Array<Object>} quads - Array of quads
 * @returns {Array<Object>} - Sorted array (new array, original unchanged)
 */
export function sortQuads(quads) {
  return [...quads].sort((a, b) => {
    const aNT = quadToNTriples(a);
    const bNT = quadToNTriples(b);
    return aNT.localeCompare(bNT);
  });
}

/**
 * Canonicalize object for deterministic hashing
 * Ensures same input always produces same output
 * @param {Object} obj - Object to canonicalize
 * @returns {string} - Canonical JSON string
 */
export function canonicalize(obj) {
  // Convert Sets to sorted arrays
  const normalized = JSON.parse(JSON.stringify(obj, (key, value) => {
    if (value instanceof Set) {
      return Array.from(value).sort();
    }
    return value;
  }));

  // Sort object keys recursively
  const sortKeys = (item) => {
    if (Array.isArray(item)) {
      return item.map(sortKeys);
    }
    if (item !== null && typeof item === 'object') {
      return Object.keys(item)
        .sort()
        .reduce((result, key) => {
          result[key] = sortKeys(item[key]);
          return result;
        }, {});
    }
    return item;
  };

  const sorted = sortKeys(normalized);
  return JSON.stringify(sorted);
}

/**
 * Compute SHA-256 hash of canonicalized object
 * @param {string} canonicalString - Output from canonicalize()
 * @returns {string} - SHA-256 hash in hex format
 */
export function sha256(canonicalString) {
  return createHash('sha256').update(canonicalString).digest('hex');
}

/**
 * Generate deterministic hash for conflict certificate
 * @param {Object} certificate - Certificate object to hash
 * @returns {string} - SHA-256 hash
 */
export function hashCertificate(certificate) {
  const canonical = canonicalize(certificate);
  return sha256(canonical);
}
