/**
 * @fileoverview Determinism enforcement utilities for AUTONOMIC_INNOVATION
 * Ensures all outputs are stable and reproducible across runs
 */

import { createHash } from 'node:crypto';

/**
 * Canonical sort for object keys (deterministic ordering)
 * @param {Object} obj - Object to sort
 * @returns {Object} Sorted object with alphabetical keys
 */
export function sortObjectKeys(obj) {
  if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
    return obj;
  }

  const sorted = {};
  Object.keys(obj)
    .sort()
    .forEach((key) => {
      sorted[key] = obj[key];
    });
  return sorted;
}

/**
 * Deterministic JSON serialization with canonical ordering
 * @param {*} value - Value to serialize
 * @returns {string} Canonical JSON string
 */
export function canonicalJSON(value) {
  return JSON.stringify(value, (key, val) => {
    if (typeof val === 'object' && val !== null && !Array.isArray(val)) {
      return sortObjectKeys(val);
    }
    return val;
  });
}

/**
 * Compute SHA256 hash of input with deterministic serialization
 * @param {*} input - Input to hash
 * @returns {string} 64-character hex SHA256 hash
 */
export function hashDeterministic(input) {
  const canonical = canonicalJSON(input);
  return createHash('sha256').update(canonical).digest('hex');
}

/**
 * Sort array of strings deterministically
 * @param {string[]} arr - Array to sort
 * @returns {string[]} Sorted array
 */
export function sortStrings(arr) {
  return [...arr].sort((a, b) => a.localeCompare(b));
}

/**
 * Sort array of objects by field, deterministically
 * @param {Object[]} arr - Array to sort
 * @param {string} field - Field to sort by
 * @returns {Object[]} Sorted array
 */
export function sortByField(arr, field) {
  return [...arr].sort((a, b) => {
    const aVal = String(a[field] || '');
    const bVal = String(b[field] || '');
    return aVal.localeCompare(bVal);
  });
}

/**
 * Enforce determinism: run function twice, verify outputs match
 * @param {Function} fn - Function to verify
 * @param {*} input - Input to function
 * @param {Object} options - Options
 * @param {number} [options.tolerance=0] - Byte tolerance (0 = exact match)
 * @returns {Promise<{deterministic: boolean, hash1: string, hash2: string, diff?: string}>}
 */
export async function verifyDeterminism(fn, input, options = {}) {
  const { tolerance = 0 } = options;

  const result1 = await fn(input);
  const result2 = await fn(input);

  const hash1 = hashDeterministic(result1);
  const hash2 = hashDeterministic(result2);

  const deterministic = hash1 === hash2;

  return {
    deterministic,
    hash1,
    hash2,
    diff: deterministic ? undefined : 'Hashes differ',
  };
}

/**
 * Normalize timestamps for determinism (exclude from hashing)
 * @param {Object} obj - Object with potential timestamp
 * @param {string} [field='timestamp'] - Field name to remove
 * @returns {Object} Object without timestamp
 */
export function removeTimestamp(obj, field = 'timestamp') {
  if (typeof obj !== 'object' || obj === null) {
    return obj;
  }

  const { [field]: _, ...rest } = obj;
  return rest;
}
