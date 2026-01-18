/**
 * @fileoverview Lens utility functions for normalization and comparison
 * @module lens-utils
 */

import { createHash } from 'node:crypto';

/**
 * Normalizes a path to reduce churn and ensure consistency
 * - Removes leading/trailing slashes
 * - Converts multiple slashes to single
 * - Converts to lowercase
 * - Removes query parameters and fragments
 *
 * @param {string} path - Path to normalize
 * @returns {string} Normalized path
 *
 * @example
 * normalizePath('/api/users/') // => 'api/users'
 * normalizePath('//API//Users') // => 'api/users'
 */
export function normalizePath(path) {
  if (typeof path !== 'string') {
    throw new TypeError(`Expected string, got ${typeof path}`);
  }

  return path
    .toLowerCase()
    .replace(/^\/+|\/+$/g, '') // Remove leading/trailing slashes
    .replace(/\/+/g, '/') // Convert multiple slashes to single
    .split('?')[0] // Remove query parameters
    .split('#')[0]; // Remove fragments
}

/**
 * Normalizes an ID to ensure consistency
 * - Trims whitespace
 * - Converts to lowercase if string
 * - Ensures consistent type (string)
 *
 * @param {string|number} id - ID to normalize
 * @returns {string} Normalized ID
 *
 * @example
 * normalizeId('  USER-123  ') // => 'user-123'
 * normalizeId(123) // => '123'
 */
export function normalizeId(id) {
  if (id === null || id === undefined) {
    throw new TypeError('ID cannot be null or undefined');
  }

  const stringId = String(id).trim();

  if (stringId === '') {
    throw new Error('ID cannot be empty');
  }

  // Convert to lowercase for consistent comparison
  return stringId.toLowerCase();
}

/**
 * Performs deep equality check between two values
 * - Handles primitives, objects, arrays, null, undefined
 * - Detects circular references
 * - Order-independent for object keys
 *
 * @param {*} a - First value
 * @param {*} b - Second value
 * @param {Set<*>} [_seen=new Set()] - Internal: tracks seen objects for circular reference detection
 * @returns {boolean} True if deeply equal
 *
 * @example
 * deepEqual({ a: 1, b: 2 }, { b: 2, a: 1 }) // => true
 * deepEqual([1, [2, 3]], [1, [2, 3]]) // => true
 */
export function deepEqual(a, b, _seen = new Set()) {
  // Same reference or primitive equality
  if (a === b) return true;

  // Type check
  if (typeof a !== typeof b) return false;

  // Null check (typeof null === 'object')
  if (a === null || b === null) return false;

  // Handle primitives
  if (typeof a !== 'object') return false;

  // Circular reference detection
  if (_seen.has(a)) return true;
  _seen.add(a);

  // Array check
  const aIsArray = Array.isArray(a);
  const bIsArray = Array.isArray(b);

  if (aIsArray !== bIsArray) return false;

  if (aIsArray) {
    if (a.length !== b.length) return false;

    for (let i = 0; i < a.length; i++) {
      if (!deepEqual(a[i], b[i], _seen)) return false;
    }

    return true;
  }

  // Object comparison
  const aKeys = Object.keys(a).sort();
  const bKeys = Object.keys(b).sort();

  if (aKeys.length !== bKeys.length) return false;

  // Key name check
  for (let i = 0; i < aKeys.length; i++) {
    if (aKeys[i] !== bKeys[i]) return false;
  }

  // Value check
  for (const key of aKeys) {
    if (!deepEqual(a[key], b[key], _seen)) return false;
  }

  return true;
}

/**
 * Creates a deterministic hash of an object
 * - Sorts keys for consistency
 * - Handles nested objects and arrays
 * - Returns SHA-256 hash in hex format
 *
 * @param {*} obj - Object to hash
 * @returns {string} SHA-256 hash (64 hex characters)
 *
 * @example
 * hashObject({ b: 2, a: 1 }) === hashObject({ a: 1, b: 2 }) // => true
 */
export function hashObject(obj) {
  /**
   * Recursively converts object to canonical string representation
   * @param {*} value - Value to stringify
   * @returns {string} Canonical string
   */
  function canonicalize(value) {
    if (value === null) return 'null';
    if (value === undefined) return 'undefined';

    const type = typeof value;

    if (type === 'boolean' || type === 'number') {
      return String(value);
    }

    if (type === 'string') {
      return JSON.stringify(value);
    }

    if (Array.isArray(value)) {
      const items = value.map(canonicalize);
      return `[${items.join(',')}]`;
    }

    if (type === 'object') {
      const keys = Object.keys(value).sort();
      const pairs = keys.map(key => {
        const canonKey = JSON.stringify(key);
        const canonValue = canonicalize(value[key]);
        return `${canonKey}:${canonValue}`;
      });
      return `{${pairs.join(',')}}`;
    }

    // Functions, symbols, etc.
    return String(value);
  }

  const canonical = canonicalize(obj);
  const hash = createHash('sha256');
  hash.update(canonical);
  return hash.digest('hex');
}

/**
 * Validates that a value is a pure function
 *
 * @param {*} fn - Value to check
 * @param {string} name - Function name for error messages
 * @throws {TypeError} If not a function
 */
export function validateFunction(fn, name) {
  if (typeof fn !== 'function') {
    throw new TypeError(`${name} must be a function, got ${typeof fn}`);
  }
}

/**
 * Validates that a value is a non-empty string
 *
 * @param {*} str - Value to check
 * @param {string} name - Field name for error messages
 * @throws {TypeError} If not a string or empty
 */
export function validateString(str, name) {
  if (typeof str !== 'string' || str.trim() === '') {
    throw new TypeError(`${name} must be a non-empty string`);
  }
}

/**
 * Clones an object deeply to prevent mutations
 *
 * @param {*} obj - Object to clone
 * @returns {*} Deep clone
 */
export function deepClone(obj) {
  if (obj === null || typeof obj !== 'object') {
    return obj;
  }

  if (Array.isArray(obj)) {
    return obj.map(deepClone);
  }

  const cloned = {};
  for (const key in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, key)) {
      cloned[key] = deepClone(obj[key]);
    }
  }

  return cloned;
}
