/**
 * Canonical Diff - Deep comparison with deterministic ordering
 * @module @autonomic/agent-9/canonical-diff
 */

/**
 * @typedef {Object} DiffResult
 * @property {boolean} hasDifference - Whether values differ
 * @property {string[]} paths - Array of difference paths
 */

/**
 * Deep comparison with canonical ordering
 * Compares two values recursively and returns paths to all differences
 *
 * @param {any} legacy - Legacy value
 * @param {any} facade - Facade value
 * @returns {DiffResult}
 *
 * @example
 * const diff = canonicalDiff({ x: 1 }, { x: 2 });
 * // => { hasDifference: true, paths: ['root.x: 1 vs 2'] }
 */
export function canonicalDiff(legacy, facade) {
  const paths = [];

  /**
   * @param {any} a
   * @param {any} b
   * @param {string} path
   */
  function compare(a, b, path = 'root') {
    // Type mismatch
    if (typeof a !== typeof b) {
      paths.push(`${path}: type differs (${typeof a} vs ${typeof b})`);
      return;
    }

    // Handle null explicitly (typeof null === 'object')
    if (a === null || b === null) {
      if (a !== b) {
        paths.push(`${path}: ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);
      }
      return;
    }

    // Primitive comparison
    if (typeof a !== 'object') {
      if (a !== b) {
        paths.push(`${path}: ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);
      }
      return;
    }

    // Array comparison (order-sensitive)
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) {
        paths.push(`${path}.length: ${a.length} vs ${b.length}`);
      }
      const len = Math.max(a.length, b.length);
      for (let i = 0; i < len; i++) {
        compare(a[i], b[i], `${path}[${i}]`);
      }
      return;
    }

    // Array vs non-array mismatch
    if (Array.isArray(a) !== Array.isArray(b)) {
      paths.push(
        `${path}: type differs (${Array.isArray(a) ? 'array' : 'object'} vs ${Array.isArray(b) ? 'array' : 'object'})`
      );
      return;
    }

    // Object comparison (key-order independent)
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    const allKeys = new Set([...keysA, ...keysB]);
    for (const key of allKeys) {
      if (!(key in a)) {
        paths.push(`${path}.${key}: missing in legacy`);
      } else if (!(key in b)) {
        paths.push(`${path}.${key}: missing in facade`);
      } else {
        compare(a[key], b[key], `${path}.${key}`);
      }
    }
  }

  compare(legacy, facade);

  return {
    hasDifference: paths.length > 0,
    paths,
  };
}

/**
 * Canonicalize value for deterministic serialization
 * Recursively sorts object keys and preserves array order
 *
 * @param {any} value - Value to canonicalize
 * @returns {any} - Canonicalized value
 *
 * @example
 * const canonical = canonicalSerialize({ z: 1, a: 2 });
 * // Object.keys(canonical) => ['a', 'z'] (sorted)
 */
export function canonicalSerialize(value) {
  if (value === null || value === undefined) {
    return value;
  }

  if (typeof value !== 'object') {
    return value;
  }

  if (Array.isArray(value)) {
    return value.map(canonicalSerialize);
  }

  // Sort object keys
  const sorted = {};
  Object.keys(value)
    .sort()
    .forEach((key) => {
      sorted[key] = canonicalSerialize(value[key]);
    });
  return sorted;
}

/**
 * Canonical JSON replacer (sort object keys)
 * Use with JSON.stringify(obj, canonicalReplacer) for deterministic output
 *
 * @param {string} key - Property key
 * @param {any} value - Property value
 * @returns {any} - Sorted object or original value
 */
export function canonicalReplacer(key, value) {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    return Object.keys(value)
      .sort()
      .reduce((sorted, k) => {
        sorted[k] = value[k];
        return sorted;
      }, {});
  }
  return value;
}
