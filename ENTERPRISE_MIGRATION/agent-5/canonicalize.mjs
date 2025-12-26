/**
 * @fileoverview JSON Canonicalization Scheme (RFC 8785) implementation.
 * Provides deterministic JSON serialization for cryptographic hashing.
 */

/**
 * Canonicalize a JavaScript object according to RFC 8785.
 * Ensures stable, deterministic JSON output for hashing.
 *
 * Rules:
 * - Keys sorted lexicographically
 * - No whitespace
 * - UTF-8 encoding
 * - Numbers in canonical format
 * - No trailing zeros in decimals
 *
 * @param {any} obj - Object to canonicalize
 * @returns {string} Canonical JSON string
 * @throws {Error} If object contains invalid types
 */
export function canonicalize(obj) {
  // Handle primitives
  if (obj === null) {
    return 'null';
  }

  if (obj === undefined) {
    throw new Error('Cannot canonicalize undefined');
  }

  if (typeof obj === 'boolean') {
    return obj ? 'true' : 'false';
  }

  if (typeof obj === 'number') {
    return canonicalizeNumber(obj);
  }

  if (typeof obj === 'string') {
    return JSON.stringify(obj);
  }

  if (typeof obj === 'bigint') {
    throw new Error('Cannot canonicalize BigInt');
  }

  if (typeof obj === 'symbol') {
    throw new Error('Cannot canonicalize Symbol');
  }

  if (typeof obj === 'function') {
    throw new Error('Cannot canonicalize Function');
  }

  // Handle arrays
  if (Array.isArray(obj)) {
    const elements = obj.map(item => canonicalize(item));
    return `[${elements.join(',')}]`;
  }

  // Handle objects
  if (typeof obj === 'object') {
    // Sort keys lexicographically
    const keys = Object.keys(obj).sort();
    const pairs = keys.map(key => {
      const value = obj[key];
      // Skip undefined values
      if (value === undefined) {
        return null;
      }
      return `${JSON.stringify(key)}:${canonicalize(value)}`;
    }).filter(pair => pair !== null);

    return `{${pairs.join(',')}}`;
  }

  throw new Error(`Cannot canonicalize type: ${typeof obj}`);
}

/**
 * Canonicalize a number according to RFC 8785.
 * Handles integers, floats, infinity, and NaN.
 *
 * @param {number} num - Number to canonicalize
 * @returns {string} Canonical number representation
 * @throws {Error} If number is NaN or Infinity
 */
function canonicalizeNumber(num) {
  if (!Number.isFinite(num)) {
    throw new Error('Cannot canonicalize Infinity or NaN');
  }

  // Integer check
  if (Number.isInteger(num)) {
    return num.toString();
  }

  // Float handling - use JSON stringification which follows RFC 8785
  // for scientific notation and precision
  const str = JSON.stringify(num);

  // Ensure no trailing zeros after decimal point (except for .0)
  if (str.includes('.') && !str.includes('e') && !str.includes('E')) {
    return str.replace(/\.?0+$/, '');
  }

  return str;
}

/**
 * Serialize object to canonical JSON with optional pretty printing.
 * For hashing, always use canonicalize() directly (no whitespace).
 *
 * @param {any} obj - Object to serialize
 * @param {Object} options - Serialization options
 * @param {boolean} [options.pretty=false] - Add whitespace for readability
 * @returns {string} Canonical JSON string
 */
export function serializeCanonical(obj, options = {}) {
  const canonical = canonicalize(obj);

  if (options.pretty) {
    // Parse and re-stringify with indentation
    // Note: This breaks canonicalization but useful for debugging
    return JSON.stringify(JSON.parse(canonical), null, 2);
  }

  return canonical;
}

/**
 * Deserialize canonical JSON string.
 *
 * @param {string} json - JSON string to parse
 * @returns {any} Parsed object
 * @throws {Error} If JSON is invalid
 */
export function deserializeCanonical(json) {
  if (typeof json !== 'string') {
    throw new Error('deserializeCanonical requires a string');
  }

  try {
    return JSON.parse(json);
  } catch (error) {
    throw new Error(`Invalid canonical JSON: ${error.message}`);
  }
}

/**
 * Verify if two objects produce the same canonical representation.
 *
 * @param {any} obj1 - First object
 * @param {any} obj2 - Second object
 * @returns {boolean} True if canonical forms match
 */
export function areCanonicallyEqual(obj1, obj2) {
  try {
    return canonicalize(obj1) === canonicalize(obj2);
  } catch {
    return false;
  }
}

/**
 * Sort object keys recursively for canonical representation.
 *
 * @param {any} obj - Object to sort
 * @returns {any} Object with sorted keys
 */
export function sortKeys(obj) {
  if (obj === null || typeof obj !== 'object') {
    return obj;
  }

  if (Array.isArray(obj)) {
    return obj.map(item => sortKeys(item));
  }

  const sorted = {};
  const keys = Object.keys(obj).sort();

  for (const key of keys) {
    sorted[key] = sortKeys(obj[key]);
  }

  return sorted;
}
