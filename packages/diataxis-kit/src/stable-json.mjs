/**
 * @file Stable JSON stringification for deterministic output
 * @module stable-json
 */

/**
 * @typedef {Object} StableStringifyOptions
 * @property {number} [indent=2] - Number of spaces for indentation
 * @property {boolean} [includeNull=true] - Include null values in output
 */

/**
 * Deterministically stringify an object with sorted keys
 * @param {any} obj - Object to stringify
 * @param {StableStringifyOptions} [options={}] - Stringification options
 * @returns {string} Deterministically stringified JSON
 */
export function stableStringify(obj, options = {}) {
  const indent = options.indent ?? 2;
  const includeNull = options.includeNull ?? true;

  /**
   * Internal recursive stringifier
   * @param {any} value - Value to stringify
   * @param {number} depth - Current indentation depth
   * @returns {string} Stringified value
   */
  function stringify(value, depth = 0) {
    // Handle primitives
    if (value === null) {
      return includeNull ? 'null' : undefined;
    }
    if (value === undefined) {
      return undefined;
    }
    if (typeof value === 'boolean' || typeof value === 'number') {
      return String(value);
    }
    if (typeof value === 'string') {
      return JSON.stringify(value);
    }

    // Handle arrays - preserve order, do NOT sort
    if (Array.isArray(value)) {
      if (value.length === 0) {
        return '[]';
      }
      const items = value
        .map(item => stringify(item, depth + 1))
        .filter(item => item !== undefined);

      if (indent === 0) {
        return `[${items.join(',')}]`;
      }

      const currentIndent = ' '.repeat(depth * indent);
      const nextIndent = ' '.repeat((depth + 1) * indent);
      return `[\n${nextIndent}${items.join(`,\n${nextIndent}`)}\n${currentIndent}]`;
    }

    // Handle objects - SORT keys
    if (typeof value === 'object') {
      const keys = Object.keys(value).sort((a, b) => a.localeCompare(b));

      if (keys.length === 0) {
        return '{}';
      }

      const pairs = [];
      for (const key of keys) {
        const stringified = stringify(value[key], depth + 1);
        if (stringified !== undefined) {
          pairs.push([key, stringified]);
        }
      }

      if (pairs.length === 0) {
        return '{}';
      }

      if (indent === 0) {
        return `{${pairs.map(([k, v]) => `${JSON.stringify(k)}:${v}`).join(',')}}`;
      }

      const currentIndent = ' '.repeat(depth * indent);
      const nextIndent = ' '.repeat((depth + 1) * indent);
      const pairStrings = pairs.map(([k, v]) => `${nextIndent}${JSON.stringify(k)}: ${v}`);
      return `{\n${pairStrings.join(',\n')}\n${currentIndent}}`;
    }

    // Fallback for functions, symbols, etc.
    return undefined;
  }

  const result = stringify(obj, 0);
  return result ?? 'null';
}

/**
 * Compare two objects for structural equality using stable stringify
 * @param {any} obj1 - First object
 * @param {any} obj2 - Second object
 * @returns {boolean} True if structurally equal
 */
export function stableEqual(obj1, obj2) {
  try {
    const str1 = stableStringify(obj1, { indent: 0 });
    const str2 = stableStringify(obj2, { indent: 0 });
    return str1 === str2;
  } catch {
    return false;
  }
}
