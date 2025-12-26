/**
 * @file stable-json.mjs - Canonical JSON serialization for determinism
 * @description Ensures JSON output is identical across runs (Q1 invariant)
 * @invariant Q1: Deterministic serialization independent of insertion order
 */

/**
 * Stringify with sorted keys for deterministic output
 * @param {any} obj - Object to stringify
 * @param {number} indent - Indentation level (0 for compact)
 * @returns {string} - Canonical JSON string
 */
export function stableStringify(obj, indent = 2) {
  return JSON.stringify(obj, replacer, indent)
}

/**
 * Replacer function that sorts keys at each level
 * @private
 */
function replacer(key, value) {
  if (value === null || typeof value !== 'object') {
    return value
  }

  if (Array.isArray(value)) {
    return value
  }

  // Sort object keys
  const sorted = {}
  Object.keys(value).sort().forEach(k => {
    sorted[k] = value[k]
  })

  return sorted
}

/**
 * Parse JSON and deep-sort all nested objects
 * @param {string} jsonStr - JSON string
 * @returns {any} - Parsed and sorted object
 */
export function deepSort(obj) {
  if (obj === null || typeof obj !== 'object') {
    return obj
  }

  if (Array.isArray(obj)) {
    return obj.map(deepSort)
  }

  const sorted = {}
  Object.keys(obj).sort().forEach(k => {
    sorted[k] = deepSort(obj[k])
  })

  return sorted
}

/**
 * Create deterministic canonical form
 * @param {any} obj - Object to canonicalize
 * @returns {any} - Canonical form (deep-sorted)
 */
export function canonicalize(obj) {
  return deepSort(obj)
}
