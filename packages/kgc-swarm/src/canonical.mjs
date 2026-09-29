/**
 * @file Canonical JSON serialization
 * @module @unrdf/kgc-swarm/canonical
 *
 * `JSON.stringify(v, Object.keys(v).sort())` is NOT canonical: an array replacer is a
 * whitelist applied at every depth, so nested keys absent from the top level are silently
 * dropped and tampering with nested data does not change the hash.
 */

/**
 * Serialize a value to JSON with object keys sorted recursively.
 * @param {unknown} value - Value to serialize
 * @returns {string} Canonical JSON string ('undefined' for undefined)
 */
export function canonicalStringify(value) {
  const canon = v => {
    if (v === null || typeof v !== 'object') return v;
    if (typeof v.toJSON === 'function') return canon(v.toJSON());
    if (Array.isArray(v)) return v.map(canon);
    const out = {};
    for (const key of Object.keys(v).sort()) {
      out[key] = canon(v[key]);
    }
    return out;
  };
  return JSON.stringify(canon(value)) ?? 'undefined';
}
