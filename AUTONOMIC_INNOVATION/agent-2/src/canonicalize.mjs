/**
 * Canonicalization - Deterministic serialization for content-addressing
 * @module canonicalize
 */

/**
 * Canonicalize a capsule for hashing
 * Steps:
 * 1. Sort intent ops by type, then subject, then predicate
 * 2. Sort delta quads using RDF canonical ordering
 * 3. Sort parent hashes lexicographically
 * 4. Normalize all strings to UTF-8 NFC
 * 5. Serialize to JSON with sorted keys
 *
 * @param {Object} capsule - Capsule to canonicalize
 * @returns {string} Canonical JSON string
 */
export function canonicalizeCapsule(capsule) {
  // 1. Deep clone to avoid mutation
  const canonical = JSON.parse(JSON.stringify(capsule));

  // 2. Sort intent operations
  canonical.intent.ops.sort((a, b) => {
    // Compare type first
    if (a.type !== b.type) return a.type.localeCompare(b.type);
    // Then subject
    if (a.subject !== b.subject) return a.subject.localeCompare(b.subject);
    // Then predicate (if exists)
    const aPred = a.predicate || '';
    const bPred = b.predicate || '';
    if (aPred !== bPred) return aPred.localeCompare(bPred);
    // Then target (if exists)
    const aTarget = a.target || '';
    const bTarget = b.target || '';
    if (aTarget !== bTarget) return aTarget.localeCompare(bTarget);
    // Then graph (if exists)
    const aGraph = a.graph || '';
    const bGraph = b.graph || '';
    return aGraph.localeCompare(bGraph);
  });

  // 3. Sort delta quads (RDF canonical order: S-P-O)
  canonical.delta.add = canonicalizeQuads(canonical.delta.add);
  canonical.delta.del = canonicalizeQuads(canonical.delta.del);

  // 4. Sort parent hashes
  canonical.receipt.parents.sort();

  // 5. UTF-8 NFC normalization (for Unicode stability)
  const normalized = normalizeStrings(canonical);

  // 6. JSON stringify with sorted keys (deterministic order)
  return stringifyCanonical(normalized);
}

/**
 * Canonicalize quad array using RDF spec ordering
 * @param {Array<Object>} quads - Serialized quads
 * @returns {Array<Object>} Sorted quads
 */
export function canonicalizeQuads(quads) {
  if (!Array.isArray(quads)) {
    return [];
  }

  return quads.slice().sort((a, b) => {
    // Subject comparison
    const sCompare = compareStrings(a.subject, b.subject);
    if (sCompare !== 0) return sCompare;

    // Predicate comparison
    const pCompare = compareStrings(a.predicate, b.predicate);
    if (pCompare !== 0) return pCompare;

    // Object value comparison
    const oCompare = compareStrings(a.object.value, b.object.value);
    if (oCompare !== 0) return oCompare;

    // Object type comparison
    const otCompare = compareStrings(a.object.type, b.object.type);
    if (otCompare !== 0) return otCompare;

    // Graph comparison
    return compareStrings(a.graph, b.graph);
  });
}

/**
 * Compare strings deterministically
 * @param {string} a - First string
 * @param {string} b - Second string
 * @returns {number} -1, 0, or 1
 */
function compareStrings(a, b) {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

/**
 * Normalize all strings in object to UTF-8 NFC
 * @param {*} obj - Value to normalize
 * @returns {*} Normalized value
 */
function normalizeStrings(obj) {
  if (typeof obj === 'string') {
    return obj.normalize('NFC');
  }
  if (Array.isArray(obj)) {
    return obj.map(normalizeStrings);
  }
  if (obj && typeof obj === 'object') {
    const result = {};
    for (const [key, value] of Object.entries(obj)) {
      result[key.normalize('NFC')] = normalizeStrings(value);
    }
    return result;
  }
  return obj;
}

/**
 * Stringify object with canonically sorted keys
 * @param {Object} obj - Object to stringify
 * @returns {string} JSON string with sorted keys
 */
function stringifyCanonical(obj) {
  if (obj === null || obj === undefined) {
    return JSON.stringify(obj);
  }
  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }
  if (Array.isArray(obj)) {
    return '[' + obj.map(stringifyCanonical).join(',') + ']';
  }

  // Sort keys and stringify
  const keys = Object.keys(obj).sort();
  const pairs = keys.map((key) => {
    return JSON.stringify(key) + ':' + stringifyCanonical(obj[key]);
  });
  return '{' + pairs.join(',') + '}';
}
