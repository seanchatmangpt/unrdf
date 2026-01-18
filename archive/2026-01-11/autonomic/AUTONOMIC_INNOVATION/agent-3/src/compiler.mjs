/**
 * @fileoverview Lens compilation to deterministic executable program
 * @module @unrdf/lens/compiler
 */

import { createHash } from 'crypto';

/**
 * @typedef {Object} CompiledPredicate
 * @property {string} property - Property name
 * @property {string} iri - Full predicate IRI
 * @property {string} [datatype] - Full datatype IRI
 * @property {boolean} [required] - Required field
 * @property {string} [transformId] - Transform function ID
 */

/**
 * @typedef {Object} CompiledSubject
 * @property {string} pattern - Original pattern
 * @property {string[]} keys - Key fields
 * @property {string} resolvedPattern - Pattern with {namespace} expanded
 * @property {string} [skolemPattern] - Skolem pattern if specified
 */

/**
 * @typedef {Object} CompiledMapping
 * @property {CompiledSubject} subject - Subject generation rules
 * @property {string} [type] - Full RDF type IRI
 * @property {CompiledPredicate[]} predicates - Sorted predicate list
 */

/**
 * @typedef {Object} CompiledLens
 * @property {string} lensId - Original lens ID
 * @property {string} version - Lens version
 * @property {Object} profile - Resolved profile (namespaces, conventions)
 * @property {Object<string, CompiledMapping>} compiledMappings - Pre-computed mapping rules
 * @property {string} canonicalHash - SHA-256 hash of canonical form
 */

/**
 * Resolve IRI pattern by expanding {namespace}
 * @param {string} pattern - Pattern with placeholders
 * @param {string} namespace - Base namespace
 * @returns {string} Pattern with {namespace} expanded
 */
function resolvePattern(pattern, namespace) {
  return pattern.replace(/\{namespace\}/g, namespace);
}

/**
 * Compile predicate mappings into sorted array
 * @param {Object<string, import('./lens.mjs').PredicateMapping>} predicates - Predicate mappings
 * @returns {CompiledPredicate[]} Sorted predicate array
 */
function compilePredicates(predicates) {
  const compiled = [];

  // Sort by property name (canonical ordering)
  const sortedProperties = Object.keys(predicates).sort();

  for (const property of sortedProperties) {
    const predicate = predicates[property];
    compiled.push({
      property,
      iri: predicate.iri,
      datatype: predicate.datatype,
      required: predicate.required || false,
      transformId: predicate.transformId || null,
    });
  }

  return compiled;
}

/**
 * Compile subject rule
 * @param {import('./lens.mjs').SubjectRule} subject - Subject rule
 * @param {string} namespace - Base namespace
 * @returns {CompiledSubject} Compiled subject rule
 */
function compileSubject(subject, namespace) {
  return {
    pattern: subject.pattern,
    keys: [...subject.keys], // Copy array
    resolvedPattern: resolvePattern(subject.pattern, namespace),
    skolemPattern: subject.skolemPattern,
  };
}

/**
 * Compute canonical hash of compiled lens
 * @param {Object} compiledLens - Compiled lens (without hash)
 * @returns {string} SHA-256 hash
 */
function computeCanonicalHash(compiledLens) {
  // Create canonical JSON: sorted keys at all levels
  const canonical = JSON.stringify(compiledLens, (key, value) => {
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      // Sort object keys
      const sorted = {};
      for (const k of Object.keys(value).sort()) {
        sorted[k] = value[k];
      }
      return sorted;
    }
    return value;
  });

  return createHash('sha256').update(canonical, 'utf8').digest('hex');
}

/**
 * Compile lens into deterministic executable program
 * @param {import('./lens.mjs').Lens} lens - Lens definition
 * @returns {CompiledLens} Serializable compiled program
 *
 * @example
 * const lens = defineLens('customer-v1', profile, mappings);
 * const compiled = compileLens(lens);
 * console.log(compiled.canonicalHash); // sha256:abc123...
 *
 * // Compiled lens is JSON-serializable
 * const json = JSON.stringify(compiled);
 * const restored = JSON.parse(json);
 */
export function compileLens(lens) {
  // Validate input
  if (!lens || typeof lens !== 'object') {
    throw new Error('Invalid lens: must be object');
  }

  if (!lens.id || !lens.profile || !lens.mappings) {
    throw new Error('Invalid lens: missing required fields (id, profile, mappings)');
  }

  // Compile mappings
  const compiledMappings = {};

  // Sort entity types for determinism
  const sortedEntityTypes = Object.keys(lens.mappings).sort();

  for (const entityType of sortedEntityTypes) {
    const mapping = lens.mappings[entityType];

    compiledMappings[entityType] = {
      subject: compileSubject(mapping.subject, lens.profile.namespace),
      type: mapping.type || null,
      predicates: compilePredicates(mapping.predicates),
    };
  }

  // Build compiled lens (without hash first)
  const compiledLens = {
    lensId: lens.id,
    version: lens.version,
    profile: {
      namespace: lens.profile.namespace,
      prefixes: { ...lens.profile.prefixes },
      conventions: { ...lens.profile.conventions },
    },
    compiledMappings,
  };

  // Compute canonical hash
  const canonicalHash = computeCanonicalHash(compiledLens);

  // Return final compiled lens with hash
  return {
    ...compiledLens,
    canonicalHash: `sha256:${canonicalHash}`,
  };
}

/**
 * Verify compiled lens has no closures (all data)
 * @param {CompiledLens} compiledLens - Compiled lens to check
 * @returns {boolean} True if serializable (no functions)
 *
 * @example
 * const compiled = compileLens(lens);
 * const isSerializable = verifySerializable(compiled);
 * console.log(isSerializable); // true
 */
export function verifySerializable(compiledLens) {
  try {
    const json = JSON.stringify(compiledLens);
    const restored = JSON.parse(json);

    // Check for function strings (would indicate closure leak)
    if (json.includes('function') || json.includes('=>')) {
      return false;
    }

    // Deep equality check
    return JSON.stringify(restored) === json;
  } catch (error) {
    return false;
  }
}
