/**
 * @file Pattern Matching - Find quads matching triple patterns
 * @module @unrdf/knowledge-engine/pattern-matcher
 */

import { getQuads } from '@unrdf/core';

/**
 * @typedef {import('n3').Quad} Quad
 * @typedef {import('n3').Term} Term
 * @typedef {import('n3').Store} Store
 */

/**
 * Match a pattern against the store
 *
 * @param {Store} store - RDF store
 * @param {Object} pattern - Pattern to match
 * @param {string|Object} [pattern.subject] - Subject pattern
 * @param {string|Object} [pattern.predicate] - Predicate pattern
 * @param {string|Object} [pattern.object] - Object pattern
 * @param {string|Object} [pattern.graph] - Graph pattern
 * @returns {Quad[]} Array of matching quads
 *
 * @example
 * const matches = matchPattern(store, {
 *   subject: null,
 *   predicate: 'rdf:type',
 *   object: 'foaf:Person'
 * });
 */
export function matchPattern(store, pattern) {
  const subject = pattern.subject && !isVariable(pattern.subject) ? pattern.subject : null;
  const predicate = pattern.predicate && !isVariable(pattern.predicate) ? pattern.predicate : null;
  const object = pattern.object && !isVariable(pattern.object) ? pattern.object : null;
  const graph = pattern.graph && !isVariable(pattern.graph) ? pattern.graph : null;

  return getQuads(store, subject, predicate, object, graph);
}

/**
 * Match a pattern and return variable bindings
 *
 * @param {Store} store - RDF store
 * @param {Object} pattern - Pattern to match
 * @returns {Object[]} Array of binding objects
 *
 * @example
 * const bindings = matchPatternWithBindings(store, {
 *   subject: '?person',
 *   predicate: 'rdf:type',
 *   object: 'foaf:Person'
 * });
 * // Returns: [{ person: <http://example.org/alice> }, ...]
 */
export function matchPatternWithBindings(store, pattern) {
  const matches = matchPattern(store, pattern);
  const bindings = [];

  for (const quad of matches) {
    const binding = {};

    if (isVariable(pattern.subject)) {
      const varName = getVariableName(pattern.subject);
      binding[varName] = quad.subject;
    }

    if (isVariable(pattern.predicate)) {
      const varName = getVariableName(pattern.predicate);
      binding[varName] = quad.predicate;
    }

    if (isVariable(pattern.object)) {
      const varName = getVariableName(pattern.object);
      binding[varName] = quad.object;
    }

    if (pattern.graph && isVariable(pattern.graph)) {
      const varName = getVariableName(pattern.graph);
      binding[varName] = quad.graph;
    }

    bindings.push(binding);
  }

  return bindings;
}

/**
 * Check if a pattern has at least one match
 *
 * @param {Store} store - RDF store
 * @param {Object} pattern - Pattern to match
 * @returns {boolean} True if pattern matches at least one quad
 *
 * @example
 * if (hasMatch(store, { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' })) {
 *   console.log('Found at least one person');
 * }
 */
export function hasMatch(store, pattern) {
  const matches = matchPattern(store, pattern);
  return matches.length > 0;
}

/**
 * Match multiple patterns (AND conjunction)
 *
 * @param {Store} store - RDF store
 * @param {Object[]} patterns - Array of patterns
 * @returns {Object[]} Array of binding sets that satisfy all patterns
 *
 * @example
 * const bindings = matchMultiplePatterns(store, [
 *   { subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' },
 *   { subject: '?x', predicate: 'foaf:name', object: '?name' }
 * ]);
 */
export function matchMultiplePatterns(store, patterns) {
  if (patterns.length === 0) {
    return [];
  }

  if (patterns.length === 1) {
    return matchPatternWithBindings(store, patterns[0]);
  }

  let currentBindings = matchPatternWithBindings(store, patterns[0]);

  for (let i = 1; i < patterns.length; i++) {
    const pattern = patterns[i];
    const newBindings = [];

    for (const binding of currentBindings) {
      const instantiatedPattern = instantiatePattern(pattern, binding);
      const matches = matchPatternWithBindings(store, instantiatedPattern);

      for (const match of matches) {
        const merged = { ...binding, ...match };
        newBindings.push(merged);
      }
    }

    currentBindings = newBindings;

    if (currentBindings.length === 0) {
      break;
    }
  }

  return currentBindings;
}

/**
 * Instantiate a pattern with variable bindings
 *
 * @param {Object} pattern - Pattern template
 * @param {Object} bindings - Variable bindings
 * @returns {Object} Pattern with variables replaced
 */
function instantiatePattern(pattern, bindings) {
  return {
    subject: instantiateTerm(pattern.subject, bindings),
    predicate: instantiateTerm(pattern.predicate, bindings),
    object: instantiateTerm(pattern.object, bindings),
    graph: pattern.graph ? instantiateTerm(pattern.graph, bindings) : null,
  };
}

/**
 * Instantiate a term with variable bindings
 *
 * @param {string|Object} term - Term or variable
 * @param {Object} bindings - Variable bindings
 * @returns {Object|string} Instantiated term
 */
function instantiateTerm(term, bindings) {
  if (!isVariable(term)) {
    return term;
  }

  const varName = getVariableName(term);
  return bindings[varName] || term;
}

/**
 * Check if a term is a variable
 *
 * @param {string|Object} term - Term to check
 * @returns {boolean} True if term is a variable
 */
function isVariable(term) {
  if (!term) return false;

  if (typeof term === 'object' && term.type === 'variable') {
    return true;
  }

  const str = String(term);
  return str.startsWith('?');
}

/**
 * Get variable name (remove '?' prefix)
 *
 * @param {string|Object} term - Variable term
 * @returns {string} Variable name
 */
function getVariableName(term) {
  if (typeof term === 'object' && term.value) {
    return term.value.replace(/^\?/, '');
  }

  return String(term).replace(/^\?/, '');
}
