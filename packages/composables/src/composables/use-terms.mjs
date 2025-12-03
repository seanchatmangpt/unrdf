/**
 * useTerms Composable - RDF Term Factory
 *
 * Provides helper functions for creating RDF terms with caching.
 * Terms are cached for equality comparisons.
 *
 * @module composables/use-terms
 */

import { namedNode, literal, blankNode, variable, defaultGraph, quad } from '@unrdf/core';

/**
 * Term cache for equality comparisons
 */
const termCache = new Map();

/**
 * Create RDF terms with caching
 *
 * @returns {{
 *   namedNode: (iri: string) => object,
 *   literal: (value: string, langOrDatatype?: string) => object,
 *   blankNode: (label?: string) => object,
 *   variable: (name: string) => object,
 *   defaultGraph: () => object,
 *   quad: (subject: object, predicate: object, object: object, graph?: object) => object,
 *   clearCache: () => void
 * }} Term factory functions
 * @example
 * const { namedNode, literal } = useTerms()
 * const person = namedNode('http://example.org/person/1')
 * const name = literal('Alice', 'en')
 */
export function useTerms() {
  /**
   * Create or retrieve cached named node
   *
   * @param {string} iri - IRI for the named node
   * @returns {object} Named node term
   */
  function cachedNamedNode(iri) {
    const key = `nn:${iri}`;
    if (termCache.has(key)) {
      return termCache.get(key);
    }
    const term = namedNode(iri);
    termCache.set(key, term);
    return term;
  }

  /**
   * Create or retrieve cached literal
   *
   * @param {string} value - Literal value
   * @param {string} [langOrDatatype] - Language tag or datatype IRI
   * @returns {object} Literal term
   */
  function cachedLiteral(value, langOrDatatype) {
    const key = `lit:${value}:${langOrDatatype || ''}`;
    if (termCache.has(key)) {
      return termCache.get(key);
    }
    const term = literal(value, langOrDatatype);
    termCache.set(key, term);
    return term;
  }

  /**
   * Create or retrieve cached blank node
   *
   * @param {string} [label] - Optional blank node label
   * @returns {object} Blank node term
   */
  function cachedBlankNode(label) {
    if (!label) {
      // Don't cache unlabeled blank nodes
      return blankNode();
    }
    const key = `bn:${label}`;
    if (termCache.has(key)) {
      return termCache.get(key);
    }
    const term = blankNode(label);
    termCache.set(key, term);
    return term;
  }

  /**
   * Create or retrieve cached variable
   *
   * @param {string} name - Variable name
   * @returns {object} Variable term
   */
  function cachedVariable(name) {
    const key = `var:${name}`;
    if (termCache.has(key)) {
      return termCache.get(key);
    }
    const term = variable(name);
    termCache.set(key, term);
    return term;
  }

  /**
   * Get the default graph term (singleton)
   *
   * @returns {object} Default graph term
   */
  function cachedDefaultGraph() {
    const key = 'dg:default';
    if (termCache.has(key)) {
      return termCache.get(key);
    }
    const term = defaultGraph();
    termCache.set(key, term);
    return term;
  }

  /**
   * Create a quad (not cached)
   *
   * @param {object} subject - Subject term
   * @param {object} predicate - Predicate term
   * @param {object} object - Object term
   * @param {object} [graph] - Graph term
   * @returns {object} Quad
   */
  function createQuad(subject, predicate, object, graph) {
    return quad(subject, predicate, object, graph);
  }

  /**
   * Clear the term cache
   */
  function clearCache() {
    termCache.clear();
  }

  return {
    namedNode: cachedNamedNode,
    literal: cachedLiteral,
    blankNode: cachedBlankNode,
    variable: cachedVariable,
    defaultGraph: cachedDefaultGraph,
    quad: createQuad,
    clearCache,
  };
}
