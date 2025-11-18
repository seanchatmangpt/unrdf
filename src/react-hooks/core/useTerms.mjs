/**
 * @fileoverview useTerms React hook - RDF term creation and manipulation
 * @module react-hooks/core/useTerms
 */

import { useCallback } from 'react';
import { DataFactory } from '@rdfjs/data-model';

const { quad, namedNode, literal, blankNode, defaultGraph, variable } = DataFactory;

/**
 * Hook for creating and manipulating RDF terms
 * @returns {Object} Term factory functions
 *
 * @example
 * const { createQuad, createNamedNode, createLiteral } = useTerms();
 * const q = createQuad('http://s', 'http://p', 'object value');
 */
export function useTerms() {
  const createNamedNode = useCallback((iri) => {
    if (typeof iri !== 'string') {
      throw new TypeError('IRI must be a string');
    }
    return namedNode(iri);
  }, []);

  const createLiteral = useCallback((value, languageOrDatatype) => {
    if (typeof languageOrDatatype === 'string') {
      return literal(String(value), languageOrDatatype);
    }
    return literal(String(value), languageOrDatatype);
  }, []);

  const createBlankNode = useCallback((id) => {
    return blankNode(id);
  }, []);

  const createQuad = useCallback((subject, predicate, object, graph) => {
    const s = typeof subject === 'string' ? namedNode(subject) : subject;
    const p = typeof predicate === 'string' ? namedNode(predicate) : predicate;
    const o = typeof object === 'string' ? literal(object) : object;
    const g = graph ? (typeof graph === 'string' ? namedNode(graph) : graph) : defaultGraph();

    return quad(s, p, o, g);
  }, []);

  const createVariable = useCallback((name) => {
    return variable(name);
  }, []);

  const isNamedNode = useCallback((term) => {
    return term?.termType === 'NamedNode';
  }, []);

  const isLiteral = useCallback((term) => {
    return term?.termType === 'Literal';
  }, []);

  const isBlankNode = useCallback((term) => {
    return term?.termType === 'BlankNode';
  }, []);

  const termToString = useCallback((term) => {
    if (!term) return '';
    return term.value || '';
  }, []);

  return {
    createNamedNode,
    createLiteral,
    createBlankNode,
    createQuad,
    createVariable,
    isNamedNode,
    isLiteral,
    isBlankNode,
    termToString,
    // Direct access to DataFactory
    DataFactory
  };
}
