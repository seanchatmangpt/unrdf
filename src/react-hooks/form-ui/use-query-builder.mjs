/**
 * @file use-query-builder.mjs
 * @description React hook for visual SPARQL query builder
 */

import { useState, useCallback } from 'react';

/**
 * Hook for building SPARQL queries visually
 *
 * @param {Object} config - Query builder configuration
 * @returns {Object} Query builder state and operations
 *
 * @example
 * const {
 *   addTriplePattern,
 *   addFilter,
 *   buildQuery,
 *   query,
 *   reset
 * } = useQueryBuilder();
 */
export function useQueryBuilder(config = {}) {
  const [triplePatterns, setTriplePatterns] = useState([]);
  const [filters, setFilters] = useState([]);
  const [prefixes, setPrefixes] = useState(new Map([
    ['rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'],
    ['rdfs', 'http://www.w3.org/2000/01/rdf-schema#'],
    ['schema', 'http://schema.org/']
  ]));

  const addTriplePattern = useCallback((subject, predicate, object) => {
    setTriplePatterns(prev => [...prev, { subject, predicate, object }]);
  }, []);

  const removeTriplePattern = useCallback((index) => {
    setTriplePatterns(prev => prev.filter((_, i) => i !== index));
  }, []);

  const addFilter = useCallback((variable, operator, value) => {
    setFilters(prev => [...prev, { variable, operator, value }]);
  }, []);

  const removeFilter = useCallback((index) => {
    setFilters(prev => prev.filter((_, i) => i !== index));
  }, []);

  const buildQuery = useCallback(() => {
    let query = '';

    // Add prefixes
    prefixes.forEach((uri, prefix) => {
      query += `PREFIX ${prefix}: <${uri}>\n`;
    });

    query += '\nSELECT * WHERE {\n';

    // Add triple patterns
    triplePatterns.forEach(({ subject, predicate, object }) => {
      query += `  ${subject} ${predicate} ${object} .\n`;
    });

    // Add filters
    if (filters.length > 0) {
      filters.forEach(({ variable, operator, value }) => {
        query += `  FILTER(${variable} ${operator} ${value})\n`;
      });
    }

    query += '}\nLIMIT 100';

    return query;
  }, [triplePatterns, filters, prefixes]);

  const reset = useCallback(() => {
    setTriplePatterns([]);
    setFilters([]);
  }, []);

  return {
    triplePatterns,
    filters,
    prefixes,
    addTriplePattern,
    removeTriplePattern,
    addFilter,
    removeFilter,
    buildQuery,
    reset,
    query: buildQuery()
  };
}
