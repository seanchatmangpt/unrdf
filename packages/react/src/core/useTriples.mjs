/**
 * @fileoverview useTriples - React hook for querying RDF triples
 * @module react-hooks/core/useTriples
 *
 * @description
 * Hook for querying and filtering RDF triples from a store with reactive updates.
 * Supports pattern matching and automatic re-querying on store changes.
 *
 * @example
 * ```jsx
 * import { useTriples } from 'unrdf/react-hooks';
 * import { namedNode } from '@rdfjs/data-model';
 *
 * function MyComponent() {
 *   const { triples, loading, refetch } = useTriples({
 *     subject: namedNode('http://example.org/alice'),
 *     predicate: null, // Match any predicate
 *     object: null,
 *     graph: null
 *   });
 *
 *   return (
 *     <div>
 *       <h3>Triples: {triples.length}</h3>
 *       {triples.map((triple, i) => (
 *         <div key={i}>{triple.predicate.value}: {triple.object.value}</div>
 *       ))}
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useEffect, useCallback, useMemo } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Triple pattern for matching
 * @typedef {Object} TriplePattern
 * @property {Object|null} subject - Subject term or null for any
 * @property {Object|null} predicate - Predicate term or null for any
 * @property {Object|null} object - Object term or null for any
 * @property {Object|null} graph - Graph term or null for any
 */

/**
 * Hook for querying RDF triples
 *
 * @param {TriplePattern} pattern - Triple pattern to match
 * @param {Object} [options] - Query options
 * @param {boolean} [options.autoRefetch=true] - Automatically refetch on store changes
 * @param {number} [options.limit] - Maximum number of results
 * @returns {Object} Query result with triples and operations
 */
export function useTriples(pattern = {}, options = {}) {
  const { autoRefetch = true, limit } = options;
  const { store } = useKnowledgeEngineContext();

  const [triples, setTriples] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  const { subject = null, predicate = null, object = null, graph = null } = pattern;

  /**
   * Fetch triples matching pattern
   */
  const fetchTriples = useCallback(() => {
    if (!store) {
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const results = store.getQuads(subject, predicate, object, graph);

      // Apply limit if specified
      const limitedResults = limit ? results.slice(0, limit) : results;

      setTriples(limitedResults);
      setLoading(false);
    } catch (err) {
      console.error('[useTriples] Query failed:', err);
      setError(err);
      setLoading(false);
    }
  }, [store, subject, predicate, object, graph, limit]);

  /**
   * Manually refetch triples
   */
  const refetch = useCallback(() => {
    fetchTriples();
  }, [fetchTriples]);

  /**
   * Get count of matching triples
   */
  const count = useMemo(() => {
    if (!store) return 0;
    return store.countQuads(subject, predicate, object, graph);
  }, [store, subject, predicate, object, graph, triples.length]);

  /**
   * Check if any triples match
   */
  const hasMatches = useMemo(() => count > 0, [count]);

  // Initial fetch and auto-refetch
  useEffect(() => {
    fetchTriples();
  }, [fetchTriples, autoRefetch]);

  return {
    triples,
    loading,
    error,
    refetch,
    count,
    hasMatches,
  };
}
