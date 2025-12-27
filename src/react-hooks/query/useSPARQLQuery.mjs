/**
 * @fileoverview useSPARQLQuery - React hook for synchronous SPARQL query execution
 * @module react-hooks/query/useSPARQLQuery
 *
 * @description
 * Hook for executing SPARQL queries with reactive state management.
 * Supports SELECT, ASK, CONSTRUCT, and DESCRIBE queries.
 *
 * @example
 * ```jsx
 * import { useSPARQLQuery } from 'unrdf/react-hooks';
 *
 * function QueryResults() {
 *   const { data, loading, error, refetch } = useSPARQLQuery(`
 *     PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *     SELECT ?name WHERE {
 *       ?person foaf:name ?name .
 *     } LIMIT 10
 *   `);
 *
 *   if (loading) return <div>Loading...</div>;
 *   if (error) return <div>Error: {error.message}</div>;
 *
 *   return <div>Results: {data?.rows?.length}</div>;
 * }
 * ```
 */

import { useState, useEffect, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for executing SPARQL queries
 *
 * @param {string} query - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {boolean} [options.autoExecute=true] - Execute query automatically
 * @param {number} [options.limit] - Result limit
 * @param {AbortSignal} [options.signal] - Abort signal
 * @param {Object} [options.dependencies=[]] - Dependencies for auto-refetch
 * @returns {Object} Query result state
 */
export function useSPARQLQuery(query, options = {}) {
  const { autoExecute = true, limit, signal, dependencies = [] } = options;

  const { engine, store } = useKnowledgeEngineContext();

  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(autoExecute);
  const [error, setError] = useState(null);

  /**
   * Execute the SPARQL query
   */
  const execute = useCallback(async () => {
    if (!engine || !store || !query) {
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const result = await engine.query(store, query, { limit, signal });

      setData(result);
      setLoading(false);
    } catch (err) {
      console.error('[useSPARQLQuery] Query failed:', err);
      setError(err);
      setLoading(false);
    }
  }, [engine, store, query, limit, signal]);

  /**
   * Manually refetch query
   */
  const refetch = useCallback(() => {
    return execute();
  }, [execute]);

  /**
   * Clear query results
   */
  const clear = useCallback(() => {
    setData(null);
    setError(null);
  }, []);

  // Auto-execute on mount and when dependencies change
  useEffect(() => {
    if (autoExecute) {
      execute();
    }
  }, [autoExecute, execute, ...dependencies]);

  return {
    data,
    loading,
    error,
    execute,
    refetch,
    clear,
  };
}
