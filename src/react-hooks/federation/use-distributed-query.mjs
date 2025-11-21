/**
 * @file use-distributed-query.mjs
 * @description React hook for executing distributed SPARQL queries across federated stores
 */

import { useState, useCallback, useEffect } from 'react';
import { useFederatedSystem } from './use-federated-system.mjs';

/**
 * Hook for distributed SPARQL query execution with intelligent routing
 *
 * @param {string|null} sparql - SPARQL query string (null for manual execution)
 * @param {Object} options - Query options
 * @param {string[]} [options.stores] - Specific stores to query
 * @param {string} [options.strategy='fastest'] - Query strategy
 * @param {string} [options.aggregation='union'] - Result aggregation
 * @param {number} [options.timeout=30000] - Query timeout
 * @param {boolean} [options.cache=true] - Enable result caching
 * @returns {Object} Query state and execution function
 *
 * @example
 * // Auto-execute on mount
 * const { data, loading, error } = useDistributedQuery(`
 *   SELECT ?s ?p ?o WHERE {
 *     ?s ?p ?o
 *   } LIMIT 100
 * `, { strategy: 'quorum', aggregation: 'union' });
 *
 * // Manual execution
 * const { execute, data, loading } = useDistributedQuery(null);
 * await execute('SELECT * WHERE { ?s ?p ?o }');
 */
export function useDistributedQuery(sparql = null, options = {}) {
  const { query: federatedQuery, system } = useFederatedSystem(options.federation || {});
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(!!sparql);
  const [error, setError] = useState(null);
  const [executionStats, setExecutionStats] = useState(null);

  // Execute query on mount if sparql provided
  useEffect(() => {
    if (!sparql || !system) return;

    let mounted = true;

    async function executeQuery() {
      try {
        setLoading(true);
        setError(null);

        const startTime = performance.now();

        const result = await federatedQuery(sparql, {
          stores: options.stores,
          strategy: options.strategy || 'fastest', // 'fastest', 'quorum', 'all', 'leader'
          aggregation: options.aggregation || 'union', // 'union', 'intersection'
          timeout: options.timeout || 30000,
          cache: options.cache !== false
        });

        const duration = performance.now() - startTime;

        if (!mounted) return;

        setData(result.bindings || result);
        setExecutionStats({
          duration,
          storesQueried: result.storesQueried || [],
          strategy: result.strategy,
          cacheHit: result.cacheHit || false
        });
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    executeQuery();

    return () => {
      mounted = false;
    };
  }, [sparql, system, JSON.stringify(options)]);

  // Manual execute function
  const execute = useCallback(async (queryString, executeOptions = {}) => {
    if (!system) {
      throw new Error('Federation system not initialized');
    }

    try {
      setLoading(true);
      setError(null);

      const startTime = performance.now();

      const result = await federatedQuery(queryString, {
        ...options,
        ...executeOptions
      });

      const duration = performance.now() - startTime;

      setData(result.bindings || result);
      setExecutionStats({
        duration,
        storesQueried: result.storesQueried || [],
        strategy: result.strategy,
        cacheHit: result.cacheHit || false
      });
      setLoading(false);

      return result;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [federatedQuery, system, options]);

  // Refetch current query
  const refetch = useCallback(() => {
    if (!sparql) {
      throw new Error('No query to refetch. Use execute() for manual queries.');
    }
    return execute(sparql);
  }, [sparql, execute]);

  return {
    data,
    loading,
    error,
    executionStats,
    execute,
    refetch
  };
}
