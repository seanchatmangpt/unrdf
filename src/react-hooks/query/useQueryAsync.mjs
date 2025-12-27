/**
 * @fileoverview useQueryAsync - React hook for async SPARQL queries with loading states
 * @module react-hooks/query/useQueryAsync
 *
 * @description
 * Advanced async query hook with comprehensive loading state management,
 * caching, and retry logic.
 *
 * @example
 * ```jsx
 * import { useQueryAsync } from 'unrdf/react-hooks';
 *
 * function AsyncQuery() {
 *   const { data, loading, error, refetch, isStale } = useQueryAsync(
 *     'SELECT * WHERE { ?s ?p ?o } LIMIT 100',
 *     { cache: true, retries: 3 }
 *   );
 *
 *   return (
 *     <div>
 *       {loading && <Spinner />}
 *       {error && <Error message={error.message} onRetry={refetch} />}
 *       {data && <Results data={data} isStale={isStale} />}
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for async SPARQL query execution
 *
 * @param {string} query - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {boolean} [options.cache=false] - Enable caching
 * @param {number} [options.cacheTime=300000] - Cache time in ms (5 minutes)
 * @param {number} [options.retries=0] - Number of retry attempts
 * @param {number} [options.retryDelay=1000] - Delay between retries in ms
 * @param {Function} [options.onSuccess] - Success callback
 * @param {Function} [options.onError] - Error callback
 * @returns {Object} Async query state
 */
export function useQueryAsync(query, options = {}) {
  const {
    cache = false,
    cacheTime = 300000,
    retries = 0,
    retryDelay = 1000,
    onSuccess,
    onError,
  } = options;

  const { engine, store } = useKnowledgeEngineContext();

  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const [isStale, setIsStale] = useState(false);
  const [attemptCount, setAttemptCount] = useState(0);

  const cacheRef = useRef(new Map());
  const timestampRef = useRef(null);
  const abortControllerRef = useRef(null);

  /**
   * Generate cache key for query
   */
  const getCacheKey = useCallback(query => {
    return `query:${query}`;
  }, []);

  /**
   * Check if cached data is valid
   */
  const isCacheValid = useCallback(
    timestamp => {
      if (!timestamp) return false;
      return Date.now() - timestamp < cacheTime;
    },
    [cacheTime]
  );

  /**
   * Execute query with retry logic
   */
  const executeWithRetry = useCallback(
    async (attemptNumber = 0) => {
      if (!engine || !store || !query) {
        return;
      }

      try {
        // Check cache first
        if (cache) {
          const cacheKey = getCacheKey(query);
          const cached = cacheRef.current.get(cacheKey);
          const timestamp = timestampRef.current;

          if (cached && isCacheValid(timestamp)) {
            setData(cached);
            setIsStale(false);
            return cached;
          }
        }

        setLoading(true);
        setError(null);
        setIsStale(false);

        // Create abort controller
        abortControllerRef.current = new AbortController();

        const result = await engine.query(store, query, {
          signal: abortControllerRef.current.signal,
        });

        // Update cache
        if (cache) {
          const cacheKey = getCacheKey(query);
          cacheRef.current.set(cacheKey, result);
          timestampRef.current = Date.now();
        }

        setData(result);
        setLoading(false);
        setAttemptCount(0);

        if (onSuccess) {
          onSuccess(result);
        }

        return result;
      } catch (err) {
        // Handle retries
        if (attemptNumber < retries) {
          console.log(`[useQueryAsync] Retry ${attemptNumber + 1}/${retries}`);
          await new Promise(resolve => setTimeout(resolve, retryDelay));
          setAttemptCount(attemptNumber + 1);
          return executeWithRetry(attemptNumber + 1);
        }

        console.error('[useQueryAsync] Query failed after retries:', err);
        setError(err);
        setLoading(false);

        if (onError) {
          onError(err);
        }

        throw err;
      }
    },
    [
      engine,
      store,
      query,
      cache,
      getCacheKey,
      isCacheValid,
      retries,
      retryDelay,
      onSuccess,
      onError,
    ]
  );

  /**
   * Execute query
   */
  const execute = useCallback(() => {
    return executeWithRetry(0);
  }, [executeWithRetry]);

  /**
   * Refetch query (bypass cache)
   */
  const refetch = useCallback(() => {
    // Invalidate cache
    if (cache) {
      const cacheKey = getCacheKey(query);
      cacheRef.current.delete(cacheKey);
      timestampRef.current = null;
    }
    return execute();
  }, [cache, getCacheKey, query, execute]);

  /**
   * Cancel ongoing query
   */
  const cancel = useCallback(() => {
    if (abortControllerRef.current) {
      abortControllerRef.current.abort();
      abortControllerRef.current = null;
      setLoading(false);
    }
  }, []);

  /**
   * Clear query results and cache
   */
  const clear = useCallback(() => {
    setData(null);
    setError(null);
    setIsStale(false);
    if (cache) {
      cacheRef.current.clear();
      timestampRef.current = null;
    }
  }, [cache]);

  // Auto-execute on mount
  useEffect(() => {
    execute();

    return () => {
      cancel();
    };
  }, [query]);

  return {
    data,
    loading,
    error,
    isStale,
    attemptCount,
    execute,
    refetch,
    cancel,
    clear,
  };
}
