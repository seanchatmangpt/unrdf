/**
 * @fileoverview useMemoizedQuery - Hook for memoized query execution
 * @module react-hooks/cache/useMemoizedQuery
 */

import { useState, useEffect, useMemo } from 'react';
import { useQueryCache } from './useQueryCache.mjs';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

export function useMemoizedQuery(query, dependencies = []) {
  const { engine, store } = useKnowledgeEngineContext();
  const { get, set } = useQueryCache();
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);

  const cacheKey = useMemo(() => `query:${query}`, [query]);

  useEffect(() => {
    const execute = async () => {
      const cached = get(cacheKey);
      if (cached) {
        setData(cached);
        setLoading(false);
        return;
      }

      setLoading(true);
      try {
        const result = await engine.query(store, query);
        set(cacheKey, result);
        setData(result);
      } catch (err) {
        console.error('[useMemoizedQuery] failed:', err);
      }
      setLoading(false);
    };

    execute();
  }, [query, ...dependencies]);

  return { data, loading };
}
