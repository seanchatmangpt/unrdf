/**
 * @fileoverview useQueryCache - Hook for query cache management
 * @module react-hooks/cache/useQueryCache
 */

import { useState, useCallback, useRef } from 'react';
import { LRUCache } from 'lru-cache';

/**
 *
 */
export function useQueryCache(options = {}) {
  const { max = 100, ttl = 300000 } = options;
  const cacheRef = useRef(new LRUCache({ max, ttl }));
  const [stats, setStats] = useState({ hits: 0, misses: 0 });

  const get = useCallback((key) => {
    const value = cacheRef.current.get(key);
    if (value !== undefined) {
      setStats((s) => ({ ...s, hits: s.hits + 1 }));
    } else {
      setStats((s) => ({ ...s, misses: s.misses + 1 }));
    }
    return value;
  }, []);

  const set = useCallback((key, value) => {
    cacheRef.current.set(key, value);
  }, []);

  const clear = useCallback(() => {
    cacheRef.current.clear();
    setStats({ hits: 0, misses: 0 });
  }, []);

  return { get, set, clear, stats };
}
