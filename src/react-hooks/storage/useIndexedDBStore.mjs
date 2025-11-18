/**
 * @fileoverview useIndexedDBStore - Hook for IndexedDB quad store operations
 * @module react-hooks/storage/useIndexedDBStore
 */

import { useState, useEffect, useCallback } from 'react';
import { IndexedDBQuadStore } from '../../../browser/indexeddb-store.mjs';

export function useIndexedDBStore(options = {}) {
  const [store, setStore] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    const initStore = async () => {
      try {
        const idbStore = new IndexedDBQuadStore();
        await idbStore.init();
        setStore(idbStore);
        setLoading(false);
      } catch (err) {
        setError(err);
        setLoading(false);
      }
    };

    initStore();
  }, []);

  const addQuad = useCallback(async (quad) => {
    if (!store) return false;
    try {
      await store.addQuad(quad);
      return true;
    } catch (err) {
      console.error('[useIndexedDBStore] addQuad failed:', err);
      return false;
    }
  }, [store]);

  const match = useCallback(async (pattern) => {
    if (!store) return [];
    return store.match(pattern);
  }, [store]);

  const clear = useCallback(async () => {
    if (!store) return false;
    try {
      await store.clear();
      return true;
    } catch (err) {
      console.error('[useIndexedDBStore] clear failed:', err);
      return false;
    }
  }, [store]);

  return { store, loading, error, addQuad, match, clear };
}
