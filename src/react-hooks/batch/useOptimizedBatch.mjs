/**
 * @fileoverview useOptimizedBatch - Hook for Dark Matter optimized batch operations
 * @module react-hooks/batch/useOptimizedBatch
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 *
 */
export function useOptimizedBatch() {
  const { engine, store } = useKnowledgeEngineContext();
  const [pending, setPending] = useState(false);

  const executeDarkMatter = useCallback(
    async queries => {
      setPending(true);
      try {
        const results = await engine.darkMatter?.executeBatch(store, queries);
        setPending(false);
        return results;
      } catch (err) {
        setPending(false);
        throw err;
      }
    },
    [engine, store]
  );

  return { executeDarkMatter, pending };
}
