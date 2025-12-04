/**
 * @fileoverview useBatchOperations - Hook for batch query/mutation operations
 * @module react-hooks/batch/useBatchOperations
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 *
 */
export function useBatchOperations() {
  const { engine, store } = useKnowledgeEngineContext();
  const [pending, setPending] = useState(false);

  const executeBatch = useCallback(
    async (operations) => {
      setPending(true);
      try {
        const results = await Promise.all(
          operations.map((op) => engine.query(store, op.query, op.options))
        );
        setPending(false);
        return results;
      } catch (err) {
        setPending(false);
        throw err;
      }
    },
    [engine, store]
  );

  return { executeBatch, pending };
}
