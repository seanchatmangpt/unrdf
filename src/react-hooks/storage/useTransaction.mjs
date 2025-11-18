/**
 * @fileoverview useTransaction - Hook for transaction management
 * @module react-hooks/storage/useTransaction
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

export function useTransaction() {
  const { engine, store, applyTransaction } = useKnowledgeEngineContext();
  const [pending, setPending] = useState(false);
  const [lastReceipt, setLastReceipt] = useState(null);

  const apply = useCallback(async (delta, options = {}) => {
    setPending(true);
    try {
      const result = await applyTransaction(delta, options);
      setLastReceipt(result.receipt);
      setPending(false);
      return result;
    } catch (err) {
      setPending(false);
      throw err;
    }
  }, [applyTransaction]);

  return { apply, pending, lastReceipt };
}
