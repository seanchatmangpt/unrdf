/**
 * @fileoverview useAuditTrail - Hook for lockchain audit trail access
 * @module react-hooks/storage/useAuditTrail
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 *
 */
export function useAuditTrail() {
  const { engine } = useKnowledgeEngineContext();
  const [trail, setTrail] = useState([]);

  const fetch = useCallback(async () => {
    if (!engine?.lockchainWriter) return [];
    try {
      const entries = await engine.lockchainWriter.getAuditTrail();
      setTrail(entries);
      return entries;
    } catch (err) {
      console.error('[useAuditTrail] fetch failed:', err);
      return [];
    }
  }, [engine]);

  const verify = useCallback(
    async (entryId) => {
      if (!engine?.lockchainWriter) return false;
      try {
        return await engine.lockchainWriter.verify(entryId);
      } catch (err) {
        console.error('[useAuditTrail] verify failed:', err);
        return false;
      }
    },
    [engine]
  );

  return { trail, fetch, verify };
}
