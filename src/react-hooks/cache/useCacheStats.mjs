/**
 * @fileoverview useCacheStats - Hook for cache statistics
 * @module react-hooks/cache/useCacheStats
 */

import { useState, useEffect } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

export function useCacheStats() {
  const { engine } = useKnowledgeEngineContext();
  const [stats, setStats] = useState({});

  useEffect(() => {
    const update = () => {
      if (engine?.queryCache) {
        setStats(engine.queryCache.getStats());
      }
    };

    update();
    const interval = setInterval(update, 1000);
    return () => clearInterval(interval);
  }, [engine]);

  return stats;
}
