/**
 * @fileoverview useDeltaTracking - Hook for tracking store changes
 * @module react-hooks/effects/useDeltaTracking
 */

import { useState, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 *
 */
export function useDeltaTracking(callback) {
  const { store } = useKnowledgeEngineContext();
  const [deltas, setDeltas] = useState([]);
  const previousSizeRef = useRef(0);

  useEffect(() => {
    if (!store) return;

    const size = store.size;
    const delta = size - previousSizeRef.current;

    if (delta !== 0) {
      const change = { timestamp: Date.now(), delta, size };
      setDeltas((prev) => [...prev, change]);

      if (callback) callback(change);
    }

    previousSizeRef.current = size;
  }, [store, store?.size, callback]);

  return deltas;
}
