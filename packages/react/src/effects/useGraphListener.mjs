/**
 * @fileoverview useGraphListener - Hook for listening to graph updates
 * @module react-hooks/effects/useGraphListener
 */

import { useEffect } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 *
 */
export function useGraphListener(eventType, handler) {
  const { engine } = useKnowledgeEngineContext();

  useEffect(() => {
    if (!engine || !engine.on) return;

    engine.on(eventType, handler);

    return () => {
      if (engine.off) {
        engine.off(eventType, handler);
      }
    };
  }, [engine, eventType, handler]);
}
