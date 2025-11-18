/**
 * @fileoverview useKnowledgeEffect - Effect hook with graph lifecycle
 * @module react-hooks/effects/useKnowledgeEffect
 */

import { useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

export function useKnowledgeEffect(effect, dependencies = []) {
  const { store, engine } = useKnowledgeEngineContext();
  const cleanupRef = useRef(null);

  useEffect(() => {
    if (!engine || !store) return;

    const cleanup = effect({ store, engine });
    if (typeof cleanup === 'function') {
      cleanupRef.current = cleanup;
    }

    return () => {
      if (cleanupRef.current) {
        cleanupRef.current();
        cleanupRef.current = null;
      }
    };
  }, [store, engine, ...dependencies]);
}
