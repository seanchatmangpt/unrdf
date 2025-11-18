/**
 * @fileoverview useDeltaQuery - React hook for delta/change tracking queries
 * @module react-hooks/query/useDeltaQuery
 *
 * @description
 * Hook for tracking changes in RDF data over time and querying deltas.
 *
 * @example
 * ```jsx
 * import { useDeltaQuery } from 'unrdf/react-hooks';
 *
 * function ChangeLog() {
 *   const { additions, removals, changes, reset } = useDeltaQuery();
 *
 *   return (
 *     <div>
 *       <h3>Changes</h3>
 *       <p>Added: {additions.length}</p>
 *       <p>Removed: {removals.length}</p>
 *       <button onClick={reset}>Reset</button>
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for delta/change tracking
 *
 * @param {Object} [options] - Delta options
 * @param {boolean} [options.trackChanges=true] - Enable change tracking
 * @param {number} [options.maxHistory=100] - Maximum history entries
 * @returns {Object} Delta state and operations
 */
export function useDeltaQuery(options = {}) {
  const { trackChanges = true, maxHistory = 100 } = options;
  const { store } = useKnowledgeEngineContext();

  const [additions, setAdditions] = useState([]);
  const [removals, setRemovals] = useState([]);
  const [changes, setChanges] = useState([]);

  const previousStoreRef = useRef(null);
  const historyRef = useRef([]);

  /**
   * Calculate delta between current and previous store
   */
  const calculateDelta = useCallback(() => {
    if (!store || !previousStoreRef.current) {
      return { additions: [], removals: [] };
    }

    const currentQuads = new Set(
      store.getQuads().map(q =>
        `${q.subject.value}|${q.predicate.value}|${q.object.value}|${q.graph?.value || ''}`
      )
    );

    const previousQuads = new Set(
      previousStoreRef.current.getQuads().map(q =>
        `${q.subject.value}|${q.predicate.value}|${q.object.value}|${q.graph?.value || ''}`
      )
    );

    const added = [];
    const removed = [];

    // Find additions
    for (const quad of store.getQuads()) {
      const key = `${quad.subject.value}|${quad.predicate.value}|${quad.object.value}|${quad.graph?.value || ''}`;
      if (!previousQuads.has(key)) {
        added.push(quad);
      }
    }

    // Find removals
    for (const quad of previousStoreRef.current.getQuads()) {
      const key = `${quad.subject.value}|${quad.predicate.value}|${quad.object.value}|${quad.graph?.value || ''}`;
      if (!currentQuads.has(key)) {
        removed.push(quad);
      }
    }

    return { additions: added, removals: removed };
  }, [store]);

  /**
   * Update delta state
   */
  const updateDelta = useCallback(() => {
    if (!trackChanges || !store) return;

    if (previousStoreRef.current) {
      const delta = calculateDelta();

      setAdditions(delta.additions);
      setRemovals(delta.removals);

      if (delta.additions.length > 0 || delta.removals.length > 0) {
        const change = {
          timestamp: Date.now(),
          additions: delta.additions,
          removals: delta.removals
        };

        setChanges(prev => {
          const updated = [...prev, change];
          // Limit history size
          return updated.slice(-maxHistory);
        });

        // Update history ref
        historyRef.current.push(change);
        if (historyRef.current.length > maxHistory) {
          historyRef.current = historyRef.current.slice(-maxHistory);
        }
      }
    }

    // Update previous store reference
    previousStoreRef.current = store;
  }, [trackChanges, store, calculateDelta, maxHistory]);

  /**
   * Reset delta tracking
   */
  const reset = useCallback(() => {
    setAdditions([]);
    setRemovals([]);
    setChanges([]);
    historyRef.current = [];
    previousStoreRef.current = store;
  }, [store]);

  /**
   * Get changes since timestamp
   */
  const getChangesSince = useCallback((timestamp) => {
    return changes.filter(change => change.timestamp >= timestamp);
  }, [changes]);

  /**
   * Get recent changes
   */
  const getRecentChanges = useCallback((count = 10) => {
    return changes.slice(-count);
  }, [changes]);

  // Track store changes
  useEffect(() => {
    if (trackChanges && store) {
      updateDelta();
    }
  }, [trackChanges, store, updateDelta]);

  return {
    additions,
    removals,
    changes,
    reset,
    getChangesSince,
    getRecentChanges,
    hasChanges: additions.length > 0 || removals.length > 0,
    changeCount: changes.length
  };
}
