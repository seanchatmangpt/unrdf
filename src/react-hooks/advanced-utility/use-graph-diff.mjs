/**
 * @file use-graph-diff.mjs
 * @description React hook for computing differences between RDF graphs
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for computing and visualizing differences between RDF graphs
 *
 * @param {Object} config - Graph diff configuration
 * @returns {Object} Graph diff state and operations
 *
 * @example
 * const {
 *   computeDiff,
 *   diff,
 *   added,
 *   removed,
 *   modified,
 *   applyPatch
 * } = useGraphDiff();
 */
export function useGraphDiff(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [diff, setDiff] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const computeDiff = useCallback(async (graph1Uri, graph2Uri) => {
    try {
      setLoading(true);

      const quads1 = await engine.match(null, null, null, graph1Uri);
      const quads2 = await engine.match(null, null, null, graph2Uri);

      const set1 = new Set(quads1.map(q => quadToString(q)));
      const set2 = new Set(quads2.map(q => quadToString(q)));

      const added = quads2.filter(q => !set1.has(quadToString(q)));
      const removed = quads1.filter(q => !set2.has(quadToString(q)));

      const diffResult = {
        added,
        removed,
        modified: [],
        stats: {
          addedCount: added.length,
          removedCount: removed.length,
          modifiedCount: 0
        }
      };

      setDiff(diffResult);
      setLoading(false);
      return diffResult;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine]);

  const applyPatch = useCallback(async (targetGraph, patch) => {
    try {
      setLoading(true);

      if (patch.removed?.length > 0) {
        await engine.delete(patch.removed);
      }

      if (patch.added?.length > 0) {
        await engine.insert(patch.added);
      }

      setLoading(false);
      return { success: true };
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine]);

  function quadToString(quad) {
    return `${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`;
  }

  return {
    computeDiff,
    applyPatch,
    diff,
    added: diff?.added || [],
    removed: diff?.removed || [],
    modified: diff?.modified || [],
    loading,
    error
  };
}
