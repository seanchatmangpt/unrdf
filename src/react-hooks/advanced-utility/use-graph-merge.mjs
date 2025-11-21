/**
 * @file use-graph-merge.mjs
 * @description React hook for merging multiple RDF graphs
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for merging multiple RDF graphs with conflict resolution
 *
 * @param {Object} config - Graph merge configuration
 * @param {Function} [config.conflictResolver] - Custom conflict resolution
 * @returns {Object} Graph merge state and operations
 *
 * @example
 * const {
 *   mergeGraphs,
 *   conflicts,
 *   resolveConflict,
 *   mergedGraph
 * } = useGraphMerge({
 *   conflictResolver: (conflicts) => conflicts[0] // Use first value
 * });
 */
export function useGraphMerge(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [mergedGraph, setMergedGraph] = useState(null);
  const [conflicts, setConflicts] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const mergeGraphs = useCallback(async (graphUris, targetGraph) => {
    try {
      setLoading(true);

      const allQuads = [];
      const quadMap = new Map();
      const detectedConflicts = [];

      for (const graphUri of graphUris) {
        const quads = await engine.match(null, null, null, graphUri);

        for (const quad of quads) {
          const key = `${quad.subject.value}|${quad.predicate.value}`;

          if (quadMap.has(key)) {
            const existing = quadMap.get(key);
            if (existing.object.value !== quad.object.value) {
              detectedConflicts.push({
                subject: quad.subject.value,
                predicate: quad.predicate.value,
                values: [existing.object.value, quad.object.value],
                graphs: [existing.graph, graphUri]
              });
            }
          } else {
            quadMap.set(key, { ...quad, graph: graphUri });
            allQuads.push(quad);
          }
        }
      }

      if (detectedConflicts.length > 0 && !config.conflictResolver) {
        setConflicts(detectedConflicts);
        setLoading(false);
        return { success: false, conflicts: detectedConflicts };
      }

      if (detectedConflicts.length > 0 && config.conflictResolver) {
        for (const conflict of detectedConflicts) {
          const resolved = config.conflictResolver(conflict.values);
          const quad = Array.from(quadMap.values()).find(
            q => q.subject.value === conflict.subject &&
                 q.predicate.value === conflict.predicate
          );
          if (quad) {
            quad.object = { value: resolved, termType: 'Literal' };
          }
        }
      }

      await engine.insert(allQuads);
      setMergedGraph(targetGraph);
      setLoading(false);
      return { success: true, quadCount: allQuads.length };
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, config]);

  const resolveConflict = useCallback((conflict, chosenValue) => {
    setConflicts(prev => prev.filter(c => c !== conflict));
    return { resolved: true, value: chosenValue };
  }, []);

  return { mergeGraphs, conflicts, resolveConflict, mergedGraph, loading, error };
}
