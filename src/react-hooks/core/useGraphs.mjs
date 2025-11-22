/**
 * @fileoverview useGraphs - React hook for RDF graph navigation
 * @module react-hooks/core/useGraphs
 *
 * @description
 * Hook for listing, selecting, and navigating between RDF named graphs.
 * Provides reactive graph management with metadata tracking.
 *
 * @example
 * ```jsx
 * import { useGraphs } from 'unrdf/react-hooks';
 *
 * function GraphSelector() {
 *   const { graphs, selectGraph, selectedGraph, graphStats } = useGraphs();
 *
 *   return (
 *     <div>
 *       <h3>Available Graphs</h3>
 *       {graphs.map(graph => (
 *         <button key={graph} onClick={() => selectGraph(graph)}>
 *           {graph} ({graphStats[graph]?.size || 0} triples)
 *         </button>
 *       ))}
 *       <p>Selected: {selectedGraph}</p>
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useEffect, useCallback, useMemo } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';
import { DataFactory } from 'n3';

const { defaultGraph, namedNode } = DataFactory;

/**
 * Hook for RDF graph navigation and management
 *
 * @param {Object} [options] - Hook options
 * @param {string} [options.defaultGraph] - Default graph to select
 * @returns {Object} Graph operations and state
 */
export function useGraphs(options = {}) {
  const { defaultGraph: initialGraph = null } = options;
  const { store } = useKnowledgeEngineContext();

  const [selectedGraph, setSelectedGraph] = useState(initialGraph);
  const [graphStats, setGraphStats] = useState({});

  /**
   * Get all unique graph URIs
   */
  const graphs = useMemo(() => {
    if (!store) return [];

    const graphSet = new Set();
    const quads = store.getQuads();

    for (const quad of quads) {
      const graphValue = quad.graph?.value || 'default';
      graphSet.add(graphValue);
    }

    return Array.from(graphSet).sort();
  }, [store]);

  /**
   * Calculate statistics for each graph
   */
  const updateGraphStats = useCallback(() => {
    if (!store) return;

    const stats = {};

    for (const graphUri of graphs) {
      const graph = graphUri === 'default' ? defaultGraph() : namedNode(graphUri);
      const quads = store.getQuads(null, null, null, graph);

      stats[graphUri] = {
        size: quads.length,
        subjects: new Set(quads.map(q => q.subject.value)).size,
        predicates: new Set(quads.map(q => q.predicate.value)).size,
        objects: new Set(quads.map(q => q.object.value)).size,
      };
    }

    setGraphStats(stats);
  }, [store, graphs]);

  /**
   * Select a graph
   */
  const selectGraph = useCallback(graphUri => {
    setSelectedGraph(graphUri);
  }, []);

  /**
   * Get quads from selected graph
   */
  const getSelectedGraphQuads = useCallback(() => {
    if (!store || !selectedGraph) return [];

    const graph = selectedGraph === 'default' ? defaultGraph() : namedNode(selectedGraph);

    return store.getQuads(null, null, null, graph);
  }, [store, selectedGraph]);

  /**
   * Get statistics for selected graph
   */
  const selectedGraphStats = useMemo(() => {
    return selectedGraph ? graphStats[selectedGraph] || null : null;
  }, [selectedGraph, graphStats]);

  /**
   * Clear selected graph
   */
  const clearSelectedGraph = useCallback(() => {
    if (!store || !selectedGraph) return false;

    try {
      const graph = selectedGraph === 'default' ? defaultGraph() : namedNode(selectedGraph);

      store.removeMatches(null, null, null, graph);
      updateGraphStats();
      return true;
    } catch (error) {
      console.error('[useGraphs] Failed to clear graph:', error);
      return false;
    }
  }, [store, selectedGraph, updateGraphStats]);

  /**
   * Check if graph exists
   */
  const hasGraph = useCallback(
    graphUri => {
      return graphs.includes(graphUri);
    },
    [graphs]
  );

  // Update stats when graphs change
  useEffect(() => {
    updateGraphStats();
  }, [updateGraphStats]);

  return {
    graphs,
    selectedGraph,
    selectGraph,
    graphStats,
    selectedGraphStats,
    getSelectedGraphQuads,
    clearSelectedGraph,
    hasGraph,
    refreshStats: updateGraphStats,
  };
}
