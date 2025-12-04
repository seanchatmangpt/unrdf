/**
 * @file use-graph-visualizer.mjs
 * @description React hook for graph visualization
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for visualizing RDF graphs
 *
 * @param {Object} config - Visualizer configuration
 * @returns {Object} Visualizer state and operations
 *
 * @example
 * const {
 *   visualize,
 *   nodes,
 *   edges,
 *   layout,
 *   setLayout
 * } = useGraphVisualizer({
 *   layout: 'force-directed'
 * });
 */
export function useGraphVisualizer(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [nodes, setNodes] = useState([]);
  const [edges, setEdges] = useState([]);
  const [layout, setLayout] = useState(config.layout || 'force-directed');
  const [loading, setLoading] = useState(false);

  const visualize = useCallback(
    async (sparql) => {
      try {
        setLoading(true);

        const result = await engine.query(sparql);

        const graphNodes = new Map();
        const graphEdges = [];

        result.forEach((binding) => {
          if (binding.s && binding.p && binding.o) {
            if (!graphNodes.has(binding.s.value)) {
              graphNodes.set(binding.s.value, {
                id: binding.s.value,
                label: binding.s.value.split('/').pop(),
                type: 'subject',
              });
            }

            if (!graphNodes.has(binding.o.value)) {
              graphNodes.set(binding.o.value, {
                id: binding.o.value,
                label: binding.o.value.split('/').pop(),
                type: 'object',
              });
            }

            graphEdges.push({
              source: binding.s.value,
              target: binding.o.value,
              label: binding.p.value.split('/').pop(),
              predicate: binding.p.value,
            });
          }
        });

        setNodes(Array.from(graphNodes.values()));
        setEdges(graphEdges);
        setLoading(false);

        return { nodes: Array.from(graphNodes.values()), edges: graphEdges };
      } catch (err) {
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  return { visualize, nodes, edges, layout, setLayout, loading };
}
