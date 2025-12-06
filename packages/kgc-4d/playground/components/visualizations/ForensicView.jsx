'use client';

/**
 * ForensicView - Interactive Causal Cone DAG Visualization
 *
 * Visualizes the causal dependency chain for a target event using:
 * - @xyflow/react for interactive graph rendering
 * - elkjs for automatic left-to-right time-flow layout
 *
 * Node colors:
 * - Green: ACK (successful delta)
 * - Red: REJECT (rejected delta)
 * - Yellow: Pending
 * - Blue: SNAPSHOT event
 */

import { useCallback, useMemo, useEffect, useState } from 'react';
import { ReactFlow, Background, Controls, MiniMap } from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import ELK from 'elkjs/lib/elk.bundled.js';
import { Clock, GitBranch, AlertCircle } from 'lucide-react';

const elk = new ELK();

// ELK layout options for left-to-right time flow
const elkOptions = {
  'elk.algorithm': 'layered',
  'elk.direction': 'RIGHT',
  'elk.spacing.nodeNode': '80',
  'elk.layered.spacing.nodeNodeBetweenLayers': '100',
};

export function ForensicView({ causalCone, onNodeClick, selectedNodeId }) {
  const [nodes, setNodes] = useState([]);
  const [edges, setEdges] = useState([]);
  const [layouting, setLayouting] = useState(false);

  // Build graph from causal cone
  useEffect(() => {
    if (!causalCone || !causalCone.events || causalCone.events.length === 0) {
      setNodes([]);
      setEdges([]);
      return;
    }

    const buildGraph = async () => {
      setLayouting(true);

      // Create nodes
      const nodeData = causalCone.events.map((event) => {
        // Determine node color based on status
        let bgColor = '#3b82f6'; // blue-500 (default)
        let borderColor = '#60a5fa'; // blue-400
        if (event.type === 'SNAPSHOT') {
          bgColor = '#22c55e'; // green-500
          borderColor = '#4ade80';
        } else if (event.status === 'ACK') {
          bgColor = '#22c55e'; // green-500
          borderColor = '#4ade80';
        } else if (event.status === 'REJECT') {
          bgColor = '#ef4444'; // red-500
          borderColor = '#f87171';
        } else if (event.status === 'PENDING') {
          bgColor = '#f59e0b'; // amber-500
          borderColor = '#fbbf24';
        }

        // Highlight selected node
        if (event.id === selectedNodeId) {
          borderColor = '#818cf8'; // indigo-400
        }

        return {
          id: event.id,
          width: 180,
          height: 80,
          data: {
            label: (
              <div className="text-xs">
                <div className="font-semibold truncate">{event.type}</div>
                <div className="text-gray-300 truncate">{formatTime(event.t_ns)}</div>
                {event.payload?.entity && (
                  <div className="text-gray-400 truncate mt-1">{event.payload.entity}</div>
                )}
              </div>
            ),
            event,
          },
          style: {
            background: bgColor,
            color: 'white',
            border: `2px solid ${borderColor}`,
            borderRadius: '8px',
            padding: '10px',
            cursor: 'pointer',
          },
        };
      });

      // Create edges (happened-before relationships)
      const edgeData = [];
      for (let i = 0; i < causalCone.events.length; i++) {
        const event = causalCone.events[i];
        for (let j = i + 1; j < causalCone.events.length; j++) {
          const laterEvent = causalCone.events[j];

          // Check if event is a direct predecessor
          // (Simplified: connect if time ordering suggests dependency)
          if (BigInt(event.t_ns) < BigInt(laterEvent.t_ns)) {
            edgeData.push({
              id: `${event.id}-${laterEvent.id}`,
              source: event.id,
              target: laterEvent.id,
              type: 'smoothstep',
              animated: false,
              style: { stroke: '#64748b', strokeWidth: 2 },
            });

            // Only connect direct predecessors (break after first match for cleaner graph)
            break;
          }
        }
      }

      // Layout with ELK
      try {
        const graph = {
          id: 'root',
          layoutOptions: elkOptions,
          children: nodeData.map((n) => ({
            id: n.id,
            width: n.width,
            height: n.height,
          })),
          edges: edgeData.map((e) => ({
            id: e.id,
            sources: [e.source],
            targets: [e.target],
          })),
        };

        const layout = await elk.layout(graph);

        // Apply layout positions
        const layoutedNodes = nodeData.map((node) => {
          const elkNode = layout.children.find((n) => n.id === node.id);
          return {
            ...node,
            position: { x: elkNode?.x || 0, y: elkNode?.y || 0 },
          };
        });

        setNodes(layoutedNodes);
        setEdges(edgeData);
      } catch (error) {
        console.error('[ForensicView] Layout error:', error);
        // Fallback: simple vertical layout
        const fallbackNodes = nodeData.map((node, i) => ({
          ...node,
          position: { x: i * 200, y: 100 },
        }));
        setNodes(fallbackNodes);
        setEdges(edgeData);
      } finally {
        setLayouting(false);
      }
    };

    buildGraph();
  }, [causalCone, selectedNodeId]);

  const onNodeClickHandler = useCallback(
    (event, node) => {
      if (onNodeClick) {
        onNodeClick(node.data.event);
      }
    },
    [onNodeClick]
  );

  if (!causalCone || !causalCone.events || causalCone.events.length === 0) {
    return (
      <div className="bg-slate-900 border border-slate-700 rounded-lg p-8 text-center">
        <AlertCircle className="w-12 h-12 text-slate-600 mx-auto mb-4" />
        <div className="text-slate-400">No causal cone to display</div>
        <div className="text-sm text-slate-500 mt-2">
          Select an event to visualize its causal dependencies
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-2">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2 text-sm font-medium text-slate-300">
          <GitBranch className="w-4 h-4" />
          <span>Causal Cone ({causalCone.count} events)</span>
        </div>
        {causalCone.target && (
          <div className="flex items-center gap-2 text-xs text-slate-400">
            <Clock className="w-3 h-3" />
            <span>Target: {formatTime(causalCone.target.t_ns)}</span>
          </div>
        )}
      </div>

      {/* Graph */}
      <div className="bg-slate-900 border border-slate-700 rounded-lg overflow-hidden" style={{ height: '500px' }}>
        {layouting ? (
          <div className="flex items-center justify-center h-full">
            <div className="text-slate-400">Calculating layout...</div>
          </div>
        ) : (
          <ReactFlow
            nodes={nodes}
            edges={edges}
            onNodeClick={onNodeClickHandler}
            fitView
            minZoom={0.1}
            maxZoom={2}
            attributionPosition="bottom-right"
          >
            <Background color="#475569" gap={16} />
            <Controls />
            <MiniMap
              nodeColor={(node) => node.style?.background || '#3b82f6'}
              maskColor="rgba(0, 0, 0, 0.5)"
              style={{ background: '#1e293b' }}
            />
          </ReactFlow>
        )}
      </div>

      {/* Timeline Info */}
      {causalCone.earliestTime && causalCone.latestTime && (
        <div className="flex items-center justify-between text-xs text-slate-400">
          <div>Earliest: {formatTime(causalCone.earliestTime)}</div>
          <div>Latest: {formatTime(causalCone.latestTime)}</div>
        </div>
      )}
    </div>
  );
}

/**
 * Format BigInt timestamp for display
 */
function formatTime(time) {
  if (!time) return '—';
  const t = typeof time === 'bigint' ? time : BigInt(time);
  const ms = Number(t / 1000000n);
  const date = new Date(ms);
  return date.toISOString().slice(0, 19).replace('T', ' ');
}
