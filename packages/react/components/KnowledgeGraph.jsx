'use client';

import { useEffect, useRef } from 'react';
import cytoscape from 'cytoscape';

export default function KnowledgeGraph({ data }) {
  const containerRef = useRef(null);
  const cyRef = useRef(null);

  useEffect(() => {
    if (!containerRef.current) return;

    // Define knowledge graph elements
    const elements = [
      // Nodes: Operators
      {
        data: { id: 'μ1', label: 'Validation', type: 'operator', entropy: 50 },
        classes: 'operator high-entropy',
      },
      {
        data: { id: 'μ2', label: 'Temporal', type: 'operator', entropy: 45.8 },
        classes: 'operator high-entropy',
      },
      {
        data: { id: 'μ3', label: 'Geographic', type: 'operator', entropy: 40.0 },
        classes: 'operator high-entropy',
      },
      {
        data: { id: 'μ4', label: 'Batch', type: 'operator', entropy: 32.9 },
        classes: 'operator medium-entropy',
      },
      {
        data: { id: 'μ5', label: 'Threshold', type: 'operator', entropy: 26.6 },
        classes: 'operator medium-entropy',
      },
      {
        data: { id: 'μ6', label: 'Cascading', type: 'operator', entropy: 20.4 },
        classes: 'operator low-entropy',
      },
      {
        data: { id: 'μ7', label: 'Complex', type: 'operator', entropy: 14.2 },
        classes: 'operator low-entropy',
      },
      {
        data: { id: 'μ8', label: 'Deterministic', type: 'operator', entropy: 8.0 },
        classes: 'operator very-low-entropy',
      },

      // Nodes: JTBD Scenarios
      {
        data: { id: 'jtbd1', label: 'Order Fulfillment', type: 'jtbd' },
        classes: 'jtbd',
      },
      {
        data: { id: 'jtbd2', label: 'Recurring Purchase', type: 'jtbd' },
        classes: 'jtbd',
      },
      {
        data: { id: 'jtbd3', label: 'Listing Compliance', type: 'jtbd' },
        classes: 'jtbd',
      },
      {
        data: { id: 'jtbd5', label: 'Address Validation', type: 'jtbd' },
        classes: 'jtbd',
      },
      {
        data: { id: 'jtbd6', label: 'Bulk Updates', type: 'jtbd' },
        classes: 'jtbd',
      },

      // Nodes: Trigger Types
      {
        data: { id: 'before-add', label: 'before-add', type: 'trigger' },
        classes: 'trigger',
      },
      {
        data: { id: 'after-add', label: 'after-add', type: 'trigger' },
        classes: 'trigger',
      },
      {
        data: { id: 'on-interval', label: 'on-interval', type: 'trigger' },
        classes: 'trigger',
      },

      // Edges: Operator chain (information flow)
      { data: { source: 'μ1', target: 'μ2', type: 'flow', weight: 4.2 } },
      { data: { source: 'μ2', target: 'μ3', type: 'flow', weight: 5.8 } },
      { data: { source: 'μ3', target: 'μ4', type: 'flow', weight: 7.1 } },
      { data: { source: 'μ4', target: 'μ5', type: 'flow', weight: 6.3 } },
      { data: { source: 'μ5', target: 'μ6', type: 'flow', weight: 5.9 } },
      { data: { source: 'μ6', target: 'μ7', type: 'flow', weight: 6.2 } },
      { data: { source: 'μ7', target: 'μ8', type: 'flow', weight: 5.4 } },

      // Edges: JTBD to operators
      { data: { source: 'jtbd1', target: 'μ1', type: 'uses' } },
      { data: { source: 'jtbd2', target: 'μ2', type: 'uses' } },
      { data: { source: 'jtbd3', target: 'μ1', type: 'uses' } },
      { data: { source: 'jtbd5', target: 'μ3', type: 'uses' } },
      { data: { source: 'jtbd6', target: 'μ4', type: 'uses' } },

      // Edges: Triggers to operators
      { data: { source: 'before-add', target: 'μ1', type: 'triggers' } },
      { data: { source: 'before-add', target: 'μ3', type: 'triggers' } },
      { data: { source: 'after-add', target: 'μ7', type: 'triggers' } },
      { data: { source: 'on-interval', target: 'μ2', type: 'triggers' } },
    ];

    // Initialize Cytoscape
    const cy = cytoscape({
      container: containerRef.current,
      elements,
      style: [
        {
          selector: 'node',
          style: {
            'background-color': '#1e293b',
            'border-color': '#cbd5e1',
            'border-width': 2,
            'text-valign': 'center',
            'text-halign': 'center',
            'font-size': 12,
            color: '#e2e8f0',
            padding: '8px',
            'min-zoomed-font-size': 8,
          },
        },
        {
          selector: 'node.operator',
          style: {
            shape: 'round-rectangle',
            width: 70,
            height: 50,
            'font-weight': 'bold',
            color: '#06b6d4',
          },
        },
        {
          selector: 'node.operator.high-entropy',
          style: {
            'background-color': '#ea580c',
            'border-color': '#f97316',
            color: '#ffffff',
          },
        },
        {
          selector: 'node.operator.medium-entropy',
          style: {
            'background-color': '#d97706',
            'border-color': '#f59e0b',
            color: '#ffffff',
          },
        },
        {
          selector: 'node.operator.low-entropy',
          style: {
            'background-color': '#dc2626',
            'border-color': '#ef4444',
            color: '#ffffff',
          },
        },
        {
          selector: 'node.operator.very-low-entropy',
          style: {
            'background-color': '#7c3aed',
            'border-color': '#a78bfa',
            color: '#ffffff',
          },
        },
        {
          selector: 'node.jtbd',
          style: {
            shape: 'square',
            width: 60,
            height: 60,
            'background-color': '#0369a1',
            'border-color': '#0ea5e9',
            color: '#e0f2fe',
            'font-size': 10,
          },
        },
        {
          selector: 'node.trigger',
          style: {
            shape: 'diamond',
            width: 50,
            height: 50,
            'background-color': '#059669',
            'border-color': '#10b981',
            color: '#d1fae5',
            'font-size': 9,
          },
        },
        {
          selector: 'edge',
          style: {
            'line-color': '#64748b',
            'target-arrow-color': '#64748b',
            'target-arrow-shape': 'triangle',
            'arrow-scale': 1.2,
            'curve-style': 'bezier',
            width: 1.5,
          },
        },
        {
          selector: 'edge[type="flow"]',
          style: {
            'line-color': '#a855f7',
            'target-arrow-color': '#a855f7',
            width: 2,
          },
        },
        {
          selector: 'edge[type="uses"]',
          style: {
            'line-color': '#0ea5e9',
            'target-arrow-color': '#0ea5e9',
            width: 1.5,
            'line-style': 'dashed',
          },
        },
        {
          selector: 'edge[type="triggers"]',
          style: {
            'line-color': '#10b981',
            'target-arrow-color': '#10b981',
            width: 1.5,
            'line-style': 'dotted',
          },
        },
        {
          selector: ':selected',
          style: {
            'background-color': '#06b6d4',
            'border-color': '#00d9ff',
            'border-width': 3,
            'line-color': '#06b6d4',
            'target-arrow-color': '#06b6d4',
          },
        },
        {
          selector: ':hover',
          style: {
            'background-color': '#22d3ee',
            'border-color': '#06f',
            'border-width': 3,
          },
        },
      ],
      layout: {
        name: 'cose',
        directed: true,
        roots: '#μ1',
        animate: true,
        animationDuration: 500,
        avoidOverlap: true,
        nodeSpacing: 10,
        gravity: -30,
        cooling: 0.99,
        coolingFactor: 0.999,
        initialTemp: 1000,
        nestingFactor: 1.2,
        repulsiveStrength: 100,
        attractiveStrength: 1,
        edgeElasticity: 100,
        randomize: false,
      },
    });

    cyRef.current = cy;

    // Add interaction handlers
    cy.on('tap', 'node', (evt) => {
      const node = evt.target;
      console.log('Selected:', node.data());
    });

    cy.on('mouseover', 'node', (evt) => {
      evt.target.style('overlay-opacity', 0.2);
    });

    cy.on('mouseout', 'node', (evt) => {
      evt.target.style('overlay-opacity', 0);
    });

    // Auto-fit on load
    cy.fit();

    return () => {
      cy.destroy();
    };
  }, [data]);

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h3 className="text-lg font-semibold text-cyan-400">Knowledge Hook Graph</h3>
        <p className="text-sm text-slate-400">
          Interactive visualization of μ(O) operator chains, JTBD scenarios, and trigger mappings.
          Purple edges show information flow; blue dashed edges show JTBD usage; green dotted edges
          show trigger associations.
        </p>
      </div>

      <div
        className="card h-96 rounded border border-slate-700 overflow-hidden bg-slate-950"
        ref={containerRef}
      >
        {/* Graph renders here */}
      </div>

      {/* Legend */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="bg-slate-900/50 border border-slate-700 rounded p-4 space-y-3">
          <div className="font-semibold text-cyan-400">Operators (Nodes)</div>
          <div className="space-y-2 text-xs text-slate-300">
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-orange-500 rounded"></div>
              <span>High Entropy (μ₁-μ₄): 50 → 32.9 nats</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-amber-600 rounded"></div>
              <span>Medium Entropy (μ₅): 26.6 nats</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-red-600 rounded"></div>
              <span>Low Entropy (μ₆-μ₇): 20.4 → 14.2 nats</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-purple-600 rounded"></div>
              <span>Deterministic (μ₈): 8.0 nats</span>
            </div>
          </div>
        </div>

        <div className="bg-slate-900/50 border border-slate-700 rounded p-4 space-y-3">
          <div className="font-semibold text-cyan-400">JTBD Scenarios</div>
          <div className="space-y-2 text-xs text-slate-300">
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-cyan-600 rounded"></div>
              <span>Job To Be Done scenarios</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-4 h-4 bg-green-600 rounded"></div>
              <span>Hook Triggers (before-add, after-add, on-interval)</span>
            </div>
          </div>
        </div>

        <div className="bg-slate-900/50 border border-slate-700 rounded p-4 space-y-3">
          <div className="font-semibold text-cyan-400">Edge Types</div>
          <div className="space-y-2 text-xs text-slate-300">
            <div className="flex items-center gap-2">
              <div className="w-6 h-0.5 bg-purple-500"></div>
              <span>Information Flow</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-6 h-0.5 bg-cyan-500" style={{ borderTop: '1px dashed' }}></div>
              <span>JTBD Usage</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-6 h-0.5 bg-green-500" style={{ borderTop: '1px dotted' }}></div>
              <span>Trigger Association</span>
            </div>
          </div>
        </div>
      </div>

      {/* Insights */}
      <div className="bg-slate-800/50 rounded-lg p-4 space-y-3">
        <div className="font-semibold text-cyan-400">Graph Insights</div>
        <ul className="space-y-2 text-sm text-slate-300">
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1 flex-shrink-0"></span>
            <span>
              Operators form a deterministic chain: user intent (50 nats) → observable outcome (1
              nat)
            </span>
          </li>
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-purple-400 mt-1 flex-shrink-0"></span>
            <span>5 JTBD scenarios depend on validators (μ₁, μ₃) and batch processor (μ₄)</span>
          </li>
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1 flex-shrink-0"></span>
            <span>
              Three hook triggers activate different operators: before-add, after-add, on-interval
            </span>
          </li>
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-orange-400 mt-1 flex-shrink-0"></span>
            <span>Click nodes to inspect details; use mouse to pan and zoom</span>
          </li>
        </ul>
      </div>
    </div>
  );
}
