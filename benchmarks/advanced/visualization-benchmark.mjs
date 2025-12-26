/**
 * @file Visualization Performance Benchmarks
 * @module benchmarks/advanced/visualization-benchmark
 *
 * @description
 * Benchmarks for graph visualization operations:
 * - Render time for different graph sizes
 * - Update latency
 * - Memory per node
 * - Layout algorithm performance
 */

import { suite, randomString, randomInt } from '../framework.mjs';

// =============================================================================
// Graph Data Structures
// =============================================================================

class GraphNode {
  constructor(id, label, type = 'default') {
    this.id = id;
    this.label = label;
    this.type = type;
    this.x = Math.random() * 1000;
    this.y = Math.random() * 1000;
    this.edges = [];
  }

  addEdge(targetId, label = '') {
    this.edges.push({ target: targetId, label });
  }
}

class Graph {
  constructor() {
    this.nodes = new Map();
    this.edges = [];
  }

  addNode(id, label, type) {
    const node = new GraphNode(id, label, type);
    this.nodes.set(id, node);
    return node;
  }

  addEdge(sourceId, targetId, label = '') {
    const edge = { source: sourceId, target: targetId, label };
    this.edges.push(edge);

    const sourceNode = this.nodes.get(sourceId);
    if (sourceNode) {
      sourceNode.addEdge(targetId, label);
    }

    return edge;
  }

  getNode(id) {
    return this.nodes.get(id);
  }

  toJSON() {
    return {
      nodes: Array.from(this.nodes.values()).map(node => ({
        id: node.id,
        label: node.label,
        type: node.type,
        x: node.x,
        y: node.y
      })),
      edges: this.edges
    };
  }
}

// =============================================================================
// Layout Algorithms
// =============================================================================

/**
 * Simple force-directed layout (single iteration)
 * @param {Graph} graph - Graph to layout
 */
function forceDirectedLayout(graph) {
  const nodes = Array.from(graph.nodes.values());
  const k = 100; // Optimal distance
  const c = 0.01; // Cooling factor

  for (const node of nodes) {
    let fx = 0;
    let fy = 0;

    // Repulsive forces between all nodes
    for (const other of nodes) {
      if (node.id === other.id) continue;

      const dx = node.x - other.x;
      const dy = node.y - other.y;
      const dist = Math.sqrt(dx * dx + dy * dy) || 1;
      const force = (k * k) / dist;

      fx += (dx / dist) * force;
      fy += (dy / dist) * force;
    }

    // Attractive forces along edges
    for (const edge of node.edges) {
      const target = graph.getNode(edge.target);
      if (!target) continue;

      const dx = target.x - node.x;
      const dy = target.y - node.y;
      const dist = Math.sqrt(dx * dx + dy * dy) || 1;
      const force = (dist * dist) / k;

      fx += (dx / dist) * force;
      fy += (dy / dist) * force;
    }

    // Apply forces
    node.x += fx * c;
    node.y += fy * c;
  }
}

/**
 * Simple hierarchical layout
 * @param {Graph} graph - Graph to layout
 */
function hierarchicalLayout(graph) {
  const nodes = Array.from(graph.nodes.values());
  const layers = new Map();

  // Assign layers (simplified BFS)
  let currentLayer = 0;
  const visited = new Set();
  const queue = [nodes[0]?.id].filter(Boolean);

  while (queue.length > 0) {
    const nodeId = queue.shift();
    if (visited.has(nodeId)) continue;

    visited.add(nodeId);
    if (!layers.has(currentLayer)) {
      layers.set(currentLayer, []);
    }
    layers.get(currentLayer).push(nodeId);

    const node = graph.getNode(nodeId);
    if (node) {
      for (const edge of node.edges) {
        if (!visited.has(edge.target)) {
          queue.push(edge.target);
        }
      }
    }

    if (queue.length === 0 && visited.size < nodes.length) {
      currentLayer++;
      const unvisited = nodes.find(n => !visited.has(n.id));
      if (unvisited) queue.push(unvisited.id);
    }
  }

  // Position nodes
  let y = 0;
  for (const [layer, nodeIds] of layers.entries()) {
    const xSpacing = 1000 / (nodeIds.length + 1);
    nodeIds.forEach((nodeId, index) => {
      const node = graph.getNode(nodeId);
      if (node) {
        node.x = xSpacing * (index + 1);
        node.y = y;
      }
    });
    y += 100;
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate random graph
 * @param {number} nodeCount - Number of nodes
 * @param {number} edgeDensity - Edge density (0-1)
 * @returns {Graph} Generated graph
 */
function generateGraph(nodeCount, edgeDensity = 0.2) {
  const graph = new Graph();

  // Create nodes
  for (let i = 0; i < nodeCount; i++) {
    graph.addNode(
      `node_${i}`,
      `Node ${i}`,
      i % 3 === 0 ? 'workflow' : (i % 3 === 1 ? 'task' : 'case')
    );
  }

  // Create edges
  const maxEdges = nodeCount * (nodeCount - 1) / 2;
  const edgeCount = Math.floor(maxEdges * edgeDensity);

  for (let i = 0; i < edgeCount; i++) {
    const source = `node_${randomInt(0, nodeCount - 1)}`;
    const target = `node_${randomInt(0, nodeCount - 1)}`;
    if (source !== target) {
      graph.addEdge(source, target, `edge_${i}`);
    }
  }

  return graph;
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const visualizationBenchmarks = suite('Visualization Performance', {
  'create graph (100 nodes)': {
    fn: () => {
      return generateGraph(100, 0.2);
    },
    iterations: 1000,
    warmup: 100
  },

  'create graph (1000 nodes)': {
    fn: () => {
      return generateGraph(1000, 0.05);
    },
    iterations: 100,
    warmup: 10
  },

  'create graph (10000 nodes)': {
    fn: () => {
      return generateGraph(10000, 0.01);
    },
    iterations: 10,
    warmup: 2
  },

  'force-directed layout (100 nodes)': {
    setup: () => {
      const graph = generateGraph(100, 0.2);
      return { graph };
    },
    fn: function() {
      forceDirectedLayout(this.graph);
    },
    iterations: 1000,
    warmup: 100
  },

  'force-directed layout (1000 nodes)': {
    setup: () => {
      const graph = generateGraph(1000, 0.05);
      return { graph };
    },
    fn: function() {
      forceDirectedLayout(this.graph);
    },
    iterations: 100,
    warmup: 10
  },

  'hierarchical layout (100 nodes)': {
    setup: () => {
      const graph = generateGraph(100, 0.2);
      return { graph };
    },
    fn: function() {
      hierarchicalLayout(this.graph);
    },
    iterations: 1000,
    warmup: 100
  },

  'hierarchical layout (1000 nodes)': {
    setup: () => {
      const graph = generateGraph(1000, 0.05);
      return { graph };
    },
    fn: function() {
      hierarchicalLayout(this.graph);
    },
    iterations: 100,
    warmup: 10
  },

  'serialize graph to JSON (100 nodes)': {
    setup: () => {
      const graph = generateGraph(100, 0.2);
      return { graph };
    },
    fn: function() {
      return JSON.stringify(this.graph.toJSON());
    },
    iterations: 5000,
    warmup: 500
  },

  'serialize graph to JSON (1000 nodes)': {
    setup: () => {
      const graph = generateGraph(1000, 0.05);
      return { graph };
    },
    fn: function() {
      return JSON.stringify(this.graph.toJSON());
    },
    iterations: 500,
    warmup: 50
  },

  'add node to existing graph': {
    setup: () => {
      const graph = generateGraph(100, 0.2);
      return { graph, nodeId: 0 };
    },
    fn: function() {
      this.graph.addNode(`new_node_${this.nodeId}`, `New Node ${this.nodeId}`, 'default');
      this.nodeId++;
    },
    iterations: 10000,
    warmup: 1000
  },

  'add edge to existing graph': {
    setup: () => {
      const graph = generateGraph(100, 0.2);
      return { graph, edgeId: 0 };
    },
    fn: function() {
      const source = `node_${randomInt(0, 99)}`;
      const target = `node_${randomInt(0, 99)}`;
      this.graph.addEdge(source, target, `new_edge_${this.edgeId}`);
      this.edgeId++;
    },
    iterations: 10000,
    warmup: 1000
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await visualizationBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
