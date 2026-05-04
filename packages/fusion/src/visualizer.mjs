/**
 * @file Unified Visualization System - Deterministic SVG/JSON output
 * @module @unrdf/fusion/visualizer
 *
 * @description
 * Server-side visualization generator with deterministic output:
 * - No DOM dependency (pure string generation)
 * - No timestamps or randomness in DETERMINISTIC mode
 * - Stable JSON key ordering
 * - Cryptographic hashing for verification
 *
 * @example
 * ```javascript
 * import { createVisualizer, serializeVisualization } from '@unrdf/fusion';
 *
 * const viz = await createVisualizer(engine);
 * const svg = viz.renderWorkflow(workflow);
 * const serialized = await serializeVisualization({ type: 'workflow', data: svg });
 * console.log(serialized.hash); // Deterministic hash
 * ```
 */

import { createHash } from 'node:crypto';

// =============================================================================
// Constants - Reuse from yawl-viz
// =============================================================================

/**
 * Visual encoding for Van der Aalst workflow patterns
 */
export const PATTERN_STYLES = {
  SEQUENCE: { fill: '#4A90E2', stroke: '#2E5C8A', shape: 'rect', label: 'Seq' },
  PARALLEL_SPLIT: { fill: '#7ED321', stroke: '#5A9C18', shape: 'diamond', label: 'AND' },
  SYNCHRONIZATION: { fill: '#7ED321', stroke: '#5A9C18', shape: 'diamond', label: 'Sync' },
  EXCLUSIVE_CHOICE: { fill: '#F5A623', stroke: '#C77D1A', shape: 'diamond', label: 'XOR' },
  SIMPLE_MERGE: { fill: '#F5A623', stroke: '#C77D1A', shape: 'diamond', label: 'Merge' },
  MULTI_CHOICE: { fill: '#BD10E0', stroke: '#8B0CA8', shape: 'diamond', label: 'OR' },
  STRUCTURED_SYNC_MERGE: { fill: '#BD10E0', stroke: '#8B0CA8', shape: 'diamond', label: 'OR-Sync' },
  DEFAULT: { fill: '#4A90E2', stroke: '#2E5C8A', shape: 'rect', label: '' },
};

/**
 * Task state colors
 */
export const STATE_COLORS = {
  ENABLED: '#FFF59D',
  RUNNING: '#81C784',
  COMPLETED: '#A5D6A7',
  CANCELLED: '#EF5350',
  FAILED: '#D32F2F',
  IDLE: '#E0E0E0',
};

// =============================================================================
// Layout Algorithm - Deterministic hierarchical layout
// =============================================================================

/**
 * Compute deterministic hierarchical layout for workflow graph
 * Uses topological sort + level assignment for reproducible positioning
 *
 * @param {Array} nodes - Graph nodes
 * @param {Array} links - Graph edges
 * @param {number} width - Canvas width
 * @param {number} height - Canvas height
 * @returns {Array} Nodes with x,y positions
 */
function computeHierarchicalLayout(nodes, links, width = 1200, height = 800) {
  const nodeMap = new Map(nodes.map((n) => [n.id, { ...n }]));

  // Build adjacency list
  const adjacency = new Map();
  const inDegree = new Map();

  for (const node of nodes) {
    adjacency.set(node.id, []);
    inDegree.set(node.id, 0);
  }

  for (const link of links) {
    const sourceId = typeof link.source === 'object' ? link.source.id : link.source;
    const targetId = typeof link.target === 'object' ? link.target.id : link.target;

    adjacency.get(sourceId).push(targetId);
    inDegree.set(targetId, inDegree.get(targetId) + 1);
  }

  // Topological sort with level assignment (Kahn's algorithm)
  const levels = new Map();
  const queue = [];

  // Start with nodes that have no incoming edges
  for (const [nodeId, degree] of inDegree) {
    if (degree === 0) {
      queue.push(nodeId);
      levels.set(nodeId, 0);
    }
  }

  // Process queue
  while (queue.length > 0) {
    const nodeId = queue.shift();
    const currentLevel = levels.get(nodeId);

    for (const neighbor of adjacency.get(nodeId)) {
      const newDegree = inDegree.get(neighbor) - 1;
      inDegree.set(neighbor, newDegree);

      const neighborLevel = levels.get(neighbor) ?? -1;
      levels.set(neighbor, Math.max(neighborLevel, currentLevel + 1));

      if (newDegree === 0) {
        queue.push(neighbor);
      }
    }
  }

  // Group nodes by level
  const levelGroups = new Map();
  for (const [nodeId, level] of levels) {
    if (!levelGroups.has(level)) {
      levelGroups.set(level, []);
    }
    levelGroups.get(level).push(nodeId);
  }

  // Assign positions
  const maxLevel = Math.max(...levels.values());
  const levelWidth = width / (maxLevel + 1);

  for (const [level, nodeIds] of levelGroups) {
    // Sort for determinism
    nodeIds.sort();

    const x = levelWidth * (level + 0.5);
    const nodeCount = nodeIds.length;
    const levelHeight = height / (nodeCount + 1);

    for (let i = 0; i < nodeIds.length; i++) {
      const nodeId = nodeIds[i];
      const y = levelHeight * (i + 1);

      const node = nodeMap.get(nodeId);
      node.x = Math.round(x);
      node.y = Math.round(y);
    }
  }

  return Array.from(nodeMap.values());
}

// =============================================================================
// SVG Generation - Pure string templates
// =============================================================================

/**
 * Generate SVG for a diamond shape (split/join patterns)
 */
function renderDiamond(x, y, size, style) {
  const half = size / 2;
  const path = `M ${x},${y - half} L ${x + half},${y} L ${x},${y + half} L ${x - half},${y} Z`;

  return `<path d="${path}" fill="${style.fill}" stroke="${style.stroke}" stroke-width="2" />`;
}

/**
 * Generate SVG for a rectangle (sequence tasks)
 */
function renderRect(x, y, width, height, style, rx = 5) {
  return `<rect x="${x - width / 2}" y="${y - height / 2}" width="${width}" height="${height}" rx="${rx}" fill="${style.fill}" stroke="${style.stroke}" stroke-width="2" />`;
}

/**
 * Generate SVG for a circle (start/end nodes)
 */
function renderCircle(x, y, radius, fill, stroke = '#333') {
  return `<circle cx="${x}" cy="${y}" r="${radius}" fill="${fill}" stroke="${stroke}" stroke-width="2" />`;
}

/**
 * Generate SVG text element
 */
function renderText(x, y, text, fill = '#fff', fontSize = 12) {
  // Escape XML special characters
  const escaped = text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  return `<text x="${x}" y="${y}" text-anchor="middle" fill="${fill}" font-size="${fontSize}" font-weight="bold" dominant-baseline="middle">${escaped}</text>`;
}

/**
 * Generate SVG for an edge/link
 */
function renderLink(x1, y1, x2, y2) {
  return `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="#999" stroke-width="2" marker-end="url(#arrowhead)" />`;
}

/**
 * Generate SVG arrowhead marker definition
 */
function renderArrowMarker() {
  return `
    <defs>
      <marker id="arrowhead" viewBox="0 -5 10 10" refX="25" refY="0" orient="auto" markerWidth="6" markerHeight="6">
        <path d="M 0,-5 L 10,0 L 0,5" fill="#666" stroke="none" />
      </marker>
    </defs>
  `;
}

// =============================================================================
// Visualizer API
// =============================================================================

/**
 * Create unified visualizer instance
 *
 * @param {Object} engine - Fusion engine or KGC store
 * @returns {Promise<Object>} Visualizer API
 */
export async function createVisualizer(engine) {
  const isDeterministic = process.env.DETERMINISTIC === '1';

  return {
    /**
     * Render workflow as SVG string
     *
     * @param {Object} workflow - Workflow definition with tasks and flows
     * @returns {string} SVG markup
     */
    renderWorkflow(workflow) {
      const width = 1200;
      const height = 800;

      // Convert workflow to graph
      const nodes = [];
      const links = [];

      // Add start node
      nodes.push({ id: 'start', label: 'Start', type: 'start' });

      // Add task nodes
      for (const [taskId, taskDef] of (workflow.tasks || new Map())) {
        nodes.push({
          id: taskId,
          label: taskDef.name || taskId,
          type: 'task',
          pattern: detectPattern(taskDef),
        });
      }

      // Add end node
      nodes.push({ id: 'end', label: 'End', type: 'end' });

      // Add links from flows
      for (const [from, toSet] of (workflow.flows || new Map())) {
        for (const to of toSet) {
          links.push({ source: from, target: to });
        }
      }

      // Add start/end links
      if (workflow.startTaskId) {
        links.push({ source: 'start', target: workflow.startTaskId });
      }
      for (const endTaskId of workflow.endTaskIds || []) {
        links.push({ source: endTaskId, target: 'end' });
      }

      // Compute layout
      const positionedNodes = computeHierarchicalLayout(nodes, links, width, height);

      // Generate SVG
      let svg = `<svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">\n`;
      svg += renderArrowMarker();

      // Render links first (background layer)
      for (const link of links) {
        const source = positionedNodes.find((n) => n.id === link.source);
        const target = positionedNodes.find((n) => n.id === link.target);
        if (source && target) {
          svg += renderLink(source.x, source.y, target.x, target.y) + '\n';
        }
      }

      // Render nodes
      for (const node of positionedNodes) {
        if (node.type === 'start') {
          svg += renderCircle(node.x, node.y, 20, '#4CAF50') + '\n';
          svg += renderText(node.x, node.y, node.label, '#fff', 10) + '\n';
        } else if (node.type === 'end') {
          svg += renderCircle(node.x, node.y, 20, '#F44336') + '\n';
          svg += renderText(node.x, node.y, node.label, '#fff', 10) + '\n';
        } else {
          const style = PATTERN_STYLES[node.pattern] || PATTERN_STYLES.DEFAULT;

          if (style.shape === 'diamond') {
            svg += renderDiamond(node.x, node.y, 60, style) + '\n';
          } else {
            svg += renderRect(node.x, node.y, 80, 50, style) + '\n';
          }

          svg += renderText(node.x, node.y, node.label) + '\n';

          // Add pattern badge
          if (style.label) {
            svg += renderText(node.x, node.y - 35, style.label, style.stroke, 10) + '\n';
          }
        }
      }

      svg += '</svg>';

      return svg;
    },

    /**
     * Render receipts as JSON + SVG timeline
     *
     * @param {Array} receipts - Receipt objects from prove()
     * @returns {Object} { json: {...}, svg: "..." }
     */
    renderReceipts(receipts) {
      // Sort receipts by timestamp for determinism
      const sorted = [...receipts].sort((a, b) => {
        const aTime = a.timestamp?.toString() || '';
        const bTime = b.timestamp?.toString() || '';
        return aTime.localeCompare(bTime);
      });

      // Generate JSON with stable key ordering
      const json = {
        count: sorted.length,
        receipts: sorted.map((r) => ({
          allocation: r.allocation,
          eventType: r.eventType,
          hookId: r.hookId,
          phase: r.phase,
          timestamp: isDeterministic ? '[DETERMINISTIC]' : r.timestamp,
          valid: r.valid,
        })),
      };

      // Generate SVG timeline
      const width = 1200;
      const height = 400;
      const padding = 100;
      const timelineY = height / 2;

      let svg = `<svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">\n`;

      // Timeline axis
      svg += `<line x1="${padding}" y1="${timelineY}" x2="${width - padding}" y2="${timelineY}" stroke="#333" stroke-width="2" />\n`;

      // Receipt markers
      const step = (width - 2 * padding) / (sorted.length + 1);
      for (let i = 0; i < sorted.length; i++) {
        const x = padding + step * (i + 1);
        const receipt = sorted[i];

        // Marker circle
        svg += renderCircle(x, timelineY, 8, '#4A90E2') + '\n';

        // Phase label
        svg += renderText(x, timelineY - 30, receipt.phase, '#333', 10) + '\n';
      }

      svg += '</svg>';

      return { json, svg };
    },

    /**
     * Render resource allocation as JSON chart data
     *
     * @param {Object} pools - Resource pool allocations
     * @returns {Object} Chart-ready JSON
     */
    renderAllocation(pools) {
      // Stable key ordering
      const sorted = Object.keys(pools).sort();

      return {
        type: 'bar-chart',
        data: sorted.map((key) => ({
          label: key,
          value: pools[key],
        })),
        total: sorted.reduce((sum, key) => sum + (pools[key] || 0), 0),
      };
    },

    /**
     * Render policy hooks as JSON + SVG DAG
     *
     * @param {Array} policies - Policy hook definitions
     * @returns {Object} { json: {...}, svg: "..." }
     */
    renderPolicy(policies) {
      // Sort policies by ID for determinism
      const sorted = [...policies].sort((a, b) => a.id.localeCompare(b.id));

      const json = {
        count: sorted.length,
        policies: sorted.map((p) => ({
          id: p.id,
          trigger: p.trigger,
          type: p.type || 'validation',
        })),
      };

      // Generate SVG DAG
      const width = 1200;
      const height = 600;
      const nodeWidth = 120;
      const nodeHeight = 60;
      const padding = 100;

      let svg = `<svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">\n`;

      // Render policy nodes in a grid
      const cols = Math.ceil(Math.sqrt(sorted.length));
      const stepX = (width - 2 * padding) / cols;
      const stepY = (height - 2 * padding) / Math.ceil(sorted.length / cols);

      for (let i = 0; i < sorted.length; i++) {
        const col = i % cols;
        const row = Math.floor(i / cols);
        const x = padding + stepX * (col + 0.5);
        const y = padding + stepY * (row + 0.5);

        const policy = sorted[i];
        const style = PATTERN_STYLES.EXCLUSIVE_CHOICE;

        svg += renderRect(x, y, nodeWidth, nodeHeight, style) + '\n';
        svg += renderText(x, y, policy.id, '#fff', 10) + '\n';
      }

      svg += '</svg>';

      return { json, svg };
    },
  };
}

/**
 * Serialize visualization to deterministic JSON with hash
 *
 * @param {Object} viz - Visualization object { type, data, ... }
 * @returns {Promise<Object>} { type, data, hash }
 */
export async function serializeVisualization(viz) {
  const isDeterministic = process.env.DETERMINISTIC === '1';

  // Sort keys for determinism
  const sortedViz = {
    data: viz.data,
    type: viz.type,
  };

  // Generate deterministic JSON string
  const json = JSON.stringify(sortedViz, null, 2);

  // Compute SHA-256 hash
  const hash = createHash('sha256').update(json).digest('hex');

  return {
    type: viz.type,
    data: viz.data,
    hash,
    timestamp: isDeterministic ? '[DETERMINISTIC]' : new Date().toISOString(),
  };
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Detect Van der Aalst pattern from task definition
 */
function detectPattern(taskDef) {
  const { splitType = 'sequence', joinType = 'sequence' } = taskDef;

  if (splitType === 'and' && joinType === 'sequence') return 'PARALLEL_SPLIT';
  if (splitType === 'sequence' && joinType === 'and') return 'SYNCHRONIZATION';
  if (splitType === 'xor' && joinType === 'sequence') return 'EXCLUSIVE_CHOICE';
  if (splitType === 'sequence' && joinType === 'xor') return 'SIMPLE_MERGE';
  if (splitType === 'or' && joinType === 'sequence') return 'MULTI_CHOICE';
  if (splitType === 'sequence' && joinType === 'or') return 'STRUCTURED_SYNC_MERGE';

  return 'SEQUENCE';
}

// =============================================================================
// Exports
// =============================================================================

export default {
  createVisualizer,
  serializeVisualization,
  PATTERN_STYLES,
  STATE_COLORS,
};
