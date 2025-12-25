/**
 * Real-Time Workflow Visualization
 * Live workflow visualization using D3.js and Observable Plot
 *
 * @module @unrdf/yawl/visualization/live-workflow-viz
 * @description
 * Integrates D3.js and Observable Plot for real-time visualization of
 * YAWL workflow execution. Subscribes to YAWL events and renders live
 * state transitions, task progress, and control flow.
 */

import { z } from 'zod';
import * as d3 from 'd3';
import * as Plot from '@observablehq/plot';
import { YAWL_EVENT_TYPES } from '../events/yawl-events.mjs';

// =============================================================================
// Configuration Schemas
// =============================================================================

/**
 * Visualization configuration schema
 */
export const VisualizationConfigSchema = z.object({
  /** Container element selector or DOM element */
  container: z.union([z.string(), z.any()]),
  /** Visualization width in pixels */
  width: z.number().int().positive().default(800),
  /** Visualization height in pixels */
  height: z.number().int().positive().default(600),
  /** Enable auto-refresh */
  autoRefresh: z.boolean().default(true),
  /** Refresh interval in milliseconds */
  refreshInterval: z.number().int().positive().default(1000),
  /** Color scheme for visualization */
  colorScheme: z.enum(['light', 'dark', 'custom']).default('light'),
  /** Custom colors */
  colors: z.object({
    enabled: z.string().default('#4CAF50'),
    started: z.string().default('#2196F3'),
    completed: z.string().default('#8BC34A'),
    failed: z.string().default('#F44336'),
    cancelled: z.string().default('#9E9E9E'),
  }).optional(),
});

/**
 * Workflow node schema
 */
const WorkflowNodeSchema = z.object({
  id: z.string(),
  label: z.string(),
  type: z.enum(['task', 'condition', 'split', 'join']),
  status: z.enum(['pending', 'enabled', 'started', 'completed', 'failed', 'cancelled']),
  x: z.number().optional(),
  y: z.number().optional(),
});

/**
 * Workflow edge schema
 */
const WorkflowEdgeSchema = z.object({
  source: z.string(),
  target: z.string(),
  label: z.string().optional(),
  condition: z.string().optional(),
});

// =============================================================================
// Live Workflow Visualizer
// =============================================================================

/**
 * Creates a live workflow visualizer
 *
 * @param {Object} engine - YAWL engine instance
 * @param {Object} config - Visualization configuration
 * @returns {Object} Visualizer instance
 *
 * @example
 * const visualizer = createLiveWorkflowVisualizer(yawlEngine, {
 *   container: '#workflow-viz',
 *   width: 1200,
 *   height: 800,
 *   autoRefresh: true
 * });
 *
 * visualizer.start();
 */
export function createLiveWorkflowVisualizer(engine, config) {
  const validated = VisualizationConfigSchema.parse(config);

  // Get or create container element
  const containerElement = typeof validated.container === 'string'
    ? document.querySelector(validated.container)
    : validated.container;

  if (!containerElement) {
    throw new Error(`Container element not found: ${validated.container}`);
  }

  // State management
  let nodes = [];
  let edges = [];
  let eventHistory = [];
  let refreshTimer = null;
  let isRunning = false;

  // Color mapping based on status
  const getNodeColor = (status) => {
    const colors = validated.colors || {
      enabled: '#4CAF50',
      started: '#2196F3',
      completed: '#8BC34A',
      failed: '#F44336',
      cancelled: '#9E9E9E',
    };

    return colors[status] || '#757575';
  };

  /**
   * Initialize visualization
   */
  function initialize() {
    // Clear container
    d3.select(containerElement).selectAll('*').remove();

    // Create SVG canvas
    const svg = d3.select(containerElement)
      .append('svg')
      .attr('width', validated.width)
      .attr('height', validated.height)
      .attr('class', 'workflow-visualization');

    // Add groups for edges and nodes
    svg.append('g').attr('class', 'edges');
    svg.append('g').attr('class', 'nodes');

    // Add event timeline container
    const timeline = d3.select(containerElement)
      .append('div')
      .attr('class', 'event-timeline')
      .style('margin-top', '20px');

    return { svg, timeline };
  }

  /**
   * Update visualization with current workflow state
   */
  function render() {
    const svg = d3.select(containerElement).select('svg');

    // Render edges
    const edgeGroup = svg.select('.edges');
    const edgeSelection = edgeGroup.selectAll('line')
      .data(edges, (d) => `${d.source}-${d.target}`);

    edgeSelection.enter()
      .append('line')
      .attr('x1', (d) => {
        const source = nodes.find((n) => n.id === d.source);
        return source?.x || 0;
      })
      .attr('y1', (d) => {
        const source = nodes.find((n) => n.id === d.source);
        return source?.y || 0;
      })
      .attr('x2', (d) => {
        const target = nodes.find((n) => n.id === d.target);
        return target?.x || 0;
      })
      .attr('y2', (d) => {
        const target = nodes.find((n) => n.id === d.target);
        return target?.y || 0;
      })
      .attr('stroke', '#999')
      .attr('stroke-width', 2)
      .attr('marker-end', 'url(#arrowhead)');

    edgeSelection.exit().remove();

    // Render nodes
    const nodeGroup = svg.select('.nodes');
    const nodeSelection = nodeGroup.selectAll('g')
      .data(nodes, (d) => d.id);

    const nodeEnter = nodeSelection.enter()
      .append('g')
      .attr('transform', (d) => `translate(${d.x || 0},${d.y || 0})`);

    nodeEnter.append('circle')
      .attr('r', 30)
      .attr('fill', (d) => getNodeColor(d.status))
      .attr('stroke', '#333')
      .attr('stroke-width', 2);

    nodeEnter.append('text')
      .attr('text-anchor', 'middle')
      .attr('dy', '.35em')
      .attr('fill', '#fff')
      .text((d) => d.label);

    nodeSelection.select('circle')
      .transition()
      .duration(300)
      .attr('fill', (d) => getNodeColor(d.status));

    nodeSelection.exit().remove();

    // Render event timeline using Observable Plot
    renderEventTimeline();
  }

  /**
   * Render event timeline using Observable Plot
   */
  function renderEventTimeline() {
    const timeline = d3.select(containerElement).select('.event-timeline');
    timeline.selectAll('*').remove();

    if (eventHistory.length === 0) {
      return;
    }

    // Create Plot visualization
    const plot = Plot.plot({
      width: validated.width,
      height: 200,
      marginLeft: 60,
      x: {
        label: 'Time',
        type: 'time',
      },
      y: {
        label: 'Event Type',
      },
      marks: [
        Plot.dot(eventHistory, {
          x: 'timestamp',
          y: 'eventType',
          fill: 'eventType',
          r: 5,
          tip: true,
        }),
        Plot.ruleX(eventHistory, {
          x: 'timestamp',
          stroke: '#ddd',
          strokeWidth: 1,
        }),
      ],
    });

    timeline.node().appendChild(plot);
  }

  /**
   * Handle YAWL event and update visualization
   *
   * @param {Object} event - YAWL event
   */
  function handleEvent(event) {
    // Add to event history
    eventHistory.push({
      timestamp: new Date(event.timestamp || Date.now()),
      eventType: event.type,
      caseId: event.caseId,
      taskId: event.taskId,
    });

    // Limit history size
    if (eventHistory.length > 100) {
      eventHistory = eventHistory.slice(-100);
    }

    // Update node status based on event type
    if (event.type === YAWL_EVENT_TYPES.TASK_ENABLED) {
      const node = nodes.find((n) => n.id === event.taskId);
      if (node) {
        node.status = 'enabled';
      } else {
        nodes.push({
          id: event.taskId,
          label: event.taskId,
          type: 'task',
          status: 'enabled',
          x: Math.random() * (validated.width - 100) + 50,
          y: Math.random() * (validated.height - 100) + 50,
        });
      }
    } else if (event.type === YAWL_EVENT_TYPES.TASK_STARTED) {
      const node = nodes.find((n) => n.id === event.taskId);
      if (node) {
        node.status = 'started';
      }
    } else if (event.type === YAWL_EVENT_TYPES.TASK_COMPLETED) {
      const node = nodes.find((n) => n.id === event.taskId);
      if (node) {
        node.status = 'completed';
      }
    } else if (event.type === YAWL_EVENT_TYPES.TASK_CANCELLED) {
      const node = nodes.find((n) => n.id === event.taskId);
      if (node) {
        node.status = 'cancelled';
      }
    }

    // Re-render if not auto-refreshing
    if (!validated.autoRefresh) {
      render();
    }
  }

  /**
   * Start visualization and event listening
   */
  function start() {
    if (isRunning) {
      return;
    }

    isRunning = true;
    initialize();

    // Subscribe to YAWL events if engine supports it
    if (engine.on) {
      engine.on('event', handleEvent);
    }

    // Start auto-refresh if enabled
    if (validated.autoRefresh) {
      refreshTimer = setInterval(() => {
        render();
      }, validated.refreshInterval);
    }

    // Initial render
    render();
  }

  /**
   * Stop visualization and cleanup
   */
  function stop() {
    if (!isRunning) {
      return;
    }

    isRunning = false;

    // Unsubscribe from events
    if (engine.off) {
      engine.off('event', handleEvent);
    }

    // Stop auto-refresh
    if (refreshTimer) {
      clearInterval(refreshTimer);
      refreshTimer = null;
    }
  }

  /**
   * Manually trigger re-render
   */
  function refresh() {
    render();
  }

  /**
   * Export current visualization as SVG
   *
   * @returns {string} SVG string
   */
  function exportSVG() {
    const svg = d3.select(containerElement).select('svg');
    return svg.node().outerHTML;
  }

  /**
   * Get current visualization state
   *
   * @returns {Object} Current state
   */
  function getState() {
    return {
      nodes: [...nodes],
      edges: [...edges],
      eventHistory: [...eventHistory],
      isRunning,
    };
  }

  return {
    start,
    stop,
    refresh,
    exportSVG,
    getState,
    handleEvent,
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Create a static workflow diagram (non-live)
 *
 * @param {Object} workflowSpec - Workflow specification
 * @param {Object} config - Visualization configuration
 * @returns {Object} Static visualizer
 *
 * @example
 * const viz = createStaticWorkflowDiagram(spec, {
 *   container: '#diagram',
 *   width: 1000,
 *   height: 600
 * });
 */
export function createStaticWorkflowDiagram(workflowSpec, config) {
  const validated = VisualizationConfigSchema.parse(config);

  // Extract nodes and edges from workflow spec
  const nodes = workflowSpec.tasks?.map((task) => ({
    id: task.id,
    label: task.name || task.id,
    type: 'task',
    status: 'pending',
  })) || [];

  const edges = workflowSpec.flows?.map((flow) => ({
    source: flow.from,
    target: flow.to,
    label: flow.condition || '',
  })) || [];

  // Use D3 force simulation for layout
  const simulation = d3.forceSimulation(nodes)
    .force('link', d3.forceLink(edges).id((d) => d.id))
    .force('charge', d3.forceManyBody().strength(-300))
    .force('center', d3.forceCenter(validated.width / 2, validated.height / 2));

  simulation.on('end', () => {
    // Nodes now have x, y positions
  });

  return {
    nodes,
    edges,
    simulation,
  };
}
