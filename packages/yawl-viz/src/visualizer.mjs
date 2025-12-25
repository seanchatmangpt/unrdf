/**
 * @file YAWL Visualizer - Real-time D3.js workflow visualization
 * @module @unrdf/yawl-viz
 *
 * @description
 * Interactive D3.js-based visualization for YAWL workflows with:
 * - Live updates from YAWL event stream
 * - Van der Aalst pattern visual encoding
 * - Hierarchical graph layout with force simulation
 * - Interactive drill-down into case instances
 * - Real-time state highlighting
 *
 * @example
 * ```javascript
 * import { YAWLVisualizer } from '@unrdf/yawl-viz';
 * import { createWorkflowEngine } from '@unrdf/yawl';
 *
 * const engine = createWorkflowEngine();
 * const viz = new YAWLVisualizer({
 *   engine,
 *   container: document.getElementById('workflow-canvas'),
 *   width: 1200,
 *   height: 800,
 * });
 *
 * // Subscribe to live updates
 * viz.start();
 *
 * // Render specific workflow
 * viz.renderWorkflow('expense-approval');
 * ```
 */

import * as d3 from 'd3';

// =============================================================================
// Pattern Visual Styles
// =============================================================================

/**
 * Visual encoding for Van der Aalst workflow patterns
 * Each pattern has distinct color, shape, and border styling
 */
export const PATTERN_STYLES = {
  // WP1: Sequence
  SEQUENCE: {
    fill: '#4A90E2',
    stroke: '#2E5C8A',
    shape: 'rect',
    label: 'Seq',
    strokeWidth: 2,
  },
  // WP2: Parallel Split (AND-split)
  PARALLEL_SPLIT: {
    fill: '#7ED321',
    stroke: '#5A9C18',
    shape: 'diamond',
    label: 'AND',
    strokeWidth: 3,
    strokeDasharray: '5,0',
  },
  // WP3: Synchronization (AND-join)
  SYNCHRONIZATION: {
    fill: '#7ED321',
    stroke: '#5A9C18',
    shape: 'diamond',
    label: 'Sync',
    strokeWidth: 3,
    strokeDasharray: '0',
  },
  // WP4: Exclusive Choice (XOR-split)
  EXCLUSIVE_CHOICE: {
    fill: '#F5A623',
    stroke: '#C77D1A',
    shape: 'diamond',
    label: 'XOR',
    strokeWidth: 2,
  },
  // WP5: Simple Merge (XOR-join)
  SIMPLE_MERGE: {
    fill: '#F5A623',
    stroke: '#C77D1A',
    shape: 'diamond',
    label: 'Merge',
    strokeWidth: 2,
  },
  // WP6: Multi-Choice (OR-split)
  MULTI_CHOICE: {
    fill: '#BD10E0',
    stroke: '#8B0CA8',
    shape: 'diamond',
    label: 'OR',
    strokeWidth: 2,
    strokeDasharray: '5,5',
  },
  // WP7: Structured Sync Merge (OR-join)
  STRUCTURED_SYNC_MERGE: {
    fill: '#BD10E0',
    stroke: '#8B0CA8',
    shape: 'diamond',
    label: 'OR-Sync',
    strokeWidth: 2,
  },
  // Default task
  DEFAULT: {
    fill: '#4A90E2',
    stroke: '#2E5C8A',
    shape: 'rect',
    label: '',
    strokeWidth: 2,
  },
};

/**
 * Task state colors for real-time highlighting
 */
export const STATE_COLORS = {
  ENABLED: '#FFF59D',    // Yellow
  RUNNING: '#81C784',    // Green
  COMPLETED: '#A5D6A7',  // Light green
  CANCELLED: '#EF5350',  // Red
  FAILED: '#D32F2F',     // Dark red
  IDLE: '#E0E0E0',       // Gray
};

// =============================================================================
// YAWLVisualizer Class
// =============================================================================

/**
 * Real-time D3.js visualizer for YAWL workflows
 *
 * Features:
 * - Live event stream subscription
 * - Force-directed graph layout
 * - Van der Aalst pattern visual encoding
 * - Interactive zoom and pan
 * - Case instance drill-down
 *
 * @class
 */
export class YAWLVisualizer {
  /**
   * Create a new YAWL visualizer
   *
   * @param {Object} config - Configuration
   * @param {WorkflowEngine} config.engine - YAWL engine instance
   * @param {HTMLElement} config.container - DOM container element
   * @param {number} [config.width=1200] - Canvas width
   * @param {number} [config.height=800] - Canvas height
   * @param {boolean} [config.autoSubscribe=true] - Auto-subscribe to engine events
   */
  constructor(config) {
    this.engine = config.engine;
    this.container = config.container;
    this.width = config.width || 1200;
    this.height = config.height || 800;
    this.autoSubscribe = config.autoSubscribe !== false;

    // Visualization state
    this.currentWorkflow = null;
    this.currentCase = null;
    this.nodes = [];
    this.links = [];
    this.taskStates = new Map(); // taskId -> state

    // D3 elements
    this.svg = null;
    this.g = null;
    this.simulation = null;
    this.zoom = null;

    // Event handlers storage
    this.eventHandlers = [];

    // Initialize visualization
    this._initSVG();

    if (this.autoSubscribe) {
      this.subscribeToEvents();
    }
  }

  // ===========================================================================
  // Initialization
  // ===========================================================================

  /**
   * Initialize SVG canvas with zoom/pan
   * @private
   */
  _initSVG() {
    // Clear container
    this.container.innerHTML = '';

    // Create SVG
    this.svg = d3.select(this.container)
      .append('svg')
      .attr('width', this.width)
      .attr('height', this.height)
      .style('border', '1px solid #ccc')
      .style('background', '#fafafa');

    // Add zoom behavior
    this.zoom = d3.zoom()
      .scaleExtent([0.1, 4])
      .on('zoom', (event) => {
        this.g.attr('transform', event.transform);
      });

    this.svg.call(this.zoom);

    // Create main group for graph
    this.g = this.svg.append('g');

    // Add defs for markers (arrowheads)
    const defs = this.svg.append('defs');
    defs.append('marker')
      .attr('id', 'arrowhead')
      .attr('viewBox', '-0 -5 10 10')
      .attr('refX', 25)
      .attr('refY', 0)
      .attr('orient', 'auto')
      .attr('markerWidth', 6)
      .attr('markerHeight', 6)
      .append('path')
      .attr('d', 'M 0,-5 L 10,0 L 0,5')
      .attr('fill', '#666')
      .style('stroke', 'none');
  }

  // ===========================================================================
  // Event Subscription
  // ===========================================================================

  /**
   * Subscribe to YAWL engine events for live updates
   *
   * @returns {Function} Unsubscribe function
   */
  subscribeToEvents() {
    const eventTypes = [
      'task:enabled',
      'task:started',
      'task:completed',
      'task:cancelled',
      'task:failed',
      'case:created',
      'case:started',
      'case:completed',
      'workflow:registered',
    ];

    // Subscribe to all relevant events
    for (const eventType of eventTypes) {
      const unsubscribe = this.engine.on(eventType, (event) => {
        this._handleEvent(event);
      });
      this.eventHandlers.push(unsubscribe);
    }

    return () => this.unsubscribeFromEvents();
  }

  /**
   * Unsubscribe from all engine events
   */
  unsubscribeFromEvents() {
    for (const unsubscribe of this.eventHandlers) {
      unsubscribe();
    }
    this.eventHandlers = [];
  }

  /**
   * Handle incoming engine event
   * @private
   */
  _handleEvent(event) {
    const { type, caseId, taskId, workItemId } = event;

    // Update task state based on event
    switch (type) {
      case 'task:enabled':
        this.taskStates.set(workItemId || taskId, 'ENABLED');
        break;
      case 'task:started':
        this.taskStates.set(workItemId || taskId, 'RUNNING');
        break;
      case 'task:completed':
        this.taskStates.set(workItemId || taskId, 'COMPLETED');
        break;
      case 'task:cancelled':
        this.taskStates.set(workItemId || taskId, 'CANCELLED');
        break;
      case 'task:failed':
        this.taskStates.set(workItemId || taskId, 'FAILED');
        break;
      case 'case:created':
      case 'case:started':
        this.currentCase = caseId;
        break;
    }

    // Re-render to reflect state changes
    if (this.currentWorkflow && (caseId === this.currentCase || type.startsWith('workflow:'))) {
      this._updateVisualization();
    }
  }

  // ===========================================================================
  // Workflow Rendering
  // ===========================================================================

  /**
   * Render a workflow as an interactive graph
   *
   * @param {string} workflowId - Workflow ID to render
   * @returns {Object} Graph data (nodes and links)
   *
   * @example
   * ```javascript
   * viz.renderWorkflow('expense-approval');
   * ```
   */
  renderWorkflow(workflowId) {
    const workflow = this.engine.workflows.get(workflowId);
    if (!workflow) {
      throw new Error(`Workflow ${workflowId} not found`);
    }

    this.currentWorkflow = workflow;

    // Convert workflow to graph data
    const graphData = this._workflowToGraph(workflow);
    this.nodes = graphData.nodes;
    this.links = graphData.links;

    // Render graph
    this._renderGraph();

    return graphData;
  }

  /**
   * Convert workflow definition to graph data structure
   * @private
   */
  _workflowToGraph(workflow) {
    const nodes = [];
    const links = [];
    const nodeMap = new Map();

    // Add start node
    nodes.push({
      id: 'start',
      label: 'Start',
      type: 'start',
      pattern: 'SEQUENCE',
      x: 100,
      y: this.height / 2,
    });
    nodeMap.set('start', nodes[nodes.length - 1]);

    // Add task nodes
    for (const [taskId, taskDef] of workflow.tasks.entries()) {
      const pattern = this._detectPattern(taskDef, workflow);
      const node = {
        id: taskId,
        label: taskDef.name || taskId,
        type: 'task',
        pattern,
        splitType: taskDef.splitType || 'sequence',
        joinType: taskDef.joinType || 'sequence',
        taskDef,
      };
      nodes.push(node);
      nodeMap.set(taskId, node);
    }

    // Add end node
    nodes.push({
      id: 'end',
      label: 'End',
      type: 'end',
      pattern: 'SEQUENCE',
      x: this.width - 100,
      y: this.height / 2,
    });
    nodeMap.set('end', nodes[nodes.length - 1]);

    // Add links from flows
    for (const [from, toSet] of workflow.flows.entries()) {
      for (const to of toSet) {
        links.push({
          source: from,
          target: to,
          id: `${from}-${to}`,
        });
      }
    }

    // Add start links
    if (workflow.startTaskId) {
      links.push({
        source: 'start',
        target: workflow.startTaskId,
        id: 'start-link',
      });
    }

    // Add end links
    for (const endTaskId of workflow.endTaskIds || []) {
      links.push({
        source: endTaskId,
        target: 'end',
        id: `${endTaskId}-end`,
      });
    }

    return { nodes, links };
  }

  /**
   * Detect Van der Aalst pattern for a task
   * @private
   */
  _detectPattern(taskDef, workflow) {
    const { splitType, joinType } = taskDef;

    // Determine pattern based on split/join types
    if (splitType === 'and' && joinType === 'sequence') {
      return 'PARALLEL_SPLIT';
    }
    if (splitType === 'sequence' && joinType === 'and') {
      return 'SYNCHRONIZATION';
    }
    if (splitType === 'xor' && joinType === 'sequence') {
      return 'EXCLUSIVE_CHOICE';
    }
    if (splitType === 'sequence' && joinType === 'xor') {
      return 'SIMPLE_MERGE';
    }
    if (splitType === 'or' && joinType === 'sequence') {
      return 'MULTI_CHOICE';
    }
    if (splitType === 'sequence' && joinType === 'or') {
      return 'STRUCTURED_SYNC_MERGE';
    }

    return 'SEQUENCE';
  }

  /**
   * Render graph using D3 force simulation
   * @private
   */
  _renderGraph() {
    // Clear existing graph
    this.g.selectAll('*').remove();

    // Initialize force simulation
    this.simulation = d3.forceSimulation(this.nodes)
      .force('link', d3.forceLink(this.links)
        .id(d => d.id)
        .distance(150))
      .force('charge', d3.forceManyBody().strength(-500))
      .force('center', d3.forceCenter(this.width / 2, this.height / 2))
      .force('collision', d3.forceCollide().radius(60));

    // Render links
    const link = this.g.append('g')
      .attr('class', 'links')
      .selectAll('line')
      .data(this.links)
      .enter()
      .append('line')
      .attr('stroke', '#999')
      .attr('stroke-width', 2)
      .attr('marker-end', 'url(#arrowhead)');

    // Render nodes
    const node = this.g.append('g')
      .attr('class', 'nodes')
      .selectAll('g')
      .data(this.nodes)
      .enter()
      .append('g')
      .attr('class', 'node')
      .call(this._dragBehavior());

    // Add shapes based on pattern
    node.each(function(d) {
      const group = d3.select(this);
      const style = PATTERN_STYLES[d.pattern] || PATTERN_STYLES.DEFAULT;

      if (d.type === 'start' || d.type === 'end') {
        // Start/end as circles
        group.append('circle')
          .attr('r', 20)
          .attr('fill', d.type === 'start' ? '#4CAF50' : '#F44336')
          .attr('stroke', '#333')
          .attr('stroke-width', 2);
      } else if (style.shape === 'diamond') {
        // Diamond for split/join patterns
        group.append('path')
          .attr('d', 'M 0,-30 L 30,0 L 0,30 L -30,0 Z')
          .attr('fill', style.fill)
          .attr('stroke', style.stroke)
          .attr('stroke-width', style.strokeWidth)
          .attr('stroke-dasharray', style.strokeDasharray || '0');
      } else {
        // Rectangle for sequence tasks
        group.append('rect')
          .attr('x', -40)
          .attr('y', -25)
          .attr('width', 80)
          .attr('height', 50)
          .attr('rx', 5)
          .attr('fill', style.fill)
          .attr('stroke', style.stroke)
          .attr('stroke-width', style.strokeWidth);
      }

      // Add label
      group.append('text')
        .attr('dy', 4)
        .attr('text-anchor', 'middle')
        .attr('fill', '#fff')
        .attr('font-size', '12px')
        .attr('font-weight', 'bold')
        .text(d.label);

      // Add pattern badge
      if (style.label && d.type === 'task') {
        group.append('text')
          .attr('dy', -35)
          .attr('text-anchor', 'middle')
          .attr('fill', style.stroke)
          .attr('font-size', '10px')
          .attr('font-weight', 'bold')
          .text(style.label);
      }
    });

    // Add tooltips
    node.append('title')
      .text(d => {
        if (d.type === 'task') {
          return `${d.label}\nPattern: ${d.pattern}\nSplit: ${d.splitType}\nJoin: ${d.joinType}`;
        }
        return d.label;
      });

    // Update positions on simulation tick
    this.simulation.on('tick', () => {
      link
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);

      node
        .attr('transform', d => `translate(${d.x},${d.y})`);
    });

    // Store references for updates
    this._linkSelection = link;
    this._nodeSelection = node;
  }

  /**
   * Update visualization based on current state
   * @private
   */
  _updateVisualization() {
    if (!this._nodeSelection) return;

    // Update node colors based on task states
    this._nodeSelection.each(function(d) {
      if (d.type !== 'task') return;

      const group = d3.select(this);
      const state = this.taskStates.get(d.id) || 'IDLE';
      const color = STATE_COLORS[state];

      // Update fill color
      group.select('rect, path')
        .transition()
        .duration(300)
        .attr('fill', color);
    }.bind(this));
  }

  /**
   * Create drag behavior for nodes
   * @private
   */
  _dragBehavior() {
    function dragstarted(event) {
      if (!event.active) this.simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }

    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }

    function dragended(event) {
      if (!event.active) this.simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    }

    return d3.drag()
      .on('start', dragstarted.bind(this))
      .on('drag', dragged.bind(this))
      .on('end', dragended.bind(this));
  }

  // ===========================================================================
  // Case Instance Visualization
  // ===========================================================================

  /**
   * Render a specific case instance on the workflow
   *
   * @param {string} caseId - Case ID to visualize
   *
   * @example
   * ```javascript
   * viz.renderCase('case-123');
   * ```
   */
  renderCase(caseId) {
    const yawlCase = this.engine.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    this.currentCase = caseId;

    // Update task states from case work items
    this.taskStates.clear();
    for (const [workItemId, task] of yawlCase.workItems.entries()) {
      this.taskStates.set(workItemId, task.status);
    }

    // Re-render to show case state
    if (this.currentWorkflow) {
      this._updateVisualization();
    } else {
      this.renderWorkflow(yawlCase.workflowId);
    }
  }

  // ===========================================================================
  // Lifecycle Methods
  // ===========================================================================

  /**
   * Start the visualizer (subscribe to events)
   */
  start() {
    if (this.eventHandlers.length === 0) {
      this.subscribeToEvents();
    }
  }

  /**
   * Stop the visualizer (unsubscribe from events)
   */
  stop() {
    this.unsubscribeFromEvents();
  }

  /**
   * Destroy the visualizer and clean up resources
   */
  destroy() {
    this.stop();

    if (this.simulation) {
      this.simulation.stop();
    }

    if (this.container) {
      this.container.innerHTML = '';
    }

    this.nodes = [];
    this.links = [];
    this.taskStates.clear();
  }

  /**
   * Reset zoom to fit all nodes
   */
  resetZoom() {
    this.svg.transition()
      .duration(750)
      .call(this.zoom.transform, d3.zoomIdentity);
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a new YAWL visualizer instance
 *
 * @param {Object} config - Configuration
 * @returns {YAWLVisualizer} Visualizer instance
 *
 * @example
 * ```javascript
 * import { createVisualizer } from '@unrdf/yawl-viz';
 *
 * const viz = createVisualizer({
 *   engine: myEngine,
 *   container: document.getElementById('canvas'),
 *   width: 1200,
 *   height: 800,
 * });
 *
 * viz.renderWorkflow('my-workflow');
 * ```
 */
export function createVisualizer(config) {
  return new YAWLVisualizer(config);
}

// =============================================================================
// Exports
// =============================================================================

export default {
  YAWLVisualizer,
  createVisualizer,
  PATTERN_STYLES,
  STATE_COLORS,
};
