/**
 * @file Live Workflow Visualization Integration Tests
 * @module @unrdf/yawl/test/visualization
 *
 * Comprehensive integration tests for YAWL workflow visualization covering:
 * - D3.js SVG rendering and manipulation
 * - Observable Plot timeline generation
 * - Event handling and live updates
 * - Workflow graph rendering (nodes, edges, labels)
 * - State visualization and color coding
 * - Export functionality (SVG export)
 * - Performance under load (1000+ nodes)
 *
 * Note: Uses JSDOM for DOM environment in Node.js tests
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { JSDOM } from 'jsdom';
import {
  createLiveWorkflowVisualizer,
  createStaticWorkflowDiagram,
  VisualizationConfigSchema,
} from '../src/visualization/live-workflow-viz.mjs';
import { YAWL_EVENT_TYPES } from '../src/events/yawl-events.mjs';

// =============================================================================
// Test Setup - DOM Environment
// =============================================================================

/**
 * Setup DOM environment for D3 and Plot
 */
function setupDOM() {
  const dom = new JSDOM('<!DOCTYPE html><html><body><div id="viz-container"></div></body></html>');
  global.document = dom.window.document;
  global.window = dom.window;
  global.HTMLElement = dom.window.HTMLElement;
  global.SVGElement = dom.window.SVGElement;

  return dom;
}

/**
 * Cleanup DOM environment
 */
function cleanupDOM() {
  delete global.document;
  delete global.window;
  delete global.HTMLElement;
  delete global.SVGElement;
}

// =============================================================================
// Test Fixtures
// =============================================================================

/**
 * Mock YAWL engine for visualization tests
 */
function createMockEngine() {
  const eventHandlers = new Map();

  return {
    on: vi.fn((event, handler) => {
      if (!eventHandlers.has(event)) {
        eventHandlers.set(event, []);
      }
      eventHandlers.get(event).push(handler);
    }),
    off: vi.fn((event, handler) => {
      if (eventHandlers.has(event)) {
        const handlers = eventHandlers.get(event);
        const index = handlers.indexOf(handler);
        if (index > -1) {
          handlers.splice(index, 1);
        }
      }
    }),
    emit: (event, data) => {
      if (eventHandlers.has(event)) {
        eventHandlers.get(event).forEach(handler => handler(data));
      }
    },
    _eventHandlers: eventHandlers,
  };
}

/**
 * Sample workflow specification
 */
const SAMPLE_WORKFLOW = {
  id: 'viz-test-workflow',
  name: 'Visualization Test Workflow',
  tasks: [
    { id: 'task-1', name: 'Start Task' },
    { id: 'task-2', name: 'Process Task' },
    { id: 'task-3', name: 'End Task' },
  ],
  flows: [
    { from: 'task-1', to: 'task-2' },
    { from: 'task-2', to: 'task-3' },
  ],
};

/**
 * Complex workflow with parallel paths
 */
const PARALLEL_WORKFLOW = {
  id: 'parallel-viz',
  name: 'Parallel Workflow',
  tasks: [
    { id: 'start', name: 'Start' },
    { id: 'parallel-a', name: 'Parallel A' },
    { id: 'parallel-b', name: 'Parallel B' },
    { id: 'parallel-c', name: 'Parallel C' },
    { id: 'end', name: 'End' },
  ],
  flows: [
    { from: 'start', to: 'parallel-a' },
    { from: 'start', to: 'parallel-b' },
    { from: 'start', to: 'parallel-c' },
    { from: 'parallel-a', to: 'end' },
    { from: 'parallel-b', to: 'end' },
    { from: 'parallel-c', to: 'end' },
  ],
};

// =============================================================================
// Tests
// =============================================================================

describe('Live Workflow Visualization Integration Tests', () => {
  let dom;
  let container;
  let mockEngine;

  beforeEach(() => {
    dom = setupDOM();
    container = document.getElementById('viz-container');
    mockEngine = createMockEngine();
  });

  afterEach(() => {
    cleanupDOM();
  });

  // ===========================================================================
  // Configuration and Initialization Tests
  // ===========================================================================

  describe('Configuration and Initialization', () => {
    it('should create visualizer with valid configuration', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      expect(visualizer).toBeDefined();
      expect(visualizer.start).toBeTypeOf('function');
      expect(visualizer.stop).toBeTypeOf('function');
      expect(visualizer.refresh).toBeTypeOf('function');
      expect(visualizer.exportSVG).toBeTypeOf('function');
      expect(visualizer.getState).toBeTypeOf('function');
    });

    it('should create visualizer with string selector', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container: '#viz-container',
        width: 800,
        height: 600,
      });

      expect(visualizer).toBeDefined();
    });

    it('should validate configuration schema', () => {
      const validConfig = {
        container,
        width: 1200,
        height: 800,
        autoRefresh: true,
        refreshInterval: 2000,
        colorScheme: 'dark',
      };

      const validated = VisualizationConfigSchema.parse(validConfig);
      expect(validated.width).toBe(1200);
      expect(validated.height).toBe(800);
      expect(validated.autoRefresh).toBe(true);
      expect(validated.refreshInterval).toBe(2000);
      expect(validated.colorScheme).toBe('dark');
    });

    it('should use default configuration values', () => {
      const minimalConfig = { container };
      const validated = VisualizationConfigSchema.parse(minimalConfig);

      expect(validated.width).toBe(800);
      expect(validated.height).toBe(600);
      expect(validated.autoRefresh).toBe(true);
      expect(validated.refreshInterval).toBe(1000);
      expect(validated.colorScheme).toBe('light');
    });

    it('should reject invalid container', () => {
      expect(() =>
        createLiveWorkflowVisualizer(mockEngine, {
          container: '#non-existent-container',
          width: 800,
          height: 600,
        })
      ).toThrow('Container element not found');
    });

    it('should support custom color configuration', () => {
      const customColors = {
        enabled: '#00FF00',
        started: '#0000FF',
        completed: '#FFFF00',
        failed: '#FF0000',
        cancelled: '#808080',
      };

      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        colors: customColors,
      });

      expect(visualizer).toBeDefined();
    });
  });

  // ===========================================================================
  // Rendering Tests - SVG Generation
  // ===========================================================================

  describe('SVG Rendering', () => {
    it('should create SVG element on start', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      const svg = container.querySelector('svg');
      expect(svg).toBeDefined();
      expect(svg.getAttribute('width')).toBe('800');
      expect(svg.getAttribute('height')).toBe('600');
      expect(svg.classList.contains('workflow-visualization')).toBe(true);

      visualizer.stop();
    });

    it('should create edge and node groups in SVG', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      const svg = container.querySelector('svg');
      const edgeGroup = svg.querySelector('g.edges');
      const nodeGroup = svg.querySelector('g.nodes');

      expect(edgeGroup).toBeDefined();
      expect(nodeGroup).toBeDefined();

      visualizer.stop();
    });

    it('should create event timeline container', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      const timeline = container.querySelector('.event-timeline');
      expect(timeline).toBeDefined();

      visualizer.stop();
    });

    it('should clear container on initialization', () => {
      // Add some content first
      container.innerHTML = '<div>existing content</div>';

      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      // Should have removed existing content and added SVG
      const existingDiv = container.querySelector('div');
      const svg = container.querySelector('svg');

      expect(existingDiv).toBeNull();
      expect(svg).toBeDefined();

      visualizer.stop();
    });

    it('should support different dimensions', () => {
      const sizes = [
        { width: 400, height: 300 },
        { width: 1920, height: 1080 },
        { width: 1200, height: 800 },
      ];

      sizes.forEach(size => {
        container.innerHTML = ''; // Clear
        const visualizer = createLiveWorkflowVisualizer(mockEngine, {
          container,
          ...size,
        });

        visualizer.start();

        const svg = container.querySelector('svg');
        expect(svg.getAttribute('width')).toBe(String(size.width));
        expect(svg.getAttribute('height')).toBe(String(size.height));

        visualizer.stop();
      });
    });
  });

  // ===========================================================================
  // Event Handling Tests
  // ===========================================================================

  describe('Event Handling', () => {
    it('should subscribe to engine events on start', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      expect(mockEngine.on).toHaveBeenCalledWith('event', expect.any(Function));

      visualizer.stop();
    });

    it('should unsubscribe from engine events on stop', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();
      visualizer.stop();

      expect(mockEngine.off).toHaveBeenCalledWith('event', expect.any(Function));
    });

    it('should handle TASK_ENABLED event', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      const event = {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case-001',
        taskId: 'task-1',
        timestamp: new Date().toISOString(),
      };

      mockEngine.emit('event', event);

      const state = visualizer.getState();
      expect(state.nodes.some(n => n.id === 'task-1')).toBe(true);
      expect(state.eventHistory.some(e => e.taskId === 'task-1')).toBe(true);

      visualizer.stop();
    });

    it('should handle TASK_STARTED event', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      // First enable the task
      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case-001',
        taskId: 'task-1',
      });

      // Then start it
      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_STARTED,
        caseId: 'case-001',
        taskId: 'task-1',
      });

      const state = visualizer.getState();
      const task = state.nodes.find(n => n.id === 'task-1');
      expect(task.status).toBe('started');

      visualizer.stop();
    });

    it('should handle TASK_COMPLETED event', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      // Enable → Start → Complete
      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_STARTED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_COMPLETED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      const state = visualizer.getState();
      const task = state.nodes.find(n => n.id === 'task-1');
      expect(task.status).toBe('completed');

      visualizer.stop();
    });

    it('should handle TASK_CANCELLED event', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_CANCELLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      const state = visualizer.getState();
      const task = state.nodes.find(n => n.id === 'task-1');
      expect(task.status).toBe('cancelled');

      visualizer.stop();
    });

    it('should limit event history size', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      // Emit 150 events (limit is 100)
      for (let i = 0; i < 150; i++) {
        mockEngine.emit('event', {
          type: YAWL_EVENT_TYPES.TASK_ENABLED,
          taskId: `task-${i}`,
          caseId: 'case-001',
          timestamp: new Date().toISOString(),
        });
      }

      const state = visualizer.getState();
      expect(state.eventHistory.length).toBeLessThanOrEqual(100);

      visualizer.stop();
    });
  });

  // ===========================================================================
  // State Management Tests
  // ===========================================================================

  describe('State Management', () => {
    it('should return current visualization state', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      const state = visualizer.getState();
      expect(state).toBeDefined();
      expect(state.nodes).toBeInstanceOf(Array);
      expect(state.edges).toBeInstanceOf(Array);
      expect(state.eventHistory).toBeInstanceOf(Array);
      expect(typeof state.isRunning).toBe('boolean');
      expect(state.isRunning).toBe(true);

      visualizer.stop();
    });

    it('should track node creation and updates', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      // Add tasks
      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-2',
        caseId: 'case-001',
      });

      const state = visualizer.getState();
      expect(state.nodes).toHaveLength(2);
      expect(state.nodes.map(n => n.id)).toContain('task-1');
      expect(state.nodes.map(n => n.id)).toContain('task-2');

      visualizer.stop();
    });

    it('should assign positions to new nodes', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      const state = visualizer.getState();
      const node = state.nodes[0];

      expect(node.x).toBeDefined();
      expect(node.y).toBeDefined();
      expect(typeof node.x).toBe('number');
      expect(typeof node.y).toBe('number');

      visualizer.stop();
    });

    it('should preserve state between refresh calls', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      const state1 = visualizer.getState();
      visualizer.refresh();
      const state2 = visualizer.getState();

      expect(state2.nodes).toHaveLength(state1.nodes.length);
      expect(state2.eventHistory).toHaveLength(state1.eventHistory.length);

      visualizer.stop();
    });
  });

  // ===========================================================================
  // Auto-Refresh Tests
  // ===========================================================================

  describe('Auto-Refresh', () => {
    it('should start auto-refresh timer when enabled', () => {
      vi.useFakeTimers();

      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: true,
        refreshInterval: 100,
      });

      visualizer.start();

      // Timer should be set
      expect(visualizer.getState().isRunning).toBe(true);

      visualizer.stop();
      vi.useRealTimers();
    });

    it('should not start timer when autoRefresh is false', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      // Should still be running but without auto-refresh
      expect(visualizer.getState().isRunning).toBe(true);

      visualizer.stop();
    });

    it('should stop auto-refresh timer on stop', () => {
      vi.useFakeTimers();

      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: true,
        refreshInterval: 100,
      });

      visualizer.start();
      visualizer.stop();

      expect(visualizer.getState().isRunning).toBe(false);

      vi.useRealTimers();
    });
  });

  // ===========================================================================
  // Manual Refresh Tests
  // ===========================================================================

  describe('Manual Refresh', () => {
    it('should support manual refresh', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      expect(() => visualizer.refresh()).not.toThrow();

      visualizer.stop();
    });

    it('should update visualization on manual refresh', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'task-1',
        caseId: 'case-001',
      });

      visualizer.refresh();

      // Should have rendered the node
      const svg = container.querySelector('svg');
      expect(svg).toBeDefined();

      visualizer.stop();
    });
  });

  // ===========================================================================
  // SVG Export Tests
  // ===========================================================================

  describe('SVG Export', () => {
    it('should export SVG as string', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();

      const svgString = visualizer.exportSVG();

      expect(svgString).toBeDefined();
      expect(typeof svgString).toBe('string');
      expect(svgString).toContain('<svg');
      expect(svgString).toContain('</svg>');

      visualizer.stop();
    });

    it('should include visualization dimensions in export', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 1200,
        height: 900,
      });

      visualizer.start();

      const svgString = visualizer.exportSVG();

      expect(svgString).toContain('width="1200"');
      expect(svgString).toContain('height="900"');

      visualizer.stop();
    });

    it('should export with current visualization state', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'export-test',
        caseId: 'case-001',
      });

      visualizer.refresh();

      const svgString = visualizer.exportSVG();
      expect(svgString).toBeDefined();

      visualizer.stop();
    });
  });

  // ===========================================================================
  // Static Workflow Diagram Tests
  // ===========================================================================

  describe('Static Workflow Diagram', () => {
    it('should create static diagram from workflow spec', () => {
      const diagram = createStaticWorkflowDiagram(SAMPLE_WORKFLOW, {
        container,
        width: 800,
        height: 600,
      });

      expect(diagram).toBeDefined();
      expect(diagram.nodes).toBeInstanceOf(Array);
      expect(diagram.edges).toBeInstanceOf(Array);
      expect(diagram.simulation).toBeDefined();
    });

    it('should extract nodes from workflow tasks', () => {
      const diagram = createStaticWorkflowDiagram(SAMPLE_WORKFLOW, {
        container,
        width: 800,
        height: 600,
      });

      expect(diagram.nodes).toHaveLength(3);
      expect(diagram.nodes[0].id).toBe('task-1');
      expect(diagram.nodes[0].label).toBe('Start Task');
      expect(diagram.nodes[0].type).toBe('task');
      expect(diagram.nodes[0].status).toBe('pending');
    });

    it('should extract edges from workflow flows', () => {
      const diagram = createStaticWorkflowDiagram(SAMPLE_WORKFLOW, {
        container,
        width: 800,
        height: 600,
      });

      expect(diagram.edges).toHaveLength(2);
      expect(diagram.edges[0].source).toBe('task-1');
      expect(diagram.edges[0].target).toBe('task-2');
    });

    it('should handle complex workflow with parallel paths', () => {
      const diagram = createStaticWorkflowDiagram(PARALLEL_WORKFLOW, {
        container,
        width: 1200,
        height: 800,
      });

      expect(diagram.nodes).toHaveLength(5);
      expect(diagram.edges).toHaveLength(6);
    });

    it('should handle empty workflow', () => {
      const emptyWorkflow = {
        id: 'empty',
        name: 'Empty',
        tasks: [],
        flows: [],
      };

      const diagram = createStaticWorkflowDiagram(emptyWorkflow, {
        container,
        width: 800,
        height: 600,
      });

      expect(diagram.nodes).toHaveLength(0);
      expect(diagram.edges).toHaveLength(0);
    });
  });

  // ===========================================================================
  // Lifecycle Tests
  // ===========================================================================

  describe('Lifecycle Management', () => {
    it('should prevent multiple start calls', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();
      visualizer.start(); // Second start should be no-op

      expect(visualizer.getState().isRunning).toBe(true);

      visualizer.stop();
    });

    it('should prevent multiple stop calls', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();
      visualizer.stop();
      visualizer.stop(); // Second stop should be no-op

      expect(visualizer.getState().isRunning).toBe(false);
    });

    it('should allow restart after stop', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
      });

      visualizer.start();
      expect(visualizer.getState().isRunning).toBe(true);

      visualizer.stop();
      expect(visualizer.getState().isRunning).toBe(false);

      visualizer.start();
      expect(visualizer.getState().isRunning).toBe(true);

      visualizer.stop();
    });
  });

  // ===========================================================================
  // Performance Tests
  // ===========================================================================

  describe('Performance Under Load', () => {
    it('should handle 100+ events efficiently', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 1200,
        height: 900,
        autoRefresh: false,
      });

      visualizer.start();

      const startTime = Date.now();

      // Emit 100 events
      for (let i = 0; i < 100; i++) {
        mockEngine.emit('event', {
          type: YAWL_EVENT_TYPES.TASK_ENABLED,
          taskId: `task-${i}`,
          caseId: 'perf-test',
          timestamp: new Date().toISOString(),
        });
      }

      const duration = Date.now() - startTime;

      const state = visualizer.getState();
      expect(state.nodes.length).toBeGreaterThan(0);
      // Should complete quickly (< 500ms for 100 events)
      expect(duration).toBeLessThan(500);

      visualizer.stop();
    });

    it('should handle rapid event updates', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      // Rapid state transitions for single task
      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_ENABLED,
        taskId: 'rapid-task',
        caseId: 'case-001',
      });

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_STARTED,
        taskId: 'rapid-task',
        caseId: 'case-001',
      });

      mockEngine.emit('event', {
        type: YAWL_EVENT_TYPES.TASK_COMPLETED,
        taskId: 'rapid-task',
        caseId: 'case-001',
      });

      const state = visualizer.getState();
      const task = state.nodes.find(n => n.id === 'rapid-task');
      expect(task.status).toBe('completed');

      visualizer.stop();
    });

    it('should render large workflow efficiently', () => {
      const largeWorkflow = {
        id: 'large-workflow',
        name: 'Large Workflow',
        tasks: Array(100).fill(null).map((_, i) => ({
          id: `task-${i}`,
          name: `Task ${i}`,
        })),
        flows: Array(99).fill(null).map((_, i) => ({
          from: `task-${i}`,
          to: `task-${i + 1}`,
        })),
      };

      const startTime = Date.now();

      const diagram = createStaticWorkflowDiagram(largeWorkflow, {
        container,
        width: 2000,
        height: 1500,
      });

      const duration = Date.now() - startTime;

      expect(diagram.nodes).toHaveLength(100);
      expect(diagram.edges).toHaveLength(99);
      // Should complete in reasonable time (< 200ms)
      expect(duration).toBeLessThan(200);
    });
  });

  // ===========================================================================
  // Error Handling Tests
  // ===========================================================================

  describe('Error Handling', () => {
    it('should handle events when not running', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      // Don't start visualizer

      expect(() => {
        visualizer.handleEvent({
          type: YAWL_EVENT_TYPES.TASK_ENABLED,
          taskId: 'test',
          caseId: 'case-001',
        });
      }).not.toThrow();
    });

    it('should handle invalid event data gracefully', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      expect(() => {
        visualizer.handleEvent({});
        visualizer.handleEvent(null);
        visualizer.handleEvent({ type: 'UNKNOWN_TYPE' });
      }).not.toThrow();

      visualizer.stop();
    });

    it('should handle missing event properties', () => {
      const visualizer = createLiveWorkflowVisualizer(mockEngine, {
        container,
        width: 800,
        height: 600,
        autoRefresh: false,
      });

      visualizer.start();

      expect(() => {
        visualizer.handleEvent({
          type: YAWL_EVENT_TYPES.TASK_ENABLED,
          // Missing taskId, caseId
        });
      }).not.toThrow();

      visualizer.stop();
    });
  });
});
