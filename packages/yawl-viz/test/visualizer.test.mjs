/**
 * @file YAWL Visualizer Tests
 * @module @unrdf/yawl-viz/test
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { JSDOM } from 'jsdom';
import { YAWLVisualizer, PATTERN_STYLES, STATE_COLORS } from '../src/visualizer.mjs';

// =============================================================================
// Test Setup
// =============================================================================

/**
 * Create mock YAWL engine for testing
 */
function createMockEngine() {
  const workflows = new Map();
  const cases = new Map();
  const eventHandlers = new Map();

  return {
    workflows,
    cases,
    on(eventType, handler) {
      if (!eventHandlers.has(eventType)) {
        eventHandlers.set(eventType, []);
      }
      eventHandlers.get(eventType).push(handler);
      return () => {
        const handlers = eventHandlers.get(eventType);
        const index = handlers.indexOf(handler);
        if (index > -1) handlers.splice(index, 1);
      };
    },
    emit(eventType, data) {
      const handlers = eventHandlers.get(eventType) || [];
      for (const handler of handlers) {
        handler({ type: eventType, ...data });
      }
    },
    _eventHandlers: eventHandlers,
  };
}

/**
 * Create mock workflow definition
 */
function createMockWorkflow(id = 'test-workflow') {
  return {
    id,
    name: 'Test Workflow',
    tasks: new Map([
      ['task1', { name: 'Task 1', splitType: 'sequence', joinType: 'sequence' }],
      ['task2', { name: 'Task 2', splitType: 'and', joinType: 'sequence' }],
      ['task3', { name: 'Task 3', splitType: 'sequence', joinType: 'and' }],
      ['task4', { name: 'Task 4', splitType: 'xor', joinType: 'sequence' }],
    ]),
    flows: new Map([
      ['task1', new Set(['task2'])],
      ['task2', new Set(['task3', 'task4'])],
      ['task3', new Set(['task4'])],
    ]),
    startTaskId: 'task1',
    endTaskIds: ['task4'],
  };
}

/**
 * Setup DOM environment
 */
function setupDOM() {
  const dom = new JSDOM('<!DOCTYPE html><html><body><div id="canvas"></div></body></html>');
  global.document = dom.window.document;
  global.window = dom.window;
  global.SVGElement = dom.window.SVGElement;
  return dom;
}

// =============================================================================
// Test Suite
// =============================================================================

describe('YAWLVisualizer', () => {
  let dom;
  let container;
  let engine;
  let workflow;

  beforeEach(() => {
    dom = setupDOM();
    container = document.getElementById('canvas');
    engine = createMockEngine();
    workflow = createMockWorkflow();
    engine.workflows.set(workflow.id, workflow);
  });

  afterEach(() => {
    if (dom) {
      dom.window.close();
    }
  });

  // ===========================================================================
  // Constructor Tests
  // ===========================================================================

  describe('constructor', () => {
    it('should create visualizer with default config', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      expect(viz.engine).toBe(engine);
      expect(viz.container).toBe(container);
      expect(viz.width).toBe(1200);
      expect(viz.height).toBe(800);
      expect(viz.autoSubscribe).toBe(true);
    });

    it('should create visualizer with custom dimensions', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        width: 800,
        height: 600,
      });

      expect(viz.width).toBe(800);
      expect(viz.height).toBe(600);
    });

    it('should auto-subscribe to events when enabled', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: true,
      });

      expect(viz.eventHandlers.length).toBeGreaterThan(0);
    });

    it('should not auto-subscribe when disabled', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      expect(viz.eventHandlers.length).toBe(0);
    });

    it('should initialize SVG canvas', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      const svg = container.querySelector('svg');
      expect(svg).toBeTruthy();
      expect(svg.getAttribute('width')).toBe('1200');
      expect(svg.getAttribute('height')).toBe('800');
    });
  });

  // ===========================================================================
  // Event Subscription Tests
  // ===========================================================================

  describe('event subscription', () => {
    it('should subscribe to all engine events', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      viz.subscribeToEvents();

      const expectedEvents = [
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

      for (const eventType of expectedEvents) {
        expect(engine._eventHandlers.has(eventType)).toBe(true);
        expect(engine._eventHandlers.get(eventType).length).toBeGreaterThan(0);
      }
    });

    it('should handle task:enabled events', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      engine.emit('task:enabled', {
        taskId: 'task1',
        workItemId: 'wi1',
        caseId: 'case1',
      });

      expect(viz.taskStates.get('wi1')).toBe('ENABLED');
    });

    it('should handle task:started events', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      engine.emit('task:started', {
        taskId: 'task1',
        workItemId: 'wi1',
        caseId: 'case1',
      });

      expect(viz.taskStates.get('wi1')).toBe('RUNNING');
    });

    it('should handle task:completed events', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      engine.emit('task:completed', {
        taskId: 'task1',
        workItemId: 'wi1',
        caseId: 'case1',
      });

      expect(viz.taskStates.get('wi1')).toBe('COMPLETED');
    });

    it('should unsubscribe from events', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      const initialCount = viz.eventHandlers.length;
      expect(initialCount).toBeGreaterThan(0);

      viz.unsubscribeFromEvents();

      expect(viz.eventHandlers.length).toBe(0);
    });
  });

  // ===========================================================================
  // Workflow Rendering Tests
  // ===========================================================================

  describe('workflow rendering', () => {
    it('should render workflow to graph data', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const graphData = viz.renderWorkflow(workflow.id);

      expect(graphData).toBeDefined();
      expect(graphData.nodes).toBeDefined();
      expect(graphData.links).toBeDefined();
    });

    it('should create nodes for all tasks', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const graphData = viz.renderWorkflow(workflow.id);

      // Should have: start + 4 tasks + end = 6 nodes
      expect(graphData.nodes.length).toBe(6);

      const taskNodes = graphData.nodes.filter(n => n.type === 'task');
      expect(taskNodes.length).toBe(4);
    });

    it('should create links from workflow flows', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const graphData = viz.renderWorkflow(workflow.id);

      // Should have links for all flows
      expect(graphData.links.length).toBeGreaterThan(0);

      // Verify specific link exists
      const task1ToTask2 = graphData.links.find(
        l => l.source === 'task1' && l.target === 'task2'
      );
      expect(task1ToTask2).toBeDefined();
    });

    it('should detect Van der Aalst patterns correctly', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const graphData = viz.renderWorkflow(workflow.id);

      // task2 has splitType='and' → PARALLEL_SPLIT
      const task2Node = graphData.nodes.find(n => n.id === 'task2');
      expect(task2Node.pattern).toBe('PARALLEL_SPLIT');

      // task3 has joinType='and' → SYNCHRONIZATION
      const task3Node = graphData.nodes.find(n => n.id === 'task3');
      expect(task3Node.pattern).toBe('SYNCHRONIZATION');

      // task4 has splitType='xor' → EXCLUSIVE_CHOICE
      const task4Node = graphData.nodes.find(n => n.id === 'task4');
      expect(task4Node.pattern).toBe('EXCLUSIVE_CHOICE');
    });

    it('should throw error for non-existent workflow', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      expect(() => {
        viz.renderWorkflow('non-existent');
      }).toThrow('Workflow non-existent not found');
    });
  });

  // ===========================================================================
  // Pattern Detection Tests
  // ===========================================================================

  describe('pattern detection', () => {
    it('should detect PARALLEL_SPLIT pattern', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const taskDef = { splitType: 'and', joinType: 'sequence' };
      const pattern = viz._detectPattern(taskDef, workflow);

      expect(pattern).toBe('PARALLEL_SPLIT');
    });

    it('should detect SYNCHRONIZATION pattern', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const taskDef = { splitType: 'sequence', joinType: 'and' };
      const pattern = viz._detectPattern(taskDef, workflow);

      expect(pattern).toBe('SYNCHRONIZATION');
    });

    it('should detect EXCLUSIVE_CHOICE pattern', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const taskDef = { splitType: 'xor', joinType: 'sequence' };
      const pattern = viz._detectPattern(taskDef, workflow);

      expect(pattern).toBe('EXCLUSIVE_CHOICE');
    });

    it('should detect SIMPLE_MERGE pattern', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const taskDef = { splitType: 'sequence', joinType: 'xor' };
      const pattern = viz._detectPattern(taskDef, workflow);

      expect(pattern).toBe('SIMPLE_MERGE');
    });

    it('should default to SEQUENCE for unknown patterns', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const taskDef = { splitType: 'unknown', joinType: 'unknown' };
      const pattern = viz._detectPattern(taskDef, workflow);

      expect(pattern).toBe('SEQUENCE');
    });
  });

  // ===========================================================================
  // Case Rendering Tests
  // ===========================================================================

  describe('case rendering', () => {
    it('should render case with work item states', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      const mockCase = {
        id: 'case1',
        workflowId: workflow.id,
        workItems: new Map([
          ['wi1', { id: 'wi1', taskDefId: 'task1', status: 'RUNNING' }],
          ['wi2', { id: 'wi2', taskDefId: 'task2', status: 'ENABLED' }],
        ]),
      };

      engine.cases.set('case1', mockCase);

      viz.renderCase('case1');

      expect(viz.currentCase).toBe('case1');
      expect(viz.taskStates.get('wi1')).toBe('RUNNING');
      expect(viz.taskStates.get('wi2')).toBe('ENABLED');
    });

    it('should throw error for non-existent case', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      expect(() => {
        viz.renderCase('non-existent');
      }).toThrow('Case non-existent not found');
    });
  });

  // ===========================================================================
  // Lifecycle Tests
  // ===========================================================================

  describe('lifecycle', () => {
    it('should start visualizer', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
        autoSubscribe: false,
      });

      expect(viz.eventHandlers.length).toBe(0);

      viz.start();

      expect(viz.eventHandlers.length).toBeGreaterThan(0);
    });

    it('should stop visualizer', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      const initialCount = viz.eventHandlers.length;
      expect(initialCount).toBeGreaterThan(0);

      viz.stop();

      expect(viz.eventHandlers.length).toBe(0);
    });

    it('should destroy visualizer', () => {
      const viz = new YAWLVisualizer({
        engine,
        container,
      });

      viz.renderWorkflow(workflow.id);

      expect(viz.nodes.length).toBeGreaterThan(0);
      expect(viz.links.length).toBeGreaterThan(0);
      expect(container.children.length).toBeGreaterThan(0);

      viz.destroy();

      expect(viz.nodes.length).toBe(0);
      expect(viz.links.length).toBe(0);
      expect(viz.taskStates.size).toBe(0);
      expect(container.children.length).toBe(0);
    });
  });

  // ===========================================================================
  // Constants Tests
  // ===========================================================================

  describe('pattern styles', () => {
    it('should have styles for all Van der Aalst patterns', () => {
      expect(PATTERN_STYLES.SEQUENCE).toBeDefined();
      expect(PATTERN_STYLES.PARALLEL_SPLIT).toBeDefined();
      expect(PATTERN_STYLES.SYNCHRONIZATION).toBeDefined();
      expect(PATTERN_STYLES.EXCLUSIVE_CHOICE).toBeDefined();
      expect(PATTERN_STYLES.SIMPLE_MERGE).toBeDefined();
      expect(PATTERN_STYLES.MULTI_CHOICE).toBeDefined();
      expect(PATTERN_STYLES.STRUCTURED_SYNC_MERGE).toBeDefined();
    });

    it('should have distinct colors for each pattern', () => {
      const colors = new Set();
      for (const [key, style] of Object.entries(PATTERN_STYLES)) {
        if (key !== 'DEFAULT') {
          colors.add(style.fill);
        }
      }

      // Should have multiple distinct colors
      expect(colors.size).toBeGreaterThan(3);
    });
  });

  describe('state colors', () => {
    it('should have colors for all task states', () => {
      expect(STATE_COLORS.ENABLED).toBeDefined();
      expect(STATE_COLORS.RUNNING).toBeDefined();
      expect(STATE_COLORS.COMPLETED).toBeDefined();
      expect(STATE_COLORS.CANCELLED).toBeDefined();
      expect(STATE_COLORS.FAILED).toBeDefined();
      expect(STATE_COLORS.IDLE).toBeDefined();
    });
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('YAWLVisualizer Integration', () => {
  let dom;
  let container;
  let engine;
  let workflow;

  beforeEach(() => {
    dom = setupDOM();
    container = document.getElementById('canvas');
    engine = createMockEngine();
    workflow = createMockWorkflow('approval-workflow');

    // Create more realistic workflow
    workflow.tasks = new Map([
      ['submit', { name: 'Submit', splitType: 'sequence', joinType: 'sequence' }],
      ['parallel-split', { name: 'Split', splitType: 'and', joinType: 'sequence' }],
      ['review1', { name: 'Review 1', splitType: 'sequence', joinType: 'sequence' }],
      ['review2', { name: 'Review 2', splitType: 'sequence', joinType: 'sequence' }],
      ['sync', { name: 'Sync', splitType: 'sequence', joinType: 'and' }],
      ['approve', { name: 'Approve', splitType: 'xor', joinType: 'sequence' }],
    ]);

    workflow.flows = new Map([
      ['submit', new Set(['parallel-split'])],
      ['parallel-split', new Set(['review1', 'review2'])],
      ['review1', new Set(['sync'])],
      ['review2', new Set(['sync'])],
      ['sync', new Set(['approve'])],
    ]);

    engine.workflows.set(workflow.id, workflow);
  });

  afterEach(() => {
    if (dom) {
      dom.window.close();
    }
  });

  it('should handle complete workflow execution', () => {
    const viz = new YAWLVisualizer({
      engine,
      container,
    });

    viz.renderWorkflow(workflow.id);

    // Simulate workflow execution
    engine.emit('case:created', { caseId: 'case1', workflowId: workflow.id });
    engine.emit('task:enabled', { taskId: 'submit', workItemId: 'wi1', caseId: 'case1' });
    engine.emit('task:started', { taskId: 'submit', workItemId: 'wi1', caseId: 'case1' });
    engine.emit('task:completed', { taskId: 'submit', workItemId: 'wi1', caseId: 'case1' });

    expect(viz.taskStates.get('wi1')).toBe('COMPLETED');
    expect(viz.currentCase).toBe('case1');
  });

  it('should handle parallel execution', () => {
    const viz = new YAWLVisualizer({
      engine,
      container,
    });

    viz.renderWorkflow(workflow.id);

    // Start parallel branches
    engine.emit('task:enabled', { taskId: 'review1', workItemId: 'wi2', caseId: 'case1' });
    engine.emit('task:enabled', { taskId: 'review2', workItemId: 'wi3', caseId: 'case1' });
    engine.emit('task:started', { taskId: 'review1', workItemId: 'wi2', caseId: 'case1' });
    engine.emit('task:started', { taskId: 'review2', workItemId: 'wi3', caseId: 'case1' });

    expect(viz.taskStates.get('wi2')).toBe('RUNNING');
    expect(viz.taskStates.get('wi3')).toBe('RUNNING');
  });
});
