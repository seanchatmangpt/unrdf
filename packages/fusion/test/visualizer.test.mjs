/**
 * @file Tests for unified visualizer - determinism verification
 */

import { describe, it, expect } from 'vitest';
import {
  createVisualizer,
  serializeVisualization,
  PATTERN_STYLES,
  STATE_COLORS,
} from '../src/visualizer.mjs';

describe('Unified Visualizer', () => {
  describe('createVisualizer', () => {
    it('creates visualizer instance with all render methods', async () => {
      const viz = await createVisualizer({});

      expect(viz).toHaveProperty('renderWorkflow');
      expect(viz).toHaveProperty('renderReceipts');
      expect(viz).toHaveProperty('renderAllocation');
      expect(viz).toHaveProperty('renderPolicy');
    });
  });

  describe('renderWorkflow', () => {
    it('renders workflow as SVG string', async () => {
      const viz = await createVisualizer({});

      const workflow = {
        tasks: new Map([
          ['task1', { name: 'Task 1', splitType: 'sequence', joinType: 'sequence' }],
          ['task2', { name: 'Task 2', splitType: 'and', joinType: 'sequence' }],
        ]),
        flows: new Map([['task1', new Set(['task2'])]]),
        startTaskId: 'task1',
        endTaskIds: ['task2'],
      };

      const svg = viz.renderWorkflow(workflow);

      expect(svg).toContain('<svg');
      expect(svg).toContain('</svg>');
      expect(svg).toContain('Task 1');
      expect(svg).toContain('Task 2');
      expect(svg).toContain('Start');
      expect(svg).toContain('End');
      expect(svg).toContain('<line'); // Links
      expect(svg).toContain('marker-end'); // Arrowheads
    });

    it('produces deterministic SVG (identical on repeated calls)', async () => {
      const viz = await createVisualizer({});

      const workflow = {
        tasks: new Map([['task1', { name: 'Task 1' }]]),
        flows: new Map(),
        startTaskId: 'task1',
        endTaskIds: ['task1'],
      };

      const svg1 = viz.renderWorkflow(workflow);
      const svg2 = viz.renderWorkflow(workflow);

      expect(svg1).toBe(svg2);
    });

    it('detects Van der Aalst patterns correctly', async () => {
      const viz = await createVisualizer({});

      const workflow = {
        tasks: new Map([
          ['parallel', { name: 'Parallel Split', splitType: 'and', joinType: 'sequence' }],
          ['sync', { name: 'Sync', splitType: 'sequence', joinType: 'and' }],
          ['xor', { name: 'XOR', splitType: 'xor', joinType: 'sequence' }],
        ]),
        flows: new Map(),
        startTaskId: 'parallel',
        endTaskIds: ['xor'],
      };

      const svg = viz.renderWorkflow(workflow);

      // Check for pattern badges
      expect(svg).toContain('AND'); // Parallel split
      expect(svg).toContain('Sync'); // Synchronization
      expect(svg).toContain('XOR'); // Exclusive choice
    });
  });

  describe('renderReceipts', () => {
    it('renders receipts as JSON + SVG timeline', async () => {
      const viz = await createVisualizer({});

      const receipts = [
        { phase: 'store-created', timestamp: '2025-01-01T00:00:00Z' },
        { phase: 'policy-applied', timestamp: '2025-01-01T00:00:01Z', hookId: 'test-hook' },
        { phase: 'case-executed', timestamp: '2025-01-01T00:00:02Z', valid: true },
      ];

      const result = viz.renderReceipts(receipts);

      expect(result).toHaveProperty('json');
      expect(result).toHaveProperty('svg');

      expect(result.json.count).toBe(3);
      expect(result.json.receipts).toHaveLength(3);
      expect(result.json.receipts[0].phase).toBe('store-created');

      expect(result.svg).toContain('<svg');
      expect(result.svg).toContain('store-created');
      expect(result.svg).toContain('policy-applied');
      expect(result.svg).toContain('case-executed');
    });

    it('produces stable JSON key ordering', async () => {
      const viz = await createVisualizer({});

      const receipts = [
        { phase: 'test', timestamp: '2025-01-01T00:00:00Z', valid: true, hookId: 'abc' },
      ];

      const result = viz.renderReceipts(receipts);
      const json = JSON.stringify(result.json);

      // Keys should be alphabetically ordered
      const keys = Object.keys(result.json.receipts[0]);
      const sortedKeys = [...keys].sort();
      expect(keys).toEqual(sortedKeys);
    });

    it('replaces timestamps in DETERMINISTIC mode', async () => {
      process.env.DETERMINISTIC = '1';

      const viz = await createVisualizer({});
      const receipts = [{ phase: 'test', timestamp: '2025-01-01T00:00:00Z' }];
      const result = viz.renderReceipts(receipts);

      expect(result.json.receipts[0].timestamp).toBe('[DETERMINISTIC]');

      delete process.env.DETERMINISTIC;
    });
  });

  describe('renderAllocation', () => {
    it('renders resource allocation as JSON chart data', async () => {
      const viz = await createVisualizer({});

      const pools = {
        cacheL1: 1024 * 1024,
        cacheL2: 10 * 1024 * 1024,
        memory: 512 * 1024 * 1024,
      };

      const result = viz.renderAllocation(pools);

      expect(result.type).toBe('bar-chart');
      expect(result.data).toHaveLength(3);
      expect(result.total).toBe(1024 * 1024 + 10 * 1024 * 1024 + 512 * 1024 * 1024);

      // Check stable key ordering (alphabetical)
      expect(result.data[0].label).toBe('cacheL1');
      expect(result.data[1].label).toBe('cacheL2');
      expect(result.data[2].label).toBe('memory');
    });
  });

  describe('renderPolicy', () => {
    it('renders policies as JSON + SVG DAG', async () => {
      const viz = await createVisualizer({});

      const policies = [
        { id: 'policy-a', trigger: 'before-add', type: 'validation' },
        { id: 'policy-b', trigger: 'after-add', type: 'transformation' },
      ];

      const result = viz.renderPolicy(policies);

      expect(result).toHaveProperty('json');
      expect(result).toHaveProperty('svg');

      expect(result.json.count).toBe(2);
      expect(result.json.policies).toHaveLength(2);
      expect(result.json.policies[0].id).toBe('policy-a');

      expect(result.svg).toContain('<svg');
      expect(result.svg).toContain('policy-a');
      expect(result.svg).toContain('policy-b');
    });

    it('sorts policies by ID for determinism', async () => {
      const viz = await createVisualizer({});

      const policies = [
        { id: 'z-policy', trigger: 'before-add' },
        { id: 'a-policy', trigger: 'after-add' },
      ];

      const result = viz.renderPolicy(policies);

      // Should be sorted alphabetically
      expect(result.json.policies[0].id).toBe('a-policy');
      expect(result.json.policies[1].id).toBe('z-policy');
    });
  });

  describe('serializeVisualization', () => {
    it('serializes visualization with hash', async () => {
      const viz = { type: 'workflow', data: '<svg>test</svg>' };

      const result = await serializeVisualization(viz);

      expect(result).toHaveProperty('type', 'workflow');
      expect(result).toHaveProperty('data', '<svg>test</svg>');
      expect(result).toHaveProperty('hash');
      expect(result.hash).toMatch(/^[a-f0-9]{64}$/); // SHA-256 hex
    });

    it('produces identical hashes for identical data', async () => {
      const viz = { type: 'workflow', data: '<svg>test</svg>' };

      const result1 = await serializeVisualization(viz);
      const result2 = await serializeVisualization(viz);

      expect(result1.hash).toBe(result2.hash);
    });

    it('produces different hashes for different data', async () => {
      const viz1 = { type: 'workflow', data: '<svg>test1</svg>' };
      const viz2 = { type: 'workflow', data: '<svg>test2</svg>' };

      const result1 = await serializeVisualization(viz1);
      const result2 = await serializeVisualization(viz2);

      expect(result1.hash).not.toBe(result2.hash);
    });

    it('replaces timestamp in DETERMINISTIC mode', async () => {
      process.env.DETERMINISTIC = '1';

      const viz = { type: 'workflow', data: '<svg>test</svg>' };
      const result = await serializeVisualization(viz);

      expect(result.timestamp).toBe('[DETERMINISTIC]');

      delete process.env.DETERMINISTIC;
    });
  });

  describe('Constants', () => {
    it('exports PATTERN_STYLES', () => {
      expect(PATTERN_STYLES).toHaveProperty('SEQUENCE');
      expect(PATTERN_STYLES).toHaveProperty('PARALLEL_SPLIT');
      expect(PATTERN_STYLES).toHaveProperty('EXCLUSIVE_CHOICE');
      expect(PATTERN_STYLES.SEQUENCE.fill).toBe('#4A90E2');
    });

    it('exports STATE_COLORS', () => {
      expect(STATE_COLORS).toHaveProperty('ENABLED');
      expect(STATE_COLORS).toHaveProperty('RUNNING');
      expect(STATE_COLORS).toHaveProperty('COMPLETED');
      expect(STATE_COLORS.RUNNING).toBe('#81C784');
    });
  });

  describe('Determinism verification', () => {
    it('produces identical hashes for repeated workflow renders', async () => {
      process.env.DETERMINISTIC = '1';

      const viz = await createVisualizer({});
      const workflow = {
        tasks: new Map([
          ['task1', { name: 'Task 1' }],
          ['task2', { name: 'Task 2' }],
        ]),
        flows: new Map([['task1', new Set(['task2'])]]),
        startTaskId: 'task1',
        endTaskIds: ['task2'],
      };

      const svg1 = viz.renderWorkflow(workflow);
      const svg2 = viz.renderWorkflow(workflow);

      const serialized1 = await serializeVisualization({ type: 'workflow', data: svg1 });
      const serialized2 = await serializeVisualization({ type: 'workflow', data: svg2 });

      expect(serialized1.hash).toBe(serialized2.hash);

      delete process.env.DETERMINISTIC;
    });

    it('produces identical hashes for repeated receipt renders', async () => {
      process.env.DETERMINISTIC = '1';

      const viz = await createVisualizer({});
      const receipts = [
        { phase: 'test1', timestamp: '2025-01-01T00:00:00Z' },
        { phase: 'test2', timestamp: '2025-01-01T00:00:01Z' },
      ];

      const result1 = viz.renderReceipts(receipts);
      const result2 = viz.renderReceipts(receipts);

      const serialized1 = await serializeVisualization({ type: 'receipts', data: result1.json });
      const serialized2 = await serializeVisualization({ type: 'receipts', data: result2.json });

      expect(serialized1.hash).toBe(serialized2.hash);

      delete process.env.DETERMINISTIC;
    });
  });
});
