/**
 * @file Parallel Hook Execution Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  DependencyGraph,
  createDependencyGraph,
  buildExecutionLayers,
} from '../src/hooks/dependency-graph.mjs';
import {
  ParallelHookExecutor,
  executeHooksParallel,
} from '../src/hooks/parallel-executor.mjs';
import { quad, namedNode, literal } from '../../test-utils/src/index.mjs';

describe('Dependency Graph', () => {
  let graph;

  beforeEach(() => {
    graph = new DependencyGraph();
  });

  describe('Node Management', () => {
    it('should add a single node', () => {
      graph.addNode('hook1');
      expect(graph.nodes.has('hook1')).toBe(true);
    });

    it('should add multiple nodes', () => {
      graph.addNode('hook1');
      graph.addNode('hook2');
      graph.addNode('hook3');
      expect(graph.nodes.size).toBe(3);
    });

    it('should add node with dependencies', () => {
      graph.addNode('hook1', []);
      graph.addNode('hook2', ['hook1']);

      const deps = graph.dependencies.get('hook2');
      expect(deps).toBeInstanceOf(Set);
      expect(deps.has('hook1')).toBe(true);
    });

    it('should remove a node', () => {
      graph.addNode('hook1');
      const removed = graph.removeNode('hook1');

      expect(removed).toBe(true);
      expect(graph.nodes.has('hook1')).toBe(false);
    });

    it('should return false when removing non-existent node', () => {
      const removed = graph.removeNode('hook1');
      expect(removed).toBe(false);
    });

    it('should add multiple nodes at once', () => {
      graph.addNodes([
        { hookId: 'hook1', dependsOn: [] },
        { hookId: 'hook2', dependsOn: ['hook1'] },
        { hookId: 'hook3', dependsOn: ['hook1'] },
      ]);

      expect(graph.nodes.size).toBe(3);
    });

    it('should clear all nodes', () => {
      graph.addNode('hook1');
      graph.addNode('hook2');
      graph.clear();

      expect(graph.nodes.size).toBe(0);
      expect(graph.dependencies.size).toBe(0);
      expect(graph.dependents.size).toBe(0);
    });
  });

  describe('Cycle Detection', () => {
    it('should detect no cycle in simple graph', () => {
      graph.addNode('hook1', []);
      graph.addNode('hook2', ['hook1']);

      const result = graph.detectCycle();

      expect(result.hasCycle).toBe(false);
      expect(result.cycle).toBeUndefined();
    });

    it('should detect direct cycle (A -> B -> A)', () => {
      graph.addNode('hookA', ['hookB']);
      graph.addNode('hookB', ['hookA']);

      const result = graph.detectCycle();

      expect(result.hasCycle).toBe(true);
      expect(new Set(result.cycle)).toEqual(new Set(['hookA', 'hookB']));
    });

    it('should detect indirect cycle (A -> B -> C -> A)', () => {
      // Clear first to avoid conflicts with other tests
      graph.clear();
      graph.addNode('hookA', ['hookC']);
      graph.addNode('hookB', ['hookA']);
      graph.addNode('hookC', ['hookB']);

      const result = graph.detectCycle();

      expect(result.hasCycle).toBe(true);
      expect(result.cycle).toContain('hookA');
      expect(result.cycle).toContain('hookB');
      expect(result.cycle).toContain('hookC');
    });

    it('should detect no cycle in complex DAG', () => {
      // Diamond shape (no cycle):
      //     A
      //    / \
      //   B   C
      //    \ /
      //     D
      graph.addNode('A', []);
      graph.addNode('B', ['A']);
      graph.addNode('C', ['A']);
      graph.addNode('D', ['B', 'C']);

      const result = graph.detectCycle();

      expect(result.hasCycle).toBe(false);
    });

    it('should detect self-cycle (A -> A)', () => {
      graph.addNode('hookA', ['hookA']);

      const result = graph.detectCycle();

      expect(result.hasCycle).toBe(true);
    });
  });

  describe('Topological Sort', () => {
    it('should sort simple linear chain', () => {
      graph.addNode('hook1', []);
      graph.addNode('hook2', ['hook1']);
      graph.addNode('hook3', ['hook2']);

      const result = graph.topologicalSort();

      expect(result.hasCycle).toBe(false);
      expect(result.layers).toHaveLength(3);
      expect(result.layers[0]).toEqual(['hook1']);
      expect(result.layers[1]).toEqual(['hook2']);
      expect(result.layers[2]).toEqual(['hook3']);
    });

    it('should sort independent nodes into single layer', () => {
      graph.addNode('hook1', []);
      graph.addNode('hook2', []);
      graph.addNode('hook3', []);

      const result = graph.topologicalSort();

      expect(result.hasCycle).toBe(false);
      expect(result.layers).toHaveLength(1);
      expect(new Set(result.layers[0])).toEqual(new Set(['hook1', 'hook2', 'hook3']));
    });

    it('should sort diamond shape into 3 layers', () => {
      //     A
      //    / \
      //   B   C
      //    \ /
      //     D
      graph.addNode('A', []);
      graph.addNode('B', ['A']);
      graph.addNode('C', ['A']);
      graph.addNode('D', ['B', 'C']);

      const result = graph.topologicalSort();

      expect(result.hasCycle).toBe(false);
      expect(result.layers).toHaveLength(3);
      expect(result.layers[0]).toEqual(['A']);
      expect(result.layers[1].sort()).toEqual(['B', 'C'].sort());
      expect(result.layers[2]).toEqual(['D']);
    });

    it('should handle empty graph', () => {
      const result = graph.topologicalSort();

      expect(result.hasCycle).toBe(false);
      expect(result.layers).toHaveLength(0);
    });

    it('should return cycle info when cycle exists', () => {
      graph.clear();
      graph.addNode('hookA', ['hookB']);
      graph.addNode('hookB', ['hookA']);

      const result = graph.topologicalSort();

      expect(result.hasCycle).toBe(true);
      expect(result.layers).toHaveLength(0);
      expect(result.cycle).toBeDefined();
    });
  });

  describe('Transitive Dependencies', () => {
    it('should get direct dependencies', () => {
      graph.addNode('hook1', []);
      graph.addNode('hook2', ['hook1']);
      graph.addNode('hook3', ['hook2']);

      const deps = graph.getTransitiveDependencies('hook3');

      expect(deps).toBeInstanceOf(Set);
      expect(deps.has('hook2')).toBe(true);
      expect(deps.has('hook1')).toBe(true);
      expect(deps.has('hook3')).toBe(false);
    });

    it('should get transitive dependents', () => {
      graph.addNode('hook1', []);
      graph.addNode('hook2', ['hook1']);
      graph.addNode('hook3', ['hook2']);

      const dependents = graph.getTransitiveDependents('hook1');

      expect(dependents).toBeInstanceOf(Set);
      expect(dependents.has('hook2')).toBe(true);
      expect(dependents.has('hook3')).toBe(true);
      expect(dependents.has('hook1')).toBe(false);
    });
  });

  describe('Statistics', () => {
    it('should calculate graph statistics', () => {
      graph.addNode('A', []);
      graph.addNode('B', ['A']);
      graph.addNode('C', ['A']);
      graph.addNode('D', ['B', 'C']);

      const stats = graph.getStats();

      expect(stats.nodeCount).toBe(4);
      expect(stats.edgeCount).toBe(4); // B->A, C->A, D->B, D->C
      expect(stats.maxDepth).toBe(3);
      // Average branching factor: 4 edges / 4 nodes = 1 (B has 1 dependent)
      expect(stats.avgBranchingFactor).toBeCloseTo(1, 1);
    });

    it('should handle empty graph stats', () => {
      const stats = graph.getStats();

      expect(stats.nodeCount).toBe(0);
      expect(stats.edgeCount).toBe(0);
      expect(stats.maxDepth).toBe(0);
      expect(stats.avgBranchingFactor).toBe(0);
    });
  });

  describe('Factory Functions', () => {
    it('should create graph from hooks', () => {
      const hooks = [
        { id: 'hook1', dependsOn: [] },
        { id: 'hook2', dependsOn: ['hook1'] },
        { id: 'hook3', dependsOn: ['hook1'] },
      ];

      const graph = createDependencyGraph(hooks);

      expect(graph.nodes.size).toBe(3);
    });

    it('should build execution layers from hooks', () => {
      const hooks = [
        { id: 'hook1', dependsOn: [] },
        { id: 'hook2', dependsOn: ['hook1'] },
        { id: 'hook3', dependsOn: ['hook1'] },
      ];

      const result = buildExecutionLayers(hooks);

      expect(result.hasCycle).toBe(false);
      expect(result.layers[0]).toEqual(['hook1']);
      expect(result.layers[1].sort()).toEqual(['hook2', 'hook3'].sort());
    });
  });
});

describe('Parallel Hook Executor', () => {
  let executor;
  const testStore = {
    _quads: [],
    add(quad) {
      this._quads.push(quad);
    },
    query() {
      return this._quads;
    },
  };

  afterEach(async () => {
    if (executor) {
      await executor.shutdown();
      executor = null;
    }
  });

  describe('Executor Creation', () => {
    it('should create executor with default config', () => {
      executor = new ParallelHookExecutor();

      expect(executor).toBeInstanceOf(ParallelHookExecutor);
    });

    it('should create executor with custom config', () => {
      executor = new ParallelHookExecutor({
        maxWorkers: 8,
        taskTimeout: 60000,
      });

      expect(executor.maxWorkers).toBe(8);
      expect(executor.taskTimeout).toBe(60000);
    });
  });

  describe('Hook Registration', () => {
    it('should register hooks for execution', () => {
      executor = new ParallelHookExecutor();

      const hooks = [
        { id: 'hook1', name: 'Hook 1', run: async () => ({}) },
        { id: 'hook2', name: 'Hook 2', run: async () => ({}) },
      ];

      executor.registerHooks(hooks);

      const stats = executor.getStats();

      expect(stats.graphStats.nodeCount).toBe(2);
    });
  });

  describe('Parallel Execution', () => {
    it('should execute hooks in parallel based on dependencies', async () => {
      executor = new ParallelHookExecutor({ maxWorkers: 4 });

      const hooks = [
        {
          id: 'hook1',
          dependsOn: [],
          run: async ({ store }) => {
            return { result: 'hook1' };
          },
        },
        {
          id: 'hook2',
          dependsOn: ['hook1'],
          run: async ({ store }) => {
            return { result: 'hook2' };
          },
        },
        {
          id: 'hook3',
          dependsOn: ['hook1'],
          run: async ({ store }) => {
            return { result: 'hook3' };
          },
        },
      ];

      const delta = { adds: [], deletes: [] };

      const results = await executor.execute(hooks, testStore, delta);

      expect(results).toHaveLength(3);
      expect(results.every(r => r.success)).toBe(true);
    });

    it('should execute independent hooks in parallel', async () => {
      executor = new ParallelHookExecutor({ maxWorkers: 4 });

      const hooks = [
        {
          id: 'hook1',
          run: async () => {
            return { result: 'hook1' };
          },
        },
        {
          id: 'hook2',
          run: async () => {
            return { result: 'hook2' };
          },
        },
        {
          id: 'hook3',
          run: async () => {
            return { result: 'hook3' };
          },
        },
      ];

      const delta = { adds: [], deletes: [] };

      const results = await executor.execute(hooks, testStore, delta);

      expect(results).toHaveLength(3);
      // Fallback mode returns results directly
      expect(results[0].result).toEqual({ result: 'hook1' });
      expect(results[1].result).toEqual({ result: 'hook2' });
      expect(results[2].result).toEqual({ result: 'hook3' });
    });

    it('should handle execution errors gracefully', async () => {
      executor = new ParallelHookExecutor({ maxWorkers: 2 });

      const hooks = [
        {
          id: 'hook1',
          run: async () => {
            return { ok: true };
          },
        },
        {
          id: 'hook2',
          run: async () => {
            throw new Error('Hook execution failed');
          },
        },
      ];

      const delta = { adds: [], deletes: [] };

      const results = await executor.execute(hooks, testStore, delta);

      // Should have at least one failure
      const hasFailure = results.some(r => !r.success);
      expect(hasFailure).toBe(true);

      // Error message should be preserved
      const failedResult = results.find(r => !r.success);
      expect(failedResult.error).toContain('Hook execution failed');
    });

    it('should fallback to sequential on cycle detection', async () => {
      executor = new ParallelHookExecutor({
        maxWorkers: 2,
        fallbackToSequential: true,
      });

      // Create cyclic dependency
      const hooks = [
        {
          id: 'hook1',
          dependsOn: ['hook2'],
          run: async () => ({ result: 'hook1' }),
        },
        {
          id: 'hook2',
          dependsOn: ['hook1'],
          run: async () => ({ result: 'hook2' }),
        },
      ];

      const delta = { adds: [], deletes: [] };

      const results = await executor.execute(hooks, testStore, delta);

      // Should still execute (fallback to sequential)
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('Statistics', () => {
    it('should report execution statistics', async () => {
      executor = new ParallelHookExecutor({ maxWorkers: 2 });

      const hooks = [
        {
          id: 'hook1',
          run: async () => ({ ok: true }),
        },
        {
          id: 'hook2',
          dependsOn: ['hook1'],
          run: async () => ({ ok: true }),
        },
      ];

      executor.registerHooks(hooks);

      const stats = executor.getStats();

      expect(stats.graphStats).toBeDefined();
      expect(stats.poolStats).toBeDefined();
    });
  });
});

describe('Convenience Functions', () => {
    it('should execute hooks with convenience function', async () => {
      const hooks = [
        {
          id: 'hook1',
          run: async () => ({ result: 'ok' }),
        },
      ];

      const store = {
        _quads: [],
        add(quad) {
          this._quads.push(quad);
        },
        query() {
          return this._quads;
        },
      };

      const delta = { adds: [], deletes: [] };

      const results = await executeHooksParallel(hooks, store, delta);

      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
    });
});

describe('Performance Benchmarks', () => {
  const testStore = {
    _quads: [],
    add(quad) {
      this._quads.push(quad);
    },
    query() {
      return this._quads;
    },
  };

  it('should demonstrate parallel speedup', async () => {
    const executor = new ParallelHookExecutor({ maxWorkers: 4 });

    // Create hooks that simulate work
    const hooks = [];
    for (let i = 0; i < 10; i++) {
      hooks.push({
        id: `hook${i}`,
        run: async () => {
          // Simulate work (10ms delay)
          await new Promise(resolve => setTimeout(resolve, 10));
          return { result: i };
        },
      });
    }

    const delta = { adds: [], deletes: [] };

    const start = Date.now();
    const results = await executor.execute(hooks, testStore, delta);
    const duration = Date.now() - start;

    console.log(`Parallel execution of 10 hooks: ${duration}ms`);
    console.log(`Results: ${results.filter(r => r.success).length}/${results.length} successful`);

    // With 4 workers and 10ms per hook, should be around 30-40ms (not 100ms sequential)
    expect(duration).toBeLessThan(100);
    expect(results.every(r => r.success)).toBe(true);

    await executor.shutdown();
  });
});
