/**
 * @fileoverview Tests for useGraphs hook
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { renderHook, act } from '@testing-library/react';
import { useGraphs } from '../../../src/react-hooks/core/useGraphs.mjs';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal, defaultGraph } = DataFactory;

describe('useGraphs', () => {
  let testStore;

  beforeEach(() => {
    testStore = new Store([
      quad(namedNode('http://s1'), namedNode('http://p1'), literal('o1'), defaultGraph()),
      quad(
        namedNode('http://s2'),
        namedNode('http://p2'),
        literal('o2'),
        namedNode('http://graph1')
      ),
      quad(
        namedNode('http://s3'),
        namedNode('http://p3'),
        literal('o3'),
        namedNode('http://graph1')
      ),
      quad(
        namedNode('http://s4'),
        namedNode('http://p4'),
        literal('o4'),
        namedNode('http://graph2')
      ),
    ]);
  });

  describe('Initialization', () => {
    it('should list all graphs', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      expect(result.current.graphs).toBeDefined();
      expect(result.current.graphs.length).toBeGreaterThan(0);
    });

    it('should count graphs correctly', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      expect(result.current.graphCount).toBe(3); // default, graph1, graph2
    });

    it('should have no selected graph initially', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      expect(result.current.selectedGraph).toBeNull();
    });
  });

  describe('Graph Retrieval', () => {
    it('should get graph by IRI', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const graph = result.current.getGraph('http://graph1');

      expect(graph).toBeInstanceOf(Store);
      expect(graph.size).toBe(2);
    });

    it('should get default graph', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const graph = result.current.getDefaultGraph();

      expect(graph).toBeInstanceOf(Store);
      expect(graph.size).toBe(1);
    });

    it('should return empty store for non-existent graph', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const graph = result.current.getGraph('http://nonexistent');

      expect(graph).toBeInstanceOf(Store);
      expect(graph.size).toBe(0);
    });
  });

  describe('Graph Selection', () => {
    it('should select a graph', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      act(() => {
        result.current.selectGraph('http://graph1');
      });

      expect(result.current.selectedGraph).toBe('http://graph1');
    });

    it('should update current graph when selected', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      act(() => {
        result.current.selectGraph('http://graph1');
      });

      expect(result.current.currentGraph.size).toBe(2);
    });

    it('should return full store when no graph selected', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      expect(result.current.currentGraph).toBe(testStore);
    });
  });

  describe('Graph Modification', () => {
    it('should add quad to named graph', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const newQuad = quad(namedNode('http://s5'), namedNode('http://p5'), literal('o5'));

      act(() => {
        result.current.addToGraph(newQuad, 'http://graph3');
      });

      const graph = result.current.getGraph('http://graph3');
      expect(graph.size).toBe(1);
    });

    it('should return created quad', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const originalQuad = quad(namedNode('http://s5'), namedNode('http://p5'), literal('o5'));

      let createdQuad;
      act(() => {
        createdQuad = result.current.addToGraph(originalQuad, 'http://graph3');
      });

      expect(createdQuad.graph.value).toBe('http://graph3');
      expect(createdQuad.subject.value).toBe('http://s5');
    });
  });

  describe('Graph Listing', () => {
    it('should list all graph IRIs', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const graphs = result.current.graphs;

      expect(graphs).toContain('http://graph1');
      expect(graphs).toContain('http://graph2');
    });

    it('should include default graph', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      const graphs = result.current.graphs;

      expect(graphs).toContain(defaultGraph().value);
    });
  });

  describe('State Updates', () => {
    it('should update when store changes', () => {
      const { result, rerender } = renderHook(({ store }) => useGraphs(store), {
        initialProps: { store: testStore },
      });

      const initialCount = result.current.graphCount;

      const newStore = new Store([
        ...testStore,
        quad(
          namedNode('http://s5'),
          namedNode('http://p5'),
          literal('o5'),
          namedNode('http://graph3')
        ),
      ]);

      rerender({ store: newStore });

      expect(result.current.graphCount).toBeGreaterThan(initialCount);
    });
  });

  describe('Performance', () => {
    it('should handle many graphs efficiently', () => {
      const largeStore = new Store();

      for (let i = 0; i < 100; i++) {
        for (let j = 0; j < 100; j++) {
          largeStore.add(
            quad(
              namedNode(`http://s${j}`),
              namedNode(`http://p${j}`),
              literal(`o${j}`),
              namedNode(`http://graph${i}`)
            )
          );
        }
      }

      const start = performance.now();
      const { result } = renderHook(() => useGraphs(largeStore));
      const duration = performance.now() - start;

      expect(result.current.graphCount).toBe(100);
      expect(duration).toBeLessThan(1000);
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty store', () => {
      const { result } = renderHook(() => useGraphs(new Store()));

      expect(result.current.graphs).toHaveLength(0);
      expect(result.current.graphCount).toBe(0);
    });

    it('should handle selecting non-existent graph', () => {
      const { result } = renderHook(() => useGraphs(testStore));

      act(() => {
        result.current.selectGraph('http://nonexistent');
      });

      expect(result.current.selectedGraph).toBe('http://nonexistent');
      expect(result.current.currentGraph.size).toBe(0);
    });
  });
});
