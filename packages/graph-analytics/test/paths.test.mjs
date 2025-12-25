import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import { rdfToGraph } from '../src/converter/rdf-to-graph.mjs';
import {
  findShortestPath,
  findAllPaths,
  findCommonNeighbors,
  findKHopNeighbors,
} from '../src/paths/relationship-finder.mjs';

const { namedNode, triple } = dataFactory;

describe('Path Finder', () => {
  it('should find shortest path between two nodes', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const knows = namedNode('http://schema.org/knows');

    store.add(triple(a, knows, b));
    store.add(triple(b, knows, c));

    const graph = rdfToGraph(store);
    const path = findShortestPath(
      graph,
      'http://example.org/A',
      'http://example.org/C'
    );

    expect(path).toBeTruthy();
    expect(path.length).toBe(2);
    expect(path.path).toEqual([
      'http://example.org/A',
      'http://example.org/B',
      'http://example.org/C',
    ]);
  });

  it('should return null when no path exists', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const knows = namedNode('http://schema.org/knows');

    store.add(triple(a, knows, b));

    const graph = rdfToGraph(store);
    const path = findShortestPath(
      graph,
      'http://example.org/B',
      'http://example.org/A'
    );

    expect(path).toBeNull();
  });

  it('should find all paths up to max depth', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const knows = namedNode('http://schema.org/knows');

    // Two paths: A->B->C and A->C
    store.add(triple(a, knows, b));
    store.add(triple(b, knows, c));
    store.add(triple(a, knows, c));

    const graph = rdfToGraph(store);
    const paths = findAllPaths(
      graph,
      'http://example.org/A',
      'http://example.org/C',
      { maxDepth: 3 }
    );

    expect(paths.length).toBeGreaterThanOrEqual(1);
  });

  it('should find common neighbors', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const knows = namedNode('http://schema.org/knows');

    // Both A and B know C
    store.add(triple(a, knows, c));
    store.add(triple(b, knows, c));

    const graph = rdfToGraph(store);
    const common = findCommonNeighbors(
      graph,
      'http://example.org/A',
      'http://example.org/B'
    );

    expect(common.commonOutNeighbors).toContain('http://example.org/C');
  });

  it('should find K-hop neighbors', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const knows = namedNode('http://schema.org/knows');

    store.add(triple(a, knows, b));
    store.add(triple(b, knows, c));

    const graph = rdfToGraph(store);
    const neighbors = findKHopNeighbors(graph, 'http://example.org/A', 2);

    expect(neighbors.size).toBeGreaterThanOrEqual(2);
    expect(neighbors.get('http://example.org/B')).toBe(1);
    expect(neighbors.get('http://example.org/C')).toBe(2);
  });
});
