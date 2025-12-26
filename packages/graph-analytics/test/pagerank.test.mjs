import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import { rdfToGraph } from '../src/converter/rdf-to-graph.mjs';
import {
  computePageRank,
  computeDegreeCentrality,
  getTopNodes,
} from '../src/centrality/pagerank-analyzer.mjs';

const { namedNode, triple } = dataFactory;

describe('PageRank Analyzer', () => {
  it('should compute PageRank for simple graph', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const knows = namedNode('http://schema.org/knows');

    store.add(triple(a, knows, b));

    const graph = rdfToGraph(store);
    const scores = computePageRank(graph);

    expect(scores.size).toBe(2);
    expect(scores.get('http://example.org/A')).toBeGreaterThan(0);
    expect(scores.get('http://example.org/B')).toBeGreaterThan(0);
  });

  it('should identify hub nodes with high PageRank', () => {
    const store = createStore();
    const hub = namedNode('http://example.org/Hub');
    const knows = namedNode('http://schema.org/knows');

    // Create star topology - hub connected to 5 nodes
    for (let i = 0; i < 5; i++) {
      const node = namedNode(`http://example.org/Node${i}`);
      store.add(triple(node, knows, hub));
    }

    const graph = rdfToGraph(store);
    const scores = computePageRank(graph);
    const top = getTopNodes(scores, 1);

    expect(top[0].node).toBe('http://example.org/Hub');
  });

  it('should compute degree centrality', () => {
    const store = createStore();
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const knows = namedNode('http://schema.org/knows');

    store.add(triple(a, knows, b));
    store.add(triple(a, knows, c));

    const graph = rdfToGraph(store);
    const centrality = computeDegreeCentrality(graph);

    expect(centrality.outDegree.get('http://example.org/A')).toBeGreaterThan(0);
    expect(centrality.inDegree.get('http://example.org/B')).toBeGreaterThan(0);
  });

  it('should return empty scores for empty graph', () => {
    const store = createStore();
    const graph = rdfToGraph(store);
    const scores = computePageRank(graph);

    expect(scores.size).toBe(0);
  });
});
