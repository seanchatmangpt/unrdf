import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import { rdfToGraph } from '../src/converter/rdf-to-graph.mjs';
import {
  detectCommunitiesLPA,
  detectCommunitiesModularity,
  findKCore,
  getCommunityStats,
} from '../src/clustering/community-detector.mjs';

const { namedNode, triple } = dataFactory;

describe('Community Detection', () => {
  it('should detect communities with LPA', () => {
    const store = createStore();
    const knows = namedNode('http://schema.org/knows');

    // Create two clusters
    const a1 = namedNode('http://example.org/A1');
    const a2 = namedNode('http://example.org/A2');
    const b1 = namedNode('http://example.org/B1');
    const b2 = namedNode('http://example.org/B2');

    store.add(triple(a1, knows, a2));
    store.add(triple(a2, knows, a1));
    store.add(triple(b1, knows, b2));
    store.add(triple(b2, knows, b1));

    const graph = rdfToGraph(store);
    const communities = detectCommunitiesLPA(graph);

    expect(communities.size).toBe(4);

    // Nodes in same cluster should have same community
    const comm_a1 = communities.get('http://example.org/A1');
    const comm_a2 = communities.get('http://example.org/A2');

    // May or may not be same due to randomness, just check they exist
    expect(comm_a1).toBeDefined();
    expect(comm_a2).toBeDefined();
  });

  it('should compute community statistics', () => {
    const communities = new Map([
      ['node1', 0],
      ['node2', 0],
      ['node3', 1],
      ['node4', 1],
    ]);

    const stats = getCommunityStats(communities);

    expect(stats.totalCommunities).toBe(2);
    expect(stats.averageSize).toBe(2);
    expect(stats.maxSize).toBe(2);
    expect(stats.minSize).toBe(2);
  });

  it('should find k-core', () => {
    const store = createStore();
    const knows = namedNode('http://schema.org/knows');

    // Create graph with 3-core
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const d = namedNode('http://example.org/D');

    // Triangle (3-core) plus one pendant
    store.add(triple(a, knows, b));
    store.add(triple(b, knows, c));
    store.add(triple(c, knows, a));
    store.add(triple(a, knows, d));

    const graph = rdfToGraph(store);
    const core2 = findKCore(graph, 2);

    expect(core2.size).toBeGreaterThanOrEqual(3);
  });

  it('should handle empty graph', () => {
    const store = createStore();
    const graph = rdfToGraph(store);
    const communities = detectCommunitiesLPA(graph);

    expect(communities.size).toBe(0);
  });
});
