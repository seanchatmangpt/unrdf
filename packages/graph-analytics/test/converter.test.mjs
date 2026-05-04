import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import { rdfToGraph, getGraphStats } from '../src/converter/rdf-to-graph.mjs';

const { namedNode, triple } = dataFactory;

describe('RDF to Graph Converter', () => {
  it('should convert empty store to empty graph', () => {
    const store = createStore();
    const graph = rdfToGraph(store);

    expect(graph.nodeCount()).toBe(0);
    expect(graph.edgeCount()).toBe(0);
  });

  it('should convert simple triple to graph', () => {
    const store = createStore();
    const subject = namedNode('http://example.org/Alice');
    const predicate = namedNode('http://schema.org/knows');
    const object = namedNode('http://example.org/Bob');

    store.add(triple(subject, predicate, object));

    const graph = rdfToGraph(store);

    expect(graph.nodeCount()).toBe(2);
    expect(graph.edgeCount()).toBe(1);
    expect(graph.hasNode('http://example.org/Alice')).toBe(true);
    expect(graph.hasNode('http://example.org/Bob')).toBe(true);
  });

  it('should include edge metadata', () => {
    const store = createStore();
    const subject = namedNode('http://example.org/Alice');
    const predicate = namedNode('http://schema.org/knows');
    const object = namedNode('http://example.org/Bob');

    store.add(triple(subject, predicate, object));

    const graph = rdfToGraph(store);
    const edge = graph.edge('http://example.org/Alice', 'http://example.org/Bob');

    expect(edge.predicate).toBe('http://schema.org/knows');
    expect(edge.weight).toBe(1);
  });

  it('should compute graph statistics', () => {
    const store = createStore();

    // Create triangle
    const a = namedNode('http://example.org/A');
    const b = namedNode('http://example.org/B');
    const c = namedNode('http://example.org/C');
    const knows = namedNode('http://schema.org/knows');

    store.add(triple(a, knows, b));
    store.add(triple(b, knows, c));
    store.add(triple(c, knows, a));

    const graph = rdfToGraph(store);
    const stats = getGraphStats(graph);

    expect(stats.nodeCount).toBe(3);
    expect(stats.edgeCount).toBe(3);
    expect(stats.averageDegree).toBeGreaterThan(0);
    expect(stats.isDirected).toBe(true);
  });
});
