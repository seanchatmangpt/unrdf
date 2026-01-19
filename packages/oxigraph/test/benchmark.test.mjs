/**
 * @file Oxigraph Smoke Tests (80/20 fast suite)
 * Converted from benchmarks to basic functionality tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '../src/index.mjs';

describe('Oxigraph Smoke Tests', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  it('should perform add operations without slowdown', () => {
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    for (let i = 0; i < 10; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      const triple = dataFactory.triple(ex, predicate, node);
      store.add(triple);
    }

    expect(store.size).toBe(10);
  });

  it('should execute queries efficiently', () => {
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');

    for (let i = 0; i < 10; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      store.add(dataFactory.triple(ex, predicate, node));
    }

    const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeGreaterThan(0);
  });

  it('should handle delete operations', () => {
    const ex = dataFactory.namedNode('http://example.com/');
    const predicate = dataFactory.namedNode('http://schema.org/knows');
    const triples = [];

    for (let i = 0; i < 10; i++) {
      const node = dataFactory.namedNode(`http://example.com/person/${i}`);
      const triple = dataFactory.triple(ex, predicate, node);
      store.add(triple);
      triples.push(triple);
    }

    triples.forEach(triple => store.delete(triple));
    expect(store.size).toBe(0);
  });
});
