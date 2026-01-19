/**
 * @file Documentation Alignment Tests (80/20 fast suite)
 * @description Validates that documented APIs work as described
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';

describe('Documentation Alignment', () => {
  let store;
  const { namedNode, literal, quad } = dataFactory;

  beforeEach(() => {
    store = createStore();
  });

  it('createStore() and basic operations work as documented', () => {
    expect(store).toBeDefined();
    expect(store.size).toBe(0);

    const alice = namedNode('http://example.org/alice');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const bob = namedNode('http://example.org/bob');

    store.add(quad(alice, knows, bob));
    expect(store.size).toBe(1);
  });

  it('dataFactory creates valid RDF terms', () => {
    const subject = namedNode('http://example.org/resource');
    const predicate = namedNode('http://example.org/property');
    const object = literal('value');

    expect(subject).toBeDefined();
    expect(predicate).toBeDefined();
    expect(object).toBeDefined();
  });

  it('complete quickstart workflow', () => {
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');

    store.add(quad(alice, knows, bob));
    const results = store.query('SELECT ?s ?o WHERE { ?s <http://xmlns.com/foaf/0.1/knows> ?o }');

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeGreaterThan(0);
  });
});
