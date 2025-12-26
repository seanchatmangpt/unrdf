/**
 * @file Documentation Alignment Tests
 * @description Validates that documented APIs actually work as described.
 * These tests ensure code-documentation alignment for DX.
 */

import { describe, it, expect, beforeEach } from 'vitest';

// Import from @unrdf/oxigraph (as documented in quickstart)
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Import from @unrdf/core (as documented)
import {
  executeSelectSync,
  executeAskSync,
  executeConstructSync,
} from '../src/index.mjs';

describe('Documentation Alignment - Quickstart Tutorial', () => {
  let store;
  const { namedNode, literal, quad } = dataFactory;

  beforeEach(() => {
    store = createStore();
  });

  it('createStore() works as documented', () => {
    expect(store).toBeDefined();
    expect(store.size).toBe(0);
  });

  it('dataFactory exports work as documented', () => {
    expect(namedNode).toBeDefined();
    expect(literal).toBeDefined();
    expect(quad).toBeDefined();
  });

  it('store.add() works as documented', () => {
    const alice = namedNode('http://example.org/alice');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const bob = namedNode('http://example.org/bob');

    store.add(quad(alice, knows, bob));

    expect(store.size).toBe(1);
  });

  it('complete quickstart example works', () => {
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.add(quad(alice, knows, bob));
    store.add(quad(alice, name, literal('Alice')));
    store.add(quad(bob, name, literal('Bob')));

    expect(store.size).toBe(3);
  });
});

describe('Documentation Alignment - SPARQL APIs', () => {
  let store;
  const { namedNode, literal, quad } = dataFactory;

  beforeEach(() => {
    store = createStore();

    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.add(quad(alice, knows, bob));
    store.add(quad(alice, name, literal('Alice')));
    store.add(quad(bob, name, literal('Bob')));
  });

  it('executeSelectSync() works as documented', () => {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person ?friendName WHERE {
        ?person foaf:knows ?friend .
        ?friend foaf:name ?friendName .
      }
    `;

    const results = executeSelectSync(store, query);

    expect(results).toBeDefined();
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(1);

    const binding = results[0];
    // Handle both Map-style .get() and object-style property access
    const person = binding.get ? binding.get('person') : binding.person;
    const friendName = binding.get ? binding.get('friendName') : binding.friendName;
    expect(person.value).toBe('http://example.org/alice');
    expect(friendName.value).toBe('Bob');
  });

  it('executeAskSync() works as documented', () => {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      ASK { ?s foaf:knows ?o }
    `;

    const result = executeAskSync(store, query);

    expect(typeof result).toBe('boolean');
    expect(result).toBe(true);
  });

  it('executeConstructSync() works as documented', () => {
    const query = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ex: <http://example.org/>
      CONSTRUCT { ?person ex:hasFriend ?friend }
      WHERE { ?person foaf:knows ?friend }
    `;

    const results = executeConstructSync(store, query);

    expect(results).toBeDefined();
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(1);
  });
});

describe('Documentation Alignment - RDF Terms', () => {
  const { namedNode, literal, blankNode, quad, defaultGraph } = dataFactory;

  it('namedNode() creates valid IRI', () => {
    const node = namedNode('http://example.org/test');
    expect(node.termType).toBe('NamedNode');
    expect(node.value).toBe('http://example.org/test');
  });

  it('literal() creates string literal', () => {
    const lit = literal('Hello World');
    expect(lit.termType).toBe('Literal');
    expect(lit.value).toBe('Hello World');
  });

  it('literal() creates typed literal', () => {
    const xsdInteger = namedNode('http://www.w3.org/2001/XMLSchema#integer');
    const lit = literal('42', xsdInteger);
    expect(lit.value).toBe('42');
    expect(lit.datatype.value).toBe('http://www.w3.org/2001/XMLSchema#integer');
  });

  it('blankNode() creates blank node', () => {
    const bn = blankNode();
    expect(bn.termType).toBe('BlankNode');
  });

  it('defaultGraph() returns default graph', () => {
    const dg = defaultGraph();
    expect(dg.termType).toBe('DefaultGraph');
  });

  it('quad() creates valid quad', () => {
    const s = namedNode('http://example.org/s');
    const p = namedNode('http://example.org/p');
    const o = literal('object');
    const q = quad(s, p, o);

    expect(q.subject.value).toBe('http://example.org/s');
    expect(q.predicate.value).toBe('http://example.org/p');
    expect(q.object.value).toBe('object');
  });
});

describe('Documentation Alignment - Store Operations', () => {
  let store;
  const { namedNode, literal, quad } = dataFactory;

  beforeEach(() => {
    store = createStore();
  });

  it('store.size returns correct count', () => {
    expect(store.size).toBe(0);

    store.add(quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('o')
    ));

    expect(store.size).toBe(1);
  });

  it('store.has() checks quad existence', () => {
    const q = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('o')
    );

    expect(store.has(q)).toBe(false);
    store.add(q);
    expect(store.has(q)).toBe(true);
  });

  it('store.delete() removes quad', () => {
    const q = quad(
      namedNode('http://example.org/s'),
      namedNode('http://example.org/p'),
      literal('o')
    );

    store.add(q);
    expect(store.size).toBe(1);

    store.delete(q);
    expect(store.size).toBe(0);
  });

  it('store.match() finds quads by pattern', () => {
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');

    store.add(quad(alice, knows, bob));

    const matches = [...store.match(alice, null, null)];
    expect(matches.length).toBe(1);
    expect(matches[0].object.value).toBe('http://example.org/bob');
  });
});
