/**
 * @file Application SPARQL Smoke Tests (80/20 fast suite)
 * Converted from JTBD benchmarks to basic functionality tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore as createOxigraphStore, dataFactory } from '../src/index.mjs';

describe('Application SPARQL Functionality', () => {
  let store;

  beforeEach(() => {
    store = createOxigraphStore();
  });

  it('should handle search/autocomplete patterns', () => {
    const type = dataFactory.namedNode('http://schema.org/Person');
    const name = dataFactory.namedNode('http://schema.org/name');

    for (let i = 0; i < 5; i++) {
      const person = dataFactory.namedNode(`http://example.com/person/${i}`);
      store.add(
        dataFactory.triple(person, type, dataFactory.namedNode('http://schema.org/Person'))
      );
      store.add(dataFactory.triple(person, name, dataFactory.literal(`Alice Person ${i}`)));
    }

    const query = `
      SELECT ?name WHERE {
        ?person a <http://schema.org/Person> ;
                <http://schema.org/name> ?name .
        FILTER (STRSTARTS(?name, "Alice"))
      }
      LIMIT 10
    `;

    const results = store.query(query);
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeGreaterThan(0);
  });

  it('should handle entity detail view patterns', () => {
    const alice = dataFactory.namedNode('http://example.org/alice');
    const type = dataFactory.namedNode('http://schema.org/Person');
    const name = dataFactory.namedNode('http://schema.org/name');
    const email = dataFactory.namedNode('http://schema.org/email');

    store.add(dataFactory.triple(alice, type, dataFactory.namedNode('http://schema.org/Person')));
    store.add(dataFactory.triple(alice, name, dataFactory.literal('Alice')));
    store.add(dataFactory.triple(alice, email, dataFactory.literal('alice@example.com')));

    const query = `
      SELECT ?name ?email WHERE {
        ?person a <http://schema.org/Person> ;
                <http://schema.org/name> ?name ;
                <http://schema.org/email> ?email .
      }
    `;

    const results = store.query(query);
    expect(Array.isArray(results)).toBe(true);
  });

  it('should handle graph traversal patterns', () => {
    const alice = dataFactory.namedNode('http://example.org/alice');
    const bob = dataFactory.namedNode('http://example.org/bob');
    const knows = dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows');

    store.add(dataFactory.triple(alice, knows, bob));

    const query = `
      SELECT ?person WHERE {
        <http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> ?person .
      }
    `;

    const results = store.query(query);
    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBe(1);
  });
});
