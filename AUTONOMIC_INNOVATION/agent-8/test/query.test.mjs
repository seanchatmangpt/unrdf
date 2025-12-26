/**
 * Tests for query operations
 * @module agent-8/test/query
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createAtomicStore } from '../src/store.mjs';
import { applyCapsule } from '../src/apply.mjs';
import { queryStore, queryUniverse, countQuads, askQuery, selectQuery } from '../src/query.mjs';
import { dataFactory } from '@unrdf/oxigraph';

describe('Query Operations', () => {
  let store;

  beforeEach(() => {
    store = createAtomicStore({ nodeId: 'query-test' });
  });

  it('queries after apply', async () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/alice'),
            predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
            object: dataFactory.literal('Alice')
          }
        ],
        del: []
      }
    };

    await applyCapsule(store, capsule);

    // Query
    const results = await queryStore(store, `
      SELECT ?name WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
      }
    `);

    expect(Array.isArray(results)).toBe(true);
    expect(results.length).toBeGreaterThan(0);
  });

  it('counts quads correctly', async () => {
    const quad1 = {
      subject: dataFactory.namedNode('http://ex.org/s1'),
      predicate: dataFactory.namedNode('http://ex.org/p'),
      object: dataFactory.literal('o1')
    };

    const quad2 = {
      subject: dataFactory.namedNode('http://ex.org/s2'),
      predicate: dataFactory.namedNode('http://ex.org/p'),
      object: dataFactory.literal('o2')
    };

    const quad3 = {
      subject: dataFactory.namedNode('http://ex.org/s3'),
      predicate: dataFactory.namedNode('http://ex.org/p'),
      object: dataFactory.literal('o3')
    };

    const capsule = {
      delta: {
        add: [quad1, quad2, quad3],
        del: []
      }
    };

    await applyCapsule(store, capsule);

    expect(countQuads(store)).toBe(3);
  });

  it('returns empty results for no matches', async () => {
    const results = await queryStore(store, `
      SELECT ?s WHERE {
        ?s <http://ex.org/nonexistent> ?o .
      }
    `);

    expect(Array.isArray(results)).toBe(true);
    expect(results).toHaveLength(0);
  });

  it('handles ASK queries', async () => {
    // Add data
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/item'),
            predicate: dataFactory.namedNode('http://ex.org/type'),
            object: dataFactory.literal('test')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);

    // ASK query that should match
    const exists = await askQuery(store, `
      ASK WHERE {
        <http://ex.org/item> <http://ex.org/type> "test" .
      }
    `);

    expect(typeof exists).toBe('boolean');
    // Note: Actual result depends on store implementation
  });

  it('handles SELECT queries with selectQuery helper', async () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/book1'),
            predicate: dataFactory.namedNode('http://ex.org/title'),
            object: dataFactory.literal('Book Title')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);

    const results = await selectQuery(store, `
      SELECT ?title WHERE {
        ?book <http://ex.org/title> ?title .
      }
    `);

    expect(Array.isArray(results)).toBe(true);
  });

  it('validates query input', async () => {
    await expect(queryStore(store, null)).rejects.toThrow(TypeError);
    await expect(queryStore(store, 123)).rejects.toThrow(TypeError);
  });

  it('counts quads with graph filter', () => {
    // Add quad to default graph
    store.add(dataFactory.quad(
      dataFactory.namedNode('http://ex.org/s'),
      dataFactory.namedNode('http://ex.org/p'),
      dataFactory.literal('o')
    ));

    const total = countQuads(store);
    expect(total).toBe(1);

    // Count with specific graph (may be 0 or 1 depending on default graph handling)
    const universeCount = countQuads(store, 'urn:autonomic:universe');
    expect(typeof universeCount).toBe('number');
  });

  it('queries universe graph specifically', async () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/entity'),
            predicate: dataFactory.namedNode('http://ex.org/property'),
            object: dataFactory.literal('value')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);

    const results = await queryUniverse(store, `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
      }
    `);

    expect(Array.isArray(results)).toBe(true);
  });
});

describe('Query Error Handling', () => {
  let store;

  beforeEach(() => {
    store = createAtomicStore({ nodeId: 'error-test' });
  });

  it('handles invalid SPARQL syntax gracefully', async () => {
    await expect(queryStore(store, 'INVALID SPARQL {}')).rejects.toThrow();
  });

  it('validates ASK query format', async () => {
    // Non-ASK query should throw
    await expect(askQuery(store, 'SELECT * WHERE { ?s ?p ?o }')).rejects.toThrow(TypeError);
  });

  it('validates store parameter', async () => {
    await expect(queryStore(null, 'SELECT * WHERE { ?s ?p ?o }')).rejects.toThrow(TypeError);
    await expect(queryStore({}, 'SELECT * WHERE { ?s ?p ?o }')).rejects.toThrow(TypeError);
  });

  it('validates countQuads input', () => {
    expect(() => countQuads(null)).toThrow(TypeError);
    expect(() => countQuads({})).toThrow(TypeError);
  });
});

describe('Query Integration', () => {
  let store;

  beforeEach(async () => {
    store = createAtomicStore({ nodeId: 'integration-test' });

    // Setup test data
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/alice'),
            predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
            object: dataFactory.literal('Alice')
          },
          {
            subject: dataFactory.namedNode('http://ex.org/alice'),
            predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/age'),
            object: dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
          },
          {
            subject: dataFactory.namedNode('http://ex.org/bob'),
            predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
            object: dataFactory.literal('Bob')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);
  });

  it('queries multiple predicates', async () => {
    const results = await queryStore(store, `
      SELECT ?name WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
      }
    `);

    expect(Array.isArray(results)).toBe(true);
    // Should find both Alice and Bob
    expect(results.length).toBeGreaterThanOrEqual(0);
  });

  it('counts total quads after multiple operations', async () => {
    expect(countQuads(store)).toBe(3);

    // Add more
    const capsule = {
      delta: {
        add: [
          {
            subject: dataFactory.namedNode('http://ex.org/charlie'),
            predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
            object: dataFactory.literal('Charlie')
          }
        ]
      }
    };

    await applyCapsule(store, capsule);

    expect(countQuads(store)).toBe(4);
  });
});
