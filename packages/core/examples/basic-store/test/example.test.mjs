/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { DataFactory, Store } from 'n3';
import {
  createSampleStore,
  queryStore,
  removeQuads,
  exportToNTriples,
  getStoreStats
} from '../src/index.mjs';

const { namedNode, literal } = DataFactory;

describe('Basic Store Operations', () => {
  it('creates store with sample data', () => {
    const store = createSampleStore();
    expect(store.size).toBe(4);
  });

  it('queries all quads from store', () => {
    const store = createSampleStore();
    const quads = queryStore(store);
    expect(quads).toHaveLength(4);
  });

  it('queries quads by subject pattern', () => {
    const store = createSampleStore();
    const aliceQuads = queryStore(store, {
      subject: namedNode('http://example.org/alice')
    });
    expect(aliceQuads).toHaveLength(3);
  });

  it('queries quads by predicate pattern', () => {
    const store = createSampleStore();
    const nameQuads = queryStore(store, {
      predicate: namedNode('http://xmlns.com/foaf/0.1/name')
    });
    expect(nameQuads).toHaveLength(2);
  });

  it('removes quads matching pattern', () => {
    const store = createSampleStore();
    const removed = removeQuads(store, {
      predicate: namedNode('http://xmlns.com/foaf/0.1/age')
    });
    expect(removed).toBe(1);
    expect(store.size).toBe(3);
  });

  it('exports store to N-Triples format', () => {
    const store = createSampleStore();
    const ntriples = exportToNTriples(store);
    expect(ntriples).toContain('<http://example.org/alice>');
    expect(ntriples).toContain('<http://xmlns.com/foaf/0.1/name>');
    expect(ntriples).toContain('"Alice"');
  });

  it('calculates store statistics', () => {
    const store = createSampleStore();
    const stats = getStoreStats(store);
    expect(stats.totalQuads).toBe(4);
    expect(stats.uniqueSubjects).toBe(2);
    expect(stats.uniquePredicates).toBe(3);
  });

  it('queries quads by object pattern', () => {
    const store = createSampleStore();
    const bobQuads = queryStore(store, {
      object: namedNode('http://example.org/bob')
    });
    expect(bobQuads).toHaveLength(1);
    expect(bobQuads[0].predicate.value).toBe('http://xmlns.com/foaf/0.1/knows');
  });

  it('queries quads by multiple patterns', () => {
    const store = createSampleStore();
    const aliceNameQuads = queryStore(store, {
      subject: namedNode('http://example.org/alice'),
      predicate: namedNode('http://xmlns.com/foaf/0.1/name')
    });
    expect(aliceNameQuads).toHaveLength(1);
    expect(aliceNameQuads[0].object.value).toBe('Alice');
  });

  it('handles empty store gracefully', () => {
    const emptyStore = new Store();
    const quads = queryStore(emptyStore);
    expect(quads).toHaveLength(0);
    const stats = getStoreStats(emptyStore);
    expect(stats.totalQuads).toBe(0);
  });

  it('handles removal of non-existent patterns', () => {
    const store = createSampleStore();
    const removed = removeQuads(store, {
      predicate: namedNode('http://example.org/nonexistent')
    });
    expect(removed).toBe(0);
    expect(store.size).toBe(4);
  });

  it('removes all quads matching subject', () => {
    const store = createSampleStore();
    const removed = removeQuads(store, {
      subject: namedNode('http://example.org/alice')
    });
    expect(removed).toBe(3);
    expect(store.size).toBe(1);
  });

  it('exports empty store', () => {
    const emptyStore = new Store();
    const ntriples = exportToNTriples(emptyStore);
    expect(ntriples).toBe('');
  });

  it('handles duplicate quads', () => {
    const store = createSampleStore();
    const initialSize = store.size;
    store.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    );
    expect(store.size).toBe(initialSize);
  });

  it('exports typed literals correctly', () => {
    const store = createSampleStore();
    const ntriples = exportToNTriples(store);
    expect(ntriples).toContain('^^<http://www.w3.org/2001/XMLSchema#integer>');
  });

  it('counts unique objects in statistics', () => {
    const store = createSampleStore();
    const stats = getStoreStats(store);
    expect(stats.uniqueObjects).toBe(4);
  });
});

describe('Edge Cases and Error Handling', () => {
  it('handles null pattern queries', () => {
    const store = createSampleStore();
    const quads = queryStore(store, {});
    expect(quads).toHaveLength(4);
  });

  it('handles queries with all null patterns', () => {
    const store = createSampleStore();
    const quads = queryStore(store, {
      subject: null,
      predicate: null,
      object: null
    });
    expect(quads).toHaveLength(4);
  });

  it('exports store with blank node subjects', () => {
    const store = new Store();
    store.addQuad(
      { termType: 'BlankNode', value: '_:b1' },
      namedNode('http://example.org/pred'),
      literal('value')
    );
    const ntriples = exportToNTriples(store);
    expect(ntriples).toContain('_:b1');
  });

  it('exports store with typed literals', () => {
    const store = new Store();
    store.addQuad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/pred'),
      literal('123', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    );
    const ntriples = exportToNTriples(store);
    expect(ntriples).toContain('^^<http://www.w3.org/2001/XMLSchema#integer>');
  });

  it('exports store with string literals (no datatype)', () => {
    const store = new Store();
    store.addQuad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/pred'),
      literal('plain string')
    );
    const ntriples = exportToNTriples(store);
    expect(ntriples).toContain('"plain string"');
  });
});
