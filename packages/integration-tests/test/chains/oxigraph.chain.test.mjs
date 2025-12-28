/**
 * Oxigraph Chain Integration Tests
 * Phase 5: 10 tests covering Store CRUD, SPARQL, Streaming, Federation, Persistence
 *
 * @module @unrdf/integration-tests/test/chains/oxigraph
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad: createQuad, defaultGraph } = dataFactory;

/**
 * Helper: Generate test quads
 * @param {number} count - Number of quads to generate
 * @returns {Array} Array of quads
 */
function generateTestQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push(createQuad(
      namedNode(`http://example.org/subject/${i}`),
      namedNode('http://example.org/predicate/value'),
      literal(`value-${i}`),
      defaultGraph()
    ));
  }
  return quads;
}

describe('Oxigraph Chain Integration Tests', () => {
  /** @type {import('@unrdf/oxigraph').OxigraphStore} */
  let store;

  beforeEach(() => {
    store = createStore();
  });

  afterEach(() => {
    store = null;
  });

  // Test 1: Store CRUD - Create
  it('should create store and add quads', () => {
    const quad = createQuad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('object value'),
      defaultGraph()
    );

    store.add(quad);

    expect(store.size).toBe(1);
    expect(store.has(quad)).toBe(true);
  });

  // Test 2: Store CRUD - Add 1000 quads
  it('should add 1000 quads efficiently', () => {
    const quads = generateTestQuads(1000);
    const startTime = performance.now();

    quads.forEach(q => store.add(q));

    const duration = performance.now() - startTime;

    expect(store.size).toBe(1000);
    expect(duration).toBeLessThan(1000); // <1s for 1000 quads
  });

  // Test 3: Store CRUD - Query/Match
  it('should query quads by pattern', () => {
    const quads = generateTestQuads(100);
    quads.forEach(q => store.add(q));

    // Query specific subject
    const subject = namedNode('http://example.org/subject/50');
    const matches = store.match(subject, null, null, null);

    expect(matches.length).toBe(1);
    expect(matches[0].subject.value).toBe('http://example.org/subject/50');
  });

  // Test 4: Store CRUD - Delete
  it('should delete quads correctly', () => {
    const quad1 = createQuad(
      namedNode('http://example.org/s1'),
      namedNode('http://example.org/p1'),
      literal('v1'),
      defaultGraph()
    );
    const quad2 = createQuad(
      namedNode('http://example.org/s2'),
      namedNode('http://example.org/p2'),
      literal('v2'),
      defaultGraph()
    );

    store.add(quad1);
    store.add(quad2);
    expect(store.size).toBe(2);

    store.delete(quad1);
    expect(store.size).toBe(1);
    expect(store.has(quad1)).toBe(false);
    expect(store.has(quad2)).toBe(true);
  });

  // Test 5: SPARQL - Simple SELECT query
  it('should execute simple SPARQL SELECT query', () => {
    const quads = [
      createQuad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice'),
        defaultGraph()
      ),
      createQuad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob'),
        defaultGraph()
      ),
    ];
    quads.forEach(q => store.add(q));

    const results = store.query(`
      SELECT ?person ?name WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
      }
    `);

    expect(Array.from(results).length).toBe(2);
  });

  // Test 6: SPARQL - Complex query with FILTER
  it('should execute complex SPARQL query with FILTER', () => {
    // Add test data with numeric values
    for (let i = 0; i < 10; i++) {
      store.add(createQuad(
        namedNode(`http://example.org/item/${i}`),
        namedNode('http://example.org/value'),
        literal(i.toString(), namedNode('http://www.w3.org/2001/XMLSchema#integer')),
        defaultGraph()
      ));
    }

    const results = store.query(`
      SELECT ?item ?val WHERE {
        ?item <http://example.org/value> ?val .
        FILTER(?val > 5)
      }
    `);

    const resultArray = Array.from(results);
    expect(resultArray.length).toBe(4); // 6, 7, 8, 9
  });

  // Test 7: Streaming inserts - 10k quads via batches
  it('should handle streaming inserts of 10k quads', async () => {
    const BATCH_SIZE = 1000;
    const TOTAL_QUADS = 10000;
    const startTime = performance.now();

    for (let batch = 0; batch < TOTAL_QUADS / BATCH_SIZE; batch++) {
      const quads = generateTestQuads(BATCH_SIZE);
      quads.forEach((q, i) => {
        // Create unique subject for each quad
        const uniqueQuad = createQuad(
          namedNode(`http://example.org/subject/${batch * BATCH_SIZE + i}`),
          q.predicate,
          q.object,
          q.graph
        );
        store.add(uniqueQuad);
      });
    }

    const duration = performance.now() - startTime;

    expect(store.size).toBe(TOTAL_QUADS);
    expect(duration).toBeLessThan(5000); // <5s for 10k quads
  });

  // Test 8: Cross-store federation - 2 stores, JOIN query
  it('should support cross-store federation pattern', () => {
    const store1 = createStore();
    const store2 = createStore();

    // Store 1: Person data
    store1.add(createQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice'),
      defaultGraph()
    ));
    store1.add(createQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/worksAt'),
      namedNode('http://example.org/company/acme'),
      defaultGraph()
    ));

    // Store 2: Company data
    store2.add(createQuad(
      namedNode('http://example.org/company/acme'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('ACME Corp'),
      defaultGraph()
    ));

    // Federated query pattern: get Alice's company info
    const aliceCompany = store1.match(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/worksAt'),
      null,
      null
    );
    expect(aliceCompany.length).toBe(1);

    const companyUri = aliceCompany[0].object;
    const companyInfo = store2.match(companyUri, null, null, null);
    expect(companyInfo.length).toBe(1);
    expect(companyInfo[0].object.value).toBe('ACME Corp');
  });

  // Test 9: Store persistence - serialize/restore/verify
  it('should serialize and restore store state', () => {
    // Add data
    const quads = generateTestQuads(50);
    quads.forEach(q => store.add(q));

    // Serialize to N-Quads (required for dataset format)
    const serialized = store.dump({ format: 'application/n-quads' });
    expect(typeof serialized).toBe('string');
    expect(serialized.length).toBeGreaterThan(0);

    // Create new store and restore
    const restoredStore = createStore();
    restoredStore.load(serialized, { format: 'application/n-quads' });

    // Verify
    expect(restoredStore.size).toBe(50);

    // Verify specific quad exists
    const firstQuad = quads[0];
    const matches = restoredStore.match(firstQuad.subject, null, null, null);
    expect(matches.length).toBe(1);
  });

  // Test 10: Store clear and reinitialize
  it('should clear store and reinitialize correctly', () => {
    // Add data
    const quads = generateTestQuads(100);
    quads.forEach(q => store.add(q));
    expect(store.size).toBe(100);

    // Clear
    store.clear();
    expect(store.size).toBe(0);

    // Reinitialize with new data
    const newQuads = generateTestQuads(25);
    newQuads.forEach(q => store.add(q));
    expect(store.size).toBe(25);

    // Verify old data is gone
    const oldSubject = namedNode('http://example.org/subject/99');
    const matches = store.match(oldSubject, null, null, null);
    expect(matches.length).toBe(0);
  });
});
