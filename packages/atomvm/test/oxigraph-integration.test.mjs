/**
 * @fileoverview Oxigraph Integration Tests
 *
 * Tests the integration between OxigraphBridge and real @unrdf/oxigraph.
 * Verifies roundtrip operations with actual Oxigraph store.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createOxigraphStore,
  createOxigraphBridge,
  createIntegratedStore,
  dataFactory,
} from '../src/oxigraph-integration.mjs';

const { namedNode, literal, quad } = dataFactory;

describe('Oxigraph Integration', () => {
  describe('createOxigraphStore', () => {
    it('should create empty store', () => {
      const store = createOxigraphStore();

      expect(store).toBeDefined();
      expect(store.size).toBe(0);
    });

    it('should create store with initial quads', () => {
      const initialQuads = [
        quad(
          namedNode('http://example.org/s1'),
          namedNode('http://example.org/p1'),
          literal('v1')
        ),
        quad(
          namedNode('http://example.org/s2'),
          namedNode('http://example.org/p2'),
          literal('v2')
        ),
      ];

      const store = createOxigraphStore(initialQuads);

      expect(store.size).toBe(2);
    });

    it('should support add operation', () => {
      const store = createOxigraphStore();
      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('value')
      );

      store.add(q);

      expect(store.size).toBe(1);
      expect(store.has(q)).toBe(true);
    });

    it('should support match operation', () => {
      const store = createOxigraphStore();
      const subject = namedNode('http://example.org/subject');
      const predicate = namedNode('http://example.org/predicate');
      const object = literal('value');

      store.add(quad(subject, predicate, object));

      const matches = store.match(subject, null, null);
      expect(matches).toHaveLength(1);
      expect(matches[0].subject.value).toBe('http://example.org/subject');
    });

    it('should support delete operation', () => {
      const store = createOxigraphStore();
      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('value')
      );

      store.add(q);
      expect(store.size).toBe(1);

      store.delete(q);
      expect(store.size).toBe(0);
    });
  });

  describe('createOxigraphBridge', () => {
    it('should create bridge with empty store', () => {
      const bridge = createOxigraphBridge();

      expect(bridge).toBeDefined();
      expect(bridge.state).toBe('Ready');
      expect(bridge.isReady()).toBe(true);
    });

    it('should create bridge with initial quads', () => {
      const initialQuads = [
        quad(
          namedNode('http://example.org/s'),
          namedNode('http://example.org/p'),
          literal('v')
        ),
      ];

      const bridge = createOxigraphBridge(initialQuads);

      expect(bridge.state).toBe('Ready');
    });
  });

  describe('Integration Tests - Add and Query Roundtrip', () => {
    let bridge;

    beforeEach(() => {
      bridge = createOxigraphBridge();
    });

    it('should add triple and query it back', async () => {
      const subject = namedNode('http://example.org/person/1');
      const predicate = namedNode('http://schema.org/name');
      const object = literal('Alice');

      // Add triple
      const addResult = await bridge.addTriples([
        { subject, predicate, object },
      ]);

      expect(addResult.success).toBe(true);
      expect(addResult.count).toBe(1);
      expect(addResult.errors).toHaveLength(0);

      // Query it back
      const queryResult = await bridge.queryPattern(subject, null, null);

      expect(queryResult).toHaveLength(1);
      expect(queryResult[0].subject.value).toBe('http://example.org/person/1');
      expect(queryResult[0].predicate.value).toBe('http://schema.org/name');
      expect(queryResult[0].object.value).toBe('Alice');
    });

    it('should add multiple triples and query by pattern', async () => {
      const person1 = namedNode('http://example.org/person/1');
      const person2 = namedNode('http://example.org/person/2');
      const namePred = namedNode('http://schema.org/name');
      const agePred = namedNode('http://schema.org/age');

      // Add multiple triples
      await bridge.addTriples([
        { subject: person1, predicate: namePred, object: literal('Alice') },
        { subject: person1, predicate: agePred, object: literal('30') },
        { subject: person2, predicate: namePred, object: literal('Bob') },
        { subject: person2, predicate: agePred, object: literal('25') },
      ]);

      // Query all triples for person1
      const person1Triples = await bridge.queryPattern(person1, null, null);
      expect(person1Triples).toHaveLength(2);

      // Query all name predicates
      const nameTriples = await bridge.queryPattern(null, namePred, null);
      expect(nameTriples).toHaveLength(2);

      // Query all triples
      const allTriples = await bridge.getAllTriples();
      expect(allTriples).toHaveLength(4);
    });

    it('should support s/p/o shorthand format', async () => {
      const s = namedNode('http://example.org/s');
      const p = namedNode('http://example.org/p');
      const o = literal('value');

      // Add using shorthand
      await bridge.addTriples([{ s, p, o }]);

      // Query back
      const results = await bridge.queryPattern(s, null, null);
      expect(results).toHaveLength(1);
    });
  });

  describe('Integration Tests - Delete Operations', () => {
    let bridge;

    beforeEach(() => {
      bridge = createOxigraphBridge();
    });

    it('should delete triple and verify it is gone', async () => {
      const subject = namedNode('http://example.org/item/1');
      const predicate = namedNode('http://schema.org/name');
      const object = literal('Item 1');

      // Add triple
      await bridge.addTriples([{ subject, predicate, object }]);

      // Verify it exists
      let results = await bridge.queryPattern(subject, null, null);
      expect(results).toHaveLength(1);

      // Delete triple
      const deleteResult = await bridge.removeTriples([
        { subject, predicate, object },
      ]);

      expect(deleteResult.success).toBe(true);
      expect(deleteResult.count).toBe(1);

      // Verify it is gone
      results = await bridge.queryPattern(subject, null, null);
      expect(results).toHaveLength(0);
    });

    it('should delete multiple triples in batch', async () => {
      const s1 = namedNode('http://example.org/s1');
      const s2 = namedNode('http://example.org/s2');
      const p = namedNode('http://example.org/p');
      const o1 = literal('v1');
      const o2 = literal('v2');

      // Add two triples
      await bridge.addTriples([
        { subject: s1, predicate: p, object: o1 },
        { subject: s2, predicate: p, object: o2 },
      ]);

      expect((await bridge.getAllTriples())).toHaveLength(2);

      // Delete both
      await bridge.removeTriples([
        { subject: s1, predicate: p, object: o1 },
        { subject: s2, predicate: p, object: o2 },
      ]);

      expect((await bridge.getAllTriples())).toHaveLength(0);
    });
  });

  describe('Integration Tests - SPARQL Query', () => {
    let bridge;
    let store;

    beforeEach(() => {
      const integrated = createIntegratedStore();
      store = integrated.store;
      bridge = integrated.bridge;
    });

    it('should execute SPARQL SELECT query', async () => {
      // Add test data
      const s = namedNode('http://example.org/subject');
      const p = namedNode('http://example.org/predicate');
      const o = literal('value');

      await bridge.addTriples([{ subject: s, predicate: p, object: o }]);

      // Execute SELECT query
      const results = await bridge.sparqlQuery(
        'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
      );

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });

    it('should execute SPARQL ASK query', async () => {
      // Add test data
      await bridge.addTriples([
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('v'),
        },
      ]);

      // Execute ASK query - should return true
      const exists = await bridge.sparqlQuery(
        'ASK { <http://example.org/s> <http://example.org/p> "v" }'
      );

      expect(typeof exists).toBe('boolean');
      expect(exists).toBe(true);

      // ASK for non-existent triple - should return false
      const notExists = await bridge.sparqlQuery(
        'ASK { <http://example.org/missing> ?p ?o }'
      );

      expect(notExists).toBe(false);
    });

    it('should use store directly for SPARQL queries', () => {
      // Add data directly to store
      store.add(
        quad(
          namedNode('http://example.org/book/1'),
          namedNode('http://purl.org/dc/elements/1.1/title'),
          literal('The Great Book')
        )
      );

      // Query using store directly
      const results = store.query(
        'SELECT ?title WHERE { ?book <http://purl.org/dc/elements/1.1/title> ?title }'
      );

      expect(results).toBeDefined();
    });
  });

  describe('createIntegratedStore', () => {
    it('should create both store and bridge', () => {
      const { store, bridge } = createIntegratedStore();

      expect(store).toBeDefined();
      expect(bridge).toBeDefined();
      expect(bridge.state).toBe('Ready');
    });

    it('should share data between store and bridge', async () => {
      const { store, bridge } = createIntegratedStore();

      // Add via bridge
      await bridge.addTriples([
        {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        },
      ]);

      // Verify via store
      expect(store.size).toBe(1);
      const matches = store.match(null, null, null);
      expect(matches).toHaveLength(1);
    });

    it('should create integrated store with initial quads', () => {
      const initialQuads = [
        quad(
          namedNode('http://example.org/s'),
          namedNode('http://example.org/p'),
          literal('v')
        ),
      ];

      const { store, bridge } = createIntegratedStore(initialQuads);

      expect(store.size).toBe(1);
    });
  });

  describe('dataFactory export', () => {
    it('should export namedNode factory', () => {
      const node = namedNode('http://example.org/test');

      expect(node).toBeDefined();
      expect(node.value).toBe('http://example.org/test');
      expect(node.termType).toBe('NamedNode');
    });

    it('should export literal factory', () => {
      const lit = literal('test value');

      expect(lit).toBeDefined();
      expect(lit.value).toBe('test value');
      expect(lit.termType).toBe('Literal');
    });

    it('should export quad factory', () => {
      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      );

      expect(q).toBeDefined();
      expect(q.subject.value).toBe('http://example.org/s');
      expect(q.predicate.value).toBe('http://example.org/p');
      expect(q.object.value).toBe('o');
    });
  });

  describe('End-to-End Workflow', () => {
    it('should support complete RDF workflow', async () => {
      // Step 1: Create integrated store
      const { store, bridge } = createIntegratedStore();

      // Step 2: Add data via bridge
      const book1 = namedNode('http://example.org/book/1');
      const book2 = namedNode('http://example.org/book/2');
      const titlePred = namedNode('http://purl.org/dc/elements/1.1/title');
      const authorPred = namedNode('http://purl.org/dc/elements/1.1/creator');

      await bridge.addTriples([
        { subject: book1, predicate: titlePred, object: literal('Book One') },
        { subject: book1, predicate: authorPred, object: literal('Author A') },
        { subject: book2, predicate: titlePred, object: literal('Book Two') },
        { subject: book2, predicate: authorPred, object: literal('Author B') },
      ]);

      // Step 3: Verify via pattern matching
      const book1Data = await bridge.queryPattern(book1, null, null);
      expect(book1Data).toHaveLength(2);

      // Step 4: Query via SPARQL
      const titles = await bridge.sparqlQuery(
        `SELECT ?title WHERE {
          ?book <http://purl.org/dc/elements/1.1/title> ?title
        }`
      );
      expect(titles.length).toBeGreaterThanOrEqual(2);

      // Step 5: Delete one book
      await bridge.removeTriples([
        { subject: book1, predicate: titlePred, object: literal('Book One') },
        { subject: book1, predicate: authorPred, object: literal('Author A') },
      ]);

      // Step 6: Verify deletion
      const remaining = await bridge.getAllTriples();
      expect(remaining).toHaveLength(2);

      // Step 7: Verify via store directly
      expect(store.size).toBe(2);
    });
  });
});
