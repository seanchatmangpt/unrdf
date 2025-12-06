/**
 * @vitest-environment node
 * Comprehensive Test Suite for UnrdfStore
 * Coverage Target: 90%+ (Currently at 63.89%, need 26.11% more)
 * Pattern: Chicago School TDD
 *
 * CRITICAL GAPS TO COVER:
 * - queryAsync (async wrapper)
 * - bulkAdd (batch insert)
 * - bulkRemove (batch delete)
 * - transaction (atomic operations)
 * - update (SPARQL UPDATE)
 * - load (RDF serialization load)
 * - dump (RDF serialization export)
 */

import { describe, it, expect } from 'vitest';
import { createUnrdfStore, namedNode, literal, quad } from '../../src/index.mjs';

describe('UnrdfStore - Comprehensive Coverage Tests', () => {
  describe('Constructor and Initialization', () => {
    it('creates empty store with no arguments', () => {
      const store = createUnrdfStore();
      expect(store).toBeDefined();
      expect(store.size()).toBe(0);
    });

    it('creates store with initial quads', () => {
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];
      const store = createUnrdfStore(quads);

      expect(store.size()).toBe(2);
    });

    it('creates store with options', () => {
      const options = { baseIri: 'http://example.org/' };
      const store = createUnrdfStore([], options);

      expect(store).toBeDefined();
      expect(store._options).toEqual(options);
    });

    it('initializes version counter to 0', () => {
      const store = createUnrdfStore();
      expect(store.version).toBe(0);
    });
  });

  describe('queryAsync - Async Query Wrapper (CRITICAL GAP)', () => {
    it('executes SELECT query asynchronously', async () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('Alice')));

      const sparql = 'SELECT ?o WHERE { ?s ?p ?o }';
      const result = await store.queryAsync(sparql);

      expect(result).toBeDefined();
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(1);
      expect(result[0]).toHaveProperty('o');
    });

    it('executes ASK query asynchronously', async () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('Alice')));

      const sparql = 'ASK { ?s ?p "Alice" }';
      const result = await store.queryAsync(sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('executes CONSTRUCT query asynchronously', async () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('Alice')));

      const sparql = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
      const result = await store.queryAsync(sparql);

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
    });

    it('passes options through to synchronous query', async () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
      store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const options = { limit: 1, resultsFormat: 'json' };

      const result = await store.queryAsync(sparql, options);

      expect(result).toBeDefined();
      expect(result).toHaveProperty('head');
      expect(result).toHaveProperty('results');
    });

    it('returns Promise that resolves to query results', async () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const promise = store.queryAsync(sparql);

      expect(promise).toBeInstanceOf(Promise);
      const result = await promise;
      expect(result).toBeDefined();
    });

    it('handles empty store gracefully in async query', async () => {
      const store = createUnrdfStore();
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      const result = await store.queryAsync(sparql);

      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    it('propagates errors from synchronous query in async', async () => {
      const store = createUnrdfStore();
      const invalidSparql = 'INVALID QUERY SYNTAX {{{';

      await expect(store.queryAsync(invalidSparql)).rejects.toThrow();
    });
  });

  describe('bulkAdd - Batch Insert (CRITICAL GAP)', () => {
    it('adds multiple quads in single operation', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
        quad(namedNode('http://s3'), namedNode('http://p'), literal('o3')),
      ];

      store.bulkAdd(quads);

      expect(store.size()).toBe(3);
    });

    it('increments version once for entire bulk operation', () => {
      const store = createUnrdfStore();
      const initialVersion = store.version;

      const quads = Array.from({ length: 100 }, (_, i) =>
        quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`))
      );

      store.bulkAdd(quads);

      expect(store.version).toBe(initialVersion + 1);
    });

    it('handles empty array gracefully', () => {
      const store = createUnrdfStore();
      const initialSize = store.size();

      store.bulkAdd([]);

      expect(store.size()).toBe(initialSize);
    });

    it('throws TypeError for non-array input', () => {
      const store = createUnrdfStore();

      expect(() => store.bulkAdd('not an array')).toThrow(TypeError);
      expect(() => store.bulkAdd('not an array')).toThrow('bulkAdd: quads must be an array');
    });

    it('throws TypeError for null input', () => {
      const store = createUnrdfStore();

      expect(() => store.bulkAdd(null)).toThrow(TypeError);
    });

    it('throws TypeError for undefined input', () => {
      const store = createUnrdfStore();

      expect(() => store.bulkAdd(undefined)).toThrow(TypeError);
    });
  });

  describe('bulkRemove - Batch Delete (CRITICAL GAP)', () => {
    it('removes multiple quads in single operation', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
        quad(namedNode('http://s3'), namedNode('http://p'), literal('o3')),
      ];

      store.bulkAdd(quads);
      expect(store.size()).toBe(3);

      store.bulkRemove(quads);
      expect(store.size()).toBe(0);
    });

    it('increments version once for entire bulk operation', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];

      store.bulkAdd(quads);
      const versionBeforeRemove = store.version;

      store.bulkRemove(quads);

      expect(store.version).toBe(versionBeforeRemove + 1);
    });

    it('handles empty array gracefully', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      const initialSize = store.size();

      store.bulkRemove([]);

      expect(store.size()).toBe(initialSize);
    });

    it('throws TypeError for non-array input', () => {
      const store = createUnrdfStore();

      expect(() => store.bulkRemove('not an array')).toThrow(TypeError);
      expect(() => store.bulkRemove('not an array')).toThrow('bulkRemove: quads must be an array');
    });

    it('throws TypeError for null input', () => {
      const store = createUnrdfStore();

      expect(() => store.bulkRemove(null)).toThrow(TypeError);
    });

    it('handles partial removal (some quads not in store)', () => {
      const store = createUnrdfStore();
      const existingQuad = quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'));
      const nonExistentQuad = quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'));

      store.add(existingQuad);
      expect(store.size()).toBe(1);

      // Try to remove both (only one exists)
      store.bulkRemove([existingQuad, nonExistentQuad]);

      expect(store.size()).toBe(0);
    });
  });

  describe('transaction - Atomic Operations (CRITICAL GAP)', () => {
    it('executes operations atomically (all succeed)', () => {
      const store = createUnrdfStore();
      const q1 = quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'));
      const q2 = quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'));

      store.transaction(txStore => {
        txStore.add(q1);
        txStore.add(q2);
      });

      expect(store.size()).toBe(2);
    });

    it('rolls back on error (all fail)', () => {
      const store = createUnrdfStore();
      const q1 = quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'));

      store.add(q1);
      expect(store.size()).toBe(1);

      try {
        store.transaction(txStore => {
          txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
          throw new Error('Transaction failed');
        });
      } catch (error) {
        expect(error.message).toContain('Transaction failed');
      }

      // Store should be rolled back to original state
      expect(store.size()).toBe(1);
    });

    it('preserves original store state on rollback', () => {
      const store = createUnrdfStore();
      const q1 = quad(namedNode('http://s1'), namedNode('http://p'), literal('original'));

      store.add(q1);

      try {
        store.transaction(txStore => {
          txStore.delete(q1);
          txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('new')));
          throw new Error('Fail');
        });
      } catch (error) {
        // Expected
      }

      // Original quad should still be there
      expect(store.size()).toBe(1);
      expect(store.has(q1)).toBe(true);
    });

    it('handles multiple operations in transaction', () => {
      const store = createUnrdfStore();
      const q1 = quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'));
      const q2 = quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'));
      const q3 = quad(namedNode('http://s3'), namedNode('http://p'), literal('o3'));

      store.transaction(txStore => {
        txStore.add(q1);
        txStore.add(q2);
        txStore.add(q3);
        txStore.delete(q2);
      });

      expect(store.size()).toBe(2);
      expect(store.has(q1)).toBe(true);
      expect(store.has(q2)).toBe(false);
      expect(store.has(q3)).toBe(true);
    });

    it('throws TypeError for non-function input', () => {
      const store = createUnrdfStore();

      expect(() => store.transaction('not a function')).toThrow(TypeError);
      expect(() => store.transaction('not a function')).toThrow(
        'transaction: fn must be a function'
      );
    });

    it('throws TypeError for null input', () => {
      const store = createUnrdfStore();

      expect(() => store.transaction(null)).toThrow(TypeError);
    });

    it('re-throws transaction errors with context', () => {
      const store = createUnrdfStore();

      try {
        store.transaction(() => {
          throw new Error('Custom error');
        });
        expect.fail('Should have thrown');
      } catch (error) {
        expect(error.message).toContain('Transaction failed');
        expect(error.message).toContain('Custom error');
      }
    });
  });

  describe('update - SPARQL UPDATE (CRITICAL GAP)', () => {
    it('executes INSERT DATA update', () => {
      const store = createUnrdfStore();

      const sparqlUpdate = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        INSERT DATA {
          <http://example.org/alice> foaf:name "Alice" .
        }
      `;

      store.update(sparqlUpdate);

      expect(store.size()).toBeGreaterThan(0);
    });

    it('executes DELETE DATA update', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      expect(store.size()).toBe(1);

      const sparqlUpdate = `
        DELETE DATA {
          <http://example.org/alice> <http://foaf/name> "Alice" .
        }
      `;

      store.update(sparqlUpdate);

      expect(store.size()).toBe(0);
    });

    it('increments version after update', () => {
      const store = createUnrdfStore();
      const initialVersion = store.version;

      const sparqlUpdate = `
        INSERT DATA {
          <http://example.org/s> <http://example.org/p> "o" .
        }
      `;

      store.update(sparqlUpdate);

      expect(store.version).toBe(initialVersion + 1);
    });

    it('accepts options with baseIri', () => {
      const store = createUnrdfStore();
      const options = { baseIri: 'http://example.org/' };

      const sparqlUpdate = `
        INSERT DATA {
          <alice> <http://foaf/name> "Alice" .
        }
      `;

      store.update(sparqlUpdate, options);

      expect(store.size()).toBeGreaterThan(0);
    });

    it('throws TypeError for non-string SPARQL', () => {
      const store = createUnrdfStore();

      expect(() => store.update(123)).toThrow(TypeError);
      expect(() => store.update(123)).toThrow('update: sparql must be a string');
    });

    it('throws TypeError for null SPARQL', () => {
      const store = createUnrdfStore();

      expect(() => store.update(null)).toThrow(TypeError);
    });

    it('handles complex updates with multiple operations', () => {
      const store = createUnrdfStore();

      const sparqlUpdate = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        INSERT DATA {
          <http://example.org/alice> foaf:name "Alice" .
          <http://example.org/bob> foaf:name "Bob" .
        }
      `;

      store.update(sparqlUpdate);

      expect(store.size()).toBeGreaterThanOrEqual(2);
    });
  });

  describe('load - RDF Serialization Load (CRITICAL GAP)', () => {
    it('loads Turtle format data', () => {
      const store = createUnrdfStore();
      const turtleData = `
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <http://example.org/alice> foaf:name "Alice" .
      `;

      store.load(turtleData, { format: 'text/turtle' });

      expect(store.size()).toBeGreaterThan(0);
    });

    it('loads N-Triples format data', () => {
      const store = createUnrdfStore();
      const ntriplesData = '<http://example.org/s> <http://example.org/p> "o" .';

      store.load(ntriplesData, { format: 'application/n-triples' });

      expect(store.size()).toBeGreaterThan(0);
    });

    it('increments version after load', () => {
      const store = createUnrdfStore();
      const initialVersion = store.version;

      const turtleData = '<http://example.org/s> <http://example.org/p> "o" .';
      store.load(turtleData, { format: 'text/turtle' });

      expect(store.version).toBe(initialVersion + 1);
    });

    it('loads multiple quads from Turtle', () => {
      const store = createUnrdfStore();
      const turtleData = `
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <http://example.org/alice> foaf:name "Alice" ;
                                    foaf:age 30 .
        <http://example.org/bob> foaf:name "Bob" .
      `;

      store.load(turtleData, { format: 'text/turtle' });

      expect(store.size()).toBeGreaterThanOrEqual(3);
    });
  });

  describe('dump - RDF Serialization Export (CRITICAL GAP)', () => {
    it('dumps store to N-Quads format', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('o'))
      );

      const nquads = store.dump({ format: 'application/n-quads' });

      expect(typeof nquads).toBe('string');
      expect(nquads.length).toBeGreaterThan(0);
    });

    it('dumps store to TriG format', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('o'))
      );

      const trig = store.dump({ format: 'application/trig' });

      expect(typeof trig).toBe('string');
      expect(trig.length).toBeGreaterThan(0);
    });

    it('dumps empty store', () => {
      const store = createUnrdfStore();

      const nquads = store.dump({ format: 'application/n-quads' });

      expect(typeof nquads).toBe('string');
    });

    it('dumps multiple quads correctly', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
      store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));

      const nquads = store.dump({ format: 'application/n-quads' });

      expect(nquads.length).toBeGreaterThan(0);
    });

    it('roundtrip: dump and load produce same store', () => {
      const store1 = createUnrdfStore();
      store1.add(
        quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('test'))
      );

      const nquads = store1.dump({ format: 'application/n-quads' });

      const store2 = createUnrdfStore();
      store2.load(nquads, { format: 'application/n-quads' });

      expect(store2.size()).toBe(store1.size());
    });
  });

  describe('Version Tracking for Reactivity', () => {
    it('increments version on add', () => {
      const store = createUnrdfStore();
      const initialVersion = store.version;

      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      expect(store.version).toBe(initialVersion + 1);
    });

    it('increments version on delete', () => {
      const store = createUnrdfStore();
      const q = quad(namedNode('http://s'), namedNode('http://p'), literal('o'));
      store.add(q);
      const versionAfterAdd = store.version;

      store.delete(q);

      expect(store.version).toBe(versionAfterAdd + 1);
    });

    it('increments version on clear', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      const versionBeforeClear = store.version;

      store.clear();

      expect(store.version).toBe(versionBeforeClear + 1);
    });

    it('version remains stable when no mutations', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      const version = store.version;

      // Read operations should not change version
      store.query('SELECT * WHERE { ?s ?p ?o }');
      store.match();
      store.size();

      expect(store.version).toBe(version);
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('handles duplicate quads gracefully', () => {
      const store = createUnrdfStore();
      const q = quad(namedNode('http://s'), namedNode('http://p'), literal('o'));

      store.add(q);
      store.add(q); // Duplicate

      // Oxigraph handles duplicates internally
      expect(store.size()).toBeGreaterThanOrEqual(1);
    });

    it('handles malformed SPARQL in query', () => {
      const store = createUnrdfStore();
      const invalidSparql = 'MALFORMED QUERY {{{';

      expect(() => store.query(invalidSparql)).toThrow();
    });

    it('handles malformed SPARQL in update', () => {
      const store = createUnrdfStore();
      const invalidUpdate = 'MALFORMED UPDATE {{{';

      expect(() => store.update(invalidUpdate)).toThrow();
    });

    it('handles malformed RDF in load', () => {
      const store = createUnrdfStore();
      const malformedTurtle = '@prefix foaf: MALFORMED DATA';

      expect(() => store.load(malformedTurtle, { format: 'text/turtle' })).toThrow();
    });
  });

  describe('Integration - Full Workflow', () => {
    it('complete workflow: bulkAdd → query → transaction → dump', () => {
      const store = createUnrdfStore();

      // 1. Bulk add data
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://foaf/name'),
          literal('Alice')
        ),
        quad(namedNode('http://example.org/bob'), namedNode('http://foaf/name'), literal('Bob')),
      ];
      store.bulkAdd(quads);

      // 2. Query data
      const result1 = store.query('SELECT * WHERE { ?s ?p ?o }');
      expect(result1.length).toBe(2);

      // 3. Transaction to modify
      store.transaction(txStore => {
        txStore.add(
          quad(
            namedNode('http://example.org/charlie'),
            namedNode('http://foaf/name'),
            literal('Charlie')
          )
        );
      });

      // 4. Query again
      const result2 = store.query('SELECT * WHERE { ?s ?p ?o }');
      expect(result2.length).toBe(3);

      // 5. Dump to N-Quads
      const nquads = store.dump({ format: 'application/n-quads' });
      expect(nquads.length).toBeGreaterThan(0);

      // 6. Load into new store
      const store2 = createUnrdfStore();
      store2.load(nquads, { format: 'application/n-quads' });
      expect(store2.size()).toBe(store.size());
    });
  });
});
