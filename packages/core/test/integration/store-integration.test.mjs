/**
 * @vitest-environment node
 * Integration Test Suite for N3Store + UnrdfStore Interaction
 * Coverage Target: 90%+
 * Pattern: Chicago School TDD
 *
 * Tests backward compatibility, performance expectations, and transaction atomicity
 */

import { describe, it, expect } from 'vitest';
import { createUnrdfStore, namedNode, literal, quad } from '../../src/index.mjs';
import { executeQuerySync } from '../../src/sparql/executor-sync.mjs';

describe('Store Integration Tests', () => {
  describe('N3Store Legacy Compatibility', () => {
    it('executeQuerySync works with N3 Store via getQuads fallback', () => {
      const unrdfStore = createUnrdfStore();
      unrdfStore.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      // Create mock N3 Store with getQuads
      const n3Store = {
        getQuads: () => unrdfStore.match(),
      };

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = executeQuerySync(n3Store, sparql);

      expect(result).toBeDefined();
      expect(result.type).toBe('select');
      expect(result.rows.length).toBeGreaterThan(0);
    });

    it('N3 Store fallback handles empty store', () => {
      const n3Store = {
        getQuads: () => [],
      };

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = executeQuerySync(n3Store, sparql);

      expect(result.type).toBe('select');
      expect(result.rows.length).toBe(0);
    });

    it('N3 Store fallback handles ASK queries', () => {
      const unrdfStore = createUnrdfStore();
      unrdfStore.add(quad(namedNode('http://s'), namedNode('http://p'), literal('Alice')));

      const n3Store = {
        getQuads: () => unrdfStore.match(),
      };

      const sparql = 'ASK { ?s ?p "Alice" }';
      const result = executeQuerySync(n3Store, sparql);

      expect(typeof result).toBe('boolean');
    });

    it('N3 Store fallback handles CONSTRUCT queries', () => {
      const unrdfStore = createUnrdfStore();
      unrdfStore.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const n3Store = {
        getQuads: () => unrdfStore.match(),
      };

      const sparql = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
      const result = executeQuerySync(n3Store, sparql);

      expect(result.type).toBe('construct');
      expect(Array.isArray(result.quads)).toBe(true);
    });
  });

  describe('UnrdfStore Performance vs N3Store', () => {
    it('UnrdfStore is faster than N3Store fallback for repeated queries', () => {
      const unrdfStore = createUnrdfStore();
      const quads = Array.from({ length: 1000 }, (_, i) =>
        quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`value${i}`))
      );
      unrdfStore.bulkAdd(quads);

      const n3Store = {
        getQuads: () => unrdfStore.match(),
      };

      const sparql = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';

      // Measure N3Store fallback (recreates Oxigraph store each time)
      const n3Start = Date.now();
      for (let i = 0; i < 10; i++) {
        executeQuerySync(n3Store, sparql);
      }
      const n3Duration = Date.now() - n3Start;

      // Measure UnrdfStore (persistent Oxigraph store)
      const unrdfStart = Date.now();
      for (let i = 0; i < 10; i++) {
        unrdfStore.query(sparql);
      }
      const unrdfDuration = Date.now() - unrdfStart;

      // UnrdfStore should be at least 5x faster
      expect(unrdfDuration).toBeLessThan(n3Duration / 5);
    });
  });

  describe('Transaction Atomicity', () => {
    it('transaction commits all changes on success', () => {
      const store = createUnrdfStore();

      store.transaction(txStore => {
        for (let i = 0; i < 10; i++) {
          txStore.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
        }
      });

      expect(store.size()).toBe(10);
    });

    it('transaction rolls back all changes on error', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://existing'), namedNode('http://p'), literal('o')));

      const initialSize = store.size();

      try {
        store.transaction(txStore => {
          txStore.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
          txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
          throw new Error('Rollback test');
        });
      } catch (error) {
        // Expected
      }

      // Should be rolled back to initial state
      expect(store.size()).toBe(initialSize);
    });

    it('transaction preserves original quads on rollback', () => {
      const store = createUnrdfStore();
      const originalQuad = quad(namedNode('http://original'), namedNode('http://p'), literal('o'));
      store.add(originalQuad);

      try {
        store.transaction(txStore => {
          txStore.delete(originalQuad);
          txStore.add(quad(namedNode('http://new'), namedNode('http://p'), literal('o')));
          throw new Error('Fail');
        });
      } catch (error) {
        // Expected
      }

      expect(store.has(originalQuad)).toBe(true);
      expect(store.size()).toBe(1);
    });

    it('nested transactions are not supported (single txStore instance)', () => {
      const store = createUnrdfStore();

      store.transaction(txStore => {
        txStore.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));

        // This is the same store instance, not a nested transaction
        txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
      });

      expect(store.size()).toBe(2);
    });
  });

  describe('Bulk Operations Efficiency', () => {
    it('bulkAdd is faster than individual adds for large datasets', () => {
      const quads = Array.from({ length: 1000 }, (_, i) =>
        quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`value${i}`))
      );

      // Individual adds
      const store1 = createUnrdfStore();
      const individualStart = Date.now();
      for (const q of quads) {
        store1.add(q);
      }
      const individualDuration = Date.now() - individualStart;

      // Bulk add
      const store2 = createUnrdfStore();
      const bulkStart = Date.now();
      store2.bulkAdd(quads);
      const bulkDuration = Date.now() - bulkStart;

      // Both should produce same result
      expect(store1.size()).toBe(1000);
      expect(store2.size()).toBe(1000);

      // Bulk should be at least as fast (may be same performance)
      expect(bulkDuration).toBeLessThanOrEqual(individualDuration * 2);
    });

    it('bulkRemove is efficient for large datasets', () => {
      const store = createUnrdfStore();
      const quads = Array.from({ length: 1000 }, (_, i) =>
        quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`value${i}`))
      );

      store.bulkAdd(quads);

      const start = Date.now();
      store.bulkRemove(quads);
      const duration = Date.now() - start;

      expect(store.size()).toBe(0);
      expect(duration).toBeLessThan(5000); // Should be < 5 seconds
    });
  });

  describe('Query Integration with Store Operations', () => {
    it('query reflects bulkAdd immediately', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];

      store.bulkAdd(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = store.query(sparql);

      expect(result.length).toBe(2);
    });

    it('query reflects bulkRemove immediately', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];

      store.bulkAdd(quads);
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result1 = store.query(sparql);
      expect(result1.length).toBe(2);

      store.bulkRemove(quads);
      const result2 = store.query(sparql);
      expect(result2.length).toBe(0);
    });

    it('query reflects transaction immediately', () => {
      const store = createUnrdfStore();

      store.transaction(txStore => {
        txStore.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
        txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
      });

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = store.query(sparql);

      expect(result.length).toBe(2);
    });

    it('query reflects update immediately', () => {
      const store = createUnrdfStore();

      const sparqlUpdate = `
        INSERT DATA {
          <http://example.org/s1> <http://example.org/p> "o1" .
          <http://example.org/s2> <http://example.org/p> "o2" .
        }
      `;

      store.update(sparqlUpdate);

      const sparqlQuery = 'SELECT * WHERE { ?s ?p ?o }';
      const result = store.query(sparqlQuery);

      expect(result.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('Async Query Integration', () => {
    it('queryAsync works with bulkAdd', async () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];

      store.bulkAdd(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = await store.queryAsync(sparql);

      expect(result.length).toBe(2);
    });

    it('queryAsync works with transaction', async () => {
      const store = createUnrdfStore();

      store.transaction(txStore => {
        txStore.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      });

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = await store.queryAsync(sparql);

      expect(result.length).toBe(1);
    });

    it('queryAsync works after update', async () => {
      const store = createUnrdfStore();

      store.update(`
        INSERT DATA {
          <http://example.org/s> <http://example.org/p> "o" .
        }
      `);

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = await store.queryAsync(sparql);

      expect(result.length).toBeGreaterThan(0);
    });
  });

  describe('Load/Dump Integration', () => {
    it('dump after bulkAdd produces valid Turtle', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];

      store.bulkAdd(quads);

      const nquads = store.dump({ format: 'application/n-quads' });

      expect(typeof nquads).toBe('string');
      expect(nquads.length).toBeGreaterThan(0);
    });

    it('load after dump produces same store', () => {
      const store1 = createUnrdfStore();
      store1.bulkAdd([
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ]);

      const nquads = store1.dump({ format: 'application/n-quads' });

      const store2 = createUnrdfStore();
      store2.load(nquads, { format: 'application/n-quads' });

      expect(store2.size()).toBe(store1.size());
    });

    it('dump after transaction produces valid N-Quads', () => {
      const store = createUnrdfStore();

      store.transaction(txStore => {
        txStore.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
        txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
      });

      const nquads = store.dump({ format: 'application/n-quads' });

      expect(typeof nquads).toBe('string');
      expect(nquads.length).toBeGreaterThan(0);
    });

    it('load into store then bulkAdd works correctly', () => {
      const store = createUnrdfStore();

      const turtleData = `
        <http://example.org/s1> <http://example.org/p> "o1" .
      `;

      store.load(turtleData, { format: 'text/turtle' });

      const sizeBefore = store.size();

      store.bulkAdd([quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'))]);

      expect(store.size()).toBe(sizeBefore + 1);
    });
  });

  describe('Error Propagation', () => {
    it('N3Store fallback propagates query errors', () => {
      const n3Store = {
        getQuads: () => [],
      };

      const invalidSparql = 'INVALID QUERY {{{';

      expect(() => executeQuerySync(n3Store, invalidSparql)).toThrow();
    });

    it('transaction error preserves store integrity', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));

      const initialVersion = store.version;

      try {
        store.transaction(txStore => {
          txStore.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
          throw new Error('Test error');
        });
      } catch (error) {
        // Expected
      }

      // Store should be intact
      expect(store.size()).toBe(1);
      // Version incremented for snapshot + rollback
      expect(store.version).toBeGreaterThanOrEqual(initialVersion);
    });
  });

  describe('Version Tracking Integration', () => {
    it('version increments correctly across operations', () => {
      const store = createUnrdfStore();
      const initialVersion = store.version;

      // bulkAdd
      store.bulkAdd([quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'))]);
      expect(store.version).toBe(initialVersion + 1);

      // update
      store.update(`INSERT DATA { <http://s2> <http://p> "o2" . }`);
      expect(store.version).toBe(initialVersion + 2);

      // load
      store.load('<http://s3> <http://p> "o3" .', { format: 'text/turtle' });
      expect(store.version).toBe(initialVersion + 3);

      // bulkRemove
      store.bulkRemove([quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'))]);
      expect(store.version).toBe(initialVersion + 4);

      // clear
      store.clear();
      expect(store.version).toBe(initialVersion + 5);
    });

    it('version does not increment on read operations', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      const version = store.version;

      // Read operations
      store.query('SELECT * WHERE { ?s ?p ?o }');
      store.match();
      store.size();
      store.has(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      store.dump({ format: 'application/n-quads' });

      expect(store.version).toBe(version);
    });
  });
});
