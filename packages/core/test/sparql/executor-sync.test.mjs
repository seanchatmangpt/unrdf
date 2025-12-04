/**
 * @vitest-environment node
 * Comprehensive Test Suite for Synchronous SPARQL Executors
 * Coverage Target: 90%+
 * Pattern: Chicago School TDD
 */

import { describe, it, expect } from 'vitest';
import {
  executeQuerySync,
  executeSelectSync,
  executeAskSync,
  executeConstructSync,
  prepareQuerySync,
} from '../../src/sparql/executor-sync.mjs';
import { createUnrdfStore, namedNode, literal, quad } from '../../src/index.mjs';

describe('Synchronous SPARQL Executors - executor-sync.mjs', () => {
  describe('prepareQuerySync - Query Parsing and Validation', () => {
    it('parses SELECT query and returns query type', () => {
      const sparql = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
      const result = prepareQuerySync(sparql);

      expect(result).toBeDefined();
      expect(result.queryType).toBe('SELECT');
      expect(result.sparql).toBe(sparql);
      expect(result.variables).toBeDefined();
      expect(result.prefixes).toBeDefined();
    });

    it('parses CONSTRUCT query with PREFIX declarations', () => {
      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        CONSTRUCT { ?s foaf:name ?name }
        WHERE { ?s foaf:name ?name }
      `;
      const result = prepareQuerySync(sparql);

      expect(result.queryType).toBe('CONSTRUCT');
      expect(result.sparql).toBe(sparql);
    });

    it('parses ASK query', () => {
      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = prepareQuerySync(sparql);

      expect(result.queryType).toBe('ASK');
    });

    it('parses DESCRIBE query', () => {
      const sparql = 'DESCRIBE <http://example.org/alice>';
      const result = prepareQuerySync(sparql);

      expect(result.queryType).toBe('DESCRIBE');
    });

    it('defaults to first keyword for unknown query types', () => {
      const sparql = 'INVALID QUERY';
      const result = prepareQuerySync(sparql);

      // Should use first keyword when no valid keyword found
      expect(result.queryType).toBe('INVALID');
    });

    it('throws TypeError for non-string SPARQL', () => {
      expect(() => prepareQuerySync(123)).toThrow(TypeError);
      expect(() => prepareQuerySync(123)).toThrow('sparql must be a string');
    });

    it('throws TypeError for null SPARQL', () => {
      expect(() => prepareQuerySync(null)).toThrow(TypeError);
    });

    it('throws TypeError for undefined SPARQL', () => {
      expect(() => prepareQuerySync(undefined)).toThrow(TypeError);
    });

    it('handles empty string SPARQL', () => {
      const result = prepareQuerySync('');
      expect(result).toBeDefined();
      expect(result.sparql).toBe('');
      expect(result.queryType).toBe('unknown');
    });

    it('handles SPARQL with multiple PREFIX declarations', () => {
      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX dc: <http://purl.org/dc/elements/1.1/>
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE { ?s foaf:name ?name }
      `;
      const result = prepareQuerySync(sparql);

      expect(result.queryType).toBe('SELECT');
      expect(result.sparql).toContain('PREFIX foaf:');
      expect(result.sparql).toContain('PREFIX dc:');
    });
  });

  describe('executeQuerySync - Core Synchronous Execution', () => {
    it('executes SELECT query synchronously (no Promise returned)', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('Alice'))
      );

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = executeQuerySync(store, sparql);

      // CRITICAL: Validate result is NOT a Promise
      expect(result).not.toBeInstanceOf(Promise);
      expect(result).toBeDefined();
      expect(result.type).toBe('select');
      expect(Array.isArray(result.rows)).toBe(true);
    });

    it('executes ASK query synchronously returning boolean', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = executeQuerySync(store, sparql);

      // CRITICAL: Result must be a boolean, not a Promise
      expect(result).not.toBeInstanceOf(Promise);
      expect(typeof result).toBe('boolean');
    });

    it('executes CONSTRUCT query synchronously returning quads', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;
      const result = executeQuerySync(store, sparql);

      // CRITICAL: Result must have type 'construct' and quads array, not a Promise
      expect(result).not.toBeInstanceOf(Promise);
      expect(result.type).toBe('construct');
      expect(Array.isArray(result.quads)).toBe(true);
    });

    it('throws TypeError for invalid store (null)', () => {
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      expect(() => executeQuerySync(null, sparql)).toThrow(TypeError);
      expect(() => executeQuerySync(null, sparql)).toThrow('store is required');
    });

    it('throws error for invalid store (no query or getQuads method)', () => {
      const invalidStore = { someOtherMethod: () => [] };
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      // With defensive code removed, this will fail naturally when trying to call
      // store.query() or store.getQuads() - let it fail with natural JS error
      expect(() => executeQuerySync(invalidStore, sparql)).toThrow();
    });

    it('supports N3 Store with getQuads() for backward compatibility', () => {
      // Simulate N3 Store interface
      const n3StoreStub = {
        getQuads: () => [
          quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('value'))
        ]
      };

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = executeQuerySync(n3StoreStub, sparql);

      expect(result).toBeDefined();
      expect(result.type).toBe('select');
      expect(Array.isArray(result.rows)).toBe(true);
    });

    it('throws TypeError for non-string SPARQL', () => {
      const store = createUnrdfStore();
      expect(() => executeQuerySync(store, 123)).toThrow(TypeError);
      expect(() => executeQuerySync(store, 123)).toThrow('sparql must be a string');
    });

    it('throws TypeError for null SPARQL', () => {
      const store = createUnrdfStore();
      expect(() => executeQuerySync(store, null)).toThrow(TypeError);
    });

    it('throws TypeError for undefined SPARQL', () => {
      const store = createUnrdfStore();
      expect(() => executeQuerySync(store, undefined)).toThrow(TypeError);
    });

    it('passes options to underlying store.query()', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://example.org/s1'), namedNode('http://p'), literal('o1')));
      store.add(quad(namedNode('http://example.org/s2'), namedNode('http://p'), literal('o2')));

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const options = { limit: 1, offset: 0 };

      // Should pass options through without error
      const result = executeQuerySync(store, sparql, options);
      expect(result).toBeDefined();
    });

    it('handles empty store gracefully', () => {
      const store = createUnrdfStore();
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      const result = executeQuerySync(store, sparql);

      expect(result).toBeDefined();
      expect(result.type).toBe('select');
      expect(Array.isArray(result.rows)).toBe(true);
      expect(result.rows.length).toBe(0);
    });

    it('handles query with no matches', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT * WHERE { ?s <http://nonexistent> ?o }';
      const result = executeQuerySync(store, sparql);

      expect(result.type).toBe('select');
      expect(Array.isArray(result.rows)).toBe(true);
      expect(result.rows.length).toBe(0);
    });

    it('wraps execution errors with descriptive message', () => {
      const store = createUnrdfStore();
      const invalidSparql = 'INVALID SPARQL SYNTAX {{{{';

      expect(() => executeQuerySync(store, invalidSparql)).toThrow(Error);
      expect(() => executeQuerySync(store, invalidSparql)).toThrow(/SPARQL query execution failed/);
    });
  });

  describe('executeSelectSync - SELECT Query Specialization', () => {
    it('returns bindings array for SELECT query', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';
      const rows = executeSelectSync(store, sparql);

      expect(Array.isArray(rows)).toBe(true);
      expect(rows.length).toBeGreaterThan(0);
      expect(rows[0]).toHaveProperty('name');
    });

    it('returns bindings with multiple variables', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/age'), literal('30'))
      );

      const sparql = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
      const rows = executeSelectSync(store, sparql);

      expect(rows.length).toBeGreaterThanOrEqual(2);
      expect(rows[0]).toHaveProperty('s');
      expect(rows[0]).toHaveProperty('p');
      expect(rows[0]).toHaveProperty('o');
    });

    it('returns empty array for SELECT with no matches', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://nonexistent> ?name }';
      const rows = executeSelectSync(store, sparql);

      expect(Array.isArray(rows)).toBe(true);
      expect(rows.length).toBe(0);
    });

    it('throws error for non-SELECT query (ASK)', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';

      expect(() => executeSelectSync(store, sparql)).toThrow(Error);
      expect(() => executeSelectSync(store, sparql)).toThrow('not a SELECT query');
    });

    it('throws error for non-SELECT query (CONSTRUCT)', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;

      expect(() => executeSelectSync(store, sparql)).toThrow(Error);
      expect(() => executeSelectSync(store, sparql)).toThrow('not a SELECT query');
    });

    it('passes options through to executeQuerySync', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://example.org/s1'), namedNode('http://p'), literal('o1')));
      store.add(quad(namedNode('http://example.org/s2'), namedNode('http://p'), literal('o2')));

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const options = { limit: 1 };

      // Should not throw with options
      const rows = executeSelectSync(store, sparql, options);
      expect(rows).toBeDefined();
    });

    it('handles SELECT with FILTER clause', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/age'), literal('25'))
      );
      store.add(
        quad(namedNode('http://example.org/bob'), namedNode('http://foaf/age'), literal('35'))
      );

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://foaf/age> ?age .
          FILTER(?age > 30)
        }
      `;

      const rows = executeSelectSync(store, sparql);
      expect(Array.isArray(rows)).toBe(true);
    });

    it('handles SELECT with PREFIX declarations', () => {
      const store = createUnrdfStore();
      store.add(
        quad(
          namedNode('http://xmlns.com/foaf/0.1/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );

      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE { ?s foaf:name ?name }
      `;

      const rows = executeSelectSync(store, sparql);
      expect(Array.isArray(rows)).toBe(true);
    });
  });

  describe('executeAskSync - ASK Query Specialization', () => {
    it('returns boolean true when pattern matches', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Alice" }';
      const result = executeAskSync(store, sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('returns boolean false when pattern does not match', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'ASK { ?s <http://foaf/name> "Bob" }';
      const result = executeAskSync(store, sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(false);
    });

    it('returns boolean false for empty store', () => {
      const store = createUnrdfStore();
      const sparql = 'ASK { ?s ?p ?o }';

      const result = executeAskSync(store, sparql);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(false);
    });

    it('throws error for non-ASK query (SELECT)', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = 'SELECT ?name WHERE { ?s <http://foaf/name> ?name }';

      expect(() => executeAskSync(store, sparql)).toThrow(Error);
      expect(() => executeAskSync(store, sparql)).toThrow('not an ASK query');
    });

    it('throws error for non-ASK query (CONSTRUCT)', () => {
      const store = createUnrdfStore();
      const sparql = `
        CONSTRUCT { ?s <http://label> ?o }
        WHERE { ?s ?p ?o }
      `;

      expect(() => executeAskSync(store, sparql)).toThrow(Error);
      expect(() => executeAskSync(store, sparql)).toThrow('not an ASK query');
    });

    it('handles ASK with complex patterns', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/age'), literal('30'))
      );

      const sparql = `
        ASK {
          ?s <http://foaf/name> "Alice" .
          ?s <http://foaf/age> ?age
        }
      `;

      const result = executeAskSync(store, sparql);
      expect(typeof result).toBe('boolean');
    });

    it('handles ASK with PREFIX declarations', () => {
      const store = createUnrdfStore();
      store.add(
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );

      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        ASK { ?s foaf:name "Alice" }
      `;

      const result = executeAskSync(store, sparql);
      expect(typeof result).toBe('boolean');
    });

    it('handles ASK with FILTER clause', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/age'), literal('25'))
      );

      const sparql = `
        ASK {
          ?s <http://foaf/age> ?age .
          FILTER(?age > 20)
        }
      `;

      const result = executeAskSync(store, sparql);
      expect(typeof result).toBe('boolean');
    });
  });

  describe('executeConstructSync - CONSTRUCT Query Specialization', () => {
    it('returns array of quads for CONSTRUCT query', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/label> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;

      const quads = executeConstructSync(store, sparql);

      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThan(0);
    });

    it('returns empty array when CONSTRUCT has no matches', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://label> ?name }
        WHERE { ?s <http://nonexistent> ?name }
      `;

      const quads = executeConstructSync(store, sparql);

      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBe(0);
    });

    it('constructs multiple quads from pattern', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );
      store.add(
        quad(namedNode('http://example.org/bob'), namedNode('http://foaf/name'), literal('Bob'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/person> ?name }
        WHERE { ?s <http://foaf/name> ?name }
      `;

      const quads = executeConstructSync(store, sparql);

      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThanOrEqual(2);
    });

    it('throws error for non-CONSTRUCT query (SELECT)', () => {
      const store = createUnrdfStore();
      const sparql = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';

      expect(() => executeConstructSync(store, sparql)).toThrow(Error);
      expect(() => executeConstructSync(store, sparql)).toThrow('not a CONSTRUCT query');
    });

    it('throws error for non-CONSTRUCT query (ASK)', () => {
      const store = createUnrdfStore();
      const sparql = 'ASK { ?s ?p ?o }';

      expect(() => executeConstructSync(store, sparql)).toThrow(Error);
      expect(() => executeConstructSync(store, sparql)).toThrow('not a CONSTRUCT query');
    });

    it('handles CONSTRUCT with PREFIX declarations', () => {
      const store = createUnrdfStore();
      store.add(
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      );

      const sparql = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX ex: <http://example.org/>
        CONSTRUCT { ?s ex:label ?name }
        WHERE { ?s foaf:name ?name }
      `;

      const quads = executeConstructSync(store, sparql);
      expect(Array.isArray(quads)).toBe(true);
    });

    it('handles CONSTRUCT with complex WHERE clause', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );
      store.add(
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/age'), literal('30'))
      );

      const sparql = `
        CONSTRUCT { ?s <http://example.org/profile> ?name }
        WHERE {
          ?s <http://foaf/name> ?name .
          ?s <http://foaf/age> ?age .
          FILTER(?age > 25)
        }
      `;

      const quads = executeConstructSync(store, sparql);
      expect(Array.isArray(quads)).toBe(true);
    });
  });

  describe('Synchronous Execution - No Promises', () => {
    it('executeQuerySync never returns a Promise', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const selectResult = executeQuerySync(store, 'SELECT * WHERE { ?s ?p ?o }');
      const askResult = executeQuerySync(store, 'ASK { ?s ?p ?o }');

      expect(selectResult).not.toBeInstanceOf(Promise);
      expect(askResult).not.toBeInstanceOf(Promise);
    });

    it('executeSelectSync never returns a Promise', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const result = executeSelectSync(store, 'SELECT * WHERE { ?s ?p ?o }');

      expect(result).not.toBeInstanceOf(Promise);
      expect(Array.isArray(result)).toBe(true);
    });

    it('executeAskSync never returns a Promise', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const result = executeAskSync(store, 'ASK { ?s ?p ?o }');

      expect(result).not.toBeInstanceOf(Promise);
      expect(typeof result).toBe('boolean');
    });

    it('executeConstructSync never returns a Promise', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const result = executeConstructSync(store, 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');

      expect(result).not.toBeInstanceOf(Promise);
      expect(Array.isArray(result)).toBe(true);
    });

    it('prepareQuerySync never returns a Promise', () => {
      const result = prepareQuerySync('SELECT * WHERE { ?s ?p ?o }');

      expect(result).not.toBeInstanceOf(Promise);
      expect(result.queryType).toBeDefined();
      expect(result.variables).toBeDefined();
      expect(result.prefixes).toBeDefined();
      expect(result.sparql).toBeDefined();
    });
  });

  describe('Error Handling - Clear and Actionable Messages', () => {
    it('provides clear error for missing store', () => {
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      try {
        executeQuerySync(null, sparql);
        expect.fail('Should have thrown TypeError');
      } catch (error) {
        expect(error).toBeInstanceOf(TypeError);
        expect(error.message).toContain('store is required');
      }
    });

    it('provides clear error for invalid SPARQL type', () => {
      const store = createUnrdfStore();

      try {
        executeQuerySync(store, 123);
        expect.fail('Should have thrown TypeError');
      } catch (error) {
        expect(error).toBeInstanceOf(TypeError);
        expect(error.message).toContain('sparql must be a string');
      }
    });

    it('provides clear error for invalid query syntax', () => {
      const store = createUnrdfStore();
      const invalidSparql = 'INVALID SPARQL {{{';

      try {
        executeQuerySync(store, invalidSparql);
        expect.fail('Should have thrown Error');
      } catch (error) {
        expect(error).toBeInstanceOf(Error);
        expect(error.message).toContain('query execution failed');
      }
    });

    it('provides clear error for wrong query type in executeSelectSync', () => {
      const store = createUnrdfStore();
      const sparql = 'ASK { ?s ?p ?o }';

      try {
        executeSelectSync(store, sparql);
        expect.fail('Should have thrown Error');
      } catch (error) {
        expect(error).toBeInstanceOf(Error);
        expect(error.message).toContain('not a SELECT query');
      }
    });

    it('provides clear error for wrong query type in executeAskSync', () => {
      const store = createUnrdfStore();
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      try {
        executeAskSync(store, sparql);
        expect.fail('Should have thrown Error');
      } catch (error) {
        expect(error).toBeInstanceOf(Error);
        expect(error.message).toContain('not an ASK query');
      }
    });

    it('provides clear error for wrong query type in executeConstructSync', () => {
      const store = createUnrdfStore();
      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      try {
        executeConstructSync(store, sparql);
        expect.fail('Should have thrown Error');
      } catch (error) {
        expect(error).toBeInstanceOf(Error);
        expect(error.message).toContain('not a CONSTRUCT query');
      }
    });
  });

  describe('Edge Cases and Boundary Conditions', () => {
    it('handles large result sets efficiently', () => {
      const store = createUnrdfStore();

      // Add 100 quads
      for (let i = 0; i < 100; i++) {
        store.add(
          quad(namedNode(`http://example.org/s${i}`), namedNode('http://p'), literal(`value${i}`))
        );
      }

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const start = Date.now();
      const result = executeQuerySync(store, sparql);
      const duration = Date.now() - start;

      expect(result.type).toBe('select');
      expect(result.rows.length).toBe(100);
      expect(duration).toBeLessThan(1000); // Should be fast (< 1 second)
    });

    it('handles empty SPARQL string', () => {
      const store = createUnrdfStore();

      // Empty query should fail during preparation
      expect(() => executeQuerySync(store, '')).toThrow();
    });

    it('handles SPARQL with only whitespace', () => {
      const store = createUnrdfStore();

      expect(() => executeQuerySync(store, '   \n\t  ')).toThrow();
    });

    it('handles Unicode in SPARQL queries', () => {
      const store = createUnrdfStore();
      store.add(
        quad(namedNode('http://example.org/s'), namedNode('http://p'), literal('こんにちは'))
      );

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = executeQuerySync(store, sparql);

      expect(result.length).toBeGreaterThan(0);
    });

    it('handles very long SPARQL queries', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://example.org/s'), namedNode('http://p'), literal('o')));

      // Generate long query with many PREFIX declarations
      const prefixes = Array.from(
        { length: 50 },
        (_, i) => `PREFIX p${i}: <http://example.org/prefix${i}/>`
      ).join('\n');
      const sparql = `${prefixes}\nSELECT * WHERE { ?s ?p ?o }`;

      const result = executeQuerySync(store, sparql);
      expect(result).toBeDefined();
    });

    it('handles concurrent query executions', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';

      // Execute multiple queries "concurrently" (synchronously in loop)
      const results = [];
      for (let i = 0; i < 10; i++) {
        results.push(executeQuerySync(store, sparql));
      }

      expect(results.length).toBe(10);
      results.forEach(result => {
        expect(result.type).toBe('select');
        expect(result.rows.length).toBeGreaterThan(0);
      });
    });
  });

  describe('Integration with UnrdfStore', () => {
    it('works with store populated via bulkAdd', () => {
      const store = createUnrdfStore();
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
        quad(namedNode('http://s3'), namedNode('http://p'), literal('o3')),
      ];
      store.bulkAdd(quads);

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result = executeQuerySync(store, sparql);

      expect(result.type).toBe('select');
      expect(result.rows.length).toBe(3);
    });

    it('works with store modified during execution', () => {
      const store = createUnrdfStore();
      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));

      const sparql1 = 'SELECT * WHERE { ?s ?p ?o }';
      const result1 = executeQuerySync(store, sparql1);
      expect(result1.type).toBe('select');
      expect(result1.rows.length).toBe(1);

      // Add more quads
      store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));

      const sparql2 = 'SELECT * WHERE { ?s ?p ?o }';
      const result2 = executeQuerySync(store, sparql2);
      expect(result2.type).toBe('select');
      expect(result2.rows.length).toBe(2);
    });

    it('reflects store deletions in query results', () => {
      const store = createUnrdfStore();
      const q = quad(namedNode('http://s'), namedNode('http://p'), literal('o'));
      store.add(q);

      const sparql = 'SELECT * WHERE { ?s ?p ?o }';
      const result1 = executeQuerySync(store, sparql);
      expect(result1.type).toBe('select');
      expect(result1.rows.length).toBe(1);

      store.delete(q);
      const result2 = executeQuerySync(store, sparql);
      expect(result2.type).toBe('select');
      expect(result2.rows.length).toBe(0);
    });
  });
});
