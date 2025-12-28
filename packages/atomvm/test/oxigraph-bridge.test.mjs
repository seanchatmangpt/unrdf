/**
 * @fileoverview OxigraphBridge Tests
 *
 * Comprehensive test suite for the Oxigraph Bridge module.
 * Tests BEAM to Oxigraph RDF store integration.
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { OxigraphBridge, BRIDGE_OPERATIONS } from '../src/oxigraph-bridge.mjs';

/**
 * Create a mock RDF term (named node)
 * @param {string} value - IRI value
 * @returns {object} Mock named node
 */
function mockNamedNode(value) {
  return {
    termType: 'NamedNode',
    value,
    equals: (other) => other?.value === value,
  };
}

/**
 * Create a mock RDF literal
 * @param {string} value - Literal value
 * @returns {object} Mock literal
 */
function mockLiteral(value) {
  return {
    termType: 'Literal',
    value,
    equals: (other) => other?.value === value,
  };
}

/**
 * Create a mock Oxigraph store for testing
 * @returns {object} Mock store with required methods
 */
function createMockStore() {
  const quads = [];

  return {
    quads,
    add(quad) {
      if (!quad) throw new Error('Quad is required');
      quads.push(quad);
    },
    addQuad(subject, predicate, object, graph) {
      const quad = { subject, predicate, object, graph };
      quads.push(quad);
    },
    delete(quad) {
      const index = quads.findIndex(
        (q) =>
          q.subject?.value === quad.subject?.value &&
          q.predicate?.value === quad.predicate?.value &&
          q.object?.value === quad.object?.value
      );
      if (index >= 0) {
        quads.splice(index, 1);
      }
    },
    removeQuad(subject, predicate, object, graph) {
      const index = quads.findIndex(
        (q) =>
          q.subject?.value === subject?.value &&
          q.predicate?.value === predicate?.value &&
          q.object?.value === object?.value
      );
      if (index >= 0) {
        quads.splice(index, 1);
      }
    },
    match(subject, predicate, object, graph) {
      return quads.filter((q) => {
        if (subject && q.subject?.value !== subject?.value) return false;
        if (predicate && q.predicate?.value !== predicate?.value) return false;
        if (object && q.object?.value !== object?.value) return false;
        return true;
      });
    },
    getQuads(subject, predicate, object, graph) {
      return this.match(subject, predicate, object, graph);
    },
    query(queryString) {
      if (!queryString || typeof queryString !== 'string') {
        throw new Error('Query must be a non-empty string');
      }

      const upperQuery = queryString.toUpperCase().trim();
      if (upperQuery.startsWith('SELECT')) {
        return quads.map((q) => ({
          s: q.subject,
          p: q.predicate,
          o: q.object,
        }));
      }
      if (upperQuery.startsWith('ASK')) {
        return quads.length > 0;
      }
      if (upperQuery.startsWith('CONSTRUCT')) {
        return quads;
      }

      throw new Error(`Unsupported query type: ${queryString.substring(0, 20)}`);
    },
    has(quad) {
      return quads.some(
        (q) =>
          q.subject?.value === quad.subject?.value &&
          q.predicate?.value === quad.predicate?.value &&
          q.object?.value === quad.object?.value
      );
    },
    get size() {
      return quads.length;
    },
    clear() {
      quads.length = 0;
    },
  };
}

describe('OxigraphBridge', () => {
  let store;
  let bridge;

  beforeEach(() => {
    store = createMockStore();
    bridge = new OxigraphBridge(store);
  });

  describe('Constructor', () => {
    it('should create bridge instance with valid store', () => {
      expect(bridge).toBeDefined();
      expect(bridge.state).toBe('Ready');
      expect(bridge.isReady()).toBe(true);
    });

    it('should throw error when store is null', () => {
      expect(() => new OxigraphBridge(null)).toThrow('store is required');
    });

    it('should throw error when store is undefined', () => {
      expect(() => new OxigraphBridge(undefined)).toThrow('store is required');
    });

    it('should throw error when store is not an object', () => {
      expect(() => new OxigraphBridge('not-a-store')).toThrow('store must be an object');
    });

    it('should throw error when store lacks add method', () => {
      expect(() => new OxigraphBridge({ match: vi.fn() })).toThrow('store must have an add() method');
    });

    it('should throw error when store lacks match/getQuads method', () => {
      expect(() => new OxigraphBridge({ add: vi.fn() })).toThrow(
        'store must have a match() or getQuads() method'
      );
    });

    it('should accept store with only getQuads method', () => {
      const storeWithGetQuads = {
        add: vi.fn(),
        getQuads: vi.fn(),
        delete: vi.fn(),
      };
      const b = new OxigraphBridge(storeWithGetQuads);
      expect(b.isReady()).toBe(true);
    });

    it('should accept store with only removeQuad method', () => {
      const storeWithRemoveQuad = {
        add: vi.fn(),
        match: vi.fn(),
        removeQuad: vi.fn(),
      };
      const b = new OxigraphBridge(storeWithRemoveQuad);
      expect(b.isReady()).toBe(true);
    });
  });

  describe('addTriples', () => {
    it('should add single triple with subject/predicate/object format', async () => {
      const triple = {
        subject: mockNamedNode('http://example.org/subject'),
        predicate: mockNamedNode('http://example.org/predicate'),
        object: mockLiteral('value'),
      };

      const result = await bridge.addTriples([triple]);

      expect(result.success).toBe(true);
      expect(result.count).toBe(1);
      expect(result.errors).toHaveLength(0);
      expect(store.size).toBe(1);
    });

    it('should add single triple with s/p/o format', async () => {
      const triple = {
        s: mockNamedNode('http://example.org/subject'),
        p: mockNamedNode('http://example.org/predicate'),
        o: mockLiteral('value'),
      };

      const result = await bridge.addTriples([triple]);

      expect(result.success).toBe(true);
      expect(result.count).toBe(1);
    });

    it('should add multiple triples in batch', async () => {
      const triples = [
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
        {
          subject: mockNamedNode('http://example.org/s2'),
          predicate: mockNamedNode('http://example.org/p2'),
          object: mockLiteral('v2'),
        },
        {
          subject: mockNamedNode('http://example.org/s3'),
          predicate: mockNamedNode('http://example.org/p3'),
          object: mockLiteral('v3'),
        },
      ];

      const result = await bridge.addTriples(triples);

      expect(result.success).toBe(true);
      expect(result.count).toBe(3);
      expect(store.size).toBe(3);
    });

    it('should update stats after adding triples', async () => {
      const triple = {
        subject: mockNamedNode('http://example.org/s'),
        predicate: mockNamedNode('http://example.org/p'),
        object: mockLiteral('v'),
      };

      await bridge.addTriples([triple]);

      const stats = bridge.stats;
      expect(stats.addCount).toBe(1);
      expect(stats.queryCount).toBe(0);
      expect(stats.removeCount).toBe(0);
      expect(stats.errorCount).toBe(0);
    });

    it('should throw error when triples is not an array', async () => {
      await expect(bridge.addTriples('not-array')).rejects.toThrow('triples must be an array');
    });

    it('should throw error when triple lacks subject', async () => {
      const triple = {
        predicate: mockNamedNode('http://example.org/p'),
        object: mockLiteral('v'),
      };

      await expect(bridge.addTriples([triple])).rejects.toThrow('must have a subject');
    });

    it('should throw error when triple lacks predicate', async () => {
      const triple = {
        subject: mockNamedNode('http://example.org/s'),
        object: mockLiteral('v'),
      };

      await expect(bridge.addTriples([triple])).rejects.toThrow('must have a predicate');
    });

    it('should throw error when triple lacks object', async () => {
      const triple = {
        subject: mockNamedNode('http://example.org/s'),
        predicate: mockNamedNode('http://example.org/p'),
      };

      await expect(bridge.addTriples([triple])).rejects.toThrow('must have an object');
    });

    it('should return to Ready state after operation', async () => {
      const triple = {
        subject: mockNamedNode('http://example.org/s'),
        predicate: mockNamedNode('http://example.org/p'),
        object: mockLiteral('v'),
      };

      await bridge.addTriples([triple]);

      expect(bridge.state).toBe('Ready');
      expect(bridge.isReady()).toBe(true);
    });
  });

  describe('queryPattern', () => {
    beforeEach(async () => {
      // Add test data
      await bridge.addTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p2'),
          object: mockLiteral('v2'),
        },
        {
          subject: mockNamedNode('http://example.org/s2'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v3'),
        },
      ]);
    });

    it('should query all triples with null pattern', async () => {
      const results = await bridge.queryPattern(null, null, null);

      expect(results).toHaveLength(3);
    });

    it('should query by subject', async () => {
      const results = await bridge.queryPattern(
        mockNamedNode('http://example.org/s1'),
        null,
        null
      );

      expect(results).toHaveLength(2);
      expect(results[0].subject.value).toBe('http://example.org/s1');
      expect(results[1].subject.value).toBe('http://example.org/s1');
    });

    it('should query by predicate', async () => {
      const results = await bridge.queryPattern(
        null,
        mockNamedNode('http://example.org/p1'),
        null
      );

      expect(results).toHaveLength(2);
    });

    it('should query by object', async () => {
      const results = await bridge.queryPattern(null, null, mockLiteral('v1'));

      expect(results).toHaveLength(1);
      expect(results[0].object.value).toBe('v1');
    });

    it('should query by subject and predicate', async () => {
      const results = await bridge.queryPattern(
        mockNamedNode('http://example.org/s1'),
        mockNamedNode('http://example.org/p1'),
        null
      );

      expect(results).toHaveLength(1);
    });

    it('should return empty array when no matches', async () => {
      const results = await bridge.queryPattern(
        mockNamedNode('http://example.org/nonexistent'),
        null,
        null
      );

      expect(results).toHaveLength(0);
    });

    it('should update query stats', async () => {
      await bridge.queryPattern(null, null, null);

      const stats = bridge.stats;
      expect(stats.queryCount).toBe(1);
    });

    it('should return normalized triple format', async () => {
      const results = await bridge.queryPattern(null, null, null);

      expect(results[0]).toHaveProperty('subject');
      expect(results[0]).toHaveProperty('predicate');
      expect(results[0]).toHaveProperty('object');
      expect(results[0]).toHaveProperty('graph');
    });
  });

  describe('removeTriples', () => {
    beforeEach(async () => {
      await bridge.addTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
        {
          subject: mockNamedNode('http://example.org/s2'),
          predicate: mockNamedNode('http://example.org/p2'),
          object: mockLiteral('v2'),
        },
      ]);
    });

    it('should remove single triple', async () => {
      expect(store.size).toBe(2);

      const result = await bridge.removeTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
      ]);

      expect(result.success).toBe(true);
      expect(result.count).toBe(1);
      expect(store.size).toBe(1);
    });

    it('should remove multiple triples in batch', async () => {
      const result = await bridge.removeTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
        {
          subject: mockNamedNode('http://example.org/s2'),
          predicate: mockNamedNode('http://example.org/p2'),
          object: mockLiteral('v2'),
        },
      ]);

      expect(result.success).toBe(true);
      expect(result.count).toBe(2);
      expect(store.size).toBe(0);
    });

    it('should update remove stats', async () => {
      await bridge.removeTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
      ]);

      const stats = bridge.stats;
      expect(stats.removeCount).toBe(1);
    });

    it('should throw error when triples is not an array', async () => {
      await expect(bridge.removeTriples('not-array')).rejects.toThrow('must be an array');
    });
  });

  describe('getAllTriples', () => {
    it('should return all triples from store', async () => {
      await bridge.addTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
        {
          subject: mockNamedNode('http://example.org/s2'),
          predicate: mockNamedNode('http://example.org/p2'),
          object: mockLiteral('v2'),
        },
      ]);

      const results = await bridge.getAllTriples();

      expect(results).toHaveLength(2);
    });

    it('should return empty array for empty store', async () => {
      const results = await bridge.getAllTriples();

      expect(results).toHaveLength(0);
    });

    it('should update query stats', async () => {
      await bridge.getAllTriples();

      const stats = bridge.stats;
      expect(stats.queryCount).toBe(1);
    });
  });

  describe('sparqlQuery', () => {
    beforeEach(async () => {
      await bridge.addTriples([
        {
          subject: mockNamedNode('http://example.org/s1'),
          predicate: mockNamedNode('http://example.org/p1'),
          object: mockLiteral('v1'),
        },
      ]);
    });

    it('should execute SELECT query', async () => {
      const results = await bridge.sparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });

    it('should execute ASK query', async () => {
      const result = await bridge.sparqlQuery('ASK { ?s ?p ?o }');

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('should execute CONSTRUCT query', async () => {
      const results = await bridge.sparqlQuery('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');

      expect(Array.isArray(results)).toBe(true);
    });

    it('should throw error for empty query', async () => {
      await expect(bridge.sparqlQuery('')).rejects.toThrow(
        'query is required and must be a non-empty string'
      );
    });

    it('should throw error for non-string query', async () => {
      await expect(bridge.sparqlQuery(123)).rejects.toThrow(
        'query is required and must be a non-empty string'
      );
    });

    it('should throw error when store lacks query method', async () => {
      const storeWithoutQuery = {
        add: vi.fn(),
        match: vi.fn().mockReturnValue([]),
        delete: vi.fn(),
      };
      const b = new OxigraphBridge(storeWithoutQuery);

      await expect(b.sparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o }')).rejects.toThrow(
        'Store does not support SPARQL queries'
      );
    });
  });

  describe('Roundtrip Integration', () => {
    it('should complete JS triple -> Oxigraph -> JS result roundtrip', async () => {
      // Step 1: Create triples in JS (simulating BEAM message)
      const originalTriple = {
        subject: mockNamedNode('http://example.org/person/1'),
        predicate: mockNamedNode('http://schema.org/name'),
        object: mockLiteral('John Doe'),
      };

      // Step 2: Add to Oxigraph via bridge
      const addResult = await bridge.addTriples([originalTriple]);
      expect(addResult.success).toBe(true);

      // Step 3: Query back from Oxigraph
      const queryResult = await bridge.queryPattern(
        mockNamedNode('http://example.org/person/1'),
        null,
        null
      );

      // Step 4: Verify roundtrip
      expect(queryResult).toHaveLength(1);
      expect(queryResult[0].subject.value).toBe('http://example.org/person/1');
      expect(queryResult[0].predicate.value).toBe('http://schema.org/name');
      expect(queryResult[0].object.value).toBe('John Doe');
    });

    it('should handle multiple roundtrips correctly', async () => {
      // Multiple add operations
      for (let i = 0; i < 5; i++) {
        await bridge.addTriples([
          {
            subject: mockNamedNode(`http://example.org/item/${i}`),
            predicate: mockNamedNode('http://schema.org/identifier'),
            object: mockLiteral(`ID-${i}`),
          },
        ]);
      }

      // Query all
      const allTriples = await bridge.getAllTriples();
      expect(allTriples).toHaveLength(5);

      // Query specific
      const item2 = await bridge.queryPattern(
        mockNamedNode('http://example.org/item/2'),
        null,
        null
      );
      expect(item2).toHaveLength(1);
      expect(item2[0].object.value).toBe('ID-2');

      // Remove one
      await bridge.removeTriples([
        {
          subject: mockNamedNode('http://example.org/item/2'),
          predicate: mockNamedNode('http://schema.org/identifier'),
          object: mockLiteral('ID-2'),
        },
      ]);

      // Verify removal
      const afterRemove = await bridge.getAllTriples();
      expect(afterRemove).toHaveLength(4);
    });
  });

  describe('State Management', () => {
    it('should be Ready after construction', () => {
      expect(bridge.state).toBe('Ready');
    });

    it('should return to Ready after operations', async () => {
      await bridge.addTriples([
        {
          subject: mockNamedNode('http://example.org/s'),
          predicate: mockNamedNode('http://example.org/p'),
          object: mockLiteral('v'),
        },
      ]);
      expect(bridge.state).toBe('Ready');

      await bridge.queryPattern(null, null, null);
      expect(bridge.state).toBe('Ready');

      await bridge.getAllTriples();
      expect(bridge.state).toBe('Ready');
    });

    it('should transition to Destroyed after destroy()', () => {
      bridge.destroy();
      expect(bridge.state).toBe('Destroyed');
    });

    it('should throw error when operating on destroyed bridge', async () => {
      bridge.destroy();

      await expect(
        bridge.addTriples([
          {
            subject: mockNamedNode('http://example.org/s'),
            predicate: mockNamedNode('http://example.org/p'),
            object: mockLiteral('v'),
          },
        ])
      ).rejects.toThrow('Bridge has been destroyed');

      await expect(bridge.queryPattern(null, null, null)).rejects.toThrow(
        'Bridge has been destroyed'
      );
    });
  });

  describe('Error Handling', () => {
    it('should recover to Ready state on error', async () => {
      // Force an error by providing invalid triple structure
      try {
        await bridge.addTriples([{ invalid: 'structure' }]);
      } catch (e) {
        // Expected to throw
      }

      // Should still be Ready
      expect(bridge.state).toBe('Ready');
      expect(bridge.isReady()).toBe(true);
    });

    it('should increment error count on failure', async () => {
      const storeWithErrorQuery = {
        add: vi.fn(),
        match: vi.fn().mockReturnValue([]),
        delete: vi.fn(),
        query: vi.fn().mockImplementation(() => {
          throw new Error('Query failed');
        }),
      };
      const b = new OxigraphBridge(storeWithErrorQuery);

      try {
        await b.sparqlQuery('SELECT * WHERE { ?s ?p ?o }');
      } catch (e) {
        // Expected
      }

      expect(b.stats.errorCount).toBe(1);
    });
  });

  describe('BRIDGE_OPERATIONS export', () => {
    it('should export operation constants', () => {
      expect(BRIDGE_OPERATIONS).toBeDefined();
      expect(BRIDGE_OPERATIONS.ADD_TRIPLES).toBe('bridge.add_triples');
      expect(BRIDGE_OPERATIONS.QUERY_PATTERN).toBe('bridge.query_pattern');
      expect(BRIDGE_OPERATIONS.REMOVE_TRIPLES).toBe('bridge.remove_triples');
      expect(BRIDGE_OPERATIONS.GET_ALL_TRIPLES).toBe('bridge.get_all_triples');
      expect(BRIDGE_OPERATIONS.SPARQL_QUERY).toBe('bridge.sparql_query');
    });
  });
});
