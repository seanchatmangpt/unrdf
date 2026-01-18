/**
 * @file SPARQL Executor Tests
 * @module cli/sync/sparql-executor.test
 * @description Tests for SPARQL query execution against RDF ontology store
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';

// Mock @unrdf/core to control executeQuery behavior
vi.mock('@unrdf/core', () => ({
  COMMON_PREFIXES: {
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    owl: 'http://www.w3.org/2002/07/owl#',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
    foaf: 'http://xmlns.com/foaf/0.1/',
    dcterms: 'http://purl.org/dc/terms/',
    skos: 'http://www.w3.org/2004/02/skos/core#',
  },
  // executeQuery will throw so it falls through to store.query
  executeQuery: vi.fn().mockImplementation(() => {
    throw new Error('Mock executeQuery - fallthrough to store');
  }),
}));

import {
  executeSparqlQuery,
  buildPrefixDeclarations,
  transformResults,
} from '../../src/cli/commands/sync/sparql-executor.mjs';

/**
 * Create a mock store that simulates RDF store behavior
 * @param {Array|Function} queryResults - Results to return or function to execute
 * @returns {Object} Mock store with query method
 */
function createMockStore(queryResults = []) {
  return {
    query: vi.fn().mockImplementation(() => {
      if (typeof queryResults === 'function') {
        return queryResults();
      }
      return queryResults;
    }),
  };
}

/**
 * Create mock SELECT results with NamedNode format
 * @param {Array<Object>} rows - Rows with key-value pairs
 * @returns {Array<Object>} Mock bindings
 */
function createSelectBindings(rows) {
  return rows.map((row) => {
    const binding = {};
    for (const [key, value] of Object.entries(row)) {
      if (typeof value === 'string' && value.startsWith('http')) {
        binding[key] = {
          termType: 'NamedNode',
          type: 'uri',
          value,
        };
      } else {
        binding[key] = {
          termType: 'Literal',
          type: 'literal',
          value: String(value),
        };
      }
    }
    return binding;
  });
}

describe('sparql-executor', () => {
  describe('executeSparqlQuery()', () => {
    it('should execute valid SELECT query and return results', async () => {
      // Arrange
      const mockResults = createSelectBindings([
        { class: 'http://example.org/Person' },
        { class: 'http://example.org/Organization' },
      ]);
      const store = createMockStore(mockResults);
      const query = 'SELECT ?class WHERE { ?class a owl:Class }';

      // Act
      const results = await executeSparqlQuery(store, query);

      // Assert
      expect(store.query).toHaveBeenCalled();
      expect(results).toHaveLength(2);
      expect(results[0]['?class']).toBe('http://example.org/Person');
      expect(results[1]['?class']).toBe('http://example.org/Organization');
    });

    it('should prepend prefix declarations to query', async () => {
      // Arrange
      const store = createMockStore([]);
      const query = 'SELECT ?class WHERE { ?class a owl:Class }';
      const customPrefixes = { ex: 'http://example.org/' };

      // Act
      await executeSparqlQuery(store, query, customPrefixes);

      // Assert
      const calledQuery = store.query.mock.calls[0][0];
      expect(calledQuery).toContain('PREFIX ex: <http://example.org/>');
      expect(calledQuery).toContain('PREFIX owl:');
      expect(calledQuery).toContain('PREFIX rdf:');
    });

    it('should handle query timeout', async () => {
      // Arrange
      const store = {
        query: vi.fn().mockImplementation(() => {
          return new Promise((resolve) => setTimeout(resolve, 10000));
        }),
      };
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      // Act & Assert
      await expect(
        executeSparqlQuery(store, query, {}, { timeout: 50 })
      ).rejects.toThrow(/timed out/);
    }, 1000);

    it('should handle store query errors', async () => {
      // Arrange
      const store = {
        query: vi.fn().mockImplementation(() => {
          throw new Error('Parse error: unexpected token');
        }),
      };
      const query = 'INVALID QUERY SYNTAX';

      // Act & Assert
      await expect(executeSparqlQuery(store, query)).rejects.toThrow(
        'SPARQL query failed'
      );
    });

    it('should handle store with execute method', async () => {
      // Arrange
      const mockResults = createSelectBindings([{ s: 'http://example.org/s1' }]);
      const store = {
        execute: vi.fn().mockReturnValue(mockResults),
      };
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      // Act
      const results = await executeSparqlQuery(store, query);

      // Assert
      expect(store.execute).toHaveBeenCalled();
      expect(results).toHaveLength(1);
    });

    it('should throw error for store without query support', async () => {
      // Arrange
      const store = {};
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      // Act & Assert
      await expect(executeSparqlQuery(store, query)).rejects.toThrow(
        'does not support SPARQL queries'
      );
    });

    it('should use default timeout of 5000ms', async () => {
      // Arrange
      const mockResults = [];
      const store = createMockStore(mockResults);
      const query = 'SELECT ?s WHERE { ?s ?p ?o }';

      // Act
      const results = await executeSparqlQuery(store, query);

      // Assert - should complete without timeout error
      expect(results).toEqual([]);
    });
  });

  describe('buildPrefixDeclarations()', () => {
    it('should generate PREFIX statements from prefixes object', () => {
      // Arrange
      const prefixes = {
        foaf: 'http://xmlns.com/foaf/0.1/',
        ex: 'http://example.org/',
      };

      // Act
      const result = buildPrefixDeclarations(prefixes);

      // Assert
      expect(result).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
      expect(result).toContain('PREFIX ex: <http://example.org/>');
    });

    it('should include COMMON_PREFIXES by default', () => {
      // Arrange - empty custom prefixes

      // Act
      const result = buildPrefixDeclarations({});

      // Assert
      expect(result).toContain('PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>');
      expect(result).toContain('PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>');
      expect(result).toContain('PREFIX owl: <http://www.w3.org/2002/07/owl#>');
      expect(result).toContain('PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>');
      expect(result).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
      expect(result).toContain('PREFIX dcterms: <http://purl.org/dc/terms/>');
      expect(result).toContain('PREFIX skos: <http://www.w3.org/2004/02/skos/core#>');
    });

    it('should allow custom prefixes to override common prefixes', () => {
      // Arrange
      const prefixes = {
        rdf: 'http://custom.rdf.org/',
      };

      // Act
      const result = buildPrefixDeclarations(prefixes);

      // Assert
      expect(result).toContain('PREFIX rdf: <http://custom.rdf.org/>');
      expect(result).not.toContain('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    });

    it('should join PREFIX statements with newlines', () => {
      // Arrange
      const prefixes = {
        ex1: 'http://example1.org/',
        ex2: 'http://example2.org/',
      };

      // Act
      const result = buildPrefixDeclarations(prefixes);

      // Assert
      const lines = result.split('\n');
      expect(lines.length).toBeGreaterThan(1);
      expect(lines.every((line) => line.startsWith('PREFIX '))).toBe(true);
    });
  });

  describe('transformResults()', () => {
    it('should transform bindings to template-friendly format', () => {
      // Arrange
      const bindings = [
        { class: { value: 'http://example.org/Person' } },
        { class: { value: 'http://example.org/Organization' } },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result).toHaveLength(2);
      expect(result[0]['?class']).toBe('http://example.org/Person');
      expect(result[1]['?class']).toBe('http://example.org/Organization');
    });

    it('should add ?var_localName for URI values', () => {
      // Arrange
      const bindings = [
        {
          class: {
            termType: 'NamedNode',
            type: 'uri',
            value: 'http://example.org/ontology#Person',
          },
        },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]['?class']).toBe('http://example.org/ontology#Person');
      expect(result[0]['?class_localName']).toBe('Person');
    });

    it('should handle URI with slash separator', () => {
      // Arrange
      const bindings = [
        {
          resource: {
            termType: 'NamedNode',
            type: 'uri',
            value: 'http://example.org/resources/Document',
          },
        },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]['?resource_localName']).toBe('Document');
    });

    it('should add _index to each row', () => {
      // Arrange
      const bindings = [
        { item: { value: 'A' } },
        { item: { value: 'B' } },
        { item: { value: 'C' } },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]._index).toBe(0);
      expect(result[1]._index).toBe(1);
      expect(result[2]._index).toBe(2);
    });

    it('should handle empty results array', () => {
      // Act
      const result = transformResults([]);

      // Assert
      expect(result).toEqual([]);
    });

    it('should handle SPARQL JSON format (results.results.bindings)', () => {
      // Arrange
      const sparqlJson = {
        results: {
          bindings: [
            { s: { value: 'http://example.org/s1' } },
            { s: { value: 'http://example.org/s2' } },
          ],
        },
      };

      // Act
      const result = transformResults(sparqlJson);

      // Assert
      expect(result).toHaveLength(2);
      expect(result[0]['?s']).toBe('http://example.org/s1');
    });

    it('should handle results.bindings format', () => {
      // Arrange
      const resultsObj = {
        bindings: [{ prop: { value: 'value1' } }],
      };

      // Act
      const result = transformResults(resultsObj);

      // Assert
      expect(result).toHaveLength(1);
      expect(result[0]['?prop']).toBe('value1');
    });

    it('should handle iterable results (generator/iterator)', () => {
      // Arrange
      function* generateBindings() {
        yield { item: { value: 'gen1' } };
        yield { item: { value: 'gen2' } };
      }

      // Act
      const result = transformResults(generateBindings());

      // Assert
      expect(result).toHaveLength(2);
      expect(result[0]['?item']).toBe('gen1');
      expect(result[1]['?item']).toBe('gen2');
    });

    it('should handle null and undefined values', () => {
      // Arrange
      const bindings = [
        { defined: { value: 'hasValue' }, nullable: null, missing: undefined },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]['?defined']).toBe('hasValue');
      expect(result[0]['?nullable']).toBeNull();
      expect(result[0]['?missing']).toBeNull();
    });

    it('should handle primitive string values directly', () => {
      // Arrange
      const bindings = [{ label: 'Plain String' }];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]['?label']).toBe('Plain String');
    });

    it('should normalize keys to start with ?', () => {
      // Arrange
      const bindings = [
        {
          class: { value: 'A' },
          '?property': { value: 'B' },
        },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert - both prefixed and clean keys should be present for compatibility
      expect(result[0]).toHaveProperty('?class');
      expect(result[0]).toHaveProperty('?property');
      expect(result[0]).toHaveProperty('class');
      expect(result[0]).toHaveProperty('property');
    });

    it('should extract value from object with id property', () => {
      // Arrange
      const bindings = [
        {
          subject: { id: 'http://example.org/subject1' },
        },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]['?subject']).toBe('http://example.org/subject1');
    });

    it('should fallback to String() for complex objects', () => {
      // Arrange
      const bindings = [
        {
          complex: {
            toString() {
              return 'custom-string';
            },
          },
        },
      ];

      // Act
      const result = transformResults(bindings);

      // Assert
      expect(result[0]['?complex']).toBe('custom-string');
    });
  });

  describe('Error handling', () => {
    it('should wrap store errors with descriptive message', async () => {
      // Arrange
      const store = {
        query: vi.fn().mockImplementation(() => {
          throw new Error('Network connection lost');
        }),
      };

      // Act & Assert
      try {
        await executeSparqlQuery(store, 'SELECT ?s WHERE { ?s ?p ?o }');
        expect.fail('Should have thrown');
      } catch (err) {
        expect(err.message).toContain('SPARQL query failed');
        expect(err.message).toContain('Network connection lost');
      }
    });

    it('should handle async store errors', async () => {
      // Arrange
      const store = {
        query: vi.fn().mockRejectedValue(new Error('Async query error')),
      };

      // Act & Assert
      await expect(
        executeSparqlQuery(store, 'SELECT ?s WHERE { ?s ?p ?o }')
      ).rejects.toThrow('SPARQL query failed: Async query error');
    });

    it('should handle null store gracefully', async () => {
      // Act & Assert
      await expect(
        executeSparqlQuery(null, 'SELECT ?s WHERE { ?s ?p ?o }')
      ).rejects.toThrow();
    });

    it('should handle undefined store gracefully', async () => {
      // Act & Assert
      await expect(
        executeSparqlQuery(undefined, 'SELECT ?s WHERE { ?s ?p ?o }')
      ).rejects.toThrow();
    });
  });

  describe('Integration scenarios', () => {
    it('should execute query with custom prefixes and return transformed results', async () => {
      // Arrange
      const mockResults = [
        {
          class: {
            termType: 'NamedNode',
            type: 'uri',
            value: 'http://example.org/Person',
          },
          label: {
            termType: 'Literal',
            value: 'Person Class',
          },
        },
      ];
      const store = createMockStore(mockResults);
      const prefixes = { ex: 'http://example.org/' };
      const query = 'SELECT ?class ?label WHERE { ?class rdfs:label ?label }';

      // Act
      const results = await executeSparqlQuery(store, query, prefixes);

      // Assert
      expect(results).toHaveLength(1);
      expect(results[0]['?class']).toBe('http://example.org/Person');
      expect(results[0]['?class_localName']).toBe('Person');
      expect(results[0]['?label']).toBe('Person Class');
      expect(results[0]['?label_localName']).toBeUndefined();
      expect(results[0]._index).toBe(0);
    });

    it('should handle multiple URIs with different separators', async () => {
      // Arrange
      const mockResults = [
        {
          hashUri: {
            termType: 'NamedNode',
            type: 'uri',
            value: 'http://example.org/ontology#EntityA',
          },
          slashUri: {
            termType: 'NamedNode',
            type: 'uri',
            value: 'http://example.org/resources/EntityB',
          },
        },
      ];
      const store = createMockStore(mockResults);

      // Act
      const results = await executeSparqlQuery(
        store,
        'SELECT ?hashUri ?slashUri WHERE { ?s ?p ?o }'
      );

      // Assert
      expect(results[0]['?hashUri_localName']).toBe('EntityA');
      expect(results[0]['?slashUri_localName']).toBe('EntityB');
    });
  });
});
