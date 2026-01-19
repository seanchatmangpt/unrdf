/**
 * @file SPARQL Executor Tests
 * @module cli/commands/sync/sparql-executor.test
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  executeSparqlQuery,
  executeParameterizedQuery,
  buildPrefixDeclarations,
  transformResults,
  substituteParameters,
  validateSparqlQuery,
  SparqlExecutionError,
} from '../../../../src/cli/commands/sync/sparql-executor.mjs';

/**
 * Create a mock store that simulates OxigraphStore behavior
 */
function createMockStore(queryResults = []) {
  return {
    query: vi.fn().mockReturnValue(queryResults),
  };
}

/**
 * Create mock SELECT results as Maps (raw Oxigraph format)
 */
function createSelectResults(rows) {
  return rows.map(row => {
    const map = new Map();
    for (const [key, value] of Object.entries(row)) {
      map.set(key, {
        termType: 'NamedNode',
        value,
      });
    }
    return map;
  });
}

/**
 * Create mock literal result
 */
function createLiteralTerm(value, language = null, datatype = null) {
  return {
    termType: 'Literal',
    value,
    language,
    datatype: datatype ? { value: datatype } : null,
  };
}

describe('sparql-executor', () => {
  describe('buildPrefixDeclarations', () => {
    it('should build PREFIX declarations from prefix map', () => {
      const prefixes = {
        foaf: 'http://xmlns.com/foaf/0.1/',
        ex: 'http://example.org/',
      };

      const result = buildPrefixDeclarations(prefixes);

      expect(result).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
      expect(result).toContain('PREFIX ex: <http://example.org/>');
    });

    it('should include common prefixes by default', () => {
      const result = buildPrefixDeclarations({});

      expect(result).toContain('PREFIX rdf:');
      expect(result).toContain('PREFIX rdfs:');
      expect(result).toContain('PREFIX owl:');
      expect(result).toContain('PREFIX xsd:');
    });

    it('should allow custom prefixes to override common ones', () => {
      const prefixes = {
        rdf: 'http://custom.rdf.org/',
      };

      const result = buildPrefixDeclarations(prefixes);

      expect(result).toContain('PREFIX rdf: <http://custom.rdf.org/>');
      expect(result).not.toContain('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    });

    it('should return empty string for no prefixes when common prefixes are empty', () => {
      // This tests with empty object - common prefixes will be included
      const result = buildPrefixDeclarations({});
      expect(result).not.toBe('');
    });
  });

  describe('transformResults', () => {
    it('should transform Map bindings to plain objects', () => {
      const mapResults = createSelectResults([
        { class: 'http://example.org/Person', label: 'Person' },
        { class: 'http://example.org/Organization', label: 'Organization' },
      ]);

      const result = transformResults(mapResults);

      expect(result).toHaveLength(2);
      expect(result[0].class).toBe('http://example.org/Person');
      expect(result[0].label).toBe('Person');
      expect(result[0]._index).toBe(0);
      expect(result[0]._meta).toBeDefined();
    });

    it('should handle boolean ASK results', () => {
      const result = transformResults(true);

      expect(result).toHaveLength(1);
      expect(result[0].result).toBe(true);
      expect(result[0]._isBoolean).toBe(true);
    });

    it('should handle empty results', () => {
      expect(transformResults([])).toEqual([]);
      expect(transformResults(null)).toEqual([]);
      expect(transformResults(undefined)).toEqual([]);
    });

    it('should handle quad results (CONSTRUCT/DESCRIBE)', () => {
      const quads = [
        {
          subject: { termType: 'NamedNode', value: 'http://example.org/s1' },
          predicate: { termType: 'NamedNode', value: 'http://example.org/p1' },
          object: { termType: 'Literal', value: 'value1' },
          graph: null,
        },
      ];

      const result = transformResults(quads);

      expect(result).toHaveLength(1);
      expect(result[0]._isQuad).toBe(true);
      expect(result[0].subject).toBe('http://example.org/s1');
      expect(result[0].predicate).toBe('http://example.org/p1');
      expect(result[0].object).toBe('value1');
    });

    it('should extract local name and namespace for URIs', () => {
      const mapResults = createSelectResults([
        { class: 'http://example.org/ontology#Person' },
      ]);

      const result = transformResults(mapResults);

      expect(result[0]._meta.class.localName).toBe('Person');
      expect(result[0]._meta.class.namespace).toBe('http://example.org/ontology#');
    });

    it('should handle literal terms with language and datatype', () => {
      const map = new Map();
      map.set('label', createLiteralTerm('Hello', 'en'));
      map.set('count', createLiteralTerm('42', null, 'http://www.w3.org/2001/XMLSchema#integer'));

      const result = transformResults([map]);

      expect(result[0].label).toBe('Hello');
      expect(result[0]._meta.label.language).toBe('en');
      expect(result[0].count).toBe('42');
      expect(result[0]._meta.count.datatype).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });
  });

  describe('substituteParameters', () => {
    it('should substitute simple string parameters', () => {
      const template = 'SELECT * WHERE { $subject a $type }';
      const params = {
        subject: 'http://example.org/item',
        type: 'owl:Class',
      };

      const result = substituteParameters(template, params);

      expect(result).toContain('<http://example.org/item>');
      expect(result).toContain('owl:Class');
    });

    it('should substitute typed URI parameters', () => {
      const template = 'SELECT * WHERE { $entity a owl:Class }';
      const params = {
        entity: { value: 'http://example.org/Person', type: 'uri' },
      };

      const result = substituteParameters(template, params);

      expect(result).toBe('SELECT * WHERE { <http://example.org/Person> a owl:Class }');
    });

    it('should substitute literal parameters with language', () => {
      const template = 'SELECT * WHERE { ?s rdfs:label $label }';
      const params = {
        label: { value: 'Hello', type: 'literal', language: 'en' },
      };

      const result = substituteParameters(template, params);

      expect(result).toContain('"Hello"@en');
    });

    it('should substitute typed literal parameters', () => {
      const template = 'SELECT * WHERE { ?s ex:count $count }';
      const params = {
        count: { value: '42', type: 'typed-literal', datatype: 'http://www.w3.org/2001/XMLSchema#integer' },
      };

      const result = substituteParameters(template, params);

      expect(result).toContain('"42"^^<http://www.w3.org/2001/XMLSchema#integer>');
    });

    it('should substitute numeric parameters', () => {
      const template = 'SELECT * WHERE { ?s ex:age $age }';
      const params = { age: 25 };

      const result = substituteParameters(template, params);

      expect(result).toContain('25');
    });

    it('should substitute boolean parameters', () => {
      const template = 'SELECT * WHERE { ?s ex:active $active }';
      const params = { active: true };

      const result = substituteParameters(template, params);

      expect(result).toContain('true');
    });

    it('should escape special characters in string literals', () => {
      const template = 'SELECT * WHERE { ?s rdfs:comment $comment }';
      const params = { comment: 'Line 1\nLine 2\twith "quotes"' };

      const result = substituteParameters(template, params);

      expect(result).toContain('\\n');
      expect(result).toContain('\\t');
      expect(result).toContain('\\"');
    });

    it('should handle prefixed names without modification', () => {
      const template = 'SELECT * WHERE { ?s a $type }';
      const params = { type: 'foaf:Person' };

      const result = substituteParameters(template, params);

      expect(result).toBe('SELECT * WHERE { ?s a foaf:Person }');
    });
  });

  describe('validateSparqlQuery', () => {
    it('should validate a correct SELECT query', () => {
      const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
      const result = validateSparqlQuery(query);

      expect(result.valid).toBe(true);
      expect(result.type).toBe('SELECT');
      expect(result.variables).toContain('s');
      expect(result.variables).toContain('p');
      expect(result.variables).toContain('o');
    });

    it('should validate CONSTRUCT queries', () => {
      const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
      const result = validateSparqlQuery(query);

      expect(result.valid).toBe(true);
      expect(result.type).toBe('CONSTRUCT');
    });

    it('should validate ASK queries', () => {
      const query = 'ASK { ?s a owl:Class }';
      const result = validateSparqlQuery(query);

      expect(result.valid).toBe(true);
      expect(result.type).toBe('ASK');
    });

    it('should validate DESCRIBE queries', () => {
      const query = 'DESCRIBE ?s WHERE { ?s a owl:Class }';
      const result = validateSparqlQuery(query);

      expect(result.valid).toBe(true);
      expect(result.type).toBe('DESCRIBE');
    });

    it('should detect invalid query without query type', () => {
      const query = '{ ?s ?p ?o }';
      const result = validateSparqlQuery(query);

      expect(result.valid).toBe(false);
      expect(result.issues.some(i => i.type === 'error')).toBe(true);
    });

    it('should detect unbalanced braces', () => {
      const query = 'SELECT ?s WHERE { ?s ?p ?o';
      const result = validateSparqlQuery(query);

      expect(result.valid).toBe(false);
      expect(result.issues.some(i => i.message.includes('Unbalanced braces'))).toBe(true);
    });

    it('should handle empty or null queries', () => {
      expect(validateSparqlQuery('')).toEqual(expect.objectContaining({ valid: false }));
      expect(validateSparqlQuery(null)).toEqual(expect.objectContaining({ valid: false }));
    });

    it('should warn about undefined prefixes', () => {
      const query = 'SELECT ?s WHERE { ?s custom:prop ?o }';
      const result = validateSparqlQuery(query);

      // Should have a warning about undefined prefix
      expect(result.issues.some(i => i.type === 'warning' && i.message.includes('custom'))).toBe(true);
    });
  });

  describe('executeSparqlQuery', () => {
    it('should execute a simple query and return results', async () => {
      const mockResults = createSelectResults([
        { class: 'http://example.org/Person' },
        { class: 'http://example.org/Organization' },
      ]);
      const store = createMockStore(mockResults);

      const results = await executeSparqlQuery(
        store,
        'SELECT ?class WHERE { ?class a owl:Class }'
      );

      expect(store.query).toHaveBeenCalled();
      expect(results).toHaveLength(2);
      expect(results[0].class).toBe('http://example.org/Person');
    });

    it('should prepend prefix declarations', async () => {
      const store = createMockStore([]);

      await executeSparqlQuery(
        store,
        'SELECT ?class WHERE { ?class a owl:Class }',
        { ex: 'http://example.org/' }
      );

      const calledQuery = store.query.mock.calls[0][0];
      expect(calledQuery).toContain('PREFIX ex: <http://example.org/>');
      expect(calledQuery).toContain('PREFIX owl:');
    });

    it('should throw SparqlExecutionError for invalid store', async () => {
      await expect(executeSparqlQuery(null, 'SELECT ?s WHERE { ?s ?p ?o }'))
        .rejects.toThrow(SparqlExecutionError);
    });

    it('should throw SparqlExecutionError for empty query', async () => {
      const store = createMockStore([]);

      await expect(executeSparqlQuery(store, ''))
        .rejects.toThrow(SparqlExecutionError);
    });

    it('should handle query execution errors', async () => {
      const store = {
        query: vi.fn().mockImplementation(() => {
          throw new Error('Parse error: unexpected token');
        }),
      };

      await expect(executeSparqlQuery(store, 'INVALID QUERY'))
        .rejects.toThrow(SparqlExecutionError);
    });

    it('should timeout long-running queries', async () => {
      const store = {
        query: vi.fn().mockImplementation(() => {
          return new Promise(resolve => setTimeout(resolve, 10000));
        }),
      };

      await expect(executeSparqlQuery(
        store,
        'SELECT ?s WHERE { ?s ?p ?o }',
        {},
        { timeout: 100 }
      )).rejects.toThrow(/timed out/);
    }, 1000);
  });

  describe('executeParameterizedQuery', () => {
    it('should substitute parameters and execute query', async () => {
      const mockResults = createSelectResults([
        { prop: 'http://example.org/name' },
      ]);
      const store = createMockStore(mockResults);

      const results = await executeParameterizedQuery(
        store,
        'SELECT ?prop WHERE { $class ?prop ?value }',
        { class: { value: 'http://example.org/Person', type: 'uri' } }
      );

      const calledQuery = store.query.mock.calls[0][0];
      expect(calledQuery).toContain('<http://example.org/Person>');
      expect(results).toHaveLength(1);
    });

    it('should handle multiple parameters', async () => {
      const store = createMockStore([]);

      await executeParameterizedQuery(
        store,
        'SELECT ?result WHERE { $subject $predicate ?result }',
        {
          subject: { value: 'http://example.org/item', type: 'uri' },
          predicate: 'rdfs:label',
        }
      );

      const calledQuery = store.query.mock.calls[0][0];
      expect(calledQuery).toContain('<http://example.org/item>');
      expect(calledQuery).toContain('rdfs:label');
    });
  });

  describe('SparqlExecutionError', () => {
    it('should include query and phase in error', () => {
      const error = new SparqlExecutionError('Test error', {
        query: 'SELECT ?s WHERE { ?s ?p ?o }',
        phase: 'execute',
      });

      expect(error.name).toBe('SparqlExecutionError');
      expect(error.message).toBe('Test error');
      expect(error.query).toBe('SELECT ?s WHERE { ?s ?p ?o }');
      expect(error.phase).toBe('execute');
    });

    it('should include cause error', () => {
      const cause = new Error('Original error');
      const error = new SparqlExecutionError('Wrapped error', { cause });

      expect(error.cause).toBe(cause);
    });
  });
});
