/**
 * @file SPARQL Executor Tests
 * @module cli/sync/sparql-executor.test
 * @description Tests for SPARQL query execution against RDF ontology store
 */

import { describe, it, expect, vi, beforeEach as _beforeEach } from 'vitest';

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
  executeQuery: vi.fn().mockImplementation(() => {
    throw new Error('Mock executeQuery - fallthrough to store');
  }),
}));

import {
  executeSparqlQuery,
  buildPrefixDeclarations,
  transformResults,
} from '../../src/cli/commands/sync/sparql-executor.mjs';

function createMockStore(queryResults = []) {
  return {
    query: vi.fn().mockImplementation(() => {
      if (typeof queryResults === 'function') return queryResults();
      return queryResults;
    }),
  };
}

function createSelectBindings(rows) {
  return rows.map((row) => {
    const binding = {};
    for (const [key, value] of Object.entries(row)) {
      if (typeof value === 'string' && value.startsWith('http')) {
        binding[key] = { termType: 'NamedNode', type: 'uri', value };
      } else {
        binding[key] = { termType: 'Literal', type: 'literal', value: String(value) };
      }
    }
    return binding;
  });
}

describe('sparql-executor', () => {
  it('should execute valid SELECT query, build prefix declarations, and transform results', async () => {
    const mockResults = createSelectBindings([
      { class: 'http://example.org/Person' },
      { class: 'http://example.org/Organization' },
    ]);
    const store = createMockStore(mockResults);
    const query = 'SELECT ?class WHERE { ?class a owl:Class }';
    const customPrefixes = { ex: 'http://example.org/' };

    // Execute
    const results = await executeSparqlQuery(store, query, customPrefixes);

    // Assert: query was called with prefixes prepended
    const calledQuery = store.query.mock.calls[0][0];
    expect(calledQuery).toContain('PREFIX ex: <http://example.org/>');

    // Assert: results transformed
    expect(results).toHaveLength(2);
    expect(results[0]['?class']).toBe('http://example.org/Person');
    expect(results[0]['?class_localName']).toBe('Person');
    expect(results[1]['?class_localName']).toBe('Organization');
    expect(results[0]._index).toBe(0);
  });

  it('should handle query timeout, store errors, and unsupported stores', async () => {
    // Timeout
    const slowStore = {
      query: vi.fn().mockImplementation(() => new Promise(resolve => setTimeout(resolve, 10000))),
    };
    await expect(
      executeSparqlQuery(slowStore, 'SELECT ?s WHERE { ?s ?p ?o }', {}, { timeout: 50 })
    ).rejects.toThrow(/timed out/);

    // Store error
    const errorStore = {
      query: vi.fn().mockImplementation(() => { throw new Error('Parse error'); }),
    };
    await expect(
      executeSparqlQuery(errorStore, 'INVALID')
    ).rejects.toThrow('SPARQL query failed');

    // Null store
    await expect(
      executeSparqlQuery(null, 'SELECT ?s WHERE { ?s ?p ?o }')
    ).rejects.toThrow();
  });

  it('should handle various result formats: bindings, SPARQL JSON, generators, nulls, primitives', () => {
    // Standard bindings with _index
    const bindings = createSelectBindings([
      { class: 'http://example.org/Person' },
      { item: 'plain' },
    ]);
    const result1 = transformResults(bindings);
    expect(result1).toHaveLength(2);
    expect(result1[0]['?class']).toBe('http://example.org/Person');
    expect(result1[0]['?class_localName']).toBe('Person');
    expect(result1[1]['?item']).toBe('plain');
    expect(result1[0]._index).toBe(0);

    // SPARQL JSON format
    const sparqlJson = { results: { bindings: [{ s: { value: 'http://x.org/s1' } }] } };
    const result2 = transformResults(sparqlJson);
    expect(result2).toHaveLength(1);
    expect(result2[0]['?s']).toBe('http://x.org/s1');

    // Generator
    function* gen() { yield { x: { value: 'a' } }; yield { x: { value: 'b' } }; }
    const result3 = transformResults(gen());
    expect(result3).toHaveLength(2);

    // Empty
    expect(transformResults([])).toEqual([]);

    // Null/undefined values
    const result4 = transformResults([{ defined: { value: 'v' }, nullable: null, missing: undefined }]);
    expect(result4[0]['?defined']).toBe('v');
    expect(result4[0]['?nullable']).toBeNull();

    // buildPrefixDeclarations includes COMMON_PREFIXES and supports override
    const prefixes = buildPrefixDeclarations({ rdf: 'http://custom.org/' });
    expect(prefixes).toContain('PREFIX rdf: <http://custom.org/>');
    expect(prefixes).toContain('PREFIX rdfs: <');
  });
});
