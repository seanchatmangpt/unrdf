/**
 * @file SPARQL Executor Tests
 * @module cli/commands/sync/sparql-executor.test
 */

import { describe, it, expect, vi } from 'vitest';
import {
  executeSparqlQuery,
  executeParameterizedQuery,
  buildPrefixDeclarations,
  transformResults,
  substituteParameters,
  validateSparqlQuery,
  SparqlExecutionError,
} from '../../../../src/cli/commands/sync/sparql-executor.mjs';

function createMockStore(queryResults = []) {
  return { query: vi.fn().mockReturnValue(queryResults) };
}

function createSelectResults(rows) {
  return rows.map(row => {
    const map = new Map();
    for (const [key, value] of Object.entries(row)) {
      map.set(key, { termType: 'NamedNode', value });
    }
    return map;
  });
}

function createLiteralTerm(value, language = null, datatype = null) {
  return { termType: 'Literal', value, language, datatype: datatype ? { value: datatype } : null };
}

describe('sparql-executor', () => {
  it('should build prefixes, transform results (Map/ASK/quad/null/literal), and extract localName/namespace', () => {
    // Build prefixes
    expect(buildPrefixDeclarations({ foaf: 'http://xmlns.com/foaf/0.1/' })).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
    expect(buildPrefixDeclarations({})).toContain('PREFIX rdf:');
    expect(buildPrefixDeclarations({ rdf: 'http://custom.org/' })).toContain('PREFIX rdf: <http://custom.org/>');
    expect(buildPrefixDeclarations({})).toContain('PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>');

    // Transform Map bindings
    const mapResults = createSelectResults([
      { class: 'http://example.org/Person', label: 'Person' },
    ]);
    const transformed = transformResults(mapResults);
    expect(transformed).toHaveLength(1);
    expect(transformed[0].class).toBe('http://example.org/Person');
    expect(transformed[0]._index).toBe(0);
    expect(transformed[0]._meta.class.localName).toBe('Person');
    expect(transformed[0]._meta.class.namespace).toBe('http://example.org/');

    // ASK results
    const askResult = transformResults(true);
    expect(askResult).toHaveLength(1);
    expect(askResult[0].result).toBe(true);
    expect(askResult[0]._isBoolean).toBe(true);

    // Empty/null
    expect(transformResults([])).toEqual([]);
    expect(transformResults(null)).toEqual([]);

    // Quad results
    const quadResults = [{
      subject: { termType: 'NamedNode', value: 'http://example.org/s1' },
      predicate: { termType: 'NamedNode', value: 'http://example.org/p1' },
      object: { termType: 'Literal', value: 'value1' },
      graph: null,
    }];
    const quadResult = transformResults(quadResults);
    expect(quadResult).toHaveLength(1);
    expect(quadResult[0]._isQuad).toBe(true);

    // Literal with language and datatype
    const litMap = new Map();
    litMap.set('label', createLiteralTerm('Hello', 'en'));
    litMap.set('count', createLiteralTerm('42', null, 'http://www.w3.org/2001/XMLSchema#integer'));
    const litResult = transformResults([litMap]);
    expect(litResult[0]._meta.label.language).toBe('en');
    expect(litResult[0]._meta.count.datatype).toBe('http://www.w3.org/2001/XMLSchema#integer');
  });

  it('should substitute parameters (string, URI, literal, numeric, boolean, escape, prefix)', () => {
    // String parameter
    expect(substituteParameters('SELECT * WHERE { $subject a $type }', {
      subject: 'http://example.org/item', type: 'owl:Class',
    })).toContain('<http://example.org/item>');

    // Typed URI parameter
    expect(substituteParameters('SELECT * WHERE { $entity a owl:Class }', {
      entity: { value: 'http://example.org/Person', type: 'uri' },
    })).toBe('SELECT * WHERE { <http://example.org/Person> a owl:Class }');

    // Literal with language
    expect(substituteParameters('SELECT * WHERE { ?s rdfs:label $label }', {
      label: { value: 'Hello', type: 'literal', language: 'en' },
    })).toContain('"Hello"@en');

    // Typed literal
    expect(substituteParameters('SELECT * WHERE { ?s ex:count $count }', {
      count: { value: '42', type: 'typed-literal', datatype: 'http://www.w3.org/2001/XMLSchema#integer' },
    })).toContain('"42"^^<http://www.w3.org/2001/XMLSchema#integer>');

    // Numeric and boolean
    expect(substituteParameters('SELECT * WHERE { ?s ex:age $age }', { age: 25 })).toContain('25');
    expect(substituteParameters('SELECT * WHERE { ?s ex:active $active }', { active: true })).toContain('true');

    // Escape special characters
    const escaped = substituteParameters('SELECT * WHERE { ?s rdfs:comment $comment }', {
      comment: 'Line 1\nLine 2\twith "quotes"',
    });
    expect(escaped).toContain('\\n');
    expect(escaped).toContain('\\t');
    expect(escaped).toContain('\\"');

    // Prefixed name without modification
    expect(substituteParameters('SELECT * WHERE { ?s a $type }', { type: 'foaf:Person' })).toBe('SELECT * WHERE { ?s a foaf:Person }');
  });

  it('should validate SPARQL queries and execute with timeout/error handling', async () => {
    // Valid queries
    expect(validateSparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o }').valid).toBe(true);
    expect(validateSparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o }').type).toBe('SELECT');
    expect(validateSparqlQuery('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }').type).toBe('CONSTRUCT');
    expect(validateSparqlQuery('ASK { ?s a owl:Class }').type).toBe('ASK');
    expect(validateSparqlQuery('DESCRIBE ?s WHERE { ?s a owl:Class }').type).toBe('DESCRIBE');

    // Invalid: no query type
    expect(validateSparqlQuery('{ ?s ?p ?o }').valid).toBe(false);
    // Invalid: unbalanced braces
    expect(validateSparqlQuery('SELECT ?s WHERE { ?s ?p ?o').valid).toBe(false);
    // Empty/null
    expect(validateSparqlQuery('').valid).toBe(false);
    expect(validateSparqlQuery(null).valid).toBe(false);
    // Undefined prefix warning
    expect(validateSparqlQuery('SELECT ?s WHERE { ?s custom:prop ?o }').issues.some(i => i.type === 'warning')).toBe(true);

    // Execute with store
    const store = createMockStore(createSelectResults([{ class: 'http://example.org/Person' }]));
    const results = await executeSparqlQuery(store, 'SELECT ?class WHERE { ?class a owl:Class }');
    expect(results).toHaveLength(1);
    expect(results[0].class).toBe('http://example.org/Person');

    // Error: null store
    await expect(executeSparqlQuery(null, 'SELECT ?s WHERE { ?s ?p ?o }')).rejects.toThrow(SparqlExecutionError);
    // Error: empty query
    await expect(executeSparqlQuery(store, '')).rejects.toThrow(SparqlExecutionError);
    // Error: parse error
    const errorStore = { query: vi.fn().mockImplementation(() => { throw new Error('Parse error'); }) };
    await expect(executeSparqlQuery(errorStore, 'INVALID')).rejects.toThrow(SparqlExecutionError);
    // Timeout
    const slowStore = { query: vi.fn().mockImplementation(() => new Promise(resolve => setTimeout(resolve, 10000))) };
    await expect(executeSparqlQuery(slowStore, 'SELECT ?s WHERE { ?s ?p ?o }', {}, { timeout: 100 })).rejects.toThrow(/timed out/);
  }, 1000);
});
