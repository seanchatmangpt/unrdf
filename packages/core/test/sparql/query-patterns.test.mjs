/**
 * @file Tests for SPARQL Query Patterns
 * @module @unrdf/core/test/sparql/query-patterns
 */

import { describe, it, expect } from 'vitest';
import {
  path,
  COMMON_PREFIXES,
  findInstancesOfType,
  findPropertiesOf,
  findByProperty,
  textSearch,
  getSubclasses,
  countInstancesByType,
  describeResource,
  findConnectedResources,
  withCommonPrefixes,
  aggregateQuery,
} from '../../src/sparql/query-patterns.mjs';

describe('PropertyPath', () => {
  it('should create simple property path', () => {
    const p = path('foaf:knows');
    expect(p.toString()).toBe('foaf:knows');
  });

  it('should create sequence path', () => {
    const p = path('foaf:knows').sequence('foaf:name');
    expect(p.toString()).toBe('foaf:knows/foaf:name');
  });

  it('should create alternative path', () => {
    const p = path('foaf:name').alternative('rdfs:label');
    expect(p.toString()).toBe('(foaf:name|rdfs:label)');
  });

  it('should create zero or more path', () => {
    const p = path('foaf:knows').zeroOrMore();
    expect(p.toString()).toBe('foaf:knows*');
  });

  it('should create one or more path', () => {
    const p = path('foaf:knows').oneOrMore();
    expect(p.toString()).toBe('foaf:knows+');
  });

  it('should create zero or one path', () => {
    const p = path('foaf:knows').zeroOrOne();
    expect(p.toString()).toBe('foaf:knows?');
  });

  it('should create inverse path', () => {
    const p = path('foaf:knows').inverse();
    expect(p.toString()).toBe('^foaf:knows');
  });

  it('should chain multiple path operations', () => {
    const p = path('foaf:knows').oneOrMore().sequence('foaf:name');
    expect(p.toString()).toBe('foaf:knows+/foaf:name');
  });

  it('should validate predicate is not empty', () => {
    expect(() => path('')).toThrow();
  });
});

describe('COMMON_PREFIXES', () => {
  it('should have common RDF prefixes', () => {
    expect(COMMON_PREFIXES.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    expect(COMMON_PREFIXES.rdfs).toBe('http://www.w3.org/2000/01/rdf-schema#');
    expect(COMMON_PREFIXES.owl).toBe('http://www.w3.org/2002/07/owl#');
    expect(COMMON_PREFIXES.foaf).toBe('http://xmlns.com/foaf/0.1/');
  });
});

describe('findInstancesOfType', () => {
  it('should generate query for type instances', () => {
    const query = findInstancesOfType('foaf:Person');

    expect(query).toContain('SELECT ?instance');
    expect(query).toContain('?instance rdf:type foaf:Person');
  });

  it('should generate query with properties', () => {
    const query = findInstancesOfType('foaf:Person', {
      properties: ['foaf:name', 'foaf:email'],
    });

    expect(query).toContain('?prop0');
    expect(query).toContain('?prop1');
    expect(query).toContain('OPTIONAL');
  });

  it('should generate query with limit', () => {
    const query = findInstancesOfType('foaf:Person', { limit: 10 });

    expect(query).toContain('LIMIT 10');
  });
});

describe('findPropertiesOf', () => {
  it('should generate query for resource properties', () => {
    const query = findPropertiesOf('<http://example.org/alice>');

    expect(query).toContain('SELECT ?property ?value');
    expect(query).toContain('<http://example.org/alice> ?property ?value');
  });

  it('should generate query with inverse properties', () => {
    const query = findPropertiesOf('<http://example.org/alice>', {
      includeInverse: true,
    });

    expect(query).toContain('UNION');
  });
});

describe('findByProperty', () => {
  it('should generate query to find by property', () => {
    const query = findByProperty('foaf:name', '"Alice"');

    expect(query).toContain('SELECT ?resource');
    expect(query).toContain('?resource foaf:name "Alice"');
  });

  it('should generate query with limit', () => {
    const query = findByProperty('foaf:name', '"Alice"', { limit: 5 });

    expect(query).toContain('LIMIT 5');
  });
});

describe('textSearch', () => {
  it('should generate case-insensitive text search query', () => {
    const query = textSearch('foaf:name', 'alice');

    expect(query).toContain('REGEX(?value, "alice", "i")');
  });

  it('should generate case-sensitive text search query', () => {
    const query = textSearch('foaf:name', 'alice', { caseInsensitive: false });

    expect(query).toContain('REGEX(?value, "alice")');
    expect(query).not.toContain(', "i"');
  });

  it('should generate text search query with limit', () => {
    const query = textSearch('foaf:name', 'alice', { limit: 10 });

    expect(query).toContain('LIMIT 10');
  });
});

describe('getSubclasses', () => {
  it('should generate query for all subclasses', () => {
    const query = getSubclasses('owl:Thing');

    expect(query).toContain('rdfs:subClassOf+');
    expect(query).toContain('owl:Thing');
  });

  it('should generate query with max depth', () => {
    const query = getSubclasses('owl:Thing', { maxDepth: 3 });

    expect(query).toContain('rdfs:subClassOf{1,3}');
  });
});

describe('countInstancesByType', () => {
  it('should generate count query', () => {
    const query = countInstancesByType('foaf:Person');

    expect(query).toContain('COUNT(?instance)');
    expect(query).toContain('?instance rdf:type foaf:Person');
  });
});

describe('describeResource', () => {
  it('should generate DESCRIBE query', () => {
    const query = describeResource('<http://example.org/alice>');

    expect(query).toBe('DESCRIBE <http://example.org/alice>');
  });
});

describe('findConnectedResources', () => {
  it('should generate graph traversal query', () => {
    const query = findConnectedResources('<http://example.org/alice>', {
      via: 'foaf:knows',
    });

    expect(query).toContain('foaf:knows{1,3}');
    expect(query).toContain('<http://example.org/alice>');
  });

  it('should generate query with custom max depth', () => {
    const query = findConnectedResources('<http://example.org/alice>', {
      via: 'foaf:knows',
      maxDepth: 5,
    });

    expect(query).toContain('foaf:knows{1,5}');
  });

  it('should validate options', () => {
    expect(() => findConnectedResources('<http://example.org/alice>', {})).toThrow();
    expect(() => findConnectedResources('<http://example.org/alice>', { via: '' })).toThrow();
  });
});

describe('withCommonPrefixes', () => {
  it('should generate query with common prefixes', () => {
    const query = withCommonPrefixes(b =>
      b.select('?name').where('?s foaf:name ?name')
    );

    expect(query).toContain('PREFIX rdf:');
    expect(query).toContain('PREFIX rdfs:');
    expect(query).toContain('PREFIX foaf:');
    expect(query).toContain('SELECT ?name');
  });
});

describe('aggregateQuery', () => {
  it('should generate aggregation query', () => {
    const query = aggregateQuery({
      groupBy: '?person',
      aggregations: [
        { fn: 'COUNT', var: '?friend', as: '?friendCount' },
      ],
      wherePattern: '?person foaf:knows ?friend',
    });

    expect(query).toContain('COUNT(?friend) AS ?friendCount');
    expect(query).toContain('GROUP BY ?person');
    expect(query).toContain('?person foaf:knows ?friend');
  });

  it('should generate query with multiple aggregations', () => {
    const query = aggregateQuery({
      groupBy: '?person',
      aggregations: [
        { fn: 'COUNT', var: '?friend', as: '?friendCount' },
        { fn: 'AVG', var: '?age', as: '?avgAge' },
      ],
      wherePattern: '?person foaf:knows ?friend',
    });

    expect(query).toContain('COUNT(?friend) AS ?friendCount');
    expect(query).toContain('AVG(?age) AS ?avgAge');
  });

  it('should validate aggregation options', () => {
    expect(() => aggregateQuery({})).toThrow();
    expect(() => aggregateQuery({ groupBy: '?p' })).toThrow();
  });
});

describe('Validation', () => {
  it('should validate resource parameter', () => {
    expect(() => findPropertiesOf('')).toThrow();
    expect(() => describeResource('')).toThrow();
  });

  it('should validate property parameter', () => {
    expect(() => findByProperty('', 'value')).toThrow();
    expect(() => textSearch('', 'term')).toThrow();
  });

  it('should validate type parameter', () => {
    expect(() => findInstancesOfType('')).toThrow();
    expect(() => countInstancesByType('')).toThrow();
  });
});
