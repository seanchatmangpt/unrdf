/**
 * @file Tests for SPARQL Query Builder
 * @module @unrdf/core/test/sparql/query-builder
 */

import { describe, it, expect } from 'vitest';
import { sparql, QueryBuilder } from '../../src/sparql/query-builder.mjs';

describe('QueryBuilder', () => {
  describe('SELECT queries', () => {
    it('should build basic SELECT query', () => {
      const query = sparql()
        .select('?name')
        .where('?person foaf:name ?name')
        .build();

      expect(query).toContain('SELECT ?name');
      expect(query).toContain('WHERE {');
      expect(query).toContain('?person foaf:name ?name');
    });

    it('should build SELECT query with multiple variables', () => {
      const query = sparql()
        .select('?name', '?email')
        .where('?person foaf:name ?name')
        .where('?person foaf:mbox ?email')
        .build();

      expect(query).toContain('SELECT ?name ?email');
      expect(query).toContain('?person foaf:name ?name');
      expect(query).toContain('?person foaf:mbox ?email');
    });

    it('should build SELECT DISTINCT query', () => {
      const query = sparql()
        .select('?name')
        .distinct()
        .where('?person foaf:name ?name')
        .build();

      expect(query).toContain('SELECT DISTINCT ?name');
    });

    it('should build SELECT query with FILTER', () => {
      const query = sparql()
        .select('?name', '?email')
        .where('?person foaf:name ?name')
        .where('?person foaf:mbox ?email')
        .filter('REGEX(?email, "@example.com")')
        .build();

      expect(query).toContain('FILTER(REGEX(?email, "@example.com"))');
    });

    it('should build SELECT query with LIMIT', () => {
      const query = sparql()
        .select('?name')
        .where('?person foaf:name ?name')
        .limit(10)
        .build();

      expect(query).toContain('LIMIT 10');
    });

    it('should build SELECT query with OFFSET', () => {
      const query = sparql()
        .select('?name')
        .where('?person foaf:name ?name')
        .offset(20)
        .build();

      expect(query).toContain('OFFSET 20');
    });

    it('should build SELECT query with ORDER BY', () => {
      const query = sparql()
        .select('?name', '?age')
        .where('?person foaf:name ?name')
        .where('?person foaf:age ?age')
        .orderBy('?name', '-?age')
        .build();

      expect(query).toContain('ORDER BY ?name DESC(?age)');
    });

    it('should build SELECT query with GROUP BY', () => {
      const query = sparql()
        .select('?person', '(COUNT(?friend) AS ?count)')
        .where('?person foaf:knows ?friend')
        .groupBy('?person')
        .build();

      expect(query).toContain('GROUP BY ?person');
    });

    it('should build SELECT query with HAVING', () => {
      const query = sparql()
        .select('?person', '(COUNT(?friend) AS ?count)')
        .where('?person foaf:knows ?friend')
        .groupBy('?person')
        .having('COUNT(?friend) > 5')
        .build();

      expect(query).toContain('HAVING (COUNT(?friend) > 5)');
    });
  });

  describe('Prefixes', () => {
    it('should add prefixes to query', () => {
      const query = sparql()
        .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
        .prefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
        .select('?name')
        .where('?person foaf:name ?name')
        .build();

      expect(query).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
      expect(query).toContain('PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>');
    });

    it('should add BASE IRI to query', () => {
      const query = sparql()
        .base('http://example.org/')
        .select('?name')
        .where('?person foaf:name ?name')
        .build();

      expect(query).toContain('BASE <http://example.org/>');
    });

    it('should initialize with prefixes from options', () => {
      const builder = new QueryBuilder({
        prefixes: {
          foaf: 'http://xmlns.com/foaf/0.1/',
        },
      });

      const query = builder
        .select('?name')
        .where('?person foaf:name ?name')
        .build();

      expect(query).toContain('PREFIX foaf: <http://xmlns.com/foaf/0.1/>');
    });
  });

  describe('OPTIONAL patterns', () => {
    it('should build query with OPTIONAL clause', () => {
      const query = sparql()
        .select('?name', '?email')
        .where('?person foaf:name ?name')
        .optional('?person foaf:mbox ?email')
        .build();

      expect(query).toContain('OPTIONAL {');
      expect(query).toContain('?person foaf:mbox ?email');
    });

    it('should build query with OPTIONAL using function', () => {
      const query = sparql()
        .select('?name', '?email')
        .where('?person foaf:name ?name')
        .optional(b => b.where('?person foaf:mbox ?email'))
        .build();

      expect(query).toContain('OPTIONAL {');
      expect(query).toContain('?person foaf:mbox ?email');
    });
  });

  describe('UNION patterns', () => {
    it('should build query with UNION clause', () => {
      const query = sparql()
        .select('?name')
        .union(
          b => b.where('?person foaf:name ?name'),
          b => b.where('?person rdfs:label ?name')
        )
        .build();

      expect(query).toContain('UNION');
      expect(query).toContain('?person foaf:name ?name');
      expect(query).toContain('?person rdfs:label ?name');
    });
  });

  describe('Named graphs', () => {
    it('should build query with GRAPH clause', () => {
      const query = sparql()
        .select('?name')
        .graph('http://example.org/graph1', b => {
          b.where('?person foaf:name ?name');
        })
        .build();

      expect(query).toContain('GRAPH http://example.org/graph1');
      expect(query).toContain('?person foaf:name ?name');
    });
  });

  describe('CONSTRUCT queries', () => {
    it('should build CONSTRUCT query', () => {
      const query = sparql()
        .construct('?s foaf:name ?name')
        .where('?s foaf:name ?name')
        .build();

      expect(query).toContain('CONSTRUCT {');
      expect(query).toContain('?s foaf:name ?name');
      expect(query).toContain('WHERE {');
    });
  });

  describe('ASK queries', () => {
    it('should build ASK query', () => {
      const query = sparql()
        .ask()
        .where('?s foaf:name "Alice"')
        .build();

      expect(query).toContain('ASK');
      expect(query).toContain('WHERE {');
      expect(query).toContain('?s foaf:name "Alice"');
    });
  });

  describe('INSERT queries', () => {
    it('should build INSERT DATA query', () => {
      const query = sparql()
        .insert('<http://example.org/alice> foaf:name "Alice"')
        .build();

      expect(query).toContain('INSERT DATA {');
      expect(query).toContain('<http://example.org/alice> foaf:name "Alice"');
    });
  });

  describe('DELETE queries', () => {
    it('should build DELETE DATA query', () => {
      const query = sparql()
        .delete('<http://example.org/alice> foaf:name "Alice"')
        .build();

      expect(query).toContain('DELETE DATA {');
      expect(query).toContain('<http://example.org/alice> foaf:name "Alice"');
    });
  });

  describe('buildWithMetadata', () => {
    it('should return query with metadata', () => {
      const result = sparql()
        .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
        .select('?name', '?email')
        .where('?person foaf:name ?name')
        .optional('?person foaf:mbox ?email')
        .filter('REGEX(?email, "@example.com")')
        .buildWithMetadata();

      expect(result.query).toBeDefined();
      expect(result.metadata).toBeDefined();
      expect(result.metadata.type).toBe('SELECT');
      expect(result.metadata.variables).toContain('?name');
      expect(result.metadata.variables).toContain('?email');
      expect(result.metadata.hasFilters).toBe(true);
      expect(result.metadata.hasOptionals).toBe(true);
      expect(result.metadata.prefixes.foaf).toBe('http://xmlns.com/foaf/0.1/');
    });
  });

  describe('Validation', () => {
    it('should throw error when building without query type', () => {
      const builder = new QueryBuilder();
      expect(() => builder.build()).toThrow('Query type must be set');
    });

    it('should validate LIMIT is positive integer', () => {
      const builder = sparql().select('?s');
      expect(() => builder.limit(-1)).toThrow();
      expect(() => builder.limit(0)).toThrow();
      expect(() => builder.limit(1.5)).toThrow();
    });

    it('should validate OFFSET is non-negative integer', () => {
      const builder = sparql().select('?s');
      expect(() => builder.offset(-1)).toThrow();
      expect(() => builder.offset(1.5)).toThrow();
      builder.offset(0);
    });

    it('should validate prefix URI is valid URL', () => {
      const builder = sparql();
      expect(() => builder.prefix('test', 'not-a-url')).toThrow();
    });

    it('should validate filter expression is not empty', () => {
      const builder = sparql().select('?s');
      expect(() => builder.filter('')).toThrow();
    });
  });

  describe('Complex queries', () => {
    it('should build complex query with all features', () => {
      const query = sparql()
        .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
        .prefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#')
        .select('?person', '?name', '?email')
        .distinct()
        .where('?person rdf:type foaf:Person')
        .where('?person foaf:name ?name')
        .optional('?person foaf:mbox ?email')
        .filter('?name != ""')
        .orderBy('?name')
        .limit(100)
        .offset(0)
        .build();

      expect(query).toContain('PREFIX foaf:');
      expect(query).toContain('PREFIX rdf:');
      expect(query).toContain('SELECT DISTINCT');
      expect(query).toContain('?person rdf:type foaf:Person');
      expect(query).toContain('OPTIONAL {');
      expect(query).toContain('FILTER');
      expect(query).toContain('ORDER BY');
      expect(query).toContain('LIMIT 100');
      expect(query).toContain('OFFSET 0');
    });
  });

  describe('Triple pattern objects', () => {
    it('should accept triple pattern as object', () => {
      const query = sparql()
        .select('?name')
        .where({ subject: '?person', predicate: 'foaf:name', object: '?name' })
        .build();

      expect(query).toContain('?person foaf:name ?name');
    });

    it('should validate triple pattern object', () => {
      const builder = sparql().select('?s');
      expect(() => builder.where({ subject: '', predicate: 'p', object: 'o' })).toThrow();
      expect(() => builder.where({ subject: 's', predicate: '', object: 'o' })).toThrow();
      expect(() => builder.where({ subject: 's', predicate: 'p', object: '' })).toThrow();
    });
  });
});
