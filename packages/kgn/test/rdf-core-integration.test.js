/**
 * @file RDF Core Integration Tests - @unrdf/core + @unrdf/kgn
 * @description Tests for RDF-aware template system with real @unrdf/core integration
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  RdfTemplateEngine,
  createRdfTemplateEngine,
  renderRdfTemplate,
  toTurtle,
  toSparql,
  rdfPrefix,
  blankNode,
  literal,
} from '../src/rdf/index.js';
import { namedNode, literal as createLiteral, quad } from '@unrdf/core';

describe('RDF Template Filters with @unrdf/core', () => {
  describe('toTurtle', () => {
    it('should convert RDF quads to Turtle format', () => {
      const quads = [
        {
          subject: 'http://example.org/alice',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'http://xmlns.com/foaf/0.1/Person',
        },
        {
          subject: 'http://example.org/alice',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: '"Alice"',
        },
      ];

      const turtle = toTurtle(quads);

      expect(turtle).toContain('@prefix');
      expect(turtle).toContain('rdf:');
      expect(turtle).toContain('foaf:');
      expect(turtle).toContain('.');
    });

    it('should handle custom prefixes', () => {
      const quads = [
        {
          subject: 'http://example.org/alice',
          predicate: 'http://example.org/knows',
          object: 'http://example.org/bob',
        },
      ];

      const turtle = toTurtle(quads, {
        prefixes: { ex: 'http://example.org/' },
      });

      expect(turtle).toContain('@prefix ex:');
      expect(turtle).toContain('ex:alice');
      expect(turtle).toContain('ex:knows');
      expect(turtle).toContain('ex:bob');
    });

    it('should include comments when requested', () => {
      const quads = [
        { subject: 'ex:s', predicate: 'ex:p', object: 'ex:o' },
      ];

      const turtle = toTurtle(quads, { includeComments: true });

      expect(turtle).toContain('# RDF Turtle Format');
    });

    it('should handle empty data gracefully', () => {
      expect(toTurtle(null)).toBe('');
      expect(toTurtle([])).toContain('@prefix');
    });
  });

  describe('toSparql', () => {
    it('should generate SELECT query from pattern', () => {
      const pattern = {
        subject: '?person',
        predicate: 'rdf:type',
        object: 'foaf:Person',
      };

      const sparql = toSparql(pattern);

      expect(sparql).toContain('PREFIX');
      expect(sparql).toContain('SELECT');
      expect(sparql).toContain('WHERE');
      expect(sparql).toContain('?person');
      expect(sparql).toContain('rdf:type');
      expect(sparql).toContain('foaf:Person');
    });

    it('should generate CONSTRUCT query', () => {
      const pattern = {
        subject: '?s',
        predicate: '?p',
        object: '?o',
      };

      const sparql = toSparql(pattern, { type: 'construct' });

      expect(sparql).toContain('CONSTRUCT');
      expect(sparql).toContain('WHERE');
    });

    it('should generate ASK query', () => {
      const pattern = {
        subject: 'ex:alice',
        predicate: 'rdf:type',
        object: 'foaf:Person',
      };

      const sparql = toSparql(pattern, { type: 'ask' });

      expect(sparql).toContain('ASK');
      expect(sparql).toContain('WHERE');
    });

    it('should support LIMIT clause', () => {
      const pattern = { subject: '?s', predicate: '?p', object: '?o' };
      const sparql = toSparql(pattern, { limit: 10 });

      expect(sparql).toContain('LIMIT 10');
    });

    it('should handle array of patterns', () => {
      const patterns = [
        { subject: '?person', predicate: 'rdf:type', object: 'foaf:Person' },
        { subject: '?person', predicate: 'foaf:name', object: '?name' },
      ];

      const sparql = toSparql(patterns);

      expect(sparql).toContain('?person rdf:type foaf:Person');
      expect(sparql).toContain('?person foaf:name ?name');
    });
  });

  describe('rdfPrefix', () => {
    it('should contract URI to CURIE', () => {
      const uri = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
      const curie = rdfPrefix(uri, 'rdf');

      expect(curie).toBe('rdf:type');
    });

    it('should return original URI if prefix does not match', () => {
      const uri = 'http://unknown.org/property';
      const result = rdfPrefix(uri, 'unknown');

      expect(result).toBe(uri);
    });

    it('should validate input parameters', () => {
      expect(() => rdfPrefix('not-a-url', 'test')).toThrow();
      expect(() => rdfPrefix('http://example.org/', '')).toThrow();
    });
  });

  describe('blankNode', () => {
    it('should create blank node with custom ID', () => {
      const bn = blankNode('person1');
      expect(bn).toBe('_:person1');
    });

    it('should generate blank node without ID', () => {
      const bn = blankNode();
      expect(bn).toMatch(/^_:blank\d+$/);
    });

    it('should validate blank node ID format', () => {
      expect(() => blankNode('invalid-id!')).toThrow();
      expect(() => blankNode('valid_id_123')).not.toThrow();
    });
  });

  describe('literal', () => {
    it('should create simple literal', () => {
      const lit = literal('Hello');
      expect(lit).toBe('"Hello"');
    });

    it('should create literal with language tag', () => {
      const lit = literal('Hello', 'en');
      expect(lit).toBe('"Hello"@en');
    });

    it('should create typed literal', () => {
      const lit = literal(42, null, 'xsd:integer');
      expect(lit).toBe('"42"^^xsd:integer');
    });

    it('should escape quotes in value', () => {
      const lit = literal('Say "hello"');
      expect(lit).toBe('"Say \\"hello\\""');
    });

    it('should handle null/undefined values', () => {
      expect(literal(null)).toBe('""');
      expect(literal(undefined)).toBe('""');
    });

    it('should validate language tag format', () => {
      expect(() => literal('test', 'invalid-lang')).toThrow();
      expect(() => literal('test', 'en')).not.toThrow();
      expect(() => literal('test', 'en-US')).not.toThrow();
    });
  });
});

describe('RdfTemplateEngine', () => {
  let engine;

  beforeEach(() => {
    engine = new RdfTemplateEngine({
      prefixes: {
        ex: 'http://example.org/',
      },
    });
  });

  describe('constructor', () => {
    it('should create engine with default config', () => {
      const eng = new RdfTemplateEngine();
      expect(eng).toBeInstanceOf(RdfTemplateEngine);
      expect(eng.getStore()).toBeTruthy();
    });

    it('should merge custom prefixes with common prefixes', () => {
      expect(engine.prefixes.ex).toBe('http://example.org/');
      expect(engine.prefixes.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    });

    it('should disable store when configured', () => {
      const eng = new RdfTemplateEngine({ enableStore: false });
      expect(eng.getStore()).toBeNull();
    });
  });

  describe('render', () => {
    it('should render template with RDF filters', () => {
      const template = '{{ uri | contractUri }}';
      const result = engine.render(template, {
        uri: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      });

      expect(result).toBe('rdf:type');
    });

    it('should render template with toTurtle filter', () => {
      const template = `{{ quads | toTurtle }}`;
      const quads = [
        { subject: 'ex:alice', predicate: 'rdf:type', object: 'foaf:Person' },
      ];

      const result = engine.render(template, { quads });

      expect(result).toContain('@prefix');
      expect(result).toContain('ex:alice');
    });

    it('should render template with toSparql filter', () => {
      const template = `{{ pattern | toSparql }}`;
      const pattern = {
        subject: '?s',
        predicate: 'rdf:type',
        object: '?type',
      };

      const result = engine.render(template, { pattern });

      expect(result).toContain('SELECT');
      expect(result).toContain('WHERE');
    });

    it('should use global RDF functions', () => {
      const template = `{{ namedNode("http://example.org/alice").value }}`;
      const result = engine.render(template);

      expect(result).toBe('http://example.org/alice');
    });

    it('should access prefixes in context', () => {
      const template = `{{ prefixes.rdf }}`;
      const result = engine.render(template);

      expect(result).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    });
  });

  describe('SPARQL query builder', () => {
    it('should build SPARQL query from specification', () => {
      const template = `{{ sparqlQuery(spec) }}`;
      const spec = {
        where: [
          { subject: '?person', predicate: 'rdf:type', object: 'foaf:Person' },
          { subject: '?person', predicate: 'foaf:name', object: '?name' },
        ],
        select: ['?person', '?name'],
        limit: 10,
      };

      const result = engine.render(template, { spec });

      expect(result).toContain('SELECT ?person ?name');
      expect(result).toContain('WHERE');
      expect(result).toContain('?person rdf:type foaf:Person');
      expect(result).toContain('LIMIT 10');
    });

    it('should support DISTINCT modifier', () => {
      const template = `{{ sparqlQuery(spec) }}`;
      const spec = {
        where: [{ subject: '?s', predicate: '?p', object: '?o' }],
        distinct: true,
      };

      const result = engine.render(template, { spec });

      expect(result).toContain('SELECT DISTINCT');
    });

    it('should support ORDER BY clause', () => {
      const template = `{{ sparqlQuery(spec) }}`;
      const spec = {
        where: [{ subject: '?s', predicate: '?p', object: '?o' }],
        orderBy: [
          { variable: '?s', direction: 'ASC' },
          { variable: '?o', direction: 'DESC' },
        ],
      };

      const result = engine.render(template, { spec });

      expect(result).toContain('ORDER BY');
      expect(result).toContain('ASC(?s)');
      expect(result).toContain('DESC(?o)');
    });
  });

  describe('addPrefix', () => {
    it('should add custom prefix', () => {
      engine.addPrefix('custom', 'http://custom.org/');
      expect(engine.prefixes.custom).toBe('http://custom.org/');
    });

    it('should validate prefix parameters', () => {
      expect(() => engine.addPrefix('', 'http://example.org/')).toThrow();
      expect(() => engine.addPrefix('test', 'not-a-url')).toThrow();
    });
  });
});

describe('Integration: Complete RDF Template Workflow', () => {
  it('should generate Turtle from template', () => {
    const template = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

{% for person in people %}
ex:{{ person.id }} rdf:type foaf:Person .
ex:{{ person.id }} foaf:name {{ person.name | literal('en') }} .
{% if person.email %}
ex:{{ person.id }} foaf:mbox <mailto:{{ person.email }}> .
{% endif %}
{% endfor %}
    `.trim();

    const people = [
      { id: 'alice', name: 'Alice Smith', email: 'alice@example.org' },
      { id: 'bob', name: 'Bob Jones' },
    ];

    const result = renderRdfTemplate(template, { people });

    expect(result).toContain('ex:alice rdf:type foaf:Person');
    expect(result).toContain('ex:alice foaf:name "Alice Smith"@en');
    expect(result).toContain('ex:alice foaf:mbox <mailto:alice@example.org>');
    expect(result).toContain('ex:bob rdf:type foaf:Person');
    expect(result).toContain('ex:bob foaf:name "Bob Jones"@en');
    expect(result).not.toContain('ex:bob foaf:mbox');
  });

  it('should generate SPARQL query from template', () => {
    const template = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?person ?name ?email
WHERE {
  ?person rdf:type foaf:Person .
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
  {% if filter %}
  FILTER (CONTAINS(?name, "{{ filter }}"))
  {% endif %}
}
{% if limit %}
LIMIT {{ limit }}
{% endif %}
    `.trim();

    const result = renderRdfTemplate(template, {
      filter: 'Alice',
      limit: 10,
    });

    expect(result).toContain('SELECT ?person ?name ?email');
    expect(result).toContain('WHERE {');
    expect(result).toContain('OPTIONAL { ?person foaf:mbox ?email }');
    expect(result).toContain('FILTER (CONTAINS(?name, "Alice"))');
    expect(result).toContain('LIMIT 10');
  });

  it('should use toSparql filter for query generation', () => {
    const template = `
{{ patterns | toSparql({ type: 'select', limit: 100 }) }}
    `.trim();

    const patterns = [
      { subject: '?book', predicate: 'rdf:type', object: 'ex:Book' },
      { subject: '?book', predicate: 'ex:title', object: '?title' },
      { subject: '?book', predicate: 'ex:author', object: '?author' },
    ];

    const result = renderRdfTemplate(template, { patterns });

    expect(result).toContain('SELECT');
    expect(result).toContain('?book rdf:type ex:Book');
    expect(result).toContain('?book ex:title ?title');
    expect(result).toContain('?book ex:author ?author');
    expect(result).toContain('LIMIT 100');
  });

  it('should generate schema-driven RDF from data model', () => {
    const template = `
{{ schema | toTurtle }}
    `.trim();

    const schema = [
      {
        subject: namedNode('http://example.org/Person'),
        predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        object: namedNode('http://www.w3.org/2000/01/rdf-schema#Class'),
      },
      {
        subject: namedNode('http://example.org/name'),
        predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        object: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'),
      },
    ];

    const result = renderRdfTemplate(template, { schema });

    expect(result).toContain('@prefix');
    expect(result).toContain('ex:Person');
    expect(result).toContain('rdfs:Class');
  });
});

describe('Factory Functions', () => {
  it('should create engine via factory', () => {
    const engine = createRdfTemplateEngine({
      prefixes: { test: 'http://test.org/' },
    });

    expect(engine).toBeInstanceOf(RdfTemplateEngine);
    expect(engine.prefixes.test).toBe('http://test.org/');
  });

  it('should render via convenience function', () => {
    const result = renderRdfTemplate(
      '{{ "http://example.org/test" | contractUri }}',
      {},
      { prefixes: { ex: 'http://example.org/' } }
    );

    expect(result).toBe('ex:test');
  });
});
