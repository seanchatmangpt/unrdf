/**
 * Grammar Smoke Test Suite
 *
 * Tests grammar subsystems for stability:
 * 1. SPARQL - Query compilation & parsing
 * 2. Turtle - RDF parsing via N3 streaming
 * 3. SHACL - Shape template generation
 * 4. Quad Operations - Foundational RDF operations
 * 5. Term Creation - RDF term factory
 *
 * Target: <10ms per test, 100% deterministic
 *
 * @module @unrdf/fusion/test/grammar-smoke
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { streamingParse, streamingWrite } from '@unrdf/core/rdf/n3-justified-only.mjs';
import { KGenSHACLTemplates } from '../../../kgn/src/base/shacl-templates.js';

describe('Grammar Smoke Suite', () => {
  describe('1. SPARQL - Query Compilation', () => {
    it('should parse simple SELECT query', async () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );
      store.add(quad);

      const query = 'SELECT ?s WHERE { ?s ?p ?o }';
      const results = store.query(query);

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });

    it('should parse CONSTRUCT query', async () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );
      store.add(quad);

      const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
      const results = store.query(query);

      expect(Array.isArray(results)).toBe(true);
    });

    it('should parse ASK query', async () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );
      store.add(quad);

      const query = 'ASK { ?s ?p ?o }';
      const result = store.query(query);

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('should handle FILTER clause', async () => {
      const store = createStore();
      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/s1'),
          dataFactory.namedNode('http://example.org/age'),
          dataFactory.literal('25', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );

      const query = `
        SELECT ?s WHERE {
          ?s <http://example.org/age> ?age .
          FILTER(?age > 20)
        }
      `;
      const results = store.query(query);

      expect(Array.isArray(results)).toBe(true);
    });

    it('should parse query with PREFIX', async () => {
      const store = createStore();
      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person'),
          dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          dataFactory.namedNode('http://www.w3.org/2000/01/rdf-schema#Class')
        )
      );

      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?s WHERE { ?s rdf:type ?o }
      `;
      const results = store.query(query);

      expect(Array.isArray(results)).toBe(true);
    });
  });

  describe('2. Turtle - RDF Parsing', () => {
    it('should parse simple Turtle triple', async () => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate "value" .
      `;

      const quads = await streamingParse(turtle, { format: 'text/turtle' });

      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBe(1);
      expect(quads[0].object.value).toBe('value');
    });

    it('should serialize quads to Turtle', async () => {
      const quads = [
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/s'),
          dataFactory.namedNode('http://example.org/p'),
          dataFactory.literal('value')
        ),
      ];

      const turtle = await streamingWrite(quads, { format: 'text/turtle' });

      expect(typeof turtle).toBe('string');
      expect(turtle).toContain('http://example.org/s');
      expect(turtle).toContain('"value"');
    });

    it('should parse Turtle with prefixes', async () => {
      const turtle = `
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix ex: <http://example.org/> .

        ex:resource rdf:type ex:Thing .
      `;

      const quads = await streamingParse(turtle, { format: 'text/turtle' });

      expect(quads.length).toBe(1);
      expect(quads[0].predicate.value).toContain('type');
    });

    it('should parse N-Triples format', async () => {
      const ntriples = '<http://example.org/s> <http://example.org/p> "value" .';

      const quads = await streamingParse(ntriples, { format: 'application/n-triples' });

      expect(quads.length).toBe(1);
      expect(quads[0].object.value).toBe('value');
    });

    it('should handle typed literals in Turtle', async () => {
      const turtle = `
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        @prefix ex: <http://example.org/> .

        ex:resource ex:age "25"^^xsd:integer .
      `;

      const quads = await streamingParse(turtle, { format: 'text/turtle' });

      expect(quads.length).toBe(1);
      expect(quads[0].object.value).toBe('25');
      expect(quads[0].object.datatype?.value).toContain('integer');
    });
  });

  describe('3. SHACL - Shape Template Generation', () => {
    it('should generate basic node shape', () => {
      const templates = new KGenSHACLTemplates({
        deterministicMode: true,
        staticBuildTime: '2024-01-01T00:00:00.000Z',
      });

      const shape = templates.generateShape('basic_node_shape', {
        shapeName: 'Person',
        targetClass: 'Person',
        properties: [
          {
            path: 'name',
            datatype: 'xsd:string',
            minCount: 1,
          },
        ],
      });

      expect(shape.name).toBe('PersonShape');
      expect(shape.content).toContain('sh:NodeShape');
      expect(shape.content).toContain('sh:path ex:name');
    });

    it('should list available SHACL templates', () => {
      const templates = new KGenSHACLTemplates();
      const names = templates.getTemplateNames();

      expect(Array.isArray(names)).toBe(true);
      expect(names).toContain('basic_node_shape');
      expect(names).toContain('property_shape');
      expect(names).toContain('template_shape');
    });

    it('should generate property shape', () => {
      const templates = new KGenSHACLTemplates({
        deterministicMode: true,
        staticBuildTime: '2024-01-01T00:00:00.000Z',
      });

      const shape = templates.generateShape('property_shape', {
        shapeName: 'Email',
        propertyPath: 'email',
        datatype: 'xsd:string',
        pattern: '^[\\w.-]+@[\\w.-]+\\.\\w+$',
      });

      expect(shape.content).toContain('sh:PropertyShape');
      expect(shape.content).toContain('sh:pattern');
    });

    it('should generate validation file with multiple shapes', () => {
      const templates = new KGenSHACLTemplates({
        deterministicMode: true,
        staticBuildTime: '2024-01-01T00:00:00.000Z',
      });

      const file = templates.generateValidationFile([
        {
          template: 'basic_node_shape',
          context: { shapeName: 'Person', targetClass: 'Person' },
        },
        {
          template: 'property_shape',
          context: { shapeName: 'Email', propertyPath: 'email' },
        },
      ]);

      expect(file.content).toContain('@prefix sh:');
      expect(file.metadata.shapeCount).toBe(2);
    });

    it('should get template statistics', () => {
      const templates = new KGenSHACLTemplates();
      const stats = templates.getStats();

      expect(stats.totalTemplates).toBeGreaterThan(0);
      expect(Array.isArray(stats.templates)).toBe(true);
    });
  });

  describe('4. Quad Operations - Foundational RDF', () => {
    it('should create and add quad to store', () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );

      store.add(quad);

      expect(store.size).toBe(1);
      expect(store.has(quad)).toBe(true);
    });

    it('should match quads by pattern', () => {
      const store = createStore();
      const quad1 = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s1'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value1')
      );
      const quad2 = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s2'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value2')
      );

      store.add(quad1);
      store.add(quad2);

      const matches = store.match(
        null,
        dataFactory.namedNode('http://example.org/p'),
        null
      );

      expect(matches.length).toBe(2);
    });

    it('should delete quad from store', () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );

      store.add(quad);
      expect(store.size).toBe(1);

      store.delete(quad);
      expect(store.size).toBe(0);
    });

    it('should handle blank nodes', () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.blankNode('b1'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value')
      );

      store.add(quad);

      expect(store.size).toBe(1);
      expect(quad.subject.termType).toBe('BlankNode');
    });

    it('should handle named graphs', () => {
      const store = createStore();
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('value'),
        dataFactory.namedNode('http://example.org/graph1')
      );

      store.add(quad);

      const matches = store.match(
        null,
        null,
        null,
        dataFactory.namedNode('http://example.org/graph1')
      );

      expect(matches.length).toBe(1);
    });
  });

  describe('5. Term Creation - RDF Factory', () => {
    it('should create named node', () => {
      const node = dataFactory.namedNode('http://example.org/resource');

      expect(node.termType).toBe('NamedNode');
      expect(node.value).toBe('http://example.org/resource');
    });

    it('should create literal', () => {
      const literal = dataFactory.literal('test value');

      expect(literal.termType).toBe('Literal');
      expect(literal.value).toBe('test value');
    });

    it('should create typed literal', () => {
      const literal = dataFactory.literal(
        '42',
        dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
      );

      expect(literal.termType).toBe('Literal');
      expect(literal.value).toBe('42');
      expect(literal.datatype.value).toContain('integer');
    });

    it('should create blank node', () => {
      const blank = dataFactory.blankNode('b1');

      expect(blank.termType).toBe('BlankNode');
      expect(blank.value).toBe('b1');
    });

    it('should create default graph', () => {
      const graph = dataFactory.defaultGraph();

      expect(graph.termType).toBe('DefaultGraph');
    });
  });
});
