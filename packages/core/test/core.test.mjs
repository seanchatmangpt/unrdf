/**
 * @unrdf/core Test Suite
 *
 * Comprehensive tests for RDF operations and SPARQL execution
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createStore,
  addQuad,
  removeQuad,
  getQuads,
  iterateQuads,
  countQuads,
  namedNode,
  literal,
  defaultGraph,
  canonicalize,
  toNTriples,
  sortQuads,
  isIsomorphic,
  executeQuery,
  executeSelect,
  executeConstruct,
  executeAsk,
  prepareQuery,
  createTerms,
  createLiteral,
  RDF,
  RDFS,
  FOAF,
  XSD,
  validateQuad,
  validateStore,
} from '../src/index.mjs';

describe('@unrdf/core', () => {
  describe('Store Operations', () => {
    let store;

    beforeEach(() => {
      store = createStore();
    });

    it('should create a new store', () => {
      expect(store).toBeDefined();
      expect(typeof store.getQuads).toBe('function');
      expect(typeof store.addQuad).toBe('function');
    });

    it('should add quads to store', () => {
      const quad = {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      };

      addQuad(store, quad);

      const quads = getQuads(store);
      expect(quads.length).toBe(1);
      expect(quads[0].subject.value).toBe('http://example.org/alice');
      expect(quads[0].object.value).toBe('Alice');
    });

    it('should retrieve quads from store', () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');

      addQuad(store, {
        subject: alice,
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      });

      addQuad(store, {
        subject: bob,
        predicate: FOAF.name,
        object: literal('Bob'),
        graph: defaultGraph(),
      });

      // Get all quads
      const allQuads = getQuads(store);
      expect(allQuads.length).toBe(2);

      // Get quads by subject
      const aliceQuads = getQuads(store, alice);
      expect(aliceQuads.length).toBe(1);
      expect(aliceQuads[0].object.value).toBe('Alice');

      // Get quads by predicate
      const nameQuads = getQuads(store, null, FOAF.name);
      expect(nameQuads.length).toBe(2);
    });

    it('should remove quads from store', () => {
      const quad = {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      };

      addQuad(store, quad);
      expect(countQuads(store)).toBe(1);

      removeQuad(store, quad);
      expect(countQuads(store)).toBe(0);
    });

    it('should iterate quads', () => {
      addQuad(store, {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      });

      addQuad(store, {
        subject: namedNode('http://example.org/bob'),
        predicate: FOAF.name,
        object: literal('Bob'),
        graph: defaultGraph(),
      });

      const quads = [];
      for (const quad of iterateQuads(store)) {
        quads.push(quad);
      }

      expect(quads.length).toBe(2);
    });

    it('should count quads', () => {
      expect(countQuads(store)).toBe(0);

      addQuad(store, {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      });

      expect(countQuads(store)).toBe(1);

      addQuad(store, {
        subject: namedNode('http://example.org/bob'),
        predicate: FOAF.name,
        object: literal('Bob'),
        graph: defaultGraph(),
      });

      expect(countQuads(store)).toBe(2);
    });

    it('should throw on invalid store', () => {
      expect(() => addQuad(null, {})).toThrow(TypeError);
      expect(() => getQuads({})).toThrow(TypeError);
    });
  });

  describe('Canonicalization', () => {
    let store;

    beforeEach(() => {
      store = createStore();
    });

    it('should canonicalize an empty store', async () => {
      const canonical = await canonicalize(store);
      expect(canonical).toBe('');
    });

    it('should canonicalize RDF data', async () => {
      addQuad(store, {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      });

      const canonical = await canonicalize(store);
      expect(canonical).toBeTruthy();
      expect(typeof canonical).toBe('string');
      expect(canonical.length).toBeGreaterThan(0);
    });

    it('should convert to N-Triples', async () => {
      const quads = [
        {
          subject: namedNode('http://example.org/alice'),
          predicate: FOAF.name,
          object: literal('Alice'),
          graph: defaultGraph(),
        },
      ];

      const ntriples = await toNTriples(quads);
      expect(ntriples).toBeTruthy();
      expect(ntriples).toContain('http://example.org/alice');
      expect(ntriples).toContain('Alice');
    });

    it('should sort quads', () => {
      const quad1 = {
        subject: namedNode('http://example.org/bob'),
        predicate: FOAF.name,
        object: literal('Bob'),
        graph: defaultGraph(),
      };

      const quad2 = {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      };

      const sorted = sortQuads([quad1, quad2]);
      expect(sorted[0].subject.value).toBe('http://example.org/alice');
      expect(sorted[1].subject.value).toBe('http://example.org/bob');
    });

    it('should check isomorphism', async () => {
      const store1 = createStore();
      const store2 = createStore();

      const quad = {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      };

      addQuad(store1, quad);
      addQuad(store2, quad);

      const result = await isIsomorphic(store1, store2);
      expect(result).toBe(true);
    });
  });

  describe('SPARQL Execution', () => {
    let store;

    beforeEach(() => {
      store = createStore();

      // Add test data
      addQuad(store, {
        subject: namedNode('http://example.org/alice'),
        predicate: RDF.type,
        object: FOAF.Person,
        graph: defaultGraph(),
      });

      addQuad(store, {
        subject: namedNode('http://example.org/alice'),
        predicate: FOAF.name,
        object: literal('Alice'),
        graph: defaultGraph(),
      });

      addQuad(store, {
        subject: namedNode('http://example.org/bob'),
        predicate: RDF.type,
        object: FOAF.Person,
        graph: defaultGraph(),
      });

      addQuad(store, {
        subject: namedNode('http://example.org/bob'),
        predicate: FOAF.name,
        object: literal('Bob'),
        graph: defaultGraph(),
      });
    });

    it('should execute a SPARQL SELECT query', async () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE {
          ?s foaf:name ?name
        }
      `;

      const results = await executeSelect(store, query);
      expect(results).toBeDefined();
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });

    it('should execute a SPARQL ASK query', async () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        ASK {
          ?s foaf:name "Alice"
        }
      `;

      const result = await executeAsk(store, query);
      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('should execute a SPARQL CONSTRUCT query', async () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        CONSTRUCT {
          ?s foaf:name ?name
        }
        WHERE {
          ?s foaf:name ?name
        }
      `;

      const quads = await executeConstruct(store, query);
      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThan(0);
    });

    it('should prepare a query', async () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE { ?s foaf:name ?name }
      `;

      const metadata = await prepareQuery(query);
      expect(metadata.type).toBe('select');
      expect(metadata.variables).toContain('name');
      expect(metadata.variables).toContain('s');
      expect(metadata.prefixes.foaf).toBe('http://xmlns.com/foaf/0.1/');
    });

    it('should throw on invalid SPARQL', async () => {
      await expect(executeQuery(store, null)).rejects.toThrow(TypeError);
      await expect(executeQuery(null, 'SELECT * WHERE {}')).rejects.toThrow(TypeError);
    });
  });

  describe('Type Creation', () => {
    it('should create terms from data', () => {
      const terms = createTerms({
        uri: 'http://example.org/alice',
        name: 'Alice',
        age: 30,
        active: true,
      });

      expect(terms.uri.termType).toBe('NamedNode');
      expect(terms.uri.value).toBe('http://example.org/alice');
      expect(terms.name.termType).toBe('Literal');
      expect(terms.name.value).toBe('Alice');
      expect(terms.age.value).toBe('30');
      expect(terms.active.value).toBe('true');
    });

    it('should create literals with datatypes', () => {
      const age = createLiteral('30', XSD.integer);
      expect(age.termType).toBe('Literal');
      expect(age.value).toBe('30');
      expect(age.datatype.value).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });

    it('should create literals with language tags', () => {
      const label = createLiteral('Nom', null, 'fr');
      expect(label.termType).toBe('Literal');
      expect(label.value).toBe('Nom');
      expect(label.language).toBe('fr');
    });
  });

  describe('Validation', () => {
    it('should validate quads', () => {
      const validQuad = {
        subject: { value: 'http://example.org/alice' },
        predicate: { value: 'http://xmlns.com/foaf/0.1/name' },
        object: { value: 'Alice' },
      };

      expect(validateQuad(validQuad)).toBe(true);
      expect(validateQuad({})).toBe(false);
      expect(validateQuad(null)).toBe(false);
    });

    it('should validate stores', () => {
      const store = createStore();
      expect(validateStore(store)).toBe(true);
      expect(validateStore({})).toBe(false);
      expect(validateStore(null)).toBe(false);
    });
  });

  describe('Constants', () => {
    it('should provide RDF namespace', () => {
      expect(RDF.type).toBeDefined();
      expect(RDF.type.value).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    });

    it('should provide RDFS namespace', () => {
      expect(RDFS.label).toBeDefined();
      expect(RDFS.label.value).toBe('http://www.w3.org/2000/01/rdf-schema#label');
    });

    it('should provide FOAF namespace', () => {
      expect(FOAF.Person).toBeDefined();
      expect(FOAF.Person.value).toBe('http://xmlns.com/foaf/0.1/Person');
    });

    it('should provide XSD namespace', () => {
      expect(XSD.integer).toBeDefined();
      expect(XSD.integer.value).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });
  });
});
