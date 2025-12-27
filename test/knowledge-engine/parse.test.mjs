/**
 * @fileoverview Unit tests for knowledge-engine/parse.mjs
 * Tests Turtle, N-Quads, and JSON-LD parsing and serialization
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';
import {
  parseTurtle,
  toTurtle,
  toNQuads,
  parseJsonLd,
  toJsonLd,
} from '../../src/knowledge-engine/parse.mjs';

const { namedNode, literal, _quad } = DataFactory;

describe('parse.mjs', () => {
  let testStore;

  beforeEach(() => {
    testStore = new Store();
    testStore.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/name'),
      literal('Alice')
    );
    testStore.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/age'),
      literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    );
    testStore.addQuad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/knows'),
      namedNode('http://example.org/bob')
    );
  });

  describe('parseTurtle', () => {
    it('should parse valid Turtle string', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" ;
                 ex:age 30 ;
                 ex:knows ex:bob .
      `;

      const store = await parseTurtle(ttl);
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(3);
    });

    it('should parse Turtle with base IRI', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" ;
                 ex:age 30 .
      `;

      const store = await parseTurtle(ttl, 'http://example.org/');
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(2);
    });

    it('should handle empty Turtle string', async () => {
      const store = await parseTurtle('');
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(0);
    });

    it('should handle Turtle with prefixes', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        ex:alice a foaf:Person ;
                 foaf:name "Alice" ;
                 foaf:age "30"^^xsd:integer .
      `;

      const store = await parseTurtle(ttl);
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(3);
    });

    it('should handle Turtle with blank nodes', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        
        ex:alice ex:address _:addr1 .
        _:addr1 ex:street "123 Main St" ;
                ex:city "Anytown" .
      `;

      const store = await parseTurtle(ttl);
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(3);
    });

    it('should throw error for invalid Turtle', async () => {
      const invalidTtl = 'invalid turtle syntax {';

      await expect(parseTurtle(invalidTtl)).rejects.toThrow('Failed to parse Turtle');
    });

    it('should throw error for non-string input', async () => {
      await expect(parseTurtle(null)).rejects.toThrow('parseTurtle: ttl must be a string');
      await expect(parseTurtle(123)).rejects.toThrow('parseTurtle: ttl must be a string');
    });

    it('should throw error for non-string baseIRI', async () => {
      const ttl = 'ex:alice ex:name "Alice" .';

      await expect(parseTurtle(ttl, null)).rejects.toThrow('parseTurtle: baseIRI must be a string');
      await expect(parseTurtle(ttl, 123)).rejects.toThrow('parseTurtle: baseIRI must be a string');
    });
  });

  describe('toTurtle', () => {
    it('should serialize store to Turtle', async () => {
      const ttl = await toTurtle(testStore);
      expect(typeof ttl).toBe('string');
      expect(ttl.length).toBeGreaterThan(0);
      expect(ttl).toContain('Alice');
      expect(ttl).toContain('30');
    });

    it('should serialize with custom prefixes', async () => {
      const prefixes = {
        ex: 'http://example.org/',
        foaf: 'http://xmlns.com/foaf/0.1/',
      };

      const ttl = await toTurtle(testStore, { prefixes });
      expect(ttl).toContain('@prefix ex:');
      expect(ttl).toContain('@prefix foaf:');
    });

    it('should serialize with base IRI', async () => {
      const ttl = await toTurtle(testStore, { baseIRI: 'http://example.org/' });
      expect(ttl).toContain('@base <http://example.org/>');
    });

    it('should handle empty store', async () => {
      const emptyStore = new Store();
      const ttl = await toTurtle(emptyStore);
      expect(ttl).toBe('');
    });

    it('should throw error for invalid store', async () => {
      await expect(toTurtle(null)).rejects.toThrow(
        'toTurtle: store must be a valid Store instance'
      );
      await expect(toTurtle('invalid')).rejects.toThrow(
        'toTurtle: store must be a valid Store instance'
      );
    });
  });

  describe('toNQuads', () => {
    it('should serialize store to N-Quads', async () => {
      const nquads = await toNQuads(testStore);
      expect(typeof nquads).toBe('string');
      expect(nquads.length).toBeGreaterThan(0);
      expect(nquads).toContain('<http://example.org/alice>');
      expect(nquads).toContain('Alice');
    });

    it('should serialize with custom options', async () => {
      const nquads = await toNQuads(testStore, {
        baseIRI: 'http://example.org/',
        prefixes: { ex: 'http://example.org/' },
      });
      expect(nquads).toContain('<http://example.org/alice>');
    });

    it('should handle empty store', async () => {
      const emptyStore = new Store();
      const nquads = await toNQuads(emptyStore);
      expect(nquads).toBe('');
    });

    it('should throw error for invalid store', async () => {
      await expect(toNQuads(null)).rejects.toThrow(
        'toNQuads: store must be a valid Store instance'
      );
    });
  });

  describe('parseJsonLd', () => {
    it('should parse valid JSON-LD', async () => {
      const jsonld = {
        '@context': {
          '@vocab': 'http://example.org/',
          name: 'http://example.org/name',
          age: 'http://example.org/age',
          knows: 'http://example.org/knows',
        },
        '@id': 'alice',
        name: 'Alice',
        age: 30,
        knows: {
          '@id': 'bob',
        },
      };

      const store = await parseJsonLd(jsonld);
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBeGreaterThan(0);
    });

    it('should parse JSON-LD with base IRI', async () => {
      const jsonld = {
        '@context': {
          name: 'http://example.org/name',
        },
        '@id': 'alice',
        name: 'Alice',
      };

      const store = await parseJsonLd(jsonld, {
        baseIRI: 'http://example.org/',
      });
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBeGreaterThan(0);
    });

    it('should handle JSON-LD array', async () => {
      const jsonld = [
        {
          '@context': { name: 'http://example.org/name' },
          '@id': 'alice',
          name: 'Alice',
        },
        {
          '@context': { name: 'http://example.org/name' },
          '@id': 'bob',
          name: 'Bob',
        },
      ];

      const store = await parseJsonLd(jsonld);
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBeGreaterThan(0);
    });

    it('should throw error for invalid JSON-LD', async () => {
      const invalidJsonld = { invalid: 'structure' };

      await expect(parseJsonLd(invalidJsonld)).rejects.toThrow('Failed to parse JSON-LD');
    });

    it('should throw error for non-object input', async () => {
      await expect(parseJsonLd(123)).rejects.toThrow(
        'parseJsonLd: jsonld must be a string or object'
      );
      await expect(parseJsonLd(null)).rejects.toThrow('Failed to parse JSON-LD');
    });
  });

  describe('toJsonLd', () => {
    it('should serialize store to JSON-LD', async () => {
      const jsonld = await toJsonLd(testStore);
      expect(typeof jsonld).toBe('object');
      expect(jsonld).toHaveProperty('@context');
      expect(jsonld).toHaveProperty('@graph');
    });

    it('should serialize with custom context', async () => {
      const context = {
        '@vocab': 'http://example.org/',
        name: 'http://example.org/name',
        age: 'http://example.org/age',
      };

      const jsonld = await toJsonLd(testStore, { context });
      expect(jsonld['@context']).toEqual(context);
    });

    it('should serialize with base IRI', async () => {
      const jsonld = await toJsonLd(testStore, {
        baseIRI: 'http://example.org/',
      });
      expect(jsonld['@context']).toHaveProperty('@base', 'http://example.org/');
    });

    it('should handle empty store', async () => {
      const emptyStore = new Store();
      const jsonld = await toJsonLd(emptyStore);
      expect(jsonld).toEqual({ '@context': {}, '@graph': [] });
    });

    it('should throw error for invalid store', async () => {
      await expect(toJsonLd(null)).rejects.toThrow(
        'toJsonLd: store must be a valid Store instance'
      );
    });
  });

  describe('roundtrip tests', () => {
    it('should maintain data integrity through Turtle roundtrip', async () => {
      const originalTtl = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" ;
                 ex:age 30 ;
                 ex:knows ex:bob .
      `;

      const store1 = await parseTurtle(originalTtl);
      const serializedTtl = await toTurtle(store1);
      const store2 = await parseTurtle(serializedTtl);

      expect(store1.size).toBe(store2.size);
      expect(store1.size).toBe(3);
    });

    it('should maintain data integrity through N-Quads roundtrip', async () => {
      const nquads1 = await toNQuads(testStore);
      const store = await parseTurtle(nquads1);
      const nquads2 = await toNQuads(store);

      expect(nquads1).toBe(nquads2);
    });

    it('should maintain data integrity through JSON-LD roundtrip', async () => {
      const jsonld1 = await toJsonLd(testStore);
      const store = await parseJsonLd(jsonld1);
      const jsonld2 = await toJsonLd(store);

      expect(jsonld1['@graph']).toEqual(jsonld2['@graph']);
    });
  });

  describe('edge cases', () => {
    it('should handle Turtle with special characters', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:test ex:value "Special chars: \\"quotes\\", \\n newlines, \\t tabs" .
      `;

      const store = await parseTurtle(ttl);
      expect(store.size).toBe(1);
    });

    it('should handle Turtle with language tags', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice"@en ;
                 ex:name "Alicia"@es .
      `;

      const store = await parseTurtle(ttl);
      expect(store.size).toBe(2);
    });

    it('should handle Turtle with datatypes', async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        ex:alice ex:age "30"^^xsd:integer ;
                 ex:height "5.5"^^xsd:decimal ;
                 ex:active "true"^^xsd:boolean .
      `;

      const store = await parseTurtle(ttl);
      expect(store.size).toBe(3);
    });

    it('should handle large datasets efficiently', async () => {
      const largeStore = new Store();
      for (let i = 0; i < 1000; i++) {
        largeStore.addQuad(
          namedNode(`http://example.org/resource${i}`),
          namedNode('http://example.org/hasValue'),
          literal(`value${i}`)
        );
      }

      const ttl = await toTurtle(largeStore);
      expect(ttl.length).toBeGreaterThan(1000);

      const parsedStore = await parseTurtle(ttl);
      expect(parsedStore.size).toBe(1000);
    });
  });
});
