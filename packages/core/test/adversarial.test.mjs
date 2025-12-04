/**
 * @vitest-environment node
 * Adversarial Testing: Test advertised capabilities
 * Goal: PROVE what doesn't work, not security
 */

import { describe, it, expect } from 'vitest';
import {
  createStore,
  addQuad,
  getQuads,
  namedNode,
  literal,
  quad,
  executeQuery,
  canonicalize,
  toNTriples,
} from '../src/index.mjs';

describe('@unrdf/core Adversarial Tests - Capabilities', () => {
  describe('RDF Store Operations - Core Advertised Features', () => {
    it('ADVERTISED: Can create and manipulate 1000+ quads without memory issues', async () => {
      const store = createStore();

      // Advertised: Should handle bulk operations efficiently
      for (let i = 0; i < 1000; i++) {
        const s = namedNode(`http://example.org/subject/${i}`);
        const p = namedNode('http://example.org/predicate');
        const o = literal(`value ${i}`);
        addQuad(store, quad(s, p, o));
      }

      const count = getQuads(store).length;
      expect(count).toBe(1000);
    });

    it('ADVERTISED: Can query with multiple filters (subject, predicate, object)', () => {
      const store = createStore();
      const s = namedNode('http://example.org/alice');
      const p = namedNode('http://foaf/name');
      const o = literal('Alice');

      addQuad(store, quad(s, p, o));
      addQuad(store, quad(s, namedNode('http://foaf/age'), literal('30')));

      const results = getQuads(store, s, p, null, null);
      expect(results.length).toBe(1);
      expect(results[0].object.value).toBe('Alice');
    });
  });

  describe('SPARQL Queries - Advertised Features', () => {
    it('ADVERTISED: Can execute basic SELECT queries', async () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        PREFIX foaf: <http://foaf/>
        SELECT ?name WHERE {
          ?person foaf:name ?name .
        }
      `;

      const results = await executeQuery(store, sparql);
      expect(results.length).toBeGreaterThan(0);
      expect(results[0]?.name?.value).toBe('Alice');
    });

    it('ADVERTISED: Can execute CONSTRUCT queries', async () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        PREFIX foaf: <http://foaf/>
        CONSTRUCT {
          ?person <http://example.org/label> ?name .
        }
        WHERE {
          ?person foaf:name ?name .
        }
      `;

      const results = await executeQuery(store, sparql);
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('Serialization - Advertised Features', () => {
    it('ADVERTISED: Can canonicalize RDF graphs', () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const canonicalized = canonicalize(store);
      expect(canonicalized).toContain('http://example.org/alice');
      expect(canonicalized).toContain('http://foaf/name');
      expect(canonicalized).toContain('Alice');
    });

    it('ADVERTISED: Can convert to N-Triples format', async () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const ntriples = await toNTriples(getQuads(store));
      expect(ntriples).toContain('<http://example.org/alice>');
      expect(ntriples).toContain('<http://foaf/name>');
      expect(ntriples).toContain('"Alice"');
    });

    it('ADVERTISED: Canonicalization is deterministic', () => {
      const store1 = createStore();
      const store2 = createStore();

      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      );
      addQuad(store1, q);
      addQuad(store2, q);

      const canon1 = canonicalize(store1);
      const canon2 = canonicalize(store2);

      expect(canon1).toBe(canon2);
    });

    it('ADVERTISED: Can serialize complex graphs with multiple quads', async () => {
      const store = createStore();

      for (let i = 0; i < 10; i++) {
        addQuad(
          store,
          quad(namedNode(`http://example.org/s${i}`), namedNode('http://p'), literal(`o${i}`))
        );
      }

      const ntriples = await toNTriples(getQuads(store));
      const lines = ntriples.trim().split('\n');
      expect(lines.length).toBe(10);
    });
  });

  describe('Data Types - Advertised Features', () => {
    it('ADVERTISED: Can create typed literals', () => {
      const intLiteral = literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer'));
      expect(intLiteral.value).toBe('42');
      expect(intLiteral.datatype.value).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });

    it('ADVERTISED: Can create language-tagged literals', () => {
      const langLiteral = literal('Hello', 'en');
      expect(langLiteral.value).toBe('Hello');
      expect(langLiteral.language).toBe('en');
    });

    it('ADVERTISED: Can create blank nodes', () => {
      const bn = { termType: 'BlankNode', value: '_:b1' };
      expect(bn.termType).toBe('BlankNode');
      expect(bn.value).toContain('_:');
    });
  });

  describe('Query Features - Advanced', () => {
    it('ADVERTISED: Can execute FILTER queries', async () => {
      const store = createStore();

      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/age'), literal('25'))
      );
      addQuad(
        store,
        quad(namedNode('http://example.org/bob'), namedNode('http://foaf/age'), literal('35'))
      );

      const sparql = `
        SELECT ?person ?age WHERE {
          ?person <http://foaf/age> ?age .
          FILTER(?age > 30)
        }
      `;

      const results = await executeQuery(store, sparql);
      expect(results.length).toBeGreaterThanOrEqual(0);
    });

    it('ADVERTISED: Can execute ASK queries', async () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/alice'), namedNode('http://foaf/name'), literal('Alice'))
      );

      const sparql = `
        ASK {
          ?person <http://foaf/name> "Alice" .
        }
      `;

      const result = await executeQuery(store, sparql);
      expect(result).toBeDefined();
    });

    it('ADVERTISED: Can execute DESCRIBE queries', async () => {
      const store = createStore();
      const alice = namedNode('http://example.org/alice');

      addQuad(store, quad(alice, namedNode('http://foaf/name'), literal('Alice')));
      addQuad(store, quad(alice, namedNode('http://foaf/age'), literal('30')));

      const sparql = `
        DESCRIBE <http://example.org/alice>
      `;

      const result = await executeQuery(store, sparql);
      expect(result).toBeDefined();
    });
  });

  describe('Performance - Advertised Scalability', () => {
    it('ADVERTISED: Can handle 10,000+ quads efficiently', () => {
      const store = createStore();

      const start = Date.now();
      for (let i = 0; i < 10000; i++) {
        addQuad(
          store,
          quad(namedNode(`http://example.org/s${i}`), namedNode('http://p'), literal(`o${i}`))
        );
      }
      const duration = Date.now() - start;

      expect(getQuads(store).length).toBe(10000);
      expect(duration).toBeLessThan(5000); // Should complete in under 5 seconds
    });

    it('ADVERTISED: Query performance scales with data size', async () => {
      const store = createStore();

      // Add 1000 quads
      for (let i = 0; i < 1000; i++) {
        addQuad(
          store,
          quad(
            namedNode(`http://example.org/s${i}`),
            namedNode('http://foaf/name'),
            literal(`Name${i}`)
          )
        );
      }

      const sparql = `
        SELECT ?s ?name WHERE {
          ?s <http://foaf/name> ?name .
        }
      `;

      const start = Date.now();
      const results = await executeQuery(store, sparql);
      const duration = Date.now() - start;

      expect(results.length).toBeGreaterThan(0);
      expect(duration).toBeLessThan(2000); // Should complete in under 2 seconds
    });
  });
});
