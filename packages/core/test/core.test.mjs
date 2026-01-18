/**
 * @unrdf/core Test Suite (80/20 fast suite)
 *
 * Critical tests for RDF operations and SPARQL execution
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createStore,
  addQuad,
  removeQuad,
  getQuads,
  countQuads,
  namedNode,
  literal,
  defaultGraph,
  executeSelect,
  executeAsk,
  FOAF,
  validateQuad,
  validateStore,
} from '../src/index.mjs';

describe('@unrdf/core', () => {
  describe('Store Operations', () => {
    let store;

    beforeEach(() => {
      store = createStore();
    });

    it('should create, add and retrieve quads', () => {
      expect(store).toBeDefined();

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
    });

    it('should remove quads and count correctly', () => {
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
  });

  describe('SPARQL Queries', () => {
    let store;

    beforeEach(() => {
      store = createStore();
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
    });

    it('should execute SELECT queries', async () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE { ?s foaf:name ?name }
      `;

      const results = await executeSelect(store, query);
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(2);
    });

    it('should execute ASK queries', async () => {
      const query = `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        ASK { ?s foaf:name "Alice" }
      `;

      const result = await executeAsk(store, query);
      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });
  });

  describe('Validation', () => {
    it('should validate quads and stores', () => {
      const store = createStore();

      const validQuad = {
        subject: { value: 'http://example.org/alice' },
        predicate: { value: 'http://xmlns.com/foaf/0.1/name' },
        object: { value: 'Alice' },
      };

      expect(validateQuad(validQuad)).toBe(true);
      expect(validateQuad(null)).toBe(false);
      expect(validateStore(store)).toBe(true);
      expect(validateStore(null)).toBe(false);
    });
  });
});
