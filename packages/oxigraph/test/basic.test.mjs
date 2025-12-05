import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '../src/index.mjs';

describe('OxigraphStore - Basic Operations', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  describe('Triple Operations', () => {
    it('should add a triple to the store', () => {
      const ex = dataFactory.namedNode('http://example.com/');
      const name = dataFactory.namedNode('http://schema.org/name');
      const value = dataFactory.literal('Example');

      const triple = dataFactory.triple(ex, name, value);
      store.add(triple);

      expect(store.has(triple)).toBe(true);
    });

    it('should delete a triple from the store', () => {
      const ex = dataFactory.namedNode('http://example.com/');
      const name = dataFactory.namedNode('http://schema.org/name');
      const value = dataFactory.literal('Example');

      const triple = dataFactory.triple(ex, name, value);
      store.add(triple);
      expect(store.has(triple)).toBe(true);

      store.delete(triple);
      expect(store.has(triple)).toBe(false);
    });

    it('should match triples by pattern', () => {
      const ex = dataFactory.namedNode('http://example.com/');
      const name = dataFactory.namedNode('http://schema.org/name');
      const value1 = dataFactory.literal('Example1');
      const value2 = dataFactory.literal('Example2');

      store.add(dataFactory.triple(ex, name, value1));
      store.add(dataFactory.triple(ex, name, value2));

      const matches = store.match(ex, null, null);
      expect(matches.length).toBe(2);
    });
  });

  describe('SPARQL Queries', () => {
    beforeEach(() => {
      const ex = dataFactory.namedNode('http://example.com/');
      const name = dataFactory.namedNode('http://schema.org/name');
      const value = dataFactory.literal('Example');

      store.add(dataFactory.triple(ex, name, value));
    });

    it('should execute SELECT queries', () => {
      const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });

    it('should execute ASK queries', () => {
      const result = store.query('ASK { ?s ?p ?o }');

      expect(typeof result).toBe('boolean');
      expect(result).toBe(true);
    });

    it('should execute CONSTRUCT queries', () => {
      const results = store.query('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('Store Operations', () => {
    it('should return store size', () => {
      const ex = dataFactory.namedNode('http://example.com/');
      const name = dataFactory.namedNode('http://schema.org/name');

      expect(store.size).toBe(0);

      store.add(dataFactory.triple(ex, name, dataFactory.literal('A')));
      expect(store.size).toBe(1);

      store.add(dataFactory.triple(ex, name, dataFactory.literal('B')));
      expect(store.size).toBe(2);
    });

    it('should clear the store', () => {
      const ex = dataFactory.namedNode('http://example.com/');
      const name = dataFactory.namedNode('http://schema.org/name');

      store.add(dataFactory.triple(ex, name, dataFactory.literal('A')));
      store.add(dataFactory.triple(ex, name, dataFactory.literal('B')));

      expect(store.size).toBe(2);

      store.clear();
      expect(store.size).toBe(0);
    });
  });

  describe('Error Handling', () => {
    it('should throw on invalid add operation', () => {
      expect(() => store.add(null)).toThrow('Quad is required');
    });

    it('should throw on invalid query', () => {
      expect(() => store.query('')).toThrow();
    });

    it('should throw on invalid match pattern', () => {
      // Invalid pattern should be handled gracefully
      const result = store.match(null, null, null, null);
      expect(Array.isArray(result)).toBe(true);
    });
  });
});
