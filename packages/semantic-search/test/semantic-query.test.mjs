import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { SemanticQueryEngine } from '../src/search/semantic-query-engine.mjs';

const { namedNode, literal, triple } = dataFactory;

describe('SemanticQueryEngine', () => {
  let store;
  let engine;

  beforeEach(() => {
    store = createStore();

    // Add test data
    const ex = (local) => namedNode(`http://example.org/${local}`);
    const schema = (local) => namedNode(`http://schema.org/${local}`);

    store.add(triple(ex('JavaScript'), schema('name'), literal('JavaScript')));
    store.add(triple(ex('JavaScript'), schema('description'), literal('programming language for web')));
    store.add(triple(ex('Python'), schema('name'), literal('Python')));
    store.add(triple(ex('Python'), schema('description'), literal('programming language for data science')));

    engine = new SemanticQueryEngine(store);
  });

  describe('Initialization', () => {
    it('should create engine with store', () => {
      expect(engine).toBeDefined();
      expect(engine.store).toBe(store);
      expect(engine.indexed).toBe(false);
    });

    it('should initialize embedder and index', async () => {
      await engine.initialize();
      expect(engine.index).toBeDefined();
      expect(engine.embedder).toBeDefined();
    }, 30000);

    it('should throw without store', () => {
      expect(() => new SemanticQueryEngine(null)).toThrow('RDF store is required');
    });
  });

  describe('Store Indexing', () => {
    it('should index all triples in store', async () => {
      const count = await engine.indexStore();

      expect(count).toBe(store.size);
      expect(engine.indexed).toBe(true);
      expect(engine.tripleCache.length).toBe(store.size);
    }, 30000);
  });

  describe('Semantic Search', () => {
    it('should search with natural language query', async () => {
      await engine.initialize();
      await engine.indexStore();

      const results = await engine.search('web development language', {
        limit: 5,
        threshold: 0.1,
      });

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);

      results.forEach(result => {
        expect(result).toHaveProperty('triple');
        expect(result).toHaveProperty('score');
        expect(result).toHaveProperty('text');
        expect(result.score).toBeGreaterThanOrEqual(0.1);
      });
    }, 30000);

    it('should respect result limit', async () => {
      await engine.indexStore();

      const results = await engine.search('programming', { limit: 2 });
      expect(results.length).toBeLessThanOrEqual(2);
    }, 30000);

    it('should filter by threshold', async () => {
      await engine.indexStore();

      const results = await engine.search('unrelated query xyz', {
        threshold: 0.9,
      });

      // Should return few or no results with high threshold
      expect(results.every(r => r.score >= 0.9)).toBe(true);
    }, 30000);

    it('should auto-index on first search', async () => {
      expect(engine.indexed).toBe(false);

      await engine.search('test query');

      expect(engine.indexed).toBe(true);
    }, 30000);
  });

  describe('Hybrid Search', () => {
    it('should combine semantic and SPARQL search', async () => {
      const nlQuery = 'web programming';
      const sparqlPattern = '?s <http://schema.org/name> "JavaScript"';

      const results = await engine.hybridSearch(nlQuery, sparqlPattern, {
        limit: 5,
        threshold: 0.1,
      });

      expect(Array.isArray(results)).toBe(true);

      results.forEach(result => {
        expect(result).toHaveProperty('score');
        expect(result).toHaveProperty('hybridScore');
        expect(result).toHaveProperty('matchesSparql');
      });
    }, 30000);

    it('should work without SPARQL pattern', async () => {
      const results = await engine.hybridSearch('programming', null, {
        limit: 3,
      });

      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBeGreaterThan(0);
    }, 30000);
  });

  describe('Similar Triples', () => {
    it('should find similar triples', async () => {
      await engine.indexStore();

      const referenceTriple = {
        subject: { value: 'http://example.org/JavaScript' },
        predicate: { value: 'http://schema.org/description' },
        object: { value: 'programming language for web' },
      };

      const similar = await engine.findSimilar(referenceTriple, 3);

      expect(Array.isArray(similar)).toBe(true);

      similar.forEach(result => {
        expect(result).toHaveProperty('triple');
        expect(result).toHaveProperty('score');

        // Should not include the reference triple itself
        expect(result.triple.subject).not.toBe(referenceTriple.subject.value);
      });
    }, 30000);
  });

  describe('Autocomplete', () => {
    it('should provide query suggestions', async () => {
      await engine.indexStore();

      const suggestions = await engine.autocomplete('program', 5);

      expect(Array.isArray(suggestions)).toBe(true);
      expect(suggestions.length).toBeGreaterThan(0);
      expect(suggestions.every(s => typeof s === 'string')).toBe(true);
    }, 30000);

    it('should limit suggestions', async () => {
      await engine.indexStore();

      const suggestions = await engine.autocomplete('test', 2);
      expect(suggestions.length).toBeLessThanOrEqual(2);
    }, 30000);
  });

  describe('Statistics', () => {
    it('should provide engine statistics', async () => {
      await engine.indexStore();

      const stats = engine.getStats();

      expect(stats).toHaveProperty('indexed');
      expect(stats).toHaveProperty('tripleCount');
      expect(stats).toHaveProperty('cacheStats');
      expect(stats).toHaveProperty('storeSize');

      expect(stats.indexed).toBe(true);
      expect(stats.tripleCount).toBe(store.size);
    }, 30000);
  });

  describe('Options Validation', () => {
    it('should use default options', async () => {
      await engine.indexStore();

      const results = await engine.search('test');

      expect(Array.isArray(results)).toBe(true);
    }, 30000);

    it('should validate options with Zod', async () => {
      await engine.indexStore();

      // Should not throw with valid options
      await expect(
        engine.search('test', { limit: 5, threshold: 0.5 })
      ).resolves.toBeDefined();
    }, 30000);
  });
});
