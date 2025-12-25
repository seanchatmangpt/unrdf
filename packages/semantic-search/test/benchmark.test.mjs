import { describe, it, expect, beforeAll } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  RDFEmbedder,
  SemanticQueryEngine,
  KnowledgeRecommender,
} from '../src/index.mjs';

const { namedNode, literal, triple } = dataFactory;

describe('Performance Benchmarks', () => {
  let store;
  let engine;
  let recommender;
  let embedder;

  beforeAll(async () => {
    // Create larger test dataset
    store = createStore();
    const ex = (local) => namedNode(`http://example.org/${local}`);
    const schema = (local) => namedNode(`http://schema.org/${local}`);

    // Add 50 entities with 5 triples each = 250 triples
    for (let i = 0; i < 50; i++) {
      const entity = ex(`Entity${i}`);
      store.add(triple(entity, schema('name'), literal(`Entity ${i}`)));
      store.add(triple(entity, schema('description'), literal(`Description for entity ${i}`)));
      store.add(triple(entity, ex('index'), literal(`${i}`)));
      store.add(triple(entity, ex('category'), literal(`Category ${i % 5}`)));
      store.add(triple(entity, ex('value'), literal(`Value ${i * 10}`)));
    }

    embedder = new RDFEmbedder();
    await embedder.initialize();

    engine = new SemanticQueryEngine(store);
    await engine.initialize();
    await engine.indexStore();

    recommender = new KnowledgeRecommender(store);
    await recommender.initialize();
  }, 60000);

  describe('Embedding Performance', () => {
    it('should embed single triple in < 100ms', async () => {
      const triple = {
        subject: { value: 'http://example.org/Test' },
        predicate: { value: 'http://schema.org/name' },
        object: { value: 'Test Entity' },
      };

      const start = performance.now();
      await embedder.embedTriple(triple);
      const duration = performance.now() - start;

      console.log(`Single triple embedding: ${duration.toFixed(2)}ms`);
      expect(duration).toBeLessThan(100);
    }, 10000);

    it('should batch embed 10 triples in < 500ms', async () => {
      const triples = [];
      for (let i = 0; i < 10; i++) {
        triples.push({
          subject: { value: `http://example.org/Entity${i}` },
          predicate: { value: 'http://schema.org/name' },
          object: { value: `Entity ${i}` },
        });
      }

      const start = performance.now();
      await embedder.embedTriples(triples);
      const duration = performance.now() - start;

      console.log(`Batch 10 triples: ${duration.toFixed(2)}ms`);
      expect(duration).toBeLessThan(500);
    }, 10000);

    it('should benefit from caching', async () => {
      const text = 'test entity description';

      // First call - not cached
      const start1 = performance.now();
      await embedder.embedText(text);
      const duration1 = performance.now() - start1;

      // Second call - cached
      const start2 = performance.now();
      await embedder.embedText(text);
      const duration2 = performance.now() - start2;

      console.log(`Uncached: ${duration1.toFixed(2)}ms, Cached: ${duration2.toFixed(2)}ms`);
      console.log(`Speedup: ${(duration1 / duration2).toFixed(1)}x`);

      expect(duration2).toBeLessThan(duration1);
    }, 10000);
  });

  describe('Search Performance', () => {
    it('should index 250 triples in < 30s', async () => {
      const freshEngine = new SemanticQueryEngine(store);
      await freshEngine.initialize();

      const start = performance.now();
      const count = await freshEngine.indexStore();
      const duration = performance.now() - start;

      console.log(`Indexed ${count} triples in ${duration.toFixed(0)}ms`);
      console.log(`Rate: ${(count / (duration / 1000)).toFixed(0)} triples/sec`);

      expect(duration).toBeLessThan(30000);
      expect(count).toBe(store.size);
    }, 60000);

    it('should search in < 1000ms', async () => {
      const queries = [
        'entity description',
        'category value',
        'test query',
      ];

      for (const query of queries) {
        const start = performance.now();
        const results = await engine.search(query, { limit: 10 });
        const duration = performance.now() - start;

        console.log(`Search "${query}": ${duration.toFixed(2)}ms, ${results.length} results`);
        expect(duration).toBeLessThan(1000);
      }
    }, 10000);

    it('should perform hybrid search in < 1500ms', async () => {
      const nlQuery = 'entity with value';
      const sparqlPattern = '?s <http://example.org/category> ?category';

      const start = performance.now();
      const results = await engine.hybridSearch(nlQuery, sparqlPattern, {
        limit: 10,
      });
      const duration = performance.now() - start;

      console.log(`Hybrid search: ${duration.toFixed(2)}ms, ${results.length} results`);
      expect(duration).toBeLessThan(1500);
    }, 10000);
  });

  describe('Recommendation Performance', () => {
    it('should find similar entities in < 2000ms', async () => {
      const entityUri = 'http://example.org/Entity0';

      const start = performance.now();
      const results = await recommender.findSimilarEntities(entityUri, {
        limit: 5,
        threshold: 0.3,
      });
      const duration = performance.now() - start;

      console.log(`Similar entities: ${duration.toFixed(2)}ms, ${results.length} results`);
      expect(duration).toBeLessThan(2000);
    }, 10000);

    it('should recommend concepts in < 2000ms', async () => {
      const query = 'entity with description';

      const start = performance.now();
      const results = await recommender.recommendConcepts(query, {
        limit: 5,
        threshold: 0.3,
      });
      const duration = performance.now() - start;

      console.log(`Concept recommendations: ${duration.toFixed(2)}ms, ${results.length} results`);
      expect(duration).toBeLessThan(2000);
    }, 10000);
  });

  describe('Memory Efficiency', () => {
    it('should report cache statistics', () => {
      const embedderStats = embedder.getCacheStats();
      const engineStats = engine.getStats();
      const recommenderStats = recommender.getStats();

      console.log('\nCache Statistics:');
      console.log('Embedder:', embedderStats);
      console.log('Engine:', engineStats);
      console.log('Recommender:', recommenderStats);

      expect(embedderStats.size).toBeGreaterThan(0);
      expect(engineStats.indexed).toBe(true);
    });
  });

  describe('Accuracy Metrics', () => {
    it('should return relevant results for specific queries', async () => {
      const results = await engine.search('entity 5 description', {
        limit: 5,
        threshold: 0.2,
      });

      console.log('\nTop 3 results for "entity 5 description":');
      results.slice(0, 3).forEach((r, i) => {
        console.log(`${i + 1}. Score: ${r.score.toFixed(3)} - ${r.text.substring(0, 60)}`);
      });

      expect(results.length).toBeGreaterThan(0);
      expect(results[0].score).toBeGreaterThan(0);
    }, 10000);
  });

  describe('Scalability', () => {
    it('should handle autocomplete efficiently', async () => {
      const partials = ['ent', 'desc', 'cat'];

      for (const partial of partials) {
        const start = performance.now();
        const suggestions = await engine.autocomplete(partial, 5);
        const duration = performance.now() - start;

        console.log(`Autocomplete "${partial}": ${duration.toFixed(2)}ms, ${suggestions.length} suggestions`);
        expect(duration).toBeLessThan(1000);
      }
    }, 10000);
  });
});
