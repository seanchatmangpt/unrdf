import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { KnowledgeRecommender } from '../src/discovery/knowledge-recommender.mjs';

const { namedNode, literal, triple } = dataFactory;

describe('KnowledgeRecommender', () => {
  let store;
  let recommender;

  beforeEach(() => {
    store = createStore();

    // Create test knowledge graph
    const ex = (local) => namedNode(`http://example.org/${local}`);
    const schema = (local) => namedNode(`http://schema.org/${local}`);

    // Entity 1: JavaScript
    store.add(triple(ex('JavaScript'), schema('name'), literal('JavaScript')));
    store.add(triple(ex('JavaScript'), schema('description'), literal('web programming language')));
    store.add(triple(ex('JavaScript'), ex('paradigm'), literal('multi-paradigm')));

    // Entity 2: Python
    store.add(triple(ex('Python'), schema('name'), literal('Python')));
    store.add(triple(ex('Python'), schema('description'), literal('data science language')));
    store.add(triple(ex('Python'), ex('paradigm'), literal('multi-paradigm')));

    // Entity 3: Rust
    store.add(triple(ex('Rust'), schema('name'), literal('Rust')));
    store.add(triple(ex('Rust'), schema('description'), literal('systems programming language')));
    store.add(triple(ex('Rust'), ex('paradigm'), literal('multi-paradigm')));

    recommender = new KnowledgeRecommender(store);
  });

  describe('Initialization', () => {
    it('should create recommender with store', () => {
      expect(recommender).toBeDefined();
      expect(recommender.store).toBe(store);
    });

    it('should initialize embedder', async () => {
      await recommender.initialize();
      expect(recommender.embedder.embedder).toBeDefined();
    }, 30000);

    it('should throw without store', () => {
      expect(() => new KnowledgeRecommender(null)).toThrow('RDF store is required');
    });
  });

  describe('Entity Triples', () => {
    it('should get triples about entity', () => {
      const entityUri = 'http://example.org/JavaScript';
      const triples = recommender.getEntityTriples(entityUri);

      expect(Array.isArray(triples)).toBe(true);
      expect(triples.length).toBeGreaterThan(0);

      triples.forEach(t => {
        const hasEntity =
          t.subject.value === entityUri || t.object.value === entityUri;
        expect(hasEntity).toBe(true);
      });
    });

    it('should return empty array for non-existent entity', () => {
      const triples = recommender.getEntityTriples('http://example.org/NonExistent');
      expect(triples).toEqual([]);
    });
  });

  describe('Entity Embedding', () => {
    it('should generate entity embedding', async () => {
      const entityUri = 'http://example.org/JavaScript';
      const embedding = await recommender.embedEntity(entityUri);

      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(384);

      // Should be normalized
      const norm = Math.sqrt(embedding.reduce((sum, v) => sum + v * v, 0));
      expect(norm).toBeCloseTo(1, 5);
    }, 30000);

    it('should cache entity embeddings', async () => {
      const entityUri = 'http://example.org/Python';

      const embedding1 = await recommender.embedEntity(entityUri);
      const embedding2 = await recommender.embedEntity(entityUri);

      expect(embedding1).toBe(embedding2);

      const stats = recommender.getStats();
      expect(stats.cachedEntities).toBeGreaterThan(0);
    }, 30000);

    it('should throw for entity with no triples', async () => {
      await expect(
        recommender.embedEntity('http://example.org/NonExistent')
      ).rejects.toThrow('No triples found');
    }, 30000);
  });

  describe('Similar Entities', () => {
    it('should find similar entities', async () => {
      await recommender.initialize();

      const results = await recommender.findSimilarEntities(
        'http://example.org/JavaScript',
        { limit: 2, threshold: 0.3 }
      );

      expect(Array.isArray(results)).toBe(true);

      results.forEach(result => {
        expect(result).toHaveProperty('entity');
        expect(result).toHaveProperty('label');
        expect(result).toHaveProperty('score');
        expect(result.entity).not.toBe('http://example.org/JavaScript');
        expect(result.score).toBeGreaterThanOrEqual(0.3);
      });
    }, 30000);

    it('should respect limit', async () => {
      const results = await recommender.findSimilarEntities(
        'http://example.org/Python',
        { limit: 1 }
      );

      expect(results.length).toBeLessThanOrEqual(1);
    }, 30000);

    it('should filter by threshold', async () => {
      const results = await recommender.findSimilarEntities(
        'http://example.org/Rust',
        { threshold: 0.9 }
      );

      results.forEach(result => {
        expect(result.score).toBeGreaterThanOrEqual(0.9);
      });
    }, 30000);
  });

  describe('Concept Recommendations', () => {
    it('should recommend concepts for query', async () => {
      await recommender.initialize();

      const results = await recommender.recommendConcepts(
        'web development programming',
        { limit: 3, threshold: 0.2 }
      );

      expect(Array.isArray(results)).toBe(true);

      results.forEach(result => {
        expect(result).toHaveProperty('entity');
        expect(result).toHaveProperty('label');
        expect(result).toHaveProperty('score');
        expect(result).toHaveProperty('tripleCount');
        expect(result.tripleCount).toBeGreaterThan(0);
      });
    }, 30000);

    it('should sort by relevance score', async () => {
      const results = await recommender.recommendConcepts('programming', {
        limit: 5,
      });

      for (let i = 0; i < results.length - 1; i++) {
        expect(results[i].score).toBeGreaterThanOrEqual(results[i + 1].score);
      }
    }, 30000);
  });

  describe('Cosine Similarity', () => {
    it('should calculate cosine similarity', () => {
      const vec1 = [1, 0, 0];
      const vec2 = [1, 0, 0];

      const similarity = recommender.cosineSimilarity(vec1, vec2);
      expect(similarity).toBeCloseTo(1, 5);
    });

    it('should handle orthogonal vectors', () => {
      const vec1 = [1, 0, 0];
      const vec2 = [0, 1, 0];

      const similarity = recommender.cosineSimilarity(vec1, vec2);
      expect(similarity).toBeCloseTo(0, 5);
    });

    it('should throw for different dimensions', () => {
      const vec1 = [1, 0];
      const vec2 = [1, 0, 0];

      expect(() => recommender.cosineSimilarity(vec1, vec2)).toThrow();
    });
  });

  describe('Entity Discovery', () => {
    it('should get all entities in graph', () => {
      const entities = recommender.getAllEntities();

      expect(Array.isArray(entities)).toBe(true);
      expect(entities.length).toBeGreaterThan(0);

      // Should only include URIs
      entities.forEach(entity => {
        expect(entity.startsWith('http')).toBe(true);
      });

      // Should include our test entities
      expect(entities).toContain('http://example.org/JavaScript');
      expect(entities).toContain('http://example.org/Python');
      expect(entities).toContain('http://example.org/Rust');
    });
  });

  describe('Cache Management', () => {
    it('should clear caches', async () => {
      await recommender.embedEntity('http://example.org/JavaScript');

      expect(recommender.entityEmbeddings.size).toBeGreaterThan(0);

      recommender.clearCache();

      expect(recommender.entityEmbeddings.size).toBe(0);
    }, 30000);
  });

  describe('Statistics', () => {
    it('should provide statistics', async () => {
      await recommender.embedEntity('http://example.org/JavaScript');

      const stats = recommender.getStats();

      expect(stats).toHaveProperty('cachedEntities');
      expect(stats).toHaveProperty('totalEntities');
      expect(stats).toHaveProperty('embedderStats');

      expect(stats.cachedEntities).toBeGreaterThan(0);
      expect(stats.totalEntities).toBeGreaterThan(0);
    }, 30000);
  });
});
