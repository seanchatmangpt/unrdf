/**
 * @file Tests for Embeddings Manager
 * @module test/ai-semantic/embeddings-manager
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';
import { createEmbeddingsManager } from '../../src/knowledge-engine/ai-semantic/embeddings-manager.mjs';

const { namedNode, quad } = DataFactory;

describe('EmbeddingsManager', () => {
  let store;
  let manager;

  beforeEach(() => {
    store = new Store();
    manager = createEmbeddingsManager({
      embeddingDim: 64,
      epochs: 10,
      batchSize: 32,
    });
  });

  describe('generateEmbeddings', () => {
    it('should generate embeddings for RDF graph', async () => {
      // Add test data
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const charlie = namedNode('http://example.org/charlie');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, knows, charlie));
      store.addQuad(quad(charlie, knows, alice));

      const result = await manager.generateEmbeddings(store);

      expect(result).toBeDefined();
      expect(result.entities).toBeGreaterThan(0);
      expect(result.relations).toBeGreaterThan(0);
      expect(result.algorithm).toBe('transe');
      expect(result.dimension).toBe(64);
      expect(result.duration).toBeGreaterThan(0);
    });

    it('should train using TransE algorithm', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await manager.generateEmbeddings(store, {
        algorithm: 'transe',
      });

      expect(result.algorithm).toBe('transe');
      expect(manager.entityEmbeddings.size).toBeGreaterThan(0);
      expect(manager.relationEmbeddings.size).toBeGreaterThan(0);
    });

    it('should support ComplEx algorithm', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await manager.generateEmbeddings(store, {
        algorithm: 'complex',
      });

      expect(result.algorithm).toBe('complex');
    });

    it('should support RotatE algorithm', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await manager.generateEmbeddings(store, {
        algorithm: 'rotate',
      });

      expect(result.algorithm).toBe('rotate');
    });
  });

  describe('getEmbedding', () => {
    beforeEach(async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      await manager.generateEmbeddings(store);
    });

    it('should get entity embedding', () => {
      const embedding = manager.getEmbedding('http://example.org/alice', 'entity');

      expect(embedding).toBeDefined();
      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(64);
      expect(embedding.every(v => typeof v === 'number')).toBe(true);
    });

    it('should get relation embedding', () => {
      const embedding = manager.getEmbedding('http://example.org/knows', 'relation');

      expect(embedding).toBeDefined();
      expect(Array.isArray(embedding)).toBe(true);
      expect(embedding.length).toBe(64);
    });

    it('should return null for unknown entities', () => {
      const embedding = manager.getEmbedding('http://example.org/unknown', 'entity');

      expect(embedding).toBeNull();
    });

    it('should cache embeddings', () => {
      const embedding1 = manager.getEmbedding('http://example.org/alice', 'entity');
      const embedding2 = manager.getEmbedding('http://example.org/alice', 'entity');

      expect(embedding1).toEqual(embedding2);

      const stats = manager.getStats();
      expect(stats.cacheHits).toBeGreaterThan(0);
    });
  });

  describe('computeSimilarity', () => {
    beforeEach(async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const charlie = namedNode('http://example.org/charlie');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, knows, charlie));

      await manager.generateEmbeddings(store);
    });

    it('should compute cosine similarity between entities', () => {
      const similarity = manager.computeSimilarity(
        'http://example.org/alice',
        'http://example.org/bob'
      );

      expect(typeof similarity).toBe('number');
      expect(similarity).toBeGreaterThanOrEqual(-1);
      expect(similarity).toBeLessThanOrEqual(1);
    });

    it('should return 0 for unknown entities', () => {
      const similarity = manager.computeSimilarity(
        'http://example.org/alice',
        'http://example.org/unknown'
      );

      expect(similarity).toBe(0);
    });

    it('should compute similarity between same entity as 1', () => {
      const similarity = manager.computeSimilarity(
        'http://example.org/alice',
        'http://example.org/alice'
      );

      expect(similarity).toBeCloseTo(1, 1);
    });
  });

  describe('batchGetEmbeddings', () => {
    beforeEach(async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const charlie = namedNode('http://example.org/charlie');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      store.addQuad(quad(bob, knows, charlie));

      await manager.generateEmbeddings(store);
    });

    it('should get embeddings in batch', () => {
      const uris = [
        'http://example.org/alice',
        'http://example.org/bob',
        'http://example.org/charlie',
      ];

      const embeddings = manager.batchGetEmbeddings(uris, 'entity');

      expect(embeddings).toBeInstanceOf(Array);
      expect(embeddings.length).toBe(3);
      expect(embeddings.every(e => e === null || Array.isArray(e))).toBe(true);
    });

    it('should handle mixed valid and invalid URIs', () => {
      const uris = [
        'http://example.org/alice',
        'http://example.org/unknown',
        'http://example.org/bob',
      ];

      const embeddings = manager.batchGetEmbeddings(uris, 'entity');

      expect(embeddings.length).toBe(3);
      expect(embeddings[0]).not.toBeNull();
      expect(embeddings[1]).toBeNull();
      expect(embeddings[2]).not.toBeNull();
    });
  });

  describe('cache management', () => {
    it('should clear cache', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      await manager.generateEmbeddings(store);

      manager.getEmbedding('http://example.org/alice', 'entity');
      manager.clearCache();

      const stats = manager.getStats();
      expect(stats.cacheSize).toBe(0);
    });

    it('should track statistics', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));
      await manager.generateEmbeddings(store);

      const stats = manager.getStats();

      expect(stats).toHaveProperty('embeddings');
      expect(stats).toHaveProperty('cacheHits');
      expect(stats).toHaveProperty('cacheMisses');
      expect(stats).toHaveProperty('trainingSessions');
      expect(stats).toHaveProperty('entityEmbeddings');
      expect(stats).toHaveProperty('relationEmbeddings');
      expect(stats).toHaveProperty('cacheSize');
      expect(stats).toHaveProperty('cacheHitRate');
    });
  });

  describe('edge cases', () => {
    it('should handle empty graph', async () => {
      const emptyStore = new Store();

      const result = await manager.generateEmbeddings(emptyStore);

      expect(result.entities).toBe(0);
      expect(result.relations).toBe(0);
    });

    it('should handle single triple', async () => {
      const alice = namedNode('http://example.org/alice');
      const bob = namedNode('http://example.org/bob');
      const knows = namedNode('http://example.org/knows');

      store.addQuad(quad(alice, knows, bob));

      const result = await manager.generateEmbeddings(store);

      expect(result.entities).toBe(2);
      expect(result.relations).toBe(1);
    });
  });

  describe('performance', () => {
    it('should train embeddings within reasonable time', async () => {
      // Create a moderate graph
      for (let i = 0; i < 20; i++) {
        const entity1 = namedNode(`http://example.org/entity${i}`);
        const entity2 = namedNode(`http://example.org/entity${i + 1}`);
        const pred = namedNode(`http://example.org/pred${i % 5}`);
        store.addQuad(quad(entity1, pred, entity2));
      }

      const result = await manager.generateEmbeddings(store);

      // Training should complete in reasonable time
      expect(result.duration).toBeLessThan(10000); // 10 seconds
    });
  });
});
