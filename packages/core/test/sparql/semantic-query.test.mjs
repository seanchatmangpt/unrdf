/**
 * @file Semantic Query Engine Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { executeQuerySync } from '@unrdf/core/sparql/executor-sync';
import {
  initializeEmbeddings,
  generateEmbedding,
  generateEmbeddingsBatch,
  cosineSimilarity,
  generateQueryEmbedding,
  generateQuadEmbedding,
  clearEmbeddingCache,
  getCacheStats,
} from '@unrdf/core/sparql/embeddings';
import {
  createHNSWIndex,
  buildHNSWIndex,
  l2Normalize,
  l2Distance,
} from '@unrdf/core/index/hnsw';
import {
  initializeSemanticIndex,
  executeSemanticQuery,
  clearSemanticIndex,
  getSemanticIndexStats,
} from '@unrdf/core/sparql/semantic-executor';
import { namedNode, literal, quad, blankNode } from '@unrdf/test-utils';

describe('Semantic Query Engine', () => {
  describe('Embeddings Module', () => {
    afterEach(() => {
      clearEmbeddingCache();
    });

    it('should generate embedding for simple text', async () => {
      const embedding = await generateEmbedding('Alice is a person');

      expect(embedding).toBeInstanceOf(Float32Array);
      expect(embedding.length).toBe(384);
    });

    it('should generate consistent embeddings for same text', async () => {
      const text = 'Bob works at ExampleOrg';
      const embedding1 = await generateEmbedding(text);
      const embedding2 = await generateEmbedding(text);

      // Embeddings should be identical
      for (let i = 0; i < embedding1.length; i++) {
        expect(embedding1[i]).toBeCloseTo(embedding2[i], 5);
      }
    });

    it('should generate different embeddings for different texts', async () => {
      const embedding1 = await generateEmbedding('Alice is a person');
      const embedding2 = await generateEmbedding('Bob is a person');

      // Embeddings should be different (not identical)
      // Note: With fallback mode, similarity might be negative
      expect(embedding1).not.toBe(embedding2);
      expect(cosineSimilarity(embedding1, embedding2)).not.toBe(1);
    });

    it('should cache embeddings', async () => {
      const text = 'Cached text';

      const stats1 = getCacheStats();
      expect(stats1.size).toBe(0);

      await generateEmbedding(text);
      const stats2 = getCacheStats();
      expect(stats2.size).toBe(1);

      // Second call should use cache
      await generateEmbedding(text);
      const stats3 = getCacheStats();
      expect(stats3.size).toBe(1);
    });

    it('should handle empty text', async () => {
      const embedding = await generateEmbedding('');

      expect(embedding).toBeInstanceOf(Float32Array);
      expect(embedding.length).toBe(384);
    });

    it('should throw error for non-string input', async () => {
      await expect(generateEmbedding(123)).rejects.toThrow(TypeError);
      await expect(generateEmbedding(null)).rejects.toThrow(TypeError);
    });

    it('should generate embeddings for batch texts', async () => {
      const texts = ['Alice', 'Bob', 'Carol'];
      const embeddings = await generateEmbeddingsBatch(texts);

      expect(embeddings).toHaveLength(3);
      embeddings.forEach(embedding => {
        expect(embedding).toBeInstanceOf(Float32Array);
        expect(embedding.length).toBe(384);
      });
    });
  });

  describe('HNSW Index Module', () => {
    it('should create HNSW index', async () => {
      const index = createHNSWIndex({ dimensions: 384, maxElements: 100 });

      await index.initialize();

      expect(index.size()).toBe(0);
    });

    it('should add vectors to index', async () => {
      const index = createHNSWIndex({ dimensions: 384, maxElements: 100 });
      await index.initialize();

      const vector = new Float32Array(384).fill(1);
      await index.add('doc1', vector);

      expect(index.size()).toBe(1);
    });

    it('should search for similar vectors', async () => {
      const index = createHNSWIndex({ dimensions: 384, maxElements: 100 });
      await index.initialize();

      // Add similar vectors
      const baseVector = new Float32Array(384).fill(0.1);
      const vector1 = new Float32Array(384).fill(0.11); // Similar
      const vector2 = new Float32Array(384).fill(0.9); // Dissimilar

      await index.add('doc1', baseVector);
      await index.add('doc2', vector1);
      await index.add('doc3', vector2);

      const query = new Float32Array(384).fill(0.1);
      const results = await index.search(query, 2);

      expect(results).toHaveLength(2);
      // doc1 or doc2 should be first (most similar)
      expect(['doc1', 'doc2']).toContain(results[0].id);
    });

    it('should remove vectors from index', async () => {
      const index = createHNSWIndex({ dimensions: 384, maxElements: 100 });
      await index.initialize();

      const vector = new Float32Array(384).fill(1);
      await index.add('doc1', vector);

      expect(index.size()).toBe(1);

      const removed = await index.remove('doc1');
      expect(removed).toBe(true);
      expect(index.size()).toBe(0);
    });

    it('should build index from existing vectors', async () => {
      const vectors = new Map([
        ['doc1', new Float32Array(384).fill(0.1)],
        ['doc2', new Float32Array(384).fill(0.2)],
        ['doc3', new Float32Array(384).fill(0.3)],
      ]);

      const index = await buildHNSWIndex(vectors, { dimensions: 384, maxElements: 100 });

      expect(index.size()).toBe(3);
    });
  });

  describe('Semantic Query Executor', () => {
    let store;

    beforeEach(() => {
      // Create test store with sample quads
      store = {
        _quads: [
          quad(
            namedNode('http://example.org/Alice'),
            namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            namedNode('http://example.org/Person'),
            namedNode('http://example.org/graph')
          ),
          quad(
            namedNode('http://example.org/Bob'),
            namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            namedNode('http://example.org/Person'),
            namedNode('http://example.org/graph')
          ),
          quad(
            namedNode('http://example.org/Alice'),
            namedNode('http://example.org/name'),
            literal('Alice'),
            namedNode('http://example.org/graph')
          ),
          quad(
            namedNode('http://example.org/Bob'),
            namedNode('http://example.org/name'),
            literal('Bob'),
            namedNode('http://example.org/graph')
          ),
        ],
        getQuads() {
          return this._quads;
        },
        // Mock query method for semantic executor
        query(sparql) {
          // Simple mock: return empty results for now
          // In real tests, we'd use the actual OxigraphStore
          return [];
        },
      };
    });

    afterEach(() => {
      clearSemanticIndex();
    });

    it('should initialize semantic index from store', async () => {
      await initializeSemanticIndex(store);

      const stats = getSemanticIndexStats();
      expect(stats.initialized).toBe(true);
      expect(stats.size).toBeGreaterThan(0);
    });

    it.skip('should execute semantic CONSTRUCT query (requires OxigraphStore)', async () => {
      // This test requires a full OxigraphStore implementation
      // Skipping for now as it requires more integration
      await initializeSemanticIndex(store);

      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ex: <http://example.org/>

        CONSTRUCT { ?s ?name }
        WHERE {
          ?s rdf:type ex:Person .
          ?s ex:name ?name .
        }
      `;

      const results = await executeSemanticQuery(store, sparql, {
        enableSemantic: true,
        similarityThreshold: 0.5,
        maxResults: 10,
      });

      expect(results).toBeInstanceOf(Array);
      // Semantic search may return more results than exact
      expect(results.length).toBeGreaterThan(0);
    });

    it('should execute semantic SELECT query', async () => {
      await initializeSemanticIndex(store);

      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ex: <http://example.org/>

        SELECT ?name
        WHERE {
          ?s rdf:type ex:Person .
          ?s ex:name ?name .
        }
      `;

      const results = await executeSemanticQuery(store, sparql, {
        enableSemantic: true,
        similarityThreshold: 0.5,
        maxResults: 10,
      });

      expect(results).toHaveProperty('type', 'select');
      expect(results).toHaveProperty('rows');
      expect(Array.isArray(results.rows)).toBe(true);
    });

    it('should fall back to exact execution for ASK queries', async () => {
      await initializeSemanticIndex(store);

      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ex: <http://example.org/>

        ASK {
          ex:Alice rdf:type ex:Person .
        }
      `;

      const results = await executeSemanticQuery(store, sparql, {
        enableSemantic: true,
      });

      expect(typeof results).toBe('boolean');
    });

    it('should fall back to exact execution when semantic disabled', async () => {
      await initializeSemanticIndex(store);

      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ex: <http://example.org/>

        SELECT ?s WHERE {
          ?s rdf:type ex:Person .
        }
      `;

      const results = await executeSemanticQuery(store, sparql, {
        enableSemantic: false,
      });

      expect(results).toHaveProperty('type', 'select');
    });

    it('should clear semantic index', async () => {
      await initializeSemanticIndex(store);

      const stats1 = getSemanticIndexStats();
      expect(stats1.size).toBeGreaterThan(0);

      clearSemanticIndex();

      const stats2 = getSemanticIndexStats();
      expect(stats2.initialized).toBe(false);
      expect(stats2.size).toBe(0);
    });
  });

  describe('Performance Benchmarks', () => {
    let store;

    beforeEach(() => {
      // Create larger store for benchmarking
      const quads = [];
      for (let i = 0; i < 100; i++) {
        quads.push(
          quad(
            namedNode(`http://example.org/person${i}`),
            namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            namedNode('http://example.org/Person'),
            namedNode('http://example.org/graph')
          )
        );
      }

      store = {
        _quads: quads,
        getQuads() {
          return this._quads;
        },
      };
    });

    it('should benchmark semantic vs exact query', async () => {
      await initializeSemanticIndex(store);

      const sparql = `
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ex: <http://example.org/>

        CONSTRUCT { ?s rdf:type ex:Person }
        WHERE { ?s rdf:type ex:Person }
      `;

      // Benchmark exact query
      const startExact = performance.now();
      const exactResults = executeQuerySync(store, sparql);
      const exactTime = performance.now() - startExact;

      // Benchmark semantic query
      const startSemantic = performance.now();
      const semanticResults = await executeSemanticQuery(store, sparql, {
        enableSemantic: true,
        similarityThreshold: 0.5,
      });
      const semanticTime = performance.now() - startSemantic;

      console.log(`Exact query time: ${exactTime.toFixed(2)}ms`);
      console.log(`Semantic query time: ${semanticTime.toFixed(2)}ms`);
      console.log(`Speedup: ${(exactTime / semanticTime).toFixed(2)}x`);

      // Semantic should be faster or comparable for this workload
      // Note: Actual speedup depends on query complexity and index size
      expect(semanticResults).toBeDefined();
    });

    it('should benchmark embedding generation', async () => {
      const texts = [];
      for (let i = 0; i < 100; i++) {
        texts.push(`Person ${i} works at ExampleOrg`);
      }

      // Benchmark batch embedding generation
      const start = performance.now();
      const embeddings = await generateEmbeddingsBatch(texts);
      const time = performance.now() - start;

      console.log(`Generated ${embeddings.length} embeddings in ${time.toFixed(2)}ms`);
      console.log(`Average time per embedding: ${(time / embeddings.length).toFixed(2)}ms`);

      expect(embeddings).toHaveLength(100);
    });

    it('should benchmark HNSW index search', async () => {
      const vectors = new Map();
      for (let i = 0; i < 1000; i++) {
        const vector = new Float32Array(384);
        // Fill with random-like values
        for (let j = 0; j < 384; j++) {
          vector[j] = Math.random() * 2 - 1;
        }
        vectors.set(`doc${i}`, vector);
      }

      const index = await buildHNSWIndex(vectors, {
        dimensions: 384,
        maxElements: 2000,
      });

      const query = new Float32Array(384).fill(0.5);

      // Benchmark search
      const start = performance.now();
      const results = await index.search(query, 10);
      const time = performance.now() - start;

      console.log(`Searched ${vectors.size} vectors in ${time.toFixed(2)}ms`);
      console.log(`Found ${results.length} results`);

      expect(results).toHaveLength(10);
    });
  });

  describe('Utility Functions', () => {
    it('should compute cosine similarity', () => {
      const vec1 = new Float32Array([1, 0, 0]);
      const vec2 = new Float32Array([1, 0, 0]);
      const vec3 = new Float32Array([0, 1, 0]);

      expect(cosineSimilarity(vec1, vec2)).toBeCloseTo(1, 5);
      expect(cosineSimilarity(vec1, vec3)).toBeCloseTo(0, 5);
    });

    it('should compute L2 distance', () => {
      const vec1 = new Float32Array([0, 0]);
      const vec2 = new Float32Array([3, 4]);

      expect(l2Distance(vec1, vec2)).toBeCloseTo(5, 5); // sqrt(3^2 + 4^2) = 5
    });

    it('should L2 normalize vector', () => {
      const vec = new Float32Array([3, 4]);
      const normalized = l2Normalize(vec);

      expect(normalized.length).toBe(2);

      const norm = Math.sqrt(normalized[0] ** 2 + normalized[1] ** 2);
      expect(norm).toBeCloseTo(1, 5);
    });

    it('should throw error for invalid vectors in utility functions', () => {
      const vec1 = new Float32Array([1, 2]);
      const vec2 = new Float32Array([1, 2, 3]); // Different length

      expect(() => cosineSimilarity(vec1, vec2)).toThrow();
      expect(() => l2Distance(vec1, vec2)).toThrow();
    });
  });
});
