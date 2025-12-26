/**
 * @file Semantic Search
 * @description Vector-based semantic search over RDF knowledge graphs
 * @module knowledge-rag/semantic-search
 */

import { trace } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('semantic-search');

/**
 * Embedding schema
 */
const EmbeddingSchema = z.object({
  text: z.string(),
  vector: z.array(z.number()),
  metadata: z.record(z.any()).optional(),
});

/**
 * Semantic Search Engine
 *
 * Features:
 * - Vector embeddings for RDF entities
 * - Cosine similarity search
 * - Hybrid search (semantic + keyword)
 * - Caching for performance
 *
 * @class
 */
export class SemanticSearch {
  /**
   * @param {object} config - Configuration
   * @param {object} config.embeddingModel - Embedding model config
   * @param {number} config.dimensions - Embedding dimensions
   */
  constructor(config = {}) {
    this.config = {
      embeddingModel: 'text-embedding-ada-002',
      dimensions: 1536,
      cacheSize: 10000,
      ...config,
    };

    this.embeddings = new Map();
    this.cache = new Map();
    this.cacheHits = 0;
    this.cacheMisses = 0;
  }

  /**
   * Generate embedding for text
   *
   * @param {string} text - Input text
   * @returns {Promise<number[]>} Embedding vector
   */
  async generateEmbedding(text) {
    return tracer.startActiveSpan('semantic.generateEmbedding', async (span) => {
      try {
        span.setAttribute('text.length', text.length);

        // Check cache
        if (this.cache.has(text)) {
          this.cacheHits++;
          span.setAttribute('cache.hit', true);
          return this.cache.get(text);
        }

        this.cacheMisses++;
        span.setAttribute('cache.hit', false);

        // In production, call embedding API
        // For now, generate random embedding
        const embedding = this._generateRandomEmbedding(this.config.dimensions);

        // Cache result
        if (this.cache.size < this.config.cacheSize) {
          this.cache.set(text, embedding);
        }

        span.setStatus({ code: 1 });
        return embedding;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Index RDF entity with embedding
   *
   * @param {string} entityUri - Entity URI
   * @param {string} text - Entity text representation
   * @param {object} metadata - Additional metadata
   * @returns {Promise<void>}
   */
  async indexEntity(entityUri, text, metadata = {}) {
    return tracer.startActiveSpan('semantic.indexEntity', async (span) => {
      try {
        span.setAttribute('entity.uri', entityUri);

        const vector = await this.generateEmbedding(text);

        this.embeddings.set(entityUri, {
          text,
          vector,
          metadata: {
            ...metadata,
            indexedAt: Date.now(),
          },
        });

        span.setAttribute('index.size', this.embeddings.size);
        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Search for similar entities
   *
   * @param {string} query - Search query
   * @param {number} topK - Number of results
   * @param {number} threshold - Similarity threshold (0-1)
   * @returns {Promise<object[]>} Search results
   */
  async search(query, topK = 10, threshold = 0.7) {
    return tracer.startActiveSpan('semantic.search', async (span) => {
      try {
        span.setAttribute('query', query);
        span.setAttribute('topK', topK);
        span.setAttribute('threshold', threshold);

        // Generate query embedding
        const queryVector = await this.generateEmbedding(query);

        // Calculate similarities
        const results = [];

        for (const [uri, embedding] of this.embeddings.entries()) {
          const similarity = this._cosineSimilarity(queryVector, embedding.vector);

          if (similarity >= threshold) {
            results.push({
              uri,
              text: embedding.text,
              similarity,
              metadata: embedding.metadata,
            });
          }
        }

        // Sort by similarity (descending)
        results.sort((a, b) => b.similarity - a.similarity);

        // Return top K
        const topResults = results.slice(0, topK);

        span.setAttribute('results.total', results.length);
        span.setAttribute('results.returned', topResults.length);
        span.setStatus({ code: 1 });

        return topResults;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Hybrid search (semantic + keyword)
   *
   * @param {string} query - Search query
   * @param {object[]} keywordResults - Keyword search results
   * @param {number} topK - Number of results
   * @param {number} alpha - Weight for semantic (0-1)
   * @returns {Promise<object[]>} Combined results
   */
  async hybridSearch(query, keywordResults, topK = 10, alpha = 0.7) {
    return tracer.startActiveSpan('semantic.hybridSearch', async (span) => {
      try {
        span.setAttribute('alpha', alpha);

        // Get semantic results
        const semanticResults = await this.search(query, topK * 2);

        // Combine and re-rank
        const combined = new Map();

        // Add semantic results
        for (const result of semanticResults) {
          combined.set(result.uri, {
            ...result,
            semanticScore: result.similarity,
            keywordScore: 0,
            combinedScore: result.similarity * alpha,
          });
        }

        // Add keyword results
        for (const result of keywordResults) {
          const existing = combined.get(result.uri);
          const keywordScore = result.score || 1.0;

          if (existing) {
            existing.keywordScore = keywordScore;
            existing.combinedScore = existing.semanticScore * alpha + keywordScore * (1 - alpha);
          } else {
            combined.set(result.uri, {
              ...result,
              semanticScore: 0,
              keywordScore,
              combinedScore: keywordScore * (1 - alpha),
            });
          }
        }

        // Sort by combined score
        const results = Array.from(combined.values()).sort(
          (a, b) => b.combinedScore - a.combinedScore
        );

        const topResults = results.slice(0, topK);

        span.setAttribute('results.semantic', semanticResults.length);
        span.setAttribute('results.keyword', keywordResults.length);
        span.setAttribute('results.combined', topResults.length);
        span.setStatus({ code: 1 });

        return topResults;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Calculate cosine similarity between vectors
   *
   * @private
   * @param {number[]} a - Vector A
   * @param {number[]} b - Vector B
   * @returns {number} Similarity (0-1)
   */
  _cosineSimilarity(a, b) {
    if (a.length !== b.length) {
      throw new Error('Vectors must have same dimensions');
    }

    let dotProduct = 0;
    let normA = 0;
    let normB = 0;

    for (let i = 0; i < a.length; i++) {
      dotProduct += a[i] * b[i];
      normA += a[i] * a[i];
      normB += b[i] * b[i];
    }

    return dotProduct / (Math.sqrt(normA) * Math.sqrt(normB));
  }

  /**
   * Generate random embedding (placeholder)
   *
   * @private
   * @param {number} dimensions - Vector dimensions
   * @returns {number[]} Random embedding
   */
  _generateRandomEmbedding(dimensions) {
    const vector = new Array(dimensions);
    for (let i = 0; i < dimensions; i++) {
      vector[i] = Math.random() * 2 - 1; // Range: -1 to 1
    }

    // Normalize
    const norm = Math.sqrt(vector.reduce((sum, val) => sum + val * val, 0));
    return vector.map((val) => val / norm);
  }

  /**
   * Get cache statistics
   *
   * @returns {object} Cache stats
   */
  getCacheStats() {
    const total = this.cacheHits + this.cacheMisses;
    return {
      size: this.cache.size,
      hits: this.cacheHits,
      misses: this.cacheMisses,
      hitRate: total > 0 ? this.cacheHits / total : 0,
    };
  }

  /**
   * Clear cache
   *
   * @returns {void}
   */
  clearCache() {
    this.cache.clear();
    this.cacheHits = 0;
    this.cacheMisses = 0;
  }
}

export default SemanticSearch;
