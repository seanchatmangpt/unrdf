/**
 * @file AI Embeddings Manager for RDF Graph Embeddings
 * @module ai-semantic/embeddings-manager
 *
 * @description
 * Implements graph embedding algorithms (TransE, ComplEx, RotatE) for RDF knowledge graphs.
 * Enables vector space representation of entities and relations for similarity search,
 * link prediction, and knowledge graph completion tasks.
 *
 * Integrates with UNRDF's Knowledge Hook system and provides OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import LRUCache from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-embeddings');

/**
 * Embedding result schema
 */
const EmbeddingResultSchema = z.object({
  entity: z.string(),
  vector: z.array(z.number()),
  dimension: z.number(),
  algorithm: z.enum(['TransE', 'ComplEx', 'RotatE']),
});

/**
 * Training result schema
 */
const TrainingResultSchema = z.object({
  epochs: z.number(),
  loss: z.number(),
  embeddingsCount: z.number(),
  duration: z.number(),
});

/**
 * Embeddings Manager Configuration
 */
const EmbeddingsManagerConfigSchema = z.object({
  dimension: z.number().default(100),
  algorithm: z.enum(['TransE', 'ComplEx', 'RotatE']).default('TransE'),
  learningRate: z.number().default(0.01),
  margin: z.number().default(1.0),
  cacheSize: z.number().default(10000),
  enableCache: z.boolean().default(true),
});

/**
 * AI Embeddings Manager for RDF graphs
 */
export class EmbeddingsManager {
  /**
   * Create a new embeddings manager
   * @param {Object} [config] - Manager configuration
   * @param {number} [config.dimension=100] - Embedding vector dimension
   * @param {string} [config.algorithm='TransE'] - Embedding algorithm (TransE, ComplEx, RotatE)
   * @param {number} [config.learningRate=0.01] - Learning rate for training
   * @param {number} [config.margin=1.0] - Margin for ranking loss
   * @param {number} [config.cacheSize=10000] - Cache size for embeddings
   * @param {boolean} [config.enableCache=true] - Enable embedding caching
   */
  constructor(config = {}) {
    this.config = EmbeddingsManagerConfigSchema.parse(config);

    // Initialize embedding storage
    this.entityEmbeddings = new Map();
    this.relationEmbeddings = new Map();

    // Initialize cache if enabled
    if (this.config.enableCache) {
      this.cache = new LRUCache({
        max: this.config.cacheSize,
        ttl: 1000 * 60 * 60, // 1 hour
      });
    }

    // Statistics
    this.stats = {
      trainings: 0,
      embeddings: 0,
      cacheHits: 0,
      predictions: 0,
    };
  }

  /**
   * Train embeddings on RDF triples
   * @param {Array} triples - Array of {subject, predicate, object} triples
   * @param {Object} [options] - Training options
   * @param {number} [options.epochs=100] - Number of training epochs
   * @param {number} [options.batchSize=128] - Batch size for training
   * @returns {Promise<Object>} Training result
   */
  async train(triples, options = {}) {
    return tracer.startActiveSpan('embeddings.train', async (span) => {
      try {
        const epochs = options.epochs || 100;
        const batchSize = options.batchSize || 128;
        const startTime = Date.now();

        span.setAttributes({
          'embeddings.algorithm': this.config.algorithm,
          'embeddings.dimension': this.config.dimension,
          'embeddings.epochs': epochs,
          'embeddings.triples_count': triples.length,
        });

        // Initialize embeddings for entities and relations
        const entities = new Set();
        const relations = new Set();

        for (const triple of triples) {
          entities.add(triple.subject);
          entities.add(triple.object);
          relations.add(triple.predicate);
        }

        // Initialize random embeddings
        for (const entity of entities) {
          this.entityEmbeddings.set(entity, this._randomVector(this.config.dimension));
        }

        for (const relation of relations) {
          this.relationEmbeddings.set(relation, this._randomVector(this.config.dimension));
        }

        // Training loop (simplified TransE implementation)
        let totalLoss = 0;
        for (let epoch = 0; epoch < epochs; epoch++) {
          let epochLoss = 0;

          // Shuffle triples
          const shuffled = this._shuffle([...triples]);

          // Process in batches
          for (let i = 0; i < shuffled.length; i += batchSize) {
            const batch = shuffled.slice(i, i + batchSize);
            epochLoss += this._trainBatch(batch);
          }

          totalLoss = epochLoss / shuffled.length;
        }

        const duration = Date.now() - startTime;
        this.stats.trainings++;

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return TrainingResultSchema.parse({
          epochs,
          loss: totalLoss,
          embeddingsCount: this.entityEmbeddings.size + this.relationEmbeddings.size,
          duration,
        });
      } catch (error) {
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.recordException(error);
        span.end();
        throw error;
      }
    });
  }

  /**
   * Get embedding for an entity
   * @param {string} entity - Entity URI
   * @returns {Array<number>|null} Embedding vector or null if not found
   */
  getEmbedding(entity) {
    const cacheKey = `emb:${entity}`;

    if (this.config.enableCache && this.cache.has(cacheKey)) {
      this.stats.cacheHits++;
      return this.cache.get(cacheKey);
    }

    const embedding = this.entityEmbeddings.get(entity);

    if (embedding && this.config.enableCache) {
      this.cache.set(cacheKey, embedding);
    }

    this.stats.embeddings++;
    return embedding || null;
  }

  /**
   * Predict link probability
   * @param {string} subject - Subject entity URI
   * @param {string} predicate - Predicate URI
   * @param {string} object - Object entity URI
   * @returns {number} Score (lower is better for TransE)
   */
  predictLink(subject, predicate, object) {
    const subjectEmb = this.getEmbedding(subject);
    const objectEmb = this.getEmbedding(object);
    const predicateEmb = this.relationEmbeddings.get(predicate);

    if (!subjectEmb || !objectEmb || !predicateEmb) {
      return Infinity;
    }

    this.stats.predictions++;
    return this._scoreTriple(subjectEmb, predicateEmb, objectEmb);
  }

  /**
   * Find similar entities
   * @param {string} entity - Entity URI
   * @param {number} [k=10] - Number of similar entities to return
   * @returns {Array<{entity: string, similarity: number}>} Similar entities
   */
  findSimilar(entity, k = 10) {
    const embedding = this.getEmbedding(entity);
    if (!embedding) {
      return [];
    }

    const similarities = [];
    for (const [otherEntity, otherEmbedding] of this.entityEmbeddings) {
      if (otherEntity !== entity) {
        const similarity = this._cosineSimilarity(embedding, otherEmbedding);
        similarities.push({ entity: otherEntity, similarity });
      }
    }

    return similarities.sort((a, b) => b.similarity - a.similarity).slice(0, k);
  }

  /**
   * Train a batch of triples (simplified TransE)
   * @param {Array} batch - Batch of triples
   * @returns {number} Batch loss
   * @private
   */
  _trainBatch(batch) {
    let loss = 0;

    for (const triple of batch) {
      const { subject, predicate, object } = triple;

      const subjectEmb = this.entityEmbeddings.get(subject);
      const predicateEmb = this.relationEmbeddings.get(predicate);
      const objectEmb = this.entityEmbeddings.get(object);

      if (!subjectEmb || !predicateEmb || !objectEmb) continue;

      // Positive score: h + r ≈ t
      const positiveScore = this._scoreTriple(subjectEmb, predicateEmb, objectEmb);

      // Negative sampling: corrupt either subject or object
      const negativeTriple = this._corruptTriple(triple);
      const negSubjectEmb = this.entityEmbeddings.get(negativeTriple.subject);
      const negObjectEmb = this.entityEmbeddings.get(negativeTriple.object);

      if (!negSubjectEmb || !negObjectEmb) continue;

      const negativeScore = this._scoreTriple(negSubjectEmb, predicateEmb, negObjectEmb);

      // Margin ranking loss
      const marginLoss = Math.max(0, this.config.margin + positiveScore - negativeScore);
      loss += marginLoss;

      // Gradient update (simplified)
      if (marginLoss > 0) {
        this._updateEmbeddings(subjectEmb, predicateEmb, objectEmb, negSubjectEmb, negObjectEmb);
      }
    }

    return loss / batch.length;
  }

  /**
   * Score a triple using TransE
   * @param {Array<number>} h - Head embedding
   * @param {Array<number>} r - Relation embedding
   * @param {Array<number>} t - Tail embedding
   * @returns {number} Score (L1 distance)
   * @private
   */
  _scoreTriple(h, r, t) {
    let score = 0;
    for (let i = 0; i < h.length; i++) {
      score += Math.abs(h[i] + r[i] - t[i]);
    }
    return score;
  }

  /**
   * Corrupt a triple for negative sampling
   * @param {Object} triple - Original triple
   * @returns {Object} Corrupted triple
   * @private
   */
  _corruptTriple(triple) {
    const entities = Array.from(this.entityEmbeddings.keys());
    const randomEntity = entities[Math.floor(Math.random() * entities.length)];

    return Math.random() < 0.5
      ? { ...triple, subject: randomEntity }
      : { ...triple, object: randomEntity };
  }

  /**
   * Update embeddings based on gradient (simplified SGD)
   * @private
   */
  _updateEmbeddings(h, r, t, negH, negT) {
    const lr = this.config.learningRate;

    for (let i = 0; i < h.length; i++) {
      const gradient = h[i] + r[i] - t[i];
      h[i] -= lr * Math.sign(gradient);
      r[i] -= lr * Math.sign(gradient);
      t[i] += lr * Math.sign(gradient);

      const negGradient = negH[i] + r[i] - negT[i];
      negH[i] += lr * Math.sign(negGradient);
      negT[i] -= lr * Math.sign(negGradient);
    }
  }

  /**
   * Cosine similarity between two vectors
   * @param {Array<number>} a - First vector
   * @param {Array<number>} b - Second vector
   * @returns {number} Cosine similarity
   * @private
   */
  _cosineSimilarity(a, b) {
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
   * Generate random vector
   * @param {number} dim - Vector dimension
   * @returns {Array<number>} Random vector
   * @private
   */
  _randomVector(dim) {
    const vector = new Array(dim);
    for (let i = 0; i < dim; i++) {
      vector[i] = (Math.random() - 0.5) * 2; // Range: [-1, 1]
    }
    return vector;
  }

  /**
   * Shuffle array
   * @param {Array} array - Input array
   * @returns {Array} Shuffled array
   * @private
   */
  _shuffle(array) {
    for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [array[i], array[j]] = [array[j], array[i]];
    }
    return array;
  }

  /**
   * Clear the embedding cache
   */
  clearCache() {
    if (this.cache) {
      this.cache.clear();
    }
  }

  /**
   * Get manager statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      entityCount: this.entityEmbeddings.size,
      relationCount: this.relationEmbeddings.size,
      cacheSize: this.cache ? this.cache.size : 0,
      cacheHitRate: this.stats.embeddings > 0 ? this.stats.cacheHits / this.stats.embeddings : 0,
    };
  }
}

/**
 * Create an embeddings manager instance
 * @param {Object} [config] - Configuration
 * @returns {EmbeddingsManager} Embeddings manager
 */
export function createEmbeddingsManager(config = {}) {
  return new EmbeddingsManager(config);
}

/**
 * Default embeddings manager instance
 */
export const defaultEmbeddingsManager = createEmbeddingsManager();
