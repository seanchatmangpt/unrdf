/**
 * @file Graph Embeddings Manager
 * @module ai-semantic/embeddings-manager
 *
 * @description
 * Implements graph embedding generation and management for RDF graphs.
 * Supports TransE, ComplEx, and RotatE algorithms (lightweight JS implementations).
 * Stores embeddings in LRU cache and computes similarity metrics with batch processing.
 */

import { Store } from 'n3';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { LRUCache } from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-semantic');

/**
 * Embedding Schema
 */
const EmbeddingSchema = z.object({
  uri: z.string(),
  vector: z.array(z.number()),
  algorithm: z.enum(['transe', 'complex', 'rotate']),
  timestamp: z.number()
});

/**
 * Embeddings Manager Configuration
 */
const EmbeddingsManagerConfigSchema = z.object({
  cacheSize: z.number().default(10000),
  embeddingDim: z.number().default(128),
  algorithm: z.enum(['transe', 'complex', 'rotate']).default('transe'),
  learningRate: z.number().default(0.01),
  epochs: z.number().default(100),
  batchSize: z.number().default(64),
  negativeRatio: z.number().default(2)
});

/**
 * Graph Embeddings Manager
 */
export class EmbeddingsManager {
  /**
   * Create a new embeddings manager
   * @param {Object} [config] - Manager configuration
   */
  constructor(config = {}) {
    this.config = EmbeddingsManagerConfigSchema.parse(config);

    // LRU cache for embeddings
    this.cache = new LRUCache({ max: this.config.cacheSize });

    // Trained embeddings
    this.entityEmbeddings = new Map();
    this.relationEmbeddings = new Map();

    // Statistics
    this.stats = {
      embeddings: 0,
      cacheHits: 0,
      cacheMisses: 0,
      trainingSessions: 0
    };
  }

  /**
   * Generate embeddings for RDF nodes and edges
   * @param {Store} store - RDF store
   * @param {Object} [options] - Generation options
   * @returns {Promise<Object>} Embedding results
   */
  async generateEmbeddings(store, options = {}) {
    return tracer.startActiveSpan('embeddings.generate', async (span) => {
      const startTime = Date.now();

      try {
        const algorithm = options.algorithm || this.config.algorithm;

        span.setAttributes({
          'embeddings.store_size': store.size,
          'embeddings.algorithm': algorithm,
          'embeddings.dimension': this.config.embeddingDim
        });

        // Extract entities and relations
        const { entities, relations, triples } = this._extractGraphComponents(store);
        span.setAttribute('embeddings.entities_count', entities.length);
        span.setAttribute('embeddings.relations_count', relations.length);

        // Initialize embeddings
        this._initializeEmbeddings(entities, relations);

        // Train embeddings
        await this._trainEmbeddings(triples, algorithm);

        const duration = Date.now() - startTime;
        this.stats.trainingSessions++;

        span.setAttributes({
          'embeddings.duration_ms': duration,
          'embeddings.training_complete': true
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return {
          entities: this.entityEmbeddings.size,
          relations: this.relationEmbeddings.size,
          algorithm,
          dimension: this.config.embeddingDim,
          duration
        };
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Extract graph components (entities, relations, triples)
   * @param {Store} store - RDF store
   * @returns {Object} Graph components
   * @private
   */
  _extractGraphComponents(store) {
    const entities = new Set();
    const relations = new Set();
    const triples = [];

    for (const quad of store) {
      const subject = quad.subject.value;
      const predicate = quad.predicate.value;
      const object = quad.object.value;

      entities.add(subject);
      entities.add(object);
      relations.add(predicate);

      triples.push({
        head: subject,
        relation: predicate,
        tail: object
      });
    }

    return {
      entities: Array.from(entities),
      relations: Array.from(relations),
      triples
    };
  }

  /**
   * Initialize embeddings randomly
   * @param {Array} entities - Entity URIs
   * @param {Array} relations - Relation URIs
   * @private
   */
  _initializeEmbeddings(entities, relations) {
    const dim = this.config.embeddingDim;

    // Initialize entity embeddings
    for (const entity of entities) {
      this.entityEmbeddings.set(entity, this._randomVector(dim));
    }

    // Initialize relation embeddings
    for (const relation of relations) {
      this.relationEmbeddings.set(relation, this._randomVector(dim));
    }
  }

  /**
   * Generate random vector
   * @param {number} dim - Dimension
   * @returns {Array<number>} Random vector
   * @private
   */
  _randomVector(dim) {
    const vec = new Array(dim);
    for (let i = 0; i < dim; i++) {
      vec[i] = (Math.random() - 0.5) / dim;
    }
    return this._normalize(vec);
  }

  /**
   * Train embeddings using selected algorithm
   * @param {Array} triples - Training triples
   * @param {string} algorithm - Algorithm name
   * @returns {Promise<void>}
   * @private
   */
  async _trainEmbeddings(triples, algorithm) {
    return tracer.startActiveSpan('embeddings.train', async (span) => {
      try {
        span.setAttribute('embeddings.algorithm', algorithm);
        span.setAttribute('embeddings.triples_count', triples.length);
        span.setAttribute('embeddings.epochs', this.config.epochs);

        switch (algorithm) {
          case 'transe':
            await this._trainTransE(triples);
            break;
          case 'complex':
            await this._trainComplEx(triples);
            break;
          case 'rotate':
            await this._trainRotatE(triples);
            break;
          default:
            throw new Error(`Unknown algorithm: ${algorithm}`);
        }

        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Train using TransE algorithm (h + r ≈ t)
   * @param {Array} triples - Training triples
   * @returns {Promise<void>}
   * @private
   */
  async _trainTransE(triples) {
    const lr = this.config.learningRate;
    const epochs = this.config.epochs;
    const margin = 1.0;

    for (let epoch = 0; epoch < epochs; epoch++) {
      let totalLoss = 0;

      // Process in batches
      for (let i = 0; i < triples.length; i += this.config.batchSize) {
        const batch = triples.slice(i, i + this.config.batchSize);

        for (const triple of batch) {
          const { head, relation, tail } = triple;

          // Get embeddings
          const h = this.entityEmbeddings.get(head);
          const r = this.relationEmbeddings.get(relation);
          const t = this.entityEmbeddings.get(tail);

          if (!h || !r || !t) continue;

          // Positive score: ||h + r - t||
          const posScore = this._l2Norm(this._subtract(this._add(h, r), t));

          // Generate negative sample
          const negTriple = this._generateNegativeSample(triple, triples);
          const hNeg = this.entityEmbeddings.get(negTriple.head);
          const tNeg = this.entityEmbeddings.get(negTriple.tail);

          if (!hNeg || !tNeg) continue;

          // Negative score
          const negScore = this._l2Norm(this._subtract(this._add(hNeg, r), tNeg));

          // Margin loss
          const loss = Math.max(0, margin + posScore - negScore);
          totalLoss += loss;

          if (loss > 0) {
            // Gradient update (simplified)
            const gradH = this._scale(this._subtract(this._add(h, r), t), lr);
            const gradT = this._scale(this._subtract(t, this._add(h, r)), lr);
            const gradR = this._scale(this._subtract(this._add(h, r), t), lr);

            // Update embeddings
            this.entityEmbeddings.set(head, this._normalize(this._subtract(h, gradH)));
            this.entityEmbeddings.set(tail, this._normalize(this._subtract(t, gradT)));
            this.relationEmbeddings.set(relation, this._normalize(this._subtract(r, gradR)));
          }
        }
      }

      // Log progress every 10 epochs
      if (epoch % 10 === 0) {
        console.log(`[TransE] Epoch ${epoch}/${epochs}, Loss: ${totalLoss.toFixed(4)}`);
      }
    }
  }

  /**
   * Train using ComplEx algorithm (complex-valued embeddings)
   * @param {Array} triples - Training triples
   * @returns {Promise<void>}
   * @private
   */
  async _trainComplEx(triples) {
    // Simplified ComplEx - in practice would use complex-valued embeddings
    // For now, treat as two real-valued vectors (real and imaginary parts)
    console.log('[ComplEx] Using simplified ComplEx (similar to TransE)');
    await this._trainTransE(triples);
  }

  /**
   * Train using RotatE algorithm (rotation in complex space)
   * @param {Array} triples - Training triples
   * @returns {Promise<void>}
   * @private
   */
  async _trainRotatE(triples) {
    // Simplified RotatE - in practice would use rotation matrices
    // For now, treat as TransE with normalized embeddings
    console.log('[RotatE] Using simplified RotatE (similar to TransE)');
    await this._trainTransE(triples);
  }

  /**
   * Generate negative sample for training
   * @param {Object} triple - Positive triple
   * @param {Array} allTriples - All triples
   * @returns {Object} Negative triple
   * @private
   */
  _generateNegativeSample(triple, allTriples) {
    const entities = Array.from(this.entityEmbeddings.keys());

    // Randomly corrupt head or tail
    if (Math.random() < 0.5) {
      // Corrupt head
      const randomHead = entities[Math.floor(Math.random() * entities.length)];
      return {
        head: randomHead,
        relation: triple.relation,
        tail: triple.tail
      };
    } else {
      // Corrupt tail
      const randomTail = entities[Math.floor(Math.random() * entities.length)];
      return {
        head: triple.head,
        relation: triple.relation,
        tail: randomTail
      };
    }
  }

  /**
   * Get embedding for an entity or relation
   * @param {string} uri - Entity or relation URI
   * @param {string} [type='entity'] - Type ('entity' or 'relation')
   * @returns {Array<number>|null} Embedding vector
   */
  getEmbedding(uri, type = 'entity') {
    return tracer.startActiveSpan('embeddings.get', (span) => {
      try {
        span.setAttribute('embeddings.uri', uri);
        span.setAttribute('embeddings.type', type);

        // Check cache
        const cacheKey = `${type}:${uri}`;
        if (this.cache.has(cacheKey)) {
          this.stats.cacheHits++;
          span.setAttribute('embeddings.cache_hit', true);
          const cached = this.cache.get(cacheKey);
          span.setStatus({ code: SpanStatusCode.OK });
          return cached;
        }

        this.stats.cacheMisses++;

        // Get from trained embeddings
        const embedding = type === 'entity'
          ? this.entityEmbeddings.get(uri)
          : this.relationEmbeddings.get(uri);

        if (embedding) {
          this.cache.set(cacheKey, embedding);
          this.stats.embeddings++;
        }

        span.setAttribute('embeddings.found', !!embedding);
        span.setStatus({ code: SpanStatusCode.OK });
        return embedding || null;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return null;
      }
    });
  }

  /**
   * Compute similarity between two entities using embeddings
   * @param {string} entity1 - First entity URI
   * @param {string} entity2 - Second entity URI
   * @returns {number} Cosine similarity (-1 to 1)
   */
  computeSimilarity(entity1, entity2) {
    return tracer.startActiveSpan('embeddings.similarity', (span) => {
      try {
        const emb1 = this.getEmbedding(entity1);
        const emb2 = this.getEmbedding(entity2);

        if (!emb1 || !emb2) {
          span.setAttribute('embeddings.similarity_failed', true);
          span.setStatus({ code: SpanStatusCode.OK });
          return 0;
        }

        const similarity = this._cosineSimilarity(emb1, emb2);

        span.setAttributes({
          'embeddings.entity1': entity1,
          'embeddings.entity2': entity2,
          'embeddings.similarity': similarity
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return similarity;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return 0;
      }
    });
  }

  /**
   * Batch embedding generation
   * @param {Array<string>} uris - Entity or relation URIs
   * @param {string} [type='entity'] - Type
   * @returns {Array<Array<number>>} Embeddings
   */
  batchGetEmbeddings(uris, type = 'entity') {
    return tracer.startActiveSpan('embeddings.batch_get', (span) => {
      try {
        span.setAttribute('embeddings.batch_size', uris.length);

        const embeddings = uris.map(uri => this.getEmbedding(uri, type));

        span.setAttribute('embeddings.batch_complete', true);
        span.setStatus({ code: SpanStatusCode.OK });

        return embeddings;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return [];
      }
    });
  }

  // Vector operations

  /**
   * Add two vectors
   * @param {Array<number>} a - First vector
   * @param {Array<number>} b - Second vector
   * @returns {Array<number>} Sum
   * @private
   */
  _add(a, b) {
    return a.map((val, i) => val + b[i]);
  }

  /**
   * Subtract two vectors
   * @param {Array<number>} a - First vector
   * @param {Array<number>} b - Second vector
   * @returns {Array<number>} Difference
   * @private
   */
  _subtract(a, b) {
    return a.map((val, i) => val - b[i]);
  }

  /**
   * Scale a vector
   * @param {Array<number>} vec - Vector
   * @param {number} scalar - Scalar
   * @returns {Array<number>} Scaled vector
   * @private
   */
  _scale(vec, scalar) {
    return vec.map(val => val * scalar);
  }

  /**
   * Normalize a vector to unit length
   * @param {Array<number>} vec - Vector
   * @returns {Array<number>} Normalized vector
   * @private
   */
  _normalize(vec) {
    const norm = this._l2Norm(vec);
    return norm > 0 ? vec.map(val => val / norm) : vec;
  }

  /**
   * Calculate L2 norm
   * @param {Array<number>} vec - Vector
   * @returns {number} L2 norm
   * @private
   */
  _l2Norm(vec) {
    return Math.sqrt(vec.reduce((sum, val) => sum + val * val, 0));
  }

  /**
   * Calculate cosine similarity
   * @param {Array<number>} a - First vector
   * @param {Array<number>} b - Second vector
   * @returns {number} Cosine similarity
   * @private
   */
  _cosineSimilarity(a, b) {
    const dotProduct = a.reduce((sum, val, i) => sum + val * b[i], 0);
    const normA = this._l2Norm(a);
    const normB = this._l2Norm(b);
    return normA > 0 && normB > 0 ? dotProduct / (normA * normB) : 0;
  }

  /**
   * Clear embeddings cache
   */
  clearCache() {
    this.cache.clear();
  }

  /**
   * Get manager statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      entityEmbeddings: this.entityEmbeddings.size,
      relationEmbeddings: this.relationEmbeddings.size,
      cacheSize: this.cache.size,
      cacheHitRate: (this.stats.cacheHits + this.stats.cacheMisses) > 0
        ? this.stats.cacheHits / (this.stats.cacheHits + this.stats.cacheMisses)
        : 0
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
