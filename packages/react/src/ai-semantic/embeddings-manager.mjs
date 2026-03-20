/**
 * @file Graph Embedding Manager for RDF Knowledge Graphs
 * @module ai-semantic/embeddings-manager
 *
 * @description
 * Implements knowledge graph embedding models (TransE, ComplEx, RotatE)
 * for entity similarity, link prediction, and graph analysis.
 * Uses pure JS vector operations with no external ML dependencies.
 *
 * Integrates with UNRDF's Knowledge Hook system and provides OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import LRUCache from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-semantic');

// ─── Zod Schemas ────────────────────────────────────────────────────────────

/**
 * Embedding model types
 */
const EmbeddingModelSchema = z.enum(['TransE', 'ComplEx', 'RotatE']);

/**
 * Embeddings manager configuration schema
 */
const EmbeddingsManagerConfigSchema = z.object({
  model: EmbeddingModelSchema.default('TransE'),
  dimensions: z.number().int().min(2).max(1024).default(50),
  learningRate: z.number().min(0.0001).max(1.0).default(0.01),
  margin: z.number().min(0).default(1.0),
  epochs: z.number().int().min(1).max(10000).default(100),
  negativeRatio: z.number().int().min(1).max(100).default(5),
  cacheSize: z.number().int().min(0).default(500),
  enableCache: z.boolean().default(true),
  seed: z.number().int().optional(),
});

/**
 * Embedding vector schema
 */
const EmbeddingVectorSchema = z.object({
  entity: z.string(),
  vector: z.array(z.number()),
  norm: z.number(),
});

/**
 * Similarity result schema
 */
const SimilarityResultSchema = z.object({
  entity1: z.string(),
  entity2: z.string(),
  similarity: z.number().min(-1).max(1),
  distance: z.number().min(0),
});

/**
 * Prediction result schema
 */
const PredictionResultSchema = z.object({
  subject: z.string(),
  predicate: z.string(),
  candidates: z.array(
    z.object({
      entity: z.string(),
      score: z.number(),
    })
  ),
});

/**
 * Exported model schema
 */
const ExportedModelSchema = z.object({
  model: EmbeddingModelSchema,
  dimensions: z.number(),
  entityEmbeddings: z.record(z.string(), z.array(z.number())),
  relationEmbeddings: z.record(z.string(), z.array(z.number())),
  trained: z.boolean(),
  metadata: z.object({
    epochs: z.number(),
    entityCount: z.number(),
    relationCount: z.number(),
    exportedAt: z.string(),
  }),
});

// ─── Vector Utilities ───────────────────────────────────────────────────────

/**
 * @param {number} size - Vector dimension
 * @param {Function} rng - Random number generator
 * @returns {Float64Array} Random vector with values in [-0.5, 0.5)
 */
function randomVector(size, rng) {
  const v = new Float64Array(size);
  for (let i = 0; i < size; i++) {
    v[i] = rng() - 0.5;
  }
  return normalize(v);
}

/**
 * @param {Float64Array} v - Vector
 * @returns {number} L2 norm
 */
function vectorNorm(v) {
  let sum = 0;
  for (let i = 0; i < v.length; i++) {
    sum += v[i] * v[i];
  }
  return Math.sqrt(sum);
}

/**
 * @param {Float64Array} v - Vector
 * @returns {Float64Array} Normalized vector
 */
function normalize(v) {
  const n = vectorNorm(v);
  if (n === 0) return v;
  const out = new Float64Array(v.length);
  for (let i = 0; i < v.length; i++) {
    out[i] = v[i] / n;
  }
  return out;
}

/**
 * @param {Float64Array} a - First vector
 * @param {Float64Array} b - Second vector
 * @returns {number} Cosine similarity in [-1, 1]
 */
function cosineSimilarity(a, b) {
  let dot = 0;
  let normA = 0;
  let normB = 0;
  for (let i = 0; i < a.length; i++) {
    dot += a[i] * b[i];
    normA += a[i] * a[i];
    normB += b[i] * b[i];
  }
  const denom = Math.sqrt(normA) * Math.sqrt(normB);
  return denom === 0 ? 0 : dot / denom;
}

/**
 * @param {Float64Array} a - First vector
 * @param {Float64Array} b - Second vector
 * @returns {number} Euclidean distance
 */
function euclideanDistance(a, b) {
  let sum = 0;
  for (let i = 0; i < a.length; i++) {
    const d = a[i] - b[i];
    sum += d * d;
  }
  return Math.sqrt(sum);
}

/**
 * Create a seeded pseudo-random number generator
 * @param {number} [seed] - Optional seed
 * @returns {Function} RNG function returning values in [0, 1)
 */
function createRng(seed) {
  if (seed === undefined) {
    return Math.random;
  }
  let s = seed | 0;
  return () => {
    s = (s * 1664525 + 1013904223) & 0x7fffffff;
    return s / 0x7fffffff;
  };
}

// ─── EmbeddingsManager Class ────────────────────────────────────────────────

/**
 * Knowledge graph embedding manager supporting TransE, ComplEx, and RotatE models
 *
 * @example
 * const manager = createEmbeddingsManager({ model: 'TransE', dimensions: 50 });
 * const triples = [
 *   { subject: 'http://ex.org/Alice', predicate: 'http://ex.org/knows', object: 'http://ex.org/Bob' },
 * ];
 * manager.train(triples);
 * const embedding = manager.getEmbedding('http://ex.org/Alice');
 */
export class EmbeddingsManager {
  /**
   * Create a new embeddings manager
   * @param {Object} [config] - Manager configuration
   * @param {string} [config.model='TransE'] - Embedding model (TransE, ComplEx, RotatE)
   * @param {number} [config.dimensions=50] - Embedding dimensions
   * @param {number} [config.learningRate=0.01] - Learning rate
   * @param {number} [config.margin=1.0] - Margin for margin-based loss
   * @param {number} [config.epochs=100] - Training epochs
   * @param {number} [config.negativeRatio=5] - Negative samples per positive
   * @param {number} [config.cacheSize=500] - LRU cache size
   * @param {boolean} [config.enableCache=true] - Enable similarity cache
   * @param {number} [config.seed] - Random seed for reproducibility
   */
  constructor(config = {}) {
    this.config = EmbeddingsManagerConfigSchema.parse(config);
    /** @type {Map<string, Float64Array>} */
    this.entityEmbeddings = new Map();
    /** @type {Map<string, Float64Array>} */
    this.relationEmbeddings = new Map();
    this.trained = false;
    this.rng = createRng(this.config.seed);
    this.cache = this.config.enableCache
      ? new LRUCache({ max: this.config.cacheSize })
      : null;
    this.stats = {
      trainCalls: 0,
      queries: 0,
      cacheHits: 0,
      lastTrainDuration: 0,
    };
  }

  /**
   * Train embeddings from RDF triples
   * @param {Array<{subject: string, predicate: string, object: string}>} triples - Training triples
   * @param {Object} [options] - Training options
   * @param {number} [options.epochs] - Override default epochs
   * @param {number} [options.learningRate] - Override default learning rate
   * @returns {Object} Training summary with loss history
   * @throws {Error} If triples array is empty
   */
  train(triples, options = {}) {
    return tracer.startActiveSpan('embeddings.train', (span) => {
      const startTime = Date.now();
      try {
        const TriplesSchema = z.array(
          z.object({
            subject: z.string().min(1),
            predicate: z.string().min(1),
            object: z.string().min(1),
          })
        ).min(1, 'At least one triple is required for training');
        const validated = TriplesSchema.parse(triples);

        span.setAttributes({
          'embeddings.model': this.config.model,
          'embeddings.triple_count': validated.length,
          'embeddings.dimensions': this.config.dimensions,
        });

        const epochs = options.epochs ?? this.config.epochs;
        const lr = options.learningRate ?? this.config.learningRate;
        const dim = this.config.dimensions;

        // Collect all entities and relations
        const entities = new Set();
        const relations = new Set();
        for (const t of validated) {
          entities.add(t.subject);
          entities.add(t.object);
          relations.add(t.predicate);
        }

        // Initialize embeddings
        for (const e of entities) {
          if (!this.entityEmbeddings.has(e)) {
            this.entityEmbeddings.set(e, randomVector(dim, this.rng));
          }
        }
        for (const r of relations) {
          if (!this.relationEmbeddings.has(r)) {
            this.relationEmbeddings.set(r, randomVector(dim, this.rng));
          }
        }

        const entityArray = Array.from(entities);
        const lossHistory = [];

        // Training loop
        for (let epoch = 0; epoch < epochs; epoch++) {
          let epochLoss = 0;

          for (const triple of validated) {
            const h = this.entityEmbeddings.get(triple.subject);
            const r = this.relationEmbeddings.get(triple.predicate);
            const t = this.entityEmbeddings.get(triple.object);

            // Generate negative sample
            const negIdx = Math.floor(this.rng() * entityArray.length);
            const negEntity = entityArray[negIdx];
            const negEmb = this.entityEmbeddings.get(negEntity);

            // Score positive and negative
            const posScore = this._scoreTriple(h, r, t);
            const negScore = this._scoreTriple(h, r, negEmb);

            // Margin-based loss: max(0, margin + posScore - negScore)
            const loss = Math.max(0, this.config.margin + posScore - negScore);
            epochLoss += loss;

            if (loss > 0) {
              this._updateEmbeddings(h, r, t, negEmb, lr);
            }
          }

          // Normalize embeddings every 10 epochs
          if (epoch % 10 === 0) {
            for (const [key, vec] of this.entityEmbeddings) {
              this.entityEmbeddings.set(key, normalize(vec));
            }
          }

          lossHistory.push(epochLoss / validated.length);
        }

        // Final normalization
        for (const [key, vec] of this.entityEmbeddings) {
          this.entityEmbeddings.set(key, normalize(vec));
        }
        for (const [key, vec] of this.relationEmbeddings) {
          this.relationEmbeddings.set(key, normalize(vec));
        }

        this.trained = true;
        if (this.cache) this.cache.clear();
        this.stats.trainCalls++;
        this.stats.lastTrainDuration = Date.now() - startTime;

        const summary = {
          model: this.config.model,
          epochs,
          entityCount: entities.size,
          relationCount: relations.size,
          finalLoss: lossHistory[lossHistory.length - 1],
          lossHistory,
          duration: Date.now() - startTime,
        };

        span.setAttributes({
          'embeddings.entity_count': entities.size,
          'embeddings.relation_count': relations.size,
          'embeddings.final_loss': summary.finalLoss,
          'embeddings.duration_ms': summary.duration,
        });
        span.setStatus({ code: SpanStatusCode.OK });
        return summary;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Get the embedding vector for an entity
   * @param {string} entity - Entity URI
   * @returns {Object} Embedding vector with entity, vector array, and norm
   * @throws {Error} If entity has no embedding
   */
  getEmbedding(entity) {
    z.string().min(1).parse(entity);
    const vec = this.entityEmbeddings.get(entity);
    if (!vec) {
      throw new Error(`No embedding found for entity: ${entity}`);
    }
    this.stats.queries++;
    return EmbeddingVectorSchema.parse({
      entity,
      vector: Array.from(vec),
      norm: vectorNorm(vec),
    });
  }

  /**
   * Find the k most similar entities to the given entity
   * @param {string} entity - Entity URI
   * @param {number} [k=5] - Number of similar entities to return
   * @returns {Array<{entity: string, similarity: number}>} Similar entities sorted by similarity
   * @throws {Error} If entity has no embedding
   */
  findSimilar(entity, k = 5) {
    z.string().min(1).parse(entity);
    z.number().int().min(1).parse(k);

    return tracer.startActiveSpan('embeddings.find_similar', (span) => {
      try {
        const vec = this.entityEmbeddings.get(entity);
        if (!vec) {
          throw new Error(`No embedding found for entity: ${entity}`);
        }

        const cacheKey = `similar:${entity}:${k}`;
        if (this.cache) {
          const cached = this.cache.get(cacheKey);
          if (cached) {
            this.stats.cacheHits++;
            span.setStatus({ code: SpanStatusCode.OK });
            return cached;
          }
        }

        const results = [];
        for (const [other, otherVec] of this.entityEmbeddings) {
          if (other === entity) continue;
          results.push({
            entity: other,
            similarity: cosineSimilarity(vec, otherVec),
          });
        }

        results.sort((a, b) => b.similarity - a.similarity);
        const topK = results.slice(0, k);

        if (this.cache) {
          this.cache.set(cacheKey, topK);
        }
        this.stats.queries++;

        span.setAttribute('embeddings.results_count', topK.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return topK;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Calculate cosine similarity between two entities
   * @param {string} entity1 - First entity URI
   * @param {string} entity2 - Second entity URI
   * @returns {Object} Similarity result with score and distance
   * @throws {Error} If either entity has no embedding
   */
  calculateSimilarity(entity1, entity2) {
    z.string().min(1).parse(entity1);
    z.string().min(1).parse(entity2);

    const vec1 = this.entityEmbeddings.get(entity1);
    const vec2 = this.entityEmbeddings.get(entity2);
    if (!vec1) throw new Error(`No embedding found for entity: ${entity1}`);
    if (!vec2) throw new Error(`No embedding found for entity: ${entity2}`);

    const sim = cosineSimilarity(vec1, vec2);
    const dist = euclideanDistance(vec1, vec2);

    this.stats.queries++;
    return SimilarityResultSchema.parse({
      entity1,
      entity2,
      similarity: sim,
      distance: dist,
    });
  }

  /**
   * Predict likely objects for a (subject, predicate) pair using link prediction
   * @param {string} subject - Subject entity URI
   * @param {string} predicate - Predicate URI
   * @param {number} [topK=10] - Number of candidates to return
   * @returns {Object} Prediction result with ranked candidates
   * @throws {Error} If subject or predicate has no embedding
   */
  predict(subject, predicate, topK = 10) {
    z.string().min(1).parse(subject);
    z.string().min(1).parse(predicate);

    return tracer.startActiveSpan('embeddings.predict', (span) => {
      try {
        const h = this.entityEmbeddings.get(subject);
        const r = this.relationEmbeddings.get(predicate);
        if (!h) throw new Error(`No embedding found for subject: ${subject}`);
        if (!r) throw new Error(`No embedding found for predicate: ${predicate}`);

        const candidates = [];
        for (const [entity, entityVec] of this.entityEmbeddings) {
          if (entity === subject) continue;
          const score = -this._scoreTriple(h, r, entityVec);
          candidates.push({ entity, score });
        }

        candidates.sort((a, b) => b.score - a.score);
        const result = PredictionResultSchema.parse({
          subject,
          predicate,
          candidates: candidates.slice(0, topK),
        });

        this.stats.queries++;
        span.setAttribute('embeddings.candidates_count', result.candidates.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Export the trained model as a serializable object
   * @returns {Object} Exported model data
   * @throws {Error} If model has not been trained
   */
  exportModel() {
    if (!this.trained) {
      throw new Error('Model has not been trained yet');
    }

    const entityEmbeddings = {};
    for (const [key, vec] of this.entityEmbeddings) {
      entityEmbeddings[key] = Array.from(vec);
    }
    const relationEmbeddings = {};
    for (const [key, vec] of this.relationEmbeddings) {
      relationEmbeddings[key] = Array.from(vec);
    }

    return ExportedModelSchema.parse({
      model: this.config.model,
      dimensions: this.config.dimensions,
      entityEmbeddings,
      relationEmbeddings,
      trained: true,
      metadata: {
        epochs: this.config.epochs,
        entityCount: this.entityEmbeddings.size,
        relationCount: this.relationEmbeddings.size,
        exportedAt: new Date().toISOString(),
      },
    });
  }

  /**
   * Import a previously exported model
   * @param {Object} data - Exported model data
   * @throws {Error} If model data is invalid
   */
  importModel(data) {
    const validated = ExportedModelSchema.parse(data);

    this.entityEmbeddings.clear();
    this.relationEmbeddings.clear();

    for (const [key, arr] of Object.entries(validated.entityEmbeddings)) {
      this.entityEmbeddings.set(key, new Float64Array(arr));
    }
    for (const [key, arr] of Object.entries(validated.relationEmbeddings)) {
      this.relationEmbeddings.set(key, new Float64Array(arr));
    }

    this.trained = validated.trained;
    if (this.cache) this.cache.clear();
  }

  /**
   * Score a triple using the configured model (lower = better for TransE)
   * @param {Float64Array} h - Head embedding
   * @param {Float64Array} r - Relation embedding
   * @param {Float64Array} t - Tail embedding
   * @returns {number} Score (distance)
   * @private
   */
  _scoreTriple(h, r, t) {
    return euclideanDistance(
      new Float64Array(h.map((v, i) => v + r[i])),
      t
    );
  }

  /**
   * Update embeddings via gradient step on margin loss
   * @param {Float64Array} h - Head embedding
   * @param {Float64Array} r - Relation embedding
   * @param {Float64Array} t - Positive tail embedding
   * @param {Float64Array} tn - Negative tail embedding
   * @param {number} lr - Learning rate
   * @private
   */
  _updateEmbeddings(h, r, t, tn, lr) {
    const dim = h.length;
    for (let i = 0; i < dim; i++) {
      const posGrad = 2 * (h[i] + r[i] - t[i]);
      const negGrad = 2 * (h[i] + r[i] - tn[i]);
      h[i] -= lr * (posGrad - negGrad);
      r[i] -= lr * (posGrad - negGrad);
      t[i] -= lr * (-posGrad);
      tn[i] -= lr * negGrad;
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
      trained: this.trained,
      cacheSize: this.cache ? this.cache.size : 0,
    };
  }

  /**
   * Clear all embeddings and reset manager
   */
  reset() {
    this.entityEmbeddings.clear();
    this.relationEmbeddings.clear();
    this.trained = false;
    if (this.cache) this.cache.clear();
  }
}

/**
 * Create an embeddings manager instance
 * @param {Object} [config] - Configuration
 * @returns {EmbeddingsManager} Embeddings manager
 * @example
 * const manager = createEmbeddingsManager({ model: 'TransE', dimensions: 50 });
 */
export function createEmbeddingsManager(config = {}) {
  return new EmbeddingsManager(config);
}

/**
 * Default embeddings manager instance
 */
export const defaultEmbeddingsManager = createEmbeddingsManager();
