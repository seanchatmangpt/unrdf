/**
 * @file Temporal Graph Neural Network for evolving knowledge graphs
 * @module ai-ml-innovations/temporal-gnn
 *
 * @description
 * Leverages KGC-4D temporal receipts to train GNNs on time-evolving RDF graphs.
 * Implements attention-based temporal aggregation for link prediction.
 *
 * Performance targets:
 * - Link prediction latency: <50ms (P95)
 * - Temporal window: 10-100 snapshots
 * - Accuracy: >85% for future link prediction
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Temporal snapshot schema
 */
const TemporalSnapshotSchema = z.object({
  timestamp: z.number(),
  receiptId: z.string(),
  graph: z.any(), // RDF store
  embeddings: z.map(z.string(), z.array(z.number())).optional(),
});

/**
 * TGNN configuration schema
 */
const TGNNConfigSchema = z.object({
  embeddingDim: z.number().min(32).max(512).default(128),
  temporalWindow: z.number().min(2).max(1000).default(10),
  aggregation: z.enum(['mean', 'attention', 'lstm']).default('attention'),
  attentionHeads: z.number().min(1).max(16).default(4),
  learningRate: z.number().min(0.0001).max(0.1).default(0.01),
  dropout: z.number().min(0).max(0.9).default(0.1),
});

/**
 * Link prediction result schema
 */
const LinkPredictionSchema = z.object({
  subject: z.string(),
  predicate: z.string(),
  object: z.string(),
  score: z.number().min(0).max(1),
  timeStep: z.number(),
  confidence: z.number().min(0).max(1),
});

/**
 * Temporal Attention Layer
 * Computes attention weights over temporal snapshots
 */
class TemporalAttention {
  /**
   * @param {number} embeddingDim - Embedding dimension
   * @param {number} numHeads - Number of attention heads
   */
  constructor(embeddingDim, numHeads = 4) {
    this.embeddingDim = embeddingDim;
    this.numHeads = numHeads;
    this.headDim = embeddingDim / numHeads;

    // Initialize attention parameters (simplified - in practice would use proper initialization)
    this.Wq = this._initializeWeights(embeddingDim, embeddingDim);
    this.Wk = this._initializeWeights(embeddingDim, embeddingDim);
    this.Wv = this._initializeWeights(embeddingDim, embeddingDim);
    this.Wo = this._initializeWeights(embeddingDim, embeddingDim);
  }

  /**
   * Aggregate temporal embeddings with multi-head attention
   *
   * @param {Array<Object>} temporalEmbeddings - Array of {embedding, timestamp, receiptId}
   * @returns {Array<number>} Aggregated embedding
   */
  aggregate(temporalEmbeddings) {
    if (temporalEmbeddings.length === 0) {
      throw new Error('Cannot aggregate empty temporal embeddings');
    }

    if (temporalEmbeddings.length === 1) {
      return temporalEmbeddings[0].embedding;
    }

    // Extract embeddings and add positional encoding
    const embeddings = temporalEmbeddings.map((te, idx) =>
      this._addPositionalEncoding(te.embedding, idx, temporalEmbeddings.length)
    );

    // Multi-head attention
    const headOutputs = [];
    for (let h = 0; h < this.numHeads; h++) {
      const headOutput = this._attentionHead(embeddings, h);
      headOutputs.push(headOutput);
    }

    // Concatenate heads and apply output projection
    const concatenated = headOutputs.flat();
    return this._matrixVectorProduct(this.Wo, concatenated);
  }

  /**
   * Single attention head
   *
   * @param {Array<Array<number>>} embeddings - Input embeddings
   * @param {number} headIdx - Head index
   * @returns {Array<number>} Head output
   * @private
   */
  _attentionHead(embeddings, headIdx) {
    const startIdx = headIdx * this.headDim;
    const endIdx = startIdx + this.headDim;

    // Query: last embedding (predict next)
    const query = embeddings[embeddings.length - 1].slice(startIdx, endIdx);

    // Keys and Values: all embeddings
    const keys = embeddings.map(e => e.slice(startIdx, endIdx));
    const values = embeddings.map(e => e.slice(startIdx, endIdx));

    // Compute attention scores
    const scores = keys.map(k => this._dotProduct(query, k) / Math.sqrt(this.headDim));

    // Softmax
    const attentionWeights = this._softmax(scores);

    // Weighted sum of values
    const output = new Array(this.headDim).fill(0);
    for (let i = 0; i < values.length; i++) {
      for (let j = 0; j < this.headDim; j++) {
        output[j] += attentionWeights[i] * values[i][j];
      }
    }

    return output;
  }

  /**
   * Add positional encoding to embedding
   *
   * @param {Array<number>} embedding - Input embedding
   * @param {number} position - Temporal position
   * @param {number} maxLen - Max sequence length
   * @returns {Array<number>} Embedding with positional encoding
   * @private
   */
  _addPositionalEncoding(embedding, position, maxLen) {
    const encoded = [...embedding];

    for (let i = 0; i < this.embeddingDim; i++) {
      const angle = position / Math.pow(10000, (2 * i) / this.embeddingDim);
      encoded[i] += i % 2 === 0 ? Math.sin(angle) : Math.cos(angle);
    }

    return encoded;
  }

  /**
   * Initialize weight matrix
   * @private
   */
  _initializeWeights(rows, cols) {
    const weights = [];
    for (let i = 0; i < rows; i++) {
      const row = [];
      for (let j = 0; j < cols; j++) {
        // Xavier initialization
        row.push((Math.random() - 0.5) * 2 * Math.sqrt(6 / (rows + cols)));
      }
      weights.push(row);
    }
    return weights;
  }

  /**
   * Matrix-vector product
   * @private
   */
  _matrixVectorProduct(matrix, vector) {
    return matrix.map(row => this._dotProduct(row, vector));
  }

  /**
   * Dot product
   * @private
   */
  _dotProduct(a, b) {
    return a.reduce((sum, val, i) => sum + val * b[i], 0);
  }

  /**
   * Softmax function
   * @private
   */
  _softmax(scores) {
    const maxScore = Math.max(...scores);
    const expScores = scores.map(s => Math.exp(s - maxScore));
    const sumExp = expScores.reduce((a, b) => a + b, 0);
    return expScores.map(e => e / sumExp);
  }
}

/**
 * Temporal Graph Neural Network
 *
 * Leverages temporal snapshots from KGC-4D to predict future knowledge graph structure.
 *
 * @class
 */
export class TemporalGraphNeuralNetwork {
  /**
   * Create a new Temporal GNN
   *
   * @param {Object} config - Configuration
   * @param {number} [config.embeddingDim=128] - Embedding dimension
   * @param {number} [config.temporalWindow=10] - Number of temporal snapshots
   * @param {string} [config.aggregation='attention'] - Aggregation method
   * @param {number} [config.attentionHeads=4] - Number of attention heads
   * @param {number} [config.learningRate=0.01] - Learning rate
   */
  constructor(config = {}) {
    this.config = TGNNConfigSchema.parse(config);

    // Initialize temporal attention
    if (this.config.aggregation === 'attention') {
      this.temporalAttention = new TemporalAttention(
        this.config.embeddingDim,
        this.config.attentionHeads
      );
    }

    // Node embeddings (entity embeddings)
    this.nodeEmbeddings = new Map();

    // Edge type embeddings (predicate embeddings)
    this.edgeEmbeddings = new Map();

    // Temporal history buffer
    this.temporalHistory = new Map(); // nodeId -> [snapshots]

    // Statistics
    this.stats = {
      predictions: 0,
      aggregations: 0,
      trainingSessions: 0,
      avgPredictionTime: 0,
    };
  }

  /**
   * Train on temporal graph snapshots
   *
   * @param {Array<Object>} snapshots - Temporal snapshots
   * @param {Object} options - Training options
   * @returns {Promise<Object>} Training results
   *
   * @example
   * const tgnn = new TemporalGraphNeuralNetwork({ embeddingDim: 128 });
   * const results = await tgnn.train(snapshots, { epochs: 100 });
   */
  async train(snapshots, options = {}) {
    return tracer.startActiveSpan('tgnn.train', async (span) => {
      try {
        const epochs = options.epochs || 100;

        span.setAttributes({
          'tgnn.snapshots': snapshots.length,
          'tgnn.epochs': epochs,
          'tgnn.embedding_dim': this.config.embeddingDim,
        });

        // Validate snapshots
        const validatedSnapshots = snapshots.map(s => TemporalSnapshotSchema.parse(s));

        // Initialize embeddings from first snapshot
        await this._initializeEmbeddings(validatedSnapshots[0].graph);

        // Train on temporal sequences
        for (let epoch = 0; epoch < epochs; epoch++) {
          let totalLoss = 0;

          // For each snapshot pair (t, t+1), predict t+1 from t
          for (let i = 0; i < validatedSnapshots.length - 1; i++) {
            const currentSnapshot = validatedSnapshots[i];
            const nextSnapshot = validatedSnapshots[i + 1];

            const loss = await this._trainStep(currentSnapshot, nextSnapshot);
            totalLoss += loss;
          }

          const avgLoss = totalLoss / (validatedSnapshots.length - 1);

          if (epoch % 10 === 0) {
            console.log(`[TGNN] Epoch ${epoch}/${epochs}, Loss: ${avgLoss.toFixed(4)}`);
          }
        }

        this.stats.trainingSessions++;

        span.setAttributes({
          'tgnn.training_complete': true,
          'tgnn.node_embeddings': this.nodeEmbeddings.size,
          'tgnn.edge_embeddings': this.edgeEmbeddings.size,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        return {
          nodeEmbeddings: this.nodeEmbeddings.size,
          edgeEmbeddings: this.edgeEmbeddings.size,
          temporalWindow: this.config.temporalWindow,
        };
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Predict future links at given time step
   *
   * @param {string} nodeId - Node ID to predict links for
   * @param {number} timeStep - Future time step
   * @param {Object} options - Prediction options
   * @returns {Promise<Array<Object>>} Predicted links with scores
   *
   * @example
   * const predictions = await tgnn.predictFutureLinks('http://example.org/entity1', 5, { topK: 10 });
   */
  async predictFutureLinks(nodeId, timeStep, options = {}) {
    return tracer.startActiveSpan('tgnn.predict_future_links', async (span) => {
      const startTime = Date.now();

      try {
        const topK = options.topK || 10;
        const threshold = options.threshold || 0.5;

        span.setAttributes({
          'tgnn.node_id': nodeId,
          'tgnn.time_step': timeStep,
          'tgnn.top_k': topK,
        });

        // Get temporal history for node
        const history = this.temporalHistory.get(nodeId) || [];

        if (history.length === 0) {
          throw new Error(`No temporal history found for node: ${nodeId}`);
        }

        // Aggregate temporal features
        const aggregatedEmbedding = await this.aggregateTemporalFeatures(nodeId, history);

        // Get candidate nodes for linking
        const candidates = await this.getLinkCandidates(nodeId);

        // Score each candidate
        const predictions = [];
        for (const candidate of candidates) {
          const score = this.scoreLinkProbability(aggregatedEmbedding, candidate.embedding);

          if (score >= threshold) {
            predictions.push(
              LinkPredictionSchema.parse({
                subject: nodeId,
                predicate: candidate.predicate,
                object: candidate.id,
                score,
                timeStep,
                confidence: score,
              })
            );
          }
        }

        // Sort by score and take top-K
        predictions.sort((a, b) => b.score - a.score);
        const topPredictions = predictions.slice(0, topK);

        const duration = Date.now() - startTime;
        this.stats.predictions++;
        this.stats.avgPredictionTime =
          (this.stats.avgPredictionTime * (this.stats.predictions - 1) + duration) /
          this.stats.predictions;

        span.setAttributes({
          'tgnn.predictions_made': topPredictions.length,
          'tgnn.duration_ms': duration,
          'tgnn.avg_score': topPredictions.reduce((sum, p) => sum + p.score, 0) / topPredictions.length,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        return topPredictions;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Aggregate temporal features for a node
   *
   * @param {string} nodeId - Node ID
   * @param {Array<Object>} snapshots - Temporal snapshots
   * @returns {Promise<Array<number>>} Aggregated embedding
   */
  async aggregateTemporalFeatures(nodeId, snapshots) {
    return tracer.startActiveSpan('tgnn.aggregate_temporal', async (span) => {
      try {
        span.setAttribute('tgnn.snapshots_count', snapshots.length);

        const temporalEmbeddings = [];

        for (const snapshot of snapshots.slice(-this.config.temporalWindow)) {
          const embedding = await this.computeSnapshotEmbedding(snapshot, nodeId);
          temporalEmbeddings.push({
            embedding,
            timestamp: snapshot.timestamp,
            receiptId: snapshot.receiptId,
          });
        }

        let aggregated;

        if (this.config.aggregation === 'attention') {
          aggregated = this.temporalAttention.aggregate(temporalEmbeddings);
        } else if (this.config.aggregation === 'mean') {
          aggregated = this._meanAggregation(temporalEmbeddings.map(te => te.embedding));
        } else if (this.config.aggregation === 'lstm') {
          // Simplified LSTM (would use proper LSTM in production)
          aggregated = temporalEmbeddings[temporalEmbeddings.length - 1].embedding;
        }

        this.stats.aggregations++;

        span.setAttributes({
          'tgnn.aggregation_method': this.config.aggregation,
          'tgnn.temporal_embeddings': temporalEmbeddings.length,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        return aggregated;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Compute embedding for node in a snapshot
   *
   * @param {Object} snapshot - Temporal snapshot
   * @param {string} nodeId - Node ID
   * @returns {Promise<Array<number>>} Node embedding
   */
  async computeSnapshotEmbedding(snapshot, nodeId) {
    // Use cached snapshot embeddings if available
    if (snapshot.embeddings && snapshot.embeddings.has(nodeId)) {
      return snapshot.embeddings.get(nodeId);
    }

    // Otherwise use current embeddings
    return this.nodeEmbeddings.get(nodeId) || this._randomVector(this.config.embeddingDim);
  }

  /**
   * Get link candidates for a node
   *
   * @param {string} nodeId - Node ID
   * @returns {Promise<Array<Object>>} Candidate nodes
   */
  async getLinkCandidates(nodeId) {
    const candidates = [];

    // All other nodes are candidates (simplified)
    for (const [candidateId, embedding] of this.nodeEmbeddings) {
      if (candidateId !== nodeId) {
        candidates.push({
          id: candidateId,
          embedding,
          predicate: 'http://example.org/relatedTo', // Simplified
        });
      }
    }

    return candidates;
  }

  /**
   * Score link probability between embeddings
   *
   * @param {Array<number>} sourceEmbedding - Source node embedding
   * @param {Array<number>} targetEmbedding - Target node embedding
   * @returns {number} Probability score [0, 1]
   */
  scoreLinkProbability(sourceEmbedding, targetEmbedding) {
    // Use cosine similarity
    const similarity = this._cosineSimilarity(sourceEmbedding, targetEmbedding);

    // Map to [0, 1]
    return (similarity + 1) / 2;
  }

  /**
   * Get TGNN statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      nodeEmbeddings: this.nodeEmbeddings.size,
      edgeEmbeddings: this.edgeEmbeddings.size,
      config: this.config,
    };
  }

  /**
   * Initialize embeddings from graph
   * @private
   */
  async _initializeEmbeddings(graph) {
    const entities = new Set();
    const predicates = new Set();

    for (const quad of graph) {
      entities.add(quad.subject.value);
      entities.add(quad.object.value);
      predicates.add(quad.predicate.value);
    }

    // Initialize random embeddings
    for (const entity of entities) {
      this.nodeEmbeddings.set(entity, this._randomVector(this.config.embeddingDim));
    }

    for (const predicate of predicates) {
      this.edgeEmbeddings.set(predicate, this._randomVector(this.config.embeddingDim));
    }
  }

  /**
   * Training step on snapshot pair
   * @private
   */
  async _trainStep(currentSnapshot, nextSnapshot) {
    // Extract new edges in next snapshot
    const currentEdges = this._extractEdges(currentSnapshot.graph);
    const nextEdges = this._extractEdges(nextSnapshot.graph);

    const newEdges = nextEdges.filter(
      (e) => !currentEdges.some((ce) => this._edgesEqual(ce, e))
    );

    let loss = 0;

    // For each new edge, compute prediction loss
    for (const edge of newEdges) {
      const h = this.nodeEmbeddings.get(edge.subject);
      const r = this.edgeEmbeddings.get(edge.predicate);
      const t = this.nodeEmbeddings.get(edge.object);

      if (!h || !r || !t) continue;

      // Positive score
      const posScore = this._scoreTriplet(h, r, t);

      // Negative sampling
      const negEdge = this._sampleNegativeEdge(edge, currentSnapshot.graph);
      const hNeg = this.nodeEmbeddings.get(negEdge.subject);
      const tNeg = this.nodeEmbeddings.get(negEdge.object);

      if (!hNeg || !tNeg) continue;

      const negScore = this._scoreTriplet(hNeg, r, tNeg);

      // Margin ranking loss
      const margin = 1.0;
      const edgeLoss = Math.max(0, margin - posScore + negScore);
      loss += edgeLoss;

      // Simple gradient update (simplified SGD)
      if (edgeLoss > 0) {
        const lr = this.config.learningRate;
        this._updateEmbedding(edge.subject, h, lr, -1);
        this._updateEmbedding(edge.object, t, lr, 1);
      }
    }

    return newEdges.length > 0 ? loss / newEdges.length : 0;
  }

  /**
   * Extract edges from graph
   * @private
   */
  _extractEdges(graph) {
    const edges = [];
    for (const quad of graph) {
      edges.push({
        subject: quad.subject.value,
        predicate: quad.predicate.value,
        object: quad.object.value,
      });
    }
    return edges;
  }

  /**
   * Check if edges are equal
   * @private
   */
  _edgesEqual(e1, e2) {
    return (
      e1.subject === e2.subject && e1.predicate === e2.predicate && e1.object === e2.object
    );
  }

  /**
   * Score triplet (h, r, t)
   * @private
   */
  _scoreTriplet(h, r, t) {
    // TransE scoring: -||h + r - t||
    const sum = this._add(h, r);
    const diff = this._subtract(sum, t);
    return -this._l2Norm(diff);
  }

  /**
   * Sample negative edge
   * @private
   */
  _sampleNegativeEdge(edge, _graph) {
    const entities = Array.from(this.nodeEmbeddings.keys());

    if (Math.random() < 0.5) {
      // Corrupt head
      return {
        subject: entities[Math.floor(Math.random() * entities.length)],
        predicate: edge.predicate,
        object: edge.object,
      };
    } else {
      // Corrupt tail
      return {
        subject: edge.subject,
        predicate: edge.predicate,
        object: entities[Math.floor(Math.random() * entities.length)],
      };
    }
  }

  /**
   * Update embedding
   * @private
   */
  _updateEmbedding(entityId, embedding, lr, direction) {
    const updated = embedding.map((val) => val + lr * direction * 0.1);
    this.nodeEmbeddings.set(entityId, this._normalize(updated));
  }

  /**
   * Mean aggregation
   * @private
   */
  _meanAggregation(embeddings) {
    const dim = embeddings[0].length;
    const mean = new Array(dim).fill(0);

    for (const emb of embeddings) {
      for (let i = 0; i < dim; i++) {
        mean[i] += emb[i] / embeddings.length;
      }
    }

    return mean;
  }

  /**
   * Random vector
   * @private
   */
  _randomVector(dim) {
    const vec = new Array(dim);
    for (let i = 0; i < dim; i++) {
      vec[i] = (Math.random() - 0.5) / Math.sqrt(dim);
    }
    return this._normalize(vec);
  }

  /**
   * Vector operations
   * @private
   */
  _add(a, b) {
    return a.map((val, i) => val + b[i]);
  }

  _subtract(a, b) {
    return a.map((val, i) => val - b[i]);
  }

  _normalize(vec) {
    const norm = this._l2Norm(vec);
    return norm > 0 ? vec.map((val) => val / norm) : vec;
  }

  _l2Norm(vec) {
    return Math.sqrt(vec.reduce((sum, val) => sum + val * val, 0));
  }

  _cosineSimilarity(a, b) {
    const dot = a.reduce((sum, val, i) => sum + val * b[i], 0);
    const normA = this._l2Norm(a);
    const normB = this._l2Norm(b);
    return normA > 0 && normB > 0 ? dot / (normA * normB) : 0;
  }
}

/**
 * Create Temporal GNN instance
 *
 * @param {Object} config - Configuration
 * @returns {TemporalGraphNeuralNetwork} TGNN instance
 */
export function createTemporalGNN(config = {}) {
  return new TemporalGraphNeuralNetwork(config);
}

export default TemporalGraphNeuralNetwork;
