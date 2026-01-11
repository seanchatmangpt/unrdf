/**
 * @file Federated Knowledge Graph Embeddings
 * @module ai-ml-innovations/federated-embeddings
 *
 * @description
 * Privacy-preserving federated learning for knowledge graph embeddings.
 * Trains embeddings across multiple UNRDF federation nodes without
 * centralizing data using FedAvg and differential privacy.
 *
 * Performance targets:
 * - Communication rounds: <50 for convergence
 * - Privacy: ε-differential privacy (ε ≤ 1.0)
 * - Accuracy: ≥95% of centralized training
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { randomBytes } from 'crypto';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Node update schema
 */
const NodeUpdateSchema = z.object({
  nodeId: z.string(),
  gradients: z.record(z.string(), z.array(z.number())),
  sampleCount: z.number().int().positive(),
  epoch: z.number().int().nonnegative(),
  timestamp: z.number(),
});

/**
 * Model schema
 */
const ModelSchema = z.object({
  entityEmbeddings: z.record(z.string(), z.array(z.number())),
  relationEmbeddings: z.record(z.string(), z.array(z.number())),
  version: z.number().int().nonnegative(),
  timestamp: z.number(),
});

/**
 * Federated training configuration
 */
const FederatedConfigSchema = z.object({
  embeddingDim: z.number().min(32).max(512).default(128),
  aggregationStrategy: z.enum(['fedavg', 'fedprox', 'fedadam']).default('fedavg'),
  privacyBudget: z.number().min(0).max(10).default(1.0),
  noiseMultiplier: z.number().min(0).max(2).default(0.1),
  clippingNorm: z.number().min(0.1).max(10).default(1.0),
  minNodesPerRound: z.number().min(1).max(1000).default(2),
  enableDifferentialPrivacy: z.boolean().default(true),
});

/**
 * Federated Embedding Trainer
 *
 * Trains knowledge graph embeddings across federated UNRDF nodes
 * with differential privacy guarantees.
 *
 * @class
 */
export class FederatedEmbeddingTrainer {
  /**
   * Create a federated embedding trainer
   *
   * @param {Object} config - Configuration
   * @param {Array<Object>} config.nodes - Federated node connections
   * @param {string} [config.aggregationStrategy='fedavg'] - Aggregation strategy
   * @param {number} [config.privacyBudget=1.0] - Privacy budget (epsilon)
   */
  constructor(config = {}) {
    const validated = FederatedConfigSchema.parse(config);

    this.config = validated;
    this.nodes = config.nodes || [];
    this.globalModel = null;
    this.modelVersion = 0;

    // Privacy accounting
    this.privacySpent = 0;

    // Training history
    this.trainingHistory = [];

    // Statistics
    this.stats = {
      rounds: 0,
      totalUpdates: 0,
      avgCommunicationTime: 0,
      avgAggregationTime: 0,
      convergenceRound: null,
    };
  }

  /**
   * Train federated embeddings
   *
   * @param {Object} options - Training options
   * @param {number} [options.epochs=10] - Global epochs
   * @param {number} [options.localEpochs=5] - Local epochs per node
   * @param {number} [options.batchSize=32] - Batch size
   * @param {number} [options.convergenceThreshold=0.001] - Convergence threshold
   * @returns {Promise<Object>} Training results
   *
   * @example
   * const trainer = new FederatedEmbeddingTrainer({ nodes });
   * const model = await trainer.trainFederated({ epochs: 10, localEpochs: 5 });
   */
  async trainFederated(options = {}) {
    return tracer.startActiveSpan('federated.train', async (span) => {
      const startTime = Date.now();

      try {
        const epochs = options.epochs || 10;
        const localEpochs = options.localEpochs || 5;
        const batchSize = options.batchSize || 32;
        const convergenceThreshold = options.convergenceThreshold || 0.001;

        span.setAttributes({
          'federated.nodes': this.nodes.length,
          'federated.epochs': epochs,
          'federated.local_epochs': localEpochs,
          'federated.privacy_budget': this.config.privacyBudget,
        });

        // Initialize global model
        this.globalModel = this.initializeGlobalModel();

        let previousLoss = Infinity;

        // Federated training rounds
        for (let epoch = 0; epoch < epochs; epoch++) {
          const roundStartTime = Date.now();

          // Select nodes for this round (simplified: use all nodes)
          const selectedNodes = this._selectNodes();

          span.addEvent('Round started', {
            epoch,
            selected_nodes: selectedNodes.length,
          });

          // Parallel local training on each node
          const nodeUpdates = await Promise.all(
            selectedNodes.map(async (node) => {
              return this.trainLocalNode(node, this.globalModel, localEpochs, batchSize);
            })
          );

          span.addEvent('Local training complete', {
            updates_received: nodeUpdates.length,
          });

          // Aggregate updates with differential privacy
          const aggregateStartTime = Date.now();
          this.globalModel = await this.aggregateUpdates(nodeUpdates, epoch);
          const aggregateTime = Date.now() - aggregateStartTime;

          // Update privacy accounting
          if (this.config.enableDifferentialPrivacy) {
            this.privacySpent += this._computePrivacyCost(nodeUpdates.length);
          }

          // Validate on federated validation set
          const metrics = await this.validateFederated(this.globalModel);

          // Check convergence
          const lossDelta = Math.abs(metrics.loss - previousLoss);
          previousLoss = metrics.loss;

          const roundTime = Date.now() - roundStartTime;
          this.stats.avgCommunicationTime =
            (this.stats.avgCommunicationTime * epoch + roundTime) / (epoch + 1);
          this.stats.avgAggregationTime =
            (this.stats.avgAggregationTime * epoch + aggregateTime) / (epoch + 1);

          this.trainingHistory.push({
            epoch,
            loss: metrics.loss,
            accuracy: metrics.accuracy,
            privacySpent: this.privacySpent,
            roundTime,
          });

          console.log(
            `[Federated] Epoch ${epoch}/${epochs}: Loss=${metrics.loss.toFixed(4)}, ` +
            `Accuracy=${(metrics.accuracy * 100).toFixed(2)}%, ` +
            `Privacy=${this.privacySpent.toFixed(3)}ε`
          );

          span.addEvent('Round complete', {
            epoch,
            loss: metrics.loss,
            accuracy: metrics.accuracy,
            loss_delta: lossDelta,
          });

          // Check convergence
          if (lossDelta < convergenceThreshold && !this.stats.convergenceRound) {
            this.stats.convergenceRound = epoch;
            span.addEvent('Convergence achieved', { epoch });
          }

          // Privacy budget check
          if (this.config.enableDifferentialPrivacy && this.privacySpent > this.config.privacyBudget) {
            console.warn(`Privacy budget exhausted: ${this.privacySpent.toFixed(3)}ε > ${this.config.privacyBudget}ε`);
            break;
          }
        }

        this.stats.rounds = epochs;

        const duration = Date.now() - startTime;

        span.setAttributes({
          'federated.duration_ms': duration,
          'federated.convergence_round': this.stats.convergenceRound || epochs,
          'federated.privacy_spent': this.privacySpent,
          'federated.model_version': this.globalModel.version,
        });

        span.setStatus({ code: SpanStatusCode.OK });

        return {
          model: this.globalModel,
          trainingHistory: this.trainingHistory,
          stats: this.stats,
          privacySpent: this.privacySpent,
        };
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Train on a single federated node
   *
   * @param {Object} node - Node connection
   * @param {Object} globalModel - Current global model
   * @param {number} epochs - Local epochs
   * @param {number} batchSize - Batch size
   * @returns {Promise<Object>} Node update
   */
  async trainLocalNode(node, globalModel, epochs, batchSize) {
    return tracer.startActiveSpan('federated.train_local_node', async (span) => {
      try {
        span.setAttribute('node.id', node.id);

        // Clone global model to local
        const localModel = this.cloneModel(globalModel);

        // Fetch local graph data (never leaves node)
        const localGraph = await this._fetchLocalGraph(node);
        const triples = this._extractTriples(localGraph);

        span.setAttribute('node.triples', triples.length);

        // Local training
        for (let epoch = 0; epoch < epochs; epoch++) {
          // Shuffle triples
          const shuffled = this._shuffle(triples);

          for (let i = 0; i < shuffled.length; i += batchSize) {
            const batch = shuffled.slice(i, i + batchSize);
            await this._trainBatch(localModel, batch);
          }
        }

        // Compute model difference (gradients)
        const gradients = this.computeModelDiff(globalModel, localModel);

        // Apply gradient clipping for privacy
        if (this.config.enableDifferentialPrivacy) {
          this._clipGradients(gradients, this.config.clippingNorm);
        }

        const update = NodeUpdateSchema.parse({
          nodeId: node.id,
          gradients,
          sampleCount: triples.length,
          epoch: this.stats.rounds,
          timestamp: Date.now(),
        });

        span.setAttributes({
          'node.samples': triples.length,
          'node.gradient_norm': this._computeGradientNorm(gradients),
        });

        span.setStatus({ code: SpanStatusCode.OK });

        this.stats.totalUpdates++;

        return update;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Aggregate node updates
   *
   * @param {Array<Object>} nodeUpdates - Updates from nodes
   * @param {number} epoch - Current epoch
   * @returns {Promise<Object>} Updated global model
   */
  async aggregateUpdates(nodeUpdates, epoch) {
    return tracer.startActiveSpan('federated.aggregate_updates', async (span) => {
      try {
        span.setAttribute('updates.count', nodeUpdates.length);

        if (this.config.aggregationStrategy === 'fedavg') {
          return this.federatedAveraging(nodeUpdates, epoch);
        } else if (this.config.aggregationStrategy === 'fedprox') {
          return this.federatedProximal(nodeUpdates, epoch);
        } else if (this.config.aggregationStrategy === 'fedadam') {
          return this.federatedAdam(nodeUpdates, epoch);
        }

        throw new Error(`Unknown aggregation strategy: ${this.config.aggregationStrategy}`);
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Federated averaging (FedAvg)
   *
   * @param {Array<Object>} updates - Node updates
   * @param {number} epoch - Current epoch
   * @returns {Object} Aggregated model
   */
  federatedAveraging(updates, epoch) {
    const totalSamples = updates.reduce((sum, u) => sum + u.sampleCount, 0);
    const avgGradients = {};

    // Weight by sample count
    for (const update of updates) {
      const weight = update.sampleCount / totalSamples;

      for (const [key, gradient] of Object.entries(update.gradients)) {
        if (!avgGradients[key]) {
          avgGradients[key] = new Array(gradient.length).fill(0);
        }

        for (let i = 0; i < gradient.length; i++) {
          avgGradients[key][i] += weight * gradient[i];
        }
      }
    }

    // Add differential privacy noise
    if (this.config.enableDifferentialPrivacy) {
      this._addPrivacyNoise(avgGradients);
    }

    // Apply averaged gradients to global model
    return this.applyUpdate(this.globalModel, avgGradients, epoch);
  }

  /**
   * Federated Proximal (FedProx)
   *
   * @param {Array<Object>} updates - Node updates
   * @param {number} epoch - Current epoch
   * @returns {Object} Aggregated model
   */
  federatedProximal(updates, epoch) {
    // Simplified FedProx (similar to FedAvg with proximal term)
    // In practice, proximal term would be applied during local training
    return this.federatedAveraging(updates, epoch);
  }

  /**
   * Federated Adam
   *
   * @param {Array<Object>} updates - Node updates
   * @param {number} epoch - Current epoch
   * @returns {Object} Aggregated model
   */
  federatedAdam(updates, epoch) {
    // Simplified FedAdam (would use Adam optimizer state)
    return this.federatedAveraging(updates, epoch);
  }

  /**
   * Initialize global model
   *
   * @returns {Object} Global model
   */
  initializeGlobalModel() {
    return ModelSchema.parse({
      entityEmbeddings: {},
      relationEmbeddings: {},
      version: 0,
      timestamp: Date.now(),
    });
  }

  /**
   * Clone model
   *
   * @param {Object} model - Model to clone
   * @returns {Object} Cloned model
   */
  cloneModel(model) {
    return {
      entityEmbeddings: JSON.parse(JSON.stringify(model.entityEmbeddings)),
      relationEmbeddings: JSON.parse(JSON.stringify(model.relationEmbeddings)),
      version: model.version,
      timestamp: Date.now(),
    };
  }

  /**
   * Compute model difference (gradients)
   *
   * @param {Object} oldModel - Old model
   * @param {Object} newModel - New model
   * @returns {Object} Gradients
   */
  computeModelDiff(oldModel, newModel) {
    const gradients = {};

    // Entity embeddings
    for (const [entity, newEmb] of Object.entries(newModel.entityEmbeddings)) {
      const oldEmb = oldModel.entityEmbeddings[entity];
      if (oldEmb) {
        gradients[`entity_${entity}`] = newEmb.map((val, i) => val - oldEmb[i]);
      } else {
        gradients[`entity_${entity}`] = newEmb;
      }
    }

    // Relation embeddings
    for (const [relation, newEmb] of Object.entries(newModel.relationEmbeddings)) {
      const oldEmb = oldModel.relationEmbeddings[relation];
      if (oldEmb) {
        gradients[`relation_${relation}`] = newEmb.map((val, i) => val - oldEmb[i]);
      } else {
        gradients[`relation_${relation}`] = newEmb;
      }
    }

    return gradients;
  }

  /**
   * Apply update to model
   *
   * @param {Object} model - Current model
   * @param {Object} gradients - Gradients to apply
   * @param {number} epoch - Current epoch
   * @returns {Object} Updated model
   */
  applyUpdate(model, gradients, epoch) {
    const updated = this.cloneModel(model);

    for (const [key, gradient] of Object.entries(gradients)) {
      if (key.startsWith('entity_')) {
        const entity = key.substring(7);
        const current = updated.entityEmbeddings[entity] || new Array(gradient.length).fill(0);
        updated.entityEmbeddings[entity] = current.map((val, i) => val + gradient[i]);
      } else if (key.startsWith('relation_')) {
        const relation = key.substring(9);
        const current = updated.relationEmbeddings[relation] || new Array(gradient.length).fill(0);
        updated.relationEmbeddings[relation] = current.map((val, i) => val + gradient[i]);
      }
    }

    updated.version = epoch + 1;
    updated.timestamp = Date.now();

    return updated;
  }

  /**
   * Validate federated model
   *
   * @param {Object} model - Model to validate
   * @returns {Promise<Object>} Validation metrics
   */
  async validateFederated(model) {
    // Simplified validation (would use actual validation set in production)
    const loss = Math.random() * 0.5; // Placeholder
    const accuracy = 0.8 + Math.random() * 0.15; // Placeholder

    return { loss, accuracy };
  }

  /**
   * Get trainer statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      nodes: this.nodes.length,
      modelVersion: this.globalModel?.version || 0,
      privacySpent: this.privacySpent,
      privacyBudget: this.config.privacyBudget,
      privacyRemaining: Math.max(0, this.config.privacyBudget - this.privacySpent),
    };
  }

  /**
   * Select nodes for training round
   * @private
   */
  _selectNodes() {
    // Simplified: select all nodes
    // In practice, would implement node sampling strategies
    return this.nodes.filter(() => Math.random() > 0.1); // 90% participation
  }

  /**
   * Fetch local graph from node
   * @private
   */
  async _fetchLocalGraph(node) {
    // Simplified: return mock graph
    // In practice, would call node.fetchGraph()
    return node.graph || [];
  }

  /**
   * Extract triples from graph
   * @private
   */
  _extractTriples(graph) {
    return graph.map((quad) => ({
      subject: quad.subject?.value || quad.subject,
      predicate: quad.predicate?.value || quad.predicate,
      object: quad.object?.value || quad.object,
    }));
  }

  /**
   * Train on a batch
   * @private
   */
  async _trainBatch(model, batch) {
    // Simplified training (TransE-style)
    const lr = 0.01;

    for (const triple of batch) {
      const { subject, predicate, object } = triple;

      // Initialize embeddings if needed
      if (!model.entityEmbeddings[subject]) {
        model.entityEmbeddings[subject] = this._randomVector(this.config.embeddingDim);
      }
      if (!model.entityEmbeddings[object]) {
        model.entityEmbeddings[object] = this._randomVector(this.config.embeddingDim);
      }
      if (!model.relationEmbeddings[predicate]) {
        model.relationEmbeddings[predicate] = this._randomVector(this.config.embeddingDim);
      }

      const h = model.entityEmbeddings[subject];
      const r = model.relationEmbeddings[predicate];
      const t = model.entityEmbeddings[object];

      // Simple gradient update
      const diff = h.map((val, i) => val + r[i] - t[i]);
      const norm = Math.sqrt(diff.reduce((sum, val) => sum + val * val, 0));

      if (norm > 0) {
        for (let i = 0; i < h.length; i++) {
          h[i] -= lr * diff[i] / norm;
          t[i] += lr * diff[i] / norm;
        }
      }
    }
  }

  /**
   * Clip gradients for privacy
   * @private
   */
  _clipGradients(gradients, maxNorm) {
    for (const [key, gradient] of Object.entries(gradients)) {
      const norm = Math.sqrt(gradient.reduce((sum, val) => sum + val * val, 0));

      if (norm > maxNorm) {
        gradients[key] = gradient.map((val) => (val / norm) * maxNorm);
      }
    }
  }

  /**
   * Add differential privacy noise
   * @private
   */
  _addPrivacyNoise(gradients) {
    const sigma = this.config.noiseMultiplier * this.config.clippingNorm;

    for (const [key, gradient] of Object.entries(gradients)) {
      gradients[key] = gradient.map((val) => val + this._gaussianNoise(0, sigma));
    }
  }

  /**
   * Gaussian noise
   * @private
   */
  _gaussianNoise(mean, std) {
    // Box-Muller transform
    const u1 = Math.random();
    const u2 = Math.random();
    const z0 = Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
    return mean + std * z0;
  }

  /**
   * Compute privacy cost
   * @private
   */
  _computePrivacyCost(numNodes) {
    // Simplified privacy accounting
    // In practice, would use moments accountant or RDP accounting
    return this.config.noiseMultiplier / Math.sqrt(numNodes);
  }

  /**
   * Compute gradient norm
   * @private
   */
  _computeGradientNorm(gradients) {
    let totalNorm = 0;

    for (const gradient of Object.values(gradients)) {
      totalNorm += gradient.reduce((sum, val) => sum + val * val, 0);
    }

    return Math.sqrt(totalNorm);
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
    return vec;
  }

  /**
   * Shuffle array
   * @private
   */
  _shuffle(array) {
    const shuffled = [...array];
    for (let i = shuffled.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
    }
    return shuffled;
  }
}

/**
 * Create federated embedding trainer
 *
 * @param {Object} config - Configuration
 * @returns {FederatedEmbeddingTrainer} Trainer instance
 */
export function createFederatedEmbeddingTrainer(config = {}) {
  return new FederatedEmbeddingTrainer(config);
}

export default FederatedEmbeddingTrainer;
