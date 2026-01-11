/**
 * @file Federated Averaging (FedAvg) Algorithm
 * @module ai-ml-innovations/fedavg
 *
 * @description
 * Production implementation of FedAvg algorithm for federated learning.
 *
 * Reference:
 * McMahan et al. "Communication-Efficient Learning of Deep Networks
 * from Decentralized Data" (2017)
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { FedAvgConfigSchema } from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Federated Averaging Aggregator
 *
 * Aggregates model updates from federated clients using
 * weighted averaging based on sample counts.
 *
 * @class
 */
export class FedAvgAggregator {
  /**
   * Create FedAvg aggregator
   *
   * @param {Object} [config={}] - Configuration
   * @param {number} [config.learningRate=0.01] - Server learning rate
   * @param {number} [config.momentum=0.9] - Server momentum
   * @param {number} [config.weightDecay=0.0001] - Weight decay
   * @param {number} [config.clientSampling=1.0] - Client sampling rate
   * @param {number} [config.minClients=1] - Minimum clients per round
   */
  constructor(config = {}) {
    const validated = FedAvgConfigSchema.parse(config);

    this.config = validated;

    // Server optimizer state
    this.velocity = null; // For momentum
    this.roundCount = 0;
  }

  /**
   * Aggregate client updates
   *
   * @param {Array<Object>} updates - Client updates
   * @param {Object} globalModel - Current global model
   * @returns {Object} Aggregated model
   * @throws {Error} If insufficient clients
   *
   * @example
   * const aggregated = aggregator.aggregate(clientUpdates, globalModel);
   */
  aggregate(updates, globalModel) {
    return tracer.startActiveSpan('fedavg.aggregate', (span) => {
      try {
        if (updates.length < this.config.minClients) {
          throw new Error(
            `Insufficient clients: ${updates.length} < ${this.config.minClients}`
          );
        }

        span.setAttributes({
          'fedavg.num_updates': updates.length,
          'fedavg.round': this.roundCount,
          'fedavg.learning_rate': this.config.learningRate,
        });

        // Weighted averaging by sample count
        const totalSamples = updates.reduce((sum, u) => sum + u.sampleCount, 0);
        const avgGradients = this._weightedAverage(updates, totalSamples);

        // Apply server optimizer
        const optimizedGradients = this._applyServerOptimizer(avgGradients);

        // Update global model
        const updated = this._updateModel(globalModel, optimizedGradients);

        this.roundCount++;

        span.setAttributes({
          'fedavg.total_samples': totalSamples,
          'fedavg.avg_gradient_norm': this._computeNorm(avgGradients),
        });

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return updated;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Weighted average of client gradients
   * @private
   */
  _weightedAverage(updates, totalSamples) {
    const avgGradients = {};

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

    return avgGradients;
  }

  /**
   * Apply server-side optimizer (momentum + weight decay)
   * @private
   */
  _applyServerOptimizer(gradients) {
    // Initialize velocity on first round
    if (!this.velocity) {
      this.velocity = {};
      for (const [key, gradient] of Object.entries(gradients)) {
        this.velocity[key] = new Array(gradient.length).fill(0);
      }
    }

    const optimized = {};

    for (const [key, gradient] of Object.entries(gradients)) {
      if (!this.velocity[key]) {
        this.velocity[key] = new Array(gradient.length).fill(0);
      }

      optimized[key] = new Array(gradient.length);

      for (let i = 0; i < gradient.length; i++) {
        // Momentum: v = β * v + g
        this.velocity[key][i] =
          this.config.momentum * this.velocity[key][i] + gradient[i];

        // Apply learning rate and weight decay
        optimized[key][i] =
          this.config.learningRate *
          (this.velocity[key][i] + this.config.weightDecay * gradient[i]);
      }
    }

    return optimized;
  }

  /**
   * Update global model with gradients
   * @private
   */
  _updateModel(model, gradients) {
    const updated = {
      entityEmbeddings: { ...model.entityEmbeddings },
      relationEmbeddings: { ...model.relationEmbeddings },
      version: model.version + 1,
      timestamp: Date.now(),
    };

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

    return updated;
  }

  /**
   * Compute gradient norm
   * @private
   */
  _computeNorm(gradients) {
    let sumSquares = 0;

    for (const gradient of Object.values(gradients)) {
      for (const val of gradient) {
        sumSquares += val * val;
      }
    }

    return Math.sqrt(sumSquares);
  }

  /**
   * Reset optimizer state
   */
  reset() {
    this.velocity = null;
    this.roundCount = 0;
  }

  /**
   * Get aggregator statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      rounds: this.roundCount,
      learningRate: this.config.learningRate,
      momentum: this.config.momentum,
      weightDecay: this.config.weightDecay,
    };
  }
}

/**
 * Create FedAvg aggregator
 *
 * @param {Object} config - Configuration
 * @returns {FedAvgAggregator} Aggregator instance
 */
export function createFedAvgAggregator(config = {}) {
  return new FedAvgAggregator(config);
}

export default FedAvgAggregator;
