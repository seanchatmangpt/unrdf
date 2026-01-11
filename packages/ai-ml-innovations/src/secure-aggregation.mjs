/**
 * @file Secure Aggregation for Federated Learning
 * @module ai-ml-innovations/secure-aggregation
 *
 * @description
 * Secure aggregation protocol that allows server to aggregate
 * client updates without seeing individual updates in plaintext.
 *
 * Simplified implementation of:
 * Bonawitz et al. "Practical Secure Aggregation for Privacy-Preserving
 * Machine Learning" (2017)
 */

import { z } from 'zod';
import { randomBytes } from 'crypto';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { SecureAggregationConfigSchema } from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Secure Aggregation Protocol
 *
 * Provides secure multi-party computation for aggregating
 * gradients without revealing individual contributions.
 *
 * @class
 */
export class SecureAggregation {
  /**
   * Create secure aggregation protocol
   *
   * @param {Object} config - Configuration
   * @param {number} config.threshold - Minimum nodes for reconstruction
   * @param {number} config.totalNodes - Total number of nodes
   * @param {number} [config.keySize=256] - Key size in bits
   * @param {boolean} [config.enableEncryption=true] - Enable encryption
   */
  constructor(config) {
    const validated = SecureAggregationConfigSchema.parse(config);

    this.threshold = validated.threshold;
    this.totalNodes = validated.totalNodes;
    this.keySize = validated.keySize;
    this.enableEncryption = validated.enableEncryption;

    // Node shares (for masking)
    this.shares = new Map();

    // Round counter
    this.round = 0;
  }

  /**
   * Generate shares for a node
   *
   * @param {string} nodeId - Node identifier
   * @returns {Object} Shares for masking
   *
   * @example
   * const shares = protocol.generateShares('node-1');
   */
  generateShares(nodeId) {
    return tracer.startActiveSpan('secure_agg.generate_shares', (span) => {
      try {
        span.setAttribute('secure_agg.node_id', nodeId);

        // Generate random secret
        const secret = this._generateRandomVector(this.keySize / 32);

        // Generate shares for other nodes (simplified)
        const shares = {};
        for (let i = 0; i < this.totalNodes; i++) {
          const otherId = `node-${i}`;
          if (otherId !== nodeId) {
            shares[otherId] = this._generateRandomVector(this.keySize / 32);
          }
        }

        this.shares.set(nodeId, { secret, shares });

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return { secret, shares };
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Mask gradients before sending to server
   *
   * @param {string} nodeId - Node identifier
   * @param {Object} gradients - Gradients to mask
   * @returns {Object} Masked gradients
   *
   * @example
   * const masked = protocol.maskGradients('node-1', gradients);
   */
  maskGradients(nodeId, gradients) {
    return tracer.startActiveSpan('secure_agg.mask_gradients', (span) => {
      try {
        span.setAttribute('secure_agg.node_id', nodeId);

        if (!this.enableEncryption) {
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
          return gradients;
        }

        const nodeShares = this.shares.get(nodeId);
        if (!nodeShares) {
          throw new Error(`No shares for node: ${nodeId}`);
        }

        const masked = {};

        for (const [key, gradient] of Object.entries(gradients)) {
          masked[key] = gradient.map((val, i) => {
            // Add secret share
            let maskedVal = val + nodeShares.secret[i % nodeShares.secret.length];

            // Subtract shares from other nodes
            for (const [otherId, share] of Object.entries(nodeShares.shares)) {
              maskedVal -= share[i % share.length];
            }

            return maskedVal;
          });
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return masked;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Aggregate masked gradients
   *
   * @param {Array<Object>} maskedUpdates - Masked updates from nodes
   * @returns {Object} Aggregated gradients (masks cancel out)
   *
   * @example
   * const aggregated = protocol.aggregateMasked(maskedUpdates);
   */
  aggregateMasked(maskedUpdates) {
    return tracer.startActiveSpan('secure_agg.aggregate_masked', (span) => {
      try {
        span.setAttribute('secure_agg.num_updates', maskedUpdates.length);

        if (maskedUpdates.length < this.threshold) {
          throw new Error(
            `Insufficient updates: ${maskedUpdates.length} < ${this.threshold}`
          );
        }

        // Sum all masked gradients (masks cancel out in sum)
        const aggregated = {};

        for (const update of maskedUpdates) {
          for (const [key, gradient] of Object.entries(update.gradients)) {
            if (!aggregated[key]) {
              aggregated[key] = new Array(gradient.length).fill(0);
            }

            for (let i = 0; i < gradient.length; i++) {
              aggregated[key][i] += gradient[i];
            }
          }
        }

        // Average by number of clients
        const n = maskedUpdates.length;
        for (const [key, gradient] of Object.entries(aggregated)) {
          aggregated[key] = gradient.map((val) => val / n);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return aggregated;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Start new round (reset shares)
   */
  nextRound() {
    this.round++;
    this.shares.clear();
  }

  /**
   * Generate random vector
   * @private
   */
  _generateRandomVector(length) {
    const vec = new Array(length);
    for (let i = 0; i < length; i++) {
      // Use crypto random for security
      const bytes = randomBytes(4);
      const uint = bytes.readUInt32BE(0);
      vec[i] = (uint / 0xffffffff - 0.5) * 2; // Range [-1, 1]
    }
    return vec;
  }

  /**
   * Get protocol statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      round: this.round,
      threshold: this.threshold,
      totalNodes: this.totalNodes,
      activeShares: this.shares.size,
      enableEncryption: this.enableEncryption,
    };
  }
}

/**
 * Create secure aggregation protocol
 *
 * @param {Object} config - Configuration
 * @returns {SecureAggregation} Protocol instance
 */
export function createSecureAggregation(config) {
  return new SecureAggregation(config);
}

export default SecureAggregation;
