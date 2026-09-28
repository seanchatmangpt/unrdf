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

import { randomBytes } from 'crypto';
import { createRandom } from './rng.mjs';
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
   * @param {number} [config.seed] - Optional seed for reproducible masks (tests only;
   *   without a seed masks come from crypto.randomBytes)
   */
  constructor(config) {
    const validated = SecureAggregationConfigSchema.parse(config);

    this.threshold = validated.threshold;
    this.totalNodes = validated.totalNodes;
    this.keySize = validated.keySize;
    this.enableEncryption = validated.enableEncryption;

    // Node shares (for masking)
    this.shares = new Map();

    // Pairwise masks shared by the two members of each node pair (unordered)
    this.pairMasks = new Map();

    // Seeded uniform source, or null => crypto randomness
    this._seeded = validated.seed === undefined ? null : createRandom(validated.seed);

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

        // Pairwise masks: the mask for (nodeId, otherId) is the SAME vector on both
        // sides of the pair, so that it cancels in the sum (see maskGradients).
        const shares = {};
        for (let i = 0; i < this.totalNodes; i++) {
          const otherId = `node-${i}`;
          if (otherId !== nodeId) {
            shares[otherId] = this._pairMask(nodeId, otherId);
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
            // Pairwise masking: the lexicographically smaller node adds the pair mask,
            // the larger one subtracts it, so each pair cancels in the sum.
            let maskedVal = val;

            for (const [otherId, share] of Object.entries(nodeShares.shares)) {
              const sign = nodeId < otherId ? 1 : -1;
              maskedVal += sign * share[i % share.length];
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
        const participants = new Set(maskedUpdates.map((u) => u.nodeId));

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

        // Dropout recovery: a participant's pair masks with nodes that did NOT submit an
        // update have no counterpart in the sum, so remove that residual.
        if (this.enableEncryption) {
          for (const update of maskedUpdates) {
            const nodeShares = this.shares.get(update.nodeId);
            if (!nodeShares) continue;

            for (const [otherId, share] of Object.entries(nodeShares.shares)) {
              if (participants.has(otherId)) continue;
              const sign = update.nodeId < otherId ? 1 : -1;

              for (const [key, values] of Object.entries(aggregated)) {
                for (let i = 0; i < values.length; i++) {
                  values[i] -= sign * share[i % share.length];
                }
              }
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
    this.pairMasks.clear();
  }

  /**
   * Generate random vector
   * @private
   */
  _generateRandomVector(length) {
    const vec = new Array(length);
    for (let i = 0; i < length; i++) {
      if (this._seeded) {
        vec[i] = (this._seeded() - 0.5) * 2; // Range [-1, 1), reproducible
      } else {
        // Use crypto random for security
        const bytes = randomBytes(4);
        const uint = bytes.readUInt32BE(0);
        vec[i] = (uint / 0xffffffff - 0.5) * 2; // Range [-1, 1]
      }
    }
    return vec;
  }

  /**
   * Get (creating on first use) the mask shared by an unordered node pair
   * @private
   */
  _pairMask(a, b) {
    const key = a < b ? `${a}|${b}` : `${b}|${a}`;
    let mask = this.pairMasks.get(key);
    if (!mask) {
      mask = this._generateRandomVector(this.keySize / 32);
      this.pairMasks.set(key, mask);
    }
    return mask;
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
