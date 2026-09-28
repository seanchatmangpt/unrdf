/**
 * @file Privacy Budget Tracking for Differential Privacy
 * @module ai-ml-innovations/privacy-budget
 *
 * @description
 * Privacy budget accounting with moments accountant for
 * composition of differential privacy guarantees.
 *
 * Implements:
 * - Basic composition (ε accumulation)
 * - Advanced composition (optimal bounds)
 * - Moments accountant (tight bounds for SGD)
 * - Rényi Differential Privacy (RDP)
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { PrivacyBudgetSchema } from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Privacy Budget Tracker
 *
 * Tracks and composes privacy costs across multiple rounds
 * of federated learning with differential privacy.
 *
 * @class
 */
export class PrivacyBudgetTracker {
  /**
   * Create privacy budget tracker
   *
   * @param {Object} config - Configuration
   * @param {number} config.epsilon - Total privacy budget (ε)
   * @param {number} [config.delta=1e-5] - Failure probability (δ)
   * @param {string} [config.composition='moments'] - Composition method
   */
  constructor(config = {}) {
    const validated = PrivacyBudgetSchema.parse({
      epsilon: config.epsilon || 1.0,
      delta: config.delta || 1e-5,
      spent: 0,
      remaining: config.epsilon || 1.0,
      rounds: 0,
      composition: config.composition || 'moments',
    });

    this.epsilon = validated.epsilon;
    this.delta = validated.delta;
    this.spent = validated.spent;
    this.rounds = validated.rounds;
    this.composition = validated.composition;

    // Track per-round costs
    this.history = [];

    // RDP orders for moments accountant
    this.rdpOrders = (this.composition === 'rdp' || this.composition === 'moments')
      ? [
          ...Array.from({ length: 99 }, (_, i) => 1 + (i + 1) / 10),
          // High orders keep the log(1/δ)/(α-1) conversion term small
          ...[12, 14, 16, 20, 24, 32, 48, 64, 128, 256],
        ]
      : [];
    this.rdpEpsilons = new Array(this.rdpOrders.length).fill(0);
  }

  /**
   * Compute privacy cost for a training round
   *
   * @param {Object} params - Round parameters
   * @param {number} params.noiseMultiplier - Noise multiplier (σ)
   * @param {number} params.samplingRate - Client sampling rate (q)
   * @param {number} [params.steps=1] - Number of gradient steps
   * @returns {Object} Privacy cost { epsilon, delta }
   *
   * @example
   * const cost = tracker.computeRoundCost({
   *   noiseMultiplier: 1.0,
   *   samplingRate: 0.1,
   *   steps: 1
   * });
   */
  computeRoundCost(params) {
    return tracer.startActiveSpan('privacy.compute_round_cost', (span) => {
      try {
        const { noiseMultiplier, samplingRate, steps = 1 } = params;

        span.setAttributes({
          'privacy.noise_multiplier': noiseMultiplier,
          'privacy.sampling_rate': samplingRate,
          'privacy.steps': steps,
          'privacy.composition': this.composition,
        });

        let cost;

        if (this.composition === 'moments' || this.composition === 'rdp') {
          cost = this._momentsAccountant(noiseMultiplier, samplingRate, steps);
        } else if (this.composition === 'advanced') {
          cost = this._advancedComposition(noiseMultiplier, samplingRate, steps);
        } else {
          cost = this._basicComposition(noiseMultiplier, samplingRate, steps);
        }

        span.setAttribute('privacy.cost_epsilon', cost.epsilon);
        span.setStatus({ code: SpanStatusCode.OK });

        return cost;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Account for a training round
   *
   * @param {Object} params - Round parameters
   * @param {number} params.noiseMultiplier - Noise multiplier (σ)
   * @param {number} params.samplingRate - Client sampling rate (q)
   * @param {number} [params.steps=1] - Number of gradient steps
   * @returns {Object} Updated budget status
   * @throws {Error} If privacy budget exhausted
   */
  accountRound(params) {
    const cost = this.computeRoundCost(params);

    if (this.composition === 'moments' || this.composition === 'rdp') {
      // RDP accumulates across rounds: commit it and take the cumulative ε
      // (adding the cumulative ε on every round would double count).
      this._commitRDP(params.noiseMultiplier, params.samplingRate, params.steps ?? 1);
      this.spent = this._rdpToDP(this.rdpEpsilons, this.rdpOrders, this.delta);
    } else {
      this.spent += cost.epsilon;
    }
    this.rounds++;

    this.history.push({
      round: this.rounds,
      epsilon: cost.epsilon,
      delta: cost.delta,
      totalSpent: this.spent,
      timestamp: Date.now(),
    });

    if (this.spent > this.epsilon) {
      throw new Error(
        `Privacy budget exhausted: ${this.spent.toFixed(4)}ε > ${this.epsilon}ε`
      );
    }

    return this.getStatus();
  }

  /**
   * Get current budget status
   *
   * @returns {Object} Budget status
   */
  getStatus() {
    return {
      epsilon: this.epsilon,
      delta: this.delta,
      spent: this.spent,
      remaining: Math.max(0, this.epsilon - this.spent),
      rounds: this.rounds,
      exhausted: this.spent >= this.epsilon,
      history: this.history,
    };
  }

  /**
   * Check if budget allows more rounds
   *
   * @param {number} [minRemaining=0.1] - Minimum remaining budget
   * @returns {boolean} True if more rounds allowed
   */
  canContinue(minRemaining = 0.1) {
    return this.epsilon - this.spent >= minRemaining;
  }

  /**
   * Reset budget tracker
   */
  reset() {
    this.spent = 0;
    this.rounds = 0;
    this.history = [];
    this.rdpEpsilons = new Array(this.rdpOrders.length).fill(0);
  }

  /**
   * Basic composition (ε accumulation)
   * @private
   */
  _basicComposition(sigma, q, steps) {
    // For Gaussian mechanism: ε ≈ q * sqrt(2 * ln(1.25/δ)) / σ
    const epsilon = (q * Math.sqrt(2 * Math.log(1.25 / this.delta))) / sigma;
    return { epsilon: epsilon * steps, delta: this.delta };
  }

  /**
   * Advanced composition (optimal bounds)
   * @private
   */
  _advancedComposition(sigma, q, steps) {
    // Advanced composition theorem
    const epsilonPrime = (q * Math.sqrt(2 * Math.log(1.25 / this.delta))) / sigma;
    const k = this.rounds + steps;

    // ε' = sqrt(2k * ln(1/δ')) * ε + k * ε * (e^ε - 1)
    const epsilon =
      Math.sqrt(2 * k * Math.log(1 / this.delta)) * epsilonPrime +
      k * epsilonPrime * (Math.exp(epsilonPrime) - 1);

    return { epsilon, delta: this.delta };
  }

  /**
   * Moments accountant (tight bounds for SGD)
   * @private
   */
  _momentsAccountant(sigma, q, steps) {
    // Simplified moments accountant (pure: state is only changed by accountRound)
    const after = this.rdpEpsilons.map(
      (e, i) => e + this._computeRDP(this.rdpOrders[i], sigma, q) * steps
    );
    const before = this._rdpToDP(this.rdpEpsilons, this.rdpOrders, this.delta);
    const total = this._rdpToDP(after, this.rdpOrders, this.delta);

    // Marginal cost of this round
    return { epsilon: Math.max(0, total - before), delta: this.delta };
  }

  /**
   * Add a round's RDP cost to the accumulated per-order totals
   * @private
   */
  _commitRDP(sigma, q, steps) {
    for (let i = 0; i < this.rdpOrders.length; i++) {
      this.rdpEpsilons[i] += this._computeRDP(this.rdpOrders[i], sigma, q) * steps;
    }
  }

  /**
   * Compute RDP at order alpha
   * @private
   */
  _computeRDP(alpha, sigma, q) {
    // RDP for subsampled Gaussian mechanism
    // Simplified formula (exact formula is more complex)

    if (alpha === 1) {
      return (q * q) / (2 * sigma * sigma);
    }

    // Approximation for alpha > 1
    const c = q * q / (2 * sigma * sigma);
    return c * alpha;
  }

  /**
   * Convert RDP to (ε, δ)-DP
   * @private
   */
  _rdpToDP(rdpEpsilons, orders, delta) {
    // ε(δ) = min_α [rdp_α + log(1/δ) / (α - 1)]
    let minEpsilon = Infinity;

    for (let i = 0; i < orders.length; i++) {
      const alpha = orders[i];
      if (alpha === 1) continue;

      const epsilon = rdpEpsilons[i] + Math.log(1 / delta) / (alpha - 1);
      minEpsilon = Math.min(minEpsilon, epsilon);
    }

    return minEpsilon;
  }
}

/**
 * Create privacy budget tracker
 *
 * @param {Object} config - Configuration
 * @returns {PrivacyBudgetTracker} Tracker instance
 */
export function createPrivacyBudgetTracker(config) {
  return new PrivacyBudgetTracker(config);
}

export default PrivacyBudgetTracker;
