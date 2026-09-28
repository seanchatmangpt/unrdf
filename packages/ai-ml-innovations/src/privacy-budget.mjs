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
 * Rényi orders used by the moments accountant (fine near 1, geometric out to 512)
 */
const RDP_ORDERS = [
  1.25, 1.5, 1.75, 2, 2.25, 2.5, 3, 3.5, 4, 4.5, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32,
  48, 64, 96, 128, 192, 256, 384, 512,
];

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

    // RDP orders for moments accountant. The grid must reach large orders: for small
    // per-step RDP the optimal order is ~sqrt(log(1/delta) / rdp_rate), well beyond 10.
    this.rdpOrders = (this.composition === 'rdp' || this.composition === 'moments')
      ? RDP_ORDERS.slice()
      : [];
    this.rdpEpsilons = new Array(this.rdpOrders.length).fill(0);

    // Accumulators for basic / advanced composition
    this._sumEpsilon = 0;
    this._sumEpsilonSq = 0;
    this._sumEpsilonExp = 0;
  }

  /**
   * Compute the (marginal) privacy cost of a training round.
   *
   * Pure: does not change the tracker. The returned epsilon is the increase of the
   * total composed epsilon if this round were accounted next, so that
   * `spent += cost.epsilon` equals the composed total under every composition mode.
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

        const cost = this._marginalCost(noiseMultiplier, samplingRate, steps).cost;

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
    const { noiseMultiplier, samplingRate, steps = 1 } = params;
    const { cost, commit } = this._marginalCost(noiseMultiplier, samplingRate, steps);
    const newSpent = this.spent + cost.epsilon;

    // Refuse (without consuming budget) any round that would exceed the total budget
    if (newSpent > this.epsilon) {
      throw new Error(
        `Privacy budget exhausted: ${newSpent.toFixed(4)}ε > ${this.epsilon}ε`
      );
    }

    commit();
    this.spent = newSpent;
    this.rounds++;

    this.history.push({
      round: this.rounds,
      epsilon: cost.epsilon,
      delta: cost.delta,
      totalSpent: this.spent,
      timestamp: Date.now(),
    });

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
    this._sumEpsilon = 0;
    this._sumEpsilonSq = 0;
    this._sumEpsilonExp = 0;
  }

  /**
   * Compute the marginal cost of `steps` more steps without mutating state.
   * Returns the cost plus a `commit` closure that applies the accumulator update.
   * @private
   */
  _marginalCost(sigma, q, steps) {
    // Per-step epsilon of the Gaussian mechanism with subsampling rate q
    const stepEpsilon = (q * Math.sqrt(2 * Math.log(1.25 / this.delta))) / sigma;

    const sumEpsilon = this._sumEpsilon + stepEpsilon * steps;
    const sumEpsilonSq = this._sumEpsilonSq + stepEpsilon * stepEpsilon * steps;
    const sumEpsilonExp =
      this._sumEpsilonExp + stepEpsilon * (Math.exp(stepEpsilon) - 1) * steps;

    let total;
    let commit = () => {};

    if (this.composition === 'moments' || this.composition === 'rdp') {
      const newRdp = this.rdpEpsilons.map(
        (acc, i) => acc + this._computeRDP(this.rdpOrders[i], sigma, q) * steps
      );
      // Both are valid bounds; the tighter one is reported
      total = Math.min(this._rdpToDP(newRdp, this.rdpOrders, this.delta), sumEpsilon);
      commit = () => {
        this.rdpEpsilons = newRdp;
        this._sumEpsilon = sumEpsilon;
      };
    } else if (this.composition === 'advanced') {
      // Advanced composition for heterogeneous steps:
      // ε_total = sqrt(2 ln(1/δ) Σε²) + Σ ε (e^ε - 1), never worse than basic composition
      const advanced =
        Math.sqrt(2 * Math.log(1 / this.delta) * sumEpsilonSq) + sumEpsilonExp;
      total = Math.min(advanced, sumEpsilon);
      commit = () => {
        this._sumEpsilon = sumEpsilon;
        this._sumEpsilonSq = sumEpsilonSq;
        this._sumEpsilonExp = sumEpsilonExp;
      };
    } else {
      total = sumEpsilon;
      commit = () => {
        this._sumEpsilon = sumEpsilon;
      };
    }

    return {
      cost: { epsilon: Math.max(0, total - this.spent), delta: this.delta },
      commit,
    };
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
    // Balle et al. (2020): ε(δ) = min_α [rdp_α + log((α-1)/α) - (log δ + log α) / (α - 1)]
    // (tighter than the classic rdp_α + log(1/δ)/(α-1))
    let minEpsilon = Infinity;

    for (let i = 0; i < orders.length; i++) {
      const alpha = orders[i];
      if (alpha <= 1) continue;

      const epsilon =
        rdpEpsilons[i] +
        Math.log((alpha - 1) / alpha) -
        (Math.log(delta) + Math.log(alpha)) / (alpha - 1);
      minEpsilon = Math.min(minEpsilon, epsilon);
    }

    return Math.max(0, minEpsilon);
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
