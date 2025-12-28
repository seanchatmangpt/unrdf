/**
 * @file convergence.mjs
 * @description Convergence detection for KGC-SWARM system
 * Implements drift calculation: drift(A_τ) := |A_τ ⊖ A_{τ-1}|
 * Stop condition: diminishing(drift(A_τ)) under budget(B)
 * Budget: B := {time ≤ T, steps ≤ S, bytes ≤ M, net ≤ N_allow}
 */

import { z } from 'zod';

/**
 * Budget schema for convergence constraints
 * @type {z.ZodObject}
 */
export const BudgetSchema = z.object({
  maxTime: z.number().positive().optional(), // milliseconds
  maxSteps: z.number().int().positive().optional(),
  maxBytes: z.number().int().positive().optional(),
  maxNetworkOps: z.number().int().positive().optional(),
});

/**
 * Convergence configuration schema
 * @type {z.ZodObject}
 */
export const ConvergenceConfigSchema = z.object({
  driftThreshold: z.number().positive().default(0.01), // Saturation threshold: ΔA_τ → 0
  windowSize: z.number().int().positive().default(3), // Window for diminishing detection
  diminishingThreshold: z.number().positive().default(0.1), // Rate of drift decrease
  budget: BudgetSchema,
});

/**
 * Artifact state representation for drift calculation
 * @typedef {Object} ArtifactState
 * @property {number} timestamp - Epoch timestamp τ
 * @property {Set<string>} artifacts - Set of artifact identifiers
 * @property {Map<string, number>} weights - Artifact weights/scores
 * @property {number} totalSize - Total size in bytes
 */

/**
 * Drift calculation result
 * @typedef {Object} DriftResult
 * @property {number} drift - |A_τ ⊖ A_{τ-1}|
 * @property {number} added - |A_τ \ A_{τ-1}|
 * @property {number} removed - |A_{τ-1} \ A_τ|
 * @property {number} modified - Modified artifact count
 * @property {number} normalized - drift / |A_τ|
 */

/**
 * ConvergenceDetector: Monitors convergence of KGC-SWARM artifact evolution
 *
 * @class
 * @example
 * const detector = new ConvergenceDetector({
 *   driftThreshold: 0.01,
 *   windowSize: 3,
 *   budget: { maxTime: 60000, maxSteps: 100 }
 * });
 *
 * const state = { timestamp: Date.now(), artifacts: new Set(['a1', 'a2']) };
 * detector.recordEpoch(state);
 * const hasConverged = detector.checkConvergence();
 */
export class ConvergenceDetector {
  /**
   * @param {Object} config - Convergence configuration
   * @param {number} [config.driftThreshold=0.01] - Saturation threshold
   * @param {number} [config.windowSize=3] - Diminishing detection window
   * @param {number} [config.diminishingThreshold=0.1] - Drift decrease rate
   * @param {Object} config.budget - Budget constraints
   */
  constructor(config) {
    this.config = ConvergenceConfigSchema.parse(config);

    /** @type {ArtifactState[]} */
    this.history = [];

    /** @type {DriftResult[]} */
    this.driftHistory = [];

    this.startTime = Date.now();
    this.stepCount = 0;
    this.totalBytes = 0;
    this.networkOps = 0;

    this.converged = false;
    this.convergenceReason = null;
  }

  /**
   * Calculate drift between two artifact states: drift(A_τ) := |A_τ ⊖ A_{τ-1}|
   *
   * @param {ArtifactState} current - Current state A_τ
   * @param {ArtifactState} previous - Previous state A_{τ-1}
   * @returns {DriftResult} Drift metrics
   */
  calculateDrift(current, previous) {
    const currentArtifacts = current.artifacts;
    const previousArtifacts = previous.artifacts;

    // Added: A_τ \ A_{τ-1}
    const added = new Set(
      [...currentArtifacts].filter(x => !previousArtifacts.has(x))
    );

    // Removed: A_{τ-1} \ A_τ
    const removed = new Set(
      [...previousArtifacts].filter(x => !currentArtifacts.has(x))
    );

    // Modified: artifacts with changed weights
    let modified = 0;
    if (current.weights && previous.weights) {
      for (const [key, currentWeight] of current.weights) {
        const prevWeight = previous.weights.get(key);
        if (prevWeight !== undefined && currentWeight !== prevWeight) {
          modified++;
        }
      }
    }

    // Symmetric difference: |A_τ ⊖ A_{τ-1}| = |added| + |removed|
    const drift = added.size + removed.size + modified;

    // Normalized drift: drift / |A_τ|
    const normalized = currentArtifacts.size > 0
      ? drift / currentArtifacts.size
      : 0;

    return {
      drift,
      added: added.size,
      removed: removed.size,
      modified,
      normalized,
    };
  }

  /**
   * Record artifact state for epoch τ
   *
   * @param {ArtifactState} state - Current artifact state
   * @returns {DriftResult|null} Drift from previous epoch, or null if first epoch
   */
  recordEpoch(state) {
    this.stepCount++;
    this.totalBytes += state.totalSize || 0;

    // Calculate drift if we have previous state
    let driftResult = null;
    if (this.history.length > 0) {
      const previousState = this.history[this.history.length - 1];
      driftResult = this.calculateDrift(state, previousState);
      this.driftHistory.push(driftResult);
    }

    // Record state
    this.history.push(state);

    return driftResult;
  }

  /**
   * Check if drift is diminishing: d(τ) < d(τ-1) < d(τ-2) ...
   *
   * @returns {boolean} True if drift is consistently decreasing
   */
  isDriftDiminishing() {
    if (this.driftHistory.length < this.config.windowSize) {
      return false;
    }

    const window = this.driftHistory.slice(-this.config.windowSize);

    // Check monotonic decrease
    for (let i = 1; i < window.length; i++) {
      if (window[i].normalized >= window[i - 1].normalized) {
        return false;
      }
    }

    // Check rate of decrease
    const firstDrift = window[0].normalized;
    const lastDrift = window[window.length - 1].normalized;
    const decreaseRate = (firstDrift - lastDrift) / firstDrift;

    return decreaseRate >= this.config.diminishingThreshold;
  }

  /**
   * Check if drift has saturated: ΔA_τ → 0
   *
   * @returns {boolean} True if drift is below threshold
   */
  isSaturated() {
    if (this.driftHistory.length === 0) {
      return false;
    }

    const lastDrift = this.driftHistory[this.driftHistory.length - 1];
    return lastDrift.normalized <= this.config.driftThreshold;
  }

  /**
   * Check budget constraints: B := {time ≤ T, steps ≤ S, bytes ≤ M, net ≤ N_allow}
   *
   * @returns {{exceeded: boolean, reason: string|null}} Budget status
   */
  checkBudget() {
    const { budget } = this.config;
    const elapsed = Date.now() - this.startTime;

    if (budget.maxTime !== undefined && elapsed >= budget.maxTime) {
      return { exceeded: true, reason: `Time budget exceeded: ${elapsed}ms >= ${budget.maxTime}ms` };
    }

    if (budget.maxSteps !== undefined && this.stepCount >= budget.maxSteps) {
      return { exceeded: true, reason: `Step budget exceeded: ${this.stepCount} >= ${budget.maxSteps}` };
    }

    if (budget.maxBytes !== undefined && this.totalBytes >= budget.maxBytes) {
      return { exceeded: true, reason: `Bytes budget exceeded: ${this.totalBytes} >= ${budget.maxBytes}` };
    }

    if (budget.maxNetworkOps !== undefined && this.networkOps >= budget.maxNetworkOps) {
      return { exceeded: true, reason: `Network ops budget exceeded: ${this.networkOps} >= ${budget.maxNetworkOps}` };
    }

    return { exceeded: false, reason: null };
  }

  /**
   * Check convergence: stop ⇔ diminishing(drift(A_τ)) under budget(B)
   *
   * @returns {{converged: boolean, reason: string}} Convergence status
   */
  checkConvergence() {
    // Check if already converged
    if (this.converged) {
      return { converged: true, reason: this.convergenceReason };
    }

    // Check budget first (hard constraint)
    const budgetStatus = this.checkBudget();
    if (budgetStatus.exceeded) {
      this.converged = true;
      this.convergenceReason = budgetStatus.reason;
      return { converged: true, reason: budgetStatus.reason };
    }

    // Check saturation: ΔA_τ → 0
    if (this.isSaturated()) {
      this.converged = true;
      this.convergenceReason = `Saturated: drift ${this.driftHistory[this.driftHistory.length - 1].normalized.toFixed(4)} ≤ ${this.config.driftThreshold}`;
      return { converged: true, reason: this.convergenceReason };
    }

    // Check diminishing drift
    if (this.isDriftDiminishing()) {
      this.converged = true;
      this.convergenceReason = `Diminishing drift detected over window size ${this.config.windowSize}`;
      return { converged: true, reason: this.convergenceReason };
    }

    return { converged: false, reason: 'Not converged' };
  }

  /**
   * Record network operation for budget tracking
   */
  recordNetworkOp() {
    this.networkOps++;
  }

  /**
   * Get current convergence metrics
   *
   * @returns {Object} Current metrics
   */
  getMetrics() {
    const currentDrift = this.driftHistory.length > 0
      ? this.driftHistory[this.driftHistory.length - 1]
      : null;

    return {
      epochCount: this.history.length,
      stepCount: this.stepCount,
      currentDrift,
      saturated: this.isSaturated(),
      diminishing: this.isDriftDiminishing(),
      converged: this.converged,
      convergenceReason: this.convergenceReason,
      elapsed: Date.now() - this.startTime,
      totalBytes: this.totalBytes,
      networkOps: this.networkOps,
    };
  }

  /**
   * Reset detector state
   */
  reset() {
    this.history = [];
    this.driftHistory = [];
    this.startTime = Date.now();
    this.stepCount = 0;
    this.totalBytes = 0;
    this.networkOps = 0;
    this.converged = false;
    this.convergenceReason = null;
  }
}
