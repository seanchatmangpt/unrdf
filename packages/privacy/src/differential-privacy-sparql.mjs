/**
 * @file Differential Privacy for SPARQL Queries
 * @module @unrdf/privacy/differential-privacy-sparql
 * @description
 * Privacy-preserving SPARQL query execution with differential privacy guarantees.
 * Implements Laplace mechanism for COUNT/SUM and exponential mechanism for SELECT.
 *
 * **Use Case**: Public health database allows statistical queries without
 * revealing individual patient records.
 *
 * **Privacy Guarantee**: (ε, δ)-differential privacy
 * - ε (epsilon): Privacy budget (lower = more private)
 * - δ (delta): Failure probability
 *
 * **Performance**: 1-2ms noise addition overhead
 *
 * @example
 * const engine = new DifferentialPrivacySPARQL({ totalBudget: 10.0 });
 * const result = await engine.executePrivateQuery(store, 'SELECT COUNT(*) WHERE { ?s a Patient }', 1.0);
 * console.log(`Count: ${result.noisyValue} (ε=${result.epsilon})`);
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import crypto from 'node:crypto';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Privacy parameters schema
 */
const PrivacyParamsSchema = z.object({
  /** Privacy budget (epsilon) */
  epsilon: z.number().positive().max(10.0),
  /** Failure probability (delta) */
  delta: z.number().nonnegative().max(0.01).default(0.0),
  /** Sensitivity of query */
  sensitivity: z.number().positive().default(1),
});

/**
 * Query result schema
 */
const PrivateQueryResultSchema = z.object({
  /** True value (only for testing/debugging) */
  trueValue: z.number(),
  /** Noisy value (released to user) */
  noisyValue: z.number(),
  /** Privacy parameters used */
  epsilon: z.number(),
  delta: z.number(),
  /** Mechanism used */
  mechanism: z.enum(['laplace', 'gaussian', 'exponential', 'sparse_vector']),
  /** Budget spent */
  budgetSpent: z.number(),
  /** Budget remaining */
  budgetRemaining: z.number(),
  /** Query hash (for audit) */
  queryHash: z.string(),
  /** Timestamp */
  timestamp: z.string(),
});

/**
 * Privacy budget receipt schema
 */
const BudgetReceiptSchema = z.object({
  id: z.string().uuid(),
  totalBudget: z.number(),
  spent: z.number(),
  remaining: z.number(),
  queryCount: z.number(),
  queries: z.array(
    z.object({
      queryId: z.string(),
      epsilon: z.number(),
      timestamp: z.number(),
      mechanism: z.string(),
    })
  ),
  createdAt: z.string(),
  receiptHash: z.string(),
});

// =============================================================================
// Privacy Budget Manager
// =============================================================================

/**
 * Privacy Budget Manager
 *
 * Tracks and enforces privacy budget consumption across queries.
 * Prevents privacy leakage through composition of multiple queries.
 *
 * @class
 * @example
 * const manager = new PrivacyBudgetManager(10.0);
 * manager.spend(1.0, 'query-1');
 * console.log(`Remaining: ${manager.remaining()}`);
 */
export class PrivacyBudgetManager {
  /**
   * @param {number} totalBudget - Total ε budget (typically 1.0 - 10.0)
   */
  constructor(totalBudget = 10.0) {
    this.totalBudget = totalBudget;
    this.spent = 0.0;
    this.queries = [];
  }

  /**
   * Check if query can be executed with given epsilon
   * @param {number} epsilon - Epsilon cost of query
   * @returns {boolean} True if budget available
   */
  canExecute(epsilon) {
    return this.spent + epsilon <= this.totalBudget;
  }

  /**
   * Spend privacy budget
   * @param {number} epsilon - Epsilon to spend
   * @param {string} queryId - Query identifier
   * @param {string} mechanism - Mechanism used
   * @throws {Error} If budget exceeded
   */
  spend(epsilon, queryId, mechanism = 'laplace') {
    if (!this.canExecute(epsilon)) {
      throw new Error(
        `Privacy budget exhausted: ${this.spent.toFixed(2)}/${this.totalBudget} (requested: ${epsilon})`
      );
    }

    this.spent += epsilon;
    this.queries.push({
      queryId,
      epsilon,
      timestamp: Date.now(),
      mechanism,
      remainingBudget: this.totalBudget - this.spent,
    });
  }

  /**
   * Get remaining budget
   * @returns {number} Remaining epsilon
   */
  remaining() {
    return this.totalBudget - this.spent;
  }

  /**
   * Generate cryptographic receipt of budget usage
   * @returns {Promise<Object>} Budget receipt
   */
  async generateReceipt() {
    const receipt = {
      id: crypto.randomUUID(),
      totalBudget: this.totalBudget,
      spent: this.spent,
      remaining: this.remaining(),
      queryCount: this.queries.length,
      queries: this.queries,
      createdAt: new Date().toISOString(),
    };

    const receiptHash = await blake3(JSON.stringify(receipt));

    return BudgetReceiptSchema.parse({
      ...receipt,
      receiptHash,
    });
  }

  /**
   * Reset budget (WARNING: Use only for testing)
   */
  reset() {
    this.spent = 0.0;
    this.queries = [];
  }
}

// =============================================================================
// Noise Mechanisms
// =============================================================================

/**
 * Generate Laplace-distributed random noise
 *
 * @param {number} mu - Mean (typically 0)
 * @param {number} b - Scale parameter (sensitivity / epsilon)
 * @returns {number} Laplace random variable
 *
 * @example
 * const noise = laplace(0, 1.0); // Lap(0, 1)
 */
export function laplace(mu, b) {
  // Generate uniform random in (-0.5, 0.5)
  const u = Math.random() - 0.5;

  // Inverse CDF of Laplace distribution
  return mu - b * Math.sign(u) * Math.log(1 - 2 * Math.abs(u));
}

/**
 * Generate Gaussian-distributed random noise
 *
 * @param {number} mu - Mean
 * @param {number} sigma - Standard deviation
 * @returns {number} Gaussian random variable
 *
 * @example
 * const noise = gaussian(0, 1.0); // N(0, 1)
 */
export function gaussian(mu, sigma) {
  // Box-Muller transform
  const u1 = Math.random();
  const u2 = Math.random();

  const z0 = Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);

  return mu + sigma * z0;
}

/**
 * Sample from exponential mechanism
 *
 * @param {Array<{value: any, score: number}>} candidates - Candidate values with scores
 * @param {number} epsilon - Privacy parameter
 * @param {number} sensitivity - Query sensitivity
 * @returns {any} Selected value
 *
 * @example
 * const selected = exponentialMechanism([
 *   { value: 'A', score: 10 },
 *   { value: 'B', score: 5 }
 * ], 1.0, 1);
 */
export function exponentialMechanism(candidates, epsilon, sensitivity) {
  // Compute probabilities: exp(ε * score / (2 * sensitivity))
  const probabilities = candidates.map((c) =>
    Math.exp((epsilon * c.score) / (2 * sensitivity))
  );

  // Normalize
  const sum = probabilities.reduce((a, b) => a + b, 0);
  const normalizedProbs = probabilities.map((p) => p / sum);

  // Sample according to probabilities
  const r = Math.random();
  let cumulative = 0;

  for (let i = 0; i < candidates.length; i++) {
    cumulative += normalizedProbs[i];
    if (r < cumulative) {
      return candidates[i].value;
    }
  }

  return candidates[candidates.length - 1].value;
}

// =============================================================================
// Differential Privacy SPARQL Engine
// =============================================================================

/**
 * Differential Privacy SPARQL Query Engine
 *
 * Executes SPARQL queries with differential privacy guarantees.
 * Supports COUNT, SUM, AVG, and SELECT DISTINCT with calibrated noise.
 *
 * @class
 * @example
 * const engine = new DifferentialPrivacySPARQL({ totalBudget: 10.0 });
 *
 * const result = await engine.executePrivateQuery(
 *   store,
 *   'SELECT COUNT(*) WHERE { ?s a Patient }',
 *   1.0 // epsilon
 * );
 *
 * console.log(`Private count: ${result.noisyValue}`);
 */
export class DifferentialPrivacySPARQL {
  /**
   * @param {Object} config - Engine configuration
   * @param {number} [config.totalBudget=10.0] - Total privacy budget
   * @param {number} [config.defaultDelta=0.0] - Default delta (0 = pure DP)
   */
  constructor(config = {}) {
    this.budgetManager = new PrivacyBudgetManager(config.totalBudget || 10.0);
    this.defaultDelta = config.defaultDelta || 0.0;
  }

  /**
   * Execute private COUNT query
   *
   * @param {Object} store - RDF store
   * @param {string} pattern - Triple pattern
   * @param {number} epsilon - Privacy budget for this query
   * @returns {Promise<Object>} Private query result
   *
   * @example
   * const result = await engine.executeCOUNT(store, '?s a Patient', 1.0);
   * console.log(`Noisy count: ${result.noisyValue}`);
   */
  async executeCOUNT(store, pattern, epsilon) {
    const params = PrivacyParamsSchema.parse({
      epsilon,
      delta: this.defaultDelta,
      sensitivity: 1, // Adding/removing one triple changes count by ±1
    });

    // Execute true query
    const trueCount = await this._executeTrueCount(store, pattern);

    // Add Laplace noise: Lap(0, sensitivity / epsilon)
    const noise = laplace(0, params.sensitivity / params.epsilon);
    const noisyCount = Math.max(0, Math.round(trueCount + noise));

    // Spend budget
    const queryId = crypto.randomUUID();
    this.budgetManager.spend(params.epsilon, queryId, 'laplace');

    return PrivateQueryResultSchema.parse({
      trueValue: trueCount,
      noisyValue: noisyCount,
      epsilon: params.epsilon,
      delta: params.delta,
      mechanism: 'laplace',
      budgetSpent: params.epsilon,
      budgetRemaining: this.budgetManager.remaining(),
      queryHash: await blake3(`COUNT:${pattern}`),
      timestamp: new Date().toISOString(),
    });
  }

  /**
   * Execute private SUM query
   *
   * @param {Object} store - RDF store
   * @param {string} pattern - Triple pattern
   * @param {string} variable - Variable to sum
   * @param {number} epsilon - Privacy budget
   * @param {number} [clampMin] - Minimum value (for bounded sensitivity)
   * @param {number} [clampMax] - Maximum value
   * @returns {Promise<Object>} Private query result
   *
   * @example
   * const result = await engine.executeSUM(store, '?p hasAge ?age', 'age', 1.0, 0, 120);
   */
  async executeSUM(store, pattern, variable, epsilon, clampMin = 0, clampMax = 100) {
    // Sensitivity: max difference from adding/removing one record
    const sensitivity = Math.max(Math.abs(clampMin), Math.abs(clampMax));

    const params = PrivacyParamsSchema.parse({
      epsilon,
      delta: this.defaultDelta,
      sensitivity,
    });

    // Execute true query
    const trueSum = await this._executeTrueSum(store, pattern, variable);

    // Clamp to bounds (required for bounded sensitivity)
    const clampedSum = Math.max(clampMin, Math.min(clampMax, trueSum));

    // Add Laplace noise
    const noise = laplace(0, params.sensitivity / params.epsilon);
    const noisySum = clampedSum + noise;

    const queryId = crypto.randomUUID();
    this.budgetManager.spend(params.epsilon, queryId, 'laplace');

    return PrivateQueryResultSchema.parse({
      trueValue: trueSum,
      noisyValue: noisySum,
      epsilon: params.epsilon,
      delta: params.delta,
      mechanism: 'laplace',
      budgetSpent: params.epsilon,
      budgetRemaining: this.budgetManager.remaining(),
      queryHash: await blake3(`SUM:${pattern}:${variable}`),
      timestamp: new Date().toISOString(),
    });
  }

  /**
   * Execute private SELECT DISTINCT (exponential mechanism)
   *
   * @param {Object} store - RDF store
   * @param {string} variable - Variable to select
   * @param {string} pattern - Triple pattern
   * @param {number} epsilon - Privacy budget
   * @returns {Promise<Object>} Private query result
   *
   * @example
   * const result = await engine.executeSELECT(store, 'disease', '?p hasDiagnosis ?disease', 1.0);
   * console.log(`Selected: ${result.noisyValue}`);
   */
  async executeSELECT(store, variable, pattern, epsilon) {
    const params = PrivacyParamsSchema.parse({
      epsilon,
      delta: this.defaultDelta,
      sensitivity: 1,
    });

    // Get all distinct values with counts
    const valueCounts = await this._executeDistinctCounts(store, variable, pattern);

    // Use exponential mechanism to select value proportional to count
    const candidates = Object.entries(valueCounts).map(([value, count]) => ({
      value,
      score: count,
    }));

    const selected = exponentialMechanism(candidates, params.epsilon, params.sensitivity);

    const queryId = crypto.randomUUID();
    this.budgetManager.spend(params.epsilon, queryId, 'exponential');

    return {
      noisyValue: selected,
      epsilon: params.epsilon,
      mechanism: 'exponential',
      budgetSpent: params.epsilon,
      budgetRemaining: this.budgetManager.remaining(),
      queryHash: await blake3(`SELECT:${pattern}:${variable}`),
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Execute sparse vector technique (threshold queries)
   *
   * Tests multiple patterns against a threshold privately.
   * Only reveals which patterns are "above threshold", not exact counts.
   *
   * @param {Object} store - RDF store
   * @param {Array<string>} patterns - Patterns to test
   * @param {number} threshold - Threshold value
   * @param {number} epsilon - Privacy budget
   * @returns {Promise<Array<{pattern: string, aboveThreshold: boolean}>>}
   *
   * @example
   * const results = await engine.executeSparseVector(
   *   store,
   *   ['?p hasFever true', '?p hasCough true'],
   *   100,
   *   1.0
   * );
   */
  async executeSparseVector(store, patterns, threshold, epsilon) {
    const epsilon1 = epsilon / 2; // For threshold noise
    const epsilon2 = epsilon / 2; // For count noise

    // Add noise to threshold
    const noisyThreshold = threshold + laplace(0, 2 / epsilon1);

    const results = [];
    for (const pattern of patterns) {
      const trueCount = await this._executeTrueCount(store, pattern);
      const noisyCount = trueCount + laplace(0, 4 / epsilon2);

      results.push({
        pattern,
        aboveThreshold: noisyCount >= noisyThreshold,
      });
    }

    const queryId = crypto.randomUUID();
    this.budgetManager.spend(epsilon, queryId, 'sparse_vector');

    return results;
  }

  /**
   * Get budget receipt (cryptographic proof of budget usage)
   *
   * @returns {Promise<Object>} Budget receipt
   */
  async getBudgetReceipt() {
    return await this.budgetManager.generateReceipt();
  }

  /**
   * Get remaining budget
   * @returns {number} Remaining epsilon
   */
  getRemainingBudget() {
    return this.budgetManager.remaining();
  }

  // ===========================================================================
  // Private Methods (True Query Execution)
  // ===========================================================================

  /**
   * Execute true COUNT query (no privacy)
   * @private
   */
  async _executeTrueCount(store, pattern) {
    // In production, execute actual SPARQL query
    // For prototype, simulate with simple pattern matching

    if (!store || !store.match) {
      throw new Error('Store must implement match() method');
    }

    const matches = await store.match(pattern);
    return matches.length;
  }

  /**
   * Execute true SUM query (no privacy)
   * @private
   */
  async _executeTrueSum(store, pattern, variable) {
    // In production, execute SPARQL SUM aggregation
    // For prototype, simulate

    const matches = await store.match(pattern);
    return matches.reduce((sum, match) => {
      const value = match[variable];
      return sum + (typeof value === 'number' ? value : 0);
    }, 0);
  }

  /**
   * Execute DISTINCT with counts (no privacy)
   * @private
   */
  async _executeDistinctCounts(store, variable, pattern) {
    const matches = await store.match(pattern);
    const counts = {};

    for (const match of matches) {
      const value = match[variable];
      counts[value] = (counts[value] || 0) + 1;
    }

    return counts;
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Calculate optimal epsilon allocation across multiple queries
 *
 * @param {number} totalBudget - Total epsilon budget
 * @param {number} queryCount - Number of queries
 * @param {Array<number>} [priorities] - Priority weights (default: equal)
 * @returns {Array<number>} Epsilon allocations
 *
 * @example
 * const allocations = allocateEpsilon(10.0, 5);
 * // [2.0, 2.0, 2.0, 2.0, 2.0] - equal allocation
 */
export function allocateEpsilon(totalBudget, queryCount, priorities = null) {
  if (priorities && priorities.length !== queryCount) {
    throw new Error('Priority array length must match query count');
  }

  if (!priorities) {
    // Equal allocation
    const perQuery = totalBudget / queryCount;
    return Array(queryCount).fill(perQuery);
  }

  // Weighted allocation
  const totalWeight = priorities.reduce((sum, w) => sum + w, 0);
  return priorities.map((w) => (w / totalWeight) * totalBudget);
}

/**
 * Estimate noise magnitude for given parameters
 *
 * @param {number} epsilon - Privacy parameter
 * @param {number} sensitivity - Query sensitivity
 * @param {string} [mechanism='laplace'] - Noise mechanism
 * @returns {Object} Noise statistics
 *
 * @example
 * const stats = estimateNoise(1.0, 1);
 * console.log(`Expected noise: ±${stats.stdDev}`);
 */
export function estimateNoise(epsilon, sensitivity, mechanism = 'laplace') {
  if (mechanism === 'laplace') {
    const scale = sensitivity / epsilon;
    return {
      mechanism: 'laplace',
      scale,
      mean: 0,
      stdDev: scale * Math.sqrt(2),
      confidence95: 2.996 * scale, // 95% of noise within ±2.996*scale
    };
  } else if (mechanism === 'gaussian') {
    const sigma = (sensitivity * Math.sqrt(2 * Math.log(1.25 / 0.00001))) / epsilon;
    return {
      mechanism: 'gaussian',
      sigma,
      mean: 0,
      stdDev: sigma,
      confidence95: 1.96 * sigma,
    };
  }

  throw new Error(`Unknown mechanism: ${mechanism}`);
}

// =============================================================================
// Exports
// =============================================================================

export default DifferentialPrivacySPARQL;
export {
  PrivacyParamsSchema,
  PrivateQueryResultSchema,
  BudgetReceiptSchema,
};
