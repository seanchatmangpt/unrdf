/**
 * @file orchestrator.mjs
 * @description KGC-SWARM Orchestrator - Main execution loop with token generation, epoch management, and budget enforcement
 * @module @unrdf/kgc-swarm/orchestrator
 */

import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';
import { TokenGenerator } from './token-generator.mjs';

/**
 * Budget constraint schema B := {time ≤ T, steps ≤ S, bytes ≤ M}
 * @typedef {Object} Budget
 * @property {number} maxTime - Maximum execution time in milliseconds
 * @property {number} maxSteps - Maximum execution steps
 * @property {number} maxBytes - Maximum memory usage in bytes
 */
const BudgetSchema = z.object({
  maxTime: z.number().positive().default(300000), // 5 minutes default
  maxSteps: z.number().int().positive().default(1000),
  maxBytes: z.number().int().positive().default(1024 * 1024 * 100), // 100MB default
});

/**
 * Observable space entry schema
 * @typedef {Object} Observation
 * @property {number} epoch - Epoch τ when observed
 * @property {string} type - Observation type
 * @property {any} data - Observation data
 * @property {number} timestamp - Observation timestamp (ms)
 */
const ObservationSchema = z.object({
  epoch: z.number().int().nonnegative(),
  type: z.string(),
  data: z.any(),
  timestamp: z.number(),
});

/**
 * Orchestrator state schema
 * @typedef {Object} OrchestratorState
 * @property {number} currentEpoch - Current epoch τ
 * @property {number} totalSteps - Total execution steps
 * @property {number} startTime - Start time (ms)
 * @property {boolean} stopped - Whether orchestrator is stopped
 * @property {string} [stopReason] - Reason for stopping
 */
const StateSchema = z.object({
  currentEpoch: z.number().int().nonnegative().default(0),
  totalSteps: z.number().int().nonnegative().default(0),
  startTime: z.number(),
  stopped: z.boolean().default(false),
  stopReason: z.string().optional(),
});

/**
 * KGCSwarmOrchestrator class
 * Main orchestrator implementing the execution loop:
 * ```
 * τ := 0
 * while ¬stop {
 *   t₁...t_n := G(σ, κ)
 *   O_τ := observe(t₁...t_n)
 *   τ := τ + 1
 *   check_budget(B)
 * }
 * ```
 *
 * @class KGCSwarmOrchestrator
 * @example
 * ```javascript
 * const orchestrator = new KGCSwarmOrchestrator({
 *   budget: { maxTime: 60000, maxSteps: 100, maxBytes: 50 * 1024 * 1024 }
 * });
 *
 * await orchestrator.run(
 *   { seed: 42, context: 'Initialize swarm' },
 *   { temperature: 0.7, maxTokens: 50 }
 * );
 *
 * const observations = orchestrator.getObservations();
 * console.log(`Completed ${observations.length} observations over ${orchestrator.getCurrentEpoch()} epochs`);
 * ```
 */
export class KGCSwarmOrchestrator {
  /**
   * Creates a new KGCSwarmOrchestrator instance
   * @param {Object} [options] - Configuration options
   * @param {Budget} [options.budget] - Budget constraints
   * @param {boolean} [options.storeObservations=true] - Store observations in RDF store
   */
  constructor(options = {}) {
    /** @type {Budget} */
    this.budget = BudgetSchema.parse(options.budget ?? {});

    /** @type {TokenGenerator} */
    this.tokenGenerator = new TokenGenerator({ deterministic: true });

    /** @type {import('@unrdf/oxigraph').Store} */
    this.store = options.storeObservations !== false ? createStore() : null;

    /** @type {Observation[]} */
    this.observationSpace = [];

    /** @type {OrchestratorState} */
    this.state = StateSchema.parse({
      currentEpoch: 0,
      totalSteps: 0,
      startTime: Date.now(),
      stopped: false,
    });
  }

  /**
   * Main execution loop
   * Runs until stop condition or budget exhausted
   *
   * @param {import('./token-generator.mjs').SeedParameter} σ - Seed parameter
   * @param {import('./token-generator.mjs').ControlParameter} κ - Control parameter
   * @param {Object} [options] - Execution options
   * @param {Function} [options.onEpoch] - Callback after each epoch
   * @param {Function} [options.shouldStop] - Custom stop condition
   * @returns {Promise<void>}
   * @throws {Error} If budget exceeded or execution fails
   *
   * @example
   * ```javascript
   * await orchestrator.run(
   *   { seed: 123 },
   *   { temperature: 0.8, maxTokens: 100 },
   *   {
   *     onEpoch: (τ, observations) => console.log(`Epoch ${τ}: ${observations.length} observations`),
   *     shouldStop: (state) => state.currentEpoch >= 10
   *   }
   * );
   * ```
   */
  async run(σ, κ, options = {}) {
    const { onEpoch, shouldStop } = options;

    // Reset state
    this.state.currentEpoch = 0;
    this.state.totalSteps = 0;
    this.state.startTime = Date.now();
    this.state.stopped = false;
    this.state.stopReason = undefined;
    this.observationSpace = [];

    // Main execution loop: while ¬stop
    while (!this.state.stopped) {
      // Check budget constraints BEFORE epoch
      const budgetCheck = this._checkBudget();
      if (budgetCheck.exceeded) {
        this.state.stopped = true;
        this.state.stopReason = budgetCheck.reason;
        break;
      }

      // Generate token sequence: t₁...t_n := G(σ, κ)
      const tokens = this.tokenGenerator.emit(σ, κ);

      // Observe tokens: O_τ := observe(t₁...t_n)
      const observations = this._observe(tokens);

      // Store observations in O_τ
      this.observationSpace.push(...observations);

      // Store in RDF if enabled
      if (this.store) {
        await this._storeObservations(observations);
      }

      // Increment epoch: τ := τ + 1
      this.state.currentEpoch++;
      this.state.totalSteps += tokens.length;

      // Call epoch callback
      if (onEpoch) {
        await onEpoch(this.state.currentEpoch, observations);
      }

      // Check custom stop condition
      if (shouldStop && shouldStop(this.state)) {
        this.state.stopped = true;
        this.state.stopReason = 'Custom stop condition met';
        break;
      }
    }

    return {
      epochs: this.state.currentEpoch,
      totalSteps: this.state.totalSteps,
      observations: this.observationSpace.length,
      stopReason: this.state.stopReason,
      duration: Date.now() - this.state.startTime,
    };
  }

  /**
   * Observe token sequence and create observations
   * @private
   * @param {import('./token-generator.mjs').Token[]} tokens - Token sequence
   * @returns {Observation[]} Observations
   */
  _observe(tokens) {
    /** @type {Observation[]} */
    const observations = [];

    for (const token of tokens) {
      observations.push({
        epoch: this.state.currentEpoch,
        type: 'token',
        data: token,
        timestamp: Date.now(),
      });
    }

    // Aggregate observation for epoch
    observations.push({
      epoch: this.state.currentEpoch,
      type: 'epoch_summary',
      data: {
        tokenCount: tokens.length,
        avgLogProb: tokens.reduce((sum, t) => sum + t.logProb, 0) / tokens.length,
        vocabulary: new Set(tokens.map(t => t.value)).size,
      },
      timestamp: Date.now(),
    });

    return observations;
  }

  /**
   * Store observations in RDF store
   * @private
   * @param {Observation[]} observations - Observations to store
   * @returns {Promise<void>}
   */
  async _storeObservations(observations) {
    if (!this.store) return;

    // Import from @unrdf/core instead of @unrdf/oxigraph per CLAUDE.md
    const { namedNode, literal, quad } = await import('@unrdf/core');

    for (const obs of observations) {
      const obsNode = namedNode(`urn:swarm:observation:${obs.epoch}:${obs.timestamp}`);

      this.store.add(quad(
        obsNode,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('urn:swarm:Observation'),
        namedNode('urn:swarm:observations')
      ));

      this.store.add(quad(
        obsNode,
        namedNode('urn:swarm:epoch'),
        literal(obs.epoch.toString(), namedNode('http://www.w3.org/2001/XMLSchema#integer')),
        namedNode('urn:swarm:observations')
      ));

      this.store.add(quad(
        obsNode,
        namedNode('urn:swarm:type'),
        literal(obs.type),
        namedNode('urn:swarm:observations')
      ));

      this.store.add(quad(
        obsNode,
        namedNode('urn:swarm:timestamp'),
        literal(obs.timestamp.toString(), namedNode('http://www.w3.org/2001/XMLSchema#integer')),
        namedNode('urn:swarm:observations')
      ));
    }
  }

  /**
   * Check budget constraints B := {time ≤ T, steps ≤ S, bytes ≤ M}
   * @private
   * @returns {{exceeded: boolean, reason?: string}} Budget check result
   */
  _checkBudget() {
    // Check time budget
    const elapsed = Date.now() - this.state.startTime;
    if (elapsed >= this.budget.maxTime) {
      return {
        exceeded: true,
        reason: `Time budget exceeded: ${elapsed}ms >= ${this.budget.maxTime}ms`,
      };
    }

    // Check steps budget
    if (this.state.totalSteps >= this.budget.maxSteps) {
      return {
        exceeded: true,
        reason: `Steps budget exceeded: ${this.state.totalSteps} >= ${this.budget.maxSteps}`,
      };
    }

    // Check memory budget (approximate)
    const memUsage = this._estimateMemoryUsage();
    if (memUsage >= this.budget.maxBytes) {
      return {
        exceeded: true,
        reason: `Memory budget exceeded: ${memUsage} bytes >= ${this.budget.maxBytes} bytes`,
      };
    }

    return { exceeded: false };
  }

  /**
   * Estimate current memory usage
   * @private
   * @returns {number} Estimated memory usage in bytes
   */
  _estimateMemoryUsage() {
    // Rough estimate: observations + tokens
    const obsSize = this.observationSpace.length * 200; // ~200 bytes per observation
    const storeSize = this.store ? this.store.size * 100 : 0; // ~100 bytes per quad
    return obsSize + storeSize;
  }

  /**
   * Stop orchestrator execution
   * @param {string} [reason] - Stop reason
   */
  stop(reason = 'Manual stop') {
    this.state.stopped = true;
    this.state.stopReason = reason;
  }

  /**
   * Get current epoch τ
   * @returns {number} Current epoch
   */
  getCurrentEpoch() {
    return this.state.currentEpoch;
  }

  /**
   * Get observable space O_τ
   * @param {number} [epoch] - Filter by epoch (optional)
   * @returns {Observation[]} Observations
   */
  getObservations(epoch) {
    if (epoch !== undefined) {
      return this.observationSpace.filter(obs => obs.epoch === epoch);
    }
    return this.observationSpace;
  }

  /**
   * Get orchestrator state
   * @returns {OrchestratorState} Current state
   */
  getState() {
    return { ...this.state };
  }

  /**
   * Get RDF store (if enabled)
   * @returns {import('@unrdf/oxigraph').Store | null} RDF store
   */
  getStore() {
    return this.store;
  }

  /**
   * Query observations using SPARQL (if store enabled)
   * @param {string} query - SPARQL query
   * @returns {Promise<Array>} Query results
   * @throws {Error} If store not enabled
   */
  async queryObservations(query) {
    if (!this.store) {
      throw new Error('RDF store not enabled. Set storeObservations: true in constructor');
    }

    const { executeQuerySync } = await import('@unrdf/core');
    return executeQuerySync(this.store, query);
  }
}

/**
 * Create a new KGCSwarmOrchestrator instance
 * @param {Object} [options] - Configuration options
 * @returns {KGCSwarmOrchestrator} Orchestrator instance
 */
export function createOrchestrator(options = {}) {
  return new KGCSwarmOrchestrator(options);
}

// Export schemas
export {
  BudgetSchema,
  ObservationSchema,
  StateSchema,
};
