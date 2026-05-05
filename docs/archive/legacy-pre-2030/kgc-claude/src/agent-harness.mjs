/**
 * Agent Harness - Per-agent lifecycle management
 *
 * Agent autonomy axiom:
 *   ∀ α∈𝔄:
 *     α selects probes p by Λ and updates P via newProbes(O)
 *     α publishes only:
 *       o(p) (typed) or Receipt-only (guarded)
 *     α never attempts ∈ H
 *
 * @module @unrdf/kgc-claude/agent-harness
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, VectorClock } from '@unrdf/kgc-4d';
import { createShard, addDelta, getPendingDeltas } from './shard-merge.mjs';
import { PokaYokeGuard } from './poka-yoke-guards.mjs';
import { ObservableIO } from './observable-io.mjs';
import { InfoScheduler } from './info-scheduler.mjs';
import { BudgetEnforcer } from './budget-enforcer.mjs';
import { DriftDetector } from './drift-detector.mjs';

/**
 * Agent configuration schema
 */
export const AgentConfigSchema = z.object({
  id: z.string(),
  scope: z.object({
    files: z.array(z.string()).default([]),
    graphs: z.array(z.string()).default([]),
    subjects: z.array(z.string()).default([]),
    predicates: z.array(z.string()).default([]),
  }),
  priority: z.number().int().default(0),
  capabilities: z.array(z.string()).default([]),
  budget: z.object({
    time_ms: z.number().int().positive().default(10000),
    steps: z.number().int().positive().default(50),
    bytes: z.number().int().positive().default(1024 * 1024),
  }).default({}),
});

/**
 * @typedef {z.infer<typeof AgentConfigSchema>} AgentConfig
 */

/**
 * Agent state schema
 */
export const AgentStateSchema = z.enum([
  'initialized',
  'running',
  'completed',
  'failed',
  'timed_out',
]);

/**
 * Agent execution result schema
 */
export const AgentResultSchema = z.object({
  agent_id: z.string(),
  state: AgentStateSchema,
  observations: z.number().int(),
  deltas: z.number().int(),
  receipts: z.number().int(),
  drift: z.number(),
  duration_ms: z.number(),
  error: z.string().optional(),
  final_hash: z.string(),
});

/**
 * @typedef {z.infer<typeof AgentResultSchema>} AgentResult
 */

/**
 * Agent Harness - Manages lifecycle of individual agent
 */
export class AgentHarness {
  /**
   * @param {AgentConfig} config
   * @param {Object} [dependencies]
   */
  constructor(config, dependencies = {}) {
    this.config = AgentConfigSchema.parse(config);
    this.state = 'initialized';
    this.startTime = null;
    this.endTime = null;

    // Initialize components
    this.shard = createShard(this.config.id, this.config.scope, {
      priority: this.config.priority,
    });

    this.guard = dependencies.guard || new PokaYokeGuard({
      root_allow: this.config.scope.files,
    });

    this.io = dependencies.io || new ObservableIO();

    this.scheduler = dependencies.scheduler || new InfoScheduler();

    this.budget = dependencies.budget || new BudgetEnforcer({
      time_ms: this.config.budget.time_ms,
      steps: this.config.budget.steps,
      bytes: this.config.budget.bytes,
      net_ops: 0,
      root_ops: 0,
    });

    this.drift = dependencies.drift || new DriftDetector({
      epsilon: 0.01,
      minEpochs: 2,
      maxEpochs: 20,
    });

    this.vectorClock = new VectorClock(this.config.id);
    this.observations = [];
    this.receipts = [];
    this.artifacts = [];
    this.epoch = 0;
  }

  /**
   * Start agent execution
   */
  start() {
    this.state = 'running';
    this.startTime = Date.now();
    this.vectorClock.increment();
  }

  /**
   * Execute probe with guard checks
   * α publishes only: o(p) (typed) or Receipt-only (guarded)
   * @param {Object} probe
   * @param {Function} executor
   * @returns {Promise<Object>}
   */
  async executeProbe(probe, executor) {
    // Check guard
    const guardResult = await this.guard.check({
      type: probe.type,
      path: probe.target,
      agentId: this.config.id,
    });

    if (!guardResult.allowed) {
      // α publishes Receipt-only when guarded
      this.receipts.push(guardResult.receipt);
      return {
        success: false,
        guarded: true,
        violation: guardResult.violation,
        receipt: guardResult.receipt,
      };
    }

    // Check budget
    const budgetReceipt = await this.budget.consume(probe.type, {
      steps: 1,
      bytes: probe.params?.size || 100,
    });

    if (!budgetReceipt.allowed) {
      return {
        success: false,
        exhausted: budgetReceipt.exhausted,
        receipt: budgetReceipt,
      };
    }

    // Start trace
    const trace = await this.io.startTrace(this.config.id, probe);

    try {
      const result = await executor(probe);
      await this.io.completeTrace(trace.id, result);

      this.observations.push({
        probe_id: probe.id,
        result,
        trace_id: trace.id,
      });

      this.scheduler.complete(probe.id, probe.expected_yield);

      return {
        success: true,
        result,
        trace_id: trace.id,
      };
    } catch (error) {
      await this.io.completeTrace(trace.id, null, error.message);
      return {
        success: false,
        error: error.message,
        trace_id: trace.id,
      };
    }
  }

  /**
   * Add delta to agent's shard
   * @param {Object} delta
   * @returns {{success: boolean, reason?: string}}
   */
  addDelta(delta) {
    return addDelta(this.shard.id, delta);
  }

  /**
   * Get pending deltas
   * @returns {Array}
   */
  getDeltas() {
    return getPendingDeltas(this.shard.id);
  }

  /**
   * Run single epoch
   * @param {Function} probeExecutor
   * @returns {Promise<Object>}
   */
  async runEpoch(probeExecutor) {
    this.epoch++;
    this.vectorClock.increment();

    // Choose probe
    const probe = this.scheduler.choose();
    if (!probe) {
      return { done: true, reason: 'no_probes' };
    }

    // Execute probe
    const result = await this.executeProbe(probe, probeExecutor);

    // Create artifact snapshot
    const artifact = {
      epoch: this.epoch,
      observations: this.observations.length,
      deltas: this.getDeltas().length,
      result,
    };
    this.artifacts.push(artifact);

    // Measure drift
    await this.drift.snapshot(artifact, this.epoch);
    const driftMeasurement = this.drift.computeDrift();

    // Check convergence
    const convergence = this.drift.checkConvergence();

    return {
      done: convergence.converged || this.budget.isExhausted(),
      epoch: this.epoch,
      drift: driftMeasurement?.drift ?? 1,
      convergence,
    };
  }

  /**
   * Run agent to completion
   * @param {Function} probeExecutor
   * @param {Object[]} initialProbes
   * @returns {Promise<AgentResult>}
   */
  async run(probeExecutor, initialProbes = []) {
    this.start();

    // Add initial probes
    if (initialProbes.length > 0) {
      this.scheduler.addProbes(initialProbes);
    }

    try {
      let done = false;

      while (!done) {
        const epochResult = await this.runEpoch(probeExecutor);
        done = epochResult.done;

        // Check timeout
        if (Date.now() - this.startTime > this.config.budget.time_ms) {
          this.state = 'timed_out';
          done = true;
        }
      }

      this.state = this.state === 'timed_out' ? 'timed_out' : 'completed';
    } catch (error) {
      this.state = 'failed';
      this.endTime = Date.now();

      return this.generateResult(error.message);
    }

    this.endTime = Date.now();
    return this.generateResult();
  }

  /**
   * Generate final result
   * @param {string} [error]
   * @returns {Promise<AgentResult>}
   */
  async generateResult(error) {
    const artifact = {
      agent_id: this.config.id,
      state: this.state,
      observations: this.observations,
      deltas: this.getDeltas(),
      receipts: this.receipts,
    };

    const hash = await blake3(JSON.stringify(artifact));

    return AgentResultSchema.parse({
      agent_id: this.config.id,
      state: this.state,
      observations: this.observations.length,
      deltas: this.getDeltas().length,
      receipts: this.receipts.length,
      drift: this.drift.getLatestDrift(),
      duration_ms: (this.endTime || Date.now()) - (this.startTime || Date.now()),
      error,
      final_hash: hash,
    });
  }

  /**
   * Get current state
   * @returns {Object}
   */
  getState() {
    return {
      id: this.config.id,
      state: this.state,
      epoch: this.epoch,
      observations: this.observations.length,
      deltas: this.getDeltas().length,
      receipts: this.receipts.length,
      budget: this.budget.getUtilization(),
      drift: this.drift.getLatestDrift(),
    };
  }
}

/**
 * Create agent harness
 * @param {AgentConfig} config
 * @param {Object} [dependencies]
 * @returns {AgentHarness}
 */
export function createAgent(config, dependencies) {
  return new AgentHarness(config, dependencies);
}

/**
 * Create multiple agents
 * @param {AgentConfig[]} configs
 * @returns {AgentHarness[]}
 */
export function createAgents(configs) {
  return configs.map(config => new AgentHarness(config));
}

export default AgentHarness;
