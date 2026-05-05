/**
 * SwarmOrchestrator - 10-agent KGC-SWARM coordination
 *
 * Universe:
 *   E ≔ VM
 *   𝔄 ≔ {α₁,…,α₁₀}
 *   τ ≔ run-epoch(E)
 *
 * Law:
 *   A = μ(O)
 *   μ∘μ = μ (idempotent projection)
 *
 * @module @unrdf/kgc-claude/swarm-orchestrator
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
import {
  createShard,
  addDelta,
  mergeDeltas,
  getPendingDeltas,
  clearShards,
} from './shard-merge.mjs';

/**
 * Agent configuration schema
 */
export const AgentConfigSchema = z.object({
  id: z.string().regex(/^α_\d+$/),
  scope: z.object({
    files: z.array(z.string()).default([]),
    graphs: z.array(z.string()).default([]),
    subjects: z.array(z.string()).default([]),
    predicates: z.array(z.string()).default([]),
  }),
  priority: z.number().int().default(0),
  capabilities: z.array(z.string()).default([]),
});

/**
 * @typedef {z.infer<typeof AgentConfigSchema>} AgentConfig
 */

/**
 * Budget constraints schema (B)
 * B := {time ≤ T, steps ≤ S, bytes ≤ M, net ≤ N_allow, root ≤ R}
 */
export const BudgetSchema = z.object({
  /** Maximum time in milliseconds */
  time: z.number().int().positive().default(30000),
  /** Maximum probe steps */
  steps: z.number().int().positive().default(100),
  /** Maximum bytes processed */
  bytes: z.number().int().positive().default(10 * 1024 * 1024),
  /** Network allowlist */
  net_allow: z.array(z.string()).default([]),
  /** Root directories allowed */
  root_allow: z.array(z.string()).default([]),
});

/**
 * @typedef {z.infer<typeof BudgetSchema>} Budget
 */

/**
 * Swarm configuration schema
 */
export const SwarmConfigSchema = z.object({
  agents: z.array(AgentConfigSchema).min(1).max(10),
  budget: BudgetSchema.default({}),
  /** Drift threshold for stopping (ε) */
  drift_epsilon: z.number().min(0).max(1).default(0.01),
  /** Minimum epochs before stopping */
  min_epochs: z.number().int().min(1).default(3),
});

/**
 * @typedef {z.infer<typeof SwarmConfigSchema>} SwarmConfig
 */

/**
 * Probe (observation request) schema
 */
export const ProbeSchema = z.object({
  id: z.string(),
  type: z.enum(['read', 'write', 'query', 'transform']),
  target: z.string(),
  params: z.record(z.any()).default({}),
  cost: z.number().positive().default(1),
  /** Expected information yield */
  expected_yield: z.number().min(0).max(1).default(0.5),
});

/**
 * @typedef {z.infer<typeof ProbeSchema>} Probe
 */

/**
 * Observation result schema
 */
export const ObservationSchema = z.object({
  probe_id: z.string(),
  agent_id: z.string(),
  t_ns: z.bigint(),
  success: z.boolean(),
  data: z.any().optional(),
  error: z.string().optional(),
  hash: z.string(),
});

/**
 * @typedef {z.infer<typeof ObservationSchema>} Observation
 */

/**
 * Receipt schema for swarm operations
 */
export const SwarmReceiptSchema = z.object({
  id: z.string(),
  epoch: z.number().int(),
  agents_run: z.number().int(),
  observations: z.number().int(),
  deltas_merged: z.number().int(),
  conflicts: z.number().int(),
  drift: z.number(),
  budget_remaining: BudgetSchema.partial(),
  hash: z.string(),
  parent_hash: z.string().optional(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof SwarmReceiptSchema>} SwarmReceipt
 */

/**
 * Generate UUID v4
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * SwarmOrchestrator - Coordinates 10-agent swarm with calculus semantics
 */
export class SwarmOrchestrator {
  /**
   * @param {SwarmConfig} config
   */
  constructor(config) {
    this.config = SwarmConfigSchema.parse(config);
    this.agents = new Map();
    this.observations = [];
    this.receipts = [];
    this.probeQueue = [];
    this.epoch = 0;
    this.budgetUsed = {
      time: 0,
      steps: 0,
      bytes: 0,
    };
    this.previousArtifact = null;
    this.vectorClock = new VectorClock('swarm-coordinator');
  }

  /**
   * Initialize all agents with their shards
   * @returns {void}
   */
  initializeAgents() {
    clearShards();

    for (const agentConfig of this.config.agents) {
      const shard = createShard(agentConfig.id, agentConfig.scope, {
        priority: agentConfig.priority,
      });

      this.agents.set(agentConfig.id, {
        config: agentConfig,
        shard,
        observations: [],
        probes: [],
        vectorClock: new VectorClock(agentConfig.id),
      });
    }
  }

  /**
   * Add probes to the queue
   * P := P ⊔ newProbes(O)
   * @param {Probe[]} probes
   */
  addProbes(probes) {
    const validated = z.array(ProbeSchema).parse(probes);
    this.probeQueue.push(...validated);
  }

  /**
   * Choose best probe using information-theoretic scheduling
   * choose(p) := argmax_{p∈P} ρ(p)
   * ρ(p) := Δ̂(p) / cost(p)
   * @returns {Probe | null}
   */
  chooseProbe() {
    if (this.probeQueue.length === 0) return null;

    // Sort by yield/cost ratio (ρ)
    const sorted = [...this.probeQueue].sort((a, b) => {
      const rhoA = a.expected_yield / a.cost;
      const rhoB = b.expected_yield / b.cost;
      return rhoB - rhoA;
    });

    const best = sorted[0];
    this.probeQueue = this.probeQueue.filter(p => p.id !== best.id);
    return best;
  }

  /**
   * Execute a probe and record observation
   * O := O ⊔ o(p)
   * @param {string} agentId
   * @param {Probe} probe
   * @param {Function} executor
   * @returns {Promise<Observation>}
   */
  async executeProbe(agentId, probe, executor) {
    const agent = this.agents.get(agentId);
    if (!agent) {
      throw new Error(`Agent not found: ${agentId}`);
    }

    const startTime = Date.now();
    let result;
    let success = true;
    let error;

    try {
      result = await executor(probe);
    } catch (e) {
      success = false;
      error = e.message;
    }

    const t_ns = now();
    this.budgetUsed.time += Date.now() - startTime;
    this.budgetUsed.steps += 1;

    if (result !== undefined) {
      const size = typeof result === 'string' ? result.length : JSON.stringify(result).length;
      this.budgetUsed.bytes += size;
    }

    // Hash the observation
    const obsData = JSON.stringify({
      probe_id: probe.id,
      agent_id: agentId,
      t_ns: t_ns.toString(),
      success,
      data: result,
      error,
    });
    const hash = await blake3(obsData);

    const observation = ObservationSchema.parse({
      probe_id: probe.id,
      agent_id: agentId,
      t_ns,
      success,
      data: result,
      error,
      hash,
    });

    agent.observations.push(observation);
    this.observations.push(observation);

    return observation;
  }

  /**
   * Project observations to artifact
   * A = μ(O)
   * μ∘μ = μ (idempotent)
   * @returns {Promise<Object>}
   */
  async project() {
    const observationData = this.observations.map(o => ({
      agent: o.agent_id,
      success: o.success,
      data: o.data,
    }));

    // Collect all deltas from agents
    const deltaSets = [];
    for (const [agentId, agent] of this.agents) {
      const deltas = getPendingDeltas(agent.shard.id);
      if (deltas.length > 0) {
        deltaSets.push(deltas);
      }
    }

    // Merge using Π operator
    let mergeResult = { merged: [], conflicts: [], receiptHash: '' };
    if (deltaSets.length > 0) {
      mergeResult = await mergeDeltas(deltaSets);
    }

    const artifact = {
      epoch: this.epoch,
      observations: observationData,
      merged_deltas: mergeResult.merged.length,
      conflicts: mergeResult.conflicts.length,
      merge_hash: mergeResult.receiptHash,
      t_ns: now().toString(),
    };

    return artifact;
  }

  /**
   * Calculate drift between artifacts
   * Δ_t = A_t ⊖ A_{t-1}
   * drift(A_t) = |Δ_t|
   * @param {Object} current
   * @param {Object} previous
   * @returns {number}
   */
  calculateDrift(current, previous) {
    if (!previous) return 1.0;

    // Compare observation counts
    const obsChange = Math.abs(
      (current.observations?.length || 0) - (previous.observations?.length || 0)
    );

    // Compare delta counts
    const deltaChange = Math.abs(
      (current.merged_deltas || 0) - (previous.merged_deltas || 0)
    );

    // Compare conflict counts
    const conflictChange = Math.abs(
      (current.conflicts || 0) - (previous.conflicts || 0)
    );

    // Normalize to [0,1]
    const maxChange = Math.max(obsChange, deltaChange, conflictChange, 1);
    const drift = (obsChange + deltaChange + conflictChange) / (3 * maxChange);

    return Math.min(1.0, drift);
  }

  /**
   * Check if budget is exhausted
   * @returns {boolean}
   */
  isBudgetExhausted() {
    return (
      this.budgetUsed.time >= this.config.budget.time ||
      this.budgetUsed.steps >= this.config.budget.steps ||
      this.budgetUsed.bytes >= this.config.budget.bytes
    );
  }

  /**
   * Check stopping condition
   * stop ⇔ argmin drift(A_t) s.t. budget(B)
   * @param {number} drift
   * @returns {boolean}
   */
  shouldStop(drift) {
    // Budget exhausted
    if (this.isBudgetExhausted()) return true;

    // Minimum epochs not reached
    if (this.epoch < this.config.min_epochs) return false;

    // Drift below threshold
    return drift <= this.config.drift_epsilon;
  }

  /**
   * Generate epoch receipt
   * @param {Object} artifact
   * @param {number} drift
   * @returns {Promise<SwarmReceipt>}
   */
  async generateReceipt(artifact, drift) {
    const parentHash = this.receipts.length > 0
      ? this.receipts[this.receipts.length - 1].hash
      : undefined;

    const t_ns = now();
    const receiptData = {
      id: `swarm-receipt-${this.epoch}-${t_ns}`,
      epoch: this.epoch,
      agents_run: this.agents.size,
      observations: this.observations.length,
      deltas_merged: artifact.merged_deltas || 0,
      conflicts: artifact.conflicts || 0,
      drift,
      budget_remaining: {
        time: this.config.budget.time - this.budgetUsed.time,
        steps: this.config.budget.steps - this.budgetUsed.steps,
        bytes: this.config.budget.bytes - this.budgetUsed.bytes,
      },
      t_ns,
    };

    // Convert BigInt to string for JSON serialization
    const serializableData = JSON.stringify(receiptData, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
    const hash = await blake3(serializableData);

    const receipt = SwarmReceiptSchema.parse({
      ...receiptData,
      hash,
      parent_hash: parentHash,
    });

    this.receipts.push(receipt);
    return receipt;
  }

  /**
   * Run single epoch
   * @param {Function} probeExecutor - Function to execute probes
   * @returns {Promise<{artifact: Object, receipt: SwarmReceipt, stopped: boolean}>}
   */
  async runEpoch(probeExecutor) {
    this.epoch++;
    this.vectorClock.increment();

    // Execute probes in parallel for all agents
    const probePromises = [];

    for (const [agentId, agent] of this.agents) {
      const probe = this.chooseProbe();
      if (probe) {
        probePromises.push(
          this.executeProbe(agentId, probe, probeExecutor)
        );
      }
    }

    await Promise.all(probePromises);

    // Project observations to artifact
    const artifact = await this.project();

    // Calculate drift
    const drift = this.calculateDrift(artifact, this.previousArtifact);
    this.previousArtifact = artifact;

    // Generate receipt
    const receipt = await this.generateReceipt(artifact, drift);

    // Check stopping condition
    const stopped = this.shouldStop(drift);

    return { artifact, receipt, stopped };
  }

  /**
   * Run full swarm execution
   *
   * Execution:
   *   O := ⊥
   *   while ¬stop:
   *     parallel ∀ α∈𝔄:
   *       p := choose(P)
   *       o := Obs(p,E,X,R,N_allow)
   *       O := O ⊔ o
   *     A := μ(O)
   *
   * @param {Function} probeExecutor
   * @param {Probe[]} initialProbes
   * @returns {Promise<{artifact: Object, receipts: SwarmReceipt[], epochs: number}>}
   */
  async run(probeExecutor, initialProbes = []) {
    this.initializeAgents();

    if (initialProbes.length > 0) {
      this.addProbes(initialProbes);
    }

    let artifact;
    let stopped = false;

    while (!stopped) {
      const result = await this.runEpoch(probeExecutor);
      artifact = result.artifact;
      stopped = result.stopped;
    }

    return {
      artifact,
      receipts: this.receipts,
      epochs: this.epoch,
    };
  }

  /**
   * Get final swarm state
   * @returns {Object}
   */
  getState() {
    return {
      epoch: this.epoch,
      agents: Array.from(this.agents.keys()),
      observations: this.observations.length,
      receipts: this.receipts.length,
      budget_used: { ...this.budgetUsed },
      budget_remaining: {
        time: this.config.budget.time - this.budgetUsed.time,
        steps: this.config.budget.steps - this.budgetUsed.steps,
        bytes: this.config.budget.bytes - this.budgetUsed.bytes,
      },
    };
  }
}

/**
 * Create a 10-agent swarm with default configuration
 * @param {Object} [options]
 * @returns {SwarmOrchestrator}
 */
export function createSwarm10(options = {}) {
  const agents = [];

  for (let i = 1; i <= 10; i++) {
    agents.push({
      id: `α_${i}`,
      scope: {
        subjects: [`http://kgc.io/agent/${i}`],
        graphs: [`http://kgc.io/graph/${i}`],
      },
      priority: i,
      capabilities: ['read', 'write', 'query'],
    });
  }

  return new SwarmOrchestrator({
    agents,
    budget: options.budget || {
      time: 30000,
      steps: 100,
      bytes: 10 * 1024 * 1024,
    },
    drift_epsilon: options.drift_epsilon || 0.01,
    min_epochs: options.min_epochs || 3,
  });
}

export default SwarmOrchestrator;
