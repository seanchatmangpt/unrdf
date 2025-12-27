/**
 * Information-Theoretic Scheduler
 *
 * Order / scheduling:
 *   Λ is ≺-total over candidate probes P
 *   choose(p) := argmax_{p∈P} ρ(p)
 *   ρ(p) := Δ̂(p) / cost(p)  // exploit highest yield per cost
 *   P dynamically expands: P := P ⊔ newProbes(O)
 *
 * @module @unrdf/kgc-claude/info-scheduler
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';

/**
 * Probe priority schema
 */
export const ProbePrioritySchema = z.object({
  id: z.string(),
  type: z.string(),
  target: z.string(),
  cost: z.number().positive(),
  expected_yield: z.number().min(0).max(1),
  rho: z.number(), // yield/cost ratio
  priority_rank: z.number().int(),
});

/**
 * @typedef {z.infer<typeof ProbePrioritySchema>} ProbePriority
 */

/**
 * Scheduler state schema
 */
export const SchedulerStateSchema = z.object({
  total_probes: z.number().int(),
  completed_probes: z.number().int(),
  total_cost: z.number(),
  total_yield: z.number(),
  average_rho: z.number(),
  efficiency: z.number(), // actual yield / expected yield
});

/**
 * @typedef {z.infer<typeof SchedulerStateSchema>} SchedulerState
 */

/**
 * Information-theoretic scheduler
 * Implements Λ ordering over probes
 */
export class InfoScheduler {
  constructor() {
    /** @type {Map<string, ProbePriority>} */
    this.probes = new Map();
    /** @type {ProbePriority[]} */
    this.completed = [];
    this.totalCost = 0;
    this.totalYield = 0;
    this.expectedYield = 0;
  }

  /**
   * Compute ρ(p) = Δ̂(p) / cost(p)
   * @param {Object} probe
   * @returns {number}
   */
  computeRho(probe) {
    return probe.expected_yield / Math.max(probe.cost, 0.001);
  }

  /**
   * Add probe to queue
   * P := P ⊔ {p}
   * @param {Object} probe
   */
  addProbe(probe) {
    const rho = this.computeRho(probe);

    const priority = ProbePrioritySchema.parse({
      id: probe.id,
      type: probe.type,
      target: probe.target,
      cost: probe.cost,
      expected_yield: probe.expected_yield,
      rho,
      priority_rank: 0, // computed on demand
    });

    this.probes.set(probe.id, priority);
    this.recomputeRanks();
  }

  /**
   * Add multiple probes
   * P := P ⊔ newProbes(O)
   * @param {Object[]} probes
   */
  addProbes(probes) {
    for (const probe of probes) {
      const rho = this.computeRho(probe);

      const priority = ProbePrioritySchema.parse({
        id: probe.id,
        type: probe.type,
        target: probe.target,
        cost: probe.cost,
        expected_yield: probe.expected_yield,
        rho,
        priority_rank: 0,
      });

      this.probes.set(probe.id, priority);
    }
    this.recomputeRanks();
  }

  /**
   * Recompute priority ranks based on ρ
   * Λ is ≺-total over P
   */
  recomputeRanks() {
    const sorted = [...this.probes.values()].sort((a, b) => b.rho - a.rho);

    for (let i = 0; i < sorted.length; i++) {
      const probe = this.probes.get(sorted[i].id);
      if (probe) {
        probe.priority_rank = i + 1;
      }
    }
  }

  /**
   * Choose best probe
   * choose(p) := argmax_{p∈P} ρ(p)
   * @returns {ProbePriority | null}
   */
  choose() {
    if (this.probes.size === 0) return null;

    let best = null;
    let bestRho = -Infinity;

    for (const probe of this.probes.values()) {
      if (probe.rho > bestRho) {
        bestRho = probe.rho;
        best = probe;
      }
    }

    return best;
  }

  /**
   * Choose top-k probes for parallel execution
   * @param {number} k
   * @returns {ProbePriority[]}
   */
  chooseK(k) {
    const sorted = [...this.probes.values()].sort((a, b) => b.rho - a.rho);
    return sorted.slice(0, k);
  }

  /**
   * Mark probe as completed
   * @param {string} probeId
   * @param {number} actualYield - measured information gain
   */
  complete(probeId, actualYield) {
    const probe = this.probes.get(probeId);
    if (!probe) return;

    this.totalCost += probe.cost;
    this.totalYield += actualYield;
    this.expectedYield += probe.expected_yield;

    this.completed.push(probe);
    this.probes.delete(probeId);
  }

  /**
   * Get queue size
   * |P|
   * @returns {number}
   */
  queueSize() {
    return this.probes.size;
  }

  /**
   * Get scheduler state
   * @returns {SchedulerState}
   */
  getState() {
    const avgRho = this.completed.length > 0
      ? this.completed.reduce((sum, p) => sum + p.rho, 0) / this.completed.length
      : 0;

    const efficiency = this.expectedYield > 0
      ? this.totalYield / this.expectedYield
      : 1;

    return SchedulerStateSchema.parse({
      total_probes: this.probes.size + this.completed.length,
      completed_probes: this.completed.length,
      total_cost: this.totalCost,
      total_yield: this.totalYield,
      average_rho: avgRho,
      efficiency,
    });
  }

  /**
   * Adaptive yield estimation
   * Update expected yields based on historical performance
   * @param {string} probeType
   * @returns {number}
   */
  estimateYield(probeType) {
    const typeProbes = this.completed.filter(p => p.type === probeType);

    if (typeProbes.length === 0) {
      return 0.5; // Default prior
    }

    // Use exponential moving average of actual yields
    const alpha = 0.3;
    let estimate = 0.5;

    for (const probe of typeProbes) {
      const actualYield = probe.expected_yield; // Would need actual yield tracking
      estimate = alpha * actualYield + (1 - alpha) * estimate;
    }

    return estimate;
  }

  /**
   * Generate new probes based on observations
   * P := P ⊔ newProbes(O)
   * @param {Object[]} observations
   * @param {Function} probeGenerator
   * @returns {Object[]}
   */
  generateProbes(observations, probeGenerator) {
    const newProbes = [];

    for (const obs of observations) {
      const generated = probeGenerator(obs);
      if (generated && generated.length > 0) {
        newProbes.push(...generated);
      }
    }

    this.addProbes(newProbes);
    return newProbes;
  }

  /**
   * Clear completed and reset
   */
  reset() {
    this.probes.clear();
    this.completed = [];
    this.totalCost = 0;
    this.totalYield = 0;
    this.expectedYield = 0;
  }
}

/**
 * Create scheduler with initial probes
 * @param {Object[]} probes
 * @returns {InfoScheduler}
 */
export function createScheduler(probes = []) {
  const scheduler = new InfoScheduler();
  if (probes.length > 0) {
    scheduler.addProbes(probes);
  }
  return scheduler;
}

export default InfoScheduler;
