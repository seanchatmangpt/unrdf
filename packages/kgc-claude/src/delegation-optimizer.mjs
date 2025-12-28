/**
 * Delegation Optimizer - Smart task routing and scheduling
 *
 * Matches tasks to optimal agent types using:
 * - Capability matching
 * - Load balancing
 * - Priority scheduling
 * - Cost optimization
 *
 * Law:
 *   assign(task, agents) := argmax_{α ∈ agents} score(α, task)
 *   score(α, task) := capability_match × availability × (1/cost)
 *
 * @module @unrdf/kgc-claude/delegation-optimizer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, VectorClock } from '@unrdf/kgc-4d';

/**
 * Task schema
 */
export const TaskSchema = z.object({
  id: z.string(),
  type: z.string(),
  required_capabilities: z.array(z.string()).default([]),
  priority: z.number().int().min(0).max(10).default(5),
  estimated_cost: z.number().positive().default(1),
  timeout_ms: z.number().int().positive().default(30000),
  payload: z.any().optional(),
});

/**
 * @typedef {z.infer<typeof TaskSchema>} Task
 */

/**
 * Agent capability schema
 */
export const AgentCapabilitySchema = z.object({
  agent_id: z.string(),
  capabilities: z.array(z.string()),
  priority: z.number().int().min(0).max(10).default(5),
  max_concurrent_tasks: z.number().int().positive().default(5),
  cost_factor: z.number().positive().default(1.0),
  current_load: z.number().int().min(0).default(0),
  success_rate: z.number().min(0).max(1).default(1.0),
});

/**
 * @typedef {z.infer<typeof AgentCapabilitySchema>} AgentCapability
 */

/**
 * Assignment schema
 */
export const AssignmentSchema = z.object({
  task_id: z.string(),
  agent_id: z.string(),
  score: z.number(),
  reason: z.string(),
  timestamp: z.bigint(),
});

/**
 * @typedef {z.infer<typeof AssignmentSchema>} Assignment
 */

/**
 * Scheduling strategy schema
 */
export const SchedulingStrategySchema = z.enum([
  'greedy',         // Assign to best available immediately
  'round_robin',    // Distribute evenly
  'priority_first', // High priority tasks first
  'load_balanced',  // Balance load across agents
  'cost_optimized', // Minimize total cost
]);

/**
 * Optimizer configuration
 */
export const OptimizerConfigSchema = z.object({
  strategy: SchedulingStrategySchema.default('greedy'),
  capability_weight: z.number().min(0).max(1).default(0.5),
  availability_weight: z.number().min(0).max(1).default(0.3),
  cost_weight: z.number().min(0).max(1).default(0.2),
  enable_load_balancing: z.boolean().default(true),
  rebalance_threshold: z.number().min(0).max(1).default(0.3),
});

/**
 * @typedef {z.infer<typeof OptimizerConfigSchema>} OptimizerConfig
 */

/**
 * DelegationOptimizer - Smart task routing
 */
export class DelegationOptimizer {
  /**
   * @param {OptimizerConfig} config
   */
  constructor(config = {}) {
    this.config = OptimizerConfigSchema.parse(config);
    this.agents = new Map();
    this.taskQueue = [];
    this.assignments = [];
    this.vectorClock = new VectorClock('delegation-optimizer');
  }

  /**
   * Register an agent with capabilities
   * @param {AgentCapability} agent
   */
  registerAgent(agent) {
    const validated = AgentCapabilitySchema.parse(agent);
    this.agents.set(validated.agent_id, validated);
  }

  /**
   * Unregister an agent
   * @param {string} agentId
   */
  unregisterAgent(agentId) {
    this.agents.delete(agentId);
  }

  /**
   * Add task to queue
   * @param {Task} task
   */
  enqueueTask(task) {
    const validated = TaskSchema.parse(task);
    this.taskQueue.push(validated);

    // Sort queue by priority
    this.taskQueue.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Assign task to optimal agent
   * @param {Task} task
   * @returns {Promise<Assignment | null>}
   */
  async assignTask(task) {
    const validated = TaskSchema.parse(task);
    this.vectorClock.increment();

    // Filter eligible agents
    const eligible = this._getEligibleAgents(validated);

    if (eligible.length === 0) {
      return null; // No eligible agents
    }

    // Score agents
    const scored = await Promise.all(
      eligible.map(async (agent) => ({
        agent,
        score: await this._scoreAgent(agent, validated),
      }))
    );

    // Select best agent
    scored.sort((a, b) => b.score - a.score);
    const best = scored[0];

    if (best.score === 0) {
      return null; // No suitable agent
    }

    // Create assignment
    const assignment = AssignmentSchema.parse({
      task_id: validated.id,
      agent_id: best.agent.agent_id,
      score: best.score,
      reason: this._explainScore(best.agent, validated, best.score),
      timestamp: now(),
    });

    // Update agent load
    best.agent.current_load++;

    this.assignments.push(assignment);
    return assignment;
  }

  /**
   * Process entire task queue
   * @returns {Promise<{assignments: Assignment[], unassigned: Task[]}>}
   */
  async processQueue() {
    const assignments = [];
    const unassigned = [];

    while (this.taskQueue.length > 0) {
      const task = this.taskQueue.shift();
      const assignment = await this.assignTask(task);

      if (assignment) {
        assignments.push(assignment);
      } else {
        unassigned.push(task);
      }
    }

    return { assignments, unassigned };
  }

  /**
   * Release agent from task (reduce load)
   * @param {string} agentId
   */
  releaseAgent(agentId) {
    const agent = this.agents.get(agentId);
    if (agent && agent.current_load > 0) {
      agent.current_load--;
    }
  }

  /**
   * Update agent success rate
   * @param {string} agentId
   * @param {boolean} success
   */
  updateSuccessRate(agentId, success) {
    const agent = this.agents.get(agentId);
    if (!agent) return;

    // Exponential moving average
    const alpha = 0.1;
    agent.success_rate =
      alpha * (success ? 1 : 0) + (1 - alpha) * agent.success_rate;
  }

  /**
   * Rebalance load across agents
   * @returns {number} Number of tasks rebalanced
   */
  rebalanceLoad() {
    if (!this.config.enable_load_balancing) {
      return 0;
    }

    const agents = Array.from(this.agents.values());
    if (agents.length < 2) {
      return 0;
    }

    // Calculate load statistics
    const loads = agents.map(a => a.current_load / a.max_concurrent_tasks);
    const avgLoad = loads.reduce((sum, l) => sum + l, 0) / loads.length;
    const maxLoad = Math.max(...loads);

    const imbalance = maxLoad - avgLoad;

    if (imbalance < this.config.rebalance_threshold) {
      return 0; // Already balanced
    }

    // Find overloaded and underloaded agents
    const overloaded = agents.filter(
      a => a.current_load / a.max_concurrent_tasks > avgLoad + 0.1
    );

    const underloaded = agents.filter(
      a => a.current_load / a.max_concurrent_tasks < avgLoad - 0.1
    );

    let rebalanced = 0;

    // Move tasks from overloaded to underloaded
    for (const over of overloaded) {
      for (const under of underloaded) {
        if (over.current_load > under.current_load + 1) {
          over.current_load--;
          under.current_load++;
          rebalanced++;
        }
      }
    }

    return rebalanced;
  }

  /**
   * Get eligible agents for task
   * @private
   */
  _getEligibleAgents(task) {
    const agents = Array.from(this.agents.values());

    return agents.filter((agent) => {
      // Check capability match
      const hasCapabilities = task.required_capabilities.every((cap) =>
        agent.capabilities.includes(cap)
      );

      // Check availability
      const isAvailable = agent.current_load < agent.max_concurrent_tasks;

      return hasCapabilities && isAvailable;
    });
  }

  /**
   * Score agent for task
   * @private
   */
  async _scoreAgent(agent, task) {
    // Capability score (exact match = 1.0, partial = 0.5)
    const capabilityScore =
      task.required_capabilities.length === 0
        ? 1.0
        : task.required_capabilities.filter((cap) =>
            agent.capabilities.includes(cap)
          ).length / task.required_capabilities.length;

    // Availability score (based on current load)
    const availabilityScore =
      1 - agent.current_load / agent.max_concurrent_tasks;

    // Cost score (inverse cost factor)
    const costScore = 1 / (agent.cost_factor * task.estimated_cost);

    // Success rate bonus
    const successBonus = agent.success_rate;

    // Weighted combination
    const score =
      this.config.capability_weight * capabilityScore +
      this.config.availability_weight * availabilityScore +
      this.config.cost_weight * costScore;

    return score * successBonus;
  }

  /**
   * Explain score for assignment
   * @private
   */
  _explainScore(agent, task, score) {
    const reasons = [];

    const capMatch = task.required_capabilities.every((cap) =>
      agent.capabilities.includes(cap)
    );

    if (capMatch) {
      reasons.push('capability_match');
    }

    const load = agent.current_load / agent.max_concurrent_tasks;
    if (load < 0.5) {
      reasons.push('low_load');
    }

    if (agent.cost_factor < 1.5) {
      reasons.push('cost_efficient');
    }

    if (agent.success_rate > 0.8) {
      reasons.push('high_success_rate');
    }

    return reasons.join(', ') || 'default_assignment';
  }

  /**
   * Get optimizer statistics
   * @returns {Object}
   */
  getStatistics() {
    const agents = Array.from(this.agents.values());

    const totalLoad = agents.reduce((sum, a) => sum + a.current_load, 0);
    const totalCapacity = agents.reduce(
      (sum, a) => sum + a.max_concurrent_tasks,
      0
    );

    const avgSuccessRate =
      agents.reduce((sum, a) => sum + a.success_rate, 0) / agents.length;

    return {
      agent_count: agents.length,
      total_assignments: this.assignments.length,
      queued_tasks: this.taskQueue.length,
      total_load: totalLoad,
      total_capacity: totalCapacity,
      utilization: totalCapacity > 0 ? totalLoad / totalCapacity : 0,
      avg_success_rate: avgSuccessRate || 0,
    };
  }

  /**
   * Get assignment history
   * @returns {Assignment[]}
   */
  getAssignments() {
    return [...this.assignments];
  }

  /**
   * Clear all state
   */
  clear() {
    this.agents.clear();
    this.taskQueue = [];
    this.assignments = [];
    this.vectorClock = new VectorClock('delegation-optimizer');
  }
}

/**
 * Create delegation optimizer
 * @param {OptimizerConfig} [config]
 * @returns {DelegationOptimizer}
 */
export function createDelegationOptimizer(config) {
  return new DelegationOptimizer(config);
}

/**
 * Create optimizer with default agents
 * @param {AgentCapability[]} agents
 * @param {OptimizerConfig} [config]
 * @returns {DelegationOptimizer}
 */
export function createOptimizerWithAgents(agents, config) {
  const optimizer = new DelegationOptimizer(config);

  for (const agent of agents) {
    optimizer.registerAgent(agent);
  }

  return optimizer;
}

export default DelegationOptimizer;
