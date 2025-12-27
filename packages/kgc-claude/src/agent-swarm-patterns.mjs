/**
 * Agent Swarm Patterns - Advanced coordination primitives
 *
 * Implements hyper-advanced swarm patterns:
 * - Fan-out/fan-in: Parallel task distribution and result gathering
 * - Pipeline chains: Sequential agent dependencies
 * - Consensus mechanisms: Multi-agent voting and agreement
 *
 * Pattern Library:
 *   FanOut: Task → {α₁, ..., αₙ} in parallel
 *   FanIn: {Result₁, ..., Resultₙ} → Aggregate
 *   Pipeline: α₁ → α₂ → ... → αₙ (sequential)
 *   Consensus: Majority(Result₁, ..., Resultₙ) → Decision
 *
 * @module @unrdf/kgc-claude/agent-swarm-patterns
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, VectorClock } from '@unrdf/kgc-4d';
import { AgentHarness } from './agent-harness.mjs';
import { mergeDeltas, getPendingDeltas } from './shard-merge.mjs';

/**
 * Pattern type schema
 */
export const PatternTypeSchema = z.enum([
  'fan_out_fan_in',
  'pipeline',
  'consensus',
  'broadcast',
  'reduce',
  'map_reduce',
]);

/**
 * Consensus strategy schema
 */
export const ConsensusStrategySchema = z.enum([
  'majority',        // >50% agreement
  'supermajority',   // ≥66% agreement
  'unanimous',       // 100% agreement
  'plurality',       // Most common result
  'weighted',        // Weight by agent priority
]);

/**
 * Pattern configuration schema
 */
export const PatternConfigSchema = z.object({
  type: PatternTypeSchema,
  agent_count: z.number().int().min(1).max(100),
  timeout_ms: z.number().int().positive().default(30000),
  allow_partial_results: z.boolean().default(true),
  min_success_count: z.number().int().min(1).optional(),
});

/**
 * @typedef {z.infer<typeof PatternConfigSchema>} PatternConfig
 */

/**
 * Pattern execution result schema
 */
export const PatternResultSchema = z.object({
  pattern: PatternTypeSchema,
  success: z.boolean(),
  agents_executed: z.number().int(),
  agents_succeeded: z.number().int(),
  agents_failed: z.number().int(),
  total_observations: z.number().int(),
  total_deltas: z.number().int(),
  consensus_achieved: z.boolean().optional(),
  consensus_value: z.any().optional(),
  result_hash: z.string(),
  duration_ms: z.number(),
  agent_results: z.array(z.any()).default([]),
});

/**
 * @typedef {z.infer<typeof PatternResultSchema>} PatternResult
 */

/**
 * AgentSwarmPatterns - Advanced coordination patterns
 */
export class AgentSwarmPatterns {
  constructor() {
    this.vectorClock = new VectorClock('swarm-patterns');
    this.executionHistory = [];
  }

  /**
   * Fan-out/fan-in pattern
   * Distributes task to N agents, waits for all, aggregates results
   *
   * @param {AgentHarness[]} agents
   * @param {Function} taskExecutor
   * @param {Object[]} tasks
   * @param {Object} [options]
   * @returns {Promise<PatternResult>}
   */
  async fanOutFanIn(agents, taskExecutor, tasks, options = {}) {
    const startTime = Date.now();
    const { timeout_ms = 30000, allow_partial_results = true } = options;

    this.vectorClock.increment();

    // Fan-out: Execute all agents in parallel
    const agentPromises = agents.map(async (agent, i) => {
      const agentTasks = tasks.filter((_, idx) => idx % agents.length === i);

      try {
        const result = await Promise.race([
          agent.run(taskExecutor, agentTasks),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error('Timeout')), timeout_ms)
          ),
        ]);

        return { success: true, agent_id: agent.config.id, result };
      } catch (error) {
        return {
          success: false,
          agent_id: agent.config.id,
          error: error.message,
        };
      }
    });

    // Fan-in: Gather all results
    const agentResults = await Promise.all(agentPromises);

    // Aggregate
    const succeeded = agentResults.filter(r => r.success);
    const failed = agentResults.filter(r => !r.success);

    const totalObservations = succeeded.reduce(
      (sum, r) => sum + (r.result?.observations || 0),
      0
    );

    const totalDeltas = succeeded.reduce(
      (sum, r) => sum + (r.result?.deltas || 0),
      0
    );

    const success =
      allow_partial_results
        ? succeeded.length > 0
        : failed.length === 0;

    const resultHash = await blake3(
      JSON.stringify({
        pattern: 'fan_out_fan_in',
        agents: agentResults.length,
        succeeded: succeeded.length,
        timestamp: Date.now(),
      })
    );

    const result = PatternResultSchema.parse({
      pattern: 'fan_out_fan_in',
      success,
      agents_executed: agents.length,
      agents_succeeded: succeeded.length,
      agents_failed: failed.length,
      total_observations: totalObservations,
      total_deltas: totalDeltas,
      result_hash: resultHash,
      duration_ms: Date.now() - startTime,
      agent_results: agentResults,
    });

    this.executionHistory.push(result);
    return result;
  }

  /**
   * Pipeline pattern
   * Executes agents sequentially, passing output to next
   *
   * @param {AgentHarness[]} agents
   * @param {Function} taskExecutor
   * @param {Object} initialInput
   * @param {Object} [options]
   * @returns {Promise<PatternResult>}
   */
  async pipeline(agents, taskExecutor, initialInput, options = {}) {
    const startTime = Date.now();
    this.vectorClock.increment();

    let currentInput = initialInput;
    const agentResults = [];
    let totalObservations = 0;
    let totalDeltas = 0;

    // Execute agents sequentially
    for (const agent of agents) {
      try {
        const tasks = [
          {
            id: `pipeline-${agent.config.id}`,
            type: 'transform',
            target: currentInput?.target || '',
            params: currentInput?.params || {},
            cost: 1,
            expected_yield: 0.5,
          },
        ];

        const result = await agent.run(taskExecutor, tasks);

        agentResults.push({
          success: true,
          agent_id: agent.config.id,
          result,
        });

        totalObservations += result.observations || 0;
        totalDeltas += result.deltas || 0;

        // Pass output to next stage
        currentInput = {
          target: result.final_hash,
          params: { previous_result: result },
        };
      } catch (error) {
        agentResults.push({
          success: false,
          agent_id: agent.config.id,
          error: error.message,
        });

        // Pipeline broken
        break;
      }
    }

    const succeeded = agentResults.filter(r => r.success);
    const failed = agentResults.filter(r => !r.success);

    const success = failed.length === 0;

    const resultHash = await blake3(
      JSON.stringify({
        pattern: 'pipeline',
        stages: agentResults.length,
        final_output: currentInput,
        timestamp: Date.now(),
      })
    );

    const result = PatternResultSchema.parse({
      pattern: 'pipeline',
      success,
      agents_executed: agentResults.length,
      agents_succeeded: succeeded.length,
      agents_failed: failed.length,
      total_observations: totalObservations,
      total_deltas: totalDeltas,
      result_hash: resultHash,
      duration_ms: Date.now() - startTime,
      agent_results: agentResults,
    });

    this.executionHistory.push(result);
    return result;
  }

  /**
   * Consensus pattern
   * Executes agents in parallel and determines consensus value
   *
   * @param {AgentHarness[]} agents
   * @param {Function} taskExecutor
   * @param {Object[]} tasks
   * @param {ConsensusStrategySchema} strategy
   * @param {Object} [options]
   * @returns {Promise<PatternResult>}
   */
  async consensus(
    agents,
    taskExecutor,
    tasks,
    strategy = 'majority',
    options = {}
  ) {
    const startTime = Date.now();
    const { timeout_ms = 30000 } = options;

    this.vectorClock.increment();

    // Execute all agents in parallel
    const agentPromises = agents.map(async (agent) => {
      try {
        const result = await Promise.race([
          agent.run(taskExecutor, tasks),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error('Timeout')), timeout_ms)
          ),
        ]);

        return {
          success: true,
          agent_id: agent.config.id,
          result,
          value: result.final_hash, // Use hash as vote
          priority: agent.config.priority || 1,
        };
      } catch (error) {
        return {
          success: false,
          agent_id: agent.config.id,
          error: error.message,
        };
      }
    });

    const agentResults = await Promise.all(agentPromises);
    const succeeded = agentResults.filter(r => r.success);
    const failed = agentResults.filter(r => !r.success);

    // Calculate consensus
    const { achieved, value } = this._calculateConsensus(
      succeeded,
      strategy
    );

    const totalObservations = succeeded.reduce(
      (sum, r) => sum + (r.result?.observations || 0),
      0
    );

    const totalDeltas = succeeded.reduce(
      (sum, r) => sum + (r.result?.deltas || 0),
      0
    );

    const resultHash = await blake3(
      JSON.stringify({
        pattern: 'consensus',
        strategy,
        achieved,
        value,
        timestamp: Date.now(),
      })
    );

    const result = PatternResultSchema.parse({
      pattern: 'consensus',
      success: achieved,
      agents_executed: agents.length,
      agents_succeeded: succeeded.length,
      agents_failed: failed.length,
      total_observations: totalObservations,
      total_deltas: totalDeltas,
      consensus_achieved: achieved,
      consensus_value: value,
      result_hash: resultHash,
      duration_ms: Date.now() - startTime,
      agent_results: agentResults,
    });

    this.executionHistory.push(result);
    return result;
  }

  /**
   * Map-reduce pattern
   * Maps tasks across agents (fan-out), reduces results (fan-in)
   *
   * @param {AgentHarness[]} agents
   * @param {Function} taskExecutor
   * @param {Object[]} tasks
   * @param {Function} reduceFunction
   * @param {Object} [options]
   * @returns {Promise<PatternResult>}
   */
  async mapReduce(agents, taskExecutor, tasks, reduceFunction, options = {}) {
    const startTime = Date.now();
    this.vectorClock.increment();

    // Map phase (fan-out)
    const mapResult = await this.fanOutFanIn(
      agents,
      taskExecutor,
      tasks,
      options
    );

    if (!mapResult.success) {
      return mapResult; // Return early if map failed
    }

    // Reduce phase
    const succeededResults = mapResult.agent_results
      .filter(r => r.success)
      .map(r => r.result);

    let reducedValue;
    try {
      reducedValue = await reduceFunction(succeededResults);
    } catch (error) {
      reducedValue = { error: error.message };
    }

    const resultHash = await blake3(
      JSON.stringify({
        pattern: 'map_reduce',
        reduced: reducedValue,
        timestamp: Date.now(),
      })
    );

    const result = PatternResultSchema.parse({
      pattern: 'map_reduce',
      success: mapResult.success && !reducedValue.error,
      agents_executed: mapResult.agents_executed,
      agents_succeeded: mapResult.agents_succeeded,
      agents_failed: mapResult.agents_failed,
      total_observations: mapResult.total_observations,
      total_deltas: mapResult.total_deltas,
      consensus_value: reducedValue,
      result_hash: resultHash,
      duration_ms: Date.now() - startTime,
      agent_results: mapResult.agent_results,
    });

    this.executionHistory.push(result);
    return result;
  }

  /**
   * Calculate consensus based on strategy
   * @private
   */
  _calculateConsensus(results, strategy) {
    if (results.length === 0) {
      return { achieved: false, value: null };
    }

    // Count votes
    const votes = new Map();
    for (const result of results) {
      const count = votes.get(result.value) || 0;
      votes.set(result.value, count + 1);
    }

    // Find most common value
    let maxCount = 0;
    let consensusValue = null;

    for (const [value, count] of votes.entries()) {
      if (count > maxCount) {
        maxCount = count;
        consensusValue = value;
      }
    }

    const totalVotes = results.length;
    const threshold = this._getConsensusThreshold(strategy, totalVotes);

    return {
      achieved: maxCount >= threshold,
      value: consensusValue,
    };
  }

  /**
   * Get consensus threshold based on strategy
   * @private
   */
  _getConsensusThreshold(strategy, total) {
    switch (strategy) {
      case 'unanimous':
        return total;
      case 'supermajority':
        return Math.ceil(total * 0.66);
      case 'majority':
        return Math.ceil(total / 2);
      case 'plurality':
        return 1;
      default:
        return Math.ceil(total / 2);
    }
  }

  /**
   * Get execution history
   * @returns {PatternResult[]}
   */
  getHistory() {
    return [...this.executionHistory];
  }

  /**
   * Clear execution history
   */
  clearHistory() {
    this.executionHistory = [];
  }
}

/**
 * Create swarm patterns coordinator
 * @returns {AgentSwarmPatterns}
 */
export function createSwarmPatterns() {
  return new AgentSwarmPatterns();
}

/**
 * Execute fan-out/fan-in (convenience function)
 * @param {AgentHarness[]} agents
 * @param {Function} taskExecutor
 * @param {Object[]} tasks
 * @param {Object} [options]
 * @returns {Promise<PatternResult>}
 */
export async function executeFanOutFanIn(agents, taskExecutor, tasks, options) {
  const patterns = new AgentSwarmPatterns();
  return patterns.fanOutFanIn(agents, taskExecutor, tasks, options);
}

/**
 * Execute pipeline (convenience function)
 * @param {AgentHarness[]} agents
 * @param {Function} taskExecutor
 * @param {Object} initialInput
 * @param {Object} [options]
 * @returns {Promise<PatternResult>}
 */
export async function executePipeline(agents, taskExecutor, initialInput, options) {
  const patterns = new AgentSwarmPatterns();
  return patterns.pipeline(agents, taskExecutor, initialInput, options);
}

/**
 * Execute consensus (convenience function)
 * @param {AgentHarness[]} agents
 * @param {Function} taskExecutor
 * @param {Object[]} tasks
 * @param {ConsensusStrategySchema} strategy
 * @param {Object} [options]
 * @returns {Promise<PatternResult>}
 */
export async function executeConsensus(
  agents,
  taskExecutor,
  tasks,
  strategy,
  options
) {
  const patterns = new AgentSwarmPatterns();
  return patterns.consensus(agents, taskExecutor, tasks, strategy, options);
}

export default AgentSwarmPatterns;
