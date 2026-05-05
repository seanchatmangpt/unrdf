/**
 * Hierarchical Delegation - Multi-level agent hierarchies
 *
 * Enables parent agents to spawn child agents, creating tree structures
 * for complex task decomposition with result bubbling and failure propagation.
 *
 * Pattern:
 *   Parent α_p spawns children {α_c1, α_c2, ...}
 *   Each child reports results to parent
 *   Parent aggregates and bubbles to coordinator
 *
 * Law:
 *   Result(α_p) = Aggregate(Result(α_c1), ..., Result(α_cn))
 *   Failure(α_ci) → Propagate(α_p) [configurable]
 *
 * @module @unrdf/kgc-claude/hierarchical-delegation
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, VectorClock } from '@unrdf/kgc-4d';
import { AgentHarness, AgentConfigSchema } from './agent-harness.mjs';
import { createShard, mergeDeltas, getPendingDeltas } from './shard-merge.mjs';

/**
 * Delegation level schema
 */
export const DelegationLevelSchema = z.enum([
  'root',      // Top-level coordinator
  'parent',    // Can spawn children
  'child',     // Spawned by parent
  'leaf',      // Cannot spawn further
]);

/**
 * Aggregation strategy schema
 */
export const AggregationStrategySchema = z.enum([
  'union',        // Merge all child results
  'consensus',    // Require majority agreement
  'first_success', // Use first successful result
  'all_or_nothing', // All must succeed or fail
]);

/**
 * Failure propagation schema
 */
export const FailurePropagationSchema = z.enum([
  'immediate',   // Fail parent on first child failure
  'collect',     // Collect all failures, fail at end
  'ignore',      // Continue despite failures
  'retry',       // Retry failed children
]);

/**
 * Hierarchy node configuration
 */
export const HierarchyNodeSchema = z.object({
  id: z.string(),
  level: DelegationLevelSchema,
  parent_id: z.string().optional(),
  children_ids: z.array(z.string()).default([]),
  agent_config: AgentConfigSchema,
  max_children: z.number().int().min(0).max(100).default(10),
  max_depth: z.number().int().min(1).max(10).default(3),
  current_depth: z.number().int().min(0).default(0),
});

/**
 * @typedef {z.infer<typeof HierarchyNodeSchema>} HierarchyNode
 */

/**
 * Delegation policy schema
 */
export const DelegationPolicySchema = z.object({
  aggregation: AggregationStrategySchema.default('union'),
  failure_propagation: FailurePropagationSchema.default('immediate'),
  min_children_success: z.number().int().min(0).default(1),
  allow_partial_results: z.boolean().default(true),
  result_timeout_ms: z.number().int().positive().default(30000),
});

/**
 * @typedef {z.infer<typeof DelegationPolicySchema>} DelegationPolicy
 */

/**
 * Delegation result schema
 */
export const DelegationResultSchema = z.object({
  node_id: z.string(),
  level: DelegationLevelSchema,
  success: z.boolean(),
  children_spawned: z.number().int(),
  children_succeeded: z.number().int(),
  children_failed: z.number().int(),
  aggregated_observations: z.number().int(),
  aggregated_deltas: z.number().int(),
  result_hash: z.string(),
  error: z.string().optional(),
  child_results: z.array(z.any()).default([]),
  duration_ms: z.number(),
});

/**
 * @typedef {z.infer<typeof DelegationResultSchema>} DelegationResult
 */

/**
 * HierarchicalDelegator - Manages multi-level agent hierarchies
 */
export class HierarchicalDelegator {
  /**
   * @param {DelegationPolicy} policy
   */
  constructor(policy = {}) {
    this.policy = DelegationPolicySchema.parse(policy);
    this.nodes = new Map();
    this.results = new Map();
    this.vectorClock = new VectorClock('hierarchy-delegator');
  }

  /**
   * Register a node in the hierarchy
   * @param {HierarchyNode} node
   */
  registerNode(node) {
    const validated = HierarchyNodeSchema.parse(node);

    // Verify depth constraints
    if (validated.parent_id) {
      const parent = this.nodes.get(validated.parent_id);
      if (parent) {
        validated.current_depth = parent.current_depth + 1;

        if (validated.current_depth > validated.max_depth) {
          throw new Error(
            `Max depth exceeded: ${validated.current_depth} > ${validated.max_depth}`
          );
        }

        // Update parent's children list
        if (!parent.children_ids.includes(validated.id)) {
          parent.children_ids.push(validated.id);
        }

        if (parent.children_ids.length > parent.max_children) {
          throw new Error(
            `Max children exceeded for ${parent.id}: ${parent.children_ids.length} > ${parent.max_children}`
          );
        }
      }
    }

    this.nodes.set(validated.id, validated);
    return validated;
  }

  /**
   * Spawn child agents from parent
   * @param {string} parentId
   * @param {Object[]} childConfigs
   * @returns {Promise<{children: AgentHarness[], parent: HierarchyNode}>}
   */
  async spawnChildren(parentId, childConfigs) {
    const parent = this.nodes.get(parentId);
    if (!parent) {
      throw new Error(`Parent node not found: ${parentId}`);
    }

    if (parent.level === 'leaf') {
      throw new Error(`Leaf nodes cannot spawn children: ${parentId}`);
    }

    const children = [];

    for (const config of childConfigs) {
      const childId = `${parentId}-child-${children.length + 1}`;

      // Create child node
      const childNode = this.registerNode({
        id: childId,
        level: parent.current_depth + 1 >= parent.max_depth ? 'leaf' : 'child',
        parent_id: parentId,
        agent_config: {
          ...config,
          id: childId,
        },
        max_children: parent.max_children,
        max_depth: parent.max_depth,
      });

      // Create agent harness
      const agent = new AgentHarness(childNode.agent_config);
      children.push(agent);
    }

    return { children, parent };
  }

  /**
   * Execute hierarchical delegation
   * @param {string} rootId
   * @param {Function} taskExecutor
   * @param {Object[]} initialTasks
   * @returns {Promise<DelegationResult>}
   */
  async execute(rootId, taskExecutor, initialTasks = []) {
    const startTime = Date.now();
    const root = this.nodes.get(rootId);

    if (!root) {
      throw new Error(`Root node not found: ${rootId}`);
    }

    this.vectorClock.increment();

    try {
      const result = await this._executeNode(
        rootId,
        taskExecutor,
        initialTasks,
        startTime
      );

      this.results.set(rootId, result);
      return result;
    } catch (error) {
      const failureResult = await this._createFailureResult(
        rootId,
        error.message,
        Date.now() - startTime
      );

      this.results.set(rootId, failureResult);
      return failureResult;
    }
  }

  /**
   * Execute a single node (recursive)
   * @private
   */
  async _executeNode(nodeId, taskExecutor, tasks, startTime) {
    const node = this.nodes.get(nodeId);
    const agent = new AgentHarness(node.agent_config);

    // Convert tasks to probes
    const probes = tasks.map((task, i) => ({
      id: `probe-${nodeId}-${i}`,
      type: task.type || 'transform',
      target: task.target || '',
      params: task.params || {},
      cost: task.cost || 1,
      expected_yield: task.expected_yield || 0.5,
    }));

    // Execute this node's tasks
    const nodeResult = await agent.run(taskExecutor, probes);

    const childResults = [];
    let childrenSpawned = 0;
    let childrenSucceeded = 0;
    let childrenFailed = 0;

    // If node has children, execute them recursively
    if (node.children_ids.length > 0) {
      for (const childId of node.children_ids) {
        childrenSpawned++;

        try {
          const childResult = await this._executeNode(
            childId,
            taskExecutor,
            [], // Children get their own tasks
            startTime
          );

          childResults.push(childResult);

          if (childResult.success) {
            childrenSucceeded++;
          } else {
            childrenFailed++;

            // Check failure propagation
            if (this.policy.failure_propagation === 'immediate') {
              throw new Error(
                `Child ${childId} failed: ${childResult.error}`
              );
            }
          }
        } catch (error) {
          childrenFailed++;

          if (this.policy.failure_propagation === 'immediate') {
            throw error;
          }

          childResults.push({
            node_id: childId,
            success: false,
            error: error.message,
          });
        }
      }
    }

    // Aggregate results
    const aggregated = await this._aggregateResults(
      nodeResult,
      childResults
    );

    // Check success criteria
    const success = this._checkSuccess(
      childrenSucceeded,
      childrenFailed,
      childrenSpawned
    );

    const resultHash = await blake3(
      JSON.stringify({
        node_id: nodeId,
        success,
        aggregated,
        children: childResults.length,
      })
    );

    return DelegationResultSchema.parse({
      node_id: nodeId,
      level: node.level,
      success,
      children_spawned: childrenSpawned,
      children_succeeded: childrenSucceeded,
      children_failed: childrenFailed,
      aggregated_observations: aggregated.observations,
      aggregated_deltas: aggregated.deltas,
      result_hash: resultHash,
      child_results: childResults,
      duration_ms: Date.now() - startTime,
    });
  }

  /**
   * Aggregate results from node and children
   * @private
   */
  async _aggregateResults(nodeResult, childResults) {
    let totalObservations = nodeResult.observations || 0;
    let totalDeltas = nodeResult.deltas || 0;

    for (const child of childResults) {
      totalObservations += child.aggregated_observations || 0;
      totalDeltas += child.aggregated_deltas || 0;
    }

    return {
      observations: totalObservations,
      deltas: totalDeltas,
    };
  }

  /**
   * Check if execution succeeded based on policy
   * @private
   */
  _checkSuccess(succeeded, failed, spawned) {
    if (spawned === 0) {
      return true; // Leaf node
    }

    switch (this.policy.aggregation) {
      case 'all_or_nothing':
        return failed === 0;

      case 'first_success':
        return succeeded >= 1;

      case 'consensus':
        return succeeded > spawned / 2;

      case 'union':
      default:
        return succeeded >= this.policy.min_children_success;
    }
  }

  /**
   * Create failure result
   * @private
   */
  async _createFailureResult(nodeId, error, durationMs) {
    const node = this.nodes.get(nodeId);
    const resultHash = await blake3(
      JSON.stringify({ node_id: nodeId, error, timestamp: Date.now() })
    );

    return DelegationResultSchema.parse({
      node_id: nodeId,
      level: node?.level || 'root',
      success: false,
      children_spawned: 0,
      children_succeeded: 0,
      children_failed: 0,
      aggregated_observations: 0,
      aggregated_deltas: 0,
      result_hash: resultHash,
      error,
      child_results: [],
      duration_ms: durationMs,
    });
  }

  /**
   * Get hierarchy tree
   * @returns {Object}
   */
  getTree() {
    const buildTree = (nodeId) => {
      const node = this.nodes.get(nodeId);
      if (!node) return null;

      return {
        id: node.id,
        level: node.level,
        depth: node.current_depth,
        children: node.children_ids.map(buildTree).filter(Boolean),
      };
    };

    // Find root nodes
    const roots = Array.from(this.nodes.values())
      .filter(n => !n.parent_id)
      .map(n => buildTree(n.id));

    return roots;
  }

  /**
   * Get result for node
   * @param {string} nodeId
   * @returns {DelegationResult | undefined}
   */
  getResult(nodeId) {
    return this.results.get(nodeId);
  }

  /**
   * Clear all state
   */
  clear() {
    this.nodes.clear();
    this.results.clear();
    this.vectorClock = new VectorClock('hierarchy-delegator');
  }
}

/**
 * Create hierarchical delegator
 * @param {DelegationPolicy} [policy]
 * @returns {HierarchicalDelegator}
 */
export function createHierarchicalDelegator(policy) {
  return new HierarchicalDelegator(policy);
}

/**
 * Create a simple 3-level hierarchy (root → parents → leaves)
 * @param {Object} config
 * @returns {HierarchicalDelegator}
 */
export function create3LevelHierarchy(config = {}) {
  const delegator = new HierarchicalDelegator(config.policy);

  const {
    rootConfig = { id: 'root', scope: { files: [] } },
    parentCount = 3,
    childrenPerParent = 3,
  } = config;

  // Register root
  delegator.registerNode({
    id: rootConfig.id,
    level: 'root',
    agent_config: rootConfig,
    max_children: parentCount,
    max_depth: 3,
    current_depth: 0,
  });

  // Register parents
  for (let i = 0; i < parentCount; i++) {
    const parentId = `${rootConfig.id}-parent-${i + 1}`;

    delegator.registerNode({
      id: parentId,
      level: 'parent',
      parent_id: rootConfig.id,
      agent_config: {
        id: parentId,
        scope: { files: [`parent-${i + 1}/**`] },
      },
      max_children: childrenPerParent,
      max_depth: 3,
    });

    // Register children for this parent
    for (let j = 0; j < childrenPerParent; j++) {
      const childId = `${parentId}-child-${j + 1}`;

      delegator.registerNode({
        id: childId,
        level: 'leaf',
        parent_id: parentId,
        agent_config: {
          id: childId,
          scope: { files: [`parent-${i + 1}/child-${j + 1}/**`] },
        },
        max_children: 0,
        max_depth: 3,
      });
    }
  }

  return delegator;
}

export default HierarchicalDelegator;
