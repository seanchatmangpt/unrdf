/**
 * @fileoverview Agent Router - Capability-based task routing
 *
 * **Purpose**: Route tasks to appropriate agents based on capabilities:
 * 1. Agent capability registration
 * 2. Task-to-agent matching
 * 3. Load-aware routing
 * 4. Agent specialization (α₁=observer, α₂=compressor, etc.)
 *
 * **Properties**:
 * - Role-based routing
 * - Capability matching
 * - Load balancing
 * - Fallback strategies
 *
 * @module orchestration/agent-router
 */

import { z } from 'zod';

/**
 * Agent role definitions (KGC-SWARM)
 */
export const AgentRole = {
  OBSERVER: 'α₁-observer',
  COMPRESSOR: 'α₂-compressor',
  VALIDATOR: 'α₃-validator',
  ORCHESTRATOR: 'α₄-orchestrator',
  ANALYZER: 'α₅-analyzer',
  OPTIMIZER: 'α₆-optimizer',
  MONITOR: 'α₇-monitor',
  AGGREGATOR: 'α₈-aggregator',
  EXECUTOR: 'α₉-executor',
  COORDINATOR: 'α₁₀-coordinator'
};

/**
 * Agent registration schema
 */
export const AgentRegistrationSchema = z.object({
  agentId: z.string(),
  role: z.string(),
  capabilities: z.array(z.string()).min(1),
  priority: z.number().default(0),
  maxConcurrent: z.number().default(1),
  metadata: z.record(z.any()).optional()
});

/**
 * Routing strategy enum
 */
export const RoutingStrategy = {
  CAPABILITY_MATCH: 'capability-match',
  ROLE_BASED: 'role-based',
  LOAD_BALANCED: 'load-balanced',
  ROUND_ROBIN: 'round-robin',
  PRIORITY: 'priority'
};

/**
 * Agent Router - Routes tasks to appropriate agents
 *
 * @class AgentRouter
 *
 * @example
 * const router = new AgentRouter();
 *
 * router.registerAgent({
 *   agentId: 'agent-1',
 *   role: AgentRole.OBSERVER,
 *   capabilities: ['observe', 'compress']
 * });
 *
 * const agents = router.route({
 *   requiredCapabilities: ['observe'],
 *   count: 1
 * });
 */
export class AgentRouter {
  /**
   * Create a new agent router
   *
   * @param {Object} [options] - Router options
   * @param {string} [options.strategy='capability-match'] - Default routing strategy
   */
  constructor(options = {}) {
    /** @type {Map<string, Object>} Registered agents */
    this.agents = new Map();

    /** @type {Map<string, number>} Agent current load */
    this.agentLoad = new Map();

    /** @type {string} Default routing strategy */
    this.defaultStrategy = options.strategy || RoutingStrategy.CAPABILITY_MATCH;

    /** @type {number} Round-robin counter */
    this.roundRobinCounter = 0;

    /** @type {Object} Router statistics */
    this.stats = {
      totalRegistrations: 0,
      totalUnregistrations: 0,
      totalRoutings: 0,
      routingFailures: 0
    };
  }

  /**
   * Register an agent
   *
   * @param {Object} registration - Agent registration
   * @returns {void}
   */
  registerAgent(registration) {
    const validated = AgentRegistrationSchema.parse(registration);

    this.agents.set(validated.agentId, validated);
    this.agentLoad.set(validated.agentId, 0);

    this.stats.totalRegistrations++;
  }

  /**
   * Unregister an agent
   *
   * @param {string} agentId - Agent ID
   * @returns {boolean} True if unregistered
   */
  unregisterAgent(agentId) {
    const deleted = this.agents.delete(agentId);
    this.agentLoad.delete(agentId);

    if (deleted) {
      this.stats.totalUnregistrations++;
    }

    return deleted;
  }

  /**
   * Route task to appropriate agent(s)
   *
   * @param {Object} request - Routing request
   * @param {string[]} request.requiredCapabilities - Required capabilities
   * @param {string} [request.preferredRole] - Preferred agent role
   * @param {number} [request.count=1] - Number of agents needed
   * @param {string} [request.strategy] - Routing strategy override
   * @returns {Object[]} Selected agents
   */
  route(request) {
    const {
      requiredCapabilities = [],
      preferredRole,
      count = 1,
      strategy = this.defaultStrategy
    } = request;

    this.stats.totalRoutings++;

    // Get matching agents
    let candidates = this._getMatchingAgents(requiredCapabilities, preferredRole);

    if (candidates.length === 0) {
      this.stats.routingFailures++;
      throw new Error(
        `No agents found for capabilities: ${requiredCapabilities.join(', ')}`
      );
    }

    // Apply routing strategy
    candidates = this._applyStrategy(candidates, strategy);

    // Select requested count
    const selected = candidates.slice(0, count);

    if (selected.length < count) {
      this.stats.routingFailures++;
      throw new Error(
        `Insufficient agents: requested ${count}, found ${selected.length}`
      );
    }

    // Increment load
    for (const agent of selected) {
      this.agentLoad.set(agent.agentId, this._getLoad(agent.agentId) + 1);
    }

    return selected;
  }

  /**
   * Release agent (decrement load)
   *
   * @param {string} agentId - Agent ID
   * @returns {void}
   */
  releaseAgent(agentId) {
    const currentLoad = this._getLoad(agentId);
    if (currentLoad > 0) {
      this.agentLoad.set(agentId, currentLoad - 1);
    }
  }

  /**
   * Get agent by ID
   *
   * @param {string} agentId - Agent ID
   * @returns {Object|undefined} Agent
   */
  getAgent(agentId) {
    return this.agents.get(agentId);
  }

  /**
   * Get all agents
   *
   * @returns {Object[]} Agents
   */
  getAgents() {
    return Array.from(this.agents.values());
  }

  /**
   * Get agents by role
   *
   * @param {string} role - Agent role
   * @returns {Object[]} Agents
   */
  getAgentsByRole(role) {
    return Array.from(this.agents.values())
      .filter(agent => agent.role === role);
  }

  /**
   * Get agents by capability
   *
   * @param {string} capability - Capability
   * @returns {Object[]} Agents
   */
  getAgentsByCapability(capability) {
    return Array.from(this.agents.values())
      .filter(agent =>
        agent.capabilities.includes(capability) ||
        agent.capabilities.includes('*')
      );
  }

  /**
   * Get router statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    const agents = Array.from(this.agents.values());
    const loads = Array.from(this.agentLoad.values());

    return {
      ...this.stats,
      totalAgents: this.agents.size,
      totalLoad: loads.reduce((sum, load) => sum + load, 0),
      averageLoad: agents.length > 0
        ? (loads.reduce((sum, load) => sum + load, 0) / agents.length).toFixed(2)
        : 0,
      routingSuccessRate: this.stats.totalRoutings > 0
        ? (((this.stats.totalRoutings - this.stats.routingFailures) / this.stats.totalRoutings) * 100).toFixed(2) + '%'
        : 'N/A'
    };
  }

  /**
   * Get matching agents
   *
   * @param {string[]} requiredCapabilities - Required capabilities
   * @param {string} [preferredRole] - Preferred role
   * @returns {Object[]} Matching agents
   * @private
   */
  _getMatchingAgents(requiredCapabilities, preferredRole) {
    let agents = Array.from(this.agents.values());

    // Filter by role if specified
    if (preferredRole) {
      const roleAgents = agents.filter(a => a.role === preferredRole);
      if (roleAgents.length > 0) {
        agents = roleAgents;
      }
    }

    // Filter by capabilities
    if (requiredCapabilities.length > 0) {
      agents = agents.filter(agent =>
        this._hasCapabilities(agent, requiredCapabilities)
      );
    }

    // Filter by availability (not at max concurrent)
    agents = agents.filter(agent => {
      const currentLoad = this._getLoad(agent.agentId);
      return currentLoad < agent.maxConcurrent;
    });

    return agents;
  }

  /**
   * Check if agent has required capabilities
   *
   * @param {Object} agent - Agent
   * @param {string[]} required - Required capabilities
   * @returns {boolean} Has capabilities
   * @private
   */
  _hasCapabilities(agent, required) {
    if (agent.capabilities.includes('*')) {
      return true;
    }

    return required.every(cap =>
      agent.capabilities.includes(cap)
    );
  }

  /**
   * Apply routing strategy
   *
   * @param {Object[]} agents - Candidate agents
   * @param {string} strategy - Strategy
   * @returns {Object[]} Ordered agents
   * @private
   */
  _applyStrategy(agents, strategy) {
    switch (strategy) {
      case RoutingStrategy.LOAD_BALANCED:
        return agents.sort((a, b) =>
          this._getLoad(a.agentId) - this._getLoad(b.agentId)
        );

      case RoutingStrategy.PRIORITY:
        return agents.sort((a, b) => b.priority - a.priority);

      case RoutingStrategy.ROUND_ROBIN:
        this.roundRobinCounter = (this.roundRobinCounter + 1) % agents.length;
        return [
          ...agents.slice(this.roundRobinCounter),
          ...agents.slice(0, this.roundRobinCounter)
        ];

      case RoutingStrategy.ROLE_BASED:
      case RoutingStrategy.CAPABILITY_MATCH:
      default:
        return agents;
    }
  }

  /**
   * Get current load for agent
   *
   * @param {string} agentId - Agent ID
   * @returns {number} Current load
   * @private
   */
  _getLoad(agentId) {
    return this.agentLoad.get(agentId) || 0;
  }
}

/**
 * Create an agent router
 *
 * @param {Object} [options] - Router options
 * @returns {AgentRouter}
 */
export function createAgentRouter(options = {}) {
  return new AgentRouter(options);
}
