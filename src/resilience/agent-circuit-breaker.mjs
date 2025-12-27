/**
 * @fileoverview Per-Agent Circuit Breaker - Fault isolation for KGC-SWARM agents
 *
 * **Purpose**: Isolate agent failures to prevent cascading system failures
 * - Per-agent circuit breakers with independent state
 * - Configurable failure thresholds per agent type
 * - Automatic recovery detection via half-open state
 * - OTEL observability for all state transitions
 *
 * **Pattern**: Reuses proven circuit-breaker.mjs pattern
 *
 * @module resilience/agent-circuit-breaker
 */

import { CircuitBreaker, CircuitBreakerRegistry } from '../knowledge-engine/utils/circuit-breaker.mjs';
import { z } from 'zod';

/**
 * Agent-specific circuit breaker configuration schema
 */
export const AgentCircuitConfigSchema = z.object({
  agentId: z.string(),
  agentType: z.string(),
  failureThreshold: z.number().min(1).default(3),
  resetTimeout: z.number().min(1000).default(30000),
  halfOpenMaxCalls: z.number().min(1).default(2),
  successThreshold: z.number().min(1).default(2),
  enableFallback: z.boolean().default(true),
  fallbackStrategy: z.enum(['skip', 'cache', 'degraded', 'simple']).default('skip'),
});

/**
 * Agent Circuit Breaker Registry - Manages circuit breakers for all agents
 *
 * @example
 * const registry = new AgentCircuitBreakerRegistry();
 * const breaker = registry.getOrCreateForAgent('agent-001', 'coder');
 * await breaker.execute(async () => agent.processTask(task));
 */
export class AgentCircuitBreakerRegistry extends CircuitBreakerRegistry {
  /**
   * Create a new agent circuit breaker registry
   * @param {Object} [config] - Default configuration for all agents
   */
  constructor(config = {}) {
    super();
    this.defaultConfig = config;
    this.agentConfigs = new Map();
    this.agentMetrics = new Map();
  }

  /**
   * Get or create circuit breaker for a specific agent
   * @param {string} agentId - Agent identifier
   * @param {string} agentType - Agent type (e.g., 'coder', 'tester', 'reviewer')
   * @param {Object} [customConfig] - Custom configuration for this agent
   * @returns {CircuitBreaker} Circuit breaker instance
   */
  getOrCreateForAgent(agentId, agentType, customConfig = {}) {
    const breakerName = `agent:${agentId}`;

    if (!this.breakers.has(breakerName)) {
      const config = {
        ...this.defaultConfig,
        ...this._getAgentTypeDefaults(agentType),
        ...customConfig,
        name: breakerName,
        onStateChange: (event) => this._handleStateChange(agentId, agentType, event),
      };

      this.agentConfigs.set(agentId, { agentId, agentType, ...config });
      this.agentMetrics.set(agentId, {
        totalExecutions: 0,
        failures: 0,
        circuitTrips: 0,
        lastFailure: null,
        lastSuccess: null,
      });

      this.breakers.set(breakerName, new CircuitBreaker(config));
    }

    return this.breakers.get(breakerName);
  }

  /**
   * Get agent-type-specific defaults
   * @param {string} agentType - Agent type
   * @returns {Object} Default configuration
   * @private
   */
  _getAgentTypeDefaults(agentType) {
    const defaults = {
      coder: { failureThreshold: 3, resetTimeout: 30000 },
      tester: { failureThreshold: 2, resetTimeout: 20000 },
      reviewer: { failureThreshold: 4, resetTimeout: 40000 },
      planner: { failureThreshold: 5, resetTimeout: 60000 },
      researcher: { failureThreshold: 3, resetTimeout: 45000 },
      // Hyper-advanced agents - higher tolerance
      'production-validator': { failureThreshold: 5, resetTimeout: 60000 },
      'code-analyzer': { failureThreshold: 4, resetTimeout: 50000 },
      'system-architect': { failureThreshold: 5, resetTimeout: 60000 },
      'performance-benchmarker': { failureThreshold: 3, resetTimeout: 45000 },
      'backend-dev': { failureThreshold: 3, resetTimeout: 30000 },
      'task-orchestrator': { failureThreshold: 5, resetTimeout: 60000 },
    };

    return defaults[agentType] || { failureThreshold: 3, resetTimeout: 30000 };
  }

  /**
   * Handle circuit state changes
   * @param {string} agentId - Agent ID
   * @param {string} agentType - Agent type
   * @param {Object} event - State change event
   * @private
   */
  _handleStateChange(agentId, agentType, event) {
    const metrics = this.agentMetrics.get(agentId);
    if (!metrics) return;

    if (event.to === 'open') {
      metrics.circuitTrips++;
      metrics.lastFailure = Date.now();
      console.warn(`[Resilience] Circuit OPEN for agent ${agentId} (${agentType})`);
    } else if (event.to === 'closed') {
      console.log(`[Resilience] Circuit CLOSED for agent ${agentId} (${agentType}) - Recovered`);
    }
  }

  /**
   * Execute agent operation with circuit breaker protection
   * @param {string} agentId - Agent ID
   * @param {string} agentType - Agent type
   * @param {Function} operation - Async operation to execute
   * @param {Object} [options] - Execution options
   * @returns {Promise<any>} Operation result or fallback
   */
  async executeWithProtection(agentId, agentType, operation, options = {}) {
    const breaker = this.getOrCreateForAgent(agentId, agentType);
    const metrics = this.agentMetrics.get(agentId);

    metrics.totalExecutions++;

    try {
      const result = await breaker.execute(operation, { agentId, agentType });
      metrics.lastSuccess = Date.now();
      return result;
    } catch (error) {
      metrics.failures++;
      metrics.lastFailure = Date.now();

      // Check if circuit is open
      if (error.name === 'CircuitOpenError') {
        return this._handleCircuitOpen(agentId, agentType, options);
      }

      throw error;
    }
  }

  /**
   * Handle circuit open scenario
   * @param {string} agentId - Agent ID
   * @param {string} agentType - Agent type
   * @param {Object} options - Execution options
   * @returns {any} Fallback result
   * @private
   */
  _handleCircuitOpen(agentId, agentType, options) {
    const config = this.agentConfigs.get(agentId);

    if (!config?.enableFallback) {
      throw new Error(`Agent ${agentId} circuit is open and no fallback enabled`);
    }

    console.warn(
      `[Resilience] Using fallback strategy '${config.fallbackStrategy}' for agent ${agentId}`
    );

    // Return fallback based on strategy
    switch (config.fallbackStrategy) {
      case 'skip':
        return { skipped: true, reason: 'circuit-open', agentId };
      case 'cache':
        return options.cachedResult || { skipped: true, reason: 'no-cache', agentId };
      case 'degraded':
        return { degraded: true, partial: true, agentId };
      case 'simple':
        return { simple: true, quality: 'reduced', agentId };
      default:
        return { skipped: true, reason: 'unknown-strategy', agentId };
    }
  }

  /**
   * Get health summary for all agents
   * @returns {Object} Health summary with per-agent status
   */
  getAgentHealthSummary() {
    const summary = {
      totalAgents: this.agentMetrics.size,
      healthy: 0,
      degraded: 0,
      failed: 0,
      agents: {},
    };

    for (const [agentId, metrics] of this.agentMetrics) {
      const config = this.agentConfigs.get(agentId);
      const breaker = this.breakers.get(`agent:${agentId}`);

      const status = breaker?.getStatus();
      const isHealthy = breaker?.isHealthy();

      summary.agents[agentId] = {
        agentType: config?.agentType,
        state: status?.state,
        healthy: isHealthy,
        metrics: {
          totalExecutions: metrics.totalExecutions,
          failures: metrics.failures,
          circuitTrips: metrics.circuitTrips,
          failureRate:
            metrics.totalExecutions > 0
              ? ((metrics.failures / metrics.totalExecutions) * 100).toFixed(2) + '%'
              : '0%',
        },
        lastFailure: metrics.lastFailure,
        lastSuccess: metrics.lastSuccess,
      };

      if (isHealthy) summary.healthy++;
      else if (status?.state === 'half_open') summary.degraded++;
      else summary.failed++;
    }

    return summary;
  }

  /**
   * Reset all circuit breakers for recovery
   */
  resetAllAgents() {
    this.resetAll();
    for (const metrics of this.agentMetrics.values()) {
      metrics.circuitTrips = 0;
    }
    console.log('[Resilience] All agent circuits reset');
  }

  /**
   * Get metrics for a specific agent
   * @param {string} agentId - Agent ID
   * @returns {Object|null} Agent metrics or null
   */
  getAgentMetrics(agentId) {
    const metrics = this.agentMetrics.get(agentId);
    const config = this.agentConfigs.get(agentId);
    const breaker = this.breakers.get(`agent:${agentId}`);

    if (!metrics || !config || !breaker) return null;

    return {
      agentId,
      agentType: config.agentType,
      state: breaker.getStatus().state,
      metrics: {
        ...metrics,
        failureRate:
          metrics.totalExecutions > 0
            ? ((metrics.failures / metrics.totalExecutions) * 100).toFixed(2) + '%'
            : '0%',
      },
      config: {
        failureThreshold: config.failureThreshold,
        resetTimeout: config.resetTimeout,
        fallbackStrategy: config.fallbackStrategy,
      },
    };
  }
}

/**
 * Default global agent circuit breaker registry
 */
export const defaultAgentRegistry = new AgentCircuitBreakerRegistry();

/**
 * Create agent circuit breaker with default configuration
 * @param {string} agentId - Agent ID
 * @param {string} agentType - Agent type
 * @param {Object} [config] - Custom configuration
 * @returns {CircuitBreaker} Circuit breaker instance
 */
export function createAgentCircuitBreaker(agentId, agentType, config = {}) {
  return defaultAgentRegistry.getOrCreateForAgent(agentId, agentType, config);
}

/**
 * Execute agent operation with circuit breaker protection
 * @param {string} agentId - Agent ID
 * @param {string} agentType - Agent type
 * @param {Function} operation - Async operation
 * @param {Object} [options] - Execution options
 * @returns {Promise<any>} Operation result
 */
export async function executeWithCircuitBreaker(agentId, agentType, operation, options = {}) {
  return defaultAgentRegistry.executeWithProtection(agentId, agentType, operation, options);
}
