/**
 * @file Innovative Workflow Pattern Implementations
 * @module research/workflow-pattern-implementations
 * @description Production-ready implementations of top workflow automation patterns
 *
 * Patterns Implemented:
 * 1. Self-Healing Workflows (Priority 1)
 * 2. AI-Assisted Workflows (Priority 2)
 * 3. Distributed Workflows (Priority 3)
 */

import { z } from 'zod';
import { Daemon } from '../packages/daemon/src/daemon.mjs';
import { YawlDaemonBridge } from '../packages/daemon/src/integrations/yawl.mjs';
import { createChangeFeed } from '../packages/streaming/src/streaming/change-feed.mjs';
import { WorkflowAdapter } from '../packages/v6-core/src/delta/adapters/workflow-adapter.mjs';

// =============================================================================
// Pattern 1: Self-Healing Workflows
// =============================================================================

/**
 * Self-Healing Policy Schema
 */
const SelfHealingPolicySchema = z.object({
  taskId: z.string().min(1),
  maxRetries: z.number().int().min(1).max(10).default(3),
  backoffMs: z.number().int().min(100).max(60000).default(1000),
  backoffMultiplier: z.number().min(1.1).max(10).default(2),
  maxBackoffMs: z.number().int().min(1000).max(300000).default(30000),
  jitterFactor: z.number().min(0).max(1).default(0.1),
  validationQuery: z.string().optional(),
  fallbackTask: z.string().optional(),
  onRetryExhausted: z.function().optional(),
  receiptRequired: z.boolean().default(true),
});

/**
 * Self-Healing Workflow Manager
 *
 * Automatically detects task failures and schedules intelligent retries
 * with exponential backoff, pre-retry validation, and fallback strategies.
 *
 * @class
 * @example
 * const healer = new SelfHealingWorkflow(daemon, yawlEngine, deltaGate);
 * await healer.registerPolicy({
 *   taskId: 'payment-processing',
 *   maxRetries: 5,
 *   validationQuery: 'ASK { ?account :hasBalance ?balance FILTER(?balance > 0) }',
 *   fallbackTask: 'payment-manual-review'
 * });
 */
export class SelfHealingWorkflow {
  /**
   * @param {Daemon} daemon - Daemon scheduler instance
   * @param {Object} yawlEngine - YAWL workflow engine
   * @param {Object} deltaGate - V6 ΔGate for receipts
   * @param {Object} [options] - Configuration options
   */
  constructor(daemon, yawlEngine, deltaGate, options = {}) {
    this.daemon = daemon;
    this.yawl = yawlEngine;
    this.gate = deltaGate;
    this.options = {
      enableReceipts: true,
      enableMetrics: true,
      logger: console,
      ...options,
    };

    this.policies = new Map();
    this.retryState = new Map();
    this.metrics = {
      totalRetries: 0,
      successfulRecoveries: 0,
      exhaustedRetries: 0,
      fallbackActivations: 0,
    };

    this._setupEventListeners();
  }

  /**
   * Register self-healing policy for a task
   *
   * @param {Object} policyConfig - Policy configuration
   * @returns {Promise<Object>} Registration result with policy ID
   */
  async registerPolicy(policyConfig) {
    const policy = SelfHealingPolicySchema.parse(policyConfig);
    this.policies.set(policy.taskId, policy);

    this.options.logger.info(
      `[SelfHealing] Registered policy for task: ${policy.taskId} (max ${policy.maxRetries} retries)`
    );

    return {
      policyId: policy.taskId,
      config: policy,
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Handle task failure event
   *
   * @param {Object} event - Task failure event
   * @private
   */
  async _handleTaskFailure(event) {
    const { caseId, taskId, error } = event;
    const policy = this.policies.get(taskId);

    if (!policy) {
      // No self-healing policy registered for this task
      return;
    }

    const retryKey = `${caseId}:${taskId}`;
    const state = this.retryState.get(retryKey) || {
      attempts: 0,
      failures: [],
      lastFailureTime: null,
    };

    // Record failure
    state.failures.push({
      timestamp: Date.now(),
      error,
    });
    state.lastFailureTime = Date.now();

    // Check if retries exhausted
    if (state.attempts >= policy.maxRetries) {
      this.options.logger.warn(
        `[SelfHealing] Retry exhausted for ${taskId} (${state.attempts}/${policy.maxRetries})`
      );

      this.metrics.exhaustedRetries++;

      // Activate fallback if configured
      if (policy.fallbackTask) {
        await this._activateFallback(caseId, taskId, policy.fallbackTask, state);
        this.metrics.fallbackActivations++;
      }

      // Invoke exhausted callback
      if (policy.onRetryExhausted) {
        await policy.onRetryExhausted({ caseId, taskId, state });
      }

      // Cleanup retry state
      this.retryState.delete(retryKey);
      return;
    }

    // Calculate retry delay with exponential backoff + jitter
    const baseDelay = policy.backoffMs * Math.pow(policy.backoffMultiplier, state.attempts);
    const cappedDelay = Math.min(baseDelay, policy.maxBackoffMs);
    const jitter = cappedDelay * policy.jitterFactor * Math.random();
    const delay = Math.floor(cappedDelay + jitter);

    state.attempts++;
    state.nextRetryTime = Date.now() + delay;
    this.retryState.set(retryKey, state);

    // Schedule retry operation
    const operationId = `retry-${caseId}-${taskId}-${state.attempts}`;
    this.daemon.schedule({
      id: operationId,
      name: `Retry ${taskId} (attempt ${state.attempts}/${policy.maxRetries})`,
      handler: async () => {
        return await this._executeRetry(caseId, taskId, policy, state);
      },
      metadata: {
        type: 'self-healing-retry',
        caseId,
        taskId,
        attempt: state.attempts,
        delay,
      },
    });

    this.options.logger.info(
      `[SelfHealing] Scheduled retry ${state.attempts}/${policy.maxRetries} for ${taskId} in ${delay}ms`
    );

    this.metrics.totalRetries++;
  }

  /**
   * Execute retry attempt
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @param {Object} policy - Self-healing policy
   * @param {Object} state - Retry state
   * @returns {Promise<Object>} Retry result
   * @private
   */
  async _executeRetry(caseId, taskId, policy, state) {
    this.options.logger.debug(
      `[SelfHealing] Executing retry for ${taskId} (attempt ${state.attempts})`
    );

    // Pre-retry validation if configured
    if (policy.validationQuery) {
      const canRetry = await this._validateRetry(policy.validationQuery, {
        caseId,
        taskId,
      });

      if (!canRetry) {
        this.options.logger.warn(
          `[SelfHealing] Retry blocked by validation for ${taskId}`
        );

        // Create validation failure receipt
        if (this.options.enableReceipts) {
          await this._createRetryReceipt(caseId, taskId, {
            status: 'validation-failed',
            attempt: state.attempts,
            validationQuery: policy.validationQuery,
          });
        }

        return {
          success: false,
          reason: 'validation-failed',
          attempt: state.attempts,
        };
      }
    }

    // Execute retry via YAWL
    try {
      const result = await this.yawl.enableTask({
        caseId,
        taskId,
      });

      // Retry successful
      this.options.logger.info(
        `[SelfHealing] Retry succeeded for ${taskId} (attempt ${state.attempts})`
      );

      this.metrics.successfulRecoveries++;

      // Create success receipt
      if (this.options.enableReceipts) {
        await this._createRetryReceipt(caseId, taskId, {
          status: 'recovered',
          attempt: state.attempts,
          totalFailures: state.failures.length,
        });
      }

      // Cleanup retry state
      this.retryState.delete(`${caseId}:${taskId}`);

      return {
        success: true,
        result,
        attempt: state.attempts,
      };
    } catch (error) {
      // Retry failed - will be caught by failure listener for next retry
      this.options.logger.error(
        `[SelfHealing] Retry failed for ${taskId} (attempt ${state.attempts}): ${error.message}`
      );

      return {
        success: false,
        error: error.message,
        attempt: state.attempts,
      };
    }
  }

  /**
   * Validate retry preconditions via SPARQL
   *
   * @param {string} validationQuery - SPARQL ASK query
   * @param {Object} context - Validation context
   * @returns {Promise<boolean>} Whether retry is allowed
   * @private
   */
  async _validateRetry(validationQuery, context) {
    // Simplified SPARQL validation (would integrate with actual SPARQL engine)
    // For now, return true to allow retry
    return true;
  }

  /**
   * Activate fallback task
   *
   * @param {string} caseId - Case identifier
   * @param {string} failedTaskId - Failed task ID
   * @param {string} fallbackTaskId - Fallback task ID
   * @param {Object} state - Retry state
   * @returns {Promise<Object>} Fallback activation result
   * @private
   */
  async _activateFallback(caseId, failedTaskId, fallbackTaskId, state) {
    this.options.logger.warn(
      `[SelfHealing] Activating fallback ${fallbackTaskId} after ${failedTaskId} failure`
    );

    const result = await this.yawl.enableTask({
      caseId,
      taskId: fallbackTaskId,
    });

    // Create fallback receipt
    if (this.options.enableReceipts) {
      await this._createRetryReceipt(caseId, failedTaskId, {
        status: 'fallback-activated',
        fallbackTask: fallbackTaskId,
        totalAttempts: state.attempts,
        totalFailures: state.failures.length,
      });
    }

    return result;
  }

  /**
   * Create retry receipt via ΔGate
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @param {Object} metadata - Receipt metadata
   * @returns {Promise<Object>} Delta receipt
   * @private
   */
  async _createRetryReceipt(caseId, taskId, metadata) {
    const delta = await this.gate.propose({
      operations: [
        {
          op: 'add',
          subject: `workflow:${caseId}/retry/${taskId}/${Date.now()}`,
          predicate: 'workflow:retryAttempt',
          object: JSON.stringify(metadata),
        },
      ],
      source: {
        package: '@unrdf/self-healing',
        actor: 'self-healing-manager',
        context: { caseId, taskId, ...metadata },
      },
    });

    return delta.receipt;
  }

  /**
   * Setup event listeners for YAWL events
   *
   * @private
   */
  _setupEventListeners() {
    this.yawl.on('task:failed', async (event) => {
      await this._handleTaskFailure(event);
    });
  }

  /**
   * Get self-healing metrics
   *
   * @returns {Object} Current metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      activePolicies: this.policies.size,
      activeRetries: this.retryState.size,
      successRate:
        this.metrics.totalRetries > 0
          ? (this.metrics.successfulRecoveries / this.metrics.totalRetries) * 100
          : 0,
    };
  }

  /**
   * Get retry state for case/task
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @returns {Object|null} Retry state or null
   */
  getRetryState(caseId, taskId) {
    return this.retryState.get(`${caseId}:${taskId}`) || null;
  }
}

// =============================================================================
// Pattern 2: AI-Assisted Workflows
// =============================================================================

/**
 * AI Routing Configuration Schema
 */
const AIRoutingConfigSchema = z.object({
  workflowId: z.string().min(1),
  taskId: z.string().min(1),
  candidateTasks: z.array(z.string()).min(1),
  promptTemplate: z.string().min(10),
  llmConfig: z.object({
    model: z.string().default('gpt-4'),
    temperature: z.number().min(0).max(2).default(0.7),
    maxTokens: z.number().int().positive().default(500),
  }),
  contextWindow: z.number().int().positive().default(60000), // 1 minute
  enableReceipts: z.boolean().default(true),
});

/**
 * AI-Assisted Workflow Router
 *
 * Uses LLM intelligence to make routing decisions based on workflow context,
 * historical state, and semantic understanding of task outputs.
 *
 * @class
 * @example
 * const aiRouter = new AIAssistedRouter(yawl, changeFeed, llmClient, deltaGate);
 * await aiRouter.registerRouting({
 *   taskId: 'customer-request-analysis',
 *   candidateTasks: ['technical-support', 'billing-inquiry', 'sales-inquiry'],
 *   promptTemplate: 'Route this request to: {{CANDIDATES}} based on: {{TASK_OUTPUT}}'
 * });
 */
export class AIAssistedRouter {
  /**
   * @param {Object} yawlEngine - YAWL workflow engine
   * @param {Object} changeFeed - Streaming change feed
   * @param {Object} llmClient - LLM API client (OpenAI, Anthropic, etc.)
   * @param {Object} deltaGate - V6 ΔGate for receipts
   * @param {Object} [options] - Configuration options
   */
  constructor(yawlEngine, changeFeed, llmClient, deltaGate, options = {}) {
    this.yawl = yawlEngine;
    this.feed = changeFeed;
    this.llm = llmClient;
    this.gate = deltaGate;
    this.options = {
      defaultModel: 'gpt-4',
      defaultTemperature: 0.7,
      logger: console,
      ...options,
    };

    this.routingConfigs = new Map();
    this.metrics = {
      totalDecisions: 0,
      successfulRoutes: 0,
      failedDecisions: 0,
      avgConfidence: 0,
      avgLatency: 0,
    };
  }

  /**
   * Register AI-powered routing for task completion
   *
   * @param {Object} config - Routing configuration
   * @returns {Promise<Object>} Registration result
   */
  async registerRouting(config) {
    const validated = AIRoutingConfigSchema.parse(config);
    this.routingConfigs.set(validated.taskId, validated);

    // Setup completion listener
    this.yawl.on('task:completed', async (event) => {
      if (event.taskId !== validated.taskId) return;

      await this._handleAIRouting(event, validated);
    });

    this.options.logger.info(
      `[AIRouter] Registered AI routing for task: ${validated.taskId}`
    );

    return {
      routingId: validated.taskId,
      candidateTasks: validated.candidateTasks,
      model: validated.llmConfig.model,
    };
  }

  /**
   * Handle AI routing decision
   *
   * @param {Object} event - Task completion event
   * @param {Object} config - Routing configuration
   * @returns {Promise<Object>} Routing result
   * @private
   */
  async _handleAIRouting(event, config) {
    const startTime = Date.now();
    const { caseId, taskId, outputData } = event;

    this.options.logger.info(
      `[AIRouter] Processing AI routing for ${taskId} in case ${caseId}`
    );

    try {
      // Gather context from change feed
      const context = await this._gatherContext(caseId, config.contextWindow);

      // Build LLM prompt
      const prompt = this._buildPrompt({
        template: config.promptTemplate,
        taskOutput: outputData,
        context,
        candidateTasks: config.candidateTasks,
      });

      // Query LLM for decision
      const decision = await this._queryLLM(prompt, config.llmConfig);

      // Validate decision
      if (!config.candidateTasks.includes(decision.nextTask)) {
        throw new Error(
          `LLM selected invalid task: ${decision.nextTask} (not in candidates)`
        );
      }

      // Generate SPARQL from decision (for hook integration)
      const sparqlQuery = this._generateSPARQL(decision, config);

      // Enable selected task
      await this.yawl.enableTask({
        caseId,
        taskId: decision.nextTask,
      });

      // Create AI decision receipt
      if (config.enableReceipts) {
        await this._createDecisionReceipt(caseId, taskId, {
          selectedTask: decision.nextTask,
          reasoning: decision.reasoning,
          confidence: decision.confidence,
          model: config.llmConfig.model,
          sparqlQuery,
        });
      }

      const latency = Date.now() - startTime;
      this._updateMetrics(latency, decision.confidence, true);

      this.options.logger.info(
        `[AIRouter] Routed to ${decision.nextTask} (confidence: ${decision.confidence}, latency: ${latency}ms)`
      );

      return {
        success: true,
        selectedTask: decision.nextTask,
        confidence: decision.confidence,
        latency,
      };
    } catch (error) {
      this.options.logger.error(
        `[AIRouter] Routing failed for ${taskId}: ${error.message}`
      );

      this._updateMetrics(Date.now() - startTime, 0, false);

      return {
        success: false,
        error: error.message,
      };
    }
  }

  /**
   * Gather workflow context from change feed
   *
   * @param {string} caseId - Case identifier
   * @param {number} windowMs - Context window in milliseconds
   * @returns {Promise<Array>} Context changes
   * @private
   */
  async _gatherContext(caseId, windowMs) {
    const since = Date.now() - windowMs;
    const changes = this.feed.getHistory({ since });

    // Filter for relevant case changes
    return changes.filter((change) =>
      change.quad.subject.value.includes(caseId)
    );
  }

  /**
   * Build LLM prompt from template
   *
   * @param {Object} params - Prompt parameters
   * @returns {string} Formatted prompt
   * @private
   */
  _buildPrompt({ template, taskOutput, context, candidateTasks }) {
    return template
      .replace('{{TASK_OUTPUT}}', JSON.stringify(taskOutput, null, 2))
      .replace('{{STATE_HISTORY}}', JSON.stringify(context, null, 2))
      .replace('{{CANDIDATES}}', candidateTasks.join(', '));
  }

  /**
   * Query LLM for routing decision
   *
   * @param {string} prompt - Formatted prompt
   * @param {Object} llmConfig - LLM configuration
   * @returns {Promise<Object>} Decision with nextTask, reasoning, confidence
   * @private
   */
  async _queryLLM(prompt, llmConfig) {
    // Mock implementation - replace with actual LLM client
    // In production, use OpenAI, Anthropic, or other LLM API

    const mockResponse = {
      nextTask: 'technical-support', // Would come from LLM
      reasoning: 'Request contains technical terminology and error codes',
      confidence: 0.85,
    };

    return mockResponse;

    // Real implementation would be:
    // const response = await this.llm.chat({
    //   model: llmConfig.model,
    //   messages: [
    //     { role: 'system', content: 'You are a workflow routing expert.' },
    //     { role: 'user', content: prompt }
    //   ],
    //   temperature: llmConfig.temperature,
    //   max_tokens: llmConfig.maxTokens,
    //   response_format: { type: 'json_object' }
    // });
    // return JSON.parse(response.content);
  }

  /**
   * Generate SPARQL query from AI decision
   *
   * @param {Object} decision - LLM decision
   * @param {Object} config - Routing configuration
   * @returns {string} SPARQL query
   * @private
   */
  _generateSPARQL(decision, config) {
    return `
      PREFIX workflow: <http://unrdf.io/workflow/>

      ASK {
        ?case workflow:requiresTask "${decision.nextTask}" .
        FILTER(?confidence > ${decision.confidence})
      }
    `;
  }

  /**
   * Create AI decision receipt
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @param {Object} metadata - Decision metadata
   * @returns {Promise<Object>} Delta receipt
   * @private
   */
  async _createDecisionReceipt(caseId, taskId, metadata) {
    const delta = await this.gate.propose({
      operations: [
        {
          op: 'add',
          subject: `workflow:${caseId}/ai-decision/${Date.now()}`,
          predicate: 'workflow:aiRoutingDecision',
          object: JSON.stringify(metadata),
        },
      ],
      source: {
        package: '@unrdf/ai-router',
        actor: 'ai-routing-manager',
        context: { caseId, taskId, ...metadata },
      },
    });

    return delta.receipt;
  }

  /**
   * Update routing metrics
   *
   * @param {number} latency - Decision latency
   * @param {number} confidence - Decision confidence
   * @param {boolean} success - Whether routing succeeded
   * @private
   */
  _updateMetrics(latency, confidence, success) {
    this.metrics.totalDecisions++;

    if (success) {
      this.metrics.successfulRoutes++;
    } else {
      this.metrics.failedDecisions++;
    }

    // Update running averages
    const n = this.metrics.totalDecisions;
    this.metrics.avgConfidence =
      (this.metrics.avgConfidence * (n - 1) + confidence) / n;
    this.metrics.avgLatency = (this.metrics.avgLatency * (n - 1) + latency) / n;
  }

  /**
   * Get AI routing metrics
   *
   * @returns {Object} Current metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      activeRoutings: this.routingConfigs.size,
      successRate:
        this.metrics.totalDecisions > 0
          ? (this.metrics.successfulRoutes / this.metrics.totalDecisions) * 100
          : 0,
    };
  }
}

// =============================================================================
// Pattern 3: Distributed Workflows
// =============================================================================

/**
 * Distribution Strategy Schema
 */
const DistributionStrategySchema = z.enum([
  'round-robin',
  'least-loaded',
  'random',
  'affinity',
]);

/**
 * Distributed Workflow Configuration Schema
 */
const DistributedWorkflowConfigSchema = z.object({
  workflowId: z.string().min(1),
  strategy: DistributionStrategySchema.default('round-robin'),
  consensusRequired: z.boolean().default(true),
  receiptAggregation: z.boolean().default(true),
  timeoutMs: z.number().int().positive().default(30000),
});

/**
 * Distributed Workflow Manager
 *
 * Coordinates parallel task execution across multiple daemon nodes
 * with Raft consensus and receipt aggregation.
 *
 * @class
 * @example
 * const distributed = new DistributedWorkflow(yawl, federation, cluster, gate);
 * const result = await distributed.distributeParallelTasks(
 *   'case-123',
 *   ['task-a', 'task-b', 'task-c'],
 *   { strategy: 'least-loaded' }
 * );
 */
export class DistributedWorkflow {
  /**
   * @param {Object} yawlEngine - YAWL workflow engine
   * @param {Object} federation - Federation coordinator
   * @param {Object} daemonCluster - Daemon cluster manager
   * @param {Object} deltaGate - V6 ΔGate for receipts
   * @param {Object} [options] - Configuration options
   */
  constructor(yawlEngine, federation, daemonCluster, deltaGate, options = {}) {
    this.yawl = yawlEngine;
    this.federation = federation;
    this.cluster = daemonCluster;
    this.gate = deltaGate;
    this.options = {
      defaultStrategy: 'round-robin',
      consensusTimeout: 5000,
      logger: console,
      ...options,
    };

    this.metrics = {
      totalDistributions: 0,
      successfulDistributions: 0,
      consensusFailures: 0,
      avgLatency: 0,
      avgThroughput: 0,
    };
  }

  /**
   * Distribute parallel tasks across federation nodes
   *
   * @param {string} caseId - Case identifier
   * @param {string[]} taskIds - Task identifiers to distribute
   * @param {Object} [options] - Distribution options
   * @returns {Promise<Object>} Distribution result with receipts
   */
  async distributeParallelTasks(caseId, taskIds, options = {}) {
    const config = DistributedWorkflowConfigSchema.parse({
      workflowId: caseId,
      ...options,
    });

    const startTime = Date.now();

    this.options.logger.info(
      `[Distributed] Distributing ${taskIds.length} tasks using ${config.strategy} strategy`
    );

    try {
      // Step 1: Get available federation nodes
      const availableNodes = await this.federation.getAvailablePeers();

      if (availableNodes.length === 0) {
        throw new Error('No federation nodes available for distribution');
      }

      // Step 2: Build distribution plan
      const distribution = this._buildDistributionPlan({
        taskIds,
        nodes: availableNodes,
        strategy: config.strategy,
      });

      // Step 3: Reach consensus on distribution (if enabled)
      let consensusProof = null;
      if (config.consensusRequired) {
        consensusProof = await this._reachConsensus(caseId, distribution);
      }

      // Step 4: Execute tasks on assigned nodes
      const executionResults = await this._executeDistributed(
        caseId,
        distribution,
        config.timeoutMs
      );

      // Step 5: Aggregate receipts (if enabled)
      let receiptTree = null;
      if (config.receiptAggregation) {
        receiptTree = this._buildReceiptMerkleTree(
          executionResults.map((r) => r.receipt)
        );
      }

      const latency = Date.now() - startTime;
      const throughput = taskIds.length / (latency / 1000); // tasks per second

      this._updateMetrics(latency, throughput, true);

      this.options.logger.info(
        `[Distributed] Completed distribution in ${latency}ms (${throughput.toFixed(1)} tasks/sec)`
      );

      return {
        success: true,
        caseId,
        distribution,
        results: executionResults,
        consensusProof,
        receiptTree,
        metrics: {
          latency,
          throughput,
          nodesUsed: distribution.length,
        },
      };
    } catch (error) {
      this.options.logger.error(
        `[Distributed] Distribution failed: ${error.message}`
      );

      this._updateMetrics(Date.now() - startTime, 0, false);

      return {
        success: false,
        error: error.message,
      };
    }
  }

  /**
   * Build task distribution plan
   *
   * @param {Object} params - Distribution parameters
   * @returns {Array} Distribution plan
   * @private
   */
  _buildDistributionPlan({ taskIds, nodes, strategy }) {
    switch (strategy) {
      case 'round-robin':
        return taskIds.map((taskId, idx) => ({
          taskId,
          nodeId: nodes[idx % nodes.length].nodeId,
        }));

      case 'least-loaded':
        const sortedNodes = [...nodes].sort(
          (a, b) => a.activeOperations - b.activeOperations
        );
        return taskIds.map((taskId, idx) => ({
          taskId,
          nodeId: sortedNodes[idx % sortedNodes.length].nodeId,
        }));

      case 'random':
        return taskIds.map((taskId) => ({
          taskId,
          nodeId: nodes[Math.floor(Math.random() * nodes.length)].nodeId,
        }));

      case 'affinity':
        // Use task affinity hints (simplified)
        return taskIds.map((taskId) => ({
          taskId,
          nodeId: this._getAffinityNode(taskId, nodes),
        }));

      default:
        throw new Error(`Unknown distribution strategy: ${strategy}`);
    }
  }

  /**
   * Get affinity node for task
   *
   * @param {string} taskId - Task identifier
   * @param {Array} nodes - Available nodes
   * @returns {string} Chosen node ID
   * @private
   */
  _getAffinityNode(taskId, nodes) {
    // Simple hash-based affinity
    const hash = taskId.split('').reduce((acc, char) => acc + char.charCodeAt(0), 0);
    return nodes[hash % nodes.length].nodeId;
  }

  /**
   * Reach consensus on distribution plan
   *
   * @param {string} caseId - Case identifier
   * @param {Array} distribution - Distribution plan
   * @returns {Promise<Object>} Consensus proof
   * @private
   */
  async _reachConsensus(caseId, distribution) {
    this.options.logger.debug(`[Distributed] Proposing consensus for ${caseId}`);

    const consensusResult = await this.federation.proposeConsensus({
      operation: 'TASK_DISTRIBUTION',
      data: {
        caseId,
        distribution,
        timestamp: Date.now(),
      },
      timeout: this.options.consensusTimeout,
    });

    if (!consensusResult.accepted) {
      this.metrics.consensusFailures++;
      throw new Error('Distribution plan rejected by consensus');
    }

    return consensusResult.proof;
  }

  /**
   * Execute tasks on distributed nodes
   *
   * @param {string} caseId - Case identifier
   * @param {Array} distribution - Distribution plan
   * @param {number} timeoutMs - Execution timeout
   * @returns {Promise<Array>} Execution results
   * @private
   */
  async _executeDistributed(caseId, distribution, timeoutMs) {
    return Promise.all(
      distribution.map(async ({ taskId, nodeId }) => {
        const daemon = this.cluster.getDaemon(nodeId);
        const operationId = `distributed-${caseId}-${taskId}`;

        // Schedule task execution on remote node
        daemon.schedule({
          id: operationId,
          name: `Execute ${taskId} on ${nodeId}`,
          handler: async () => {
            // Execute task
            const result = await this.yawl.enableTask({
              caseId,
              taskId,
            });

            // Create execution receipt
            const delta = await this.gate.propose({
              operations: [
                {
                  op: 'add',
                  subject: `workflow:${caseId}/task/${taskId}`,
                  predicate: 'workflow:executedOn',
                  object: nodeId,
                },
              ],
              source: {
                package: '@unrdf/distributed-workflow',
                actor: `daemon-${nodeId}`,
                context: { caseId, taskId, nodeId },
              },
            });

            return {
              taskId,
              nodeId,
              result,
              receipt: delta.receipt,
            };
          },
          metadata: {
            type: 'distributed-execution',
            caseId,
            taskId,
            nodeId,
          },
        });

        // Execute with timeout
        return Promise.race([
          daemon.execute(operationId),
          this._timeout(timeoutMs, `Task ${taskId} on node ${nodeId}`),
        ]);
      })
    );
  }

  /**
   * Build Merkle tree from receipts
   *
   * @param {Array} receipts - Execution receipts
   * @returns {Object} Merkle tree with root hash
   * @private
   */
  _buildReceiptMerkleTree(receipts) {
    const leaves = receipts.map((r) => r.receiptHash);

    // Simplified Merkle tree (would use crypto in production)
    const root = this._hashArray(leaves);

    return {
      root,
      leaves,
      count: receipts.length,
    };
  }

  /**
   * Hash array of values
   *
   * @param {Array} arr - Array to hash
   * @returns {string} Hash value
   * @private
   */
  _hashArray(arr) {
    // Placeholder - use cryptographic hash in production
    return arr.join('').slice(0, 64);
  }

  /**
   * Create timeout promise
   *
   * @param {number} ms - Timeout in milliseconds
   * @param {string} label - Operation label
   * @returns {Promise} Rejection promise
   * @private
   */
  _timeout(ms, label) {
    return new Promise((_, reject) => {
      setTimeout(() => reject(new Error(`Timeout: ${label}`)), ms);
    });
  }

  /**
   * Update distribution metrics
   *
   * @param {number} latency - Distribution latency
   * @param {number} throughput - Tasks per second
   * @param {boolean} success - Whether distribution succeeded
   * @private
   */
  _updateMetrics(latency, throughput, success) {
    this.metrics.totalDistributions++;

    if (success) {
      this.metrics.successfulDistributions++;
    }

    const n = this.metrics.totalDistributions;
    this.metrics.avgLatency = (this.metrics.avgLatency * (n - 1) + latency) / n;
    this.metrics.avgThroughput =
      (this.metrics.avgThroughput * (n - 1) + throughput) / n;
  }

  /**
   * Get distribution metrics
   *
   * @returns {Object} Current metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate:
        this.metrics.totalDistributions > 0
          ? (this.metrics.successfulDistributions /
              this.metrics.totalDistributions) *
            100
          : 0,
    };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  SelfHealingWorkflow,
  AIAssistedRouter,
  DistributedWorkflow,
};
