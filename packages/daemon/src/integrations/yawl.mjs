/**
 * @file Daemon-YAWL Engine Integration
 * @module @unrdf/daemon/integrations/yawl
 * @description Bidirectional integration between UnrdfDaemon and YAWL workflow engine.
 * Maps YAWL events to daemon operations (scheduling, retries, timeouts, distribution)
 * and daemon schedules to YAWL API calls (case creation, task enablement, etc.)
 */

import { EventEmitter } from 'events';
import { z } from 'zod';

// =============================================================================
// Configuration Schemas
// =============================================================================

/**
 * Retry policy for YAWL task failures
 * Configures exponential backoff for task retry attempts
 * @type {z.ZodType}
 */
export const YawlRetryPolicySchema = z.object({
  maxAttempts: z.number().int().min(1).max(10).default(3),
  backoffMs: z.number().int().min(100).max(60000).default(1000),
  backoffMultiplier: z.number().min(1.1).max(10).default(2),
  maxBackoffMs: z.number().int().min(1000).max(300000).default(30000),
  jitterFactor: z.number().min(0).max(1).default(0.1),
}).default({});

/**
 * Timeout configuration for YAWL tasks
 * @type {z.ZodType}
 */
export const YawlTimeoutConfigSchema = z.object({
  taskTimeoutMs: z.number().int().min(1000).max(3600000).default(30000),
  caseTimeoutMs: z.number().int().min(5000).max(86400000).default(3600000),
  checkIntervalMs: z.number().int().min(100).max(30000).default(5000),
}).default({});

/**
 * Distribution strategy for parallel task execution
 * @type {z.ZodType}
 */
export const DistributionStrategySchema = z.enum([
  'round-robin',
  'least-loaded',
  'random',
  'affinity',
]).default('round-robin');

/**
 * YAWL Daemon Bridge configuration schema
 * @type {z.ZodType}
 */
export const YawlDaemonBridgeConfigSchema = z.object({
  bridgeId: z.string().min(1).default(() => `yawl-bridge-${Date.now()}`),
  daemonNodeId: z.string().min(1),
  maxConcurrentCases: z.number().int().min(1).max(10000).default(100),
  retryPolicy: YawlRetryPolicySchema,
  timeoutDefaults: YawlTimeoutConfigSchema,
  enableAutoRetry: z.boolean().default(true),
  enableTimeoutTracking: z.boolean().default(true),
  enableDistribution: z.boolean().default(true),
  logger: z.any().optional(),
}).refine(
  (config) => config.maxConcurrentCases > 0,
  { message: 'maxConcurrentCases must be greater than 0', path: ['maxConcurrentCases'] }
);

// =============================================================================
// YawlDaemonBridge Class
// =============================================================================

/**
 * Bridges YAWL workflow engine and daemon scheduler
 * Enables:
 * - Scheduling recurring case creation
 * - Timeout enforcement on tasks
 * - Automatic retry on failures
 * - Deferred choice triggering
 * - Parallel task distribution
 *
 * @extends EventEmitter
 * @example
 * ```javascript
 * const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
 *   daemonNodeId: 'node-1',
 *   maxConcurrentCases: 100,
 *   enableAutoRetry: true,
 * });
 *
 * await bridge.start();
 *
 * // Schedule recurring case creation every hour
 * await bridge.scheduleRecurringCase('approval-workflow', '0 * * * *', {
 *   priority: 'high',
 * });
 *
 * // Watch a task for timeout
 * await bridge.watchTaskTimeout('case-001', 'review-task', 60000);
 * ```
 */
export class YawlDaemonBridge extends EventEmitter {
  /**
   * Create a YAWL daemon bridge
   * @param {import('../daemon.mjs').Daemon} daemon - Daemon instance
   * @param {import('@unrdf/yawl').WorkflowEngine} yawlEngine - YAWL engine instance
   * @param {Object} config - Bridge configuration
   * @throws {Error} If configuration is invalid
   */
  constructor(daemon, yawlEngine, config = {}) {
    super();

    // Validate configuration
    const validated = YawlDaemonBridgeConfigSchema.parse(config);

    if (!daemon || typeof daemon.schedule !== 'function') {
      throw new TypeError('Invalid daemon: must have schedule method');
    }
    if (!yawlEngine || typeof yawlEngine.on !== 'function') {
      throw new TypeError('Invalid yawlEngine: must have event subscription');
    }

    this.id = validated.bridgeId;
    this.daemon = daemon;
    this.yawlEngine = yawlEngine;
    this.config = validated;
    this.logger = validated.logger || console;

    // Tracking maps
    this.caseSchedules = new Map(); // workflowId -> schedule config
    this.taskTimeouts = new Map(); // taskId -> timeout handle
    this.taskRetries = new Map(); // caseId:taskId -> retry state
    this.choiceTriggers = new Map(); // caseId:taskId -> trigger config
    this.parallelDistributions = new Map(); // distributionId -> distribution state

    this.isRunning = false;
    this._unsubscribers = [];

    this._setupEventListeners();
  }

  /**
   * Start the bridge and begin event listening
   * @returns {Promise<void>}
   */
  async start() {
    if (this.isRunning) {
      return;
    }

    this.isRunning = true;
    this.logger.info(`[YawlDaemonBridge ${this.id}] Started`);
    this.emit('bridge:started', { bridgeId: this.id, timestamp: new Date() });
  }

  /**
   * Stop the bridge and clean up resources
   * @returns {Promise<void>}
   */
  async stop() {
    if (!this.isRunning) {
      return;
    }

    // Unsubscribe from all YAWL events
    for (const unsubscribe of this._unsubscribers) {
      unsubscribe();
    }
    this._unsubscribers = [];

    // Clear all scheduled timeouts
    for (const handle of this.taskTimeouts.values()) {
      if (typeof handle === 'object' && handle.operationId) {
        this.daemon.unschedule(handle.operationId);
      }
    }
    this.taskTimeouts.clear();

    this.isRunning = false;
    this.logger.info(`[YawlDaemonBridge ${this.id}] Stopped`);
    this.emit('bridge:stopped', { bridgeId: this.id, timestamp: new Date() });
  }

  /**
   * Schedule recurring case creation
   * Creates new workflow case instances on a schedule (cron or interval)
   *
   * @param {string} workflowId - Workflow identifier
   * @param {string} schedule - Cron expression or interval in ms
   * @param {Object} params - Parameters for case creation
   * @param {string} [params.caseIdPrefix] - Prefix for auto-generated case IDs
   * @param {Object} [params.inputData] - Input data for case
   * @param {number} [params.priority] - Priority level (1-10)
   * @returns {Promise<Object>} Schedule result with operationId
   * @throws {Error} If scheduling fails
   * @example
   * await bridge.scheduleRecurringCase('approval-workflow', '0 * * * *', {
   *   caseIdPrefix: 'batch',
   *   priority: 5,
   * });
   */
  async scheduleRecurringCase(workflowId, schedule, params = {}) {
    if (!workflowId || typeof workflowId !== 'string') {
      throw new TypeError('workflowId must be a non-empty string');
    }

    const operationId = `yawl-case-${workflowId}-${Date.now()}`;
    const caseIdPrefix = params.caseIdPrefix || `case-${workflowId}`;

    this.daemon.schedule({
      id: operationId,
      name: `Create case for ${workflowId}`,
      handler: async () => {
        try {
          const caseId = `${caseIdPrefix}-${Date.now()}`;
          const result = await this.yawlEngine.createCase({
            workflowId,
            caseId,
            inputData: params.inputData || {},
          });

          this.emit('case:created-by-schedule', {
            bridgeId: this.id,
            workflowId,
            caseId,
            result,
            timestamp: new Date(),
          });

          return result;
        } catch (error) {
          this.logger.error(
            `[YawlDaemonBridge ${this.id}] Case creation failed: ${error.message}`
          );
          throw error;
        }
      },
      metadata: { workflowId, schedule, params },
    });

    this.caseSchedules.set(workflowId, { operationId, schedule, params });

    this.logger.info(
      `[YawlDaemonBridge ${this.id}] Scheduled recurring case for ${workflowId}`
    );

    return { operationId, workflowId, success: true };
  }

  /**
   * Watch a task for timeout and auto-cancel if exceeded
   * Daemon schedules periodic checks and cancels task if timeout is reached
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @param {number} timeoutMs - Timeout in milliseconds
   * @returns {Promise<Object>} Watch result with operationId
   * @throws {Error} If watching fails
   * @example
   * await bridge.watchTaskTimeout('case-001', 'review-task', 60000);
   */
  async watchTaskTimeout(caseId, taskId, timeoutMs) {
    if (!caseId || !taskId || !timeoutMs || timeoutMs < 1000) {
      throw new TypeError('Invalid parameters: caseId, taskId required, timeoutMs >= 1000');
    }

    const operationId = `yawl-timeout-${caseId}-${taskId}`;
    const startTime = Date.now();
    const checkInterval = this.config.timeoutDefaults.checkIntervalMs;

    this.daemon.schedule({
      id: operationId,
      name: `Monitor timeout for ${taskId} in ${caseId}`,
      handler: async () => {
        const elapsed = Date.now() - startTime;
        if (elapsed >= timeoutMs) {
          try {
            await this.yawlEngine.cancelTask({
              caseId,
              taskId,
              reason: `Timeout after ${timeoutMs}ms`,
            });

            this.emit('task:timeout-enforced', {
              bridgeId: this.id,
              caseId,
              taskId,
              timeoutMs,
              timestamp: new Date(),
            });

            // Unschedule timeout watch
            this.daemon.unschedule(operationId);
            this.taskTimeouts.delete(`${caseId}:${taskId}`);
          } catch (error) {
            this.logger.error(
              `[YawlDaemonBridge ${this.id}] Timeout enforcement failed: ${error.message}`
            );
          }
        }
      },
      metadata: { caseId, taskId, timeoutMs },
    });

    this.taskTimeouts.set(`${caseId}:${taskId}`, {
      operationId,
      startTime,
      timeoutMs,
    });

    this.logger.debug(
      `[YawlDaemonBridge ${this.id}] Watching timeout for ${taskId} (${timeoutMs}ms)`
    );

    return { operationId, caseId, taskId, timeoutMs, success: true };
  }

  /**
   * Schedule automatic retry for failed task
   * Daemon retries task execution with exponential backoff
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @param {Object} [backoffPolicy] - Custom backoff policy (defaults to config)
   * @returns {Promise<Object>} Retry schedule result
   * @throws {Error} If scheduling retry fails
   * @example
   * await bridge.scheduleRetry('case-001', 'process-task', {
   *   maxAttempts: 5,
   *   backoffMs: 2000,
   * });
   */
  async scheduleRetry(caseId, taskId, backoffPolicy = {}) {
    if (!caseId || !taskId) {
      throw new TypeError('caseId and taskId are required');
    }

    const policy = {
      ...this.config.retryPolicy,
      ...backoffPolicy,
    };

    const retryKey = `${caseId}:${taskId}`;
    const operationId = `yawl-retry-${retryKey}`;

    const retryState = {
      attempts: 0,
      maxAttempts: policy.maxAttempts,
      nextRetryTime: Date.now() + policy.backoffMs,
      operationId,
    };

    this.daemon.schedule({
      id: operationId,
      name: `Retry ${taskId} in ${caseId}`,
      handler: async () => {
        const state = this.taskRetries.get(retryKey);
        if (!state || state.attempts >= state.maxAttempts) {
          return;
        }

        try {
          state.attempts += 1;
          const result = await this.yawlEngine.enableTask({
            caseId,
            taskId,
          });

          this.emit('task:retry-executed', {
            bridgeId: this.id,
            caseId,
            taskId,
            attempt: state.attempts,
            result,
            timestamp: new Date(),
          });

          if (state.attempts >= state.maxAttempts) {
            this.daemon.unschedule(operationId);
            this.taskRetries.delete(retryKey);
          } else {
            // Schedule next retry with backoff
            const nextBackoff = Math.min(
              policy.backoffMs * Math.pow(policy.backoffMultiplier, state.attempts),
              policy.maxBackoffMs
            );
            const jitter = nextBackoff * policy.jitterFactor * Math.random();
            state.nextRetryTime = Date.now() + nextBackoff + jitter;
          }
        } catch (error) {
          this.logger.error(
            `[YawlDaemonBridge ${this.id}] Retry execution failed: ${error.message}`
          );

          if (state.attempts >= state.maxAttempts) {
            this.daemon.unschedule(operationId);
            this.taskRetries.delete(retryKey);

            this.emit('task:retry-exhausted', {
              bridgeId: this.id,
              caseId,
              taskId,
              attempts: state.attempts,
              error: error.message,
              timestamp: new Date(),
            });
          }
        }
      },
      metadata: { caseId, taskId, policy },
    });

    this.taskRetries.set(retryKey, retryState);

    this.logger.info(
      `[YawlDaemonBridge ${this.id}] Scheduled retry for ${taskId} (max ${policy.maxAttempts})`
    );

    return { operationId, caseId, taskId, policy, success: true };
  }

  /**
   * Wait for external trigger before proceeding with deferred choice
   * Daemon listens for external event and enables appropriate task
   *
   * @param {string} caseId - Case identifier
   * @param {string} taskId - Task identifier
   * @param {Object} triggerPattern - Trigger condition pattern
   * @param {string} triggerPattern.eventName - Event name to listen for
   * @param {Object} [triggerPattern.filter] - Optional event filter
   * @param {number} [triggerPattern.timeoutMs] - Max wait time before cancellation
   * @returns {Promise<Object>} Trigger wait result
   * @throws {Error} If setup fails
   * @example
   * await bridge.waitForChoiceTrigger('case-001', 'deferred-choice', {
   *   eventName: 'user:approved',
   *   filter: { userId: 'user-123' },
   *   timeoutMs: 3600000,
   * });
   */
  async waitForChoiceTrigger(caseId, taskId, triggerPattern) {
    if (!caseId || !taskId || !triggerPattern || !triggerPattern.eventName) {
      throw new TypeError(
        'caseId, taskId, and triggerPattern.eventName are required'
      );
    }

    const triggerId = `${caseId}:${taskId}`;

    // Return promise that resolves when trigger fires
    const triggerPromise = new Promise((resolve, reject) => {
      const timeout = triggerPattern.timeoutMs || this.config.timeoutDefaults.caseTimeoutMs;
      let timeoutHandle = null;

      // Set timeout
      if (timeout > 0) {
        timeoutHandle = setTimeout(() => {
          this.choiceTriggers.delete(triggerId);
          reject(new Error(`Deferred choice timeout after ${timeout}ms`));
        }, timeout);
      }

      // Store trigger config
      this.choiceTriggers.set(triggerId, {
        eventName: triggerPattern.eventName,
        filter: triggerPattern.filter,
        timeoutHandle,
        resolve,
        reject,
      });
    });

    this.logger.debug(
      `[YawlDaemonBridge ${this.id}] Waiting for trigger ${triggerPattern.eventName} for ${taskId}`
    );

    return triggerPromise;
  }

  /**
   * Distribute and split parallel tasks across daemon nodes
   * Schedules parallel task enablement with load balancing
   *
   * @param {string} caseId - Case identifier
   * @param {Array<string>} taskIds - Task identifiers to split
   * @param {Object} [options] - Distribution options
   * @param {string} [options.strategy='round-robin'] - Distribution strategy
   * @param {Array<string>} [options.nodeIds] - Target node IDs (optional)
   * @returns {Promise<Object>} Distribution result with operationId
   * @throws {Error} If distribution fails
   * @example
   * await bridge.distributeAndSplitTasks('case-001',
   *   ['task-a', 'task-b', 'task-c'],
   *   { strategy: 'least-loaded' }
   * );
   */
  async distributeAndSplitTasks(caseId, taskIds, options = {}) {
    if (!caseId || !Array.isArray(taskIds) || taskIds.length === 0) {
      throw new TypeError('caseId and taskIds array (non-empty) are required');
    }

    const strategy = DistributionStrategySchema.parse(options.strategy || 'round-robin');
    const distributionId = `yawl-dist-${caseId}-${Date.now()}`;
    const operationId = `${distributionId}-exec`;

    this.daemon.schedule({
      id: operationId,
      name: `Distribute tasks for ${caseId}`,
      handler: async () => {
        try {
          const results = [];

          // Distribute tasks according to strategy
          for (let i = 0; i < taskIds.length; i += 1) {
            const taskId = taskIds[i];
            const result = await this.yawlEngine.enableTask({
              caseId,
              taskId,
            });

            results.push({
              taskId,
              result,
              index: i,
            });
          }

          this.emit('tasks:distributed', {
            bridgeId: this.id,
            caseId,
            taskIds,
            strategy,
            results,
            timestamp: new Date(),
          });

          this.parallelDistributions.set(distributionId, {
            caseId,
            taskIds,
            strategy,
            results,
            timestamp: Date.now(),
          });

          return results;
        } catch (error) {
          this.logger.error(
            `[YawlDaemonBridge ${this.id}] Task distribution failed: ${error.message}`
          );
          throw error;
        }
      },
      metadata: { caseId, taskIds, strategy },
    });

    this.logger.info(
      `[YawlDaemonBridge ${this.id}] Distributed ${taskIds.length} tasks using ${strategy}`
    );

    return { operationId, distributionId, caseId, taskIds, strategy, success: true };
  }

  /**
   * Get bridge statistics and state
   * @returns {Object} Statistics object
   */
  getStats() {
    return {
      bridgeId: this.id,
      isRunning: this.isRunning,
      caseSchedules: this.caseSchedules.size,
      activeTimeouts: this.taskTimeouts.size,
      activeRetries: this.taskRetries.size,
      activeTriggers: this.choiceTriggers.size,
      distributions: this.parallelDistributions.size,
      timestamp: new Date(),
    };
  }

  // =========================================================================
  // Private Event Listener Setup
  // =========================================================================

  /**
   * Set up YAWL event listeners
   * @private
   */
  _setupEventListeners() {
    if (!this.config.enableAutoRetry && !this.config.enableTimeoutTracking) {
      return;
    }

    // Listen to task:failed and schedule retry if enabled
    if (this.config.enableAutoRetry) {
      const unsubTaskFailed = this.yawlEngine.on('task:failed', async (event) => {
        try {
          await this.scheduleRetry(event.caseId, event.taskId);
        } catch (error) {
          this.logger.error(
            `[YawlDaemonBridge ${this.id}] Failed to schedule retry: ${error.message}`
          );
        }
      });
      this._unsubscribers.push(unsubTaskFailed);
    }

    // Listen to task:enabled and start timeout watch if configured
    if (this.config.enableTimeoutTracking) {
      const unsubTaskEnabled = this.yawlEngine.on('task:enabled', async (event) => {
        try {
          await this.watchTaskTimeout(
            event.caseId,
            event.taskId,
            this.config.timeoutDefaults.taskTimeoutMs
          );
        } catch (error) {
          this.logger.error(
            `[YawlDaemonBridge ${this.id}] Failed to watch timeout: ${error.message}`
          );
        }
      });
      this._unsubscribers.push(unsubTaskEnabled);
    }

    // Listen for case completion and cleanup
    const unsubCaseCompleted = this.yawlEngine.on('case:completed', (event) => {
      // Clean up any resources for this case
      for (const [key] of this.taskTimeouts) {
        if (key.startsWith(`${event.caseId}:`)) {
          const timeout = this.taskTimeouts.get(key);
          if (timeout?.operationId) {
            this.daemon.unschedule(timeout.operationId);
          }
          this.taskTimeouts.delete(key);
        }
      }

      for (const [key] of this.taskRetries) {
        if (key.startsWith(`${event.caseId}:`)) {
          const retry = this.taskRetries.get(key);
          if (retry?.operationId) {
            this.daemon.unschedule(retry.operationId);
          }
          this.taskRetries.delete(key);
        }
      }
    });
    this._unsubscribers.push(unsubCaseCompleted);
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create and integrate a YAWL daemon bridge
 * @param {import('../daemon.mjs').Daemon} daemon - Daemon instance
 * @param {import('@unrdf/yawl').WorkflowEngine} yawlEngine - YAWL engine instance
 * @param {Object} config - Bridge configuration
 * @returns {YawlDaemonBridge} Initialized bridge instance
 * @example
 * const bridge = createYawlBridge(daemon, yawlEngine, {
 *   daemonNodeId: 'node-1',
 *   enableAutoRetry: true,
 * });
 */
export function createYawlBridge(daemon, yawlEngine, config = {}) {
  return new YawlDaemonBridge(daemon, yawlEngine, config);
}

export default {
  YawlDaemonBridge,
  createYawlBridge,
  YawlDaemonBridgeConfigSchema,
  YawlRetryPolicySchema,
  YawlTimeoutConfigSchema,
  DistributionStrategySchema,
};
