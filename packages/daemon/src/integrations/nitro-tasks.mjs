/**
 * @file Nitro Tasks Integration for Daemon
 * @module @unrdf/daemon/integrations/nitro-tasks
 * @description Bidirectional integration between Daemon and Nitro Task Engine
 * Enables daemon operations to be scheduled and executed via Nitro's task system
 */

import { z } from 'zod';
import { EventEmitter } from 'events';
import { Daemon } from '../daemon.mjs';

/**
 * Nitro task configuration schema
 * @type {z.ZodType}
 */
export const NitroTaskConfigSchema = z.object({
  executorId: z.string().min(1).default(() => `nitro-executor-${Date.now()}`),
  autoStart: z.boolean().default(true),
  autoRegister: z.boolean().default(true),
  timeout: z.number().int().positive().default(30000),
  maxRetries: z.number().int().min(0).default(3),
  enableEventRelay: z.boolean().default(true),
  enableMetrics: z.boolean().default(true),
  taskPrefix: z.string().default('daemon:'),
});

/**
 * Nitro task metadata schema for mapping daemon operations
 * @type {z.ZodType}
 */
export const NitroTaskMetadataSchema = z.object({
  daemonOperationId: z.string(),
  operationType: z.string(),
  priority: z.enum(['low', 'normal', 'high']).default('normal'),
  tags: z.array(z.string()).default([]),
  cronExpression: z.string().optional(),
  description: z.string().optional(),
  retryable: z.boolean().default(true),
});

/**
 * Executor that integrates Daemon with Nitro Task Engine
 * Bridges daemon operations to Nitro's execution model
 * @extends EventEmitter
 */
export class NitroTaskExecutor extends EventEmitter {
  /**
   * Create a new Nitro task executor
   * @param {Daemon} daemon - Daemon instance to integrate
   * @param {Object} options - Configuration
   * @param {string} [options.executorId] - Executor identifier
   * @param {boolean} [options.autoStart=true] - Start on creation
   * @param {boolean} [options.autoRegister=true] - Auto-register operations as Nitro tasks
   * @param {number} [options.timeout=30000] - Task execution timeout
   * @param {number} [options.maxRetries=3] - Maximum retry attempts
   * @param {boolean} [options.enableEventRelay=true] - Relay events to Nitro
   * @param {boolean} [options.enableMetrics=true] - Collect task metrics
   * @param {string} [options.taskPrefix='daemon:'] - Prefix for Nitro task names
   */
  constructor(daemon, options = {}) {
    super();
    if (!(daemon instanceof Daemon)) {
      throw new Error('daemon must be a Daemon instance');
    }

    const config = NitroTaskConfigSchema.parse(options);
    this.id = config.executorId;
    this.daemon = daemon;
    this.config = config;

    // Task tracking
    this.daemonToNitroMap = new Map();
    this.nitroToDaemonMap = new Map();
    this.taskMetadata = new Map();

    // Metrics
    this.metrics = {
      tasksExecuted: 0,
      tasksSucceeded: 0,
      tasksFailed: 0,
      totalDuration: 0,
      averageDuration: 0,
    };

    // Mock Nitro task registry (in real implementation, would connect to actual Nitro)
    this.nitroTasks = new Map();
    this.nitroTaskResults = new Map();

    this._setupListeners();
    if (config.autoStart) this.start();
  }

  /**
   * Setup event listeners between daemon and executor
   * @private
   */
  _setupListeners() {
    if (!this.config.enableEventRelay) return;

    this.daemon.on('operation:started', (data) => {
      const nitroTaskId = this.daemonToNitroMap.get(data.operationId);
      if (nitroTaskId) {
        this.emit('task:started', {
          executorId: this.id,
          daemonOperationId: data.operationId,
          nitroTaskId,
          timestamp: new Date(),
        });
      }
    });

    this.daemon.on('operation:success', (data) => {
      const nitroTaskId = this.daemonToNitroMap.get(data.operationId);
      if (nitroTaskId && this.config.enableMetrics) {
        this.metrics.tasksSucceeded += 1;
        this.metrics.totalDuration += data.duration || 0;
        this.metrics.averageDuration = this.metrics.totalDuration / this.metrics.tasksSucceeded;
      }
      if (nitroTaskId) {
        this.emit('task:succeeded', {
          executorId: this.id,
          daemonOperationId: data.operationId,
          nitroTaskId,
          duration: data.duration,
          timestamp: new Date(),
        });
      }
    });

    this.daemon.on('operation:failure', (data) => {
      const nitroTaskId = this.daemonToNitroMap.get(data.operationId);
      if (nitroTaskId) {
        if (this.config.enableMetrics) {
          this.metrics.tasksFailed += 1;
        }
        this.emit('task:failed', {
          executorId: this.id,
          daemonOperationId: data.operationId,
          nitroTaskId,
          error: data.error,
          duration: data.duration,
          timestamp: new Date(),
        });
      }
    });
  }

  /**
   * Start the executor
   * @returns {Promise<void>}
   */
  async start() {
    await this.daemon.start();
    this.emit('executor:started', { executorId: this.id });
  }

  /**
   * Stop the executor
   * @returns {Promise<void>}
   */
  async stop() {
    await this.daemon.stop();
    this.emit('executor:stopped', { executorId: this.id });
  }

  /**
   * Register a daemon operation as a Nitro task
   * @param {string} operationId - Daemon operation ID
   * @param {string} operationName - Human-readable operation name
   * @param {Object} nitroTaskMeta - Nitro task metadata
   * @param {string} nitroTaskMeta.description - Task description
   * @param {string} [nitroTaskMeta.cronExpression] - Cron schedule
   * @param {string} [nitroTaskMeta.priority='normal'] - Task priority
   * @param {Array<string>} [nitroTaskMeta.tags=[]] - Task tags
   * @returns {Object} Registration result
   */
  registerOperationAsTask(operationId, operationName, nitroTaskMeta = {}) {
    const operation = this.daemon.operations.get(operationId);
    if (!operation) {
      throw new Error(`Operation not found: ${operationId}`);
    }

    const taskMetadata = NitroTaskMetadataSchema.parse({
      daemonOperationId: operationId,
      operationType: operationName,
      ...nitroTaskMeta,
    });

    const nitroTaskId = `${this.config.taskPrefix}${operationId}`;

    // Register in Nitro (mock implementation)
    this.nitroTasks.set(nitroTaskId, {
      id: nitroTaskId,
      name: operationName,
      meta: taskMetadata,
      handler: async (payload) => this._executeViaNitro(operationId, payload),
    });

    this.daemonToNitroMap.set(operationId, nitroTaskId);
    this.nitroToDaemonMap.set(nitroTaskId, operationId);
    this.taskMetadata.set(operationId, taskMetadata);

    this.emit('task:registered', {
      executorId: this.id,
      daemonOperationId: operationId,
      nitroTaskId,
      metadata: taskMetadata,
    });

    return { operationId, nitroTaskId, taskMetadata, success: true };
  }

  /**
   * Unregister a task
   * @param {string} operationId - Daemon operation ID to unregister
   * @returns {boolean} Whether successfully unregistered
   */
  unregisterTask(operationId) {
    const nitroTaskId = this.daemonToNitroMap.get(operationId);
    if (!nitroTaskId) return false;

    this.nitroTasks.delete(nitroTaskId);
    this.daemonToNitroMap.delete(operationId);
    this.nitroToDaemonMap.delete(nitroTaskId);
    this.taskMetadata.delete(operationId);

    this.emit('task:unregistered', {
      executorId: this.id,
      daemonOperationId: operationId,
      nitroTaskId,
    });

    return true;
  }

  /**
   * Execute a task via Nitro
   * @param {string} nitroTaskId - Nitro task ID
   * @param {Object} [payload={}] - Task payload
   * @returns {Promise<Object>} Execution result
   */
  async runTask(nitroTaskId, payload = {}) {
    const task = this.nitroTasks.get(nitroTaskId);
    if (!task) {
      throw new Error(`Task not found: ${nitroTaskId}`);
    }

    if (this.config.enableMetrics) {
      this.metrics.tasksExecuted += 1;
    }

    const operationId = this.nitroToDaemonMap.get(nitroTaskId);

    try {
      const result = await Promise.race([
        task.handler(payload),
        new Promise((_, rej) =>
          setTimeout(
            () => rej(new Error(`Task timeout: ${this.config.timeout}ms`)),
            this.config.timeout
          )
        ),
      ]);

      const execution = {
        success: true,
        taskId: nitroTaskId,
        operationId,
        result,
        timestamp: new Date(),
      };

      this.nitroTaskResults.set(nitroTaskId, execution);
      return execution;
    } catch (error) {
      const execution = {
        success: false,
        taskId: nitroTaskId,
        operationId,
        error: error instanceof Error ? error.message : String(error),
        timestamp: new Date(),
      };

      this.nitroTaskResults.set(nitroTaskId, execution);
      throw error;
    }
  }

  /**
   * Execute operation via Nitro bridge
   * @private
   * @param {string} operationId - Daemon operation ID
   * @param {Object} payload - Task payload
   * @returns {Promise<*>} Operation result
   */
  async _executeViaNitro(operationId, payload = {}) {
    const operation = this.daemon.operations.get(operationId);
    if (!operation) {
      throw new Error(`Operation not found: ${operationId}`);
    }

    // Merge payload into operation context if needed
    const _context = {
      ...payload,
      sourceSystem: 'nitro',
      timestamp: new Date(),
    };

    // Execute via daemon
    return this.daemon.execute(operationId);
  }

  /**
   * List all registered tasks
   * @returns {Array<Object>} Array of task metadata
   */
  listTasks() {
    return Array.from(this.taskMetadata.entries()).map(([opId, meta]) => ({
      daemonOperationId: opId,
      nitroTaskId: this.daemonToNitroMap.get(opId),
      ...meta,
    }));
  }

  /**
   * Discover all daemon operations as potential tasks
   * @param {Function} [filter] - Optional filter function
   * @returns {Array<Object>} Array of discovered operations
   */
  discoverOperationsAsTaskCandidates(filter) {
    const candidates = this.daemon.listOperations();

    if (filter && typeof filter === 'function') {
      return candidates.filter(filter);
    }

    return candidates;
  }

  /**
   * Get task execution history
   * @param {string} [taskId] - Optional specific task ID
   * @returns {Array<Object>} Execution records
   */
  getExecutionHistory(taskId) {
    if (taskId) {
      const result = this.nitroTaskResults.get(taskId);
      return result ? [result] : [];
    }

    return Array.from(this.nitroTaskResults.values());
  }

  /**
   * Get executor metrics
   * @returns {Object} Current metrics
   */
  getMetrics() {
    return {
      executorId: this.id,
      ...this.metrics,
      registeredTasks: this.daemonToNitroMap.size,
      executionHistory: this.nitroTaskResults.size,
      daemonHealth: this.daemon.getHealth(),
      daemonMetrics: this.daemon.getMetrics(),
    };
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = {
      tasksExecuted: 0,
      tasksSucceeded: 0,
      tasksFailed: 0,
      totalDuration: 0,
      averageDuration: 0,
    };
  }

  /**
   * Validate task can execute (pre-flight checks)
   * @param {string} taskId - Task ID to validate
   * @returns {Object} Validation result
   */
  validateTask(taskId) {
    const task = this.nitroTasks.get(taskId);
    if (!task) {
      return { valid: false, reason: 'Task not found' };
    }

    const operationId = this.nitroToDaemonMap.get(taskId);
    const operation = this.daemon.operations.get(operationId);
    if (!operation) {
      return { valid: false, reason: 'Associated operation not found' };
    }

    if (!this.daemon.isRunning) {
      return { valid: false, reason: 'Daemon not running' };
    }

    return { valid: true, taskId, operationId };
  }

  /**
   * Get status of executor and all tasks
   * @returns {Object} Status information
   */
  getStatus() {
    return {
      executorId: this.id,
      running: this.daemon.isRunning,
      registeredTasks: this.daemonToNitroMap.size,
      metrics: this.getMetrics(),
      tasks: this.listTasks(),
      lastActivity: this.nitroTaskResults.size > 0 ? this.nitroTaskResults.entries()[0]?.[1]?.timestamp : null,
    };
  }
}

/**
 * Create and initialize a Nitro task executor
 * @param {Daemon} daemon - Daemon instance
 * @param {Object} [options={}] - Configuration
 * @returns {NitroTaskExecutor} Configured executor
 */
export function createNitroTaskExecutor(daemon, options = {}) {
  return new NitroTaskExecutor(daemon, options);
}

/**
 * Integrate daemon with Nitro task system
 * @param {Daemon} daemon - Daemon instance
 * @param {Array<Object>} operations - Operations to register as tasks
 * @param {Object} [config={}] - Integration config
 * @returns {Promise<NitroTaskExecutor>} Configured executor
 */
export async function integrateNitroTasks(daemon, operations = [], config = {}) {
  const executor = new NitroTaskExecutor(daemon, config);

  if (config.autoRegister) {
    for (const op of operations) {
      executor.registerOperationAsTask(op.id, op.name, op.meta || {});
    }
  }

  return executor;
}

/**
 * Nitro Tasks Integration module exports
 * @type {Object}
 * @property {typeof NitroTaskExecutor} NitroTaskExecutor - Executor class
 * @property {z.ZodType} NitroTaskConfigSchema - Config schema
 * @property {z.ZodType} NitroTaskMetadataSchema - Metadata schema
 * @property {Function} createNitroTaskExecutor - Factory function
 * @property {Function} integrateNitroTasks - Integration function
 */
export default {
  NitroTaskExecutor,
  NitroTaskConfigSchema,
  NitroTaskMetadataSchema,
  createNitroTaskExecutor,
  integrateNitroTasks,
};
