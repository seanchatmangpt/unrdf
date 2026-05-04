/**
 * @file YAWL Queue Adapter - BullMQ Integration for Distributed YAWL Workflows
 * @module @unrdf/yawl-queue/adapter
 *
 * @description
 * Maps YAWL workflow tasks to BullMQ jobs for distributed execution.
 * Features:
 * - Task-to-job mapping with priority and delay
 * - Redis-backed distributed state management
 * - Retry policies based on YAWL cancellation regions
 * - YAWL receipt generation from BullMQ job events
 * - Worker pool coordination
 *
 * @example
 * import { YAWLQueueAdapter } from '@unrdf/yawl-queue';
 * import { createWorkflow, createWorkflowEngine } from '@unrdf/yawl';
 *
 * const adapter = new YAWLQueueAdapter({
 *   redis: { host: 'localhost', port: 6379 },
 *   queueName: 'yawl-workflows'
 * });
 *
 * const workflow = createWorkflow({ ... });
 * await adapter.registerWorkflow(workflow);
 * await adapter.executeCase(workflow.id, { initialData });
 */

import { Queue, Worker, QueueEvents } from 'bullmq';
import IORedis from 'ioredis';
import { z } from 'zod';
import { createWorkflowEngine, ENGINE_EVENTS } from '@unrdf/yawl';
import { toISO, now } from '@unrdf/kgc-4d';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Job data schema for YAWL tasks
 */
export const YAWLJobDataSchema = z.object({
  caseId: z.string(),
  workflowId: z.string(),
  taskId: z.string(),
  workItemId: z.string().optional(),
  action: z.enum(['enable', 'start', 'complete', 'cancel']),
  input: z.record(z.any()).optional(),
  output: z.record(z.any()).optional(),
  actor: z.string().optional(),
  reason: z.string().optional(),
});

/**
 * Adapter configuration schema
 */
export const AdapterConfigSchema = z.object({
  redis: z.object({
    host: z.string().default('localhost'),
    port: z.number().default(6379),
    password: z.string().optional(),
    db: z.number().optional(),
  }).optional(),
  queueName: z.string().default('yawl-workflows'),
  defaultJobOptions: z.object({
    attempts: z.number().default(3),
    backoff: z.object({
      type: z.enum(['exponential', 'fixed']).default('exponential'),
      delay: z.number().default(1000),
    }).optional(),
    removeOnComplete: z.boolean().default(false),
    removeOnFail: z.boolean().default(false),
  }).optional(),
  engineConfig: z.record(z.any()).optional(),
});

// =============================================================================
// YAWL Queue Adapter
// =============================================================================

/**
 * Adapter for executing YAWL workflows using BullMQ distributed queues
 */
export class YAWLQueueAdapter {
  /**
   * @param {Object} config - Adapter configuration
   * @param {Object} [config.redis] - Redis connection options
   * @param {string} [config.queueName='yawl-workflows'] - Queue name
   * @param {Object} [config.defaultJobOptions] - Default BullMQ job options
   * @param {Object} [config.engineConfig] - YAWL engine configuration
   */
  constructor(config = {}) {
    const validatedConfig = AdapterConfigSchema.parse(config);

    this.config = validatedConfig;
    this.queueName = validatedConfig.queueName;

    // Create Redis connection
    this.redis = new IORedis(validatedConfig.redis || {});

    // Create BullMQ queue
    this.queue = new Queue(this.queueName, {
      connection: this.redis,
      defaultJobOptions: validatedConfig.defaultJobOptions,
    });

    // Create queue events listener
    this.queueEvents = new QueueEvents(this.queueName, {
      connection: this.redis,
    });

    // Create YAWL engine
    this.engine = createWorkflowEngine(validatedConfig.engineConfig || {});

    // Track workers
    this.workers = new Map();

    // Track job-to-case mapping
    this.jobMapping = new Map(); // jobId -> { caseId, workItemId, taskId }

    // Receipt storage
    this.receipts = new Map(); // caseId -> receipts[]

    // Setup event listeners
    this._setupEventListeners();
  }

  // ===========================================================================
  // Workflow Registration
  // ===========================================================================

  /**
   * Register a workflow definition with the adapter
   * @param {import('@unrdf/yawl').Workflow} workflow - Workflow to register
   * @returns {Promise<void>}
   */
  async registerWorkflow(workflow) {
    this.engine.registerWorkflow(workflow);

    // Store workflow metadata in Redis for worker access
    await this.redis.set(
      `yawl:workflow:${workflow.id}`,
      JSON.stringify({
        id: workflow.id,
        name: workflow.name,
        version: workflow.version,
        taskCount: workflow.getTasks().length,
        registeredAt: toISO(now()),
      })
    );
  }

  /**
   * Unregister a workflow
   * @param {string} workflowId - Workflow ID to unregister
   * @returns {Promise<void>}
   */
  async unregisterWorkflow(workflowId) {
    this.engine.unregisterWorkflow(workflowId);
    await this.redis.del(`yawl:workflow:${workflowId}`);
  }

  // ===========================================================================
  // Case Execution
  // ===========================================================================

  /**
   * Execute a new workflow case via distributed queue
   * @param {string} workflowId - Workflow ID
   * @param {Object} [initialData={}] - Initial case data
   * @param {Object} [options={}] - Execution options
   * @returns {Promise<{caseId: string, jobId: string}>}
   */
  async executeCase(workflowId, initialData = {}, options = {}) {
    // Create case in engine
    const { case: caseInstance, receipt } = await this.engine.createCase(
      workflowId,
      initialData,
      options
    );

    // Store receipt
    this._storeReceipt(caseInstance.id, receipt);

    // Get enabled tasks (should be the start task)
    const enabledTasks = caseInstance.getEnabledWorkItems();

    // Queue first task
    if (enabledTasks.length > 0) {
      const firstTask = enabledTasks[0];
      const job = await this._queueTask(caseInstance.id, workflowId, firstTask);

      return {
        caseId: caseInstance.id,
        jobId: job.id,
      };
    }

    return {
      caseId: caseInstance.id,
      jobId: null,
    };
  }

  /**
   * Get case status
   * @param {string} caseId - Case ID
   * @returns {Promise<Object>} Case status
   */
  async getCaseStatus(caseId) {
    const caseInstance = this.engine.cases.get(caseId);
    if (!caseInstance) {
      throw new Error(`Case ${caseId} not found`);
    }

    return {
      caseId: caseInstance.id,
      workflowId: caseInstance.workflowId,
      status: caseInstance.status,
      enabledTasks: caseInstance.getEnabledWorkItems().length,
      activeTasks: caseInstance.getActiveWorkItems().length,
      completedTasks: caseInstance.completedTasks.size,
      receipts: this.receipts.get(caseId)?.length || 0,
    };
  }

  // ===========================================================================
  // Worker Management
  // ===========================================================================

  /**
   * Create a worker to process YAWL tasks
   * @param {Object} [options={}] - Worker options
   * @param {number} [options.concurrency=1] - Number of concurrent jobs
   * @param {Function} [options.taskHandler] - Custom task execution handler
   * @returns {Worker} BullMQ worker instance
   */
  createWorker(options = {}) {
    const { concurrency = 1, taskHandler } = options;

    const worker = new Worker(
      this.queueName,
      async (job) => {
        return this._processJob(job, taskHandler);
      },
      {
        connection: this.redis,
        concurrency,
      }
    );

    // Track worker
    const workerId = `worker-${this.workers.size + 1}`;
    this.workers.set(workerId, worker);

    // Setup worker event listeners
    worker.on('completed', (job, result) => {
      console.log(`[${workerId}] Job ${job.id} completed:`, result);
    });

    worker.on('failed', (job, err) => {
      console.error(`[${workerId}] Job ${job?.id} failed:`, err.message);
    });

    worker.on('error', (err) => {
      console.error(`[${workerId}] Worker error:`, err);
    });

    return worker;
  }

  /**
   * Close a specific worker
   * @param {string} workerId - Worker ID to close
   * @returns {Promise<void>}
   */
  async closeWorker(workerId) {
    const worker = this.workers.get(workerId);
    if (worker) {
      await worker.close();
      this.workers.delete(workerId);
    }
  }

  // ===========================================================================
  // Job Processing
  // ===========================================================================

  /**
   * Process a YAWL task job
   * @private
   * @param {Object} job - BullMQ job
   * @param {Function} [customHandler] - Custom task handler
   * @returns {Promise<Object>} Job result with receipt
   */
  async _processJob(job, customHandler) {
    const jobData = YAWLJobDataSchema.parse(job.data);
    const { caseId, workflowId, taskId, workItemId, action, input, output, actor, reason } = jobData;

    console.log(`Processing job ${job.id}: ${action} ${taskId} in case ${caseId}`);

    try {
      let result;
      let receipt;

      switch (action) {
        case 'enable':
          result = await this.engine.enableTask(caseId, taskId, actor);
          receipt = result.receipt;
          this._storeReceipt(caseId, receipt);

          // Auto-queue start action
          await this._queueTask(caseId, workflowId, result.task, 'start');
          break;

        case 'start':
          result = await this.engine.startTask(caseId, workItemId, { actor });
          receipt = result.receipt;
          this._storeReceipt(caseId, receipt);

          // Execute custom task handler if provided
          if (customHandler) {
            const taskOutput = await customHandler(job, result.task);

            // Auto-queue completion with output
            await this._queueTask(caseId, workflowId, result.task, 'complete', taskOutput);
          } else {
            // Auto-complete with input as output (passthrough)
            await this._queueTask(caseId, workflowId, result.task, 'complete', input || {});
          }
          break;

        case 'complete':
          result = await this.engine.completeTask(caseId, workItemId, output || {}, actor);
          receipt = result.receipt;
          this._storeReceipt(caseId, receipt);

          // Queue downstream enabled tasks
          for (const downstream of result.downstreamEnabled) {
            await this._queueTask(caseId, workflowId, downstream.task, 'start');
          }
          break;

        case 'cancel':
          result = await this.engine.cancelTask(caseId, workItemId, reason, actor);
          receipt = result.receipt;
          this._storeReceipt(caseId, receipt);
          break;

        default:
          throw new Error(`Unknown action: ${action}`);
      }

      return {
        success: true,
        action,
        caseId,
        taskId,
        workItemId: result?.task?.id,
        receiptId: receipt?.id,
        timestamp: toISO(now()),
      };

    } catch (error) {
      console.error(`Job ${job.id} processing error:`, error);

      // Apply retry policy based on cancellation region
      const shouldRetry = await this._shouldRetryTask(caseId, taskId, job.attemptsMade);

      if (!shouldRetry) {
        // Cancel task in YAWL engine
        if (workItemId) {
          await this.engine.cancelTask(caseId, workItemId, error.message, 'system');
        }
      }

      throw error;
    }
  }

  /**
   * Queue a YAWL task as BullMQ job
   * @private
   * @param {string} caseId - Case ID
   * @param {string} workflowId - Workflow ID
   * @param {Object} task - YAWL task instance
   * @param {string} [action='start'] - Action to perform
   * @param {Object} [data={}] - Additional data
   * @returns {Promise<Object>} BullMQ job
   */
  async _queueTask(caseId, workflowId, task, action = 'start', data = {}) {
    const jobData = {
      caseId,
      workflowId,
      taskId: task.taskDefId || task.id,
      workItemId: task.id,
      action,
      input: data,
      output: action === 'complete' ? data : undefined,
    };

    // Extract priority and delay from task metadata
    const priority = task.priority || 0;
    const delay = task.delay || 0;

    const job = await this.queue.add(
      `${action}-${task.taskDefId || task.id}`,
      jobData,
      {
        priority,
        delay,
        jobId: `${caseId}-${task.id}-${action}-${Date.now()}`,
      }
    );

    // Track mapping
    this.jobMapping.set(job.id, {
      caseId,
      workItemId: task.id,
      taskId: task.taskDefId || task.id,
    });

    return job;
  }

  /**
   * Determine if task should be retried based on cancellation region
   * @private
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task ID
   * @param {number} attemptsMade - Number of attempts made
   * @returns {Promise<boolean>} Whether to retry
   */
  async _shouldRetryTask(caseId, taskId, attemptsMade) {
    const caseInstance = this.engine.cases.get(caseId);
    if (!caseInstance) return false;

    const workflow = this.engine.workflows.get(caseInstance.workflowId);
    if (!workflow) return false;

    const taskDef = workflow.getTask(taskId);
    if (!taskDef) return false;

    // If task is in a cancellation region, apply region retry policy
    if (taskDef.cancellationRegion) {
      // Custom retry logic based on region
      const maxAttempts = this.config.defaultJobOptions?.attempts || 3;
      return attemptsMade < maxAttempts;
    }

    // Default: retry
    return true;
  }

  /**
   * Store receipt for a case
   * @private
   * @param {string} caseId - Case ID
   * @param {Object} receipt - YAWL receipt
   */
  _storeReceipt(caseId, receipt) {
    if (!this.receipts.has(caseId)) {
      this.receipts.set(caseId, []);
    }
    this.receipts.get(caseId).push(receipt);
  }

  // ===========================================================================
  // Event Listeners
  // ===========================================================================

  /**
   * Setup event listeners for BullMQ and YAWL engine
   * @private
   */
  _setupEventListeners() {
    // Listen to YAWL engine events
    this.engine.on(ENGINE_EVENTS.TASK_ENABLED, (event) => {
      console.log(`[YAWL] Task enabled: ${event.taskId} in case ${event.caseId}`);
    });

    this.engine.on(ENGINE_EVENTS.TASK_STARTED, (event) => {
      console.log(`[YAWL] Task started: ${event.workItemId} in case ${event.caseId}`);
    });

    this.engine.on(ENGINE_EVENTS.TASK_COMPLETED, (event) => {
      console.log(`[YAWL] Task completed: ${event.workItemId} in case ${event.caseId}`);
    });

    this.engine.on(ENGINE_EVENTS.CASE_COMPLETED, (event) => {
      console.log(`[YAWL] Case completed: ${event.caseId}`);
    });

    // Listen to BullMQ queue events
    this.queueEvents.on('completed', ({ jobId, returnvalue }) => {
      console.log(`[BullMQ] Job completed: ${jobId}`);
    });

    this.queueEvents.on('failed', ({ jobId, failedReason }) => {
      console.log(`[BullMQ] Job failed: ${jobId} - ${failedReason}`);
    });
  }

  // ===========================================================================
  // Cleanup
  // ===========================================================================

  /**
   * Close all workers, queue, and Redis connections
   * @returns {Promise<void>}
   */
  async close() {
    // Close all workers
    for (const [workerId, worker] of this.workers.entries()) {
      await worker.close();
      console.log(`Closed worker: ${workerId}`);
    }
    this.workers.clear();

    // Close queue events
    await this.queueEvents.close();

    // Close queue
    await this.queue.close();

    // Close Redis connection
    await this.redis.quit();

    console.log('YAWLQueueAdapter closed');
  }

  // ===========================================================================
  // Statistics
  // ===========================================================================

  /**
   * Get adapter statistics
   * @returns {Promise<Object>} Statistics
   */
  async getStats() {
    const queueStats = await this.queue.getJobCounts();

    return {
      queue: {
        name: this.queueName,
        ...queueStats,
      },
      workers: {
        count: this.workers.size,
        ids: Array.from(this.workers.keys()),
      },
      engine: this.engine.getStatistics(),
      receipts: {
        totalCases: this.receipts.size,
        totalReceipts: Array.from(this.receipts.values()).reduce((sum, r) => sum + r.length, 0),
      },
    };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default YAWLQueueAdapter;
