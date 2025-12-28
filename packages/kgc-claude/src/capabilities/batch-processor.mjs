/**
 * Batch Processor - Queue-Based Task Execution
 *
 * Provides batch processing capabilities for Claude Code automation.
 * Supports priority queues, retries, progress tracking, and result aggregation.
 *
 * @module @unrdf/kgc-claude/capabilities/batch-processor
 */

import { z } from 'zod';
import { createHeadlessRunner } from './headless-runner.mjs';
import { EventEmitter } from 'events';
import { createHash } from 'crypto';

/**
 * Task priority levels
 */
export const TaskPriority = {
  LOW: 0,
  NORMAL: 1,
  HIGH: 2,
  CRITICAL: 3,
};

/**
 * Task status enumeration
 */
export const TaskStatus = {
  PENDING: 'pending',
  QUEUED: 'queued',
  RUNNING: 'running',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
  RETRYING: 'retrying',
};

/**
 * Batch task schema
 */
export const BatchTaskSchema = z.object({
  id: z.string(),
  prompt: z.string().min(1),
  priority: z.number().int().min(0).max(3).default(1),
  options: z.any().default({}),
  retries: z.number().int().min(0).default(0),
  maxRetries: z.number().int().min(0).default(3),
  timeout: z.number().positive().default(120000),
  dependencies: z.array(z.string()).default([]),
  metadata: z.any().default({}),
});

/**
 * Batch processor configuration schema
 */
export const BatchProcessorConfigSchema = z.object({
  concurrency: z.number().int().positive().default(3),
  retryDelay: z.number().int().positive().default(5000),
  defaultPriority: z.number().int().min(0).max(3).default(1),
  defaultTimeout: z.number().positive().default(120000),
  maxRetries: z.number().int().min(0).default(3),
  debug: z.boolean().default(false),
});

/**
 * Batch result schema
 */
export const BatchResultSchema = z.object({
  taskId: z.string(),
  status: z.enum(['completed', 'failed', 'cancelled']),
  result: z.any().optional(),
  error: z.string().optional(),
  attempts: z.number(),
  duration: z.number(),
  startedAt: z.number(),
  completedAt: z.number(),
});

/**
 * Progress report schema
 */
export const ProgressReportSchema = z.object({
  total: z.number(),
  pending: z.number(),
  queued: z.number(),
  running: z.number(),
  completed: z.number(),
  failed: z.number(),
  cancelled: z.number(),
  percentComplete: z.number(),
  estimatedTimeRemaining: z.number().nullable(),
});

/**
 * Batch Processor - Queue-based task execution with priorities and retries
 */
export class BatchProcessor extends EventEmitter {
  /**
   * @param {object} config - Processor configuration
   */
  constructor(config = {}) {
    super();
    this.config = BatchProcessorConfigSchema.parse(config);
    this.runner = createHeadlessRunner({ debug: this.config.debug });

    this.tasks = new Map(); // taskId -> task
    this.results = new Map(); // taskId -> result
    this.queues = {
      [TaskPriority.CRITICAL]: [],
      [TaskPriority.HIGH]: [],
      [TaskPriority.NORMAL]: [],
      [TaskPriority.LOW]: [],
    };
    this.running = new Set();
    this.completed = new Set();
    this.failed = new Set();
    this.cancelled = new Set();

    this.isProcessing = false;
    this.processStartTime = null;
  }

  /**
   * Add task to batch queue
   *
   * @param {object} task - Task definition
   * @returns {string} Task ID
   */
  addTask(task) {
    const validated = BatchTaskSchema.parse({
      id: task.id || this._generateTaskId(task.prompt),
      priority: task.priority ?? this.config.defaultPriority,
      timeout: task.timeout ?? this.config.defaultTimeout,
      maxRetries: task.maxRetries ?? this.config.maxRetries,
      ...task,
    });

    if (this.tasks.has(validated.id)) {
      throw new Error(`Task with ID ${validated.id} already exists`);
    }

    this.tasks.set(validated.id, {
      ...validated,
      status: TaskStatus.PENDING,
      addedAt: Date.now(),
    });

    this._queueTask(validated.id);

    this.emit('task:added', { taskId: validated.id, task: validated });

    if (this.config.debug) {
      console.log(`[BatchProcessor] Task added: ${validated.id} (priority: ${validated.priority})`);
    }

    return validated.id;
  }

  /**
   * Add multiple tasks at once
   *
   * @param {Array<object>} tasks - Array of task definitions
   * @returns {Array<string>} Array of task IDs
   */
  addTasks(tasks) {
    return tasks.map(task => this.addTask(task));
  }

  /**
   * Start processing batch
   *
   * @returns {Promise<object>} Batch summary
   */
  async process() {
    if (this.isProcessing) {
      throw new Error('Batch processor is already running');
    }

    this.isProcessing = true;
    this.processStartTime = Date.now();

    this.emit('batch:start', { totalTasks: this.tasks.size });

    try {
      await this._processBatch();

      const summary = this._generateSummary();
      this.emit('batch:complete', summary);

      return summary;
    } catch (error) {
      this.emit('batch:error', { error: error.message });
      throw error;
    } finally {
      this.isProcessing = false;
    }
  }

  /**
   * Cancel specific task
   *
   * @param {string} taskId - Task ID to cancel
   */
  cancelTask(taskId) {
    const task = this.tasks.get(taskId);
    if (!task) {
      throw new Error(`Task ${taskId} not found`);
    }

    if (task.status === TaskStatus.COMPLETED || task.status === TaskStatus.FAILED) {
      throw new Error(`Cannot cancel ${task.status} task`);
    }

    task.status = TaskStatus.CANCELLED;
    this.cancelled.add(taskId);
    this._removeFromQueues(taskId);

    this.emit('task:cancelled', { taskId });
  }

  /**
   * Cancel all pending tasks
   */
  cancelAll() {
    for (const [taskId, task] of this.tasks.entries()) {
      if (task.status === TaskStatus.PENDING || task.status === TaskStatus.QUEUED) {
        try {
          this.cancelTask(taskId);
        } catch {
          // Ignore errors
        }
      }
    }
  }

  /**
   * Get current progress
   *
   * @returns {object} Progress report
   */
  getProgress() {
    const total = this.tasks.size;
    const pending = [...this.tasks.values()].filter(t => t.status === TaskStatus.PENDING).length;
    const queued = [...this.tasks.values()].filter(t => t.status === TaskStatus.QUEUED).length;
    const running = this.running.size;
    const completed = this.completed.size;
    const failed = this.failed.size;
    const cancelled = this.cancelled.size;

    const percentComplete = total > 0 ? (completed / total) * 100 : 0;

    let estimatedTimeRemaining = null;
    if (this.processStartTime && completed > 0) {
      const elapsed = Date.now() - this.processStartTime;
      const avgTimePerTask = elapsed / completed;
      const remaining = total - completed - failed - cancelled;
      estimatedTimeRemaining = avgTimePerTask * remaining;
    }

    return ProgressReportSchema.parse({
      total,
      pending,
      queued,
      running,
      completed,
      failed,
      cancelled,
      percentComplete,
      estimatedTimeRemaining,
    });
  }

  /**
   * Get task result
   *
   * @param {string} taskId - Task ID
   * @returns {object|null} Task result
   */
  getResult(taskId) {
    return this.results.get(taskId) || null;
  }

  /**
   * Get all results
   *
   * @returns {Map<string, object>} All results
   */
  getAllResults() {
    return new Map(this.results);
  }

  /**
   * Clear all tasks and results
   */
  clear() {
    this.tasks.clear();
    this.results.clear();
    for (const queue of Object.values(this.queues)) {
      queue.length = 0;
    }
    this.running.clear();
    this.completed.clear();
    this.failed.clear();
    this.cancelled.clear();
  }

  /**
   * Process batch queue
   *
   * @private
   */
  async _processBatch() {
    const workers = [];

    for (let i = 0; i < this.config.concurrency; i++) {
      workers.push(this._worker());
    }

    await Promise.all(workers);
  }

  /**
   * Worker process
   *
   * @private
   */
  async _worker() {
    while (true) {
      const taskId = this._getNextTask();
      if (!taskId) break;

      const task = this.tasks.get(taskId);
      if (!task) continue;

      // Check dependencies
      if (!this._dependenciesSatisfied(task)) {
        this._requeueTask(taskId);
        await this._sleep(100);
        continue;
      }

      await this._executeTask(taskId);
    }
  }

  /**
   * Execute single task
   *
   * @private
   */
  async _executeTask(taskId) {
    const task = this.tasks.get(taskId);
    if (!task) return;

    const startTime = Date.now();
    task.status = TaskStatus.RUNNING;
    this.running.add(taskId);

    this.emit('task:start', { taskId, task });

    try {
      const result = await this.runner.execute({
        prompt: task.prompt,
        timeout: task.timeout,
        ...task.options,
      });

      const duration = Date.now() - startTime;

      const taskResult = BatchResultSchema.parse({
        taskId,
        status: TaskStatus.COMPLETED,
        result: result.output,
        attempts: task.retries + 1,
        duration,
        startedAt: startTime,
        completedAt: Date.now(),
      });

      this.results.set(taskId, taskResult);
      task.status = TaskStatus.COMPLETED;
      this.running.delete(taskId);
      this.completed.add(taskId);

      this.emit('task:complete', { taskId, result: taskResult });
      this.emit('progress', this.getProgress());

    } catch (error) {
      const duration = Date.now() - startTime;

      if (task.retries < task.maxRetries) {
        // Retry
        task.retries++;
        task.status = TaskStatus.RETRYING;
        this.running.delete(taskId);

        this.emit('task:retry', { taskId, attempt: task.retries, error: error.message });

        await this._sleep(this.config.retryDelay);
        this._queueTask(taskId);

      } else {
        // Failed
        const taskResult = BatchResultSchema.parse({
          taskId,
          status: TaskStatus.FAILED,
          error: error.message,
          attempts: task.retries + 1,
          duration,
          startedAt: startTime,
          completedAt: Date.now(),
        });

        this.results.set(taskId, taskResult);
        task.status = TaskStatus.FAILED;
        this.running.delete(taskId);
        this.failed.add(taskId);

        this.emit('task:failed', { taskId, error: error.message });
        this.emit('progress', this.getProgress());
      }
    }
  }

  /**
   * Queue task by priority
   *
   * @private
   */
  _queueTask(taskId) {
    const task = this.tasks.get(taskId);
    if (!task) return;

    task.status = TaskStatus.QUEUED;
    this.queues[task.priority].push(taskId);
  }

  /**
   * Requeue task (for dependency waiting)
   *
   * @private
   */
  _requeueTask(taskId) {
    const task = this.tasks.get(taskId);
    if (!task) return;

    // Put at end of queue
    this._removeFromQueues(taskId);
    this.queues[task.priority].push(taskId);
  }

  /**
   * Get next task from highest priority queue
   *
   * @private
   */
  _getNextTask() {
    const priorities = [
      TaskPriority.CRITICAL,
      TaskPriority.HIGH,
      TaskPriority.NORMAL,
      TaskPriority.LOW,
    ];

    for (const priority of priorities) {
      const queue = this.queues[priority];
      if (queue.length > 0) {
        return queue.shift();
      }
    }

    return null;
  }

  /**
   * Remove task from all queues
   *
   * @private
   */
  _removeFromQueues(taskId) {
    for (const queue of Object.values(this.queues)) {
      const index = queue.indexOf(taskId);
      if (index !== -1) {
        queue.splice(index, 1);
      }
    }
  }

  /**
   * Check if task dependencies are satisfied
   *
   * @private
   */
  _dependenciesSatisfied(task) {
    if (!task.dependencies || task.dependencies.length === 0) {
      return true;
    }

    return task.dependencies.every(depId => this.completed.has(depId));
  }

  /**
   * Generate task ID from prompt
   *
   * @private
   */
  _generateTaskId(prompt) {
    return createHash('sha256')
      .update(prompt + Date.now())
      .digest('hex')
      .slice(0, 16);
  }

  /**
   * Generate batch summary
   *
   * @private
   */
  _generateSummary() {
    const duration = Date.now() - this.processStartTime;

    return {
      totalTasks: this.tasks.size,
      completed: this.completed.size,
      failed: this.failed.size,
      cancelled: this.cancelled.size,
      duration,
      successRate: this.tasks.size > 0 ? (this.completed.size / this.tasks.size) * 100 : 0,
      results: this.getAllResults(),
    };
  }

  /**
   * Sleep helper
   *
   * @private
   */
  _sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

/**
 * Create batch processor instance
 *
 * @param {object} config - Processor configuration
 * @returns {BatchProcessor} Processor instance
 */
export function createBatchProcessor(config = {}) {
  return new BatchProcessor(config);
}

/**
 * Quick batch execution helper
 *
 * @param {Array<string>} prompts - Array of prompts
 * @param {object} options - Batch options
 * @returns {Promise<object>} Batch summary
 */
export async function executeBatch(prompts, options = {}) {
  const processor = createBatchProcessor(options);

  const tasks = prompts.map((prompt, index) => ({
    id: `task-${index}`,
    prompt,
    priority: options.priority || TaskPriority.NORMAL,
  }));

  processor.addTasks(tasks);
  return processor.process();
}
