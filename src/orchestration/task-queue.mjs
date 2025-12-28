/**
 * @fileoverview Task Queue - Priority-based task scheduling
 *
 * **Purpose**: Provide efficient task queue with priority scheduling:
 * 1. Priority-based task ordering
 * 2. Task lifecycle management
 * 3. Backpressure handling
 * 4. Task timeout and cancellation
 *
 * **Properties**:
 * - Multiple priority levels
 * - FIFO within same priority
 * - Bounded queue size
 * - Task metrics and monitoring
 *
 * @module orchestration/task-queue
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Task priority enum
 */
export const TaskPriority = {
  CRITICAL: 0,
  HIGH: 1,
  NORMAL: 2,
  LOW: 3,
  BACKGROUND: 4
};

/**
 * Task status enum
 */
export const TaskStatus = {
  QUEUED: 'queued',
  RUNNING: 'running',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
  TIMEOUT: 'timeout'
};

/**
 * Task schema
 */
export const TaskSchema = z.object({
  id: z.string(),
  type: z.string(),
  priority: z.number().min(0).max(4).default(TaskPriority.NORMAL),
  capabilities: z.array(z.string()).default([]),
  payload: z.any(),
  timeout: z.number().default(60000),
  retries: z.number().default(0),
  maxRetries: z.number().default(3),
  status: z.enum(['queued', 'running', 'completed', 'failed', 'cancelled', 'timeout']).default('queued'),
  createdAt: z.number(),
  startedAt: z.number().optional(),
  completedAt: z.number().optional(),
  result: z.any().optional(),
  error: z.string().optional(),
  metadata: z.record(z.any()).optional()
});

/**
 * Queue configuration schema
 */
export const QueueConfigSchema = z.object({
  maxSize: z.number().default(1000),
  defaultPriority: z.number().default(TaskPriority.NORMAL),
  defaultTimeout: z.number().default(60000),
  maxRetries: z.number().default(3),
  enableMetrics: z.boolean().default(true)
});

/**
 * Task Queue - Priority-based task scheduling
 *
 * @class TaskQueue
 * @extends EventEmitter
 *
 * @example
 * const queue = new TaskQueue({
 *   maxSize: 1000,
 *   defaultPriority: TaskPriority.NORMAL
 * });
 *
 * const taskId = queue.enqueue({
 *   type: 'agent-task',
 *   priority: TaskPriority.HIGH,
 *   payload: { data: 'example' }
 * });
 *
 * const task = queue.dequeue();
 * queue.complete(taskId, { result: 'success' });
 */
export class TaskQueue extends EventEmitter {
  /**
   * Create a new task queue
   *
   * @param {Object} config - Queue configuration
   */
  constructor(config = {}) {
    super();

    /** @type {Object} Queue configuration */
    this.config = QueueConfigSchema.parse(config);

    /** @type {Map<number, Array>} Tasks by priority */
    this.queues = new Map([
      [TaskPriority.CRITICAL, []],
      [TaskPriority.HIGH, []],
      [TaskPriority.NORMAL, []],
      [TaskPriority.LOW, []],
      [TaskPriority.BACKGROUND, []]
    ]);

    /** @type {Map<string, Object>} All tasks (queued + running) */
    this.tasks = new Map();

    /** @type {Map<string, Object>} Running tasks */
    this.running = new Map();

    /** @type {Map<string, NodeJS.Timeout>} Task timeout timers */
    this.timers = new Map();

    /** @type {number} Task ID counter */
    this.taskIdCounter = 0;

    /** @type {Object} Queue statistics */
    this.stats = {
      totalEnqueued: 0,
      totalDequeued: 0,
      totalCompleted: 0,
      totalFailed: 0,
      totalCancelled: 0,
      totalTimeout: 0,
      totalRetried: 0
    };
  }

  /**
   * Enqueue a task
   *
   * @param {Object} taskConfig - Task configuration
   * @returns {string} Task ID
   */
  enqueue(taskConfig) {
    // Check queue size
    if (this.size() >= this.config.maxSize) {
      throw new Error(`Queue full: ${this.config.maxSize} tasks`);
    }

    // Create task
    const taskId = `task-${this.taskIdCounter++}`;
    const task = TaskSchema.parse({
      id: taskId,
      type: taskConfig.type,
      priority: taskConfig.priority ?? this.config.defaultPriority,
      capabilities: taskConfig.capabilities || [],
      payload: taskConfig.payload,
      timeout: taskConfig.timeout ?? this.config.defaultTimeout,
      maxRetries: taskConfig.maxRetries ?? this.config.maxRetries,
      retries: 0,
      status: TaskStatus.QUEUED,
      createdAt: Date.now(),
      metadata: taskConfig.metadata || {}
    });

    // Add to queue
    const priorityQueue = this.queues.get(task.priority);
    priorityQueue.push(task);
    this.tasks.set(taskId, task);

    this.stats.totalEnqueued++;

    this.emit('task:enqueued', {
      taskId,
      priority: task.priority,
      queueSize: this.size()
    });

    return taskId;
  }

  /**
   * Dequeue highest priority task
   *
   * @param {string[]} [requiredCapabilities=[]] - Required capabilities
   * @returns {Object|null} Task or null
   */
  dequeue(requiredCapabilities = []) {
    // Check each priority level (highest first)
    for (const priority of [
      TaskPriority.CRITICAL,
      TaskPriority.HIGH,
      TaskPriority.NORMAL,
      TaskPriority.LOW,
      TaskPriority.BACKGROUND
    ]) {
      const queue = this.queues.get(priority);

      // Find first task matching capabilities
      const index = queue.findIndex(task =>
        this._matchesCapabilities(task, requiredCapabilities)
      );

      if (index !== -1) {
        const [task] = queue.splice(index, 1);
        task.status = TaskStatus.RUNNING;
        task.startedAt = Date.now();

        this.running.set(task.id, task);
        this.stats.totalDequeued++;

        // Start timeout timer
        this._startTimeout(task);

        this.emit('task:dequeued', {
          taskId: task.id,
          priority: task.priority,
          queueSize: this.size()
        });

        return task;
      }
    }

    return null;
  }

  /**
   * Complete a task
   *
   * @param {string} taskId - Task ID
   * @param {*} result - Task result
   * @returns {void}
   */
  complete(taskId, result) {
    const task = this.running.get(taskId);
    if (!task) {
      throw new Error(`Task not running: ${taskId}`);
    }

    task.status = TaskStatus.COMPLETED;
    task.result = result;
    task.completedAt = Date.now();

    this.running.delete(taskId);
    this._clearTimeout(taskId);

    this.stats.totalCompleted++;

    this.emit('task:completed', {
      taskId,
      duration: task.completedAt - task.startedAt,
      retries: task.retries
    });

    // Cleanup after event
    setTimeout(() => this.tasks.delete(taskId), 1000);
  }

  /**
   * Fail a task
   *
   * @param {string} taskId - Task ID
   * @param {Error} error - Failure error
   * @param {boolean} [retry=true] - Retry if possible
   * @returns {boolean} True if retried
   */
  fail(taskId, error, retry = true) {
    const task = this.running.get(taskId);
    if (!task) {
      throw new Error(`Task not running: ${taskId}`);
    }

    this._clearTimeout(taskId);

    // Check if can retry
    if (retry && task.retries < task.maxRetries) {
      task.retries++;
      task.status = TaskStatus.QUEUED;
      task.startedAt = undefined;

      this.running.delete(taskId);

      // Re-enqueue with same priority
      const queue = this.queues.get(task.priority);
      queue.push(task);

      this.stats.totalRetried++;

      this.emit('task:retried', {
        taskId,
        retries: task.retries,
        maxRetries: task.maxRetries,
        error: error.message
      });

      return true;
    }

    // Mark as failed
    task.status = TaskStatus.FAILED;
    task.error = error.message;
    task.completedAt = Date.now();

    this.running.delete(taskId);
    this.stats.totalFailed++;

    this.emit('task:failed', {
      taskId,
      error: error.message,
      retries: task.retries
    });

    // Cleanup after event
    setTimeout(() => this.tasks.delete(taskId), 1000);

    return false;
  }

  /**
   * Cancel a task
   *
   * @param {string} taskId - Task ID
   * @returns {boolean} True if cancelled
   */
  cancel(taskId) {
    const task = this.tasks.get(taskId);
    if (!task) {
      return false;
    }

    // Remove from queue if queued
    if (task.status === TaskStatus.QUEUED) {
      const queue = this.queues.get(task.priority);
      const index = queue.findIndex(t => t.id === taskId);
      if (index !== -1) {
        queue.splice(index, 1);
      }
    }

    // Remove from running if running
    if (task.status === TaskStatus.RUNNING) {
      this.running.delete(taskId);
      this._clearTimeout(taskId);
    }

    task.status = TaskStatus.CANCELLED;
    task.completedAt = Date.now();

    this.stats.totalCancelled++;

    this.emit('task:cancelled', { taskId });

    // Cleanup after event
    setTimeout(() => this.tasks.delete(taskId), 1000);

    return true;
  }

  /**
   * Get task by ID
   *
   * @param {string} taskId - Task ID
   * @returns {Object|undefined} Task
   */
  getTask(taskId) {
    return this.tasks.get(taskId);
  }

  /**
   * Get queue size
   *
   * @returns {number} Total queued tasks
   */
  size() {
    return Array.from(this.queues.values())
      .reduce((sum, queue) => sum + queue.length, 0);
  }

  /**
   * Get running tasks count
   *
   * @returns {number} Running tasks
   */
  runningCount() {
    return this.running.size;
  }

  /**
   * Check if queue is empty
   *
   * @returns {boolean} Is empty
   */
  isEmpty() {
    return this.size() === 0;
  }

  /**
   * Check if queue is full
   *
   * @returns {boolean} Is full
   */
  isFull() {
    return this.size() >= this.config.maxSize;
  }

  /**
   * Get queue statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    const queuedByPriority = {};
    for (const [priority, queue] of this.queues.entries()) {
      const priorityName = Object.keys(TaskPriority).find(
        k => TaskPriority[k] === priority
      );
      queuedByPriority[priorityName] = queue.length;
    }

    return {
      ...this.stats,
      currentQueued: this.size(),
      currentRunning: this.running.size,
      queuedByPriority,
      successRate: this.stats.totalDequeued > 0
        ? ((this.stats.totalCompleted / this.stats.totalDequeued) * 100).toFixed(2) + '%'
        : 'N/A'
    };
  }

  /**
   * Clear all tasks
   *
   * @returns {void}
   */
  clear() {
    // Clear all queues
    for (const queue of this.queues.values()) {
      queue.length = 0;
    }

    // Clear timers
    for (const timer of this.timers.values()) {
      clearTimeout(timer);
    }
    this.timers.clear();

    // Clear maps
    this.tasks.clear();
    this.running.clear();

    this.emit('queue:cleared');
  }

  /**
   * Check if task matches required capabilities
   *
   * @param {Object} task - Task
   * @param {string[]} required - Required capabilities
   * @returns {boolean} Matches
   * @private
   */
  _matchesCapabilities(task, required) {
    if (required.length === 0 || task.capabilities.includes('*')) {
      return true;
    }

    return task.capabilities.some(cap =>
      required.includes(cap) || cap === '*'
    );
  }

  /**
   * Start timeout timer for task
   *
   * @param {Object} task - Task
   * @private
   */
  _startTimeout(task) {
    if (task.timeout <= 0) {
      return;
    }

    const timer = setTimeout(() => {
      this._handleTimeout(task.id);
    }, task.timeout);

    this.timers.set(task.id, timer);
  }

  /**
   * Clear timeout timer
   *
   * @param {string} taskId - Task ID
   * @private
   */
  _clearTimeout(taskId) {
    const timer = this.timers.get(taskId);
    if (timer) {
      clearTimeout(timer);
      this.timers.delete(taskId);
    }
  }

  /**
   * Handle task timeout
   *
   * @param {string} taskId - Task ID
   * @private
   */
  _handleTimeout(taskId) {
    const task = this.running.get(taskId);
    if (!task) {
      return;
    }

    task.status = TaskStatus.TIMEOUT;
    task.completedAt = Date.now();

    this.running.delete(taskId);
    this.stats.totalTimeout++;

    this.emit('task:timeout', {
      taskId,
      timeout: task.timeout,
      duration: task.completedAt - task.startedAt
    });

    // Cleanup after event
    setTimeout(() => this.tasks.delete(taskId), 1000);
  }
}

/**
 * Create a task queue
 *
 * @param {Object} [config] - Queue configuration
 * @returns {TaskQueue}
 */
export function createTaskQueue(config = {}) {
  return new TaskQueue(config);
}
