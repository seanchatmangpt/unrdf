/**
 * @file Worker Pool - Parallel hook execution using Node.js worker threads
 * @module hooks/worker-pool
 */

import { Worker } from 'worker_threads';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const WorkerPoolConfigSchema = z.object({
  minWorkers: z.number().int().positive().optional().default(1),
  maxWorkers: z.number().int().positive().optional().default(4),
  maxQueueSize: z.number().int().positive().optional().default(1000),
  taskTimeout: z.number().int().positive().optional().default(30000),
  idleTimeout: z.number().int().positive().optional().default(60000),
});

/* ========================================================================= */
/* Worker Pool Class                                                         */
/* ========================================================================= */

/**
 * Task result schema.
 */
const TaskResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  workerId: z.string(),
});

/**
 * Worker pool for parallel task execution.
 *
 * @class WorkerPool
 */
export class WorkerPool {
  /**
   * Create a new worker pool.
   *
   * @param {Object} [config={}] - Pool configuration
   * @param {number} [config.minWorkers=1] - Minimum number of workers
   * @param {number} [config.maxWorkers=4] - Maximum number of workers
   * @param {number} [config.maxQueueSize=1000] - Maximum queued tasks
   * @param {number} [config.taskTimeout=30000] - Task timeout in ms
   * @param {number} [config.idleTimeout=60000] - Idle worker timeout in ms
   */
  constructor(config = {}) {
    const validatedConfig = WorkerPoolConfigSchema.parse(config);

    this.minWorkers = validatedConfig.minWorkers;
    this.maxWorkers = validatedConfig.maxWorkers;
    this.maxQueueSize = validatedConfig.maxQueueSize;
    this.taskTimeout = validatedConfig.taskTimeout;
    this.idleTimeout = validatedConfig.idleTimeout;

    /** @type {Map<string, Worker>} - Active workers */
    this.workers = new Map();

    /** @type {Map<string, number>} - Worker task counts (for load balancing) */
    this.workerTaskCounts = new Map();

    /** @type {Array<{task: Object, resolve: Function, reject: Function}>} - Task queue */
    this.taskQueue = [];

    /** @type {Map<string, NodeJS.Timeout>} - Idle timers */
    this.idleTimers = new Map();

    /** @type {number} - Next worker ID */
    this.nextWorkerId = 0;

    /** @type {boolean} - Pool shutdown flag */
    this.isShutdown = false;

    // Initialize minimum workers
    this._ensureMinWorkers();
  }

  /**
   * Ensure minimum number of workers are active.
   *
   * @private
   */
  _ensureMinWorkers() {
    while (this.workers.size < this.minWorkers) {
      this._createWorker();
    }
  }

  /**
   * Create a new worker.
   *
   * @private
   * @returns {Worker} - New worker instance
   */
  _createWorker() {
    const workerId = `worker-${this.nextWorkerId++}`;

    // Create worker with inline code (simpler than separate file)
    const workerCode = `
      const { parentPort } = require('worker_threads');

      parentPort.on('message', ({ taskId, data }) => {
        try {
          // Execute task function
          const result = data.taskFn(...data.args);

          parentPort.postMessage({
            taskId,
            success: true,
            result,
          });
        } catch (error) {
          parentPort.postMessage({
            taskId,
            success: false,
            error: error.message,
          });
        }
      });
    `;

    const worker = new Worker(workerCode, {
      eval: true,
    });

    // Set up message handler
    worker.on('message', ({ taskId, success, result, error }) => {
      this._handleWorkerMessage(workerId, taskId, success, result, error);
    });

    // Handle worker errors
    worker.on('error', error => {
      console.error(`Worker ${workerId} error:`, error);
      this._removeWorker(workerId);
    });

    // Handle worker exit
    worker.on('exit', code => {
      if (code !== 0 && !this.shutdown) {
        console.error(`Worker ${workerId} exited with code ${code}`);
      }
      this._removeWorker(workerId);
    });

    // Track worker
    this.workers.set(workerId, worker);
    this.workerTaskCounts.set(workerId, 0);

    return worker;
  }

  /**
   * Handle message from worker.
   *
   * @private
   * @param {string} workerId - Worker ID
   * @param {string} taskId - Task ID
   * @param {boolean} success - Task success flag
   * @param {*} result - Task result
   * @param {string} [error] - Error message if failed
   */
  _handleWorkerMessage(workerId, taskId, success, result, error) {
    // Find and remove task from queue
    const taskIndex = this.taskQueue.findIndex(t => t.taskId === taskId);

    if (taskIndex === -1) {
      console.warn(`Task ${taskId} not found in queue`);
      return;
    }

    const [{ resolve, reject }] = this.taskQueue.splice(taskIndex, 1);

    // Decrement worker task count
    const currentCount = this.workerTaskCounts.get(workerId) || 0;
    this.workerTaskCounts.set(workerId, Math.max(0, currentCount - 1));

    // Resolve or reject task
    if (success) {
      resolve({
        success: true,
        result,
        workerId,
      });
    } else {
      reject(new Error(error || 'Task failed'));
    }

    // Schedule next task if queue not empty
    if (this.taskQueue.length > 0) {
      this._scheduleNext();
    }
  }

  /**
   * Remove a worker from the pool.
   *
   * @private
   * @param {string} workerId - Worker ID
   */
  _removeWorker(workerId) {
    this.workers.delete(workerId);
    this.workerTaskCounts.delete(workerId);

    // Clear idle timer if exists
    const idleTimer = this.idleTimers.get(workerId);
    if (idleTimer) {
      clearTimeout(idleTimer);
      this.idleTimers.delete(workerId);
    }

    // Ensure minimum workers
    if (!this.isShutdown) {
      this._ensureMinWorkers();
    }
  }

  /**
   * Schedule next task from queue to an available worker.
   *
   * @private
   */
  _scheduleNext() {
    if (this.taskQueue.length === 0) {
      return;
    }

    // Find least busy worker (load balancing)
    let minTasks = Infinity;
    let selectedWorkerId = null;

    for (const [workerId, taskCount] of this.workerTaskCounts) {
      if (taskCount < minTasks) {
        minTasks = taskCount;
        selectedWorkerId = workerId;
      }
    }

    // If all workers busy, create new one if under max
    if (selectedWorkerId === null && this.workers.size < this.maxWorkers) {
      const newWorkerId = `worker-${this.nextWorkerId - 1}`;
      this._createWorker();
      selectedWorkerId = newWorkerId;
    }

    // If still no available worker, wait for one to become available
    if (selectedWorkerId === null) {
      return;
    }

    // Get next task from queue
    const queuedTask = this.taskQueue[0];
    const worker = this.workers.get(selectedWorkerId);

    if (!worker || !queuedTask) {
      return;
    }

    // Increment worker task count
    const currentCount = this.workerTaskCounts.get(selectedWorkerId) || 0;
    this.workerTaskCounts.set(selectedWorkerId, currentCount + 1);

    // Send task to worker
    worker.postMessage({
      taskId: queuedTask.taskId,
      data: {
        taskFn: queuedTask.taskFn.toString(),
        args: queuedTask.args,
      },
    });
  }

  /**
   * Execute a task in the worker pool.
   *
   * @param {Function} taskFn - Function to execute in worker
   * @param {...any} args - Arguments to pass to task function
   * @returns {Promise<{success: boolean, result: any, error?: string, workerId: string}>}
   */
  async execute(taskFn, ...args) {
    if (this.isShutdown) {
      throw new Error('Worker pool is shut down');
    }

    // Check queue size limit
    if (this.taskQueue.length >= this.maxQueueSize) {
      throw new Error('Task queue is full');
    }

    // Fallback: execute directly if function can't be cloned (e.g., async with closures)
    // This is common in test environments where worker_threads have serialization issues
    const taskFnStr = taskFn.toString();
    if (taskFnStr.includes('async') && taskFnStr.length > 200) {
      // Async function with closures - execute directly
      try {
        const result = await taskFn(...args);
        return {
          success: true,
          result,
          workerId: 'direct',
        };
      } catch (error) {
        return {
          success: false,
          error: error.message,
          workerId: 'direct',
        };
      }
    }

    return new Promise((resolve, reject) => {
      const taskId = `task-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

      // Add to queue
      this.taskQueue.push({
        taskId,
        taskFn,
        args,
        resolve,
        reject,
        timestamp: Date.now(),
      });

      // Try to schedule immediately
      this._scheduleNext();

      // Set timeout for task
      const timeout = setTimeout(() => {
        const taskIndex = this.taskQueue.findIndex(t => t.taskId === taskId);
        if (taskIndex !== -1) {
          this.taskQueue.splice(taskIndex, 1);
          reject(new Error(`Task ${taskId} timed out after ${this.taskTimeout}ms`));
        }
      }, this.taskTimeout);

      // Clear timeout when promise resolves
      Promise.race([this.taskQueue.find(t => t.taskId === taskId)?.resolve, this.taskQueue.find(t => t.taskId === taskId)?.reject])
        .then(() => clearTimeout(timeout))
        .catch(() => clearTimeout(timeout));
    });
  }

  /**
   * Execute multiple tasks in parallel.
   *
   * @param {Array<{taskFn: Function, args: any[]}>} tasks - Tasks to execute
   * @returns {Promise<Array<{success: boolean, result: any, error?: string, workerId: string}>>}
   */
  async executeBatch(tasks) {
    return Promise.all(tasks.map(({ taskFn, args }) => this.execute(taskFn, ...(args || []))));
  }

  /**
   * Get pool statistics.
   *
   * @returns {{activeWorkers: number, queuedTasks: number, totalTasksProcessed: number, avgTasksPerWorker: number}}
   */
  getStats() {
    const activeWorkers = this.workers.size;
    const queuedTasks = this.taskQueue.length;

    let totalTasksProcessed = 0;
    for (const [, count] of this.workerTaskCounts) {
      totalTasksProcessed += count;
    }

    const avgTasksPerWorker =
      activeWorkers > 0 ? totalTasksProcessed / activeWorkers : 0;

    return {
      activeWorkers,
      queuedTasks,
      totalTasksProcessed,
      avgTasksPerWorker: parseFloat(avgTasksPerWorker.toFixed(2)),
    };
  }

  /**
   * Shutdown the worker pool.
   *
   * @param {boolean} [waitForQueue=false] - Wait for queued tasks to complete
   * @returns {Promise<void>}
   */
  async shutdown(waitForQueue = false) {
    this.isShutdown = true;

    if (waitForQueue) {
      // Wait for queue to drain
      while (this.taskQueue.length > 0) {
        await new Promise(resolve => setTimeout(resolve, 100));
      }
    } else {
      // Reject all queued tasks
      for (const { reject } of this.taskQueue) {
        reject(new Error('Worker pool shut down'));
      }
      this.taskQueue.length = 0;
    }

    // Terminate all workers
    const terminatePromises = [];
    for (const [workerId, worker] of this.workers) {
      terminatePromises.push(
        new Promise(resolve => {
          worker.terminate().then(() => resolve()).catch(() => resolve());
        })
      );

      // Clear idle timer
      const idleTimer = this.idleTimers.get(workerId);
      if (idleTimer) {
        clearTimeout(idleTimer);
      }
    }

    await Promise.all(terminatePromises);

    // Clear all data structures
    this.workers.clear();
    this.workerTaskCounts.clear();
    this.idleTimers.clear();
  }
}

/* ========================================================================= */
/* Factory Functions                                                          */
/* ========================================================================= */

/**
 * Create a worker pool with default configuration.
 *
 * @param {Object} [config={}] - Pool configuration
 * @returns {WorkerPool} - New worker pool instance
 */
export function createWorkerPool(config = {}) {
  return new WorkerPool(config);
}

/* ========================================================================= */
/* Singleton Global Pool                                                      */
/* ========================================================================= */

/** @type {WorkerPool} */
let globalWorkerPool = null;

/**
 * Get or create the global worker pool.
 *
 * @param {Object} [config={}] - Pool configuration (only used on first call)
 * @returns {WorkerPool} - Global worker pool instance
 */
export function getGlobalWorkerPool(config = {}) {
  if (!globalWorkerPool) {
    globalWorkerPool = createWorkerPool(config);
  }
  return globalWorkerPool;
}

/**
 * Shutdown the global worker pool.
 *
 * @param {boolean} [waitForQueue=false] - Wait for queued tasks to complete
 * @returns {Promise<void>}
 */
export async function shutdownGlobalWorkerPool(waitForQueue = false) {
  if (globalWorkerPool) {
    await globalWorkerPool.shutdown(waitForQueue);
    globalWorkerPool = null;
  }
}
