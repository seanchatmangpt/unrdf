/**
 * @fileoverview Worker Pool - Manages concurrent agent execution
 *
 * **Purpose**: Provide efficient worker pool for parallel agent execution:
 * 1. Worker lifecycle management (spawn, monitor, retire)
 * 2. Load balancing across available workers
 * 3. Health monitoring and auto-recovery
 * 4. Backpressure handling
 *
 * **Properties**:
 * - Fixed or dynamic pool sizing
 * - Worker health checks
 * - Graceful shutdown
 * - Task assignment strategies
 *
 * @module orchestration/worker-pool
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Worker status enum
 */
export const WorkerStatus = {
  IDLE: 'idle',
  BUSY: 'busy',
  FAILED: 'failed',
  RETIRED: 'retired'
};

/**
 * Worker schema
 */
export const WorkerSchema = z.object({
  id: z.string(),
  status: z.enum(['idle', 'busy', 'failed', 'retired']),
  capabilities: z.array(z.string()).default([]),
  currentTask: z.string().optional(),
  tasksCompleted: z.number().default(0),
  tasksFailed: z.number().default(0),
  createdAt: z.number(),
  lastActiveAt: z.number().optional(),
  metadata: z.record(z.any()).optional()
});

/**
 * Pool configuration schema
 */
export const PoolConfigSchema = z.object({
  minWorkers: z.number().min(1).default(2),
  maxWorkers: z.number().min(1).default(10),
  idleTimeout: z.number().default(30000),
  healthCheckInterval: z.number().default(10000),
  maxTasksPerWorker: z.number().default(100),
  workerFactory: z.function().optional(),
  assignmentStrategy: z.enum(['round-robin', 'least-busy', 'random']).default('least-busy')
});

/**
 * Worker Pool - Manages concurrent agent execution
 *
 * @class WorkerPool
 * @extends EventEmitter
 *
 * @example
 * const pool = new WorkerPool({
 *   minWorkers: 2,
 *   maxWorkers: 10,
 *   assignmentStrategy: 'least-busy'
 * });
 *
 * await pool.start();
 *
 * const worker = await pool.acquireWorker(['agent-alpha']);
 * await pool.releaseWorker(worker.id);
 *
 * await pool.shutdown();
 */
export class WorkerPool extends EventEmitter {
  /**
   * Create a new worker pool
   *
   * @param {Object} config - Pool configuration
   */
  constructor(config = {}) {
    super();

    /** @type {Object} Pool configuration */
    this.config = PoolConfigSchema.parse(config);

    /** @type {Map<string, Object>} Active workers */
    this.workers = new Map();

    /** @type {number} Next assignment index for round-robin */
    this.nextAssignment = 0;

    /** @type {boolean} Pool running state */
    this.running = false;

    /** @type {NodeJS.Timeout|null} Health check interval */
    this.healthCheckTimer = null;

    /** @type {NodeJS.Timeout|null} Idle cleanup timer */
    this.idleCleanupTimer = null;

    /** @type {number} Worker ID counter */
    this.workerIdCounter = 0;

    /** @type {Object} Pool statistics */
    this.stats = {
      totalTasksAssigned: 0,
      totalTasksCompleted: 0,
      totalTasksFailed: 0,
      workersCreated: 0,
      workersRetired: 0,
      peakWorkers: 0
    };
  }

  /**
   * Start the worker pool
   *
   * @returns {Promise<void>}
   */
  async start() {
    if (this.running) {
      return;
    }

    this.running = true;

    // Create minimum workers
    for (let i = 0; i < this.config.minWorkers; i++) {
      await this._createWorker();
    }

    // Start health checks
    this._startHealthChecks();

    // Start idle cleanup
    this._startIdleCleanup();

    this.emit('pool:started', {
      minWorkers: this.config.minWorkers,
      maxWorkers: this.config.maxWorkers
    });
  }

  /**
   * Shutdown the worker pool
   *
   * @param {Object} [options] - Shutdown options
   * @param {boolean} [options.graceful=true] - Wait for tasks to complete
   * @param {number} [options.timeout=30000] - Shutdown timeout
   * @returns {Promise<void>}
   */
  async shutdown(options = {}) {
    const { graceful = true, timeout = 30000 } = options;

    this.running = false;

    // Stop timers
    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
      this.healthCheckTimer = null;
    }

    if (this.idleCleanupTimer) {
      clearInterval(this.idleCleanupTimer);
      this.idleCleanupTimer = null;
    }

    if (graceful) {
      // Wait for workers to become idle
      const deadline = Date.now() + timeout;
      while (this._getBusyWorkers().length > 0 && Date.now() < deadline) {
        await this._sleep(100);
      }
    }

    // Retire all workers
    for (const worker of this.workers.values()) {
      await this._retireWorker(worker.id);
    }

    this.emit('pool:shutdown', {
      stats: this.getStats()
    });
  }

  /**
   * Acquire a worker for task execution
   *
   * @param {string[]} [requiredCapabilities=[]] - Required capabilities
   * @param {number} [timeout=5000] - Acquisition timeout
   * @returns {Promise<Object>} Worker
   */
  async acquireWorker(requiredCapabilities = [], timeout = 5000) {
    const deadline = Date.now() + timeout;

    while (Date.now() < deadline) {
      // Find available worker
      const worker = this._findAvailableWorker(requiredCapabilities);

      if (worker) {
        worker.status = WorkerStatus.BUSY;
        worker.lastActiveAt = Date.now();
        this.stats.totalTasksAssigned++;
        this.emit('worker:acquired', { workerId: worker.id });
        return worker;
      }

      // Try to scale up if possible
      if (this.workers.size < this.config.maxWorkers && this.running) {
        const newWorker = await this._createWorker(requiredCapabilities);
        if (newWorker) {
          newWorker.status = WorkerStatus.BUSY;
          newWorker.lastActiveAt = Date.now();
          this.stats.totalTasksAssigned++;
          this.emit('worker:acquired', { workerId: newWorker.id });
          return newWorker;
        }
      }

      // Wait before retry
      await this._sleep(50);
    }

    throw new Error(`Failed to acquire worker within ${timeout}ms`);
  }

  /**
   * Release a worker back to the pool
   *
   * @param {string} workerId - Worker ID
   * @param {Object} [result] - Task result
   * @returns {void}
   */
  releaseWorker(workerId, result = {}) {
    const worker = this.workers.get(workerId);
    if (!worker) {
      throw new Error(`Worker not found: ${workerId}`);
    }

    worker.status = WorkerStatus.IDLE;
    worker.currentTask = undefined;
    worker.lastActiveAt = Date.now();

    if (result.success) {
      worker.tasksCompleted++;
      this.stats.totalTasksCompleted++;
    } else {
      worker.tasksFailed++;
      this.stats.totalTasksFailed++;
    }

    // Retire worker if reached max tasks
    if (worker.tasksCompleted >= this.config.maxTasksPerWorker) {
      this._retireWorker(workerId);
    }

    this.emit('worker:released', {
      workerId,
      tasksCompleted: worker.tasksCompleted,
      tasksFailed: worker.tasksFailed
    });
  }

  /**
   * Mark worker as failed
   *
   * @param {string} workerId - Worker ID
   * @param {Error} error - Failure error
   * @returns {void}
   */
  failWorker(workerId, error) {
    const worker = this.workers.get(workerId);
    if (!worker) {
      return;
    }

    worker.status = WorkerStatus.FAILED;
    worker.tasksFailed++;
    this.stats.totalTasksFailed++;

    this.emit('worker:failed', {
      workerId,
      error: error.message
    });

    // Retire failed worker
    this._retireWorker(workerId);
  }

  /**
   * Get pool statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    const workers = Array.from(this.workers.values());

    return {
      ...this.stats,
      currentWorkers: this.workers.size,
      idleWorkers: workers.filter(w => w.status === WorkerStatus.IDLE).length,
      busyWorkers: workers.filter(w => w.status === WorkerStatus.BUSY).length,
      failedWorkers: workers.filter(w => w.status === WorkerStatus.FAILED).length,
      averageTasksPerWorker: workers.length > 0
        ? Math.round(workers.reduce((sum, w) => sum + w.tasksCompleted, 0) / workers.length)
        : 0,
      successRate: this.stats.totalTasksAssigned > 0
        ? ((this.stats.totalTasksCompleted / this.stats.totalTasksAssigned) * 100).toFixed(2) + '%'
        : 'N/A'
    };
  }

  /**
   * Get all workers
   *
   * @returns {Object[]} Workers
   */
  getWorkers() {
    return Array.from(this.workers.values());
  }

  /**
   * Create a new worker
   *
   * @param {string[]} [capabilities=[]] - Worker capabilities
   * @returns {Promise<Object>} Worker
   * @private
   */
  async _createWorker(capabilities = []) {
    const workerId = `worker-${this.workerIdCounter++}`;

    const worker = {
      id: workerId,
      status: WorkerStatus.IDLE,
      capabilities: capabilities.length > 0 ? capabilities : ['*'],
      tasksCompleted: 0,
      tasksFailed: 0,
      createdAt: Date.now(),
      metadata: {}
    };

    // Call custom factory if provided
    if (this.config.workerFactory) {
      const customWorker = await this.config.workerFactory(worker);
      Object.assign(worker, customWorker);
    }

    this.workers.set(workerId, worker);
    this.stats.workersCreated++;

    if (this.workers.size > this.stats.peakWorkers) {
      this.stats.peakWorkers = this.workers.size;
    }

    this.emit('worker:created', { workerId, capabilities });

    return worker;
  }

  /**
   * Retire a worker
   *
   * @param {string} workerId - Worker ID
   * @returns {Promise<void>}
   * @private
   */
  async _retireWorker(workerId) {
    const worker = this.workers.get(workerId);
    if (!worker) {
      return;
    }

    worker.status = WorkerStatus.RETIRED;
    this.workers.delete(workerId);
    this.stats.workersRetired++;

    this.emit('worker:retired', {
      workerId,
      tasksCompleted: worker.tasksCompleted,
      tasksFailed: worker.tasksFailed,
      lifetime: Date.now() - worker.createdAt
    });
  }

  /**
   * Find an available worker matching capabilities
   *
   * @param {string[]} requiredCapabilities - Required capabilities
   * @returns {Object|null} Worker or null
   * @private
   */
  _findAvailableWorker(requiredCapabilities = []) {
    const idleWorkers = Array.from(this.workers.values())
      .filter(w => w.status === WorkerStatus.IDLE)
      .filter(w => this._hasCapabilities(w, requiredCapabilities));

    if (idleWorkers.length === 0) {
      return null;
    }

    switch (this.config.assignmentStrategy) {
      case 'round-robin':
        this.nextAssignment = (this.nextAssignment + 1) % idleWorkers.length;
        return idleWorkers[this.nextAssignment];

      case 'least-busy':
        return idleWorkers.sort((a, b) => a.tasksCompleted - b.tasksCompleted)[0];

      case 'random':
        return idleWorkers[Math.floor(Math.random() * idleWorkers.length)];

      default:
        return idleWorkers[0];
    }
  }

  /**
   * Check if worker has required capabilities
   *
   * @param {Object} worker - Worker
   * @param {string[]} required - Required capabilities
   * @returns {boolean} Has capabilities
   * @private
   */
  _hasCapabilities(worker, required) {
    if (required.length === 0 || worker.capabilities.includes('*')) {
      return true;
    }

    return required.every(cap =>
      worker.capabilities.includes(cap) || worker.capabilities.includes('*')
    );
  }

  /**
   * Get busy workers
   *
   * @returns {Object[]} Busy workers
   * @private
   */
  _getBusyWorkers() {
    return Array.from(this.workers.values())
      .filter(w => w.status === WorkerStatus.BUSY);
  }

  /**
   * Start health checks
   *
   * @private
   */
  _startHealthChecks() {
    this.healthCheckTimer = setInterval(() => {
      this._performHealthCheck();
    }, this.config.healthCheckInterval);
  }

  /**
   * Perform health check on all workers
   *
   * @private
   */
  _performHealthCheck() {
    const now = Date.now();

    for (const worker of this.workers.values()) {
      // Check if worker is stuck (busy for too long)
      if (worker.status === WorkerStatus.BUSY && worker.lastActiveAt) {
        const busyDuration = now - worker.lastActiveAt;
        if (busyDuration > 300000) { // 5 minutes
          this.emit('worker:stuck', {
            workerId: worker.id,
            duration: busyDuration
          });
          this.failWorker(worker.id, new Error('Worker stuck'));
        }
      }
    }
  }

  /**
   * Start idle cleanup
   *
   * @private
   */
  _startIdleCleanup() {
    this.idleCleanupTimer = setInterval(() => {
      this._cleanupIdleWorkers();
    }, this.config.idleTimeout);
  }

  /**
   * Cleanup idle workers beyond minimum
   *
   * @private
   */
  _cleanupIdleWorkers() {
    if (this.workers.size <= this.config.minWorkers) {
      return;
    }

    const now = Date.now();
    const idleWorkers = Array.from(this.workers.values())
      .filter(w => w.status === WorkerStatus.IDLE)
      .filter(w => w.lastActiveAt && (now - w.lastActiveAt) > this.config.idleTimeout)
      .sort((a, b) => a.lastActiveAt - b.lastActiveAt);

    // Retire excess idle workers
    const excessCount = this.workers.size - this.config.minWorkers;
    for (let i = 0; i < Math.min(excessCount, idleWorkers.length); i++) {
      this._retireWorker(idleWorkers[i].id);
    }
  }

  /**
   * Sleep helper
   *
   * @param {number} ms - Milliseconds
   * @returns {Promise<void>}
   * @private
   */
  _sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

/**
 * Create a worker pool
 *
 * @param {Object} [config] - Pool configuration
 * @returns {WorkerPool}
 */
export function createWorkerPool(config = {}) {
  return new WorkerPool(config);
}
