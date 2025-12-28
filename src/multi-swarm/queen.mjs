/**
 * @fileoverview Queen Swarm - Meta-orchestrator for hierarchical swarms
 *
 * **Purpose**: Coordinate multiple worker swarms
 * 1. Swarm lifecycle management
 * 2. Work partitioning and distribution
 * 3. Cross-swarm result aggregation
 * 4. Queen-level receipt chain (nested)
 * 5. Fault tolerance and recovery
 *
 * **Properties**:
 * - Hierarchical coordination
 * - Multi-swarm work distribution
 * - Nested receipt chains (queen → workers)
 * - Graceful degradation
 *
 * @module multi-swarm/queen
 */

import { z } from 'zod';
import { EventEmitter } from 'events';
import { CoordinationHub, MessageType, SwarmStatus } from './coordination.mjs';
import { WorkerSwarm } from './worker-swarm.mjs';
import { ReceiptChain } from '../receipts/receipt-chain.mjs';
import { ReceiptGenerator } from '../receipts/receipt-generator.mjs';
import { blake3 } from 'hash-wasm';

/**
 * Job schema for queen coordination
 */
export const JobSchema = z.object({
  id: z.string().uuid(),
  type: z.string(),
  payload: z.any(),
  partitionStrategy: z.enum(['domain', 'round-robin', 'least-loaded']).default('domain'),
  aggregationStrategy: z.enum(['concat', 'merge', 'reduce']).default('concat'),
  priority: z.number().default(0),
  timeout: z.number().default(60000)
});

/**
 * Job status enum
 */
export const JobStatus = {
  PENDING: 'pending',
  DISTRIBUTING: 'distributing',
  EXECUTING: 'executing',
  AGGREGATING: 'aggregating',
  COMPLETED: 'completed',
  FAILED: 'failed'
};

/**
 * Queen Swarm - Meta-orchestrator for worker swarms
 *
 * @example
 * const queen = new QueenSwarm();
 *
 * // Create worker swarms
 * const compressionSwarm = new WorkerSwarm('compression-swarm', { domain: 'compression' });
 * const validationSwarm = new WorkerSwarm('validation-swarm', { domain: 'validation' });
 *
 * queen.addWorkerSwarm(compressionSwarm);
 * queen.addWorkerSwarm(validationSwarm);
 *
 * await queen.start();
 *
 * // Submit job that will be distributed across swarms
 * const result = await queen.submitJob({
 *   type: 'process-data',
 *   payload: { data: [...] },
 *   partitionStrategy: 'domain'
 * });
 */
export class QueenSwarm extends EventEmitter {
  /**
   * Create a new queen swarm
   *
   * @param {Object} [options] - Queen options
   */
  constructor(options = {}) {
    super();

    /** @type {string} Queen ID */
    this.id = options.id || 'queen';

    /** @type {CoordinationHub} Coordination hub */
    this.hub = new CoordinationHub(options);

    /** @type {Map<string, WorkerSwarm>} Worker swarms */
    this.workerSwarms = new Map();

    /** @type {Map<string, Object>} Active jobs */
    this.activeJobs = new Map();

    /** @type {Map<string, Object>} Completed jobs */
    this.completedJobs = new Map();

    /** @type {ReceiptChain} Queen receipt chain */
    this.queenReceiptChain = new ReceiptChain();

    /** @type {ReceiptGenerator} Receipt generator */
    this.receiptGenerator = new ReceiptGenerator({
      packageJsonPath: options.packageJsonPath
    });

    /** @type {boolean} Running state */
    this.running = false;

    /** @type {number} Heartbeat interval */
    this.heartbeatInterval = options.heartbeatInterval || 5000;

    /** @type {NodeJS.Timer} Heartbeat timer */
    this.heartbeatTimer = null;
  }

  /**
   * Add a worker swarm to the queen
   *
   * @param {WorkerSwarm} swarm - Worker swarm
   */
  addWorkerSwarm(swarm) {
    swarm.connectToHub(this.hub);
    this.workerSwarms.set(swarm.id, swarm);
    this.emit('swarm:added', { swarmId: swarm.id, domain: swarm.domain });
  }

  /**
   * Remove a worker swarm
   *
   * @param {string} swarmId - Swarm ID
   */
  removeWorkerSwarm(swarmId) {
    const swarm = this.workerSwarms.get(swarmId);
    if (swarm) {
      this.hub.unregisterSwarm(swarmId);
      this.workerSwarms.delete(swarmId);
      this.emit('swarm:removed', { swarmId });
    }
  }

  /**
   * Submit a job to the queen
   *
   * @param {Object} job - Job definition
   * @returns {Promise<Object>} Aggregated result
   */
  async submitJob(job) {
    const jobData = JobSchema.parse({
      id: crypto.randomUUID(),
      ...job
    });

    return new Promise((resolve, reject) => {
      const jobRecord = {
        ...jobData,
        status: JobStatus.PENDING,
        startTime: new Date(),
        workItems: [],
        results: new Map(),
        resolve,
        reject
      };

      this.activeJobs.set(jobData.id, jobRecord);
      this.emit('job:queued', { jobId: jobData.id });

      this._processJob(jobData.id).catch(reject);
    });
  }

  /**
   * Process a job
   *
   * @param {string} jobId - Job ID
   * @private
   */
  async _processJob(jobId) {
    const job = this.activeJobs.get(jobId);
    if (!job) {
      return;
    }

    try {
      // Phase 1: Partition work
      job.status = JobStatus.DISTRIBUTING;
      this.emit('job:distributing', { jobId });

      const workItems = this._partitionWork(job);
      job.workItems = workItems;

      // Phase 2: Distribute to swarms
      job.status = JobStatus.EXECUTING;
      this.emit('job:executing', { jobId, workItems: workItems.length });

      const distributionPromises = workItems.map(async (workItem) => {
        const workId = this.hub.distributeWork(workItem);
        if (!workId) {
          throw new Error(`No swarm available for work: ${workItem.id}`);
        }

        // Wait for result from hub
        return new Promise((resolve, reject) => {
          const timeoutId = setTimeout(() => {
            reject(new Error(`Work timeout: ${workId}`));
          }, job.timeout);

          const checkResult = () => {
            const result = this.hub.results.get(workId);
            if (result) {
              clearTimeout(timeoutId);
              resolve(result.result);
            } else {
              setTimeout(checkResult, 100);
            }
          };

          checkResult();
        });
      });

      const results = await Promise.all(distributionPromises);

      // Store individual results
      workItems.forEach((item, index) => {
        job.results.set(item.id, results[index]);
      });

      // Phase 3: Aggregate results
      job.status = JobStatus.AGGREGATING;
      this.emit('job:aggregating', { jobId });

      const aggregatedResult = this._aggregateResults(job, results);

      // Phase 4: Generate queen-level receipt
      const swarmReceipts = await Promise.all(
        Array.from(this.workerSwarms.values()).map(async (swarm) => {
          const verification = await swarm.verifyReceipts();
          return {
            swarmId: swarm.id,
            valid: verification.valid,
            receiptCount: swarm.receiptChain.length,
            lastReceipt: swarm.receiptChain.getLast()?.receiptHash
          };
        })
      );

      // Compute aggregate hash of all swarm receipts
      const aggregateHash = await blake3(
        JSON.stringify(swarmReceipts, Object.keys(swarmReceipts).sort())
      );

      const queenReceipt = await this.receiptGenerator.emitAdmissibilityReceipt({
        ontologyReleases: Array.from(this.workerSwarms.keys()).map(id => `swarm:${id}`),
        deltaCapsule: jobId,
        decision: 'allow',
        universeState: {
          jobId,
          workItems: job.workItems.length,
          swarmReceipts,
          aggregateHash,
          result: aggregatedResult,
          timestamp: new Date().toISOString()
        }
      });

      await this.queenReceiptChain.append(queenReceipt);

      // Complete job
      job.status = JobStatus.COMPLETED;
      job.endTime = new Date();
      job.duration = job.endTime - job.startTime;
      job.result = aggregatedResult;
      job.receipt = queenReceipt;

      this.activeJobs.delete(jobId);
      this.completedJobs.set(jobId, job);

      this.emit('job:completed', { jobId, result: aggregatedResult });
      job.resolve(aggregatedResult);

    } catch (error) {
      // Handle failure
      job.status = JobStatus.FAILED;
      job.endTime = new Date();
      job.error = error.message;

      // Generate failure receipt
      const failureReceipt = await this.receiptGenerator.emitAdmissibilityReceipt({
        ontologyReleases: Array.from(this.workerSwarms.keys()).map(id => `swarm:${id}`),
        deltaCapsule: jobId,
        decision: 'deny',
        universeState: {
          jobId,
          error: error.message,
          timestamp: new Date().toISOString()
        }
      });

      await this.queenReceiptChain.append(failureReceipt);

      this.activeJobs.delete(jobId);
      this.completedJobs.set(jobId, job);

      this.emit('job:failed', { jobId, error: error.message });
      job.reject(error);
    }
  }

  /**
   * Partition work based on strategy
   *
   * @param {Object} job - Job definition
   * @returns {Array<Object>} Work items
   * @private
   */
  _partitionWork(job) {
    const { payload, partitionStrategy, type } = job;

    // Default: single work item
    let workItems = [{
      id: crypto.randomUUID(),
      type,
      domain: job.domain,
      payload,
      priority: job.priority
    }];

    // If payload is an array, partition it
    if (Array.isArray(payload)) {
      if (partitionStrategy === 'domain') {
        // Partition by domain - one item per swarm domain
        const domains = new Set(Array.from(this.workerSwarms.values()).map(s => s.domain));
        const itemsPerDomain = Math.ceil(payload.length / domains.size);

        workItems = Array.from(domains).map((domain, index) => ({
          id: crypto.randomUUID(),
          type,
          domain,
          payload: payload.slice(index * itemsPerDomain, (index + 1) * itemsPerDomain),
          priority: job.priority
        }));

      } else if (partitionStrategy === 'round-robin') {
        // One item per swarm
        const swarmCount = this.workerSwarms.size;
        const itemsPerSwarm = Math.ceil(payload.length / swarmCount);

        workItems = Array.from({ length: swarmCount }, (_, index) => ({
          id: crypto.randomUUID(),
          type,
          payload: payload.slice(index * itemsPerSwarm, (index + 1) * itemsPerSwarm),
          priority: job.priority
        }));
      }
    }

    return workItems;
  }

  /**
   * Aggregate results based on strategy
   *
   * @param {Object} job - Job definition
   * @param {Array} results - Results from work items
   * @returns {any} Aggregated result
   * @private
   */
  _aggregateResults(job, results) {
    const { aggregationStrategy } = job;

    switch (aggregationStrategy) {
      case 'concat':
        // Concatenate array results
        return results.flat();

      case 'merge':
        // Merge object results
        return results.reduce((acc, result) => ({
          ...acc,
          ...result
        }), {});

      case 'reduce':
        // Custom reduce (if provided in payload)
        if (job.payload.reducer && typeof job.payload.reducer === 'function') {
          return results.reduce(job.payload.reducer, job.payload.initialValue);
        }
        return results;

      default:
        return results;
    }
  }

  /**
   * Start the queen swarm
   */
  async start() {
    this.running = true;
    this.hub.start();

    // Start all worker swarms
    for (const swarm of this.workerSwarms.values()) {
      await swarm.start();
    }

    // Start heartbeat
    this._startHeartbeat();

    this.emit('queen:started');
  }

  /**
   * Stop the queen swarm
   */
  async stop() {
    this.running = false;
    this.hub.stop();

    // Stop heartbeat
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }

    // Stop all worker swarms
    for (const swarm of this.workerSwarms.values()) {
      await swarm.stop();
    }

    this.emit('queen:stopped');
  }

  /**
   * Start heartbeat for swarm health monitoring
   *
   * @private
   */
  _startHeartbeat() {
    this.heartbeatTimer = setInterval(() => {
      // Send heartbeat to all swarms
      for (const swarm of this.workerSwarms.values()) {
        this.hub.sendMessage({
          type: MessageType.HEARTBEAT,
          from: this.id,
          to: swarm.id,
          payload: { timestamp: new Date().toISOString() }
        });

        // Process messages from hub
        swarm.processMessages();
      }

      this.emit('queen:heartbeat', {
        swarms: this.workerSwarms.size,
        activeJobs: this.activeJobs.size
      });
    }, this.heartbeatInterval);
  }

  /**
   * Get queen statistics
   *
   * @returns {Object}
   */
  getStats() {
    const swarmStats = Array.from(this.workerSwarms.values()).map(s => s.getStats());

    return {
      queen: {
        id: this.id,
        running: this.running,
        swarms: this.workerSwarms.size,
        activeJobs: this.activeJobs.size,
        completedJobs: this.completedJobs.size,
        queenReceipts: this.queenReceiptChain.length
      },
      coordination: this.hub.getStats(),
      swarms: swarmStats,
      jobs: {
        pending: Array.from(this.activeJobs.values())
          .filter(j => j.status === JobStatus.PENDING).length,
        distributing: Array.from(this.activeJobs.values())
          .filter(j => j.status === JobStatus.DISTRIBUTING).length,
        executing: Array.from(this.activeJobs.values())
          .filter(j => j.status === JobStatus.EXECUTING).length,
        aggregating: Array.from(this.activeJobs.values())
          .filter(j => j.status === JobStatus.AGGREGATING).length,
        completed: this.completedJobs.size,
        failed: Array.from(this.completedJobs.values())
          .filter(j => j.status === JobStatus.FAILED).length
      },
      performance: {
        averageJobDuration: this.completedJobs.size > 0
          ? Math.round(Array.from(this.completedJobs.values())
              .reduce((sum, j) => sum + (j.duration || 0), 0) / this.completedJobs.size)
          : 0,
        successRate: this.completedJobs.size > 0
          ? ((Array.from(this.completedJobs.values())
              .filter(j => j.status === JobStatus.COMPLETED).length / this.completedJobs.size) * 100)
              .toFixed(2) + '%'
          : 'N/A'
      }
    };
  }

  /**
   * Verify all receipt chains (queen + workers)
   *
   * @returns {Promise<Object>}
   */
  async verifyAllReceipts() {
    const queenVerification = await this.queenReceiptChain.verify();

    const workerVerifications = await Promise.all(
      Array.from(this.workerSwarms.values()).map(async (swarm) => ({
        swarmId: swarm.id,
        verification: await swarm.verifyReceipts()
      }))
    );

    const allValid = queenVerification.valid &&
      workerVerifications.every(v => v.verification.valid);

    return {
      valid: allValid,
      queen: queenVerification,
      workers: workerVerifications
    };
  }

  /**
   * Export queen state to JSON-LD
   *
   * @returns {Object}
   */
  toJSONLD() {
    return {
      '@context': {
        unrdf: 'https://unrdf.org/vocab#',
        swarm: 'https://unrdf.org/swarm#'
      },
      '@type': 'swarm:QueenSwarm',
      'swarm:id': this.id,
      'swarm:workerSwarms': Array.from(this.workerSwarms.values()).map(s => s.toJSONLD()),
      'swarm:queenReceipts': this.queenReceiptChain.toJSONLD(),
      'swarm:stats': this.getStats()
    };
  }

  /**
   * Create a queen swarm
   *
   * @param {Object} [options] - Queen options
   * @returns {QueenSwarm}
   */
  static create(options = {}) {
    return new QueenSwarm(options);
  }
}

/**
 * Create a queen swarm
 *
 * @param {Object} [options] - Queen options
 * @returns {QueenSwarm}
 */
export function createQueenSwarm(options = {}) {
  return new QueenSwarm(options);
}
