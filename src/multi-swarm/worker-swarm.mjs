/**
 * @fileoverview Worker Swarm - Domain-specific agent manager
 *
 * **Purpose**: Manage agents within a specific domain
 * 1. Agent lifecycle management (spawn, monitor, terminate)
 * 2. Work queue and task distribution
 * 3. Result collection and aggregation
 * 4. Domain-specific receipt chain
 * 5. Fault isolation and recovery
 *
 * **Properties**:
 * - Isolated execution environment
 * - Receipt-based audit trail
 * - Work stealing support
 * - Graceful degradation on failure
 *
 * @module multi-swarm/worker-swarm
 */

import { z } from 'zod';
import { EventEmitter } from 'events';
import { ReceiptChain } from '../receipts/receipt-chain.mjs';
import { ReceiptGenerator } from '../receipts/receipt-generator.mjs';
import { MessageType, SwarmStatus } from './coordination.mjs';

/**
 * Agent status enum
 */
export const AgentStatus = {
  IDLE: 'idle',
  WORKING: 'working',
  FAILED: 'failed',
  STOPPED: 'stopped'
};

/**
 * Work item schema
 */
export const WorkItemSchema = z.object({
  id: z.string().uuid(),
  type: z.string(),
  payload: z.any(),
  priority: z.number().default(0),
  timeout: z.number().default(30000),
  retries: z.number().default(0),
  maxRetries: z.number().default(3)
});

/**
 * Agent - Individual worker in a swarm
 */
export class Agent {
  /**
   * Create a new agent
   *
   * @param {string} id - Agent ID
   * @param {Function} processor - Work processor function
   * @param {Object} [options] - Agent options
   */
  constructor(id, processor, options = {}) {
    /** @type {string} Agent ID */
    this.id = id;

    /** @type {Function} Work processor */
    this.processor = processor;

    /** @type {string} Agent status */
    this.status = AgentStatus.IDLE;

    /** @type {Object|null} Current work */
    this.currentWork = null;

    /** @type {number} Work completed count */
    this.completedCount = 0;

    /** @type {number} Work failed count */
    this.failedCount = 0;

    /** @type {Date|null} Last activity time */
    this.lastActivity = null;
  }

  /**
   * Process work item
   *
   * @param {Object} work - Work item
   * @returns {Promise<Object>} Result
   */
  async process(work) {
    this.status = AgentStatus.WORKING;
    this.currentWork = work;
    this.lastActivity = new Date();

    try {
      const result = await this.processor(work.payload);
      this.status = AgentStatus.IDLE;
      this.currentWork = null;
      this.completedCount++;
      return { success: true, result };
    } catch (error) {
      this.status = AgentStatus.IDLE;
      this.currentWork = null;
      this.failedCount++;
      return { success: false, error: error.message };
    }
  }

  /**
   * Check if agent is available
   *
   * @returns {boolean}
   */
  isAvailable() {
    return this.status === AgentStatus.IDLE;
  }

  /**
   * Stop the agent
   */
  stop() {
    this.status = AgentStatus.STOPPED;
  }
}

/**
 * Worker Swarm - Domain-specific agent manager
 *
 * @example
 * const swarm = new WorkerSwarm('compression-swarm', {
 *   domain: 'compression',
 *   capacity: 10
 * });
 *
 * swarm.addAgent('agent-1', async (work) => {
 *   return compressData(work);
 * });
 *
 * await swarm.start();
 * const result = await swarm.submitWork({ type: 'compress', data: [...] });
 */
export class WorkerSwarm extends EventEmitter {
  /**
   * Create a new worker swarm
   *
   * @param {string} id - Swarm ID
   * @param {Object} [options] - Swarm options
   */
  constructor(id, options = {}) {
    super();

    /** @type {string} Swarm ID */
    this.id = id;

    /** @type {string} Swarm domain */
    this.domain = options.domain || 'general';

    /** @type {number} Swarm capacity */
    this.capacity = options.capacity || 10;

    /** @type {Map<string, Agent>} Agents */
    this.agents = new Map();

    /** @type {Array<Object>} Work queue */
    this.workQueue = [];

    /** @type {Map<string, Object>} Active work */
    this.activeWork = new Map();

    /** @type {Map<string, Object>} Completed work */
    this.completedWork = new Map();

    /** @type {ReceiptChain} Receipt chain */
    this.receiptChain = new ReceiptChain();

    /** @type {ReceiptGenerator} Receipt generator */
    this.receiptGenerator = new ReceiptGenerator({
      packageJsonPath: options.packageJsonPath
    });

    /** @type {string} Swarm status */
    this.status = SwarmStatus.IDLE;

    /** @type {boolean} Running state */
    this.running = false;

    /** @type {number} Work timeout */
    this.workTimeout = options.workTimeout || 30000;

    /** @type {Object} Coordination hub reference */
    this.hub = null;
  }

  /**
   * Connect to coordination hub
   *
   * @param {Object} hub - Coordination hub
   */
  connectToHub(hub) {
    this.hub = hub;
    this.hub.registerSwarm(this.id, {
      domain: this.domain,
      capacity: this.capacity
    });
  }

  /**
   * Add an agent to the swarm
   *
   * @param {string} agentId - Agent ID
   * @param {Function} processor - Work processor function
   */
  addAgent(agentId, processor) {
    if (this.agents.size >= this.capacity) {
      throw new Error(`Swarm at capacity: ${this.agents.size}/${this.capacity}`);
    }

    const agent = new Agent(agentId, processor);
    this.agents.set(agentId, agent);
    this.emit('agent:added', { agentId });
  }

  /**
   * Remove an agent from the swarm
   *
   * @param {string} agentId - Agent ID
   */
  removeAgent(agentId) {
    const agent = this.agents.get(agentId);
    if (agent) {
      agent.stop();
      this.agents.delete(agentId);
      this.emit('agent:removed', { agentId });
    }
  }

  /**
   * Submit work to the swarm
   *
   * @param {Object} work - Work item
   * @returns {Promise<Object>} Work result
   */
  async submitWork(work) {
    const workItem = WorkItemSchema.parse({
      id: crypto.randomUUID(),
      ...work
    });

    return new Promise((resolve, reject) => {
      this.workQueue.push({
        ...workItem,
        resolve,
        reject
      });

      this.emit('work:queued', { workId: workItem.id });
      this._processQueue();
    });
  }

  /**
   * Process work queue
   *
   * @private
   */
  async _processQueue() {
    if (!this.running) {
      return;
    }

    // Find available agent
    const availableAgent = Array.from(this.agents.values())
      .find(a => a.isAvailable());

    if (!availableAgent || this.workQueue.length === 0) {
      return;
    }

    // Get next work item
    const workItem = this.workQueue.shift();
    this.activeWork.set(workItem.id, {
      ...workItem,
      agentId: availableAgent.id,
      startTime: new Date()
    });

    this._updateStatus();
    this.emit('work:started', { workId: workItem.id, agentId: availableAgent.id });

    try {
      // Set timeout
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => reject(new Error('Work timeout')), workItem.timeout);
      });

      // Process work
      const resultPromise = availableAgent.process(workItem);
      const result = await Promise.race([resultPromise, timeoutPromise]);

      // Generate receipt
      const receipt = await this.receiptGenerator.emitAdmissibilityReceipt({
        ontologyReleases: [`swarm:${this.id}`],
        deltaCapsule: workItem.id,
        decision: result.success ? 'allow' : 'deny',
        universeState: {
          workId: workItem.id,
          agentId: availableAgent.id,
          result: result.result,
          timestamp: new Date().toISOString()
        }
      });

      await this.receiptChain.append(receipt);

      // Store result
      this.activeWork.delete(workItem.id);
      this.completedWork.set(workItem.id, {
        workId: workItem.id,
        result: result.result,
        receipt,
        endTime: new Date()
      });

      this._updateStatus();
      this.emit('work:completed', { workId: workItem.id, result: result.result });

      // Notify hub if connected
      if (this.hub) {
        this.hub.submitResult(workItem.id, result);
      }

      workItem.resolve(result.result);

    } catch (error) {
      // Handle failure
      const workData = this.activeWork.get(workItem.id);
      this.activeWork.delete(workItem.id);

      // Retry if possible
      if (workItem.retries < workItem.maxRetries) {
        workItem.retries++;
        this.workQueue.unshift(workItem);
        this.emit('work:retry', { workId: workItem.id, retry: workItem.retries });
      } else {
        // Generate failure receipt
        const receipt = await this.receiptGenerator.emitAdmissibilityReceipt({
          ontologyReleases: [`swarm:${this.id}`],
          deltaCapsule: workItem.id,
          decision: 'deny',
          universeState: {
            workId: workItem.id,
            agentId: availableAgent.id,
            error: error.message,
            timestamp: new Date().toISOString()
          }
        });

        await this.receiptChain.append(receipt);

        this.completedWork.set(workItem.id, {
          workId: workItem.id,
          error: error.message,
          receipt,
          endTime: new Date()
        });

        this.emit('work:failed', { workId: workItem.id, error: error.message });

        // Notify hub if connected
        if (this.hub) {
          this.hub.reportFailure(workItem.id, error);
        }

        workItem.reject(error);
      }

      this._updateStatus();
    }

    // Continue processing queue
    setImmediate(() => this._processQueue());
  }

  /**
   * Update swarm status
   *
   * @private
   */
  _updateStatus() {
    const utilization = this.activeWork.size / this.capacity;

    if (this.activeWork.size === 0) {
      this.status = SwarmStatus.IDLE;
    } else if (utilization >= 0.8) {
      this.status = SwarmStatus.OVERLOADED;
    } else {
      this.status = SwarmStatus.WORKING;
    }

    // Update hub if connected
    if (this.hub) {
      this.hub.updateSwarmStatus(this.id, {
        status: this.status,
        load: this.activeWork.size,
        capacity: this.capacity
      });
    }
  }

  /**
   * Start the swarm
   */
  async start() {
    this.running = true;
    this.status = SwarmStatus.IDLE;
    this.emit('swarm:started');
    this._processQueue();
    this._startMessagePolling();
  }

  /**
   * Start polling for messages from hub
   *
   * @private
   */
  _startMessagePolling() {
    if (!this.hub || this.messagePollingInterval) {
      return;
    }

    // Poll for messages every 100ms
    this.messagePollingInterval = setInterval(() => {
      this.processMessages();
    }, 100);
  }

  /**
   * Stop polling for messages
   *
   * @private
   */
  _stopMessagePolling() {
    if (this.messagePollingInterval) {
      clearInterval(this.messagePollingInterval);
      this.messagePollingInterval = null;
    }
  }

  /**
   * Stop the swarm
   */
  async stop() {
    this.running = false;
    this.status = SwarmStatus.STOPPED;

    // Stop message polling
    this._stopMessagePolling();

    // Stop all agents
    for (const agent of this.agents.values()) {
      agent.stop();
    }

    this.emit('swarm:stopped');
  }

  /**
   * Request work from hub (work stealing)
   */
  async requestWork() {
    if (!this.hub) {
      return null;
    }

    return this.hub.requestWorkSteal(this.id);
  }

  /**
   * Process messages from hub
   */
  async processMessages() {
    if (!this.hub) {
      return;
    }

    const messages = this.hub.receiveMessages(this.id);

    for (const message of messages) {
      switch (message.type) {
        case MessageType.WORK_REQUEST: {
          // Extract workId from payload and process
          const { workId, ...workPayload } = message.payload;
          try {
            const result = await this.submitWork(workPayload);
            // Notify hub of completion
            if (workId) {
              this.hub.submitResult(workId, result);
            }
          } catch (error) {
            // Notify hub of failure
            if (workId) {
              this.hub.reportFailure(workId, error);
            }
          }
          break;
        }

        case MessageType.WORK_STEAL: {
          const { workId, work } = message.payload;
          try {
            const result = await this.submitWork(work);
            if (workId) {
              this.hub.submitResult(workId, result);
            }
          } catch (error) {
            if (workId) {
              this.hub.reportFailure(workId, error);
            }
          }
          break;
        }

        case MessageType.STATUS:
          this._updateStatus();
          break;

        default:
          this.emit('message:unknown', message);
      }
    }
  }

  /**
   * Get swarm statistics
   *
   * @returns {Object}
   */
  getStats() {
    const agents = Array.from(this.agents.values());

    return {
      id: this.id,
      domain: this.domain,
      status: this.status,
      agents: {
        total: this.agents.size,
        idle: agents.filter(a => a.status === AgentStatus.IDLE).length,
        working: agents.filter(a => a.status === AgentStatus.WORKING).length,
        failed: agents.filter(a => a.status === AgentStatus.FAILED).length
      },
      work: {
        queued: this.workQueue.length,
        active: this.activeWork.size,
        completed: this.completedWork.size
      },
      utilization: ((this.activeWork.size / this.capacity) * 100).toFixed(2) + '%',
      receipts: this.receiptChain.length,
      performance: {
        totalCompleted: agents.reduce((sum, a) => sum + a.completedCount, 0),
        totalFailed: agents.reduce((sum, a) => sum + a.failedCount, 0),
        successRate: agents.reduce((sum, a) => sum + a.completedCount, 0) > 0
          ? ((agents.reduce((sum, a) => sum + a.completedCount, 0) /
             (agents.reduce((sum, a) => sum + a.completedCount, 0) +
              agents.reduce((sum, a) => sum + a.failedCount, 0))) * 100).toFixed(2) + '%'
          : 'N/A'
      }
    };
  }

  /**
   * Verify receipt chain
   *
   * @returns {Promise<Object>}
   */
  async verifyReceipts() {
    return this.receiptChain.verify();
  }

  /**
   * Export swarm state to JSON-LD
   *
   * @returns {Object}
   */
  toJSONLD() {
    return {
      '@context': {
        unrdf: 'https://unrdf.org/vocab#',
        swarm: 'https://unrdf.org/swarm#'
      },
      '@type': 'swarm:WorkerSwarm',
      'swarm:id': this.id,
      'swarm:domain': this.domain,
      'swarm:status': this.status,
      'swarm:capacity': this.capacity,
      'swarm:agents': Array.from(this.agents.keys()),
      'swarm:receipts': this.receiptChain.toJSONLD(),
      'swarm:stats': this.getStats()
    };
  }

  /**
   * Create a worker swarm
   *
   * @param {string} id - Swarm ID
   * @param {Object} [options] - Swarm options
   * @returns {WorkerSwarm}
   */
  static create(id, options = {}) {
    return new WorkerSwarm(id, options);
  }
}

/**
 * Create a worker swarm
 *
 * @param {string} id - Swarm ID
 * @param {Object} [options] - Swarm options
 * @returns {WorkerSwarm}
 */
export function createWorkerSwarm(id, options = {}) {
  return new WorkerSwarm(id, options);
}
