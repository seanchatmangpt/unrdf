/**
 * @fileoverview Multi-Swarm Coordination - Inter-swarm messaging and work distribution
 *
 * **Purpose**: Enable communication and coordination between swarms
 * 1. Message queue for swarm-to-swarm communication
 * 2. Work distribution and load balancing
 * 3. Result aggregation across swarms
 * 4. Fault isolation boundaries
 *
 * **Properties**:
 * - Async message passing (non-blocking)
 * - Work stealing for load balancing
 * - Isolated failure domains
 * - Receipt-based audit trail
 *
 * @module multi-swarm/coordination
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Message types for inter-swarm communication
 */
export const MessageType = {
  WORK_REQUEST: 'work_request',
  WORK_RESPONSE: 'work_response',
  WORK_STEAL: 'work_steal',
  RESULT: 'result',
  STATUS: 'status',
  HEARTBEAT: 'heartbeat',
  ERROR: 'error'
};

/**
 * Message schema
 */
export const MessageSchema = z.object({
  id: z.string().uuid(),
  type: z.enum([
    'work_request',
    'work_response',
    'work_steal',
    'result',
    'status',
    'heartbeat',
    'error'
  ]),
  from: z.string(),
  to: z.string(),
  payload: z.any(),
  timestamp: z.string().datetime(),
  correlationId: z.string().uuid().optional()
});

/**
 * Swarm status enum
 */
export const SwarmStatus = {
  IDLE: 'idle',
  WORKING: 'working',
  OVERLOADED: 'overloaded',
  FAILED: 'failed',
  STOPPED: 'stopped'
};

/**
 * Message Queue - FIFO queue with priority support
 *
 * @example
 * const queue = new MessageQueue();
 * queue.enqueue(message);
 * const msg = queue.dequeue();
 */
export class MessageQueue {
  /**
   * Create a new message queue
   *
   * @param {Object} [options] - Queue options
   * @param {number} [options.maxSize=1000] - Maximum queue size
   */
  constructor(options = {}) {
    /** @type {Array} Message queue */
    this.queue = [];

    /** @type {number} Max queue size */
    this.maxSize = options.maxSize || 1000;

    /** @type {Map<string, Array>} Priority queues by type */
    this.priorityQueues = new Map();
  }

  /**
   * Enqueue a message
   *
   * @param {Object} message - Message to enqueue
   * @param {number} [priority=0] - Priority (higher = more urgent)
   * @throws {Error} If queue is full
   */
  enqueue(message, priority = 0) {
    const validated = MessageSchema.parse(message);

    if (this.size >= this.maxSize) {
      throw new Error(`Queue full: ${this.size}/${this.maxSize}`);
    }

    if (priority > 0) {
      // Add to priority queue
      if (!this.priorityQueues.has(priority)) {
        this.priorityQueues.set(priority, []);
      }
      this.priorityQueues.get(priority).push(validated);
    } else {
      // Add to regular queue
      this.queue.push(validated);
    }
  }

  /**
   * Dequeue a message (priority first, then FIFO)
   *
   * @returns {Object|null} Next message or null if empty
   */
  dequeue() {
    // Check priority queues (highest first)
    const priorities = Array.from(this.priorityQueues.keys()).sort((a, b) => b - a);
    for (const priority of priorities) {
      const queue = this.priorityQueues.get(priority);
      if (queue.length > 0) {
        const message = queue.shift();
        if (queue.length === 0) {
          this.priorityQueues.delete(priority);
        }
        return message;
      }
    }

    // Check regular queue
    if (this.queue.length > 0) {
      return this.queue.shift();
    }

    return null;
  }

  /**
   * Peek at next message without removing it
   *
   * @returns {Object|null}
   */
  peek() {
    // Check priority queues
    const priorities = Array.from(this.priorityQueues.keys()).sort((a, b) => b - a);
    for (const priority of priorities) {
      const queue = this.priorityQueues.get(priority);
      if (queue.length > 0) {
        return queue[0];
      }
    }

    // Check regular queue
    return this.queue.length > 0 ? this.queue[0] : null;
  }

  /**
   * Get messages for a specific swarm
   *
   * @param {string} swarmId - Swarm ID
   * @returns {Object[]} Messages for swarm
   */
  getMessagesFor(swarmId) {
    const messages = [];

    // Check priority queues
    for (const queue of this.priorityQueues.values()) {
      messages.push(...queue.filter(m => m.to === swarmId));
    }

    // Check regular queue
    messages.push(...this.queue.filter(m => m.to === swarmId));

    return messages;
  }

  /**
   * Remove messages for a specific swarm
   *
   * @param {string} swarmId - Swarm ID
   * @returns {Object[]} Removed messages
   */
  removeMessagesFor(swarmId) {
    const removed = [];

    // Remove from priority queues
    for (const [priority, queue] of this.priorityQueues.entries()) {
      const filtered = queue.filter(m => {
        if (m.to === swarmId) {
          removed.push(m);
          return false;
        }
        return true;
      });
      this.priorityQueues.set(priority, filtered);
      if (filtered.length === 0) {
        this.priorityQueues.delete(priority);
      }
    }

    // Remove from regular queue
    this.queue = this.queue.filter(m => {
      if (m.to === swarmId) {
        removed.push(m);
        return false;
      }
      return true;
    });

    return removed;
  }

  /**
   * Get queue size
   *
   * @returns {number}
   */
  get size() {
    let total = this.queue.length;
    for (const queue of this.priorityQueues.values()) {
      total += queue.length;
    }
    return total;
  }

  /**
   * Check if queue is empty
   *
   * @returns {boolean}
   */
  isEmpty() {
    return this.size === 0;
  }

  /**
   * Clear all messages
   */
  clear() {
    this.queue = [];
    this.priorityQueues.clear();
  }
}

/**
 * Work Distribution Strategy
 */
export class WorkDistributor {
  /**
   * Create a new work distributor
   *
   * @param {Object} [options] - Distributor options
   * @param {string} [options.strategy='round-robin'] - Distribution strategy
   */
  constructor(options = {}) {
    /** @type {string} Distribution strategy */
    this.strategy = options.strategy || 'round-robin';

    /** @type {Map<string, Object>} Swarm registry */
    this.swarms = new Map();

    /** @type {number} Round-robin counter */
    this.roundRobinIndex = 0;
  }

  /**
   * Register a swarm
   *
   * @param {string} swarmId - Swarm ID
   * @param {Object} metadata - Swarm metadata
   */
  registerSwarm(swarmId, metadata = {}) {
    this.swarms.set(swarmId, {
      id: swarmId,
      status: SwarmStatus.IDLE,
      load: 0,
      capacity: metadata.capacity || 10,
      domain: metadata.domain,
      ...metadata
    });
  }

  /**
   * Unregister a swarm
   *
   * @param {string} swarmId - Swarm ID
   */
  unregisterSwarm(swarmId) {
    this.swarms.delete(swarmId);
  }

  /**
   * Update swarm status
   *
   * @param {string} swarmId - Swarm ID
   * @param {Object} status - Status update
   */
  updateSwarmStatus(swarmId, status) {
    const swarm = this.swarms.get(swarmId);
    if (swarm) {
      Object.assign(swarm, status);
    }
  }

  /**
   * Select swarm for work (based on strategy)
   *
   * @param {Object} work - Work item
   * @returns {string|null} Selected swarm ID or null if none available
   */
  selectSwarm(work) {
    const availableSwarms = Array.from(this.swarms.values())
      .filter(s => s.status !== SwarmStatus.FAILED && s.status !== SwarmStatus.STOPPED);

    if (availableSwarms.length === 0) {
      return null;
    }

    // Filter by domain if specified
    let candidates = availableSwarms;
    if (work.domain) {
      const domainSwarms = availableSwarms.filter(s => s.domain === work.domain);
      if (domainSwarms.length > 0) {
        candidates = domainSwarms;
      }
    }

    switch (this.strategy) {
      case 'round-robin':
        return this._selectRoundRobin(candidates);
      case 'least-loaded':
        return this._selectLeastLoaded(candidates);
      case 'random':
        return this._selectRandom(candidates);
      default:
        return this._selectRoundRobin(candidates);
    }
  }

  /**
   * Select swarm using round-robin
   *
   * @param {Array} swarms - Available swarms
   * @returns {string}
   * @private
   */
  _selectRoundRobin(swarms) {
    if (swarms.length === 0) return null;
    const swarm = swarms[this.roundRobinIndex % swarms.length];
    this.roundRobinIndex++;
    return swarm.id;
  }

  /**
   * Select least loaded swarm
   *
   * @param {Array} swarms - Available swarms
   * @returns {string}
   * @private
   */
  _selectLeastLoaded(swarms) {
    if (swarms.length === 0) return null;
    const sorted = swarms.sort((a, b) => {
      const aUtilization = a.load / a.capacity;
      const bUtilization = b.load / b.capacity;
      return aUtilization - bUtilization;
    });
    return sorted[0].id;
  }

  /**
   * Select random swarm
   *
   * @param {Array} swarms - Available swarms
   * @returns {string}
   * @private
   */
  _selectRandom(swarms) {
    if (swarms.length === 0) return null;
    const index = Math.floor(Math.random() * swarms.length);
    return swarms[index].id;
  }

  /**
   * Get overloaded swarms
   *
   * @returns {Object[]}
   */
  getOverloadedSwarms() {
    return Array.from(this.swarms.values())
      .filter(s => s.load / s.capacity > 0.8);
  }

  /**
   * Get idle swarms
   *
   * @returns {Object[]}
   */
  getIdleSwarms() {
    return Array.from(this.swarms.values())
      .filter(s => s.status === SwarmStatus.IDLE);
  }

  /**
   * Get swarm statistics
   *
   * @returns {Object}
   */
  getStats() {
    const swarms = Array.from(this.swarms.values());
    return {
      totalSwarms: swarms.length,
      activeSwarms: swarms.filter(s => s.status === SwarmStatus.WORKING).length,
      idleSwarms: swarms.filter(s => s.status === SwarmStatus.IDLE).length,
      overloadedSwarms: swarms.filter(s => s.status === SwarmStatus.OVERLOADED).length,
      failedSwarms: swarms.filter(s => s.status === SwarmStatus.FAILED).length,
      totalLoad: swarms.reduce((sum, s) => sum + s.load, 0),
      totalCapacity: swarms.reduce((sum, s) => sum + s.capacity, 0),
      averageUtilization: swarms.length > 0
        ? (swarms.reduce((sum, s) => sum + (s.load / s.capacity), 0) / swarms.length * 100).toFixed(2) + '%'
        : '0%'
    };
  }
}

/**
 * Coordination Hub - Central coordination point for all swarms
 *
 * @example
 * const hub = new CoordinationHub();
 * hub.registerSwarm('compression-swarm', { domain: 'compression' });
 * hub.registerSwarm('validation-swarm', { domain: 'validation' });
 *
 * hub.sendMessage({
 *   type: MessageType.WORK_REQUEST,
 *   from: 'queen',
 *   to: 'compression-swarm',
 *   payload: { task: 'compress' }
 * });
 */
export class CoordinationHub extends EventEmitter {
  /**
   * Create a new coordination hub
   *
   * @param {Object} [options] - Hub options
   */
  constructor(options = {}) {
    super();

    /** @type {MessageQueue} Message queue */
    this.messageQueue = new MessageQueue(options);

    /** @type {WorkDistributor} Work distributor */
    this.distributor = new WorkDistributor(options);

    /** @type {Map<string, Object>} Active work items */
    this.activeWork = new Map();

    /** @type {Map<string, Object>} Result cache */
    this.results = new Map();

    /** @type {boolean} Hub running state */
    this.running = false;
  }

  /**
   * Register a swarm with the hub
   *
   * @param {string} swarmId - Swarm ID
   * @param {Object} metadata - Swarm metadata
   */
  registerSwarm(swarmId, metadata = {}) {
    this.distributor.registerSwarm(swarmId, metadata);
    this.emit('swarm:registered', { swarmId, metadata });
  }

  /**
   * Unregister a swarm
   *
   * @param {string} swarmId - Swarm ID
   */
  unregisterSwarm(swarmId) {
    this.distributor.unregisterSwarm(swarmId);
    this.messageQueue.removeMessagesFor(swarmId);
    this.emit('swarm:unregistered', { swarmId });
  }

  /**
   * Send message to a swarm
   *
   * @param {Object} message - Message to send
   * @param {number} [priority=0] - Message priority
   */
  sendMessage(message, priority = 0) {
    const fullMessage = {
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      ...message
    };

    this.messageQueue.enqueue(fullMessage, priority);
    this.emit('message:sent', fullMessage);
  }

  /**
   * Receive messages for a swarm
   *
   * @param {string} swarmId - Swarm ID
   * @returns {Object[]} Messages for swarm
   */
  receiveMessages(swarmId) {
    const messages = this.messageQueue.getMessagesFor(swarmId);
    // Remove received messages
    this.messageQueue.removeMessagesFor(swarmId);
    this.emit('message:received', { swarmId, count: messages.length });
    return messages;
  }

  /**
   * Distribute work to swarms
   *
   * @param {Object} work - Work item
   * @returns {string|null} Work ID (or null if no swarm available)
   */
  distributeWork(work) {
    const swarmId = this.distributor.selectSwarm(work);
    if (!swarmId) {
      return null;
    }

    // Use provided ID or generate new one
    const workId = work.id || crypto.randomUUID();

    this.activeWork.set(workId, {
      id: workId,
      swarmId,
      work,
      startTime: new Date().toISOString(),
      status: 'pending'
    });

    this.sendMessage({
      type: MessageType.WORK_REQUEST,
      from: 'hub',
      to: swarmId,
      payload: { workId, ...work },
      correlationId: workId
    }, work.priority || 0);

    this.emit('work:distributed', { workId, swarmId, work });
    return workId;
  }

  /**
   * Submit work result
   *
   * @param {string} workId - Work ID
   * @param {Object} result - Work result
   */
  submitResult(workId, result) {
    const work = this.activeWork.get(workId);
    if (work) {
      work.status = 'completed';
      work.endTime = new Date().toISOString();
      work.result = result;
      this.results.set(workId, work);
      this.activeWork.delete(workId);
      this.emit('work:completed', { workId, result });
    }
  }

  /**
   * Report work failure
   *
   * @param {string} workId - Work ID
   * @param {Error} error - Error object
   */
  reportFailure(workId, error) {
    const work = this.activeWork.get(workId);
    if (work) {
      work.status = 'failed';
      work.endTime = new Date().toISOString();
      work.error = error.message;
      this.results.set(workId, work);
      this.activeWork.delete(workId);
      this.emit('work:failed', { workId, error: error.message });
    }
  }

  /**
   * Update swarm status
   *
   * @param {string} swarmId - Swarm ID
   * @param {Object} status - Status update
   */
  updateSwarmStatus(swarmId, status) {
    this.distributor.updateSwarmStatus(swarmId, status);
    this.emit('swarm:status', { swarmId, status });
  }

  /**
   * Request work stealing from overloaded swarms
   *
   * @param {string} requestingSwarmId - Requesting swarm ID
   * @returns {Object|null} Stolen work or null
   */
  requestWorkSteal(requestingSwarmId) {
    const overloaded = this.distributor.getOverloadedSwarms();
    if (overloaded.length === 0) {
      return null;
    }

    // Find work from overloaded swarm
    const targetSwarm = overloaded[0];
    const workItems = Array.from(this.activeWork.values())
      .filter(w => w.swarmId === targetSwarm.id && w.status === 'pending');

    if (workItems.length === 0) {
      return null;
    }

    // Steal work
    const stolenWork = workItems[0];
    stolenWork.swarmId = requestingSwarmId;
    stolenWork.stolen = true;

    this.sendMessage({
      type: MessageType.WORK_STEAL,
      from: requestingSwarmId,
      to: requestingSwarmId,
      payload: { workId: stolenWork.id, work: stolenWork.work }
    });

    this.emit('work:stolen', {
      workId: stolenWork.id,
      from: targetSwarm.id,
      to: requestingSwarmId
    });

    return stolenWork;
  }

  /**
   * Get coordination statistics
   *
   * @returns {Object}
   */
  getStats() {
    return {
      ...this.distributor.getStats(),
      queueSize: this.messageQueue.size,
      activeWork: this.activeWork.size,
      completedWork: Array.from(this.results.values())
        .filter(w => w.status === 'completed').length,
      failedWork: Array.from(this.results.values())
        .filter(w => w.status === 'failed').length
    };
  }

  /**
   * Start the coordination hub
   */
  start() {
    this.running = true;
    this.emit('hub:started');
  }

  /**
   * Stop the coordination hub
   */
  stop() {
    this.running = false;
    this.emit('hub:stopped');
  }
}

/**
 * Create a coordination hub
 *
 * @param {Object} [options] - Hub options
 * @returns {CoordinationHub}
 */
export function createCoordinationHub(options = {}) {
  return new CoordinationHub(options);
}
