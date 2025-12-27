/**
 * KGC Runtime - Async Work Item System
 * Provides work item management with deterministic scheduling and receipt logging
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * Work item state enumeration
 */
export const WORK_ITEM_STATES = {
  QUEUED: 'queued',
  RUNNING: 'running',
  SUCCEEDED: 'succeeded',
  FAILED: 'failed',
  DENIED: 'denied',
};

/**
 * Terminal states (cannot transition from these)
 */
const TERMINAL_STATES = new Set([
  WORK_ITEM_STATES.SUCCEEDED,
  WORK_ITEM_STATES.FAILED,
  WORK_ITEM_STATES.DENIED,
]);

/**
 * Valid state transitions
 */
const VALID_TRANSITIONS = {
  [WORK_ITEM_STATES.QUEUED]: [
    WORK_ITEM_STATES.RUNNING,
    WORK_ITEM_STATES.DENIED,
  ],
  [WORK_ITEM_STATES.RUNNING]: [
    WORK_ITEM_STATES.SUCCEEDED,
    WORK_ITEM_STATES.FAILED,
  ],
  [WORK_ITEM_STATES.SUCCEEDED]: [],
  [WORK_ITEM_STATES.FAILED]: [],
  [WORK_ITEM_STATES.DENIED]: [],
};

/**
 * Zod schema for work item bounds
 */
const BoundsSchema = z.object({
  timeout: z.number().optional(),
  maxRetries: z.number().optional(),
  priority: z.number().optional(),
  metadata: z.any().optional(),
}).optional();

/**
 * Generate nanosecond timestamp
 * @returns {bigint} Current time in nanoseconds
 */
function nowNs() {
  const hrTime = process.hrtime.bigint();
  return hrTime;
}

/**
 * Generate unique work item ID
 * @returns {string} Unique ID
 */
function generateWorkItemId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return `work-item-${crypto.randomUUID()}`;
  }
  try {
    const crypto = require('crypto');
    return `work-item-${crypto.randomUUID()}`;
  } catch {
    return `work-item-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }
}

/**
 * WorkItem Executor - Manages async work items with deterministic scheduling
 *
 * @example
 * import { WorkItemExecutor } from './work-item.mjs';
 * const executor = new WorkItemExecutor();
 * const id = await executor.enqueueWorkItem('Process data');
 * const item = await executor.pollWorkItem(id);
 * console.assert(item.status === 'queued');
 */
export class WorkItemExecutor {
  /**
   * Create new work item executor
   * @param {Object} options - Configuration options
   */
  constructor(options = {}) {
    this.store = createStore();
    this.workItems = new Map(); // In-memory cache for fast access
    this.stateTransitionCount = 0;

    // RDF predicates for work items
    this.predicates = {
      GOAL: 'http://kgc.io/goal',
      STATUS: 'http://kgc.io/status',
      CREATED_NS: 'http://kgc.io/created_ns',
      STARTED_NS: 'http://kgc.io/started_ns',
      FINISHED_NS: 'http://kgc.io/finished_ns',
      RECEIPT_LOG: 'http://kgc.io/receipt_log',
      BOUNDS: 'http://kgc.io/bounds',
      PRIORITY: 'http://kgc.io/priority',
    };

    // Named graph for work items
    this.workItemsGraph = 'http://kgc.io/var/kgc/work-items';
  }

  /**
   * Enqueue new work item
   *
   * @param {string} goal - Work item goal/description
   * @param {Object} [bounds] - Execution bounds (timeout, retries, priority, etc.)
   * @returns {Promise<string>} Work item ID
   *
   * @example
   * const id = await executor.enqueueWorkItem('Calculate sum', { priority: 5 });
   */
  async enqueueWorkItem(goal, bounds = {}) {
    // Validate bounds
    BoundsSchema.parse(bounds);

    const workItemId = generateWorkItemId();
    const created_ns = nowNs();

    const workItem = {
      id: workItemId,
      goal,
      status: WORK_ITEM_STATES.QUEUED,
      receipt_log: [],
      created_ns: created_ns.toString(),
      started_ns: null,
      finished_ns: null,
      bounds,
      priority: bounds.priority || 0,
    };

    // Store in memory
    this.workItems.set(workItemId, workItem);

    // Store in RDF triple store
    await this._persistWorkItem(workItem);

    // Count initial state
    this.stateTransitionCount++;

    return workItemId;
  }

  /**
   * Poll work item status
   *
   * @param {string} workItemId - Work item ID
   * @returns {Promise<Object|null>} Work item or null if not found
   *
   * @example
   * const item = await executor.pollWorkItem(id);
   * console.log(item.status); // 'queued'
   */
  async pollWorkItem(workItemId) {
    const workItem = this.workItems.get(workItemId);

    if (!workItem) {
      return null;
    }

    // Return deep copy to prevent external modifications
    return JSON.parse(JSON.stringify(workItem));
  }

  /**
   * Transition work item to new state
   *
   * @param {string} workItemId - Work item ID
   * @param {string} newState - Target state
   * @throws {Error} If transition is invalid
   *
   * @example
   * await executor.transitionWorkItem(id, WORK_ITEM_STATES.RUNNING);
   */
  async transitionWorkItem(workItemId, newState) {
    const workItem = this.workItems.get(workItemId);

    if (!workItem) {
      throw new Error(`Work item not found: ${workItemId}`);
    }

    const currentState = workItem.status;

    // Check if current state is terminal
    if (TERMINAL_STATES.has(currentState)) {
      throw new Error(
        `Cannot transition from terminal state: ${currentState}`
      );
    }

    // Validate state transition
    if (!VALID_TRANSITIONS[currentState]?.includes(newState)) {
      throw new Error(
        `Invalid state transition: ${currentState} -> ${newState}`
      );
    }

    // Update state
    workItem.status = newState;

    // Update timestamps
    if (newState === WORK_ITEM_STATES.RUNNING && !workItem.started_ns) {
      workItem.started_ns = nowNs().toString();
    }

    if (TERMINAL_STATES.has(newState) && !workItem.finished_ns) {
      workItem.finished_ns = nowNs().toString();
    }

    // Persist changes
    await this._persistWorkItem(workItem);

    // Increment transition counter
    this.stateTransitionCount++;
  }

  /**
   * Finalize work item with result and final receipt
   *
   * @param {string} workItemId - Work item ID
   * @param {Object} result - Execution result
   * @param {Object} receipt - Final receipt
   * @returns {Promise<Object>} Updated work item
   *
   * @example
   * const item = await executor.finalizeWorkItem(id, { value: 42 }, { done: true });
   */
  async finalizeWorkItem(workItemId, result, receipt) {
    const workItem = this.workItems.get(workItemId);

    if (!workItem) {
      throw new Error(`Work item not found: ${workItemId}`);
    }

    // Add final receipt
    await this.addReceipt(workItemId, receipt);

    // Transition to succeeded (if currently running)
    if (workItem.status === WORK_ITEM_STATES.RUNNING) {
      await this.transitionWorkItem(workItemId, WORK_ITEM_STATES.SUCCEEDED);
    }

    // Store result
    workItem.result = result;
    await this._persistWorkItem(workItem);

    return this.pollWorkItem(workItemId);
  }

  /**
   * Add receipt to work item's append-only log
   *
   * @param {string} workItemId - Work item ID
   * @param {Object} receipt - Receipt data
   *
   * @example
   * await executor.addReceipt(id, { step: 'validation', status: 'ok' });
   */
  async addReceipt(workItemId, receipt) {
    const workItem = this.workItems.get(workItemId);

    if (!workItem) {
      throw new Error(`Work item not found: ${workItemId}`);
    }

    // Deep copy receipt to ensure immutability
    const receiptCopy = JSON.parse(JSON.stringify(receipt));

    // Append to log
    workItem.receipt_log.push(receiptCopy);

    // Persist changes
    await this._persistWorkItem(workItem);
  }

  /**
   * Get deterministic execution order
   * Returns pending/queued items ordered by priority (desc) then creation time (asc)
   *
   * @returns {string[]} Ordered array of work item IDs
   *
   * @example
   * const order = executor.getExecutionOrder();
   * console.log(order); // ['work-item-1', 'work-item-2', ...]
   */
  getExecutionOrder() {
    const eligibleItems = Array.from(this.workItems.values())
      .filter(item => item.status === WORK_ITEM_STATES.QUEUED);

    // Sort by priority (descending) then by creation time (ascending)
    eligibleItems.sort((a, b) => {
      // Higher priority first
      if (a.priority !== b.priority) {
        return b.priority - a.priority;
      }
      // Earlier creation time first (FIFO for same priority)
      return BigInt(a.created_ns) < BigInt(b.created_ns) ? -1 : 1;
    });

    return eligibleItems.map(item => item.id);
  }

  /**
   * Get total state transition count
   *
   * @returns {number} Number of state transitions
   */
  getStateTransitionCount() {
    return this.stateTransitionCount;
  }

  /**
   * Query work items using SPARQL
   *
   * @param {string} sparql - SPARQL query
   * @returns {Promise<Array>} Query results
   */
  async queryWorkItems(sparql) {
    return this.store.query(sparql);
  }

  // ===== Private Methods =====

  /**
   * Persist work item to RDF triple store
   * @private
   */
  async _persistWorkItem(workItem) {
    const subject = dataFactory.namedNode(
      `http://kgc.io/var/kgc/work-items/${workItem.id}`
    );
    const graph = dataFactory.namedNode(this.workItemsGraph);

    // Remove existing quads for this work item
    const existingQuads = [];
    for (const quad of this.store.match(subject, null, null, graph)) {
      existingQuads.push(quad);
    }
    for (const quad of existingQuads) {
      this.store.delete(quad);
    }

    // Add updated quads
    const quads = [
      dataFactory.quad(
        subject,
        dataFactory.namedNode(this.predicates.GOAL),
        dataFactory.literal(workItem.goal),
        graph
      ),
      dataFactory.quad(
        subject,
        dataFactory.namedNode(this.predicates.STATUS),
        dataFactory.literal(workItem.status),
        graph
      ),
      dataFactory.quad(
        subject,
        dataFactory.namedNode(this.predicates.CREATED_NS),
        dataFactory.literal(
          workItem.created_ns,
          dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
        ),
        graph
      ),
    ];

    if (workItem.started_ns) {
      quads.push(
        dataFactory.quad(
          subject,
          dataFactory.namedNode(this.predicates.STARTED_NS),
          dataFactory.literal(
            workItem.started_ns,
            dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
          ),
          graph
        )
      );
    }

    if (workItem.finished_ns) {
      quads.push(
        dataFactory.quad(
          subject,
          dataFactory.namedNode(this.predicates.FINISHED_NS),
          dataFactory.literal(
            workItem.finished_ns,
            dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
          ),
          graph
        )
      );
    }

    if (workItem.receipt_log.length > 0) {
      quads.push(
        dataFactory.quad(
          subject,
          dataFactory.namedNode(this.predicates.RECEIPT_LOG),
          dataFactory.literal(JSON.stringify(workItem.receipt_log)),
          graph
        )
      );
    }

    if (workItem.bounds) {
      quads.push(
        dataFactory.quad(
          subject,
          dataFactory.namedNode(this.predicates.BOUNDS),
          dataFactory.literal(JSON.stringify(workItem.bounds)),
          graph
        )
      );
    }

    quads.push(
      dataFactory.quad(
        subject,
        dataFactory.namedNode(this.predicates.PRIORITY),
        dataFactory.literal(
          workItem.priority.toString(),
          dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
        ),
        graph
      )
    );

    // Add all quads to store
    for (const quad of quads) {
      this.store.add(quad);
    }
  }
}
