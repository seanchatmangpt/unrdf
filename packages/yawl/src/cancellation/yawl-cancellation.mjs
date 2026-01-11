/**
 * @file YAWL Cancellation Semantics Implementation
 * @module yawl/cancellation
 *
 * @description
 * Implements YAWL-style cancellation semantics using circuit breaker pattern
 * and EffectSandbox timeouts for reliable task abortion. Provides:
 *
 * - Cancellation regions: Groups of tasks that cancel together
 * - Work item cancellation with dependent task hooks
 * - Timeout enforcement via EffectSandbox
 * - Circuit breaker integration for cascading failure prevention
 * - Full auditability through receipt logging
 * - Time-travel support via receipt history
 *
 * @see https://www.yamlworkflow.net/
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for cancellation reason
 */
const CancellationReasonSchema = z.enum([
  'timeout',
  'manual',
  'circuit_breaker',
  'parent_cancelled',
  'region_cancelled',
  'task_disabled',
  'dependency_failed',
  'workflow_terminated',
]);

/**
 * Schema for work item state
 */
const WorkItemStateSchema = z.enum([
  'pending',
  'enabled',
  'executing',
  'completed',
  'cancelled',
  'failed',
]);

/**
 * Schema for work item
 */
const WorkItemSchema = z.object({
  id: z.string().uuid(),
  taskId: z.string().min(1),
  caseId: z.string().uuid(),
  regionId: z.string().uuid().optional(),
  state: WorkItemStateSchema,
  createdAt: z.coerce.date(),
  startedAt: z.coerce.date().optional(),
  completedAt: z.coerce.date().optional(),
  cancelledAt: z.coerce.date().optional(),
  cancellationReason: CancellationReasonSchema.optional(),
  timeoutMs: z.number().int().positive().max(300000).default(30000),
  retryCount: z.number().int().nonnegative().default(0),
  metadata: z.record(z.any()).optional(),
});

/**
 * Schema for cancellation region
 */
const CancellationRegionSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  taskIds: z.array(z.string().min(1)),
  parentRegionId: z.string().uuid().optional(),
  childRegionIds: z.array(z.string().uuid()).default([]),
  createdAt: z.coerce.date(),
  active: z.boolean().default(true),
});

/**
 * Valid receipt types
 * @type {readonly string[]}
 */
const VALID_RECEIPT_TYPES = Object.freeze([
  'CANCELLED_WORK_ITEM',
  'TIMEOUT_OCCURRED',
  'CIRCUIT_BREAKER_OPEN',
  'CIRCUIT_BREAKER_CLOSED',
  'REGION_CANCELLED',
  'TASK_DISABLED',
  'TASK_ENABLED',
  'CANCELLATION_PROPAGATED',
]);

/**
 * Schema for cancellation receipt (for export validation)
 */
const CancellationReceiptSchema = z.object({
  id: z.string(),
  type: z.string(),
  timestamp: z.any(),
  payload: z.record(z.any()),
});

/**
 * Create a validated receipt object
 * @param {string} type
 * @param {Object} payload
 * @returns {Object}
 */
function createReceipt(type, payload) {
  if (!VALID_RECEIPT_TYPES.includes(type)) {
    throw new Error(`Invalid receipt type: ${type}`);
  }
  return {
    id: randomUUID(),
    type,
    timestamp: new Date(),
    payload,
  };
}

/**
 * Schema for circuit breaker state
 */
const CircuitBreakerStateSchema = z.enum(['closed', 'open', 'half_open']);

/**
 * @typedef {z.infer<typeof WorkItemSchema>} WorkItem
 * @typedef {z.infer<typeof CancellationRegionSchema>} CancellationRegion
 * @typedef {z.infer<typeof CancellationReceiptSchema>} CancellationReceipt
 * @typedef {z.infer<typeof CancellationReasonSchema>} CancellationReason
 */

// ============================================================================
// CIRCUIT BREAKER
// ============================================================================

/**
 * Circuit breaker for task-level failure management
 * Prevents cascading failures by disabling tasks after consecutive failures
 */
export class TaskCircuitBreaker {
  /**
   * @param {Object} config
   * @param {number} [config.failureThreshold=3] - Consecutive failures before opening
   * @param {number} [config.resetTimeout=60000] - Time before half-open state
   * @param {number} [config.halfOpenMaxCalls=1] - Max calls in half-open state
   * @param {Function} [config.onStateChange] - Callback on state transitions
   */
  constructor(config = {}) {
    this.taskId = config.taskId || 'unknown';
    this.failureThreshold = config.failureThreshold ?? 3;
    this.resetTimeout = config.resetTimeout ?? 60000;
    this.halfOpenMaxCalls = config.halfOpenMaxCalls ?? 1;
    this.onStateChange = config.onStateChange ?? null;

    this.state = 'closed';
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.lastFailureTime = null;
    this.lastStateChange = Date.now();
    this.disabledAt = null;
  }

  /**
   * Record a successful task execution
   */
  recordSuccess() {
    if (this.state === 'half_open') {
      this.successCount++;
      if (this.successCount >= 1) {
        this._transition('closed');
        this.failureCount = 0;
        this.successCount = 0;
        this.halfOpenCalls = 0;
      }
    } else if (this.state === 'closed') {
      this.failureCount = 0;
    }
  }

  /**
   * Record a task failure
   * @returns {boolean} True if circuit tripped
   */
  recordFailure() {
    this.failureCount++;
    this.lastFailureTime = Date.now();

    if (this.state === 'half_open') {
      this._transition('open');
      return true;
    }

    if (this.state === 'closed' && this.failureCount >= this.failureThreshold) {
      this._transition('open');
      this.disabledAt = new Date();
      return true;
    }

    return false;
  }

  /**
   * Check if circuit allows execution
   * @returns {boolean} True if execution allowed
   */
  allowExecution() {
    this._checkTransition();

    if (this.state === 'open') {
      return false;
    }

    if (this.state === 'half_open') {
      if (this.halfOpenCalls >= this.halfOpenMaxCalls) {
        return false;
      }
      this.halfOpenCalls++;
    }

    return true;
  }

  /**
   * Manually reset the circuit breaker (enable task)
   */
  reset() {
    this._transition('closed');
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.disabledAt = null;
  }

  /**
   * Get current circuit state
   * @returns {Object} State information
   */
  getState() {
    return {
      taskId: this.taskId,
      state: this.state,
      failureCount: this.failureCount,
      failureThreshold: this.failureThreshold,
      lastFailureTime: this.lastFailureTime,
      lastStateChange: this.lastStateChange,
      disabledAt: this.disabledAt,
      isOpen: this.state === 'open',
    };
  }

  /**
   * Check if state transition should occur
   * @private
   */
  _checkTransition() {
    if (this.state === 'open') {
      const elapsed = Date.now() - this.lastStateChange;
      if (elapsed >= this.resetTimeout) {
        this._transition('half_open');
      }
    }
  }

  /**
   * Transition to a new state
   * @param {string} newState
   * @private
   */
  _transition(newState) {
    const previousState = this.state;
    this.state = newState;
    this.lastStateChange = Date.now();

    if (this.onStateChange) {
      try {
        this.onStateChange({
          taskId: this.taskId,
          from: previousState,
          to: newState,
          timestamp: new Date(),
          failureCount: this.failureCount,
        });
      } catch {
        // Ignore callback errors
      }
    }
  }
}

// ============================================================================
// CANCELLATION REGION MANAGER
// ============================================================================

/**
 * Manages cancellation regions - groups of tasks that cancel together
 */
export class CancellationRegionManager {
  /**
   *
   */
  constructor() {
    /** @type {Map<string, CancellationRegion>} */
    this.regions = new Map();
    /** @type {Map<string, Set<string>>} Task ID -> Region IDs */
    this.taskToRegions = new Map();
  }

  /**
   * Create a new cancellation region
   * @param {Object} options
   * @param {string} options.name - Region name
   * @param {string[]} options.taskIds - Task IDs in region
   * @param {string} [options.parentRegionId] - Parent region for nesting
   * @returns {CancellationRegion}
   */
  createRegion(options) {
    this._validateRegionOptions(options);

    const region = {
      id: randomUUID(),
      name: options.name,
      taskIds: options.taskIds,
      parentRegionId: options.parentRegionId,
      childRegionIds: [],
      createdAt: new Date(),
      active: true,
    };

    this._registerRegion(region);
    this._linkToParent(region.id, options.parentRegionId);

    return region;
  }

  /**
   * Validate region creation options
   * @param {Object} options
   * @private
   */
  _validateRegionOptions(options) {
    if (!options.name || typeof options.name !== 'string') {
      throw new Error('Region name is required and must be a string');
    }
    if (!Array.isArray(options.taskIds) || options.taskIds.length === 0) {
      throw new Error('taskIds must be a non-empty array');
    }
  }

  /**
   * Register region and map tasks
   * @param {CancellationRegion} region
   * @private
   */
  _registerRegion(region) {
    this.regions.set(region.id, region);

    for (const taskId of region.taskIds) {
      if (!this.taskToRegions.has(taskId)) {
        this.taskToRegions.set(taskId, new Set());
      }
      this.taskToRegions.get(taskId).add(region.id);
    }
  }

  /**
   * Link region to parent
   * @param {string} regionId
   * @param {string} [parentRegionId]
   * @private
   */
  _linkToParent(regionId, parentRegionId) {
    if (parentRegionId) {
      const parent = this.regions.get(parentRegionId);
      if (parent) {
        parent.childRegionIds.push(regionId);
      }
    }
  }

  /**
   * Get region by ID
   * @param {string} regionId
   * @returns {CancellationRegion|undefined}
   */
  getRegion(regionId) {
    return this.regions.get(regionId);
  }

  /**
   * Get all regions containing a task
   * @param {string} taskId
   * @returns {CancellationRegion[]}
   */
  getRegionsForTask(taskId) {
    const regionIds = this.taskToRegions.get(taskId);
    if (!regionIds) return [];

    return Array.from(regionIds)
      .map(id => this.regions.get(id))
      .filter(r => r !== undefined);
  }

  /**
   * Get all sibling tasks in regions containing a task
   * @param {string} taskId
   * @returns {string[]} Array of sibling task IDs (excluding input task)
   */
  getSiblingTasks(taskId) {
    const siblings = new Set();
    const regions = this.getRegionsForTask(taskId);

    for (const region of regions) {
      for (const tid of region.taskIds) {
        if (tid !== taskId) {
          siblings.add(tid);
        }
      }
    }

    return Array.from(siblings);
  }

  /**
   * Get all descendant regions (nested)
   * @param {string} regionId
   * @returns {CancellationRegion[]}
   */
  getDescendantRegions(regionId) {
    const descendants = [];
    const region = this.regions.get(regionId);
    if (!region) return descendants;

    const queue = [...region.childRegionIds];
    while (queue.length > 0) {
      const childId = queue.shift();
      const child = this.regions.get(childId);
      if (child) {
        descendants.push(child);
        queue.push(...child.childRegionIds);
      }
    }

    return descendants;
  }

  /**
   * Deactivate a region and all descendants
   * @param {string} regionId
   * @returns {string[]} IDs of all deactivated regions
   */
  deactivateRegion(regionId) {
    const deactivated = [];
    const region = this.regions.get(regionId);
    if (!region) return deactivated;

    region.active = false;
    deactivated.push(regionId);

    const descendants = this.getDescendantRegions(regionId);
    for (const desc of descendants) {
      desc.active = false;
      deactivated.push(desc.id);
    }

    return deactivated;
  }

  /**
   * Get all tasks in a region and its descendants
   * @param {string} regionId
   * @returns {string[]} All task IDs
   */
  getAllTasksInRegion(regionId) {
    const tasks = new Set();
    const region = this.regions.get(regionId);
    if (!region) return [];

    for (const taskId of region.taskIds) {
      tasks.add(taskId);
    }

    const descendants = this.getDescendantRegions(regionId);
    for (const desc of descendants) {
      for (const taskId of desc.taskIds) {
        tasks.add(taskId);
      }
    }

    return Array.from(tasks);
  }

  /**
   * Export regions for serialization
   * @returns {Object}
   */
  export() {
    return {
      regions: Array.from(this.regions.values()),
      taskMappings: Object.fromEntries(
        Array.from(this.taskToRegions.entries()).map(([k, v]) => [k, Array.from(v)])
      ),
    };
  }

  /**
   * Import regions from serialized data
   * @param {Object} data
   */
  import(data) {
    this.regions.clear();
    this.taskToRegions.clear();

    this._importRegions(data.regions || []);
    this._importTaskMappings(data.taskMappings || {});
  }

  /**
   * Import regions array
   * @param {Array} regions
   * @private
   */
  _importRegions(regions) {
    for (const region of regions) {
      const parsed = {
        id: region.id,
        name: region.name,
        taskIds: region.taskIds,
        parentRegionId: region.parentRegionId,
        childRegionIds: region.childRegionIds || [],
        createdAt: new Date(region.createdAt),
        active: region.active !== false,
      };
      this.regions.set(parsed.id, parsed);
    }
  }

  /**
   * Import task mappings
   * @param {Object} taskMappings
   * @private
   */
  _importTaskMappings(taskMappings) {
    for (const [taskId, regionIds] of Object.entries(taskMappings)) {
      this.taskToRegions.set(taskId, new Set(regionIds));
    }
  }
}

// ============================================================================
// RECEIPT LOGGER
// ============================================================================

/**
 * Logs cancellation receipts for auditability and time-travel
 */
export class CancellationReceiptLogger {
  /**
   *
   */
  constructor() {
    /** @type {CancellationReceipt[]} */
    this.receipts = [];
    /** @type {Map<string, CancellationReceipt[]>} Work item ID -> receipts */
    this.receiptsByWorkItem = new Map();
    /** @type {Map<string, CancellationReceipt[]>} Task ID -> receipts */
    this.receiptsByTask = new Map();
  }

  /**
   * Log a cancellation receipt
   * @param {string} type - Receipt type
   * @param {Object} payload - Receipt payload
   * @returns {CancellationReceipt}
   */
  log(type, payload) {
    const receipt = createReceipt(type, payload);
    this.receipts.push(receipt);
    this._indexReceipt(receipt, payload);
    return receipt;
  }

  /**
   * Index receipt by work item and task
   * @param {CancellationReceipt} receipt
   * @param {Object} payload
   * @private
   */
  _indexReceipt(receipt, payload) {
    if (payload.workItemId) {
      if (!this.receiptsByWorkItem.has(payload.workItemId)) {
        this.receiptsByWorkItem.set(payload.workItemId, []);
      }
      this.receiptsByWorkItem.get(payload.workItemId).push(receipt);
    }

    if (payload.taskId) {
      if (!this.receiptsByTask.has(payload.taskId)) {
        this.receiptsByTask.set(payload.taskId, []);
      }
      this.receiptsByTask.get(payload.taskId).push(receipt);
    }
  }

  /**
   * Log CANCELLED_WORK_ITEM receipt
   * @param {string} workItemId
   * @param {string} reason
   * @param {string} [regionId]
   * @returns {CancellationReceipt}
   */
  logCancelledWorkItem(workItemId, reason, regionId) {
    return this.log('CANCELLED_WORK_ITEM', {
      workItemId,
      reason,
      cancelledAt: new Date().toISOString(),
      region: regionId,
    });
  }

  /**
   * Log TIMEOUT_OCCURRED receipt
   * @param {string} workItemId
   * @param {string} taskId
   * @param {number} durationMs
   * @param {number} timeoutMs
   * @returns {CancellationReceipt}
   */
  logTimeoutOccurred(workItemId, taskId, durationMs, timeoutMs) {
    return this.log('TIMEOUT_OCCURRED', {
      workItemId,
      taskId,
      durationMs,
      timeoutMs,
      occurredAt: new Date().toISOString(),
    });
  }

  /**
   * Log CIRCUIT_BREAKER_OPEN receipt
   * @param {string} taskId
   * @param {number} failureCount
   * @returns {CancellationReceipt}
   */
  logCircuitBreakerOpen(taskId, failureCount) {
    return this.log('CIRCUIT_BREAKER_OPEN', {
      taskId,
      failureCount,
      disabledAt: new Date().toISOString(),
    });
  }

  /**
   * Log CIRCUIT_BREAKER_CLOSED receipt
   * @param {string} taskId
   * @returns {CancellationReceipt}
   */
  logCircuitBreakerClosed(taskId) {
    return this.log('CIRCUIT_BREAKER_CLOSED', {
      taskId,
      enabledAt: new Date().toISOString(),
    });
  }

  /**
   * Log REGION_CANCELLED receipt
   * @param {string} regionId
   * @param {string} reason
   * @param {string[]} affectedWorkItems
   * @returns {CancellationReceipt}
   */
  logRegionCancelled(regionId, reason, affectedWorkItems) {
    return this.log('REGION_CANCELLED', {
      regionId,
      reason,
      affectedWorkItems,
      cancelledAt: new Date().toISOString(),
    });
  }

  /**
   * Log CANCELLATION_PROPAGATED receipt
   * @param {string} sourceWorkItemId
   * @param {string[]} propagatedTo
   * @param {string} reason
   * @returns {CancellationReceipt}
   */
  logCancellationPropagated(sourceWorkItemId, propagatedTo, reason) {
    return this.log('CANCELLATION_PROPAGATED', {
      sourceWorkItemId,
      propagatedTo,
      reason,
      propagatedAt: new Date().toISOString(),
    });
  }

  /**
   * Get receipts for a work item
   * @param {string} workItemId
   * @returns {CancellationReceipt[]}
   */
  getReceiptsForWorkItem(workItemId) {
    return this.receiptsByWorkItem.get(workItemId) || [];
  }

  /**
   * Get receipts for a task
   * @param {string} taskId
   * @returns {CancellationReceipt[]}
   */
  getReceiptsForTask(taskId) {
    return this.receiptsByTask.get(taskId) || [];
  }

  /**
   * Get all receipts
   * @returns {CancellationReceipt[]}
   */
  getAllReceipts() {
    return [...this.receipts];
  }

  /**
   * Get receipts at a specific point in time
   * @param {Date} timestamp
   * @returns {CancellationReceipt[]}
   */
  getReceiptsAtTime(timestamp) {
    const targetTime = timestamp.getTime();
    return this.receipts.filter(r => new Date(r.timestamp).getTime() <= targetTime);
  }

  /**
   * Get receipts in a time range
   * @param {Date} from
   * @param {Date} to
   * @returns {CancellationReceipt[]}
   */
  getReceiptsInRange(from, to) {
    const fromTime = from.getTime();
    const toTime = to.getTime();
    return this.receipts.filter(r => {
      const time = new Date(r.timestamp).getTime();
      return time >= fromTime && time <= toTime;
    });
  }

  /**
   * Export receipts for serialization
   * @returns {CancellationReceipt[]}
   */
  export() {
    return this.receipts.map(r => ({
      ...r,
      timestamp: r.timestamp.toISOString(),
    }));
  }

  /**
   * Import receipts from serialized data
   * @param {Object[]} data
   */
  import(data) {
    this.receipts = [];
    this.receiptsByWorkItem.clear();
    this.receiptsByTask.clear();

    for (const receipt of data) {
      this._importReceipt(receipt);
    }
  }

  /**
   * Import single receipt
   * @param {Object} receipt
   * @private
   */
  _importReceipt(receipt) {
    const parsed = {
      id: receipt.id,
      type: receipt.type,
      timestamp: new Date(receipt.timestamp),
      payload: receipt.payload,
    };
    this.receipts.push(parsed);

    if (parsed.payload.workItemId) {
      if (!this.receiptsByWorkItem.has(parsed.payload.workItemId)) {
        this.receiptsByWorkItem.set(parsed.payload.workItemId, []);
      }
      this.receiptsByWorkItem.get(parsed.payload.workItemId).push(parsed);
    }

    if (parsed.payload.taskId) {
      if (!this.receiptsByTask.has(parsed.payload.taskId)) {
        this.receiptsByTask.set(parsed.payload.taskId, []);
      }
      this.receiptsByTask.get(parsed.payload.taskId).push(parsed);
    }
  }

  /**
   * Clear all receipts
   */
  clear() {
    this.receipts = [];
    this.receiptsByWorkItem.clear();
    this.receiptsByTask.clear();
  }
}

// ============================================================================
// YAWL CANCELLATION MANAGER
// ============================================================================

/**
 * @typedef {Object} CancellationConfig
 * @property {number} [defaultTimeout=30000] - Default timeout in ms
 * @property {number} [maxTimeout=300000] - Maximum timeout in ms
 * @property {number} [circuitBreakerThreshold=3] - Failures before circuit opens
 * @property {number} [circuitBreakerReset=60000] - Time before half-open
 * @property {Function} [onCancellation] - Callback when work item cancelled
 * @property {Function} [onTimeout] - Callback when timeout occurs
 * @property {Function} [onCircuitOpen] - Callback when circuit breaker opens
 */

/**
 * Main YAWL Cancellation Manager
 * Coordinates cancellation regions, circuit breakers, and timeout enforcement
 */
export class YawlCancellationManager {
  /**
   * @param {CancellationConfig} [config]
   */
  constructor(config = {}) {
    this.config = {
      defaultTimeout: config.defaultTimeout ?? 30000,
      maxTimeout: config.maxTimeout ?? 300000,
      circuitBreakerThreshold: config.circuitBreakerThreshold ?? 3,
      circuitBreakerReset: config.circuitBreakerReset ?? 60000,
      onCancellation: config.onCancellation ?? null,
      onTimeout: config.onTimeout ?? null,
      onCircuitOpen: config.onCircuitOpen ?? null,
    };

    /** @type {Map<string, WorkItem>} */
    this.workItems = new Map();

    /** @type {Map<string, TaskCircuitBreaker>} Task ID -> circuit breaker */
    this.circuitBreakers = new Map();

    /** @type {Map<string, Set<string>>} Task ID -> Work item IDs */
    this.workItemsByTask = new Map();

    /** @type {Map<string, Set<string>>} Case ID -> Work item IDs */
    this.workItemsByCase = new Map();

    /** @type {Map<string, NodeJS.Timeout>} Work item ID -> timeout handle */
    this.timeoutHandles = new Map();

    /** @type {Map<string, string[]>} Task ID -> downstream task IDs */
    this.taskDependencies = new Map();

    this.regionManager = new CancellationRegionManager();
    this.receiptLogger = new CancellationReceiptLogger();

    /** @type {Function[]} Cancellation hooks */
    this.cancellationHooks = [];
  }

  // --------------------------------------------------------------------------
  // WORK ITEM MANAGEMENT
  // --------------------------------------------------------------------------

  /**
   * Create a new work item
   * @param {Object} options
   * @param {string} options.taskId
   * @param {string} options.caseId
   * @param {string} [options.regionId]
   * @param {number} [options.timeoutMs]
   * @param {Object} [options.metadata]
   * @returns {WorkItem}
   */
  createWorkItem(options) {
    this._validateWorkItemOptions(options);

    const workItem = this._buildWorkItem(options);
    this.workItems.set(workItem.id, workItem);
    this._indexWorkItem(workItem);

    return workItem;
  }

  /**
   * Validate work item creation options
   * @param {Object} options
   * @private
   */
  _validateWorkItemOptions(options) {
    if (!options.taskId || typeof options.taskId !== 'string') {
      throw new Error('taskId is required and must be a string');
    }
    if (!options.caseId || typeof options.caseId !== 'string') {
      throw new Error('caseId is required and must be a string');
    }
  }

  /**
   * Build work item object
   * @param {Object} options
   * @returns {WorkItem}
   * @private
   */
  _buildWorkItem(options) {
    const timeoutMs = Math.min(
      options.timeoutMs ?? this.config.defaultTimeout,
      this.config.maxTimeout
    );

    return {
      id: randomUUID(),
      taskId: options.taskId,
      caseId: options.caseId,
      regionId: options.regionId,
      state: 'pending',
      createdAt: new Date(),
      startedAt: undefined,
      completedAt: undefined,
      cancelledAt: undefined,
      cancellationReason: undefined,
      timeoutMs,
      retryCount: 0,
      metadata: options.metadata,
    };
  }

  /**
   * Index work item by task and case
   * @param {WorkItem} workItem
   * @private
   */
  _indexWorkItem(workItem) {
    if (!this.workItemsByTask.has(workItem.taskId)) {
      this.workItemsByTask.set(workItem.taskId, new Set());
    }
    this.workItemsByTask.get(workItem.taskId).add(workItem.id);

    if (!this.workItemsByCase.has(workItem.caseId)) {
      this.workItemsByCase.set(workItem.caseId, new Set());
    }
    this.workItemsByCase.get(workItem.caseId).add(workItem.id);
  }

  /**
   * Enable a work item and start timeout enforcement
   * @param {string} workItemId
   * @returns {WorkItem|null}
   */
  enableWorkItem(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'pending') return null;

    const breaker = this._getOrCreateCircuitBreaker(workItem.taskId);
    if (!breaker.allowExecution()) {
      this.cancelWorkItem(workItemId, 'task_disabled');
      return null;
    }

    workItem.state = 'enabled';
    return workItem;
  }

  /**
   * Start executing a work item with timeout enforcement
   * @param {string} workItemId
   * @returns {WorkItem|null}
   */
  startExecution(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'enabled') return null;

    workItem.state = 'executing';
    workItem.startedAt = new Date();
    this._startTimeoutEnforcement(workItemId);

    return workItem;
  }

  /**
   * Complete a work item successfully
   * @param {string} workItemId
   * @returns {WorkItem|null}
   */
  completeWorkItem(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'executing') return null;

    this._clearTimeout(workItemId);

    const breaker = this.circuitBreakers.get(workItem.taskId);
    if (breaker) {
      breaker.recordSuccess();
    }

    workItem.state = 'completed';
    workItem.completedAt = new Date();

    return workItem;
  }

  /**
   * Record a work item failure (for circuit breaker)
   * @param {string} workItemId
   * @returns {Object} Result with circuitOpened flag
   */
  recordFailure(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) return { circuitOpened: false };

    this._clearTimeout(workItemId);
    workItem.state = 'failed';
    workItem.completedAt = new Date();

    const breaker = this._getOrCreateCircuitBreaker(workItem.taskId);
    const circuitOpened = breaker.recordFailure();

    if (circuitOpened) {
      this._handleCircuitOpen(workItem.taskId, breaker);
    }

    return { circuitOpened, breakerState: breaker.getState() };
  }

  /**
   * Handle circuit breaker opening
   * @param {string} taskId
   * @param {TaskCircuitBreaker} breaker
   * @private
   */
  _handleCircuitOpen(taskId, breaker) {
    this.receiptLogger.logCircuitBreakerOpen(taskId, breaker.failureCount);
    this._cancelAllWorkItemsForTask(taskId, 'circuit_breaker');
    this._invokeCircuitOpenCallback(taskId, breaker);
  }

  /**
   * Invoke circuit open callback
   * @param {string} taskId
   * @param {TaskCircuitBreaker} breaker
   * @private
   */
  _invokeCircuitOpenCallback(taskId, breaker) {
    if (this.config.onCircuitOpen) {
      try {
        this.config.onCircuitOpen({
          taskId,
          failureCount: breaker.failureCount,
          timestamp: new Date(),
        });
      } catch {
        // Ignore callback errors
      }
    }
  }

  // --------------------------------------------------------------------------
  // CANCELLATION
  // --------------------------------------------------------------------------

  /**
   * Cancel a work item
   * @param {string} workItemId
   * @param {CancellationReason} reason
   * @returns {Object} Result with cancelled work items
   */
  cancelWorkItem(workItemId, reason) {
    const validation = this._validateCancellation(workItemId);
    if (!validation.valid) {
      return { success: false, cancelled: [], reason: validation.reason };
    }

    const workItem = validation.workItem;
    this._updateWorkItemCancelled(workItem, reason);
    this._notifyCancellation(workItem, reason);

    const cancelled = [workItemId];
    const regionCancelled = this._handleRegionCancellation(workItem, workItemId, reason);
    cancelled.push(...regionCancelled);

    const propagated = this._propagateCancellation(workItem.taskId, reason);
    cancelled.push(...propagated);

    if (propagated.length > 0) {
      this.receiptLogger.logCancellationPropagated(workItemId, propagated, reason);
    }

    return { success: true, cancelled, reason };
  }

  /**
   * Validate cancellation request
   * @param {string} workItemId
   * @returns {Object}
   * @private
   */
  _validateCancellation(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) {
      return { valid: false, reason: 'work_item_not_found' };
    }

    if (workItem.state === 'cancelled' || workItem.state === 'completed') {
      return { valid: false, reason: 'already_terminal' };
    }

    return { valid: true, workItem };
  }

  /**
   * Update work item to cancelled state
   * @param {WorkItem} workItem
   * @param {string} reason
   * @private
   */
  _updateWorkItemCancelled(workItem, reason) {
    this._clearTimeout(workItem.id);
    workItem.state = 'cancelled';
    workItem.cancelledAt = new Date();
    workItem.cancellationReason = reason;
  }

  /**
   * Notify about cancellation via logs and callbacks
   * @param {WorkItem} workItem
   * @param {string} reason
   * @private
   */
  _notifyCancellation(workItem, reason) {
    this.receiptLogger.logCancelledWorkItem(workItem.id, reason, workItem.regionId);
    this._invokeCancellationHooks(workItem, reason);
    this._invokeCancellationCallback(workItem, reason);
  }

  /**
   * Invoke cancellation callback
   * @param {WorkItem} workItem
   * @param {string} reason
   * @private
   */
  _invokeCancellationCallback(workItem, reason) {
    if (this.config.onCancellation) {
      try {
        this.config.onCancellation({
          workItemId: workItem.id,
          taskId: workItem.taskId,
          reason,
          timestamp: new Date(),
        });
      } catch {
        // Ignore callback errors
      }
    }
  }

  /**
   * Handle region cancellation if applicable
   * @param {WorkItem} workItem
   * @param {string} sourceWorkItemId
   * @param {string} reason
   * @returns {string[]}
   * @private
   */
  _handleRegionCancellation(workItem, sourceWorkItemId, reason) {
    if (workItem.regionId && reason !== 'region_cancelled') {
      return this._cancelRegion(workItem.regionId, sourceWorkItemId, reason);
    }
    return [];
  }

  /**
   * Cancel all work items in a region
   * @param {string} regionId
   * @param {string} [sourceWorkItemId] - Work item that triggered cancellation
   * @param {string} [reason]
   * @returns {string[]} Cancelled work item IDs
   * @private
   */
  _cancelRegion(regionId, sourceWorkItemId, reason = 'region_cancelled') {
    const cancelled = [];
    const region = this.regionManager.getRegion(regionId);
    if (!region || !region.active) return cancelled;

    const tasksInRegion = this.regionManager.getAllTasksInRegion(regionId);
    this._cancelTasksInRegion(tasksInRegion, sourceWorkItemId, regionId, cancelled);
    this._deactivateRegionAndLog(regionId, reason, cancelled);

    return cancelled;
  }

  /**
   * Cancel work items for tasks in region
   * @param {string[]} taskIds
   * @param {string} sourceWorkItemId
   * @param {string} regionId
   * @param {string[]} cancelled - Output array
   * @private
   */
  _cancelTasksInRegion(taskIds, sourceWorkItemId, regionId, cancelled) {
    for (const taskId of taskIds) {
      const workItemIds = this.workItemsByTask.get(taskId);
      if (!workItemIds) continue;

      for (const wiId of workItemIds) {
        if (wiId === sourceWorkItemId) continue;
        this._cancelRegionWorkItem(wiId, regionId, cancelled);
      }
    }
  }

  /**
   * Cancel individual work item in region
   * @param {string} workItemId
   * @param {string} regionId
   * @param {string[]} cancelled - Output array
   * @private
   */
  _cancelRegionWorkItem(workItemId, regionId, cancelled) {
    const wi = this.workItems.get(workItemId);
    if (wi && wi.state !== 'cancelled' && wi.state !== 'completed') {
      this._clearTimeout(workItemId);
      wi.state = 'cancelled';
      wi.cancelledAt = new Date();
      wi.cancellationReason = 'region_cancelled';
      this.receiptLogger.logCancelledWorkItem(workItemId, 'region_cancelled', regionId);
      this._invokeCancellationHooks(wi, 'region_cancelled');
      cancelled.push(workItemId);
    }
  }

  /**
   * Deactivate region and log
   * @param {string} regionId
   * @param {string} reason
   * @param {string[]} cancelled
   * @private
   */
  _deactivateRegionAndLog(regionId, reason, cancelled) {
    this.regionManager.deactivateRegion(regionId);
    this.receiptLogger.logRegionCancelled(regionId, reason, cancelled);
  }

  /**
   * Propagate cancellation to downstream dependent tasks
   * @param {string} taskId
   * @param {string} reason
   * @returns {string[]} Cancelled work item IDs
   * @private
   */
  _propagateCancellation(taskId, reason) {
    const cancelled = [];
    const downstreamTasks = this.taskDependencies.get(taskId);
    if (!downstreamTasks) return cancelled;

    for (const downstreamTaskId of downstreamTasks) {
      this._cancelDownstreamWorkItems(downstreamTaskId, cancelled);
    }

    return cancelled;
  }

  /**
   * Cancel work items for downstream task
   * @param {string} taskId
   * @param {string[]} cancelled - Output array
   * @private
   */
  _cancelDownstreamWorkItems(taskId, cancelled) {
    const workItemIds = this.workItemsByTask.get(taskId);
    if (!workItemIds) return;

    for (const wiId of workItemIds) {
      const wi = this.workItems.get(wiId);
      if (wi && (wi.state === 'pending' || wi.state === 'enabled')) {
        this._clearTimeout(wiId);
        wi.state = 'cancelled';
        wi.cancelledAt = new Date();
        wi.cancellationReason = 'dependency_failed';
        this.receiptLogger.logCancelledWorkItem(wiId, 'dependency_failed', wi.regionId);
        this._invokeCancellationHooks(wi, 'dependency_failed');
        cancelled.push(wiId);
      }
    }
  }

  /**
   * Cancel all work items for a task
   * @param {string} taskId
   * @param {string} reason
   * @private
   */
  _cancelAllWorkItemsForTask(taskId, reason) {
    const workItemIds = this.workItemsByTask.get(taskId);
    if (!workItemIds) return;

    for (const wiId of workItemIds) {
      const wi = this.workItems.get(wiId);
      if (wi && wi.state !== 'cancelled' && wi.state !== 'completed') {
        this.cancelWorkItem(wiId, reason);
      }
    }
  }

  // --------------------------------------------------------------------------
  // TIMEOUT ENFORCEMENT
  // --------------------------------------------------------------------------

  /**
   * Start timeout enforcement for a work item
   * @param {string} workItemId
   * @private
   */
  _startTimeoutEnforcement(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem) return;

    const handle = setTimeout(() => {
      this._handleTimeout(workItemId);
    }, workItem.timeoutMs);

    this.timeoutHandles.set(workItemId, handle);
  }

  /**
   * Handle timeout for a work item
   * @param {string} workItemId
   * @private
   */
  _handleTimeout(workItemId) {
    const workItem = this.workItems.get(workItemId);
    if (!workItem || workItem.state !== 'executing') return;

    const durationMs = Date.now() - workItem.startedAt.getTime();
    this._logTimeout(workItem, durationMs);
    this._invokeTimeoutCallback(workItem, durationMs);
    this.cancelWorkItem(workItemId, 'timeout');
  }

  /**
   * Log timeout occurrence
   * @param {WorkItem} workItem
   * @param {number} durationMs
   * @private
   */
  _logTimeout(workItem, durationMs) {
    this.receiptLogger.logTimeoutOccurred(
      workItem.id,
      workItem.taskId,
      durationMs,
      workItem.timeoutMs
    );
  }

  /**
   * Invoke timeout callback
   * @param {WorkItem} workItem
   * @param {number} durationMs
   * @private
   */
  _invokeTimeoutCallback(workItem, durationMs) {
    if (this.config.onTimeout) {
      try {
        this.config.onTimeout({
          workItemId: workItem.id,
          taskId: workItem.taskId,
          durationMs,
          timeoutMs: workItem.timeoutMs,
          timestamp: new Date(),
        });
      } catch {
        // Ignore callback errors
      }
    }
  }

  /**
   * Clear timeout for a work item
   * @param {string} workItemId
   * @private
   */
  _clearTimeout(workItemId) {
    const handle = this.timeoutHandles.get(workItemId);
    if (handle) {
      clearTimeout(handle);
      this.timeoutHandles.delete(workItemId);
    }
  }

  // --------------------------------------------------------------------------
  // CIRCUIT BREAKER MANAGEMENT
  // --------------------------------------------------------------------------

  /**
   * Get or create circuit breaker for a task
   * @param {string} taskId
   * @returns {TaskCircuitBreaker}
   * @private
   */
  _getOrCreateCircuitBreaker(taskId) {
    if (!this.circuitBreakers.has(taskId)) {
      const breaker = new TaskCircuitBreaker({
        taskId,
        failureThreshold: this.config.circuitBreakerThreshold,
        resetTimeout: this.config.circuitBreakerReset,
        onStateChange: event => {
          if (event.to === 'closed') {
            this.receiptLogger.logCircuitBreakerClosed(event.taskId);
          }
        },
      });
      this.circuitBreakers.set(taskId, breaker);
    }
    return this.circuitBreakers.get(taskId);
  }

  /**
   * Enable a task (reset circuit breaker)
   * @param {string} taskId
   */
  enableTask(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    if (breaker) {
      breaker.reset();
      this.receiptLogger.log('TASK_ENABLED', {
        taskId,
        enabledAt: new Date().toISOString(),
      });
    }
  }

  /**
   * Check if a task is enabled
   * @param {string} taskId
   * @returns {boolean}
   */
  isTaskEnabled(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    if (!breaker) return true;
    return breaker.allowExecution();
  }

  /**
   * Get circuit breaker state for a task
   * @param {string} taskId
   * @returns {Object|null}
   */
  getCircuitBreakerState(taskId) {
    const breaker = this.circuitBreakers.get(taskId);
    return breaker ? breaker.getState() : null;
  }

  // --------------------------------------------------------------------------
  // CANCELLATION HOOKS
  // --------------------------------------------------------------------------

  /**
   * Register a cancellation hook
   * @param {Function} hook - Hook function (workItem, reason) => void
   */
  registerCancellationHook(hook) {
    if (typeof hook === 'function') {
      this.cancellationHooks.push(hook);
    }
  }

  /**
   * Invoke all cancellation hooks
   * @param {WorkItem} workItem
   * @param {string} reason
   * @private
   */
  _invokeCancellationHooks(workItem, reason) {
    for (const hook of this.cancellationHooks) {
      try {
        hook(workItem, reason);
      } catch {
        // Ignore hook errors
      }
    }
  }

  // --------------------------------------------------------------------------
  // TASK DEPENDENCIES
  // --------------------------------------------------------------------------

  /**
   * Set downstream dependencies for a task
   * @param {string} taskId
   * @param {string[]} downstreamTaskIds
   */
  setTaskDependencies(taskId, downstreamTaskIds) {
    this.taskDependencies.set(taskId, downstreamTaskIds);
  }

  /**
   * Get downstream dependencies for a task
   * @param {string} taskId
   * @returns {string[]}
   */
  getTaskDependencies(taskId) {
    return this.taskDependencies.get(taskId) || [];
  }

  // --------------------------------------------------------------------------
  // QUERYING & TIME-TRAVEL
  // --------------------------------------------------------------------------

  /**
   * Get work item by ID
   * @param {string} workItemId
   * @returns {WorkItem|undefined}
   */
  getWorkItem(workItemId) {
    return this.workItems.get(workItemId);
  }

  /**
   * Get all work items for a task
   * @param {string} taskId
   * @returns {WorkItem[]}
   */
  getWorkItemsForTask(taskId) {
    const ids = this.workItemsByTask.get(taskId);
    if (!ids) return [];
    return Array.from(ids)
      .map(id => this.workItems.get(id))
      .filter(wi => wi !== undefined);
  }

  /**
   * Get all work items for a case
   * @param {string} caseId
   * @returns {WorkItem[]}
   */
  getWorkItemsForCase(caseId) {
    const ids = this.workItemsByCase.get(caseId);
    if (!ids) return [];
    return Array.from(ids)
      .map(id => this.workItems.get(id))
      .filter(wi => wi !== undefined);
  }

  /**
   * Get cancellation state at a specific point in time
   * @param {Date} timestamp
   * @returns {Object} State snapshot
   */
  getStateAtTime(timestamp) {
    const receipts = this.receiptLogger.getReceiptsAtTime(timestamp);
    const state = this._buildEmptyState();
    this._processReceipts(receipts, state);

    return {
      timestamp,
      cancelledWorkItems: Array.from(state.cancelledWorkItems),
      openCircuitBreakers: Array.from(state.openCircuitBreakers),
      cancelledRegions: Array.from(state.cancelledRegions),
      receiptCount: receipts.length,
    };
  }

  /**
   * Build empty state structure
   * @returns {Object}
   * @private
   */
  _buildEmptyState() {
    return {
      cancelledWorkItems: new Set(),
      openCircuitBreakers: new Set(),
      cancelledRegions: new Set(),
    };
  }

  /**
   * Process receipts to build state
   * @param {CancellationReceipt[]} receipts
   * @param {Object} state
   * @private
   */
  _processReceipts(receipts, state) {
    for (const receipt of receipts) {
      switch (receipt.type) {
        case 'CANCELLED_WORK_ITEM':
          state.cancelledWorkItems.add(receipt.payload.workItemId);
          break;
        case 'CIRCUIT_BREAKER_OPEN':
          state.openCircuitBreakers.add(receipt.payload.taskId);
          break;
        case 'CIRCUIT_BREAKER_CLOSED':
          state.openCircuitBreakers.delete(receipt.payload.taskId);
          break;
        case 'REGION_CANCELLED':
          state.cancelledRegions.add(receipt.payload.regionId);
          break;
      }
    }
  }

  // --------------------------------------------------------------------------
  // STATISTICS
  // --------------------------------------------------------------------------

  /**
   * Get manager statistics
   * @returns {Object}
   */
  getStats() {
    const workItemsByState = this._computeWorkItemsByState();
    const circuitBreakerStats = this._computeCircuitBreakerStats();

    return {
      workItems: {
        total: this.workItems.size,
        byState: workItemsByState,
      },
      circuitBreakers: {
        total: this.circuitBreakers.size,
        states: circuitBreakerStats,
      },
      regions: {
        total: this.regionManager.regions.size,
      },
      receipts: {
        total: this.receiptLogger.receipts.length,
      },
      activeTimeouts: this.timeoutHandles.size,
    };
  }

  /**
   * Compute work items grouped by state
   * @returns {Object}
   * @private
   */
  _computeWorkItemsByState() {
    const byState = {};
    for (const wi of this.workItems.values()) {
      byState[wi.state] = (byState[wi.state] || 0) + 1;
    }
    return byState;
  }

  /**
   * Compute circuit breaker statistics
   * @returns {Object}
   * @private
   */
  _computeCircuitBreakerStats() {
    const stats = {};
    for (const [taskId, breaker] of this.circuitBreakers) {
      stats[taskId] = breaker.getState();
    }
    return stats;
  }

  // --------------------------------------------------------------------------
  // CLEANUP
  // --------------------------------------------------------------------------

  /**
   * Terminate all pending work and cleanup
   * @param {string} reason
   */
  terminate(reason = 'workflow_terminated') {
    for (const [id, workItem] of this.workItems) {
      if (workItem.state === 'executing' || workItem.state === 'enabled' || workItem.state === 'pending') {
        this.cancelWorkItem(id, reason);
      }
    }

    for (const [id, handle] of this.timeoutHandles) {
      clearTimeout(handle);
    }
    this.timeoutHandles.clear();
  }

  /**
   * Export full state for serialization
   * @returns {Object}
   */
  export() {
    return {
      workItems: this._exportWorkItems(),
      regions: this.regionManager.export(),
      receipts: this.receiptLogger.export(),
      dependencies: Object.fromEntries(this.taskDependencies),
      circuitBreakers: this._exportCircuitBreakers(),
    };
  }

  /**
   * Export work items with serialized dates
   * @returns {Array}
   * @private
   */
  _exportWorkItems() {
    return Array.from(this.workItems.values()).map(wi => ({
      ...wi,
      createdAt: wi.createdAt.toISOString(),
      startedAt: wi.startedAt?.toISOString(),
      completedAt: wi.completedAt?.toISOString(),
      cancelledAt: wi.cancelledAt?.toISOString(),
    }));
  }

  /**
   * Export circuit breaker states
   * @returns {Object}
   * @private
   */
  _exportCircuitBreakers() {
    return Object.fromEntries(
      Array.from(this.circuitBreakers.entries()).map(([k, v]) => [k, v.getState()])
    );
  }

  /**
   * Clear all state
   */
  clear() {
    this.terminate();
    this.workItems.clear();
    this.workItemsByTask.clear();
    this.workItemsByCase.clear();
    this.circuitBreakers.clear();
    this.taskDependencies.clear();
    this.receiptLogger.clear();
    this.regionManager.regions.clear();
    this.regionManager.taskToRegions.clear();
  }
}

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

/**
 * Create a cancellation manager instance
 * @param {CancellationConfig} [config]
 * @returns {YawlCancellationManager}
 */
export function createCancellationManager(config = {}) {
  return new YawlCancellationManager(config);
}

/**
 * Create a cancellation region
 * @param {YawlCancellationManager} manager
 * @param {Object} options
 * @returns {CancellationRegion}
 */
export function createCancellationRegion(manager, options) {
  return manager.regionManager.createRegion(options);
}

// ============================================================================
// EXPORTS
// ============================================================================

export {
  WorkItemSchema,
  CancellationRegionSchema,
  CancellationReceiptSchema,
  CancellationReasonSchema,
  WorkItemStateSchema,
  CircuitBreakerStateSchema,
};
