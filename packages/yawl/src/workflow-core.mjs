/**
 * @file YAWL Workflow Core - Workflow class definition
 * @module @unrdf/yawl/workflow-core
 *
 * @description
 * Core Workflow class providing workflow structure management, task/flow queries,
 * and mutation operations. Validation and execution logic are in separate modules.
 *
 * @example
 * import { Workflow } from '@unrdf/yawl/workflow-core';
 *
 * const workflow = new Workflow({
 *   id: 'order-processing',
 *   tasks: [
 *     { id: 'receive', name: 'Receive Order' },
 *     { id: 'process', name: 'Process Order' },
 *   ],
 *   flows: [{ from: 'receive', to: 'process' }],
 * });
 */

import { WorkflowSpecSchema } from './workflow-schemas.mjs';
import { validateTaskDef, validateFlowDef } from './patterns.mjs';

// =============================================================================
// Workflow Class
// =============================================================================

/**
 * Workflow class - Represents a complete workflow definition
 *
 * Provides methods for querying workflow structure, managing tasks and flows,
 * and accessing control flow semantics. Validation and execution methods
 * are added via workflow-execution.mjs.
 *
 * @example
 * const workflow = new Workflow({
 *   id: 'order-processing',
 *   tasks: [
 *     { id: 'receive', name: 'Receive Order' },
 *     { id: 'process', name: 'Process Order' },
 *     { id: 'ship', name: 'Ship Order' },
 *   ],
 *   flows: [
 *     { from: 'receive', to: 'process' },
 *     { from: 'process', to: 'ship' },
 *   ],
 * });
 */
export class Workflow {
  /**
   * Create a new Workflow instance
   * @param {Object} spec - Workflow specification
   * @param {string} spec.id - Unique workflow identifier
   * @param {string} [spec.name] - Human-readable name
   * @param {string} [spec.version='1.0.0'] - Semantic version
   * @param {Array<Object>} spec.tasks - Task definitions
   * @param {Array<Object>} [spec.flows=[]] - Flow definitions
   * @param {string} [spec.startTaskId] - Starting task ID
   * @param {string[]} [spec.endTaskIds=[]] - Ending task IDs
   * @param {Object} [spec.cancellationRegions={}] - Cancellation regions
   */
  constructor(spec) {
    // Validate and parse the specification
    const validated = WorkflowSpecSchema.parse(spec);

    // Core properties
    this.id = validated.id;
    this.name = validated.name ?? validated.id;
    this.version = validated.version;
    this.description = validated.description;
    this.author = validated.author;
    this.createdAt = validated.createdAt ?? new Date();
    this.modifiedAt = validated.modifiedAt ?? new Date();

    // Task management
    /** @type {Map<string, Object>} Task definitions indexed by ID */
    this._tasks = new Map();

    // Flow management
    /** @type {Array<Object>} All flow definitions */
    this._flows = [];
    /** @type {Map<string, Array<Object>>} Outgoing flows by task ID */
    this._outgoingFlows = new Map();
    /** @type {Map<string, Array<Object>>} Incoming flows by task ID */
    this._incomingFlows = new Map();

    // Control flow tracking
    this._startTaskId = validated.startTaskId;
    this._endTaskIds = validated.endTaskIds ?? [];

    // Cancellation regions
    /** @type {Map<string, string>} Task ID to region ID mapping */
    this._taskToRegion = new Map();
    /** @type {Map<string, string[]>} Region ID to task IDs mapping */
    this._regionToTasks = new Map();

    // Workflow state
    /** @type {boolean} Whether workflow is locked for modifications */
    this._locked = false;

    // Initialize from spec
    this._initializeTasks(validated.tasks);
    this._initializeFlows(validated.flows ?? []);
    this._initializeCancellationRegions(validated.cancellationRegions ?? {});
    this._detectStartEnd();
  }

  /**
   * Initialize tasks from specification
   * @param {Array<Object>} tasks - Task definitions
   * @private
   */
  _initializeTasks(tasks) {
    for (const taskDef of tasks) {
      const validated = validateTaskDef(taskDef);
      this._tasks.set(validated.id, validated);
      this._outgoingFlows.set(validated.id, []);
      this._incomingFlows.set(validated.id, []);

      // Track task cancellation region
      if (validated.cancellationRegion) {
        this._taskToRegion.set(validated.id, validated.cancellationRegion);
      }
    }
  }

  /**
   * Initialize flows from specification
   * @param {Array<Object>} flows - Flow definitions
   * @private
   */
  _initializeFlows(flows) {
    for (const flowDef of flows) {
      const validated = validateFlowDef(flowDef);

      // Store the flow
      this._flows.push(validated);

      // Update flow indices
      if (this._outgoingFlows.has(validated.from)) {
        this._outgoingFlows.get(validated.from).push(validated);
      }

      if (this._incomingFlows.has(validated.to)) {
        this._incomingFlows.get(validated.to).push(validated);
      }
    }
  }

  /**
   * Initialize cancellation regions from specification
   * @param {Object} regions - Region definitions
   * @private
   */
  _initializeCancellationRegions(regions) {
    for (const [regionId, taskIds] of Object.entries(regions)) {
      this._regionToTasks.set(regionId, taskIds);
      for (const taskId of taskIds) {
        this._taskToRegion.set(taskId, regionId);
      }
    }
  }

  /**
   * Detect start and end tasks if not specified
   * @private
   */
  _detectStartEnd() {
    // Detect start task (no incoming flows)
    if (!this._startTaskId) {
      for (const [taskId] of this._tasks) {
        const incoming = this._incomingFlows.get(taskId) ?? [];
        if (incoming.length === 0) {
          this._startTaskId = taskId;
          break;
        }
      }
      // Fallback to first task
      if (!this._startTaskId && this._tasks.size > 0) {
        this._startTaskId = this._tasks.keys().next().value;
      }
    }

    // Detect end tasks (no outgoing flows)
    if (this._endTaskIds.length === 0) {
      for (const [taskId] of this._tasks) {
        const outgoing = this._outgoingFlows.get(taskId) ?? [];
        if (outgoing.length === 0) {
          this._endTaskIds.push(taskId);
        }
      }
    }
  }

  // ===========================================================================
  // Query Methods
  // ===========================================================================

  /**
   * Get task definition by ID
   * @param {string} taskId - Task identifier
   * @returns {Object|undefined} Task definition or undefined if not found
   */
  getTask(taskId) {
    return this._tasks.get(taskId);
  }

  /**
   * Get all task definitions
   * @returns {Array<Object>} Array of all task definitions
   */
  getTasks() {
    return Array.from(this._tasks.values());
  }

  /**
   * Get task IDs only
   * @returns {string[]} Array of task IDs
   */
  getTaskIds() {
    return Array.from(this._tasks.keys());
  }

  /**
   * Get downstream tasks (tasks that follow a given task)
   * @param {string} taskId - Source task identifier
   * @returns {Array<Object>} Array of downstream task definitions
   */
  getDownstreamTasks(taskId) {
    const flows = this._outgoingFlows.get(taskId) ?? [];
    const downstream = [];
    for (const flow of flows) {
      const task = this._tasks.get(flow.to);
      if (task) downstream.push(task);
    }
    return downstream;
  }

  /**
   * Get upstream tasks (tasks that lead to a given task)
   * @param {string} taskId - Target task identifier
   * @returns {Array<Object>} Array of upstream task definitions
   */
  getUpstreamTasks(taskId) {
    const flows = this._incomingFlows.get(taskId) ?? [];
    const upstream = [];
    for (const flow of flows) {
      const task = this._tasks.get(flow.from);
      if (task) upstream.push(task);
    }
    return upstream;
  }

  /**
   * Get outgoing flows from a task
   * @param {string} taskId - Task identifier
   * @returns {Array<Object>} Outgoing flow definitions
   */
  getOutgoingFlows(taskId) {
    return this._outgoingFlows.get(taskId) ?? [];
  }

  /**
   * Get incoming flows to a task
   * @param {string} taskId - Task identifier
   * @returns {Array<Object>} Incoming flow definitions
   */
  getIncomingFlows(taskId) {
    return this._incomingFlows.get(taskId) ?? [];
  }

  /**
   * Get all flows
   * @returns {Array<Object>} All flow definitions
   */
  getFlows() {
    return [...this._flows];
  }

  /** Check if task is an initial task (starting point) */
  isInitialTask(taskId) { return taskId === this._startTaskId; }

  /** Check if task is a final task (ending point) */
  isFinalTask(taskId) { return this._endTaskIds.includes(taskId); }

  /** Get the starting task ID */
  getStartTaskId() { return this._startTaskId; }

  /** Get ending task IDs */
  getEndTaskIds() { return [...this._endTaskIds]; }

  /**
   * Get cancellation region for a task
   * @param {string} taskId - Task identifier
   * @returns {string[]} Array of task IDs in the cancellation region
   */
  getCancellationRegion(taskId) {
    const regionId = this._taskToRegion.get(taskId);
    return regionId ? this._regionToTasks.get(regionId) ?? [] : [];
  }

  /** Get tasks in a specific cancellation region by region ID */
  getTasksInRegion(regionId) { return this._regionToTasks.get(regionId) ?? []; }

  /** Get all cancellation regions */
  getCancellationRegions() { return new Map(this._regionToTasks); }

  // ===========================================================================
  // Mutation Methods
  // ===========================================================================

  /**
   * Lock the workflow (no more modifications)
   * @returns {Workflow} this
   */
  lock() {
    this._locked = true;
    return this;
  }

  /**
   * Check if workflow is locked
   * @returns {boolean} True if locked
   */
  isLocked() {
    return this._locked;
  }

  /**
   * Add a task to the workflow
   * @param {Object} taskDef - Task definition
   * @returns {Workflow} this
   * @throws {Error} If workflow is locked
   */
  addTask(taskDef) {
    if (this._locked) {
      throw new Error('Cannot modify locked workflow');
    }

    const validated = validateTaskDef(taskDef);

    if (this._tasks.has(validated.id)) {
      throw new Error(`Task '${validated.id}' already exists in workflow`);
    }

    this._tasks.set(validated.id, validated);
    this._outgoingFlows.set(validated.id, []);
    this._incomingFlows.set(validated.id, []);

    if (validated.cancellationRegion) {
      this._taskToRegion.set(validated.id, validated.cancellationRegion);
    }

    this.modifiedAt = new Date();
    return this;
  }

  /**
   * Add a flow between tasks
   * @param {Object} flowDef - Flow definition
   * @returns {Workflow} this
   * @throws {Error} If workflow is locked or tasks don't exist
   */
  addFlow(flowDef) {
    if (this._locked) {
      throw new Error('Cannot modify locked workflow');
    }

    const validated = validateFlowDef(flowDef);

    if (!this._tasks.has(validated.from)) {
      throw new Error(`Source task '${validated.from}' not found`);
    }
    if (!this._tasks.has(validated.to)) {
      throw new Error(`Target task '${validated.to}' not found`);
    }

    this._flows.push(validated);
    this._outgoingFlows.get(validated.from).push(validated);
    this._incomingFlows.get(validated.to).push(validated);

    this.modifiedAt = new Date();
    return this;
  }

  /**
   * Set the start task
   * @param {string} taskId - Start task ID
   * @returns {Workflow} this
   */
  setStart(taskId) {
    if (this._locked) {
      throw new Error('Cannot modify locked workflow');
    }
    if (!this._tasks.has(taskId)) {
      throw new Error(`Task '${taskId}' not found`);
    }
    this._startTaskId = taskId;
    this.modifiedAt = new Date();
    return this;
  }

  /**
   * Set end tasks
   * @param {string[]} taskIds - End task IDs
   * @returns {Workflow} this
   */
  setEnd(taskIds) {
    if (this._locked) {
      throw new Error('Cannot modify locked workflow');
    }
    for (const taskId of taskIds) {
      if (!this._tasks.has(taskId)) {
        throw new Error(`Task '${taskId}' not found`);
      }
    }
    this._endTaskIds = [...taskIds];
    this.modifiedAt = new Date();
    return this;
  }

  /**
   * Serialize workflow to JSON
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      id: this.id,
      name: this.name,
      version: this.version,
      description: this.description,
      author: this.author,
      startTaskId: this._startTaskId,
      endTaskIds: this._endTaskIds,
      tasks: this.getTasks().map(task => ({ ...task, condition: undefined })),
      flows: this._flows.map(flow => ({ ...flow, condition: undefined })),
      cancellationRegions: Object.fromEntries(this._regionToTasks),
      locked: this._locked,
      createdAt: this.createdAt?.toISOString(),
      modifiedAt: this.modifiedAt?.toISOString(),
    };
  }

  /**
   * Create workflow from JSON
   * @param {Object} json - JSON representation
   * @returns {Workflow} Workflow instance
   */
  static fromJSON(json) {
    const workflow = new Workflow({
      id: json.id,
      name: json.name,
      version: json.version,
      description: json.description,
      author: json.author,
      startTaskId: json.startTaskId,
      endTaskIds: json.endTaskIds,
      tasks: json.tasks,
      flows: json.flows,
      cancellationRegions: json.cancellationRegions,
      createdAt: json.createdAt ? new Date(json.createdAt) : undefined,
      modifiedAt: json.modifiedAt ? new Date(json.modifiedAt) : undefined,
    });
    if (json.locked) workflow.lock();
    return workflow;
  }
}

// =============================================================================
// Module Exports
// =============================================================================

export default Workflow;
