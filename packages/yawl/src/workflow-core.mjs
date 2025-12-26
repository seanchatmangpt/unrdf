/**
 * @file YAWL Workflow - Core class and basic operations
 * @module @unrdf/yawl/workflow-core
 *
 * @description
 * Core Workflow class with constructor, initialization, query methods,
 * and mutation methods. This module provides the foundation for workflow
 * management without validation or RDF serialization logic.
 */

import { z } from 'zod';
import { SPLIT_TYPE, JOIN_TYPE, validateTaskDef, validateFlowDef } from './patterns.mjs';

// =============================================================================
// Zod Schemas for Workflow Validation
// =============================================================================

/**
 * Task definition schema for workflow tasks
 */
export const TaskDefSchema = z.object({
  /** Unique task identifier within the workflow */
  id: z.string().min(1).max(100),
  /** Human-readable task name */
  name: z.string().min(1).max(200).optional(),
  /** Task kind: atomic, composite, multiple, cancellation */
  kind: z.enum(['atomic', 'composite', 'multiple', 'cancellation']).default('atomic'),
  /** Outgoing flow semantics */
  splitType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  /** Incoming flow semantics */
  joinType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  /** Task IDs in this task's cancellation region */
  cancellationRegion: z.string().optional(),
  /** Tasks cancelled when this task completes */
  cancellationSet: z.array(z.string()).optional(),
  /** Condition function for task enablement */
  condition: z.function().optional(),
  /** Timeout in milliseconds */
  timeout: z.number().positive().optional(),
  /** Assigned resource pattern */
  resource: z.string().optional(),
  /** Required role */
  role: z.string().optional(),
  /** Sub-workflow ID for composite tasks */
  subNetId: z.string().optional(),
  /** Task priority (0-100) */
  priority: z.number().int().min(0).max(100).optional(),
  /** Task documentation */
  documentation: z.string().max(2000).optional(),
});

/**
 * Flow definition schema for control flow connections
 */
export const FlowDefSchema = z.object({
  /** Source task ID */
  from: z.string().min(1),
  /** Target task ID */
  to: z.string().min(1),
  /** Condition function for conditional flows */
  condition: z.function().optional(),
  /** Evaluation priority for XOR/OR splits (higher = first) */
  priority: z.number().default(0),
  /** Whether this is the default flow */
  isDefault: z.boolean().optional(),
  /** Flow documentation */
  documentation: z.string().max(1000).optional(),
});

/**
 * Complete workflow specification schema
 */
export const WorkflowSpecSchema = z.object({
  /** Unique workflow identifier */
  id: z.string().min(1).max(100),
  /** Human-readable workflow name */
  name: z.string().min(1).max(200).optional(),
  /** Semantic version string */
  version: z.string().regex(/^\d+\.\d+\.\d+$/).default('1.0.0'),
  /** Workflow description */
  description: z.string().max(5000).optional(),
  /** Task definitions */
  tasks: z.array(TaskDefSchema).min(1),
  /** Flow definitions */
  flows: z.array(FlowDefSchema).optional().default([]),
  /** Starting task ID (auto-detected if not specified) */
  startTaskId: z.string().optional(),
  /** Ending task IDs (auto-detected if not specified) */
  endTaskIds: z.array(z.string()).optional().default([]),
  /** Cancellation regions mapping region ID to task IDs */
  cancellationRegions: z.record(z.string(), z.array(z.string())).optional().default({}),
  /** Workflow author */
  author: z.string().max(100).optional(),
  /** Creation timestamp */
  createdAt: z.date().optional(),
  /** Modification timestamp */
  modifiedAt: z.date().optional(),
});

/**
 * Validation result type
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {string[]} errors - List of validation errors
 * @property {string[]} warnings - List of validation warnings
 */

// =============================================================================
// Workflow Class - Core
// =============================================================================

/**
 * Workflow class - Represents a complete workflow definition
 *
 * Provides methods for querying workflow structure, validating integrity,
 * and managing control flow semantics.
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
   *
   * @example
   * const task = workflow.getTask('review');
   * console.log(task.name); // 'Review Expense'
   */
  getTask(taskId) {
    return this._tasks.get(taskId);
  }

  /**
   * Get all task definitions
   * @returns {Array<Object>} Array of all task definitions
   *
   * @example
   * const tasks = workflow.getTasks();
   * console.log(`Workflow has ${tasks.length} tasks`);
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
   *
   * @example
   * const downstream = workflow.getDownstreamTasks('review');
   * // Returns [{ id: 'approve', ... }, { id: 'reject', ... }]
   */
  getDownstreamTasks(taskId) {
    const flows = this._outgoingFlows.get(taskId) ?? [];
    const downstream = [];

    for (const flow of flows) {
      const task = this._tasks.get(flow.to);
      if (task) {
        downstream.push(task);
      }
    }

    return downstream;
  }

  /**
   * Get upstream tasks (tasks that lead to a given task)
   * @param {string} taskId - Target task identifier
   * @returns {Array<Object>} Array of upstream task definitions
   *
   * @example
   * const upstream = workflow.getUpstreamTasks('approve');
   * // Returns [{ id: 'review', ... }]
   */
  getUpstreamTasks(taskId) {
    const flows = this._incomingFlows.get(taskId) ?? [];
    const upstream = [];

    for (const flow of flows) {
      const task = this._tasks.get(flow.from);
      if (task) {
        upstream.push(task);
      }
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

  /**
   * Check if task is an initial task (starting point)
   * @param {string} taskId - Task identifier
   * @returns {boolean} True if task is initial
   *
   * @example
   * if (workflow.isInitialTask('submit')) {
   *   console.log('This is the starting task');
   * }
   */
  isInitialTask(taskId) {
    return taskId === this._startTaskId;
  }

  /**
   * Check if task is a final task (ending point)
   * @param {string} taskId - Task identifier
   * @returns {boolean} True if task is final
   */
  isFinalTask(taskId) {
    return this._endTaskIds.includes(taskId);
  }

  /**
   * Get the starting task ID
   * @returns {string|undefined} Start task ID
   */
  getStartTaskId() {
    return this._startTaskId;
  }

  /**
   * Get ending task IDs
   * @returns {string[]} Array of end task IDs
   */
  getEndTaskIds() {
    return [...this._endTaskIds];
  }

  /**
   * Get cancellation region for a task
   * @param {string} taskId - Task identifier
   * @returns {string[]} Array of task IDs in the cancellation region
   *
   * @example
   * const region = workflow.getCancellationRegion('timeout-task');
   * // Returns ['task-a', 'task-b', 'task-c']
   */
  getCancellationRegion(taskId) {
    const regionId = this._taskToRegion.get(taskId);
    if (!regionId) {
      return [];
    }
    return this._regionToTasks.get(regionId) ?? [];
  }

  /**
   * Get tasks in a specific cancellation region by region ID
   * @param {string} regionId - Region identifier
   * @returns {string[]} Array of task IDs in the region
   */
  getTasksInRegion(regionId) {
    return this._regionToTasks.get(regionId) ?? [];
  }

  /**
   * Get all cancellation regions
   * @returns {Map<string, string[]>} Map of region ID to task IDs
   */
  getCancellationRegions() {
    return new Map(this._regionToTasks);
  }

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

  // ===========================================================================
  // Serialization
  // ===========================================================================

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
      tasks: this.getTasks().map(task => ({
        ...task,
        // Remove function references
        condition: undefined,
      })),
      flows: this._flows.map(flow => ({
        ...flow,
        // Remove function references
        condition: undefined,
      })),
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

    if (json.locked) {
      workflow.lock();
    }

    return workflow;
  }
}
