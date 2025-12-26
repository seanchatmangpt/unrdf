/**
 * @file YAWL Workflow Class - Core workflow definition and control flow logic
 * @module @unrdf/yawl/workflow/workflow-class
 *
 * @description
 * Main Workflow class providing:
 * - Workflow structure management (tasks, flows, cancellation regions)
 * - Query methods for workflow graph traversal
 * - Control flow evaluation (split/join semantics)
 * - Mutation methods (add tasks/flows, set start/end)
 * - JSON serialization/deserialization
 *
 * @example
 * import { Workflow } from '@unrdf/yawl/workflow/workflow-class';
 *
 * const workflow = new Workflow({
 *   id: 'expense-approval',
 *   tasks: [
 *     { id: 'submit', name: 'Submit Expense' },
 *     { id: 'review', name: 'Review', splitType: 'xor' },
 *   ],
 *   flows: [{ from: 'submit', to: 'review' }],
 * });
 */

import { WorkflowSpecSchema } from './schemas.mjs';
import { validateWorkflow } from './validation.mjs';
import { evaluateDownstream as evalDownstream, canEnable as checkCanEnable } from './control-flow.mjs';
import { lockWorkflow, addTask as mutAddTask, addFlow as mutAddFlow, setStart as mutSetStart, setEnd as mutSetEnd } from './mutations.mjs';
import { workflowToJSON as toJSON } from './serialization.mjs';
import { validateTaskDef, validateFlowDef } from '../patterns.mjs';

// =============================================================================
// Workflow Class
// =============================================================================

/**
 * Workflow class - Represents a complete workflow definition
 *
 * Provides methods for querying workflow structure, validating integrity,
 * and managing control flow semantics according to YAWL patterns.
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
  // Validation Methods
  // ===========================================================================

  /**
   * Validate workflow structure and integrity
   * @returns {ValidationResult} Validation result with errors and warnings
   */
  validate() {
    return validateWorkflow(this);
  }

  /**
   * Quick check if workflow is valid
   * @returns {boolean} True if workflow is valid
   */
  isValid() {
    return this.validate().valid;
  }

  // ===========================================================================
  // Control Flow Evaluation
  // ===========================================================================

  /**
   * Evaluate which downstream tasks should be enabled after task completion
   * @param {string} completedTaskId - Task that completed
   * @param {Object} [context={}] - Evaluation context (data, case state)
   * @returns {string[]} Task IDs to enable
   */
  evaluateDownstream(completedTaskId, context = {}) {
    return evalDownstream(this, completedTaskId, context);
  }

  /**
   * Check if a task can be enabled based on join semantics
   * @param {string} taskId - Task to check
   * @param {Set<string>} completedTasks - Set of completed task IDs
   * @param {Set<string>} [activatedTasks=new Set()] - Set of tasks activated by OR-split
   * @returns {boolean} True if task can be enabled
   */
  canEnable(taskId, completedTasks, activatedTasks = new Set()) {
    return checkCanEnable(this, taskId, completedTasks, activatedTasks);
  }

  // ===========================================================================
  // Mutation Methods
  // ===========================================================================

  /**
   * Lock the workflow (no more modifications)
   * @returns {Workflow} this
   */
  lock() {
    return lockWorkflow(this);
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
    return mutAddTask(this, taskDef);
  }

  /**
   * Add a flow between tasks
   * @param {Object} flowDef - Flow definition
   * @returns {Workflow} this
   * @throws {Error} If workflow is locked or tasks don't exist
   */
  addFlow(flowDef) {
    return mutAddFlow(this, flowDef);
  }

  /**
   * Set the start task
   * @param {string} taskId - Start task ID
   * @returns {Workflow} this
   */
  setStart(taskId) {
    return mutSetStart(this, taskId);
  }

  /**
   * Set end tasks
   * @param {string[]} taskIds - End task IDs
   * @returns {Workflow} this
   */
  setEnd(taskIds) {
    return mutSetEnd(this, taskIds);
  }

  // ===========================================================================
  // Serialization
  // ===========================================================================

  /**
   * Serialize workflow to JSON
   * @returns {Object} JSON representation
   */
  toJSON() {
    return toJSON(this);
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

// =============================================================================
// Module Exports
// =============================================================================

export default Workflow;
