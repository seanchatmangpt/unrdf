/**
 * @file YAWL Workflow - Workflow definition management
 * @module @unrdf/yawl/workflow
 *
 * @description
 * Complete workflow definition management with:
 * - Workflow class for workflow structure and queries
 * - Factory function for creating validated workflows
 * - Comprehensive validation logic (control flow, split/join, reachability)
 * - RDF integration for serialization/deserialization
 * - Zod schemas for type-safe validation
 *
 * @example
 * import { Workflow, createWorkflow, workflowToRDF, workflowFromRDF } from '@unrdf/yawl';
 *
 * const workflow = createWorkflow({
 *   id: 'expense-approval',
 *   name: 'Expense Approval Process',
 *   tasks: [
 *     { id: 'submit', name: 'Submit Expense' },
 *     { id: 'review', name: 'Review', splitType: 'xor' },
 *     { id: 'approve', name: 'Approve' },
 *     { id: 'reject', name: 'Reject' },
 *   ],
 *   flows: [
 *     { from: 'submit', to: 'review' },
 *     { from: 'review', to: 'approve', condition: ctx => ctx.amount < 1000 },
 *     { from: 'review', to: 'reject', priority: 0 },
 *   ],
 * });
 */

import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';
import {
  YAWL,
  YAWL_TASK,
  WorkflowSpec,
  Task,
  Flow,
  AtomicTask,
  CompositeTask,
  MultipleInstanceTask,
  XOR_Split,
  AND_Split,
  OR_Split,
  XOR_Join,
  AND_Join,
  OR_Join,
  rdfType,
  rdfsLabel,
  taskName as taskNameProp,
  taskId as taskIdProp,
  kind,
  joinsTo,
  joinsFrom,
  splitBehavior,
  joinBehavior,
  hasTasks,
  cancellationSet,
  sourceTask,
  targetTask,
  flowCondition,
  flowPriority,
  isDefaultFlow,
  specUri,
  taskUri,
  flowUri,
  stringLiteral,
  integerLiteral,
  booleanLiteral,
} from './ontology/yawl-ontology.mjs';
import { SPLIT_TYPE, JOIN_TYPE, validateTaskDef, validateFlowDef } from './patterns.mjs';

const { quad, namedNode, literal } = dataFactory;

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
// Workflow Class
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
  // Validation Methods
  // ===========================================================================

  /**
   * Validate workflow structure and integrity
   * @returns {ValidationResult} Validation result with errors and warnings
   *
   * @example
   * const result = workflow.validate();
   * if (!result.valid) {
   *   console.error('Validation errors:', result.errors);
   * }
   */
  validate() {
    const errors = [];
    const warnings = [];

    // Basic structure validation
    this._validateBasicStructure(errors, warnings);

    // Control flow validation
    this._validateControlFlowIntegrity(errors, warnings);

    // Split/Join consistency
    this._validateSplitJoinConsistency(errors, warnings);

    // Reachability
    this._validateReachability(errors, warnings);

    // Cancellation regions
    this._validateCancellationRegions(errors, warnings);

    // Cycle detection
    this._validateNoCycles(errors, warnings);

    return {
      valid: errors.length === 0,
      errors,
      warnings,
    };
  }

  /**
   * Quick check if workflow is valid
   * @returns {boolean} True if workflow is valid
   */
  isValid() {
    return this.validate().valid;
  }

  /**
   * Validate basic structure
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateBasicStructure(errors, warnings) {
    // Must have at least one task
    if (this._tasks.size === 0) {
      errors.push('Workflow has no tasks');
    }

    // Must have start task
    if (!this._startTaskId) {
      errors.push('Workflow has no start task');
    } else if (!this._tasks.has(this._startTaskId)) {
      errors.push(`Start task '${this._startTaskId}' not found in workflow`);
    }

    // Should have at least one end task
    if (this._endTaskIds.length === 0) {
      warnings.push('Workflow has no designated end tasks');
    }

    // End tasks must exist
    for (const endTaskId of this._endTaskIds) {
      if (!this._tasks.has(endTaskId)) {
        errors.push(`End task '${endTaskId}' not found in workflow`);
      }
    }
  }

  /**
   * Validate control flow integrity
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateControlFlowIntegrity(errors, warnings) {
    // All flows must reference existing tasks
    for (const flow of this._flows) {
      if (!this._tasks.has(flow.from)) {
        errors.push(`Flow references non-existent source task '${flow.from}'`);
      }
      if (!this._tasks.has(flow.to)) {
        errors.push(`Flow references non-existent target task '${flow.to}'`);
      }

      // Check for self-loops (usually invalid in YAWL)
      if (flow.from === flow.to) {
        warnings.push(`Flow from '${flow.from}' to itself detected (self-loop)`);
      }
    }

    // Check for duplicate flows
    const flowSet = new Set();
    for (const flow of this._flows) {
      const key = `${flow.from}->${flow.to}`;
      if (flowSet.has(key)) {
        warnings.push(`Duplicate flow from '${flow.from}' to '${flow.to}'`);
      }
      flowSet.add(key);
    }
  }

  /**
   * Validate split/join type consistency
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateSplitJoinConsistency(errors, warnings) {
    // Track split-join pairs for matching
    const splitJoinPairs = new Map();

    for (const [taskId, task] of this._tasks) {
      const outgoing = this._outgoingFlows.get(taskId) ?? [];
      const incoming = this._incomingFlows.get(taskId) ?? [];

      // Validate split type matches outgoing flow count
      const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
      if (splitType === SPLIT_TYPE.SEQUENCE && outgoing.length > 1) {
        errors.push(
          `Task '${taskId}' has sequence split but ${outgoing.length} outgoing flows`
        );
      }
      if (
        (splitType === SPLIT_TYPE.AND ||
          splitType === SPLIT_TYPE.XOR ||
          splitType === SPLIT_TYPE.OR) &&
        outgoing.length < 2
      ) {
        warnings.push(
          `Task '${taskId}' has ${splitType} split but only ${outgoing.length} outgoing flow(s)`
        );
      }

      // Validate join type matches incoming flow count
      const joinType = task.joinType ?? JOIN_TYPE.SEQUENCE;
      if (joinType === JOIN_TYPE.SEQUENCE && incoming.length > 1) {
        errors.push(
          `Task '${taskId}' has sequence join but ${incoming.length} incoming flows`
        );
      }
      if (
        (joinType === JOIN_TYPE.AND ||
          joinType === JOIN_TYPE.XOR ||
          joinType === JOIN_TYPE.OR) &&
        incoming.length < 2
      ) {
        warnings.push(
          `Task '${taskId}' has ${joinType} join but only ${incoming.length} incoming flow(s)`
        );
      }

      // XOR split should have at least one flow with condition or default
      if (splitType === SPLIT_TYPE.XOR && outgoing.length > 1) {
        const hasConditions = outgoing.some(f => f.condition || f.isDefault);
        if (!hasConditions) {
          warnings.push(
            `Task '${taskId}' has XOR split but no conditions or default flow defined`
          );
        }
      }
    }

    // Check matching split-join patterns (AND-AND, XOR-XOR, OR-OR)
    this._validateMatchingSplitJoin(errors, warnings);
  }

  /**
   * Validate matching split-join patterns
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateMatchingSplitJoin(errors, warnings) {
    // Find all split points and their corresponding join points
    for (const [taskId, task] of this._tasks) {
      const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
      if (
        splitType === SPLIT_TYPE.AND ||
        splitType === SPLIT_TYPE.XOR ||
        splitType === SPLIT_TYPE.OR
      ) {
        // Find the convergence point
        const convergence = this._findConvergencePoint(taskId);
        if (convergence) {
          const joinTask = this._tasks.get(convergence);
          if (joinTask) {
            const joinType = joinTask.joinType ?? JOIN_TYPE.SEQUENCE;
            // Check for matching types
            if (splitType === SPLIT_TYPE.AND && joinType !== JOIN_TYPE.AND) {
              warnings.push(
                `AND-split at '${taskId}' converges at '${convergence}' with ${joinType}-join (expected AND-join)`
              );
            }
            // XOR split can merge with XOR or sequence
            // OR split should merge with OR join
            if (splitType === SPLIT_TYPE.OR && joinType !== JOIN_TYPE.OR) {
              warnings.push(
                `OR-split at '${taskId}' converges at '${convergence}' with ${joinType}-join (expected OR-join)`
              );
            }
          }
        }
      }
    }
  }

  /**
   * Find the convergence point for a split
   * @param {string} splitTaskId - Split task ID
   * @returns {string|null} Convergence task ID or null
   * @private
   */
  _findConvergencePoint(splitTaskId) {
    const outgoing = this._outgoingFlows.get(splitTaskId) ?? [];
    if (outgoing.length < 2) return null;

    // Simple heuristic: find first common descendant
    const visited = new Map();
    const queue = outgoing.map(f => ({ taskId: f.to, path: new Set([splitTaskId, f.to]) }));

    while (queue.length > 0) {
      const { taskId, path } = queue.shift();

      if (visited.has(taskId)) {
        const existingPaths = visited.get(taskId);
        existingPaths.push(path);

        // Check if all outgoing branches reach this point
        if (existingPaths.length >= 2) {
          return taskId;
        }
      } else {
        visited.set(taskId, [path]);
      }

      const nextFlows = this._outgoingFlows.get(taskId) ?? [];
      for (const flow of nextFlows) {
        if (!path.has(flow.to)) {
          const newPath = new Set(path);
          newPath.add(flow.to);
          queue.push({ taskId: flow.to, path: newPath });
        }
      }
    }

    return null;
  }

  /**
   * Validate all tasks are reachable from start
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateReachability(errors, warnings) {
    if (!this._startTaskId) return;

    const visited = new Set();
    const queue = [this._startTaskId];

    while (queue.length > 0) {
      const taskId = queue.shift();
      if (visited.has(taskId)) continue;
      visited.add(taskId);

      const outgoing = this._outgoingFlows.get(taskId) ?? [];
      for (const flow of outgoing) {
        if (!visited.has(flow.to)) {
          queue.push(flow.to);
        }
      }
    }

    // Check for unreachable tasks
    for (const [taskId] of this._tasks) {
      if (!visited.has(taskId)) {
        errors.push(`Task '${taskId}' is not reachable from start task`);
      }
    }
  }

  /**
   * Validate cancellation regions
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateCancellationRegions(errors, warnings) {
    for (const [regionId, taskIds] of this._regionToTasks) {
      // All tasks in region must exist
      for (const taskId of taskIds) {
        if (!this._tasks.has(taskId)) {
          errors.push(
            `Cancellation region '${regionId}' references non-existent task '${taskId}'`
          );
        }
      }

      // Region should have at least 2 tasks
      if (taskIds.length < 2) {
        warnings.push(`Cancellation region '${regionId}' has only ${taskIds.length} task(s)`);
      }
    }

    // Check cancellation sets reference valid tasks
    for (const [taskId, task] of this._tasks) {
      if (task.cancellationSet) {
        for (const cancelTaskId of task.cancellationSet) {
          if (!this._tasks.has(cancelTaskId)) {
            errors.push(
              `Task '${taskId}' cancellation set references non-existent task '${cancelTaskId}'`
            );
          }
        }
      }
    }
  }

  /**
   * Validate no invalid cycles exist
   * @param {string[]} errors - Error array to populate
   * @param {string[]} warnings - Warning array to populate
   * @private
   */
  _validateNoCycles(errors, warnings) {
    // Detect cycles using DFS
    const visited = new Set();
    const recStack = new Set();
    const cycles = [];

    const hasCycle = (taskId, path = []) => {
      if (recStack.has(taskId)) {
        const cycleStart = path.indexOf(taskId);
        cycles.push(path.slice(cycleStart).concat(taskId));
        return true;
      }
      if (visited.has(taskId)) {
        return false;
      }

      visited.add(taskId);
      recStack.add(taskId);
      path.push(taskId);

      const outgoing = this._outgoingFlows.get(taskId) ?? [];
      for (const flow of outgoing) {
        hasCycle(flow.to, path);
      }

      path.pop();
      recStack.delete(taskId);
      return false;
    };

    for (const [taskId] of this._tasks) {
      if (!visited.has(taskId)) {
        hasCycle(taskId, []);
      }
    }

    // In YAWL, some cycles are valid (loops), but we warn about them
    for (const cycle of cycles) {
      warnings.push(`Cycle detected: ${cycle.join(' -> ')}`);
    }
  }

  // ===========================================================================
  // Control Flow Evaluation
  // ===========================================================================

  /**
   * Evaluate which downstream tasks should be enabled after task completion
   * @param {string} completedTaskId - Task that completed
   * @param {Object} [context={}] - Evaluation context (data, case state)
   * @returns {string[]} Task IDs to enable
   *
   * @example
   * const toEnable = workflow.evaluateDownstream('review', { amount: 500 });
   * // Returns ['approve'] if amount < 1000
   */
  evaluateDownstream(completedTaskId, context = {}) {
    const taskDef = this._tasks.get(completedTaskId);
    if (!taskDef) return [];

    const outFlows = this._outgoingFlows.get(completedTaskId) ?? [];
    if (outFlows.length === 0) return [];

    const splitType = taskDef.splitType ?? SPLIT_TYPE.SEQUENCE;
    const toEnable = [];

    // Sort flows by priority (higher first)
    const sortedFlows = [...outFlows].sort(
      (a, b) => (b.priority ?? 0) - (a.priority ?? 0)
    );

    switch (splitType) {
      case SPLIT_TYPE.SEQUENCE:
        // Enable the single outgoing task
        if (sortedFlows.length > 0) {
          toEnable.push(sortedFlows[0].to);
        }
        break;

      case SPLIT_TYPE.AND:
        // Enable all outgoing tasks (parallel split)
        for (const flow of sortedFlows) {
          toEnable.push(flow.to);
        }
        break;

      case SPLIT_TYPE.XOR:
        // Enable first matching condition (exclusive choice)
        for (const flow of sortedFlows) {
          if (!flow.condition) {
            if (flow.isDefault) {
              // Save default for fallback
              continue;
            }
            toEnable.push(flow.to);
            break;
          }
          try {
            if (flow.condition(context)) {
              toEnable.push(flow.to);
              break;
            }
          } catch {
            // Skip flow on condition error
          }
        }
        // Use default flow if no match
        if (toEnable.length === 0) {
          const defaultFlow = sortedFlows.find(f => f.isDefault);
          if (defaultFlow) {
            toEnable.push(defaultFlow.to);
          } else if (sortedFlows.length > 0) {
            // Fallback to last flow if no default
            toEnable.push(sortedFlows[sortedFlows.length - 1].to);
          }
        }
        break;

      case SPLIT_TYPE.OR:
        // Enable all matching conditions (multi-choice)
        for (const flow of sortedFlows) {
          if (!flow.condition) {
            toEnable.push(flow.to);
          } else {
            try {
              if (flow.condition(context)) {
                toEnable.push(flow.to);
              }
            } catch {
              // Skip flow on condition error
            }
          }
        }
        // Must enable at least one
        if (toEnable.length === 0 && sortedFlows.length > 0) {
          toEnable.push(sortedFlows[0].to);
        }
        break;

      default:
        // Unknown split type - enable all
        for (const flow of sortedFlows) {
          toEnable.push(flow.to);
        }
    }

    return toEnable;
  }

  /**
   * Check if a task can be enabled based on join semantics
   * @param {string} taskId - Task to check
   * @param {Set<string>} completedTasks - Set of completed task IDs
   * @param {Set<string>} [activatedTasks=new Set()] - Set of tasks activated by OR-split
   * @returns {boolean} True if task can be enabled
   *
   * @example
   * const completed = new Set(['task-a', 'task-b']);
   * if (workflow.canEnable('merge-task', completed)) {
   *   // Enable the merge task
   * }
   */
  canEnable(taskId, completedTasks, activatedTasks = new Set()) {
    const taskDef = this._tasks.get(taskId);
    if (!taskDef) return false;

    const inFlows = this._incomingFlows.get(taskId) ?? [];
    if (inFlows.length === 0) return true; // Start task

    const joinType = taskDef.joinType ?? JOIN_TYPE.SEQUENCE;
    const incomingTaskIds = inFlows.map(f => f.from);

    switch (joinType) {
      case JOIN_TYPE.SEQUENCE:
        // Single incoming must be complete
        return incomingTaskIds.some(id => completedTasks.has(id));

      case JOIN_TYPE.AND:
        // All incoming must be complete (synchronization)
        return incomingTaskIds.every(id => completedTasks.has(id));

      case JOIN_TYPE.XOR:
        // Any one incoming complete (simple merge)
        return incomingTaskIds.some(id => completedTasks.has(id));

      case JOIN_TYPE.OR:
        // All ACTIVATED incoming must be complete (structured sync merge)
        const activated = incomingTaskIds.filter(id => activatedTasks.has(id));
        if (activated.length === 0) {
          // If none activated, any one complete suffices
          return incomingTaskIds.some(id => completedTasks.has(id));
        }
        return activated.every(id => completedTasks.has(id));

      default:
        return incomingTaskIds.some(id => completedTasks.has(id));
    }
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

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a new workflow from specification
 *
 * Factory function that validates the specification, builds task index,
 * constructs control flow graph, and returns a Workflow instance.
 *
 * @param {Object} spec - Workflow specification
 * @param {string} spec.id - Unique workflow identifier
 * @param {string} [spec.name] - Human-readable name
 * @param {string} [spec.version='1.0.0'] - Semantic version
 * @param {Array<Object>} spec.tasks - Task definitions
 * @param {Array<Object>} [spec.flows=[]] - Flow definitions
 * @param {Object} [options={}] - Creation options
 * @param {boolean} [options.validate=true] - Run validation after creation
 * @param {boolean} [options.strict=false] - Throw on validation errors
 * @returns {Workflow} Workflow instance
 *
 * @example
 * const workflow = createWorkflow({
 *   id: 'expense-approval',
 *   name: 'Expense Approval Process',
 *   tasks: [
 *     { id: 'submit', name: 'Submit Expense' },
 *     { id: 'review', name: 'Review Expense', splitType: 'xor' },
 *     { id: 'approve', name: 'Approve' },
 *     { id: 'reject', name: 'Reject' },
 *   ],
 *   flows: [
 *     { from: 'submit', to: 'review' },
 *     { from: 'review', to: 'approve', condition: ctx => ctx.amount < 1000 },
 *     { from: 'review', to: 'reject', isDefault: true },
 *   ],
 * }, { validate: true, strict: true });
 */
export function createWorkflow(spec, options = {}) {
  const { validate = true, strict = false } = options;

  // Create workflow instance
  const workflow = new Workflow(spec);

  // Run validation if requested
  if (validate) {
    const result = workflow.validate();

    if (!result.valid && strict) {
      throw new Error(`Workflow validation failed:\n${result.errors.join('\n')}`);
    }

    // Attach validation result for inspection
    workflow._validationResult = result;
  }

  return workflow;
}

// =============================================================================
// RDF Integration
// =============================================================================

/**
 * Split type to RDF node mapping
 */
const SPLIT_TYPE_TO_RDF = {
  sequence: null,
  and: AND_Split,
  xor: XOR_Split,
  or: OR_Split,
};

/**
 * Join type to RDF node mapping
 */
const JOIN_TYPE_TO_RDF = {
  sequence: null,
  and: AND_Join,
  xor: XOR_Join,
  or: OR_Join,
};

/**
 * Task kind to RDF node mapping
 */
const TASK_KIND_TO_RDF = {
  atomic: AtomicTask,
  composite: CompositeTask,
  multiple: MultipleInstanceTask,
};

/**
 * Serialize workflow to RDF representation in store
 *
 * Creates YAWL RDF representation using yawl-ontology predicates and classes.
 *
 * @param {Workflow} workflow - Workflow to serialize
 * @param {Object} store - RDF store (from @unrdf/oxigraph)
 * @param {Object} [options={}] - Serialization options
 * @param {string} [options.graph] - Named graph URI (defaults to spec-based)
 * @returns {Object} Result with specUri and quad count
 *
 * @example
 * import { createStore } from '@unrdf/oxigraph';
 * import { createWorkflow, workflowToRDF } from '@unrdf/yawl';
 *
 * const store = createStore();
 * const workflow = createWorkflow({ ... });
 * const { specUri, quadCount } = workflowToRDF(workflow, store);
 */
export function workflowToRDF(workflow, store, options = {}) {
  const specNode = specUri(workflow.id);
  const graphUri = options.graph || `${YAWL}specs/${workflow.id}`;
  const graph = namedNode(graphUri);

  let quadCount = 0;

  // Helper to add quad and count
  const addQuad = (s, p, o, g = graph) => {
    store.add(quad(s, p, o, g));
    quadCount++;
  };

  // Add workflow spec type
  addQuad(specNode, rdfType, WorkflowSpec);

  // Add workflow name
  if (workflow.name) {
    addQuad(specNode, rdfsLabel, stringLiteral(workflow.name));
  }

  // Add workflow version
  if (workflow.version) {
    addQuad(specNode, namedNode(YAWL + 'version'), stringLiteral(workflow.version));
  }

  // Add description
  if (workflow.description) {
    addQuad(specNode, namedNode(YAWL + 'description'), stringLiteral(workflow.description));
  }

  // Add tasks
  for (const task of workflow.getTasks()) {
    const taskNode = taskUri(task.id);

    // Link spec to task
    addQuad(specNode, hasTasks, taskNode);

    // Add task type
    addQuad(taskNode, rdfType, Task);

    // Add task ID
    addQuad(taskNode, taskIdProp, stringLiteral(task.id));

    // Add task name
    if (task.name) {
      addQuad(taskNode, taskNameProp, stringLiteral(task.name));
    }

    // Add task kind
    const kindNode = TASK_KIND_TO_RDF[task.kind];
    if (kindNode) {
      addQuad(taskNode, kind, kindNode);
    }

    // Add split behavior
    const splitNode = SPLIT_TYPE_TO_RDF[task.splitType];
    if (splitNode) {
      addQuad(taskNode, splitBehavior, splitNode);
    }

    // Add join behavior
    const joinNode = JOIN_TYPE_TO_RDF[task.joinType];
    if (joinNode) {
      addQuad(taskNode, joinBehavior, joinNode);
    }

    // Add cancellation set
    if (task.cancellationSet) {
      for (const cancelTaskId of task.cancellationSet) {
        addQuad(taskNode, cancellationSet, taskUri(cancelTaskId));
      }
    }

    // Add documentation
    if (task.documentation) {
      addQuad(taskNode, namedNode(YAWL + 'documentation'), stringLiteral(task.documentation));
    }

    // Add priority
    if (task.priority !== undefined) {
      addQuad(taskNode, namedNode(YAWL + 'priority'), integerLiteral(task.priority));
    }
  }

  // Add flows
  for (const flow of workflow.getFlows()) {
    const flowNode = flowUri(flow.from, flow.to);

    // Add flow type
    addQuad(flowNode, rdfType, Flow);

    // Add source and target
    addQuad(flowNode, sourceTask, taskUri(flow.from));
    addQuad(flowNode, targetTask, taskUri(flow.to));

    // Add outgoing/incoming relations
    addQuad(taskUri(flow.from), joinsTo, taskUri(flow.to));
    addQuad(taskUri(flow.to), joinsFrom, taskUri(flow.from));

    // Add priority
    if (flow.priority !== undefined && flow.priority !== 0) {
      addQuad(flowNode, flowPriority, integerLiteral(flow.priority));
    }

    // Add default flag
    if (flow.isDefault) {
      addQuad(flowNode, isDefaultFlow, booleanLiteral(true));
    }

    // Add documentation
    if (flow.documentation) {
      addQuad(flowNode, namedNode(YAWL + 'documentation'), stringLiteral(flow.documentation));
    }
  }

  // Add start/end markers
  if (workflow.getStartTaskId()) {
    addQuad(specNode, namedNode(YAWL + 'startTask'), taskUri(workflow.getStartTaskId()));
  }

  for (const endTaskId of workflow.getEndTaskIds()) {
    addQuad(specNode, namedNode(YAWL + 'endTask'), taskUri(endTaskId));
  }

  return {
    specUri: specNode.value,
    graph: graphUri,
    quadCount,
  };
}

/**
 * RDF split type to internal type mapping
 */
const RDF_TO_SPLIT_TYPE = {
  [AND_Split.value]: 'and',
  [XOR_Split.value]: 'xor',
  [OR_Split.value]: 'or',
};

/**
 * RDF join type to internal type mapping
 */
const RDF_TO_JOIN_TYPE = {
  [AND_Join.value]: 'and',
  [XOR_Join.value]: 'xor',
  [OR_Join.value]: 'or',
};

/**
 * RDF task kind to internal kind mapping
 */
const RDF_TO_TASK_KIND = {
  [AtomicTask.value]: 'atomic',
  [CompositeTask.value]: 'composite',
  [MultipleInstanceTask.value]: 'multiple',
};

/**
 * Load workflow from RDF store
 *
 * Reconstructs a Workflow instance from its RDF representation.
 *
 * @param {Object} store - RDF store (from @unrdf/oxigraph)
 * @param {string} workflowId - Workflow specification ID
 * @param {Object} [options={}] - Loading options
 * @param {string} [options.graph] - Named graph URI to query
 * @returns {Promise<Workflow|null>} Workflow instance or null if not found
 *
 * @example
 * import { createStore } from '@unrdf/oxigraph';
 * import { workflowFromRDF } from '@unrdf/yawl';
 *
 * const workflow = await workflowFromRDF(store, 'expense-approval');
 * if (workflow) {
 *   console.log(`Loaded workflow with ${workflow.getTasks().length} tasks`);
 * }
 */
export async function workflowFromRDF(store, workflowId, options = {}) {
  const specNode = specUri(workflowId);
  const graphUri = options.graph || `${YAWL}specs/${workflowId}`;
  const graph = namedNode(graphUri);

  // Check if workflow exists
  const typeQuads = store.match(specNode, rdfType, WorkflowSpec, graph);
  if (typeQuads.length === 0) {
    // Try without graph constraint
    const allTypeQuads = store.match(specNode, rdfType, WorkflowSpec, null);
    if (allTypeQuads.length === 0) {
      return null;
    }
  }

  // Get workflow name
  const labelQuads = store.match(specNode, rdfsLabel, null, null);
  const name = labelQuads.length > 0 ? labelQuads[0].object.value : workflowId;

  // Get workflow version
  const versionQuads = store.match(specNode, namedNode(YAWL + 'version'), null, null);
  const version = versionQuads.length > 0 ? versionQuads[0].object.value : '1.0.0';

  // Get description
  const descQuads = store.match(specNode, namedNode(YAWL + 'description'), null, null);
  const description = descQuads.length > 0 ? descQuads[0].object.value : undefined;

  // Get tasks
  const taskQuads = store.match(specNode, hasTasks, null, null);
  const tasks = [];

  for (const taskQuad of taskQuads) {
    const taskNode = taskQuad.object;

    // Get task ID
    const taskIdQuads = store.match(taskNode, taskIdProp, null, null);
    const taskId = taskIdQuads.length > 0
      ? taskIdQuads[0].object.value
      : taskNode.value.replace(YAWL_TASK, '');

    // Get task name
    const taskNameQuads = store.match(taskNode, taskNameProp, null, null);
    const taskName = taskNameQuads.length > 0 ? taskNameQuads[0].object.value : taskId;

    // Get task kind
    const kindQuads = store.match(taskNode, kind, null, null);
    let taskKind = 'atomic';
    if (kindQuads.length > 0) {
      taskKind = RDF_TO_TASK_KIND[kindQuads[0].object.value] ?? 'atomic';
    }

    // Get split behavior
    const splitQuads = store.match(taskNode, splitBehavior, null, null);
    let splitType = 'sequence';
    if (splitQuads.length > 0) {
      splitType = RDF_TO_SPLIT_TYPE[splitQuads[0].object.value] ?? 'sequence';
    }

    // Get join behavior
    const joinQuads = store.match(taskNode, joinBehavior, null, null);
    let joinType = 'sequence';
    if (joinQuads.length > 0) {
      joinType = RDF_TO_JOIN_TYPE[joinQuads[0].object.value] ?? 'sequence';
    }

    // Get cancellation set
    const cancelQuads = store.match(taskNode, cancellationSet, null, null);
    const cancelSet = cancelQuads.map(q => q.object.value.replace(YAWL_TASK, ''));

    // Get priority
    const priorityQuads = store.match(taskNode, namedNode(YAWL + 'priority'), null, null);
    const priority = priorityQuads.length > 0
      ? parseInt(priorityQuads[0].object.value, 10)
      : undefined;

    // Get documentation
    const docQuads = store.match(taskNode, namedNode(YAWL + 'documentation'), null, null);
    const documentation = docQuads.length > 0 ? docQuads[0].object.value : undefined;

    tasks.push({
      id: taskId,
      name: taskName,
      kind: taskKind,
      splitType,
      joinType,
      cancellationSet: cancelSet.length > 0 ? cancelSet : undefined,
      priority,
      documentation,
    });
  }

  // Get flows by querying joinsTo relationships
  const flows = [];
  for (const task of tasks) {
    const flowQuads = store.match(taskUri(task.id), joinsTo, null, null);
    for (const flowQuad of flowQuads) {
      const toTaskId = flowQuad.object.value.replace(YAWL_TASK, '');

      // Check for flow node with properties
      const flowNode = flowUri(task.id, toTaskId);
      const priorityQuads = store.match(flowNode, flowPriority, null, null);
      const priority = priorityQuads.length > 0
        ? parseInt(priorityQuads[0].object.value, 10)
        : 0;

      const defaultQuads = store.match(flowNode, isDefaultFlow, null, null);
      const isDefault = defaultQuads.length > 0
        && defaultQuads[0].object.value === 'true';

      flows.push({
        from: task.id,
        to: toTaskId,
        priority,
        isDefault: isDefault || undefined,
      });
    }
  }

  // Get start task
  const startQuads = store.match(specNode, namedNode(YAWL + 'startTask'), null, null);
  const startTaskId = startQuads.length > 0
    ? startQuads[0].object.value.replace(YAWL_TASK, '')
    : undefined;

  // Get end tasks
  const endQuads = store.match(specNode, namedNode(YAWL + 'endTask'), null, null);
  const endTaskIds = endQuads.map(q => q.object.value.replace(YAWL_TASK, ''));

  // Create workflow
  return new Workflow({
    id: workflowId,
    name,
    version,
    description,
    tasks,
    flows,
    startTaskId,
    endTaskIds,
  });
}

// =============================================================================
// Legacy Export (for backward compatibility)
// =============================================================================

/**
 * Legacy YawlWorkflow class (alias for Workflow)
 * @deprecated Use Workflow instead
 */
export const YawlWorkflow = Workflow;

// =============================================================================
// Module Exports
// =============================================================================

export default {
  // Main class
  Workflow,
  YawlWorkflow, // Legacy alias

  // Factory
  createWorkflow,

  // RDF integration
  workflowToRDF,
  workflowFromRDF,

  // Schemas
  WorkflowSpecSchema,
  TaskDefSchema,
  FlowDefSchema,

  // Split/Join constants (re-exported for convenience)
  SPLIT_TYPE,
  JOIN_TYPE,
};
