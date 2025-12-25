/**
 * @file YAWL Engine - Main workflow execution engine with KGC-4D integration
 * @module @unrdf/yawl/engine
 *
 * @description
 * Core coordinator that ties together all YAWL subsystems:
 * - workflow.mjs (workflow definitions)
 * - case.mjs (runtime state management)
 * - task.mjs (task state machine)
 * - receipt.mjs (cryptographic proofs)
 * - hooks/yawl-hooks.mjs (policy enforcement)
 * - events/yawl-events.mjs (KGC-4D event sourcing)
 *
 * Implements Van der Aalst's YAWL workflow patterns with:
 * - Time-travel debugging via KGC-4D
 * - Cryptographic receipts for auditability
 * - Hook-based policy enforcement
 * - Circuit breaker for resilience
 * - Event-driven architecture
 */

import { z } from 'zod';
import { KGCStore, GitBackbone, freezeUniverse, now, toISO } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { YawlWorkflow } from './workflow.mjs';
import { YawlCase, CaseStatus } from './case.mjs';
import { YawlTask, TaskStatus } from './task.mjs';
import { YawlResourcePool } from './resource.mjs';
import { buildReceipt, YawlReceipt } from './receipt.mjs';
import {
  createWorkflowReceipt,
  appendWorkflowEvent,
  getWorkflowAuditTrail,
  reconstructCase as kgcReconstructCase,
  YAWL_EVENT_TYPES,
} from './events/yawl-events.mjs';

// =============================================================================
// Constants
// =============================================================================

/** YAWL namespace for RDF */
export const YAWL_NS = 'http://yawl.io/';

/** Named graphs for YAWL data */
export const YAWL_GRAPHS = Object.freeze({
  WORKFLOWS: `${YAWL_NS}workflows`,
  CASES: `${YAWL_NS}cases`,
  WORKITEMS: `${YAWL_NS}workitems`,
  EVENTS: `${YAWL_NS}events`,
});

/** Engine event types for subscription */
export const ENGINE_EVENTS = Object.freeze({
  TASK_ENABLED: 'task:enabled',
  TASK_STARTED: 'task:started',
  TASK_COMPLETED: 'task:completed',
  TASK_CANCELLED: 'task:cancelled',
  TASK_FAILED: 'task:failed',
  TASK_TIMEOUT: 'task:timeout',
  CASE_CREATED: 'case:created',
  CASE_STARTED: 'case:started',
  CASE_COMPLETED: 'case:completed',
  CASE_FAILED: 'case:failed',
  CASE_CANCELLED: 'case:cancelled',
  WORKFLOW_REGISTERED: 'workflow:registered',
  CIRCUIT_BREAKER_OPEN: 'circuit:open',
  CIRCUIT_BREAKER_CLOSE: 'circuit:close',
  CHECKPOINT_CREATED: 'checkpoint:created',
  RESOURCE_ALLOCATED: 'resource:allocated',
  RESOURCE_RELEASED: 'resource:released',
});

/** Engine health status */
export const HealthStatus = Object.freeze({
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
});

// =============================================================================
// Configuration Schema
// =============================================================================

/**
 * Engine configuration schema
 */
const EngineConfigSchema = z.object({
  /** Node ID for distributed coordination */
  nodeId: z.string().optional(),
  /** Path for Git backbone storage */
  gitPath: z.string().optional(),
  /** Enable event logging to KGC-4D */
  enableEventLog: z.boolean().default(true),
  /** Enable snapshots for time-travel */
  enableSnapshots: z.boolean().default(true),
  /** Snapshot interval in milliseconds */
  snapshotInterval: z.number().positive().default(60000),
  /** Circuit breaker failure threshold */
  circuitBreakerThreshold: z.number().int().positive().default(5),
  /** Circuit breaker reset timeout in ms */
  circuitBreakerResetTimeout: z.number().int().positive().default(30000),
  /** Maximum concurrent cases */
  maxConcurrentCases: z.number().int().positive().default(1000),
  /** Default task timeout in ms */
  defaultTaskTimeout: z.number().int().positive().default(30000),
  /** External KGC store (optional) */
  store: z.any().optional(),
  /** External hook adapter (optional) */
  hookAdapter: z.any().optional(),
  /** External KGC adapter (optional) */
  kgcAdapter: z.any().optional(),
  /** External supervisor adapter (optional) */
  supervisorAdapter: z.any().optional(),
});

// =============================================================================
// WorkflowEngine Class
// =============================================================================

/**
 * Main YAWL workflow engine with KGC-4D time-travel support
 *
 * Coordinates all subsystems:
 * - Workflow registration and management
 * - Case lifecycle management
 * - Task execution with state machine
 * - Resource allocation
 * - Event sourcing with time-travel
 * - Hook-based policy enforcement
 * - Circuit breaker for resilience
 *
 * @example
 * ```javascript
 * const engine = createWorkflowEngine({
 *   nodeId: 'node-1',
 *   enableEventLog: true,
 *   enableSnapshots: true
 * });
 *
 * // Register workflow
 * const workflow = new YawlWorkflow({ id: 'approval', name: 'Approval Process' });
 * workflow.addTask({ id: 'submit', name: 'Submit Request' });
 * workflow.addTask({ id: 'review', name: 'Review Request' });
 * workflow.addFlow({ from: 'submit', to: 'review' });
 * workflow.setStart('submit');
 * workflow.setEnd(['review']);
 * engine.registerWorkflow(workflow);
 *
 * // Create and run case
 * const { case: approvalCase } = await engine.createCase('approval', { requestId: '123' });
 *
 * // Subscribe to events
 * engine.on('task:completed', (event) => {
 *   console.log('Task completed:', event.taskId);
 * });
 * ```
 */
export class WorkflowEngine {
  /**
   * Create a new workflow engine
   * @param {Object} config - Engine configuration
   */
  constructor(config = {}) {
    const validated = EngineConfigSchema.parse(config);

    // Core configuration
    this.nodeId = validated.nodeId ?? `yawl-${Date.now()}`;
    this.gitPath = validated.gitPath;
    this.enableEventLog = validated.enableEventLog;
    this.enableSnapshots = validated.enableSnapshots;
    this.snapshotInterval = validated.snapshotInterval;
    this.circuitBreakerThreshold = validated.circuitBreakerThreshold;
    this.circuitBreakerResetTimeout = validated.circuitBreakerResetTimeout;
    this.maxConcurrentCases = validated.maxConcurrentCases;
    this.defaultTaskTimeout = validated.defaultTaskTimeout;

    // External adapters
    this._hookAdapter = validated.hookAdapter;
    this._kgcAdapter = validated.kgcAdapter;
    this._supervisorAdapter = validated.supervisorAdapter;

    // Core stores
    /** @type {KGCStore} KGC-4D store for event sourcing */
    this.store = validated.store ?? new KGCStore({ nodeId: this.nodeId });
    /** @type {GitBackbone|null} Git backbone for snapshots */
    this.git = this.gitPath ? new GitBackbone(this.gitPath) : null;

    // Workflow management
    /** @type {Map<string, YawlWorkflow>} Registered workflows by ID */
    this.workflows = new Map();
    /** @type {Map<string, YawlCase>} Active cases by ID */
    this.cases = new Map();

    // Resource management
    /** @type {YawlResourcePool} Resource pool for allocation */
    this.resourcePool = new YawlResourcePool();

    // Event sourcing
    /** @type {Array<Object>} In-memory event log */
    this.events = [];
    /** @type {Map<bigint, Object>} Checkpoints for time-travel */
    this.checkpoints = new Map();

    // Event subscription
    /** @type {Map<string, Set<Function>>} Event handlers by event type */
    this._eventHandlers = new Map();

    // Hook registry
    /** @type {Map<string, Object>} Registered policy packs by workflow ID */
    this._policyPacks = new Map();

    // Circuit breaker state
    /** @type {Map<string, Object>} Circuit breaker state by task/workflow ID */
    this._circuitBreakers = new Map();

    // Statistics
    /** @type {Object} Engine statistics */
    this._stats = {
      casesCreated: 0,
      casesCompleted: 0,
      casesFailed: 0,
      casesCancelled: 0,
      tasksEnabled: 0,
      tasksStarted: 0,
      tasksCompleted: 0,
      tasksFailed: 0,
      tasksCancelled: 0,
      tasksTimedOut: 0,
      circuitBreakerTrips: 0,
      checkpointsCreated: 0,
      eventsLogged: 0,
      startedAt: now(),
    };

    // Health check state
    /** @type {Object} Health check state */
    this._health = {
      status: HealthStatus.HEALTHY,
      lastCheck: now(),
      components: {
        store: true,
        git: this.git ? true : null,
        workflows: true,
        cases: true,
      },
      errors: [],
    };

    // Initialize snapshot timer if enabled
    this._snapshotTimer = null;
    if (this.enableSnapshots && this.git) {
      this._startSnapshotTimer();
    }
  }

  // ===========================================================================
  // Workflow Management
  // ===========================================================================

  /**
   * Register a workflow definition
   * @param {YawlWorkflow|Object} workflowOrSpec - Workflow or specification object
   * @returns {YawlWorkflow} Registered workflow
   * @throws {Error} If workflow is invalid
   */
  registerWorkflow(workflowOrSpec) {
    const workflow =
      workflowOrSpec instanceof YawlWorkflow
        ? workflowOrSpec
        : new YawlWorkflow(workflowOrSpec);

    const validation = workflow.validate();
    if (!validation.valid) {
      throw new Error(`Invalid workflow: ${validation.errors.join(', ')}`);
    }

    workflow.lock();
    this.workflows.set(workflow.id, workflow);

    this._appendEvent({
      type: 'WORKFLOW_REGISTERED',
      workflowId: workflow.id,
      version: workflow.version,
    });

    this.emit(ENGINE_EVENTS.WORKFLOW_REGISTERED, {
      workflowId: workflow.id,
      version: workflow.version,
      name: workflow.name,
    });

    return workflow;
  }

  /**
   * Load a workflow from the store
   * @param {string} workflowId - Workflow ID to load
   * @returns {Promise<YawlWorkflow|null>} Workflow if found
   */
  async loadWorkflow(workflowId) {
    // First check in-memory cache
    if (this.workflows.has(workflowId)) {
      return this.workflows.get(workflowId);
    }

    // Query from RDF store if available
    const workflowGraph = dataFactory.namedNode(`${YAWL_GRAPHS.WORKFLOWS}/${workflowId}`);
    const quads = [...this.store.match(null, null, null, workflowGraph)];

    if (quads.length === 0) {
      return null;
    }

    // Reconstruct workflow from quads (simplified - full impl would parse RDF)
    // For now, return null if not in memory
    return null;
  }

  /**
   * Get a registered workflow
   * @param {string} workflowId - Workflow ID
   * @returns {YawlWorkflow|undefined} Workflow if found
   */
  getWorkflow(workflowId) {
    return this.workflows.get(workflowId);
  }

  /**
   * Get all registered workflows
   * @returns {YawlWorkflow[]} Array of workflows
   */
  getAllWorkflows() {
    return [...this.workflows.values()];
  }

  // ===========================================================================
  // Policy Pack Integration
  // ===========================================================================

  /**
   * Register a policy pack for a workflow
   * @param {string} workflowId - Workflow ID
   * @param {Object} policyPack - Policy pack from createYAWLPolicyPack
   * @returns {void}
   */
  registerPolicyPack(workflowId, policyPack) {
    if (!this.workflows.has(workflowId)) {
      throw new Error(`Workflow ${workflowId} not found`);
    }
    this._policyPacks.set(workflowId, policyPack);
  }

  /**
   * Get policy pack for a workflow
   * @param {string} workflowId - Workflow ID
   * @returns {Object|undefined} Policy pack if registered
   */
  getPolicyPack(workflowId) {
    return this._policyPacks.get(workflowId);
  }

  // ===========================================================================
  // Case Management
  // ===========================================================================

  /**
   * Create a new case for a workflow
   * @param {string} workflowId - Workflow ID
   * @param {Object} [initialData={}] - Initial case data
   * @param {Object} [options={}] - Additional options
   * @returns {Promise<{case: YawlCase, receipt: YawlReceipt}>}
   * @throws {Error} If workflow not found or max cases exceeded
   */
  async createCase(workflowId, initialData = {}, options = {}) {
    const workflow = this.workflows.get(workflowId);
    if (!workflow) {
      throw new Error(`Workflow ${workflowId} not found`);
    }

    // Check capacity
    if (this.cases.size >= this.maxConcurrentCases) {
      throw new Error(`Maximum concurrent cases (${this.maxConcurrentCases}) exceeded`);
    }

    const caseId = `case-${workflow.id}-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;
    const yawlCase = new YawlCase(
      { id: caseId, workflowId, data: initialData },
      workflow
    );

    this.cases.set(caseId, yawlCase);
    this._stats.casesCreated++;

    // Log to KGC-4D if enabled
    if (this.enableEventLog) {
      await this._logCaseEvent(YAWL_EVENT_TYPES.CASE_CREATED, {
        caseId,
        specId: workflowId,
        data: initialData,
      });
    }

    this._appendEvent({
      type: 'CASE_CREATED',
      caseId,
      workflowId,
      data: initialData,
    });

    this.emit(ENGINE_EVENTS.CASE_CREATED, {
      caseId,
      workflowId,
      data: initialData,
    });

    // Start the case (enable start task)
    const startResult = await yawlCase.start();

    this._appendEvent({
      type: 'CASE_STARTED',
      caseId,
      taskId: workflow.startTaskId,
      workItemId: startResult.task.id,
    });

    this.emit(ENGINE_EVENTS.CASE_STARTED, {
      caseId,
      workflowId,
      taskId: workflow.startTaskId,
      workItemId: startResult.task.id,
    });

    this.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId: workflow.startTaskId,
      workItemId: startResult.task.id,
    });

    this._stats.tasksEnabled++;

    return { case: yawlCase, receipt: startResult.receipt };
  }

  /**
   * Get a case by ID
   * @param {string} caseId - Case ID
   * @returns {YawlCase|undefined} Case if found
   */
  getCase(caseId) {
    return this.cases.get(caseId);
  }

  /**
   * Get case status
   * @param {string} caseId - Case ID
   * @returns {Object} Case status details
   * @throws {Error} If case not found
   */
  getCaseStatus(caseId) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    return {
      caseId,
      workflowId: yawlCase.workflowId,
      status: yawlCase.status,
      createdAt: yawlCase.createdAt?.toString(),
      startedAt: yawlCase.startedAt?.toString(),
      completedAt: yawlCase.completedAt?.toString(),
      enabledWorkItems: yawlCase.getEnabledWorkItems().length,
      runningWorkItems: yawlCase.getRunningWorkItems().length,
      completedTasks: [...yawlCase.completedTasks],
      receiptCount: yawlCase.receipts.length,
    };
  }

  /**
   * Get all active cases
   * @returns {YawlCase[]} Array of active cases
   */
  getActiveCases() {
    return [...this.cases.values()].filter(
      c => c.status === CaseStatus.RUNNING
    );
  }

  /**
   * Get all cases for a workflow
   * @param {string} workflowId - Workflow ID
   * @returns {YawlCase[]} Array of cases
   */
  getCasesForWorkflow(workflowId) {
    return [...this.cases.values()].filter(
      c => c.workflowId === workflowId
    );
  }

  // ===========================================================================
  // Task Execution
  // ===========================================================================

  /**
   * Enable a task in a case
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task definition ID
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
   * @throws {Error} If case/task not found or circuit breaker open
   */
  async enableTask(caseId, taskId, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    // Check circuit breaker
    const breakerKey = `${yawlCase.workflowId}:${taskId}`;
    if (this._isCircuitOpen(breakerKey)) {
      throw new Error(`Circuit breaker open for task ${taskId}`);
    }

    // Run pre-enablement hook if policy pack exists
    const policyPack = this._policyPacks.get(yawlCase.workflowId);
    if (policyPack && policyPack.getValidator) {
      const validator = policyPack.getValidator(taskId);
      if (validator) {
        const validation = await validator(this.store, { caseId, actor });
        if (!validation.valid) {
          throw new Error(`Task enablement denied: ${validation.receipt?.justification?.reason || 'Unknown'}`);
        }
      }
    }

    const result = await yawlCase.enableTask(taskId, actor);

    this._appendEvent({
      type: 'TASK_ENABLED',
      caseId,
      taskId,
      workItemId: result.task.id,
      actor,
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_ENABLED, {
        caseId,
        taskId,
        workItemId: result.task.id,
        enabledAt: toISO(result.task.enabledAt),
      });
    }

    this.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId,
      workItemId: result.task.id,
      actor,
    });

    this._stats.tasksEnabled++;

    return result;
  }

  /**
   * Start a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @param {Object} [options={}] - Options
   * @param {string} [options.resourceId] - Resource to allocate
   * @param {string} [options.actor] - Actor
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt, resource?: Object}>}
   */
  async startTask(caseId, workItemId, options = {}) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Try to allocate resource if role specified
    let allocatedResource;
    if (task.role || options.resourceId) {
      const allocation = this.resourcePool.allocate({
        taskId: workItemId,
        role: task.role,
        preferredResourceId: options.resourceId,
      });

      if (allocation.queued) {
        throw new Error(`No available resources for role ${task.role}`);
      }

      allocatedResource = allocation.resource;

      this.emit(ENGINE_EVENTS.RESOURCE_ALLOCATED, {
        caseId,
        workItemId,
        resourceId: allocatedResource.id,
        role: task.role,
      });
    }

    const result = await yawlCase.startTask(
      workItemId,
      allocatedResource?.id ?? options.resourceId,
      options.actor
    );

    this._appendEvent({
      type: 'TASK_STARTED',
      caseId,
      workItemId,
      resourceId: allocatedResource?.id ?? options.resourceId,
      actor: options.actor,
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_STARTED, {
        workItemId,
        startedAt: toISO(result.task.startedAt),
      }, caseId);
    }

    this.emit(ENGINE_EVENTS.TASK_STARTED, {
      caseId,
      workItemId,
      resourceId: allocatedResource?.id ?? options.resourceId,
      actor: options.actor,
    });

    this._stats.tasksStarted++;

    return { ...result, resource: allocatedResource };
  }

  /**
   * Complete a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID (or taskId for convenience)
   * @param {Object} [output={}] - Task output
   * @param {string} [actor] - Actor
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt, downstreamEnabled: Array}>}
   */
  async completeTask(caseId, workItemId, output = {}, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Verify task is active
    if (task.status !== 'running' && task.status !== TaskStatus.ACTIVE) {
      throw new Error(`Task ${workItemId} is not running (status: ${task.status})`);
    }

    // Release resource if allocated
    if (task.assignedResource) {
      const nextFromQueue = this.resourcePool.release(task.assignedResource);

      this.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
        caseId,
        workItemId,
        resourceId: task.assignedResource,
      });

      if (nextFromQueue) {
        this._appendEvent({
          type: 'RESOURCE_REALLOCATED',
          resourceId: nextFromQueue.resource.id,
          fromWorkItemId: workItemId,
          toTaskId: nextFromQueue.taskId,
        });
      }
    }

    // Run post-completion hook if policy pack exists
    const policyPack = this._policyPacks.get(yawlCase.workflowId);
    const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

    let hookRouting = null;
    if (policyPack && policyPack.getRouter) {
      const router = policyPack.getRouter(taskDefId);
      if (router) {
        hookRouting = await router(this.store, {
          caseId,
          actor,
          output,
          env: output,
        });
      }
    }

    const result = await yawlCase.completeTask(workItemId, output, actor);

    // Reset circuit breaker on success
    const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
    this._resetCircuitBreaker(breakerKey);

    this._appendEvent({
      type: 'TASK_COMPLETED',
      caseId,
      workItemId,
      output,
      actor,
      downstreamEnabled: result.downstreamEnabled.map(d => d.taskId),
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_COMPLETED, {
        workItemId,
        completedAt: toISO(result.task.completedAt),
        result: output,
      }, caseId);
    }

    this.emit(ENGINE_EVENTS.TASK_COMPLETED, {
      caseId,
      workItemId,
      taskId: taskDefId,
      output,
      actor,
      downstreamEnabled: result.downstreamEnabled,
      hookReceipt: hookRouting?.receipt,
    });

    this._stats.tasksCompleted++;

    // Emit events for downstream enabled tasks
    for (const downstream of result.downstreamEnabled) {
      this.emit(ENGINE_EVENTS.TASK_ENABLED, {
        caseId,
        taskId: downstream.taskId,
        workItemId: downstream.workItemId,
      });
      this._stats.tasksEnabled++;
    }

    // Check if case completed
    if (yawlCase.status === CaseStatus.COMPLETED) {
      this._appendEvent({
        type: 'CASE_COMPLETED',
        caseId,
      });

      this.emit(ENGINE_EVENTS.CASE_COMPLETED, {
        caseId,
        workflowId: yawlCase.workflowId,
      });

      this._stats.casesCompleted++;
    }

    return result;
  }

  /**
   * Cancel a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
   */
  async cancelTask(caseId, workItemId, reason, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Release resource if allocated
    if (task.assignedResource) {
      this.resourcePool.release(task.assignedResource);

      this.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
        caseId,
        workItemId,
        resourceId: task.assignedResource,
      });
    }

    const result = await yawlCase.cancelTask(workItemId, reason, actor);

    // Handle cancellation propagation if policy pack exists
    const policyPack = this._policyPacks.get(yawlCase.workflowId);
    const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

    if (policyPack && policyPack.getCancellationHandler) {
      const handler = policyPack.getCancellationHandler(taskDefId);
      if (handler) {
        handler(reason, { caseId, actor });
      }
    }

    this._appendEvent({
      type: 'TASK_CANCELLED',
      caseId,
      workItemId,
      reason,
      actor,
    });

    if (this.enableEventLog) {
      await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_CANCELLED, {
        workItemId,
        cancelledAt: toISO(now()),
        reason: reason || 'No reason provided',
      }, caseId);
    }

    this.emit(ENGINE_EVENTS.TASK_CANCELLED, {
      caseId,
      workItemId,
      taskId: taskDefId,
      reason,
      actor,
    });

    this._stats.tasksCancelled++;

    return result;
  }

  /**
   * Cancel all tasks in a cancellation region
   * @param {string} caseId - Case ID
   * @param {string} regionId - Cancellation region ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor
   * @returns {Promise<{cancelled: Array, receipts: Array}>}
   */
  async cancelRegion(caseId, regionId, reason, actor) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const result = await yawlCase.cancelRegion(regionId, reason, actor);

    this._appendEvent({
      type: 'REGION_CANCELLED',
      caseId,
      regionId,
      cancelledCount: result.cancelled.length,
      reason,
      actor,
    });

    for (const task of result.cancelled) {
      this.emit(ENGINE_EVENTS.TASK_CANCELLED, {
        caseId,
        workItemId: task.id,
        reason: `Region ${regionId} cancelled: ${reason || 'No reason'}`,
        actor,
      });
      this._stats.tasksCancelled++;
    }

    return result;
  }

  /**
   * Set circuit breaker state for a task
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task definition ID
   * @param {boolean} enabled - Enable or disable
   * @returns {Promise<{cancelled: Array}>}
   */
  async setCircuitBreaker(caseId, taskId, enabled) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const result = await yawlCase.setCircuitBreaker(taskId, enabled);

    this._appendEvent({
      type: 'CIRCUIT_BREAKER_SET',
      caseId,
      taskId,
      enabled,
      cancelledCount: result.cancelled.length,
    });

    if (!enabled) {
      this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
        caseId,
        taskId,
        cancelledCount: result.cancelled.length,
      });
    } else {
      this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, {
        caseId,
        taskId,
      });
    }

    return result;
  }

  /**
   * Handle timeout for a work item
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @returns {Promise<{task: YawlTask, receipt: YawlReceipt}>}
   */
  async timeoutTask(caseId, workItemId) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    const task = yawlCase.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    // Release resource if allocated
    if (task.assignedResource) {
      this.resourcePool.release(task.assignedResource);
    }

    const beforeState = yawlCase.getState();
    task.timedOut();
    const afterState = yawlCase.getState();

    const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);
    const previousReceipt = yawlCase.receipts[yawlCase.receipts.length - 1];

    const receipt = await buildReceipt({
      caseId,
      taskId: taskDefId,
      action: 'timeout',
      beforeState,
      afterState,
      previousReceipt,
    });

    yawlCase.receipts.push(receipt);

    // Increment circuit breaker failure count
    const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
    this._recordCircuitFailure(breakerKey);

    this._appendEvent({
      type: 'TASK_TIMEOUT',
      caseId,
      workItemId,
    });

    this.emit(ENGINE_EVENTS.TASK_TIMEOUT, {
      caseId,
      workItemId,
      taskId: taskDefId,
    });

    this._stats.tasksTimedOut++;

    return { task, receipt };
  }

  // ===========================================================================
  // Time-Travel
  // ===========================================================================

  /**
   * Create a checkpoint for time-travel
   * @param {string} [label] - Optional checkpoint label
   * @returns {Promise<{timestamp: bigint, hash: string}>}
   */
  async checkpoint(label) {
    if (!this.git) {
      throw new Error('Git backbone required for checkpoints');
    }

    const freezeResult = await freezeUniverse(this.store, this.git);

    // Store checkpoint with case states
    const caseStates = new Map();
    for (const [caseId, yawlCase] of this.cases) {
      caseStates.set(caseId, yawlCase.toJSON());
    }

    this.checkpoints.set(BigInt(freezeResult.t_ns), {
      label,
      hash: freezeResult.universe_hash,
      gitRef: freezeResult.git_ref,
      caseStates: Object.fromEntries(caseStates),
      events: [...this.events],
    });

    this._stats.checkpointsCreated++;

    this.emit(ENGINE_EVENTS.CHECKPOINT_CREATED, {
      timestamp: BigInt(freezeResult.t_ns).toString(),
      hash: freezeResult.universe_hash,
      label,
    });

    return {
      timestamp: BigInt(freezeResult.t_ns),
      hash: freezeResult.universe_hash,
    };
  }

  /**
   * Replay a case to a specific point in time
   * @param {string} caseId - Case ID
   * @param {bigint} targetTime - Target timestamp (nanoseconds)
   * @returns {Promise<{state: Object, events: Array, verified: boolean}>}
   */
  async replayCase(caseId, targetTime) {
    // If Git backbone available, use KGC-4D reconstruction
    if (this.git) {
      try {
        return await kgcReconstructCase(this.store, this.git, caseId, targetTime);
      } catch {
        // Fall back to checkpoint-based reconstruction
      }
    }

    // Find the checkpoint before or at timestamp
    let targetCheckpoint = null;
    let targetCheckpointTime = 0n;

    for (const [checkpointTime, checkpoint] of this.checkpoints) {
      if (checkpointTime <= targetTime && checkpointTime > targetCheckpointTime) {
        targetCheckpointTime = checkpointTime;
        targetCheckpoint = checkpoint;
      }
    }

    if (!targetCheckpoint) {
      throw new Error(`No checkpoint found before ${targetTime}`);
    }

    // Get case state from checkpoint
    const caseState = targetCheckpoint.caseStates[caseId];
    if (!caseState) {
      throw new Error(`Case ${caseId} not found in checkpoint`);
    }

    // Filter events to only those before timestamp
    const eventsBeforeTimestamp = targetCheckpoint.events.filter(
      e => BigInt(e.timestamp) <= targetTime
    );

    // Verify receipt chain
    const yawlCase = this.cases.get(caseId);
    let verified = true;

    if (yawlCase && yawlCase.receipts.length > 0) {
      for (let i = 0; i < yawlCase.receipts.length; i++) {
        const receipt = yawlCase.receipts[i];
        const previous = i > 0 ? yawlCase.receipts[i - 1] : null;

        if (BigInt(receipt.timestamp) > targetTime) break;

        const chainResult = await receipt.verifyChain(previous);
        if (!chainResult.valid) {
          verified = false;
          break;
        }
      }
    }

    return {
      state: caseState,
      events: eventsBeforeTimestamp.filter(e => e.caseId === caseId),
      verified,
      reconstructedAt: toISO(targetTime),
    };
  }

  /**
   * Get complete event history for a case
   * @param {string} caseId - Case ID
   * @returns {Promise<Object>} Audit trail with events and receipts
   */
  async getCaseHistory(caseId) {
    // If KGC-4D event log enabled, get from store
    if (this.enableEventLog) {
      try {
        return await getWorkflowAuditTrail(this.store, caseId);
      } catch {
        // Fall back to in-memory events
      }
    }

    // Get from in-memory event log
    const caseEvents = this.events.filter(e => e.caseId === caseId);

    // Get receipts from case if available
    const yawlCase = this.cases.get(caseId);
    const receipts = yawlCase
      ? yawlCase.receipts.map(r => r.toJSON())
      : [];

    return {
      caseId,
      events: caseEvents,
      receipts,
      eventCount: caseEvents.length,
      exportedAt: toISO(now()),
    };
  }

  /**
   * Replay workflow to a specific receipt
   * @param {string} caseId - Case ID
   * @param {string} receiptId - Target receipt ID
   * @returns {Promise<{state: Object, verified: boolean}>}
   */
  async replayToReceipt(caseId, receiptId) {
    const yawlCase = this.cases.get(caseId);
    if (!yawlCase) {
      throw new Error(`Case ${caseId} not found`);
    }

    // Find target receipt
    const targetReceiptIndex = yawlCase.receipts.findIndex(r => r.id === receiptId);
    if (targetReceiptIndex === -1) {
      throw new Error(`Receipt ${receiptId} not found`);
    }

    const targetReceipt = yawlCase.receipts[targetReceiptIndex];

    // Verify receipt chain up to target
    let verified = true;
    for (let i = 0; i <= targetReceiptIndex; i++) {
      const receipt = yawlCase.receipts[i];
      const previous = i > 0 ? yawlCase.receipts[i - 1] : null;

      const chainResult = await receipt.verifyChain(previous);
      if (!chainResult.valid) {
        verified = false;
        break;
      }
    }

    return {
      state: {
        afterHash: targetReceipt.afterHash,
        timestamp: targetReceipt.timestamp,
        action: targetReceipt.action,
        taskId: targetReceipt.taskId,
      },
      verified,
    };
  }

  // ===========================================================================
  // Resource Management
  // ===========================================================================

  /**
   * Add a resource to the pool
   * @param {Object} resourceData - Resource data
   * @returns {Object} Added resource
   */
  addResource(resourceData) {
    return this.resourcePool.addResource(resourceData);
  }

  /**
   * Remove a resource from the pool
   * @param {string} resourceId - Resource ID
   * @returns {boolean} True if removed
   */
  removeResource(resourceId) {
    return this.resourcePool.removeResource(resourceId);
  }

  /**
   * Get resource pool statistics
   * @returns {Object} Resource stats
   */
  getResourceStats() {
    return this.resourcePool.getStats();
  }

  // ===========================================================================
  // Event Subscription
  // ===========================================================================

  /**
   * Subscribe to engine events
   * @param {string} eventType - Event type from ENGINE_EVENTS
   * @param {Function} handler - Event handler function
   * @returns {Function} Unsubscribe function
   *
   * @example
   * ```javascript
   * const unsubscribe = engine.on('task:completed', (event) => {
   *   console.log('Task completed:', event.taskId, event.output);
   * });
   *
   * // Later...
   * unsubscribe();
   * ```
   */
  on(eventType, handler) {
    if (typeof handler !== 'function') {
      throw new TypeError('Handler must be a function');
    }

    if (!this._eventHandlers.has(eventType)) {
      this._eventHandlers.set(eventType, new Set());
    }

    this._eventHandlers.get(eventType).add(handler);

    // Return unsubscribe function
    return () => {
      const handlers = this._eventHandlers.get(eventType);
      if (handlers) {
        handlers.delete(handler);
      }
    };
  }

  /**
   * Emit an event to all subscribers
   * @param {string} eventType - Event type
   * @param {Object} data - Event data
   */
  emit(eventType, data) {
    const handlers = this._eventHandlers.get(eventType);
    if (!handlers) return;

    const event = {
      type: eventType,
      timestamp: now().toString(),
      timestampISO: toISO(now()),
      ...data,
    };

    for (const handler of handlers) {
      try {
        handler(event);
      } catch (error) {
        // Log error but don't throw to avoid disrupting other handlers
        console.error(`Error in event handler for ${eventType}:`, error);
      }
    }
  }

  /**
   * Remove all handlers for an event type
   * @param {string} eventType - Event type
   */
  off(eventType) {
    this._eventHandlers.delete(eventType);
  }

  // ===========================================================================
  // Health & Statistics
  // ===========================================================================

  /**
   * Perform health check
   * @returns {Object} Health status
   */
  healthCheck() {
    const errors = [];

    // Check store
    let storeHealthy = true;
    try {
      // Simple store check - try to get size
      this.store.size;
    } catch (e) {
      storeHealthy = false;
      errors.push(`Store error: ${e.message}`);
    }

    // Check Git backbone if configured
    let gitHealthy = null;
    if (this.git) {
      gitHealthy = true; // Would add actual Git health check here
    }

    // Check workflow count
    const workflowHealthy = this.workflows.size > 0 || true;

    // Check case capacity
    const casesHealthy = this.cases.size < this.maxConcurrentCases;
    if (!casesHealthy) {
      errors.push(`Case capacity reached: ${this.cases.size}/${this.maxConcurrentCases}`);
    }

    // Determine overall status
    let status = HealthStatus.HEALTHY;
    if (!storeHealthy) {
      status = HealthStatus.UNHEALTHY;
    } else if (!casesHealthy || errors.length > 0) {
      status = HealthStatus.DEGRADED;
    }

    this._health = {
      status,
      lastCheck: now(),
      components: {
        store: storeHealthy,
        git: gitHealthy,
        workflows: workflowHealthy,
        cases: casesHealthy,
      },
      errors,
      uptime: now() - this._stats.startedAt,
    };

    return this._health;
  }

  /**
   * Get engine statistics
   * @returns {Object} Engine stats
   */
  getStats() {
    return {
      ...this._stats,
      uptimeNs: (now() - this._stats.startedAt).toString(),
      uptimeMs: Number(now() - this._stats.startedAt) / 1_000_000,
      workflowCount: this.workflows.size,
      activeCaseCount: [...this.cases.values()].filter(
        c => c.status === CaseStatus.RUNNING
      ).length,
      totalCaseCount: this.cases.size,
      checkpointCount: this.checkpoints.size,
      eventCount: this.events.length,
      resourceStats: this.resourcePool.getStats(),
      circuitBreakers: Object.fromEntries(this._circuitBreakers),
    };
  }

  // ===========================================================================
  // Event Log (Internal)
  // ===========================================================================

  /**
   * Append an event to the in-memory log
   * @param {Object} eventData - Event data
   * @private
   */
  _appendEvent(eventData) {
    const timestamp = now();
    this.events.push({
      ...eventData,
      timestamp: timestamp.toString(),
      timestampISO: toISO(timestamp),
    });
    this._stats.eventsLogged++;
  }

  /**
   * Get events for a case
   * @param {string} caseId - Case ID
   * @returns {Array} Events for the case
   */
  getEventsForCase(caseId) {
    return this.events.filter(e => e.caseId === caseId);
  }

  /**
   * Get all events
   * @returns {Array} All events
   */
  getAllEvents() {
    return [...this.events];
  }

  // ===========================================================================
  // KGC-4D Integration (Internal)
  // ===========================================================================

  /**
   * Log a case event to KGC-4D
   * @param {string} eventType - Event type
   * @param {Object} payload - Event payload
   * @private
   */
  async _logCaseEvent(eventType, payload) {
    try {
      const receipt = await createWorkflowReceipt({
        beforeState: { empty: true },
        afterState: payload,
        decision: { action: eventType, ...payload },
        justification: { reasoning: `Case event: ${eventType}` },
      });

      await appendWorkflowEvent(this.store, eventType, {
        ...payload,
        timestamp: toISO(now()),
        receipt,
      });
    } catch (error) {
      console.error(`Failed to log case event ${eventType}:`, error);
    }
  }

  /**
   * Log a task event to KGC-4D
   * @param {string} eventType - Event type
   * @param {Object} payload - Event payload
   * @param {string} [caseId] - Case ID for context
   * @private
   */
  async _logTaskEvent(eventType, payload, caseId) {
    try {
      const receipt = await createWorkflowReceipt({
        beforeState: payload.beforeState || { workItemId: payload.workItemId },
        afterState: payload,
        decision: { action: eventType, ...payload },
        justification: { reasoning: `Task event: ${eventType}` },
      });

      await appendWorkflowEvent(
        this.store,
        eventType,
        {
          ...payload,
          receipt,
        },
        { caseId }
      );
    } catch (error) {
      console.error(`Failed to log task event ${eventType}:`, error);
    }
  }

  // ===========================================================================
  // Circuit Breaker (Internal)
  // ===========================================================================

  /**
   * Check if circuit breaker is open
   * @param {string} key - Circuit breaker key
   * @returns {boolean}
   * @private
   */
  _isCircuitOpen(key) {
    const breaker = this._circuitBreakers.get(key);
    if (!breaker) return false;

    if (breaker.state === 'open') {
      // Check if reset timeout has passed
      const elapsed = Number(now() - breaker.openedAt) / 1_000_000;
      if (elapsed >= this.circuitBreakerResetTimeout) {
        breaker.state = 'half-open';
        return false;
      }
      return true;
    }

    return false;
  }

  /**
   * Record a circuit breaker failure
   * @param {string} key - Circuit breaker key
   * @private
   */
  _recordCircuitFailure(key) {
    let breaker = this._circuitBreakers.get(key);
    if (!breaker) {
      breaker = { failures: 0, state: 'closed', openedAt: null };
      this._circuitBreakers.set(key, breaker);
    }

    breaker.failures++;

    if (breaker.failures >= this.circuitBreakerThreshold) {
      breaker.state = 'open';
      breaker.openedAt = now();
      this._stats.circuitBreakerTrips++;

      this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
        key,
        failures: breaker.failures,
      });
    }
  }

  /**
   * Reset circuit breaker on success
   * @param {string} key - Circuit breaker key
   * @private
   */
  _resetCircuitBreaker(key) {
    const breaker = this._circuitBreakers.get(key);
    if (breaker) {
      const wasOpen = breaker.state !== 'closed';
      breaker.failures = 0;
      breaker.state = 'closed';
      breaker.openedAt = null;

      if (wasOpen) {
        this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, { key });
      }
    }
  }

  // ===========================================================================
  // Snapshot Timer (Internal)
  // ===========================================================================

  /**
   * Start automatic snapshot timer
   * @private
   */
  _startSnapshotTimer() {
    if (this._snapshotTimer) {
      clearInterval(this._snapshotTimer);
    }

    this._snapshotTimer = setInterval(async () => {
      try {
        await this.checkpoint('auto');
      } catch (error) {
        console.error('Auto checkpoint failed:', error);
      }
    }, this.snapshotInterval);
  }

  /**
   * Stop automatic snapshot timer
   * @private
   */
  _stopSnapshotTimer() {
    if (this._snapshotTimer) {
      clearInterval(this._snapshotTimer);
      this._snapshotTimer = null;
    }
  }

  // ===========================================================================
  // Serialization
  // ===========================================================================

  /**
   * Serialize engine state
   * @returns {Object} JSON-serializable state
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      workflows: [...this.workflows.values()].map(w => w.toJSON()),
      cases: [...this.cases.values()].map(c => c.toJSON()),
      resourcePool: this.resourcePool.toJSON(),
      events: this.events,
      stats: this._stats,
    };
  }

  /**
   * Shutdown engine gracefully
   */
  shutdown() {
    this._stopSnapshotTimer();
    this._eventHandlers.clear();
  }
}

// =============================================================================
// Backward Compatibility Alias
// =============================================================================

/**
 * Alias for WorkflowEngine for backward compatibility
 * @deprecated Use WorkflowEngine instead
 */
export const YawlEngine = WorkflowEngine;

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a new workflow engine instance
 *
 * @param {Object} [config={}] - Engine configuration
 * @param {Object} [config.store] - External KGC store instance
 * @param {Object} [config.kgcAdapter] - KGC-4D adapter for event sourcing
 * @param {Object} [config.hookAdapter] - Hook adapter for policy enforcement
 * @param {Object} [config.supervisorAdapter] - Supervisor adapter for process management
 * @param {boolean} [config.enableEventLog=true] - Enable KGC-4D event logging
 * @param {boolean} [config.enableSnapshots=true] - Enable automatic snapshots
 * @param {number} [config.snapshotInterval=60000] - Snapshot interval in ms
 * @param {string} [config.nodeId] - Node ID for distributed coordination
 * @param {string} [config.gitPath] - Path for Git backbone storage
 * @param {number} [config.maxConcurrentCases=1000] - Maximum concurrent cases
 * @param {number} [config.defaultTaskTimeout=30000] - Default task timeout in ms
 * @param {number} [config.circuitBreakerThreshold=5] - Circuit breaker failure threshold
 * @param {number} [config.circuitBreakerResetTimeout=30000] - Circuit breaker reset timeout
 * @returns {WorkflowEngine} Configured workflow engine
 *
 * @example
 * ```javascript
 * // Basic usage
 * const engine = createWorkflowEngine();
 *
 * // With configuration
 * const engine = createWorkflowEngine({
 *   nodeId: 'production-node-1',
 *   enableEventLog: true,
 *   enableSnapshots: true,
 *   gitPath: '/var/lib/yawl/snapshots',
 *   maxConcurrentCases: 5000,
 * });
 *
 * // With external adapters
 * const engine = createWorkflowEngine({
 *   store: myKGCStore,
 *   hookAdapter: myHookRegistry,
 *   kgcAdapter: myKGCAdapter,
 *   supervisorAdapter: mySupervisor,
 * });
 * ```
 */
export function createWorkflowEngine(config = {}) {
  return new WorkflowEngine(config);
}

// =============================================================================
// Default Export
// =============================================================================

export default {
  WorkflowEngine,
  YawlEngine,
  createWorkflowEngine,
  ENGINE_EVENTS,
  YAWL_NS,
  YAWL_GRAPHS,
  HealthStatus,
};
