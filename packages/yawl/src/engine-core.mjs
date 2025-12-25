/**
 * @file YAWL Engine Core - Core engine class with workflow/case management
 * @module @unrdf/yawl/engine-core
 */

import { z } from 'zod';
import { KGCStore, GitBackbone, now } from '@unrdf/kgc-4d';
import { YawlWorkflow } from './workflow.mjs';
import { YawlCase, CaseStatus } from './case.mjs';
import { YawlResourcePool } from './resource.mjs';

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
export const EngineConfigSchema = z.object({
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
// Engine Core Class
// =============================================================================

/**
 * Core engine state and initialization
 *
 * This module contains the base WorkflowEngine class with:
 * - Constructor and configuration
 * - Core data structures (workflows, cases, resources)
 * - Workflow registration and management
 * - Resource pool management
 *
 * Other functionality is added via mixins from other engine-*.mjs modules.
 */
export class EngineCore {
  /**
   * Create a new workflow engine core
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

    // Initialize snapshot timer if enabled (managed by engine-snapshots.mjs)
    this._snapshotTimer = null;
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

    // Event logging handled by mixins
    this._onWorkflowRegistered(workflow);

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
    // (Simplified implementation - full version would parse RDF)
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
    if (this._stopSnapshotTimer) {
      this._stopSnapshotTimer();
    }
    this._eventHandlers.clear();
  }

  // ===========================================================================
  // Hooks for Mixins (Overridden by other modules)
  // ===========================================================================

  /**
   * Called when a workflow is registered (override in mixins)
   * @param {YawlWorkflow} workflow - Registered workflow
   * @protected
   */
  _onWorkflowRegistered(workflow) {
    // Default: no-op, overridden by event mixin
  }
}
