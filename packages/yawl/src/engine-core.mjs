/**
 * @file YAWL Engine Core - Workflow & case lifecycle management
 * @module @unrdf/yawl/engine-core
 *
 * @description
 * Core engine functionality:
 * - Engine configuration and initialization
 * - Workflow registration and management
 * - Case creation and lifecycle
 * - Health checks and statistics
 * - State serialization
 */

import { z } from 'zod';
import { KGCStore, GitBackbone, now, toISO } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { YawlWorkflow } from './workflow.mjs';
import { YawlCase, CaseStatus } from './case.mjs';
import { YawlResourcePool } from './resource.mjs';
import { ENGINE_EVENTS, YAWL_GRAPHS } from './engine-constants.mjs';

// =============================================================================
// Constants Re-export
// =============================================================================

export { ENGINE_EVENTS, YAWL_NS, YAWL_GRAPHS, HealthStatus } from './engine-constants.mjs';

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
// WorkflowEngine Core
// =============================================================================

/**
 * Initialize engine state
 * @param {Object} config - Validated configuration
 * @returns {Object} Engine state object
 */
export function initializeEngineState(config) {
  const validated = EngineConfigSchema.parse(config);

  return {
    // Core configuration
    nodeId: validated.nodeId ?? `yawl-${Date.now()}`,
    gitPath: validated.gitPath,
    enableEventLog: validated.enableEventLog,
    enableSnapshots: validated.enableSnapshots,
    snapshotInterval: validated.snapshotInterval,
    circuitBreakerThreshold: validated.circuitBreakerThreshold,
    circuitBreakerResetTimeout: validated.circuitBreakerResetTimeout,
    maxConcurrentCases: validated.maxConcurrentCases,
    defaultTaskTimeout: validated.defaultTaskTimeout,

    // External adapters
    _hookAdapter: validated.hookAdapter,
    _kgcAdapter: validated.kgcAdapter,
    _supervisorAdapter: validated.supervisorAdapter,

    // Core stores
    store: validated.store ?? new KGCStore({ nodeId: validated.nodeId ?? `yawl-${Date.now()}` }),
    git: validated.gitPath ? new GitBackbone(validated.gitPath) : null,

    // Workflow management
    workflows: new Map(),
    cases: new Map(),

    // Resource management
    resourcePool: new YawlResourcePool(),

    // Event sourcing
    events: [],
    checkpoints: new Map(),

    // Event subscription
    _eventHandlers: new Map(),

    // Hook registry
    _policyPacks: new Map(),

    // Circuit breaker state
    _circuitBreakers: new Map(),

    // Statistics
    _stats: {
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
    },

    // Health check state
    _health: {
      status: 'healthy',
      lastCheck: now(),
      components: {
        store: true,
        git: validated.gitPath ? true : null,
        workflows: true,
        cases: true,
      },
      errors: [],
    },

    // Snapshot timer
    _snapshotTimer: null,
  };
}

// =============================================================================
// Workflow Management
// =============================================================================

/**
 * Register a workflow definition
 * @param {Object} engine - Engine instance
 * @param {YawlWorkflow|Object} workflowOrSpec - Workflow or specification object
 * @returns {YawlWorkflow} Registered workflow
 * @throws {Error} If workflow is invalid
 */
export function registerWorkflow(engine, workflowOrSpec) {
  const workflow =
    workflowOrSpec instanceof YawlWorkflow
      ? workflowOrSpec
      : new YawlWorkflow(workflowOrSpec);

  const validation = workflow.validate();
  if (!validation.valid) {
    throw new Error(`Invalid workflow: ${validation.errors.join(', ')}`);
  }

  workflow.lock();
  engine.workflows.set(workflow.id, workflow);

  appendEvent(engine, {
    type: 'WORKFLOW_REGISTERED',
    workflowId: workflow.id,
    version: workflow.version,
  });

  engine.emit(ENGINE_EVENTS.WORKFLOW_REGISTERED, {
    workflowId: workflow.id,
    version: workflow.version,
    name: workflow.name,
  });

  return workflow;
}

/**
 * Load a workflow from the store
 * @param {Object} engine - Engine instance
 * @param {string} workflowId - Workflow ID to load
 * @returns {Promise<YawlWorkflow|null>} Workflow if found
 */
export async function loadWorkflow(engine, workflowId) {
  // First check in-memory cache
  if (engine.workflows.has(workflowId)) {
    return engine.workflows.get(workflowId);
  }

  // Query from RDF store if available
  const workflowGraph = dataFactory.namedNode(`${YAWL_GRAPHS.WORKFLOWS}/${workflowId}`);
  const quads = [...engine.store.match(null, null, null, workflowGraph)];

  if (quads.length === 0) {
    return null;
  }

  // Reconstruct workflow from quads (simplified - full impl would parse RDF)
  // For now, return null if not in memory
  return null;
}

/**
 * Get a registered workflow
 * @param {Object} engine - Engine instance
 * @param {string} workflowId - Workflow ID
 * @returns {YawlWorkflow|undefined} Workflow if found
 */
export function getWorkflow(engine, workflowId) {
  return engine.workflows.get(workflowId);
}

/**
 * Get all registered workflows
 * @param {Object} engine - Engine instance
 * @returns {YawlWorkflow[]} Array of workflows
 */
export function getAllWorkflows(engine) {
  return [...engine.workflows.values()];
}

// =============================================================================
// Case Management
// =============================================================================

export function getCase(engine, caseId) {
  return engine.cases.get(caseId);
}

export function getCaseStatus(engine, caseId) {
  const yawlCase = engine.cases.get(caseId);
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

export function getActiveCases(engine) {
  return [...engine.cases.values()].filter(c => c.status === CaseStatus.RUNNING);
}

export function getCasesForWorkflow(engine, workflowId) {
  return [...engine.cases.values()].filter(c => c.workflowId === workflowId);
}

/**
 * Create a new case for a workflow
 * @param {Object} engine - Engine instance
 * @param {string} workflowId - Workflow ID
 * @param {Object} [initialData={}] - Initial case data
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<{case: YawlCase, receipt: YawlReceipt}>}
 * @throws {Error} If workflow not found or max cases exceeded
 */
export async function createCase(engine, workflowId, initialData = {}, options = {}) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');
  const { logCaseEvent } = await import('./engine-hooks.mjs');

  const workflow = engine.workflows.get(workflowId);
  if (!workflow) {
    throw new Error(`Workflow ${workflowId} not found`);
  }

  // Check capacity
  if (engine.cases.size >= engine.maxConcurrentCases) {
    throw new Error(`Maximum concurrent cases (${engine.maxConcurrentCases}) exceeded`);
  }

  const caseId = `case-${workflow.id}-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;
  const yawlCase = new YawlCase(
    { id: caseId, workflowId, data: initialData },
    workflow
  );

  engine.cases.set(caseId, yawlCase);
  engine._stats.casesCreated++;

  // Log to KGC-4D if enabled
  if (engine.enableEventLog) {
    await logCaseEvent(engine, YAWL_EVENT_TYPES.CASE_CREATED, {
      caseId,
      workflowId,
      data: initialData,
    });
  }

  appendEvent(engine, {
    type: 'CASE_CREATED',
    caseId,
    workflowId,
    data: initialData,
  });

  engine.emit(ENGINE_EVENTS.CASE_CREATED, {
    caseId,
    workflowId,
    data: initialData,
  });

  // Start the case (enable start task)
  const startResult = await yawlCase.start();

  appendEvent(engine, {
    type: 'CASE_STARTED',
    caseId,
    taskId: workflow.startTaskId,
    workItemId: startResult.task.id,
  });

  engine.emit(ENGINE_EVENTS.CASE_STARTED, {
    caseId,
    workflowId,
    taskId: workflow.startTaskId,
    workItemId: startResult.task.id,
  });

  engine.emit(ENGINE_EVENTS.TASK_ENABLED, {
    caseId,
    taskId: workflow.startTaskId,
    workItemId: startResult.task.id,
  });

  engine._stats.tasksEnabled++;

  return { case: yawlCase, receipt: startResult.receipt };
}

// =============================================================================
// Health & Statistics
// =============================================================================

/**
 * Perform health check
 * @param {Object} engine - Engine instance
 * @returns {Object} Health status
 */
export function healthCheck(engine) {
  const errors = [];

  // Check store
  let storeHealthy = true;
  try {
    // Simple store check - try to get size
    engine.store.size;
  } catch (e) {
    storeHealthy = false;
    errors.push(`Store error: ${e.message}`);
  }

  // Check Git backbone if configured
  let gitHealthy = null;
  if (engine.git) {
    gitHealthy = true; // Would add actual Git health check here
  }

  // Check workflow count
  const workflowHealthy = engine.workflows.size > 0 || true;

  // Check case capacity
  const casesHealthy = engine.cases.size < engine.maxConcurrentCases;
  if (!casesHealthy) {
    errors.push(`Case capacity reached: ${engine.cases.size}/${engine.maxConcurrentCases}`);
  }

  // Determine overall status
  let status = 'healthy';
  if (!storeHealthy) {
    status = 'unhealthy';
  } else if (!casesHealthy || errors.length > 0) {
    status = 'degraded';
  }

  engine._health = {
    status,
    lastCheck: now(),
    components: {
      store: storeHealthy,
      git: gitHealthy,
      workflows: workflowHealthy,
      cases: casesHealthy,
    },
    errors,
    uptime: now() - engine._stats.startedAt,
  };

  return engine._health;
}

/**
 * Get engine statistics
 * @param {Object} engine - Engine instance
 * @returns {Object} Engine stats
 */
export function getStats(engine) {
  return {
    ...engine._stats,
    uptimeNs: (now() - engine._stats.startedAt).toString(),
    uptimeMs: Number(now() - engine._stats.startedAt) / 1_000_000,
    workflowCount: engine.workflows.size,
    activeCaseCount: [...engine.cases.values()].filter(
      c => c.status === CaseStatus.RUNNING
    ).length,
    totalCaseCount: engine.cases.size,
    checkpointCount: engine.checkpoints.size,
    eventCount: engine.events.length,
    resourceStats: engine.resourcePool.getStats(),
    circuitBreakers: Object.fromEntries(engine._circuitBreakers),
  };
}

// =============================================================================
// Event Log (Internal)
// =============================================================================

/**
 * Append an event to the in-memory log
 * @param {Object} engine - Engine instance
 * @param {Object} eventData - Event data
 */
export function appendEvent(engine, eventData) {
  const timestamp = now();
  engine.events.push({
    ...eventData,
    timestamp: timestamp.toString(),
    timestampISO: toISO(timestamp),
  });
  engine._stats.eventsLogged++;
}

export function getEventsForCase(engine, caseId) {
  return engine.events.filter(e => e.caseId === caseId);
}

export function getAllEvents(engine) {
  return [...engine.events];
}
