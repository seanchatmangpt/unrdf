/**
 * @file YAWL Metrics Collector - Prometheus metrics for workflow observability
 * @module @unrdf/yawl-observability/metrics
 *
 * @description
 * Exposes workflow execution metrics in Prometheus format:
 * - workflow_cases_total: Total cases created/completed by workflow
 * - task_duration_seconds: Histogram of task execution durations
 * - pattern_usage_count: Counter of YAWL pattern usage (split/join types)
 * - workflow_task_status: Gauge of tasks in each status
 * - case_completion_time_seconds: Histogram of end-to-end case completion
 */

import promClient from 'prom-client';
import { z } from 'zod';

// =============================================================================
// Configuration Schema
// =============================================================================

/**
 * Metrics collector configuration schema
 */
export const MetricsConfigSchema = z.object({
  /** Prefix for all metric names */
  prefix: z.string().default('yawl'),
  /** Enable default Node.js metrics (CPU, memory, etc.) */
  collectDefaultMetrics: z.boolean().default(true),
  /** Default metrics collection interval in ms */
  defaultMetricsInterval: z.number().positive().default(10000),
  /** Custom labels to add to all metrics */
  defaultLabels: z.record(z.string()).default({}),
  /** Histogram buckets for duration metrics (seconds) */
  durationBuckets: z.array(z.number()).default([0.001, 0.01, 0.1, 0.5, 1, 2.5, 5, 10, 30, 60]),
});

// =============================================================================
// YAWLMetricsCollector Class
// =============================================================================

/**
 * Prometheus metrics collector for YAWL workflow engine
 *
 * Hooks into YAWL engine events to collect and expose metrics:
 * - Case lifecycle (created, started, completed, failed)
 * - Task execution (enabled, started, completed, duration)
 * - Pattern usage (AND/XOR/OR splits and joins)
 * - Resource allocation and utilization
 *
 * @example
 * ```javascript
 * import { createWorkflowEngine } from '@unrdf/yawl';
 * import { YAWLMetricsCollector } from '@unrdf/yawl-observability';
 *
 * const engine = createWorkflowEngine();
 * const metrics = new YAWLMetricsCollector(engine, {
 *   defaultLabels: { environment: 'production', region: 'us-east-1' }
 * });
 *
 * // Expose metrics endpoint
 * app.get('/metrics', async (req, res) => {
 *   res.set('Content-Type', metrics.contentType);
 *   res.end(await metrics.getMetrics());
 * });
 * ```
 */
export class YAWLMetricsCollector {
  /**
   * @param {Object} engine - YAWL WorkflowEngine instance
   * @param {Object} [config={}] - Metrics configuration
   */
  constructor(engine, config = {}) {
    const validated = MetricsConfigSchema.parse(config);

    /** @type {Object} YAWL engine reference */
    this.engine = engine;
    /** @type {Object} Configuration */
    this.config = validated;
    /** @type {Object} Prometheus registry */
    this.register = new promClient.Registry();

    // Set default labels
    if (Object.keys(validated.defaultLabels).length > 0) {
      this.register.setDefaultLabels(validated.defaultLabels);
    }

    // Initialize metrics
    this._initializeMetrics();

    // Collect default metrics if enabled
    if (validated.collectDefaultMetrics) {
      promClient.collectDefaultMetrics({
        register: this.register,
        timeout: validated.defaultMetricsInterval,
      });
    }

    // Attach event listeners
    this._attachEventListeners();

    /** @type {Map<string, bigint>} Track case start times for duration calculation */
    this._caseStartTimes = new Map();
    /** @type {Map<string, bigint>} Track task start times for duration calculation */
    this._taskStartTimes = new Map();
  }

  // ===========================================================================
  // Metric Initialization
  // ===========================================================================

  /**
   * Initialize all Prometheus metrics
   * @private
   */
  _initializeMetrics() {
    const prefix = this.config.prefix;

    // Case metrics
    this.metrics = {
      // Counter: Total cases created/completed by workflow and status
      casesTotal: new promClient.Counter({
        name: `${prefix}_workflow_cases_total`,
        help: 'Total number of workflow cases by status',
        labelNames: ['workflow_id', 'status'],
        registers: [this.register],
      }),

      // Histogram: Case completion time (end-to-end)
      caseCompletionTime: new promClient.Histogram({
        name: `${prefix}_case_completion_time_seconds`,
        help: 'End-to-end case completion time in seconds',
        labelNames: ['workflow_id'],
        buckets: this.config.durationBuckets,
        registers: [this.register],
      }),

      // Counter: Total tasks by workflow and status
      tasksTotal: new promClient.Counter({
        name: `${prefix}_workflow_tasks_total`,
        help: 'Total number of tasks by workflow and status',
        labelNames: ['workflow_id', 'task_id', 'status'],
        registers: [this.register],
      }),

      // Histogram: Task execution duration (from start to complete)
      taskDuration: new promClient.Histogram({
        name: `${prefix}_task_duration_seconds`,
        help: 'Task execution duration in seconds',
        labelNames: ['workflow_id', 'task_id'],
        buckets: this.config.durationBuckets,
        registers: [this.register],
      }),

      // Histogram: Task wait time (from enabled to started)
      taskWaitTime: new promClient.Histogram({
        name: `${prefix}_task_wait_time_seconds`,
        help: 'Task wait time from enabled to started in seconds',
        labelNames: ['workflow_id', 'task_id'],
        buckets: this.config.durationBuckets,
        registers: [this.register],
      }),

      // Counter: Pattern usage by type
      patternUsage: new promClient.Counter({
        name: `${prefix}_pattern_usage_count`,
        help: 'YAWL pattern usage count by type (split/join)',
        labelNames: ['workflow_id', 'task_id', 'pattern_type', 'operation'],
        registers: [this.register],
      }),

      // Gauge: Current active tasks by workflow
      activeTasks: new promClient.Gauge({
        name: `${prefix}_workflow_active_tasks`,
        help: 'Number of currently active (running) tasks by workflow',
        labelNames: ['workflow_id'],
        registers: [this.register],
      }),

      // Gauge: Current enabled tasks by workflow
      enabledTasks: new promClient.Gauge({
        name: `${prefix}_workflow_enabled_tasks`,
        help: 'Number of currently enabled (ready) tasks by workflow',
        labelNames: ['workflow_id'],
        registers: [this.register],
      }),

      // Counter: Task errors by workflow and task
      taskErrors: new promClient.Counter({
        name: `${prefix}_task_errors_total`,
        help: 'Total number of task failures, cancellations, and timeouts',
        labelNames: ['workflow_id', 'task_id', 'error_type'],
        registers: [this.register],
      }),

      // Counter: Resource allocations
      resourceAllocations: new promClient.Counter({
        name: `${prefix}_resource_allocations_total`,
        help: 'Total number of resource allocations by role',
        labelNames: ['workflow_id', 'role', 'resource_id'],
        registers: [this.register],
      }),

      // Gauge: Resource utilization
      resourceUtilization: new promClient.Gauge({
        name: `${prefix}_resource_utilization`,
        help: 'Current resource utilization (0-1 scale)',
        labelNames: ['resource_id', 'role'],
        registers: [this.register],
      }),
    };
  }

  // ===========================================================================
  // Event Listeners
  // ===========================================================================

  /**
   * Attach event listeners to YAWL engine
   * @private
   */
  _attachEventListeners() {
    const engine = this.engine;

    // Case lifecycle events
    engine.on('CASE_CREATED', (event) => {
      this.metrics.casesTotal.inc({
        workflow_id: event.workflowId,
        status: 'created',
      });
      this._caseStartTimes.set(event.caseId, process.hrtime.bigint());
    });

    engine.on('CASE_STARTED', (event) => {
      this.metrics.casesTotal.inc({
        workflow_id: event.workflowId,
        status: 'started',
      });
    });

    engine.on('CASE_COMPLETED', (event) => {
      this.metrics.casesTotal.inc({
        workflow_id: event.workflowId,
        status: 'completed',
      });

      // Record case completion time
      const startTime = this._caseStartTimes.get(event.caseId);
      if (startTime) {
        const duration = Number(process.hrtime.bigint() - startTime) / 1e9; // Convert to seconds
        this.metrics.caseCompletionTime.observe(
          { workflow_id: event.workflowId },
          duration
        );
        this._caseStartTimes.delete(event.caseId);
      }
    });

    // Task lifecycle events
    engine.on('TASK_ENABLED', (event) => {
      this.metrics.tasksTotal.inc({
        workflow_id: event.workflowId || this._getWorkflowId(event.caseId),
        task_id: event.taskId,
        status: 'enabled',
      });

      // Increment enabled tasks gauge
      const workflowId = event.workflowId || this._getWorkflowId(event.caseId);
      this.metrics.enabledTasks.inc({ workflow_id: workflowId });

      // Track pattern usage (when task is enabled, record its join pattern)
      const taskDef = this._getTaskDefinition(event.caseId, event.taskId);
      if (taskDef && taskDef.joinType !== 'sequence') {
        this.metrics.patternUsage.inc({
          workflow_id: workflowId,
          task_id: event.taskId,
          pattern_type: taskDef.joinType,
          operation: 'join',
        });
      }
    });

    engine.on('TASK_STARTED', (event) => {
      const workflowId = event.workflowId || this._getWorkflowId(event.caseId);

      this.metrics.tasksTotal.inc({
        workflow_id: workflowId,
        task_id: event.taskId || this._getTaskIdFromWorkItem(event.caseId, event.workItemId),
        status: 'started',
      });

      // Update gauges
      this.metrics.enabledTasks.dec({ workflow_id: workflowId });
      this.metrics.activeTasks.inc({ workflow_id: workflowId });

      // Track start time for duration calculation
      this._taskStartTimes.set(event.workItemId, process.hrtime.bigint());
    });

    engine.on('TASK_COMPLETED', (event) => {
      const workflowId = event.workflowId || this._getWorkflowId(event.caseId);
      const taskId = event.taskId || this._getTaskIdFromWorkItem(event.caseId, event.workItemId);

      this.metrics.tasksTotal.inc({
        workflow_id: workflowId,
        task_id: taskId,
        status: 'completed',
      });

      // Update active tasks gauge
      this.metrics.activeTasks.dec({ workflow_id: workflowId });

      // Record task duration
      const startTime = this._taskStartTimes.get(event.workItemId);
      if (startTime) {
        const duration = Number(process.hrtime.bigint() - startTime) / 1e9;
        this.metrics.taskDuration.observe(
          { workflow_id: workflowId, task_id: taskId },
          duration
        );
        this._taskStartTimes.delete(event.workItemId);
      }

      // Track pattern usage (when task completes, record its split pattern)
      const taskDef = this._getTaskDefinition(event.caseId, taskId);
      if (taskDef && taskDef.splitType !== 'sequence') {
        this.metrics.patternUsage.inc({
          workflow_id: workflowId,
          task_id: taskId,
          pattern_type: taskDef.splitType,
          operation: 'split',
        });
      }
    });

    // Task error events
    engine.on('TASK_CANCELLED', (event) => {
      const workflowId = event.workflowId || this._getWorkflowId(event.caseId);
      const taskId = event.taskId || this._getTaskIdFromWorkItem(event.caseId, event.workItemId);

      this.metrics.tasksTotal.inc({
        workflow_id: workflowId,
        task_id: taskId,
        status: 'cancelled',
      });

      this.metrics.taskErrors.inc({
        workflow_id: workflowId,
        task_id: taskId,
        error_type: 'cancelled',
      });

      // Update gauges
      this.metrics.activeTasks.dec({ workflow_id: workflowId }, 0);
      this.metrics.enabledTasks.dec({ workflow_id: workflowId }, 0);

      // Clean up tracking
      this._taskStartTimes.delete(event.workItemId);
    });

    engine.on('TASK_TIMEOUT', (event) => {
      const workflowId = this._getWorkflowId(event.caseId);
      const taskId = event.taskId || this._getTaskIdFromWorkItem(event.caseId, event.workItemId);

      this.metrics.taskErrors.inc({
        workflow_id: workflowId,
        task_id: taskId,
        error_type: 'timeout',
      });

      this.metrics.activeTasks.dec({ workflow_id: workflowId }, 0);
      this._taskStartTimes.delete(event.workItemId);
    });

    // Resource events
    engine.on('RESOURCE_ALLOCATED', (event) => {
      const workflowId = this._getWorkflowId(event.caseId);

      this.metrics.resourceAllocations.inc({
        workflow_id: workflowId,
        role: event.role || 'default',
        resource_id: event.resourceId,
      });

      // Update utilization (this is a simplified calculation)
      this._updateResourceUtilization(event.resourceId, event.role);
    });

    engine.on('RESOURCE_RELEASED', (event) => {
      this._updateResourceUtilization(event.resourceId, event.role);
    });
  }

  // ===========================================================================
  // Helper Methods
  // ===========================================================================

  /**
   * Get workflow ID from case ID
   * @private
   * @param {string} caseId - Case ID
   * @returns {string} Workflow ID
   */
  _getWorkflowId(caseId) {
    const yawlCase = this.engine.cases.get(caseId);
    return yawlCase ? yawlCase.workflowId : 'unknown';
  }

  /**
   * Get task ID from work item ID
   * @private
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @returns {string} Task definition ID
   */
  _getTaskIdFromWorkItem(caseId, workItemId) {
    const yawlCase = this.engine.cases.get(caseId);
    if (!yawlCase) return 'unknown';

    const workItem = yawlCase.workItems.get(workItemId);
    return workItem ? workItem.taskDefId : 'unknown';
  }

  /**
   * Get task definition
   * @private
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task definition ID
   * @returns {Object|null} Task definition
   */
  _getTaskDefinition(caseId, taskId) {
    const yawlCase = this.engine.cases.get(caseId);
    if (!yawlCase) return null;

    const workflow = this.engine.workflows.get(yawlCase.workflowId);
    if (!workflow) return null;

    return workflow.tasks.get(taskId) || null;
  }

  /**
   * Update resource utilization metric
   * @private
   * @param {string} resourceId - Resource ID
   * @param {string} role - Role name
   */
  _updateResourceUtilization(resourceId, role) {
    // Get resource pool state
    const pool = this.engine.resourcePool;
    const resource = pool.resources.get(resourceId);

    if (resource) {
      const utilization = resource.isAllocated ? 1 : 0;
      this.metrics.resourceUtilization.set(
        { resource_id: resourceId, role: role || 'default' },
        utilization
      );
    }
  }

  // ===========================================================================
  // Public API
  // ===========================================================================

  /**
   * Get all metrics in Prometheus format
   * @returns {Promise<string>} Prometheus formatted metrics
   */
  async getMetrics() {
    return this.register.metrics();
  }

  /**
   * Get metrics content type for HTTP response
   * @returns {string} Content-Type header value
   */
  get contentType() {
    return this.register.contentType;
  }

  /**
   * Reset all metrics (useful for testing)
   */
  resetMetrics() {
    this.register.resetMetrics();
  }

  /**
   * Get specific metric by name
   * @param {string} name - Metric name (without prefix)
   * @returns {Object|undefined} Metric object
   */
  getMetric(name) {
    const fullName = `${this.config.prefix}_${name}`;
    return this.register.getSingleMetric(fullName);
  }

  /**
   * Remove all event listeners (cleanup)
   */
  destroy() {
    // Remove all listeners we attached
    this.engine.removeAllListeners('CASE_CREATED');
    this.engine.removeAllListeners('CASE_STARTED');
    this.engine.removeAllListeners('CASE_COMPLETED');
    this.engine.removeAllListeners('TASK_ENABLED');
    this.engine.removeAllListeners('TASK_STARTED');
    this.engine.removeAllListeners('TASK_COMPLETED');
    this.engine.removeAllListeners('TASK_CANCELLED');
    this.engine.removeAllListeners('TASK_TIMEOUT');
    this.engine.removeAllListeners('RESOURCE_ALLOCATED');
    this.engine.removeAllListeners('RESOURCE_RELEASED');

    // Clear tracking maps
    this._caseStartTimes.clear();
    this._taskStartTimes.clear();
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a YAWLMetricsCollector instance
 *
 * @param {Object} engine - YAWL WorkflowEngine instance
 * @param {Object} [config={}] - Metrics configuration
 * @returns {YAWLMetricsCollector} Metrics collector instance
 *
 * @example
 * ```javascript
 * const metrics = createMetricsCollector(engine, {
 *   prefix: 'myapp_yawl',
 *   defaultLabels: { environment: 'staging' }
 * });
 * ```
 */
export function createMetricsCollector(engine, config = {}) {
  return new YAWLMetricsCollector(engine, config);
}
