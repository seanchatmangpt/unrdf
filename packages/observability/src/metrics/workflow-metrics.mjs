/**
 * @unrdf/observability - Workflow Metrics Module
 *
 * Prometheus metrics collection for distributed workflow execution.
 * Provides real-time metrics for workflow executions, task completions,
 * resource utilization, and custom business metrics.
 *
 * @module @unrdf/observability/metrics
 */

import { register, Counter, Histogram, Gauge, Summary } from 'prom-client';
import { z } from 'zod';

/**
 * Workflow execution status enum
 */
export const WorkflowStatus = {
  PENDING: 'pending',
  RUNNING: 'running',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
};

/**
 * Metric configuration schema
 * @type {z.ZodObject}
 */
const MetricConfigSchema = z.object({
  enableDefaultMetrics: z.boolean().default(true),
  prefix: z.string().default('unrdf_workflow_'),
  labels: z.record(z.string()).optional(),
  collectInterval: z.number().min(1000).default(10000), // 10s default
});

/**
 * WorkflowMetrics - Comprehensive Prometheus metrics for workflows
 *
 * Collects and exposes metrics for:
 * - Workflow executions (total, active, completed, failed)
 * - Task performance (duration, success rate)
 * - Resource utilization (CPU, memory, queue depth)
 * - Custom business metrics
 *
 * @class
 */
export class WorkflowMetrics {
  /**
   * @param {object} config - Metric configuration
   * @param {boolean} [config.enableDefaultMetrics=true] - Enable Node.js default metrics
   * @param {string} [config.prefix='unrdf_workflow_'] - Metric name prefix
   * @param {Record<string, string>} [config.labels] - Global labels for all metrics
   * @param {number} [config.collectInterval=10000] - Collection interval in ms
   */
  constructor(config = {}) {
    const validated = MetricConfigSchema.parse(config);
    this.config = validated;
    this.registry = register;

    // Enable default Node.js metrics (heap, CPU, event loop, etc.)
    if (validated.enableDefaultMetrics) {
      register.setDefaultLabels(validated.labels || {});
    }

    this._initializeMetrics();
  }

  /**
   * Initialize all Prometheus metrics
   * @private
   */
  _initializeMetrics() {
    const { prefix } = this.config;

    // Workflow Execution Metrics
    this.workflowExecutionsTotal = new Counter({
      name: `${prefix}executions_total`,
      help: 'Total number of workflow executions',
      labelNames: ['workflow_id', 'status', 'pattern'],
    });

    this.workflowExecutionDuration = new Histogram({
      name: `${prefix}execution_duration_seconds`,
      help: 'Workflow execution duration in seconds',
      labelNames: ['workflow_id', 'status', 'pattern'],
      buckets: [0.1, 0.5, 1, 2, 5, 10, 30, 60, 120, 300], // 100ms to 5min
    });

    this.workflowActiveGauge = new Gauge({
      name: `${prefix}active_workflows`,
      help: 'Number of currently active workflows',
      labelNames: ['workflow_id', 'pattern'],
    });

    // Task Execution Metrics
    this.taskExecutionsTotal = new Counter({
      name: `${prefix}task_executions_total`,
      help: 'Total number of task executions',
      labelNames: ['workflow_id', 'task_id', 'task_type', 'status'],
    });

    this.taskExecutionDuration = new Histogram({
      name: `${prefix}task_duration_seconds`,
      help: 'Task execution duration in seconds',
      labelNames: ['workflow_id', 'task_id', 'task_type'],
      buckets: [0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10, 30], // 10ms to 30s
    });

    this.taskQueueDepth = new Gauge({
      name: `${prefix}task_queue_depth`,
      help: 'Number of tasks waiting in queue',
      labelNames: ['workflow_id', 'queue_name'],
    });

    // Resource Utilization Metrics
    this.resourceUtilization = new Gauge({
      name: `${prefix}resource_utilization`,
      help: 'Resource utilization percentage (0-100)',
      labelNames: ['resource_type', 'resource_id'],
    });

    this.resourceAllocations = new Counter({
      name: `${prefix}resource_allocations_total`,
      help: 'Total resource allocation events',
      labelNames: ['resource_type', 'status'],
    });

    // Event Sourcing Metrics
    this.eventsAppended = new Counter({
      name: `${prefix}events_appended_total`,
      help: 'Total events appended to event store',
      labelNames: ['event_type', 'workflow_id'],
    });

    this.eventStoreSize = new Gauge({
      name: `${prefix}event_store_size_bytes`,
      help: 'Size of event store in bytes',
      labelNames: ['workflow_id'],
    });

    // Business Metrics
    this.policyEvaluations = new Counter({
      name: `${prefix}policy_evaluations_total`,
      help: 'Total policy evaluations',
      labelNames: ['policy_name', 'result'],
    });

    this.cryptoReceiptGenerations = new Counter({
      name: `${prefix}crypto_receipts_total`,
      help: 'Total cryptographic receipts generated',
      labelNames: ['workflow_id', 'algorithm'],
    });

    // Performance Metrics
    this.latencyPercentiles = new Summary({
      name: `${prefix}latency_percentiles`,
      help: 'Latency percentiles for workflow operations',
      labelNames: ['operation'],
      percentiles: [0.5, 0.9, 0.95, 0.99],
    });

    // Error Metrics
    this.errors = new Counter({
      name: `${prefix}errors_total`,
      help: 'Total errors encountered',
      labelNames: ['error_type', 'workflow_id', 'severity'],
    });
  }

  /**
   * Record workflow execution start
   * @param {string} workflowId - Workflow identifier
   * @param {string} pattern - YAWL pattern type
   */
  recordWorkflowStart(workflowId, pattern = 'unknown') {
    this.workflowActiveGauge.inc({ workflow_id: workflowId, pattern });
    this.workflowExecutionsTotal.inc({ workflow_id: workflowId, status: 'started', pattern });
  }

  /**
   * Record workflow execution completion
   * @param {string} workflowId - Workflow identifier
   * @param {string} status - Completion status
   * @param {number} durationSeconds - Execution duration in seconds
   * @param {string} pattern - YAWL pattern type
   */
  recordWorkflowComplete(workflowId, status, durationSeconds, pattern = 'unknown') {
    this.workflowActiveGauge.dec({ workflow_id: workflowId, pattern });
    this.workflowExecutionsTotal.inc({ workflow_id: workflowId, status, pattern });
    this.workflowExecutionDuration.observe({ workflow_id: workflowId, status, pattern }, durationSeconds);
  }

  /**
   * Record task execution
   * @param {string} workflowId - Workflow identifier
   * @param {string} taskId - Task identifier
   * @param {string} taskType - Task type
   * @param {string} status - Execution status
   * @param {number} durationSeconds - Execution duration in seconds
   */
  recordTaskExecution(workflowId, taskId, taskType, status, durationSeconds) {
    this.taskExecutionsTotal.inc({ workflow_id: workflowId, task_id: taskId, task_type: taskType, status });
    this.taskExecutionDuration.observe({ workflow_id: workflowId, task_id: taskId, task_type: taskType }, durationSeconds);
  }

  /**
   * Update task queue depth
   * @param {string} workflowId - Workflow identifier
   * @param {string} queueName - Queue name
   * @param {number} depth - Current queue depth
   */
  updateTaskQueueDepth(workflowId, queueName, depth) {
    this.taskQueueDepth.set({ workflow_id: workflowId, queue_name: queueName }, depth);
  }

  /**
   * Record resource utilization
   * @param {string} resourceType - Type of resource (cpu, memory, disk, etc.)
   * @param {string} resourceId - Resource identifier
   * @param {number} utilizationPercent - Utilization percentage (0-100)
   */
  recordResourceUtilization(resourceType, resourceId, utilizationPercent) {
    this.resourceUtilization.set({ resource_type: resourceType, resource_id: resourceId }, utilizationPercent);
  }

  /**
   * Record resource allocation event
   * @param {string} resourceType - Type of resource
   * @param {string} status - Allocation status (allocated, deallocated, failed)
   */
  recordResourceAllocation(resourceType, status) {
    this.resourceAllocations.inc({ resource_type: resourceType, status });
  }

  /**
   * Record event appended to event store
   * @param {string} eventType - Type of event
   * @param {string} workflowId - Workflow identifier
   */
  recordEventAppended(eventType, workflowId) {
    this.eventsAppended.inc({ event_type: eventType, workflow_id: workflowId });
  }

  /**
   * Update event store size
   * @param {string} workflowId - Workflow identifier
   * @param {number} sizeBytes - Size in bytes
   */
  updateEventStoreSize(workflowId, sizeBytes) {
    this.eventStoreSize.set({ workflow_id: workflowId }, sizeBytes);
  }

  /**
   * Record policy evaluation
   * @param {string} policyName - Policy name
   * @param {string} result - Evaluation result (allow, deny, error)
   */
  recordPolicyEvaluation(policyName, result) {
    this.policyEvaluations.inc({ policy_name: policyName, result });
  }

  /**
   * Record cryptographic receipt generation
   * @param {string} workflowId - Workflow identifier
   * @param {string} algorithm - Hash algorithm (BLAKE3, SHA256, etc.)
   */
  recordCryptoReceipt(workflowId, algorithm = 'BLAKE3') {
    this.cryptoReceiptGenerations.inc({ workflow_id: workflowId, algorithm });
  }

  /**
   * Record operation latency for percentile calculation
   * @param {string} operation - Operation name
   * @param {number} latencyMs - Latency in milliseconds
   */
  recordLatency(operation, latencyMs) {
    this.latencyPercentiles.observe({ operation }, latencyMs / 1000);
  }

  /**
   * Record error
   * @param {string} errorType - Error type
   * @param {string} workflowId - Workflow identifier
   * @param {string} severity - Error severity (low, medium, high, critical)
   */
  recordError(errorType, workflowId, severity = 'medium') {
    this.errors.inc({ error_type: errorType, workflow_id: workflowId, severity });
  }

  /**
   * Get all metrics in Prometheus format
   * @returns {Promise<string>} Prometheus metrics text format
   */
  async getMetrics() {
    return this.registry.metrics();
  }

  /**
   * Get metrics as JSON
   * @returns {Promise<object[]>} Metrics in JSON format
   */
  async getMetricsJSON() {
    return this.registry.getMetricsAsJSON();
  }

  /**
   * Clear all metrics (useful for testing)
   */
  clearMetrics() {
    this.registry.clear();
    this._initializeMetrics();
  }

  /**
   * Get metric registry
   * @returns {object} Prometheus registry
   */
  getRegistry() {
    return this.registry;
  }
}

/**
 * Create a workflow metrics instance
 * @param {object} config - Metric configuration
 * @returns {WorkflowMetrics} Metrics instance
 */
export function createWorkflowMetrics(config = {}) {
  return new WorkflowMetrics(config);
}

export default WorkflowMetrics;
