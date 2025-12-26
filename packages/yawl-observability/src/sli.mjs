/**
 * @file YAWL SLI Calculator - Service Level Indicators for workflow performance
 * @module @unrdf/yawl-observability/sli
 *
 * @description
 * Calculates custom SLIs for workflow execution:
 * - Workflow completion rate (percentage of cases that complete successfully)
 * - Task error rate (percentage of tasks that fail/timeout/cancel)
 * - p95 latency (95th percentile task execution time)
 * - Resource utilization (percentage of time resources are allocated)
 * - SLO compliance (whether metrics meet defined Service Level Objectives)
 */

import { z } from 'zod';

// =============================================================================
// Configuration Schema
// =============================================================================

/**
 * SLI configuration schema
 */
export const SLIConfigSchema = z.object({
  /** Time window for SLI calculations in milliseconds */
  windowMs: z.number().positive().default(300000), // 5 minutes
  /** Target workflow completion rate (0-1) */
  targetCompletionRate: z.number().min(0).max(1).default(0.95),
  /** Target task success rate (0-1) */
  targetTaskSuccessRate: z.number().min(0).max(1).default(0.99),
  /** Target p95 latency in seconds */
  targetP95Latency: z.number().positive().default(5.0),
  /** Target resource utilization (0-1) */
  targetResourceUtilization: z.number().min(0).max(1).default(0.80),
});

// =============================================================================
// SLI Data Structures
// =============================================================================

/**
 * @typedef {Object} SLISnapshot
 * @property {number} timestamp - Unix timestamp in milliseconds
 * @property {number} completionRate - Workflow completion rate (0-1)
 * @property {number} taskSuccessRate - Task success rate (0-1)
 * @property {number} taskErrorRate - Task error rate (0-1)
 * @property {number} p95Latency - 95th percentile task duration in seconds
 * @property {number} p99Latency - 99th percentile task duration in seconds
 * @property {number} medianLatency - Median task duration in seconds
 * @property {number} resourceUtilization - Average resource utilization (0-1)
 * @property {Object} sloCompliance - SLO compliance status
 * @property {boolean} sloCompliance.completionRate - Meets completion rate target
 * @property {boolean} sloCompliance.taskSuccessRate - Meets task success rate target
 * @property {boolean} sloCompliance.p95Latency - Meets p95 latency target
 * @property {boolean} sloCompliance.resourceUtilization - Meets resource utilization target
 * @property {number} sloCompliance.score - Overall compliance score (0-1)
 */

// =============================================================================
// YAWLSLICalculator Class
// =============================================================================

/**
 * Service Level Indicator calculator for YAWL workflows
 *
 * Tracks workflow and task execution metrics over time windows to calculate:
 * - Success rates and error rates
 * - Latency percentiles (p50, p95, p99)
 * - Resource utilization
 * - SLO compliance against defined targets
 *
 * @example
 * ```javascript
 * import { createWorkflowEngine } from '@unrdf/yawl';
 * import { YAWLSLICalculator } from '@unrdf/yawl-observability';
 *
 * const engine = createWorkflowEngine();
 * const sli = new YAWLSLICalculator(engine, {
 *   targetCompletionRate: 0.95,
 *   targetTaskSuccessRate: 0.99,
 *   targetP95Latency: 5.0
 * });
 *
 * // Get current SLI snapshot
 * const snapshot = sli.getSnapshot();
 * console.log(`Completion rate: ${snapshot.completionRate * 100}%`);
 * console.log(`SLO compliance: ${snapshot.sloCompliance.score * 100}%`);
 * ```
 */
export class YAWLSLICalculator {
  /**
   * @param {Object} engine - YAWL WorkflowEngine instance
   * @param {Object} [config={}] - SLI configuration
   */
  constructor(engine, config = {}) {
    const validated = SLIConfigSchema.parse(config);

    /** @type {Object} YAWL engine reference */
    this.engine = engine;
    /** @type {Object} Configuration */
    this.config = validated;

    /** @type {Array<Object>} Case events within window */
    this._caseEvents = [];
    /** @type {Array<Object>} Task events within window */
    this._taskEvents = [];
    /** @type {Array<number>} Task durations in seconds */
    this._taskDurations = [];
    /** @type {Map<string, Object>} Resource allocation tracking */
    this._resourceAllocations = new Map();

    // Attach event listeners
    this._attachEventListeners();
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

    // Track case lifecycle
    engine.on('CASE_CREATED', (event) => {
      this._recordCaseEvent({
        type: 'created',
        caseId: event.caseId,
        workflowId: event.workflowId,
        timestamp: Date.now(),
      });
    });

    engine.on('CASE_COMPLETED', (event) => {
      this._recordCaseEvent({
        type: 'completed',
        caseId: event.caseId,
        workflowId: event.workflowId,
        timestamp: Date.now(),
      });
    });

    // Track task lifecycle
    engine.on('TASK_STARTED', (event) => {
      this._recordTaskEvent({
        type: 'started',
        caseId: event.caseId,
        workItemId: event.workItemId,
        timestamp: Date.now(),
      });
    });

    engine.on('TASK_COMPLETED', (event) => {
      const startEvent = this._findTaskEvent(event.workItemId, 'started');
      if (startEvent) {
        const duration = (Date.now() - startEvent.timestamp) / 1000; // Convert to seconds
        this._taskDurations.push(duration);
      }

      this._recordTaskEvent({
        type: 'completed',
        caseId: event.caseId,
        workItemId: event.workItemId,
        timestamp: Date.now(),
      });
    });

    engine.on('TASK_CANCELLED', (event) => {
      this._recordTaskEvent({
        type: 'error',
        errorType: 'cancelled',
        caseId: event.caseId,
        workItemId: event.workItemId,
        timestamp: Date.now(),
      });
    });

    engine.on('TASK_TIMEOUT', (event) => {
      this._recordTaskEvent({
        type: 'error',
        errorType: 'timeout',
        caseId: event.caseId,
        workItemId: event.workItemId,
        timestamp: Date.now(),
      });
    });

    // Track resource utilization
    engine.on('RESOURCE_ALLOCATED', (event) => {
      this._resourceAllocations.set(event.resourceId, {
        resourceId: event.resourceId,
        allocatedAt: Date.now(),
      });
    });

    engine.on('RESOURCE_RELEASED', (event) => {
      this._resourceAllocations.delete(event.resourceId);
    });

    // Periodic cleanup of old events
    this._cleanupInterval = setInterval(() => {
      this._cleanupOldEvents();
    }, this.config.windowMs / 2);
  }

  // ===========================================================================
  // Event Recording
  // ===========================================================================

  /**
   * Record a case event
   * @private
   * @param {Object} event - Case event
   */
  _recordCaseEvent(event) {
    this._caseEvents.push(event);
  }

  /**
   * Record a task event
   * @private
   * @param {Object} event - Task event
   */
  _recordTaskEvent(event) {
    this._taskEvents.push(event);
  }

  /**
   * Find a task event by work item ID and type
   * @private
   * @param {string} workItemId - Work item ID
   * @param {string} type - Event type
   * @returns {Object|null} Task event or null
   */
  _findTaskEvent(workItemId, type) {
    return this._taskEvents.find(
      (e) => e.workItemId === workItemId && e.type === type
    ) || null;
  }

  /**
   * Clean up events outside the time window
   * @private
   */
  _cleanupOldEvents() {
    const cutoff = Date.now() - this.config.windowMs;

    this._caseEvents = this._caseEvents.filter((e) => e.timestamp > cutoff);
    this._taskEvents = this._taskEvents.filter((e) => e.timestamp > cutoff);

    // Keep task durations within window
    if (this._taskDurations.length > 1000) {
      this._taskDurations = this._taskDurations.slice(-1000);
    }
  }

  // ===========================================================================
  // SLI Calculations
  // ===========================================================================

  /**
   * Calculate workflow completion rate
   * @returns {number} Completion rate (0-1)
   */
  calculateCompletionRate() {
    const created = this._caseEvents.filter((e) => e.type === 'created').length;
    const completed = this._caseEvents.filter((e) => e.type === 'completed').length;

    if (created === 0) return 1.0; // No cases = 100% (avoid division by zero)
    return completed / created;
  }

  /**
   * Calculate task success rate
   * @returns {number} Success rate (0-1)
   */
  calculateTaskSuccessRate() {
    const total = this._taskEvents.filter((e) => e.type === 'started').length;
    const successful = this._taskEvents.filter((e) => e.type === 'completed').length;

    if (total === 0) return 1.0;
    return successful / total;
  }

  /**
   * Calculate task error rate
   * @returns {number} Error rate (0-1)
   */
  calculateTaskErrorRate() {
    return 1.0 - this.calculateTaskSuccessRate();
  }

  /**
   * Calculate latency percentile
   * @param {number} percentile - Percentile to calculate (0-100)
   * @returns {number} Latency in seconds
   */
  calculateLatencyPercentile(percentile) {
    if (this._taskDurations.length === 0) return 0;

    const sorted = [...this._taskDurations].sort((a, b) => a - b);
    const index = Math.ceil((percentile / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }

  /**
   * Calculate p95 latency
   * @returns {number} 95th percentile latency in seconds
   */
  calculateP95Latency() {
    return this.calculateLatencyPercentile(95);
  }

  /**
   * Calculate p99 latency
   * @returns {number} 99th percentile latency in seconds
   */
  calculateP99Latency() {
    return this.calculateLatencyPercentile(99);
  }

  /**
   * Calculate median latency
   * @returns {number} Median (p50) latency in seconds
   */
  calculateMedianLatency() {
    return this.calculateLatencyPercentile(50);
  }

  /**
   * Calculate resource utilization
   * @returns {number} Average utilization (0-1)
   */
  calculateResourceUtilization() {
    const pool = this.engine.resourcePool;
    if (!pool || pool.resources.size === 0) return 0;

    let allocatedCount = 0;
    for (const resource of pool.resources.values()) {
      if (resource.isAllocated) {
        allocatedCount++;
      }
    }

    return allocatedCount / pool.resources.size;
  }

  /**
   * Calculate SLO compliance
   * @returns {Object} SLO compliance status
   */
  calculateSLOCompliance() {
    const completionRate = this.calculateCompletionRate();
    const taskSuccessRate = this.calculateTaskSuccessRate();
    const p95Latency = this.calculateP95Latency();
    const resourceUtilization = this.calculateResourceUtilization();

    const compliance = {
      completionRate: completionRate >= this.config.targetCompletionRate,
      taskSuccessRate: taskSuccessRate >= this.config.targetTaskSuccessRate,
      p95Latency: p95Latency <= this.config.targetP95Latency,
      resourceUtilization: resourceUtilization <= this.config.targetResourceUtilization,
    };

    // Calculate overall compliance score
    const meetsCount = Object.values(compliance).filter(Boolean).length;
    compliance.score = meetsCount / 4;

    return compliance;
  }

  // ===========================================================================
  // Public API
  // ===========================================================================

  /**
   * Get current SLI snapshot
   * @returns {SLISnapshot} Current SLI values
   */
  getSnapshot() {
    return {
      timestamp: Date.now(),
      completionRate: this.calculateCompletionRate(),
      taskSuccessRate: this.calculateTaskSuccessRate(),
      taskErrorRate: this.calculateTaskErrorRate(),
      p95Latency: this.calculateP95Latency(),
      p99Latency: this.calculateP99Latency(),
      medianLatency: this.calculateMedianLatency(),
      resourceUtilization: this.calculateResourceUtilization(),
      sloCompliance: this.calculateSLOCompliance(),
    };
  }

  /**
   * Get SLI snapshot as Prometheus metrics format
   * @returns {string} Prometheus formatted metrics
   */
  toPrometheus() {
    const snapshot = this.getSnapshot();
    const lines = [];

    lines.push('# HELP yawl_sli_completion_rate Workflow completion rate');
    lines.push('# TYPE yawl_sli_completion_rate gauge');
    lines.push(`yawl_sli_completion_rate ${snapshot.completionRate}`);

    lines.push('# HELP yawl_sli_task_success_rate Task success rate');
    lines.push('# TYPE yawl_sli_task_success_rate gauge');
    lines.push(`yawl_sli_task_success_rate ${snapshot.taskSuccessRate}`);

    lines.push('# HELP yawl_sli_task_error_rate Task error rate');
    lines.push('# TYPE yawl_sli_task_error_rate gauge');
    lines.push(`yawl_sli_task_error_rate ${snapshot.taskErrorRate}`);

    lines.push('# HELP yawl_sli_p95_latency_seconds 95th percentile task latency');
    lines.push('# TYPE yawl_sli_p95_latency_seconds gauge');
    lines.push(`yawl_sli_p95_latency_seconds ${snapshot.p95Latency}`);

    lines.push('# HELP yawl_sli_resource_utilization Resource utilization');
    lines.push('# TYPE yawl_sli_resource_utilization gauge');
    lines.push(`yawl_sli_resource_utilization ${snapshot.resourceUtilization}`);

    lines.push('# HELP yawl_slo_compliance SLO compliance score');
    lines.push('# TYPE yawl_slo_compliance gauge');
    lines.push(`yawl_slo_compliance ${snapshot.sloCompliance.score}`);

    return lines.join('\n') + '\n';
  }

  /**
   * Get detailed SLO status report
   * @returns {Object} SLO status report
   */
  getSLOReport() {
    const snapshot = this.getSnapshot();
    const compliance = snapshot.sloCompliance;

    return {
      timestamp: new Date(snapshot.timestamp).toISOString(),
      overall: {
        compliant: compliance.score === 1.0,
        score: compliance.score,
        meetsCount: Object.values(compliance).filter((v) => v === true).length,
        totalCount: 4,
      },
      metrics: [
        {
          name: 'Completion Rate',
          current: snapshot.completionRate,
          target: this.config.targetCompletionRate,
          compliant: compliance.completionRate,
          status: compliance.completionRate ? 'PASS' : 'FAIL',
        },
        {
          name: 'Task Success Rate',
          current: snapshot.taskSuccessRate,
          target: this.config.targetTaskSuccessRate,
          compliant: compliance.taskSuccessRate,
          status: compliance.taskSuccessRate ? 'PASS' : 'FAIL',
        },
        {
          name: 'P95 Latency',
          current: snapshot.p95Latency,
          target: this.config.targetP95Latency,
          compliant: compliance.p95Latency,
          status: compliance.p95Latency ? 'PASS' : 'FAIL',
        },
        {
          name: 'Resource Utilization',
          current: snapshot.resourceUtilization,
          target: this.config.targetResourceUtilization,
          compliant: compliance.resourceUtilization,
          status: compliance.resourceUtilization ? 'PASS' : 'FAIL',
        },
      ],
    };
  }

  /**
   * Reset all collected data
   */
  reset() {
    this._caseEvents = [];
    this._taskEvents = [];
    this._taskDurations = [];
  }

  /**
   * Remove all event listeners and stop cleanup interval
   */
  destroy() {
    clearInterval(this._cleanupInterval);

    this.engine.removeAllListeners('CASE_CREATED');
    this.engine.removeAllListeners('CASE_COMPLETED');
    this.engine.removeAllListeners('TASK_STARTED');
    this.engine.removeAllListeners('TASK_COMPLETED');
    this.engine.removeAllListeners('TASK_CANCELLED');
    this.engine.removeAllListeners('TASK_TIMEOUT');
    this.engine.removeAllListeners('RESOURCE_ALLOCATED');
    this.engine.removeAllListeners('RESOURCE_RELEASED');

    this.reset();
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a YAWLSLICalculator instance
 *
 * @param {Object} engine - YAWL WorkflowEngine instance
 * @param {Object} [config={}] - SLI configuration
 * @returns {YAWLSLICalculator} SLI calculator instance
 *
 * @example
 * ```javascript
 * const sli = createSLICalculator(engine, {
 *   targetCompletionRate: 0.95,
 *   targetP95Latency: 5.0
 * });
 * ```
 */
export function createSLICalculator(engine, config = {}) {
  return new YAWLSLICalculator(engine, config);
}
