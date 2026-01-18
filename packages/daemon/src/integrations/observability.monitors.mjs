/**
 * @file Daemon Observability Monitors
 * @module @unrdf/daemon/integrations/observability-monitors
 * @description Monitoring components: YAWL metrics, alerts, and health monitoring
 */

import { z } from 'zod';
import { ConfigSchema } from './observability.schema.mjs';

/**
 * YAWL workflow metrics collector
 * Tracks workflow execution and case management metrics
 */
export class YawlMetricsCollector {
  /**
   * Create YAWL metrics collector
   * @param {Object} config - Collector configuration
   */
  constructor(config = {}) {
    this.config = ConfigSchema.parse(config);
    this.caseMetrics = new Map();
    this.taskMetrics = new Map();
    this.workflowMetrics = new Map();
    this.history = [];
  }

  /**
   * Record case creation
   * @param {string} caseId - Case ID
   * @param {string} specId - Specification ID
   * @param {Object} metadata - Case metadata
   */
  recordCaseCreation(caseId, specId, metadata = {}) {
    const caseMetric = {
      caseId,
      specId,
      createdAt: Date.now(),
      metadata,
      tasks: [],
      status: 'created',
      duration: null,
    };

    this.caseMetrics.set(caseId, caseMetric);
    this.history.push({ type: 'case_created', caseId, timestamp: Date.now() });
  }

  /**
   * Record task execution
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task ID
   * @param {Object} metadata - Task metadata
   */
  recordTaskExecution(caseId, taskId, metadata = {}) {
    const caseMetric = this.caseMetrics.get(caseId);
    if (!caseMetric) return;

    const taskMetric = {
      taskId,
      startTime: Date.now(),
      metadata,
      status: 'executing',
      duration: null,
      retries: 0,
    };

    caseMetric.tasks.push(taskMetric);
    this.taskMetrics.set(taskId, taskMetric);
    this.history.push({ type: 'task_started', caseId, taskId, timestamp: Date.now() });
  }

  /**
   * Record task completion
   * @param {string} taskId - Task ID
   * @param {string} result - Task result (success, failure, timeout)
   * @param {Object} metadata - Completion metadata
   */
  recordTaskCompletion(taskId, result, metadata = {}) {
    const taskMetric = this.taskMetrics.get(taskId);
    if (!taskMetric) return;

    taskMetric.endTime = Date.now();
    taskMetric.duration = taskMetric.endTime - taskMetric.startTime;
    taskMetric.status = result;
    taskMetric.metadata = { ...taskMetric.metadata, ...metadata };

    this.history.push({ type: 'task_completed', taskId, result, timestamp: Date.now() });
  }

  /**
   * Record task retry
   * @param {string} taskId - Task ID
   * @param {Object} _metadata - Retry metadata (reserved for future use)
   */
  recordTaskRetry(taskId, _metadata = {}) {
    const taskMetric = this.taskMetrics.get(taskId);
    if (!taskMetric) return;

    taskMetric.retries += 1;
    this.history.push({ type: 'task_retried', taskId, retries: taskMetric.retries, timestamp: Date.now() });
  }

  /**
   * Record case completion
   * @param {string} caseId - Case ID
   * @param {string} result - Case result (success, failure)
   * @param {Object} metadata - Completion metadata
   */
  recordCaseCompletion(caseId, result, metadata = {}) {
    const caseMetric = this.caseMetrics.get(caseId);
    if (!caseMetric) return;

    caseMetric.endTime = Date.now();
    caseMetric.duration = caseMetric.endTime - caseMetric.createdAt;
    caseMetric.status = result;
    caseMetric.metadata = { ...caseMetric.metadata, ...metadata };

    this.history.push({ type: 'case_completed', caseId, result, timestamp: Date.now() });
  }

  /**
   * Get workflow metrics
   * @param {string} specId - Specification ID
   * @returns {Object} Workflow metrics
   */
  getWorkflowMetrics(specId) {
    const casesForSpec = Array.from(this.caseMetrics.values()).filter((c) => c.specId === specId);

    const completedCases = casesForSpec.filter(
      (c) => c.status && (c.status === 'success' || c.status === 'completed')
    ).length;
    const failedCases = casesForSpec.filter(
      (c) => c.status && (c.status === 'failure' || c.status === 'failed' || c.status === 'error')
    ).length;
    const casesWithDuration = casesForSpec.filter((c) => c.duration !== null && c.duration !== undefined);
    const avgDuration =
      casesWithDuration.length > 0
        ? casesWithDuration.reduce((sum, c) => sum + c.duration, 0) / casesWithDuration.length
        : 0;

    return {
      specId,
      totalCases: casesForSpec.length,
      completedCases,
      failedCases,
      avgDuration,
      totalTasks: casesForSpec.reduce((sum, c) => sum + c.tasks.length, 0),
    };
  }

  /** Get case metrics */
  getCaseMetrics(caseId) {
    return this.caseMetrics.get(caseId);
  }

  /** Get all metrics */
  getAllMetrics() {
    const specIds = new Set(Array.from(this.caseMetrics.values()).map((c) => c.specId));

    return {
      totalCases: this.caseMetrics.size,
      workflows: Array.from(specIds).map((specId) => this.getWorkflowMetrics(specId)),
      recentHistory: this.history.slice(-100),
    };
  }
}

/**
 * Alert manager for daemon with threshold-based triggering
 */
export class AlertManager {
  /**
   * Create alert manager
   * @param {Object} config - Alert configuration
   * @param {number} [config.queueBacklogThreshold=100] - Queue backlog alert threshold
   * @param {number} [config.errorRateThreshold=0.1] - Error rate alert threshold (0-1)
   * @param {number} [config.responseTimeThreshold=5000] - Response time threshold (ms)
   */
  constructor(config = {}) {
    this.config = {
      queueBacklogThreshold: config.queueBacklogThreshold || 100,
      errorRateThreshold: config.errorRateThreshold || 0.1,
      responseTimeThreshold: config.responseTimeThreshold || 5000,
    };

    this.alerts = [];
    this.rules = [];
    this.metrics = {};
  }

  /**
   * Add alert rule
   * @param {Object} rule - Alert rule
   * @param {string} rule.name - Rule name
   * @param {string} rule.condition - Condition to evaluate
   * @param {string} rule.severity - Alert severity (info, warning, error, critical)
   * @param {Function} rule.evaluate - Evaluation function
   */
  addRule(rule) {
    const validated = z
      .object({
        name: z.string(),
        condition: z.string(),
        severity: z.enum(['info', 'warning', 'error', 'critical']),
        evaluate: z.function(),
      })
      .parse(rule);

    this.rules.push(validated);
  }

  /**
   * Evaluate metric against rules
   * @param {string} metric - Metric name
   * @param {number} value - Metric value
   * @param {Object} context - Evaluation context
   * @returns {Array} Triggered alerts
   */
  evaluateMetric(metric, value, context = {}) {
    const triggered = [];

    for (const rule of this.rules) {
      try {
        if (rule.evaluate({ metric, value, context })) {
          const alert = {
            ruleId: rule.name,
            severity: rule.severity,
            metric,
            value,
            context,
            timestamp: Date.now(),
            message: `${rule.name}: ${metric}=${value}`,
          };

          this.alerts.push(alert);
          triggered.push(alert);

          if (this.alerts.length > 1000) {
            this.alerts.shift();
          }
        }
      } catch (error) {
        // Silently ignore evaluation errors
      }
    }

    return triggered;
  }

  /** Get active alerts */
  getActiveAlerts() {
    const oneMinuteAgo = Date.now() - 60000;
    return this.alerts.filter((a) => a.timestamp > oneMinuteAgo);
  }

  /** Clear resolved alerts */
  clearAlerts(resolvedAlertIds = []) {
    this.alerts = this.alerts.filter((a) => !resolvedAlertIds.includes(a.ruleId));
  }
}

/**
 * Integrate alert manager with daemon
 * @param {Object} daemon - Daemon instance
 * @param {Object} alertManager - Alert manager instance
 * @returns {Object} Alert handler
 */
export function integrateAlertManager(daemon, alertManager) {
  const noop = { evaluate: () => {}, onOperationFailure: () => {}, onQueueBacklogHigh: () => {}, onHealthDegraded: () => {} };
  if (!alertManager) return noop;

  const evaluate = (metric, value, context = {}) => {
    if (alertManager?.evaluateMetric) alertManager.evaluateMetric(metric, value, context);
  };

  return {
    evaluate,
    onOperationFailure(reason, ctx = {}) {
      evaluate('daemon.operation.failed', 1, { reason, severity: 'error', ...ctx });
    },
    onQueueBacklogHigh(backlog, threshold) {
      if (backlog > threshold) evaluate('daemon.queue.backlog.high', backlog, { threshold, severity: 'warning' });
    },
    onHealthDegraded(status) {
      evaluate('daemon.health.degraded', 1, { status, severity: status === 'unhealthy' ? 'critical' : 'warning' });
    },
  };
}

/**
 * Health monitor for daemon operations
 * Tracks success rates and health status (healthy >95%, degraded 80-95%, unhealthy <80%)
 */
export class DaemonHealthMonitor {
  /**
   * Create health monitor
   * @param {Object} config - Health monitor configuration
   * @param {string} [config.serviceName='unrdf-daemon'] - Service name
   * @param {number} [config.queueBacklogThreshold=100] - Queue backlog threshold
   * @param {number} [config.healthCheckInterval=10000] - Health check interval (ms)
   */
  constructor(config = {}) {
    this.config = ConfigSchema.parse(config);
    this.successCount = 0;
    this.failureCount = 0;
    this.queueBacklog = 0;
    this.durations = [];
    this.status = 'healthy';
    this.lastCheck = Date.now();
  }

  /** Initialize periodic health checks */
  async initialize() {
    this.timer = setInterval(() => this._check(), this.config.healthCheckInterval);
  }

  /** Record successful operation */
  recordSuccess(duration) {
    this.successCount++;
    this.durations.push(duration);
    if (this.durations.length > 1000) this.durations.shift();
  }

  /** Record failed operation */
  recordFailure() {
    this.failureCount++;
  }

  /** Update queue backlog size */
  updateQueueBacklog(backlog) {
    this.queueBacklog = backlog;
  }

  /** Get current health status */
  getHealthStatus() {
    const total = this.successCount + this.failureCount;
    if (total === 0) return 'healthy';
    const rate = this.successCount / total;
    return rate > 0.95 ? 'healthy' : rate >= 0.8 ? 'degraded' : 'unhealthy';
  }

  /** Get current metrics snapshot */
  getMetrics() {
    const total = this.successCount + this.failureCount;
    const rate = total > 0 ? (this.successCount / total) * 100 : 0;
    const avg = this.durations.length > 0
      ? this.durations.reduce((a, b) => a + b, 0) / this.durations.length : 0;

    const sorted = [...this.durations].sort((a, b) => a - b);
    const pct = (p) => sorted[Math.floor(sorted.length * p)] || 0;

    return {
      status: this.getHealthStatus(),
      successCount: this.successCount,
      failureCount: this.failureCount,
      successRate: Math.round(rate * 100) / 100,
      avgDuration: Math.round(avg * 100) / 100,
      p50: pct(0.5),
      p95: pct(0.95),
      p99: pct(0.99),
      queueBacklog: this.queueBacklog,
      lastCheck: this.lastCheck,
    };
  }

  /** @private Perform health check and update status */
  _check() {
    const newStatus = this.getHealthStatus();
    if (newStatus !== this.status) {
      this.status = newStatus;
      console.log(`[DaemonHealthMonitor] Health: ${newStatus}`);
    }
    this.lastCheck = Date.now();
  }

  /** Shutdown health monitor and clear interval */
  async shutdown() {
    if (this.timer) clearInterval(this.timer);
  }
}
