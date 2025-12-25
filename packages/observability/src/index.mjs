/**
 * @unrdf/observability - Main Entry Point
 *
 * Innovative Prometheus/Grafana observability for UNRDF distributed workflows.
 * Provides comprehensive metrics collection, dashboard generation, and alerting.
 *
 * @module @unrdf/observability
 */

export { WorkflowMetrics, createWorkflowMetrics, WorkflowStatus } from './metrics/workflow-metrics.mjs';
export { GrafanaExporter, createGrafanaExporter } from './exporters/grafana-exporter.mjs';
export { AlertManager, createAlertManager, AlertSeverity } from './alerts/alert-manager.mjs';

/**
 * Create a complete observability stack
 * @param {object} config - Observability configuration
 * @param {object} [config.metrics] - Metrics configuration
 * @param {object} [config.grafana] - Grafana configuration
 * @param {object} [config.alerts] - Alert configuration
 * @returns {object} Complete observability stack
 */
export function createObservabilityStack(config = {}) {
  const { createWorkflowMetrics } = await import('./metrics/workflow-metrics.mjs');
  const { createGrafanaExporter } = await import('./exporters/grafana-exporter.mjs');
  const { createAlertManager } = await import('./alerts/alert-manager.mjs');

  const metrics = createWorkflowMetrics(config.metrics || {});
  const grafana = createGrafanaExporter(config.grafana || {});
  const alerts = createAlertManager(config.alerts || {});

  // Wire up metrics to alerts
  const originalRecordWorkflowComplete = metrics.recordWorkflowComplete.bind(metrics);
  metrics.recordWorkflowComplete = (workflowId, status, duration, pattern) => {
    originalRecordWorkflowComplete(workflowId, status, duration, pattern);
    alerts.evaluateMetric('workflow_duration', duration, { workflow_id: workflowId, status, pattern });
  };

  const originalRecordError = metrics.recordError.bind(metrics);
  metrics.recordError = (errorType, workflowId, severity) => {
    originalRecordError(errorType, workflowId, severity);
    alerts.evaluateMetric('error_count', 1, { error_type: errorType, workflow_id: workflowId, severity });
  };

  return {
    metrics,
    grafana,
    alerts,
  };
}

export default {
  createWorkflowMetrics,
  createGrafanaExporter,
  createAlertManager,
  createObservabilityStack,
};
