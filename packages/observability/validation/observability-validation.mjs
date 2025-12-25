#!/usr/bin/env node
/**
 * @unrdf/observability - Validation Script
 *
 * Validates all observability features following Adversarial PM principles.
 * Every claim must be PROVEN with evidence.
 *
 * @module @unrdf/observability/validation
 */

import { performance } from 'node:perf_hooks';

/**
 * Observability Validator
 */
class ObservabilityValidator {
  constructor() {
    this.results = [];
    this.metrics = { totalTestTime: 0 };
  }

  log(message) {
    const ts = new Date().toISOString();
    console.log(`[${ts}] ${message}`);
  }

  addResult(result) {
    this.results.push(result);
    const emoji = result.status === 'PASS' ? '✅' : '❌';
    this.log(`${emoji} ${result.claim} - ${result.status}`);
    if (result.evidence) {
      this.log(`   Evidence: ${JSON.stringify(result.evidence)}`);
    }
  }

  /**
   * CLAIM 1: WorkflowMetrics can record and export metrics
   */
  async validateClaim1_MetricsRecording() {
    try {
      const { createWorkflowMetrics } = await import('../src/metrics/workflow-metrics.mjs');

      const metrics = createWorkflowMetrics({
        enableDefaultMetrics: false,
        prefix: 'test_',
      });

      // Record various metrics
      metrics.recordWorkflowStart('test-wf-1', 'SEQUENCE');
      metrics.recordWorkflowComplete('test-wf-1', 'completed', 2.5, 'SEQUENCE');
      metrics.recordTaskExecution('test-wf-1', 'task-1', 'atomic', 'completed', 0.5);
      metrics.recordError('test-error', 'test-wf-1', 'medium');

      // Export metrics
      const prometheusMetrics = await metrics.getMetrics();
      const hasWorkflowMetrics = prometheusMetrics.includes('test_executions_total');
      const hasTaskMetrics = prometheusMetrics.includes('test_task_executions_total');
      const hasErrorMetrics = prometheusMetrics.includes('test_errors_total');

      this.addResult({
        claim: 'WorkflowMetrics records and exports metrics',
        status: (hasWorkflowMetrics && hasTaskMetrics && hasErrorMetrics) ? 'PASS' : 'FAIL',
        evidence: {
          workflowMetrics: hasWorkflowMetrics,
          taskMetrics: hasTaskMetrics,
          errorMetrics: hasErrorMetrics,
        },
      });
    } catch (error) {
      this.addResult({
        claim: 'WorkflowMetrics records and exports metrics',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * CLAIM 2: AlertManager can evaluate thresholds
   */
  async validateClaim2_AlertThresholds() {
    try {
      const { createAlertManager } = await import('../src/alerts/alert-manager.mjs');

      const alerts = createAlertManager({
        rules: [
          {
            id: 'test-rule',
            name: 'Test Rule',
            metric: 'test_metric',
            threshold: 100,
            operator: 'gt',
            severity: 'warning',
          },
        ],
      });

      let alertFired = false;
      alerts.on('alert', (alert) => {
        alertFired = true;
      });

      // Should trigger alert (value > threshold)
      await alerts.evaluateMetric('test_metric', 150);
      await new Promise(resolve => setTimeout(resolve, 100));

      this.addResult({
        claim: 'AlertManager evaluates thresholds correctly',
        status: alertFired ? 'PASS' : 'FAIL',
        evidence: { alertFired, activeAlerts: alerts.getActiveAlerts().length },
      });
    } catch (error) {
      this.addResult({
        claim: 'AlertManager evaluates thresholds correctly',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * CLAIM 3: AlertManager detects anomalies
   */
  async validateClaim3_AnomalyDetection() {
    try {
      const { createAlertManager } = await import('../src/alerts/alert-manager.mjs');

      const alerts = createAlertManager({
        enableAnomalyDetection: true,
      });

      let anomalyDetected = false;
      alerts.on('alert', (alert) => {
        if (alert.type === 'anomaly') {
          anomalyDetected = true;
        }
      });

      // Build baseline (30+ samples required)
      for (let i = 0; i < 35; i++) {
        await alerts.evaluateMetric('anomaly_test', 50 + Math.random() * 5);
      }

      // Inject anomaly (far outside normal range)
      await alerts.evaluateMetric('anomaly_test', 500);
      await new Promise(resolve => setTimeout(resolve, 100));

      this.addResult({
        claim: 'AlertManager detects statistical anomalies',
        status: anomalyDetected ? 'PASS' : 'FAIL',
        evidence: { anomalyDetected },
      });
    } catch (error) {
      this.addResult({
        claim: 'AlertManager detects statistical anomalies',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * CLAIM 4: GrafanaExporter generates valid dashboard JSON
   */
  async validateClaim4_GrafanaDashboard() {
    try {
      const { createGrafanaExporter } = await import('../src/exporters/grafana-exporter.mjs');

      const exporter = createGrafanaExporter({
        title: 'Test Dashboard',
      });

      const dashboard = exporter.generateDashboard();
      const hasDashboard = !!dashboard.dashboard;
      const hasTitle = dashboard.dashboard?.title === 'Test Dashboard';
      const hasPanels = dashboard.dashboard?.panels?.length > 0;
      const hasTemplating = !!dashboard.dashboard?.templating;

      const jsonExport = exporter.exportJSON(true);
      const validJSON = !!JSON.parse(jsonExport);

      this.addResult({
        claim: 'GrafanaExporter generates valid dashboard JSON',
        status: (hasDashboard && hasTitle && hasPanels && validJSON) ? 'PASS' : 'FAIL',
        evidence: {
          hasDashboard,
          hasTitle,
          panelCount: dashboard.dashboard?.panels?.length || 0,
          validJSON,
        },
      });
    } catch (error) {
      this.addResult({
        claim: 'GrafanaExporter generates valid dashboard JSON',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * CLAIM 5: Alert history is tracked correctly
   */
  async validateClaim5_AlertHistory() {
    try {
      const { createAlertManager } = await import('../src/alerts/alert-manager.mjs');

      const alerts = createAlertManager({
        rules: [
          {
            id: 'history-test',
            name: 'History Test',
            metric: 'history_metric',
            threshold: 50,
            operator: 'gt',
            severity: 'info',
          },
        ],
      });

      // Trigger multiple alerts
      await alerts.evaluateMetric('history_metric', 100);
      await alerts.evaluateMetric('history_metric', 75);
      await alerts.evaluateMetric('history_metric', 25); // Should resolve

      const history = alerts.getAlertHistory({ limit: 10 });
      const stats = alerts.getStatistics();

      this.addResult({
        claim: 'Alert history tracked correctly',
        status: (history.length > 0 && stats.total > 0) ? 'PASS' : 'FAIL',
        evidence: {
          historyCount: history.length,
          totalAlerts: stats.total,
          activeAlerts: stats.active,
        },
      });
    } catch (error) {
      this.addResult({
        claim: 'Alert history tracked correctly',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * CLAIM 6: All metric types are supported
   */
  async validateClaim6_MetricTypes() {
    try {
      const { createWorkflowMetrics } = await import('../src/metrics/workflow-metrics.mjs');

      const metrics = createWorkflowMetrics({
        enableDefaultMetrics: false,
        prefix: 'types_test_',
      });

      // Test all metric types
      metrics.recordWorkflowStart('wf-1', 'SEQ'); // Counter + Gauge
      metrics.recordResourceUtilization('cpu', 'node-1', 75.5); // Gauge
      metrics.recordLatency('task_exec', 150); // Summary
      metrics.recordWorkflowComplete('wf-1', 'ok', 1.5, 'SEQ'); // Histogram

      const prometheusMetrics = await metrics.getMetrics();

      const hasCounter = prometheusMetrics.includes('# TYPE types_test_executions_total counter');
      const hasGauge = prometheusMetrics.includes('# TYPE types_test_active_workflows gauge');
      const hasHistogram = prometheusMetrics.includes('# TYPE types_test_execution_duration_seconds histogram');
      const hasSummary = prometheusMetrics.includes('# TYPE types_test_latency_percentiles summary');

      this.addResult({
        claim: 'All Prometheus metric types supported',
        status: (hasCounter && hasGauge && hasHistogram && hasSummary) ? 'PASS' : 'FAIL',
        evidence: { hasCounter, hasGauge, hasHistogram, hasSummary },
      });
    } catch (error) {
      this.addResult({
        claim: 'All Prometheus metric types supported',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * CLAIM 7: Module exports are correct
   */
  async validateClaim7_ModuleExports() {
    try {
      const mainModule = await import('../src/index.mjs');

      const hasWorkflowMetrics = !!mainModule.createWorkflowMetrics;
      const hasGrafanaExporter = !!mainModule.createGrafanaExporter;
      const hasAlertManager = !!mainModule.createAlertManager;
      const hasObservabilityStack = !!mainModule.createObservabilityStack;

      this.addResult({
        claim: 'Module exports all required functions',
        status: (hasWorkflowMetrics && hasGrafanaExporter && hasAlertManager && hasObservabilityStack) ? 'PASS' : 'FAIL',
        evidence: { hasWorkflowMetrics, hasGrafanaExporter, hasAlertManager, hasObservabilityStack },
      });
    } catch (error) {
      this.addResult({
        claim: 'Module exports all required functions',
        status: 'FAIL',
        evidence: { error: error.message },
      });
    }
  }

  /**
   * Run all validations
   */
  async runAll() {
    this.log('Starting @unrdf/observability Validation');
    this.log('='.repeat(50));

    const startTime = performance.now();

    await this.validateClaim1_MetricsRecording();
    await this.validateClaim2_AlertThresholds();
    await this.validateClaim3_AnomalyDetection();
    await this.validateClaim4_GrafanaDashboard();
    await this.validateClaim5_AlertHistory();
    await this.validateClaim6_MetricTypes();
    await this.validateClaim7_ModuleExports();

    this.metrics.totalTestTime = performance.now() - startTime;
    this.printReport();
  }

  /**
   * Print validation report
   */
  printReport() {
    this.log('='.repeat(50));
    const passed = this.results.filter(r => r.status === 'PASS').length;
    const failed = this.results.filter(r => r.status === 'FAIL').length;

    this.log(`RESULTS: ${passed}/${this.results.length} PASSED`);
    this.log(`Failed: ${failed}`);
    this.log(`Total Time: ${this.metrics.totalTestTime.toFixed(2)}ms`);

    // Exit code
    process.exit(passed === this.results.length ? 0 : 1);
  }
}

// Run validation
const validator = new ObservabilityValidator();
validator.runAll().catch(err => {
  console.error('FATAL ERROR:', err.message);
  process.exit(1);
});
