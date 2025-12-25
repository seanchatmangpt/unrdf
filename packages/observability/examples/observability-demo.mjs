#!/usr/bin/env node
/**
 * @unrdf/observability - Live Demo
 *
 * Demonstrates real-time workflow metrics collection, alerting,
 * and Prometheus metrics endpoint exposure.
 *
 * Usage:
 *   node examples/observability-demo.mjs
 *   curl http://localhost:9090/metrics
 *
 * @module @unrdf/observability/examples
 */

import express from 'express';
import { createWorkflowMetrics } from '../src/metrics/workflow-metrics.mjs';
import { createGrafanaExporter } from '../src/exporters/grafana-exporter.mjs';
import { createAlertManager, AlertSeverity } from '../src/alerts/alert-manager.mjs';

/**
 * Simulated workflow execution
 */
class WorkflowSimulator {
  constructor(metrics, alerts) {
    this.metrics = metrics;
    this.alerts = alerts;
    this.workflowCount = 0;
  }

  /**
   * Simulate a workflow execution
   */
  async simulateWorkflow() {
    const workflowId = `wf-${++this.workflowCount}`;
    const pattern = this._randomPattern();
    const taskCount = Math.floor(Math.random() * 10) + 1;

    console.log(`[${new Date().toISOString()}] Starting workflow ${workflowId} (pattern: ${pattern}, tasks: ${taskCount})`);

    // Record workflow start
    this.metrics.recordWorkflowStart(workflowId, pattern);

    const startTime = Date.now();

    // Simulate tasks
    for (let i = 0; i < taskCount; i++) {
      await this._simulateTask(workflowId, i);
    }

    // Random workflow outcome
    const success = Math.random() > 0.1; // 90% success rate
    const duration = (Date.now() - startTime) / 1000;
    const status = success ? 'completed' : 'failed';

    // Record workflow completion
    this.metrics.recordWorkflowComplete(workflowId, status, duration, pattern);

    // Evaluate against alert rules
    await this.alerts.evaluateMetric('workflow_duration', duration, { workflow_id: workflowId, status, pattern });

    if (!success) {
      this.metrics.recordError('execution_failed', workflowId, 'high');
    }

    // Record event sourcing
    this.metrics.recordEventAppended('workflow_completed', workflowId);
    this.metrics.updateEventStoreSize(workflowId, Math.floor(Math.random() * 1000000));

    // Simulate crypto receipt
    if (success) {
      this.metrics.recordCryptoReceipt(workflowId, 'BLAKE3');
    }

    console.log(`[${new Date().toISOString()}] Workflow ${workflowId} ${status} in ${duration.toFixed(2)}s`);

    return { workflowId, status, duration };
  }

  /**
   * Simulate a task execution
   * @private
   */
  async _simulateTask(workflowId, taskIndex) {
    const taskId = `task-${taskIndex}`;
    const taskType = this._randomTaskType();
    const startTime = Date.now();

    // Simulate task execution time
    const executionTime = Math.random() * 500 + 50; // 50-550ms
    await new Promise(resolve => setTimeout(resolve, executionTime));

    const success = Math.random() > 0.05; // 95% success rate
    const duration = (Date.now() - startTime) / 1000;
    const status = success ? 'completed' : 'failed';

    // Record task execution
    this.metrics.recordTaskExecution(workflowId, taskId, taskType, status, duration);

    // Record latency
    this.metrics.recordLatency(taskType, executionTime);

    // Update queue depth (random)
    const queueDepth = Math.floor(Math.random() * 20);
    this.metrics.updateTaskQueueDepth(workflowId, 'default', queueDepth);

    if (!success) {
      this.metrics.recordError('task_execution_failed', workflowId, 'medium');
    }

    return { taskId, status, duration };
  }

  /**
   * Simulate resource monitoring
   */
  simulateResourceMetrics() {
    // CPU utilization
    const cpuUtil = Math.random() * 100;
    this.metrics.recordResourceUtilization('cpu', 'node-0', cpuUtil);
    this.alerts.evaluateMetric('cpu_utilization', cpuUtil, { resource_id: 'node-0' });

    // Memory utilization
    const memUtil = Math.random() * 100;
    this.metrics.recordResourceUtilization('memory', 'node-0', memUtil);
    this.alerts.evaluateMetric('memory_utilization', memUtil, { resource_id: 'node-0' });

    // Disk utilization
    const diskUtil = Math.random() * 100;
    this.metrics.recordResourceUtilization('disk', 'node-0', diskUtil);

    // Random resource allocations
    if (Math.random() > 0.7) {
      this.metrics.recordResourceAllocation('compute', Math.random() > 0.9 ? 'failed' : 'allocated');
    }
  }

  /**
   * Simulate policy evaluations
   */
  simulatePolicyEvaluations() {
    const policies = ['resource-allocation', 'task-enablement', 'workflow-completion'];
    const policy = policies[Math.floor(Math.random() * policies.length)];
    const result = Math.random() > 0.1 ? 'allow' : 'deny';

    this.metrics.recordPolicyEvaluation(policy, result);
  }

  _randomPattern() {
    const patterns = ['SEQUENCE', 'PARALLEL_SPLIT', 'EXCLUSIVE_CHOICE', 'MULTI_CHOICE'];
    return patterns[Math.floor(Math.random() * patterns.length)];
  }

  _randomTaskType() {
    const types = ['atomic', 'composite', 'service', 'user'];
    return types[Math.floor(Math.random() * types.length)];
  }
}

/**
 * Main demo application
 */
async function main() {
  console.log('=== UNRDF Observability Dashboard Demo ===\n');

  // Create observability stack
  const metrics = createWorkflowMetrics({
    enableDefaultMetrics: true,
    prefix: 'unrdf_workflow_',
    labels: { environment: 'demo', version: '1.0.0' },
  });

  const grafana = createGrafanaExporter({
    title: 'UNRDF Workflow Dashboard - Demo',
    datasource: 'Prometheus',
  });

  const alerts = createAlertManager({
    rules: [
      {
        id: 'high-workflow-duration',
        name: 'High Workflow Duration',
        metric: 'workflow_duration',
        threshold: 5,
        operator: 'gt',
        severity: 'warning',
      },
      {
        id: 'high-cpu-utilization',
        name: 'High CPU Utilization',
        metric: 'cpu_utilization',
        threshold: 80,
        operator: 'gt',
        severity: 'critical',
      },
      {
        id: 'high-memory-utilization',
        name: 'High Memory Utilization',
        metric: 'memory_utilization',
        threshold: 85,
        operator: 'gt',
        severity: 'critical',
      },
    ],
    enableAnomalyDetection: true,
  });

  // Alert event handlers
  alerts.on('alert', (alert) => {
    console.log(`\n🚨 ALERT FIRED: ${alert.name} (${alert.severity})`);
    console.log(`   Metric: ${alert.metric} = ${alert.value} (threshold: ${alert.threshold})`);
  });

  alerts.on('alert:resolved', (alert) => {
    console.log(`\n✅ ALERT RESOLVED: ${alert.name}`);
  });

  // Create Express app for metrics endpoint
  const app = express();
  const PORT = process.env.PORT || 9090;

  // Metrics endpoint
  app.get('/metrics', async (req, res) => {
    try {
      const metricsText = await metrics.getMetrics();
      res.set('Content-Type', 'text/plain; version=0.0.4; charset=utf-8');
      res.send(metricsText);
    } catch (error) {
      res.status(500).send(`Error generating metrics: ${error.message}`);
    }
  });

  // Metrics JSON endpoint
  app.get('/metrics/json', async (req, res) => {
    try {
      const metricsJSON = await metrics.getMetricsJSON();
      res.json(metricsJSON);
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });

  // Dashboard endpoint
  app.get('/dashboard', (req, res) => {
    const dashboard = grafana.generateDashboard();
    res.json(dashboard);
  });

  // Dashboard JSON export
  app.get('/dashboard/export', (req, res) => {
    const dashboardJSON = grafana.exportJSON(true);
    res.set('Content-Type', 'application/json');
    res.set('Content-Disposition', 'attachment; filename="unrdf-dashboard.json"');
    res.send(dashboardJSON);
  });

  // Alerts endpoint
  app.get('/alerts', (req, res) => {
    const activeAlerts = alerts.getActiveAlerts();
    res.json({ alerts: activeAlerts, count: activeAlerts.length });
  });

  // Alert history endpoint
  app.get('/alerts/history', (req, res) => {
    const history = alerts.getAlertHistory({ limit: 50 });
    res.json({ history, count: history.length });
  });

  // Statistics endpoint
  app.get('/stats', (req, res) => {
    const stats = alerts.getStatistics();
    res.json(stats);
  });

  // Health check
  app.get('/health', (req, res) => {
    res.json({ status: 'healthy', timestamp: Date.now() });
  });

  // Start server
  app.listen(PORT, () => {
    console.log(`✅ Metrics server running on http://localhost:${PORT}`);
    console.log(`   Prometheus metrics: http://localhost:${PORT}/metrics`);
    console.log(`   Metrics JSON: http://localhost:${PORT}/metrics/json`);
    console.log(`   Grafana dashboard: http://localhost:${PORT}/dashboard`);
    console.log(`   Download dashboard: http://localhost:${PORT}/dashboard/export`);
    console.log(`   Active alerts: http://localhost:${PORT}/alerts`);
    console.log(`   Alert history: http://localhost:${PORT}/alerts/history`);
    console.log(`   Statistics: http://localhost:${PORT}/stats`);
    console.log('\nSimulating workflows...\n');
  });

  // Create simulator
  const simulator = new WorkflowSimulator(metrics, alerts);

  // Simulate workflows periodically
  setInterval(() => {
    simulator.simulateWorkflow().catch(console.error);
  }, 3000); // Every 3 seconds

  // Simulate resource metrics
  setInterval(() => {
    simulator.simulateResourceMetrics();
  }, 5000); // Every 5 seconds

  // Simulate policy evaluations
  setInterval(() => {
    simulator.simulatePolicyEvaluations();
  }, 2000); // Every 2 seconds

  // Graceful shutdown
  process.on('SIGINT', () => {
    console.log('\n\nShutting down gracefully...');
    console.log('\nFinal Statistics:');
    console.log(JSON.stringify(alerts.getStatistics(), null, 2));
    process.exit(0);
  });
}

// Run demo
main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
