/**
 * @unrdf/observability - Grafana Dashboard Exporter
 *
 * Generates and exports Grafana dashboard configurations for UNRDF workflows.
 * Provides pre-built dashboards for workflow monitoring, resource utilization,
 * and performance analysis.
 *
 * @module @unrdf/observability/exporters
 */

import { z } from 'zod';

/**
 * Dashboard configuration schema
 * @type {z.ZodObject}
 */
const DashboardConfigSchema = z.object({
  title: z.string(),
  datasource: z.string().default('Prometheus'),
  refreshInterval: z.string().default('5s'),
  timeRange: z.object({
    from: z.string().default('now-1h'),
    to: z.string().default('now'),
  }).default({}),
  tags: z.array(z.string()).default(['unrdf', 'workflow']),
});

/**
 * GrafanaExporter - Dashboard configuration generator
 *
 * Creates Grafana dashboard JSON configurations with:
 * - Workflow execution overview
 * - Task performance panels
 * - Resource utilization graphs
 * - Error rate monitoring
 * - Custom business metrics
 *
 * @class
 */
export class GrafanaExporter {
  /**
   * @param {object} config - Dashboard configuration
   * @param {string} config.title - Dashboard title
   * @param {string} [config.datasource='Prometheus'] - Prometheus datasource name
   * @param {string} [config.refreshInterval='5s'] - Dashboard refresh interval
   */
  constructor(config = {}) {
    this.config = DashboardConfigSchema.parse({
      title: 'UNRDF Workflow Dashboard',
      ...config,
    });
  }

  /**
   * Generate complete Grafana dashboard JSON
   * @returns {object} Grafana dashboard configuration
   */
  generateDashboard() {
    return {
      dashboard: {
        id: null,
        uid: 'unrdf-workflow-dashboard',
        title: this.config.title,
        tags: this.config.tags,
        timezone: 'browser',
        schemaVersion: 38,
        version: 1,
        refresh: this.config.refreshInterval,
        time: this.config.timeRange,
        timepicker: {
          refresh_intervals: ['5s', '10s', '30s', '1m', '5m', '15m', '30m', '1h'],
          time_options: ['5m', '15m', '1h', '6h', '12h', '24h', '2d', '7d', '30d'],
        },
        panels: this._generatePanels(),
        templating: this._generateTemplating(),
        annotations: {
          list: [
            {
              datasource: this.config.datasource,
              enable: true,
              hide: false,
              iconColor: 'rgba(255, 96, 96, 1)',
              name: 'Workflow Events',
              target: {
                expr: 'unrdf_workflow_executions_total',
              },
            },
          ],
        },
      },
      overwrite: true,
    };
  }

  /**
   * Generate dashboard panels
   * @private
   * @returns {object[]} Panel configurations
   */
  _generatePanels() {
    return [
      this._createWorkflowOverviewPanel(),
      this._createActiveWorkflowsPanel(),
      this._createWorkflowDurationPanel(),
      this._createTaskExecutionPanel(),
      this._createErrorRatePanel(),
      this._createResourceUtilizationPanel(),
      this._createEventStorePanel(),
      this._createLatencyPercentilesPanel(),
    ];
  }

  /**
   * Create workflow overview panel
   * @private
   */
  _createWorkflowOverviewPanel() {
    return {
      id: 1,
      gridPos: { h: 8, w: 12, x: 0, y: 0 },
      type: 'graph',
      title: 'Workflow Executions by Status',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'rate(unrdf_workflow_executions_total[5m])',
          legendFormat: '{{status}} - {{pattern}}',
          refId: 'A',
        },
      ],
      yaxes: [
        { format: 'short', label: 'Executions/sec' },
        { format: 'short' },
      ],
      legend: { show: true, alignAsTable: true, values: true },
    };
  }

  /**
   * Create active workflows gauge panel
   * @private
   */
  _createActiveWorkflowsPanel() {
    return {
      id: 2,
      gridPos: { h: 8, w: 12, x: 12, y: 0 },
      type: 'stat',
      title: 'Active Workflows',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'sum(unrdf_workflow_active_workflows)',
          refId: 'A',
        },
      ],
      options: {
        orientation: 'auto',
        textMode: 'value_and_name',
        colorMode: 'value',
        graphMode: 'area',
      },
      fieldConfig: {
        defaults: {
          thresholds: {
            mode: 'absolute',
            steps: [
              { value: 0, color: 'green' },
              { value: 50, color: 'yellow' },
              { value: 100, color: 'red' },
            ],
          },
        },
      },
    };
  }

  /**
   * Create workflow duration histogram panel
   * @private
   */
  _createWorkflowDurationPanel() {
    return {
      id: 3,
      gridPos: { h: 8, w: 24, x: 0, y: 8 },
      type: 'heatmap',
      title: 'Workflow Execution Duration Distribution',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'rate(unrdf_workflow_execution_duration_seconds_bucket[5m])',
          legendFormat: '{{le}}',
          refId: 'A',
        },
      ],
      heatmap: {
        colorScheme: 'interpolateSpectral',
      },
    };
  }

  /**
   * Create task execution panel
   * @private
   */
  _createTaskExecutionPanel() {
    return {
      id: 4,
      gridPos: { h: 8, w: 12, x: 0, y: 16 },
      type: 'graph',
      title: 'Task Executions by Type',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'rate(unrdf_workflow_task_executions_total[5m])',
          legendFormat: '{{task_type}} - {{status}}',
          refId: 'A',
        },
      ],
      yaxes: [
        { format: 'short', label: 'Tasks/sec' },
        { format: 'short' },
      ],
    };
  }

  /**
   * Create error rate panel
   * @private
   */
  _createErrorRatePanel() {
    return {
      id: 5,
      gridPos: { h: 8, w: 12, x: 12, y: 16 },
      type: 'graph',
      title: 'Error Rate by Severity',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'rate(unrdf_workflow_errors_total[5m])',
          legendFormat: '{{severity}} - {{error_type}}',
          refId: 'A',
        },
      ],
      yaxes: [
        { format: 'short', label: 'Errors/sec' },
        { format: 'short' },
      ],
      alert: {
        conditions: [
          {
            evaluator: { params: [1], type: 'gt' },
            operator: { type: 'and' },
            query: { params: ['A', '5m', 'now'] },
            reducer: { params: [], type: 'avg' },
            type: 'query',
          },
        ],
      },
    };
  }

  /**
   * Create resource utilization panel
   * @private
   */
  _createResourceUtilizationPanel() {
    return {
      id: 6,
      gridPos: { h: 8, w: 12, x: 0, y: 24 },
      type: 'graph',
      title: 'Resource Utilization',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'unrdf_workflow_resource_utilization',
          legendFormat: '{{resource_type}} - {{resource_id}}',
          refId: 'A',
        },
      ],
      yaxes: [
        { format: 'percent', label: 'Utilization %', max: 100, min: 0 },
        { format: 'short' },
      ],
    };
  }

  /**
   * Create event store panel
   * @private
   */
  _createEventStorePanel() {
    return {
      id: 7,
      gridPos: { h: 8, w: 12, x: 12, y: 24 },
      type: 'graph',
      title: 'Event Store Metrics',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'rate(unrdf_workflow_events_appended_total[5m])',
          legendFormat: '{{event_type}} events/sec',
          refId: 'A',
        },
        {
          expr: 'unrdf_workflow_event_store_size_bytes',
          legendFormat: 'Store size ({{workflow_id}})',
          refId: 'B',
        },
      ],
    };
  }

  /**
   * Create latency percentiles panel
   * @private
   */
  _createLatencyPercentilesPanel() {
    return {
      id: 8,
      gridPos: { h: 8, w: 24, x: 0, y: 32 },
      type: 'graph',
      title: 'Operation Latency Percentiles',
      datasource: this.config.datasource,
      targets: [
        {
          expr: 'unrdf_workflow_latency_percentiles{quantile="0.5"}',
          legendFormat: 'p50 - {{operation}}',
          refId: 'A',
        },
        {
          expr: 'unrdf_workflow_latency_percentiles{quantile="0.9"}',
          legendFormat: 'p90 - {{operation}}',
          refId: 'B',
        },
        {
          expr: 'unrdf_workflow_latency_percentiles{quantile="0.99"}',
          legendFormat: 'p99 - {{operation}}',
          refId: 'C',
        },
      ],
      yaxes: [
        { format: 's', label: 'Latency' },
        { format: 'short' },
      ],
    };
  }

  /**
   * Generate dashboard templating/variables
   * @private
   */
  _generateTemplating() {
    return {
      list: [
        {
          name: 'workflow_id',
          type: 'query',
          datasource: this.config.datasource,
          query: 'label_values(unrdf_workflow_executions_total, workflow_id)',
          multi: true,
          includeAll: true,
          refresh: 1,
        },
        {
          name: 'pattern',
          type: 'query',
          datasource: this.config.datasource,
          query: 'label_values(unrdf_workflow_executions_total, pattern)',
          multi: true,
          includeAll: true,
          refresh: 1,
        },
      ],
    };
  }

  /**
   * Export dashboard to JSON string
   * @param {boolean} pretty - Pretty print JSON
   * @returns {string} Dashboard JSON
   */
  exportJSON(pretty = true) {
    const dashboard = this.generateDashboard();
    return JSON.stringify(dashboard, null, pretty ? 2 : 0);
  }

  /**
   * Generate alert dashboard
   * @returns {object} Alert-focused dashboard configuration
   */
  generateAlertDashboard() {
    return {
      dashboard: {
        id: null,
        uid: 'unrdf-alerts-dashboard',
        title: 'UNRDF Alerts & SLOs',
        tags: [...this.config.tags, 'alerts'],
        panels: [
          {
            id: 1,
            gridPos: { h: 8, w: 24, x: 0, y: 0 },
            type: 'table',
            title: 'Active Alerts',
            datasource: this.config.datasource,
            targets: [
              {
                expr: 'ALERTS{alertstate="firing"}',
                refId: 'A',
              },
            ],
          },
        ],
      },
      overwrite: true,
    };
  }
}

/**
 * Create Grafana exporter instance
 * @param {object} config - Dashboard configuration
 * @returns {GrafanaExporter} Exporter instance
 */
export function createGrafanaExporter(config = {}) {
  return new GrafanaExporter(config);
}

export default GrafanaExporter;
