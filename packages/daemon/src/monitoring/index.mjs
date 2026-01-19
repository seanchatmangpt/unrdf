/**
 * @file Daemon Monitoring Integration
 * @module @unrdf/daemon/monitoring
 * @description Comprehensive monitoring suite including health checks,
 * Prometheus-style metrics collection, and alerting with notifications.
 */

// Health Check System
export {
  HealthCheckSystem,
  createHealthCheckSystem,
  HealthStatus,
  HealthCheckResultSchema,
  DependencyCheckConfigSchema,
  HealthCheckConfigSchema,
  HealthReportSchema,
} from './health-check.mjs';

// Metrics Collection
export {
  MetricsCollector,
  createMetricsCollector,
  Counter,
  Gauge,
  Histogram,
  MetricType,
  LabelsSchema,
  CounterConfigSchema,
  GaugeConfigSchema,
  HistogramConfigSchema,
  MetricsCollectorConfigSchema,
} from './metrics-collector.mjs';

// Alerting System
export {
  AlertManager,
  createAlertManager,
  AlertSeverity,
  AlertState,
  ComparisonOperator,
  ThresholdAlertRuleSchema,
  RateOfChangeAlertRuleSchema,
  AlertRuleSchema,
  AlertSchema,
  WebhookChannelSchema,
  ConsoleChannelSchema,
  NotificationChannelSchema,
  AlertingConfigSchema,
} from './alerting.mjs';
