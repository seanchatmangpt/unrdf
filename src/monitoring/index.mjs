/**
 * @fileoverview Monitoring - Real-time performance monitoring and optimization
 * @module monitoring
 *
 * @description
 * Complete monitoring solution:
 * - MetricsCollector: Real-time metrics collection
 * - Dashboard: Live ASCII dashboard
 * - AdaptiveOptimizer: Auto-tuning system
 * - PrometheusExporter: Prometheus-compatible metrics
 */

export { MetricsCollector } from './metrics-collector.mjs';
export { Dashboard } from './dashboard.mjs';
export { AdaptiveOptimizer } from './adaptive-optimizer.mjs';
export { PrometheusExporter } from './prometheus-exporter.mjs';

/**
 * Create a complete monitoring system
 * @param {object} options - Configuration options
 * @returns {object} Monitoring system components
 */
export function createMonitoringSystem(options = {}) {
  const { MetricsCollector } = await import('./metrics-collector.mjs');
  const { Dashboard } = await import('./dashboard.mjs');
  const { AdaptiveOptimizer } = await import('./adaptive-optimizer.mjs');
  const { PrometheusExporter } = await import('./prometheus-exporter.mjs');

  const collector = new MetricsCollector(options.metrics);
  const dashboard = new Dashboard(collector, options.dashboard);
  const optimizer = new AdaptiveOptimizer(collector, options.optimizer);
  const exporter = new PrometheusExporter(collector, options.prometheus);

  return {
    collector,
    dashboard,
    optimizer,
    exporter,

    /**
     * Start all monitoring components
     */
    startAll() {
      collector.start();
      optimizer.start();
      exporter.start();
      if (options.showDashboard !== false) {
        dashboard.start();
      }
    },

    /**
     * Stop all monitoring components
     */
    stopAll() {
      dashboard.stop();
      optimizer.stop();
      exporter.stop();
      collector.stop();
    },
  };
}
