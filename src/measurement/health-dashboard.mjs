/**
 * @fileoverview Health Dashboard - Real-time metrics emitter
 *
 * Provides real-time monitoring and alerting for system health metrics.
 * Integrates all four core measurements (D_t, TC, TE, C_t) into a
 * unified dashboard with time-series storage and anomaly detection.
 *
 * Features:
 * - Real-time metrics computation at configurable intervals
 * - Time-series storage for trend analysis
 * - Alerting based on configurable thresholds
 * - Multiple output formats (console, JSON, events)
 * - Comparison against baselines
 *
 * @module measurement/health-dashboard
 */

import { z } from 'zod';
import { DimensionComputer } from './dimension-computer.mjs';
import { CorrelationComputer } from './correlation-computer.mjs';
import { TransferEntropyComputer } from './transfer-entropy-computer.mjs';
import { CapacityComputer } from './capacity-computer.mjs';
import { CertificateGenerator } from './certificate-generator.mjs';

/**
 * Alert configuration schema
 */
const AlertConfigSchema = z.object({
  dimension: z.object({
    minValue: z.number().default(10),
    maxChange: z.number().default(20),
    maxUtilization: z.number().default(90)
  }).default({}),
  correlation: z.object({
    maxTC: z.number().default(0.7),
    maxNormalizedTC: z.number().default(0.8),
    maxChange: z.number().default(30)
  }).default({}),
  transferEntropy: z.object({
    maxTE: z.number().default(0.9),
    maxCausalEdges: z.number().default(20)
  }).default({}),
  capacity: z.object({
    minAdmissionRate: z.number().default(0.5),
    minThroughput: z.number().default(0.1),
    maxChange: z.number().default(50)
  }).default({})
});

/**
 * Dashboard configuration schema
 */
const DashboardConfigSchema = z.object({
  updateInterval: z.number().default(60000),
  historySize: z.number().default(1000),
  alertConfig: AlertConfigSchema.default({}),
  outputFormat: z.enum(['console', 'json', 'silent']).default('console'),
  enableAlerts: z.boolean().default(true),
  enableTrends: z.boolean().default(true),
  enableForecasts: z.boolean().default(false)
});

/**
 * Health Dashboard - Unified real-time monitoring
 *
 * @class HealthDashboard
 * @example
 * const dashboard = new HealthDashboard();
 * dashboard.start(universe, receipts);
 *
 * // Listen for alerts
 * dashboard.on('alert', alert => console.log('Alert:', alert));
 *
 * // Get current metrics
 * const metrics = dashboard.getCurrentMetrics();
 */
export class HealthDashboard {
  /**
   * Create a new Health Dashboard
   * @param {Object} [config] - Dashboard configuration
   */
  constructor(config = {}) {
    this.config = DashboardConfigSchema.parse(config);

    // Initialize computers
    this.dimensionComputer = new DimensionComputer();
    this.correlationComputer = new CorrelationComputer();
    this.transferEntropyComputer = new TransferEntropyComputer();
    this.capacityComputer = new CapacityComputer();
    this.certificateGenerator = new CertificateGenerator();

    /** @type {Array<Object>} Time series of measurements */
    this.timeSeries = [];

    /** @type {Object|null} Current metrics snapshot */
    this.currentMetrics = null;

    /** @type {Array<Object>} Active alerts */
    this.activeAlerts = [];

    /** @type {Map<string, Function>} Event listeners */
    this._listeners = new Map();

    /** @type {NodeJS.Timer|null} Update interval timer */
    this._updateTimer = null;

    /** @type {boolean} Is dashboard running */
    this._running = false;

    /** @type {Object|null} Baseline for comparison */
    this._baseline = null;

    /** @type {number} Dashboard start time */
    this._startTime = null;
  }

  /**
   * Start the health dashboard
   *
   * @param {Object} universe - Universe to monitor
   * @param {Object} [dataSource] - Optional data source for receipts/events
   * @returns {void}
   */
  start(universe, dataSource = {}) {
    if (this._running) {
      console.warn('Dashboard is already running');
      return;
    }

    this._running = true;
    this._startTime = Date.now();
    this._universe = universe;
    this._dataSource = dataSource;

    // Initial measurement
    this._performMeasurement();

    // Set up periodic updates
    if (this.config.updateInterval > 0) {
      this._updateTimer = setInterval(
        () => this._performMeasurement(),
        this.config.updateInterval
      );
    }

    this._emit('started', { timestamp: new Date().toISOString() });
    this._log('Dashboard started');
  }

  /**
   * Stop the health dashboard
   */
  stop() {
    if (!this._running) return;

    this._running = false;

    if (this._updateTimer) {
      clearInterval(this._updateTimer);
      this._updateTimer = null;
    }

    this._emit('stopped', { timestamp: new Date().toISOString() });
    this._log('Dashboard stopped');
  }

  /**
   * Force an immediate metrics update
   *
   * @returns {Promise<Object>} Current metrics
   */
  async refresh() {
    await this._performMeasurement();
    return this.currentMetrics;
  }

  /**
   * Get current metrics
   *
   * @returns {Object|null} Current metrics snapshot
   */
  getCurrentMetrics() {
    return this.currentMetrics;
  }

  /**
   * Get metrics history
   *
   * @param {number} [limit=100] - Number of records to return
   * @returns {Array<Object>} Historical metrics
   */
  getHistory(limit = 100) {
    return this.timeSeries.slice(-limit);
  }

  /**
   * Get active alerts
   *
   * @returns {Array<Object>} Current alerts
   */
  getAlerts() {
    return [...this.activeAlerts];
  }

  /**
   * Set baseline for comparison
   *
   * @param {Object} [baseline] - Baseline metrics (uses current if not provided)
   */
  setBaseline(baseline = null) {
    this._baseline = baseline || { ...this.currentMetrics };
    this._log(`Baseline set: ${JSON.stringify(this._baseline?.summary || {})}`);
  }

  /**
   * Compare current metrics against baseline
   *
   * @returns {Object|null} Comparison result
   */
  compareToBaseline() {
    if (!this._baseline || !this.currentMetrics) {
      return null;
    }

    return {
      dimension: this._compareMetric(
        this._baseline.dimension?.systemDimension,
        this.currentMetrics.dimension?.systemDimension
      ),
      correlation: this._compareMetric(
        this._baseline.correlation?.totalCorrelation,
        this.currentMetrics.correlation?.totalCorrelation
      ),
      transferEntropy: this._compareMetric(
        this._baseline.transferEntropy?.totalTE,
        this.currentMetrics.transferEntropy?.totalTE
      ),
      capacity: this._compareMetric(
        this._baseline.capacity?.systemCapacity,
        this.currentMetrics.capacity?.systemCapacity
      ),
      baselineEpoch: this._baseline.epoch,
      currentEpoch: this.currentMetrics.epoch,
      comparedAt: new Date().toISOString()
    };
  }

  /**
   * Get metrics summary
   *
   * @returns {Object} Summary statistics
   */
  getSummary() {
    if (!this.currentMetrics) {
      return { status: 'no_data' };
    }

    const m = this.currentMetrics;

    return {
      epoch: m.epoch,
      status: m.health.status,
      healthScore: m.health.score,
      dimension: {
        value: m.dimension?.systemDimension?.toFixed(2) || 'N/A',
        trend: m.trends?.dimension || 'unknown'
      },
      correlation: {
        value: m.correlation?.totalCorrelation?.toFixed(3) || 'N/A',
        normalized: m.correlation?.normalizedTC?.toFixed(3) || 'N/A',
        trend: m.trends?.correlation || 'unknown'
      },
      transferEntropy: {
        totalTE: m.transferEntropy?.totalTE?.toFixed(3) || 'N/A',
        causalEdges: m.transferEntropy?.causalEdges?.length || 0,
        trend: m.trends?.transferEntropy || 'unknown'
      },
      capacity: {
        value: m.capacity?.systemCapacity?.toFixed(3) || 'N/A',
        admissionRate: ((m.capacity?.admissionRate || 0) * 100).toFixed(1) + '%',
        trend: m.trends?.capacity || 'unknown'
      },
      activeAlerts: this.activeAlerts.length,
      uptime: this._getUptime(),
      lastUpdate: m.timestamp
    };
  }

  /**
   * Register event listener
   *
   * @param {string} event - Event name (metrics, alert, started, stopped)
   * @param {Function} callback - Callback function
   */
  on(event, callback) {
    if (!this._listeners.has(event)) {
      this._listeners.set(event, []);
    }
    this._listeners.get(event).push(callback);
  }

  /**
   * Remove event listener
   *
   * @param {string} event - Event name
   * @param {Function} callback - Callback to remove
   */
  off(event, callback) {
    if (this._listeners.has(event)) {
      const listeners = this._listeners.get(event);
      const idx = listeners.indexOf(callback);
      if (idx >= 0) {
        listeners.splice(idx, 1);
      }
    }
  }

  /**
   * Format metrics for display
   *
   * @param {Object} [metrics] - Metrics to format (uses current if not provided)
   * @returns {string} Formatted output
   */
  formatMetrics(metrics = null) {
    const m = metrics || this.currentMetrics;
    if (!m) return 'No metrics available';

    const lines = [
      '',
      '='.repeat(60),
      `  SYSTEM HEALTH DASHBOARD - ${m.epoch}`,
      '='.repeat(60),
      '',
      `  Status: ${m.health.status.toUpperCase()} (Score: ${m.health.score}/100)`,
      '',
      '  DIMENSION (D_t)',
      `    System Dimension: ${m.dimension?.systemDimension?.toFixed(2) || 'N/A'} bits`,
      `    Utilization: ${((m.dimension?.utilizationRatio || 0) * 100).toFixed(1)}%`,
      `    Trend: ${m.trends?.dimension || 'unknown'}`,
      '',
      '  CORRELATION (TC)',
      `    Total Correlation: ${m.correlation?.totalCorrelation?.toFixed(3) || 'N/A'}`,
      `    Normalized TC: ${m.correlation?.normalizedTC?.toFixed(3) || 'N/A'}`,
      `    Trend: ${m.trends?.correlation || 'unknown'}`,
      '',
      '  TRANSFER ENTROPY (TE)',
      `    Total TE: ${m.transferEntropy?.totalTE?.toFixed(3) || 'N/A'}`,
      `    Causal Edges: ${m.transferEntropy?.causalEdges?.length || 0}`,
      `    Trend: ${m.trends?.transferEntropy || 'unknown'}`,
      '',
      '  CAPACITY (C_t)',
      `    System Capacity: ${m.capacity?.systemCapacity?.toFixed(3) || 'N/A'}`,
      `    Admission Rate: ${((m.capacity?.admissionRate || 0) * 100).toFixed(1)}%`,
      `    Throughput: ${m.capacity?.throughput?.deltasPerSecond?.toFixed(2) || 0} deltas/s`,
      `    Trend: ${m.trends?.capacity || 'unknown'}`,
      ''
    ];

    if (this.activeAlerts.length > 0) {
      lines.push('  ACTIVE ALERTS');
      for (const alert of this.activeAlerts) {
        lines.push(`    [${alert.severity.toUpperCase()}] ${alert.message}`);
      }
      lines.push('');
    }

    lines.push('='.repeat(60));
    lines.push('');

    return lines.join('\n');
  }

  /**
   * Export time series data
   *
   * @param {string} [format='json'] - Export format
   * @returns {string} Exported data
   */
  exportTimeSeries(format = 'json') {
    switch (format) {
      case 'json':
        return JSON.stringify(this.timeSeries, null, 2);

      case 'csv':
        return this._toCSV(this.timeSeries);

      case 'ndjson':
        return this.timeSeries.map(r => JSON.stringify(r)).join('\n');

      default:
        return JSON.stringify(this.timeSeries);
    }
  }

  /**
   * Get trend analysis for all metrics
   *
   * @param {number} [windowSize=10] - Analysis window
   * @returns {Object} Trend analysis
   */
  getTrends(windowSize = 10) {
    return {
      dimension: this.dimensionComputer.getDimensionTrend(windowSize),
      correlation: this.correlationComputer.getCorrelationTrend(windowSize),
      capacity: this.capacityComputer.getCapacityTrend(windowSize),
      overall: this._computeOverallTrend(windowSize)
    };
  }

  /**
   * Get forecasts for metrics
   *
   * @param {number} [horizonEpochs=10] - Forecast horizon
   * @returns {Object} Forecasts
   */
  getForecasts(horizonEpochs = 10) {
    return {
      capacity: this.capacityComputer.forecastCapacity(horizonEpochs),
      timestamp: new Date().toISOString()
    };
  }

  /**
   * Perform a measurement cycle
   * @private
   */
  async _performMeasurement() {
    const startTime = Date.now();
    const epoch = this._generateEpoch();

    try {
      // Compute all four metrics
      const dimension = await this._computeDimension();
      const correlation = await this._computeCorrelation();
      const transferEntropy = await this._computeTransferEntropy();
      const capacity = await this._computeCapacity();

      // Get trends
      const trends = {
        dimension: this.dimensionComputer.getDimensionTrend(10).trend,
        correlation: this.correlationComputer.getCorrelationTrend(10).trend,
        transferEntropy: 'stable', // TE trends require more history
        capacity: this.capacityComputer.getCapacityTrend(10).trend
      };

      // Generate certificate (which includes health evaluation)
      const certificate = await this.certificateGenerator.generateCertificate({
        dimension,
        correlation,
        transferEntropy,
        capacity
      });

      // Build current metrics
      this.currentMetrics = {
        epoch,
        dimension,
        correlation,
        transferEntropy: {
          ...transferEntropy,
          totalTE: transferEntropy.metrics?.totalTransferEntropy || 0,
          causalEdges: transferEntropy.edges || []
        },
        capacity,
        trends,
        health: certificate.health,
        certificate: certificate.certificateHash,
        timestamp: new Date().toISOString(),
        computeTimeMs: Date.now() - startTime
      };

      // Store in time series
      this._recordTimeSeries(this.currentMetrics);

      // Check alerts
      if (this.config.enableAlerts) {
        this._checkAlerts(this.currentMetrics);
      }

      // Emit metrics event
      this._emit('metrics', this.currentMetrics);

      // Output based on format
      this._outputMetrics();

    } catch (error) {
      this._log(`Measurement error: ${error.message}`, 'error');
      this._emit('error', { error: error.message, epoch, timestamp: new Date().toISOString() });
    }
  }

  /**
   * Compute dimension metrics
   * @private
   */
  async _computeDimension() {
    if (!this._universe) {
      return { systemDimension: 0, utilizationRatio: 0, partitionDimensions: [] };
    }

    try {
      return await this.dimensionComputer.computeSystemDimension(this._universe);
    } catch (error) {
      return { systemDimension: 0, utilizationRatio: 0, error: error.message };
    }
  }

  /**
   * Compute correlation metrics
   * @private
   */
  async _computeCorrelation() {
    // Record any new admission events from data source
    if (this._dataSource?.admissions) {
      for (const event of this._dataSource.admissions) {
        this.correlationComputer.recordAdmission(event);
      }
      this._dataSource.admissions = []; // Clear processed events
    }

    return this.correlationComputer.computeTotalCorrelation();
  }

  /**
   * Compute transfer entropy metrics
   * @private
   */
  async _computeTransferEntropy() {
    // Record any new state events from data source
    if (this._dataSource?.states) {
      for (const event of this._dataSource.states) {
        this.transferEntropyComputer.recordState(event);
      }
      this._dataSource.states = []; // Clear processed events
    }

    return this.transferEntropyComputer.computeCausalGraph();
  }

  /**
   * Compute capacity metrics
   * @private
   */
  async _computeCapacity() {
    // Record any new deltas from data source
    if (this._dataSource?.deltas) {
      for (const delta of this._dataSource.deltas) {
        this.capacityComputer.recordDelta(delta);
      }
      this._dataSource.deltas = []; // Clear processed events
    }

    return this.capacityComputer.computeSystemCapacity();
  }

  /**
   * Record metrics in time series
   * @private
   */
  _recordTimeSeries(metrics) {
    const record = {
      epoch: metrics.epoch,
      timestamp: metrics.timestamp,
      dimension: metrics.dimension?.systemDimension,
      correlation: metrics.correlation?.totalCorrelation,
      normalizedTC: metrics.correlation?.normalizedTC,
      transferEntropy: metrics.transferEntropy?.totalTE,
      capacity: metrics.capacity?.systemCapacity,
      admissionRate: metrics.capacity?.admissionRate,
      healthScore: metrics.health?.score,
      healthStatus: metrics.health?.status
    };

    this.timeSeries.push(record);

    // Trim history
    while (this.timeSeries.length > this.config.historySize) {
      this.timeSeries.shift();
    }
  }

  /**
   * Check for alert conditions
   * @private
   */
  _checkAlerts(metrics) {
    const newAlerts = [];
    const t = this.config.alertConfig;

    // Dimension alerts
    if (metrics.dimension?.systemDimension < t.dimension.minValue) {
      newAlerts.push({
        metric: 'dimension',
        type: 'low_value',
        severity: 'warning',
        message: `System dimension ${metrics.dimension.systemDimension.toFixed(2)} below threshold ${t.dimension.minValue}`,
        value: metrics.dimension.systemDimension,
        threshold: t.dimension.minValue,
        timestamp: new Date().toISOString()
      });
    }

    if ((metrics.dimension?.utilizationRatio || 0) * 100 > t.dimension.maxUtilization) {
      newAlerts.push({
        metric: 'dimension',
        type: 'high_utilization',
        severity: 'critical',
        message: `Dimension utilization ${(metrics.dimension.utilizationRatio * 100).toFixed(1)}% exceeds threshold`,
        value: metrics.dimension.utilizationRatio * 100,
        threshold: t.dimension.maxUtilization,
        timestamp: new Date().toISOString()
      });
    }

    // Correlation alerts
    if ((metrics.correlation?.normalizedTC || 0) > t.correlation.maxNormalizedTC) {
      newAlerts.push({
        metric: 'correlation',
        type: 'high_coupling',
        severity: 'warning',
        message: `Normalized TC ${metrics.correlation.normalizedTC.toFixed(3)} indicates high coupling`,
        value: metrics.correlation.normalizedTC,
        threshold: t.correlation.maxNormalizedTC,
        timestamp: new Date().toISOString()
      });
    }

    // Capacity alerts
    if ((metrics.capacity?.admissionRate || 1) < t.capacity.minAdmissionRate) {
      newAlerts.push({
        metric: 'capacity',
        type: 'low_admission',
        severity: 'warning',
        message: `Admission rate ${(metrics.capacity.admissionRate * 100).toFixed(1)}% below threshold`,
        value: metrics.capacity.admissionRate,
        threshold: t.capacity.minAdmissionRate,
        timestamp: new Date().toISOString()
      });
    }

    // Update active alerts
    this.activeAlerts = newAlerts;

    // Emit alert events
    for (const alert of newAlerts) {
      this._emit('alert', alert);
    }
  }

  /**
   * Output metrics based on format config
   * @private
   */
  _outputMetrics() {
    switch (this.config.outputFormat) {
      case 'console':
        console.log(this.formatMetrics());
        break;

      case 'json':
        console.log(JSON.stringify(this.currentMetrics, null, 2));
        break;

      case 'silent':
        // No output
        break;
    }
  }

  /**
   * Emit event to listeners
   * @private
   */
  _emit(event, data) {
    if (this._listeners.has(event)) {
      for (const callback of this._listeners.get(event)) {
        try {
          callback(data);
        } catch (error) {
          console.error(`Event listener error: ${error.message}`);
        }
      }
    }
  }

  /**
   * Log message
   * @private
   */
  _log(message, level = 'info') {
    if (this.config.outputFormat !== 'silent') {
      const prefix = level === 'error' ? '[ERROR]' : '[INFO]';
      console.log(`${prefix} Dashboard: ${message}`);
    }
  }

  /**
   * Generate epoch string
   * @private
   */
  _generateEpoch() {
    const now = new Date();
    const year = now.getUTCFullYear();
    const month = String(now.getUTCMonth() + 1).padStart(2, '0');
    const day = String(now.getUTCDate()).padStart(2, '0');
    const hour = String(now.getUTCHours()).padStart(2, '0');
    const minute = String(now.getUTCMinutes()).padStart(2, '0');
    const ms = String(now.getUTCMilliseconds()).padStart(3, '0');

    return `tau_${year}_${month}_${day}_${hour}${minute}_${ms}`;
  }

  /**
   * Compare two metric values
   * @private
   */
  _compareMetric(baseline, current) {
    if (baseline === undefined || current === undefined) {
      return { status: 'unknown' };
    }

    const delta = current - baseline;
    const percentChange = baseline !== 0 ? (delta / baseline) * 100 : 0;

    return {
      baseline,
      current,
      delta,
      percentChange,
      direction: delta > 0 ? 'increased' : (delta < 0 ? 'decreased' : 'stable')
    };
  }

  /**
   * Compute overall trend
   * @private
   */
  _computeOverallTrend(windowSize) {
    if (this.timeSeries.length < 2) {
      return 'insufficient_data';
    }

    const recent = this.timeSeries.slice(-windowSize);
    const scores = recent.map(r => r.healthScore || 0);

    const mean = scores.reduce((a, b) => a + b, 0) / scores.length;
    const first = scores[0];
    const last = scores[scores.length - 1];

    if (last > first + 5) return 'improving';
    if (last < first - 5) return 'degrading';
    return 'stable';
  }

  /**
   * Get uptime string
   * @private
   */
  _getUptime() {
    if (!this._startTime) return 'N/A';

    const uptimeMs = Date.now() - this._startTime;
    const seconds = Math.floor(uptimeMs / 1000);
    const minutes = Math.floor(seconds / 60);
    const hours = Math.floor(minutes / 60);

    if (hours > 0) {
      return `${hours}h ${minutes % 60}m`;
    }
    if (minutes > 0) {
      return `${minutes}m ${seconds % 60}s`;
    }
    return `${seconds}s`;
  }

  /**
   * Convert time series to CSV
   * @private
   */
  _toCSV(data) {
    if (data.length === 0) return '';

    const headers = Object.keys(data[0]);
    const lines = [headers.join(',')];

    for (const row of data) {
      const values = headers.map(h => {
        const v = row[h];
        if (v === null || v === undefined) return '';
        if (typeof v === 'string' && v.includes(',')) return `"${v}"`;
        return v;
      });
      lines.push(values.join(','));
    }

    return lines.join('\n');
  }

  /**
   * Feed admission event to dashboard
   *
   * @param {Object} event - Admission event
   */
  feedAdmission(event) {
    this.correlationComputer.recordAdmission(event);
    this.capacityComputer.recordDelta({
      ...event,
      quadCount: event.quadCount || 0
    });
  }

  /**
   * Feed state event to dashboard
   *
   * @param {Object} event - State event
   */
  feedState(event) {
    this.transferEntropyComputer.recordState(event);
  }

  /**
   * Clear all data
   */
  clear() {
    this.timeSeries = [];
    this.currentMetrics = null;
    this.activeAlerts = [];
    this._baseline = null;

    this.dimensionComputer.clearCache();
    this.dimensionComputer.clearHistory();
    this.correlationComputer.clear();
    this.transferEntropyComputer.clear();
    this.capacityComputer.clear();
    this.certificateGenerator.clearHistory();
  }

  /**
   * Export dashboard state
   *
   * @returns {Object} Exportable state
   */
  exportState() {
    return {
      timeSeries: [...this.timeSeries],
      currentMetrics: this.currentMetrics,
      activeAlerts: [...this.activeAlerts],
      baseline: this._baseline,
      config: { ...this.config },
      computers: {
        dimension: this.dimensionComputer.exportData(),
        correlation: this.correlationComputer.exportData(),
        transferEntropy: this.transferEntropyComputer.exportData(),
        capacity: this.capacityComputer.exportData(),
        certificates: this.certificateGenerator.exportState()
      },
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Import dashboard state
   *
   * @param {Object} state - Previously exported state
   */
  importState(state) {
    if (state.timeSeries) {
      this.timeSeries = [...state.timeSeries];
    }
    if (state.currentMetrics) {
      this.currentMetrics = state.currentMetrics;
    }
    if (state.activeAlerts) {
      this.activeAlerts = [...state.activeAlerts];
    }
    if (state.baseline) {
      this._baseline = state.baseline;
    }
    if (state.computers) {
      if (state.computers.dimension) {
        this.dimensionComputer.importHistory(state.computers.dimension.history);
      }
      if (state.computers.correlation) {
        this.correlationComputer.importData(state.computers.correlation);
      }
      if (state.computers.transferEntropy) {
        this.transferEntropyComputer.importData(state.computers.transferEntropy);
      }
      if (state.computers.capacity) {
        this.capacityComputer.importData(state.computers.capacity);
      }
      if (state.computers.certificates) {
        this.certificateGenerator.importState(state.computers.certificates);
      }
    }
  }
}

/**
 * Create a health dashboard with default configuration
 * @param {Object} [config] - Dashboard configuration
 * @returns {HealthDashboard} New dashboard
 */
export function createHealthDashboard(config = {}) {
  return new HealthDashboard(config);
}

/**
 * Start a dashboard and return it
 * @param {Object} universe - Universe to monitor
 * @param {Object} [config] - Dashboard configuration
 * @returns {HealthDashboard} Running dashboard
 */
export function startDashboard(universe, config = {}) {
  const dashboard = new HealthDashboard(config);
  dashboard.start(universe);
  return dashboard;
}

export default HealthDashboard;
