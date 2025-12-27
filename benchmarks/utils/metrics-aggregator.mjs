/**
 * Metrics Aggregator for Benchmark Results
 *
 * Collects, aggregates, and validates benchmark results against baselines.
 * Detects performance regressions and generates reports.
 */

import { readFileSync, writeFileSync } from 'fs';

export class MetricsAggregator {
  /**
   * @param {Object} [baselineTargets=null] - Baseline targets object
   */
  constructor(baselineTargets = null) {
    this.baselineTargets = baselineTargets;
    this.results = {};
  }

  /**
   * Record a benchmark result
   * @param {string} benchmarkName - Name of the benchmark
   * @param {string} scenario - Scenario name (e.g., "simple", "complex")
   * @param {string} metric - Metric name (e.g., "latency", "throughput")
   * @param {number} value - Metric value
   * @param {string} [unit=''] - Unit of measurement
   */
  recordResult(benchmarkName, scenario, metric, value, unit = '') {
    if (!this.results[benchmarkName]) {
      this.results[benchmarkName] = {};
    }

    if (!this.results[benchmarkName][scenario]) {
      this.results[benchmarkName][scenario] = {};
    }

    this.results[benchmarkName][scenario][metric] = {
      value,
      unit,
      timestamp: Date.now()
    };
  }

  /**
   * Get all aggregated results
   * @returns {Object} Complete results object
   */
  getResults() {
    return this.results;
  }

  /**
   * Compare results against baseline targets and detect regressions
   * @returns {{ regressions: Array, warnings: Array, passed: Array }}
   */
  checkRegressions() {
    if (!this.baselineTargets || !this.baselineTargets.benchmarks) {
      return {
        regressions: [],
        warnings: [],
        passed: []
      };
    }

    const regressions = [];
    const warnings = [];
    const passed = [];

    const thresholds = this.baselineTargets.regressionThresholds || {
      critical: { latency: 20, throughput: 20, memory: 30 },
      warning: { latency: 10, throughput: 10, memory: 15 }
    };

    for (const [benchmarkName, scenarios] of Object.entries(this.results)) {
      const baseline = this.baselineTargets.benchmarks[benchmarkName];

      if (!baseline) {
        continue;
      }

      for (const [scenario, metrics] of Object.entries(scenarios)) {
        for (const [metricName, metricData] of Object.entries(metrics)) {
          const baselineMetric = baseline[metricName];

          if (!baselineMetric || !baselineMetric.target) {
            continue;
          }

          const actual = metricData.value;
          const target = baselineMetric.target;
          const metricType = this.getMetricType(metricName);

          // Calculate deviation
          const deviation = this.calculateDeviation(actual, target, metricType);
          const deviationPercent = Math.abs(deviation);

          const result = {
            benchmark: benchmarkName,
            scenario,
            metric: metricName,
            actual,
            target,
            deviation,
            deviationPercent,
            unit: metricData.unit || baselineMetric.unit,
            priority: baselineMetric.priority || 'P2'
          };

          // Determine if this is a regression
          const threshold = thresholds.critical[metricType] || 20;
          const warningThreshold = thresholds.warning[metricType] || 10;

          if (deviationPercent > threshold) {
            regressions.push(result);
          } else if (deviationPercent > warningThreshold) {
            warnings.push(result);
          } else {
            passed.push(result);
          }
        }
      }
    }

    return { regressions, warnings, passed };
  }

  /**
   * Get metric type (latency, throughput, or memory)
   * @param {string} metricName - Name of the metric
   * @returns {string} Metric type
   */
  getMetricType(metricName) {
    const name = metricName.toLowerCase();

    if (name.includes('latency') || name.includes('duration') || name.includes('time')) {
      return 'latency';
    }

    if (name.includes('throughput') || name.includes('ops') || name.includes('rate')) {
      return 'throughput';
    }

    if (name.includes('memory') || name.includes('heap') || name.includes('footprint')) {
      return 'memory';
    }

    return 'other';
  }

  /**
   * Calculate deviation percentage
   * @param {number} actual - Actual value
   * @param {number} target - Target value
   * @param {string} metricType - Type of metric
   * @returns {number} Deviation percentage (positive = regression)
   */
  calculateDeviation(actual, target, metricType) {
    if (target === 0) {
      return 0;
    }

    const ratio = (actual - target) / target * 100;

    // For latency and memory, higher is worse
    if (metricType === 'latency' || metricType === 'memory') {
      return ratio; // Positive = regression (slower/more memory)
    }

    // For throughput, lower is worse
    if (metricType === 'throughput') {
      return -ratio; // Negative actual value = regression (less throughput)
    }

    return ratio;
  }

  /**
   * Export results as JSON file
   * @param {string} filepath - Path to save JSON file
   */
  exportJSON(filepath) {
    const output = {
      timestamp: Date.now(),
      results: this.results,
      regressions: this.checkRegressions()
    };

    writeFileSync(filepath, JSON.stringify(output, null, 2), 'utf-8');
  }

  /**
   * Load baseline targets from JSON file
   * @param {string} filepath - Path to baseline JSON file
   * @returns {MetricsAggregator} New aggregator with loaded baselines
   */
  static loadBaseline(filepath) {
    const data = JSON.parse(readFileSync(filepath, 'utf-8'));
    return new MetricsAggregator(data);
  }

  /**
   * Generate human-readable report
   * @returns {string} Formatted report
   */
  generateReport() {
    const { regressions, warnings, passed } = this.checkRegressions();

    let report = '=== Benchmark Results Report ===\n\n';

    if (regressions.length > 0) {
      report += '❌ REGRESSIONS DETECTED:\n';
      for (const r of regressions) {
        report += `  ${r.benchmark}/${r.scenario}/${r.metric}: ${r.actual}${r.unit} (target: ${r.target}${r.unit}, deviation: ${r.deviationPercent.toFixed(1)}%)\n`;
      }
      report += '\n';
    }

    if (warnings.length > 0) {
      report += '⚠️  WARNINGS:\n';
      for (const w of warnings) {
        report += `  ${w.benchmark}/${w.scenario}/${w.metric}: ${w.actual}${w.unit} (target: ${w.target}${w.unit}, deviation: ${w.deviationPercent.toFixed(1)}%)\n`;
      }
      report += '\n';
    }

    if (passed.length > 0) {
      report += `✅ PASSED: ${passed.length} metrics within target\n\n`;
    }

    return report;
  }
}
