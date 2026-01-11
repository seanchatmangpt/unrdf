/**
 * @file Daemon Benchmark Suite
 * @module @unrdf/daemon/benchmarks/suite
 * @description Core benchmarking infrastructure with regression detection, trend analysis, and CI/CD integration
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * Performance baseline thresholds and targets
 * @type {Object}
 */
const REGRESSION_THRESHOLDS = {
  latency: 1.15, // 15% regression threshold
  memory: 1.25, // 25% regression threshold
  throughput: 0.85, // 15% regression threshold (lower is worse)
  variance: 0.05, // 5% max variance
};

/**
 * Stores benchmark result
 * @param {Object} result - Benchmark result
 * @param {string} result.name - Benchmark name
 * @param {number} result.value - Benchmark value
 * @param {string} result.unit - Unit (ms, bytes, ops/sec)
 * @param {number} result.variance - Variance percentage
 * @returns {Object} Stored result with timestamp
 */
export function storeBenchmarkResult(result) {
  const stored = {
    ...result,
    timestamp: new Date().toISOString(),
    nodeVersion: process.version,
    platform: process.platform,
  };

  return stored;
}

/**
 * Compares current benchmark result to baseline
 * @param {Object} current - Current benchmark result
 * @param {Object} baseline - Baseline benchmark result
 * @param {string} type - Benchmark type (latency, memory, throughput)
 * @returns {Object} Regression analysis
 */
export function detectRegression(current, baseline, type) {
  if (!baseline) {
    return {
      isRegression: false,
      reason: 'no-baseline',
      severity: 'none',
    };
  }

  const ratio = current.value / baseline.value;
  const threshold = REGRESSION_THRESHOLDS[type] || 1.2;
  const isRegression = type === 'throughput' ? ratio < threshold : ratio > threshold;

  const percentChange = ((ratio - 1) * 100).toFixed(2);
  const severity = isRegression
    ? Math.abs(ratio - 1) > 0.3 ? 'critical' : 'warning'
    : 'none';

  return {
    isRegression,
    ratio: ratio.toFixed(4),
    percentChange: `${percentChange}%`,
    threshold: threshold.toFixed(4),
    severity,
    baselineValue: baseline.value,
    currentValue: current.value,
  };
}

/**
 * Analyzes variance in benchmark runs
 * @param {Array<number>} values - Array of benchmark values
 * @returns {Object} Variance analysis
 */
export function analyzeVariance(values) {
  if (values.length < 2) {
    return {
      mean: values[0] || 0,
      stdDev: 0,
      variance: 0,
      coefficientOfVariation: 0,
      min: values[0] || 0,
      max: values[0] || 0,
    };
  }

  const mean = values.reduce((a, b) => a + b, 0) / values.length;
  const squaredDiffs = values.map(v => Math.pow(v - mean, 2));
  const variance = squaredDiffs.reduce((a, b) => a + b, 0) / values.length;
  const stdDev = Math.sqrt(variance);
  const coefficientOfVariation = mean > 0 ? (stdDev / mean) * 100 : 0;

  return {
    mean: mean.toFixed(4),
    stdDev: stdDev.toFixed(4),
    variance: variance.toFixed(4),
    coefficientOfVariation: coefficientOfVariation.toFixed(2),
    min: Math.min(...values).toFixed(4),
    max: Math.max(...values).toFixed(4),
  };
}

/**
 * Generates performance trend report
 * @param {Array<Object>} historicalResults - Previous benchmark results
 * @param {Object} currentResult - Current benchmark result
 * @returns {Object} Trend analysis
 */
export function analyzeTrend(historicalResults, currentResult) {
  if (historicalResults.length === 0) {
    return {
      trend: 'no-history',
      direction: 'neutral',
      percentChangeOverTime: 0,
    };
  }

  const oldest = historicalResults[0];
  const percentChange = ((currentResult.value - oldest.value) / oldest.value) * 100;
  const direction = percentChange < -5 ? 'improving' : percentChange > 5 ? 'degrading' : 'stable';

  return {
    trend: direction,
    direction,
    percentChangeOverTime: percentChange.toFixed(2),
    oldestRecord: oldest.timestamp,
    newestRecord: currentResult.timestamp,
    recordCount: historicalResults.length + 1,
  };
}

/**
 * Generates comprehensive JSON report
 * @param {Object} benchmarkResults - Map of benchmark results
 * @param {Object} baselineData - Baseline reference data
 * @returns {Object} Formatted JSON report
 */
export function generateReport(benchmarkResults, baselineData = {}) {
  const regressions = [];
  const results = {};

  for (const [name, benchmark] of Object.entries(benchmarkResults)) {
    const baseline = baselineData[name];
    const regression = detectRegression(
      benchmark,
      baseline,
      benchmark.type || 'latency'
    );

    results[name] = {
      benchmark,
      baseline,
      regression,
    };

    if (regression.isRegression) {
      regressions.push({
        name,
        severity: regression.severity,
        ...regression,
      });
    }
  }

  const totalRegressions = regressions.length;
  const criticalRegressions = regressions.filter(r => r.severity === 'critical').length;
  const warningRegressions = regressions.filter(r => r.severity === 'warning').length;

  return {
    timestamp: new Date().toISOString(),
    summary: {
      totalBenchmarks: Object.keys(benchmarkResults).length,
      totalRegressions,
      criticalRegressions,
      warningRegressions,
      passRate: ((Object.keys(benchmarkResults).length - totalRegressions) /
        Object.keys(benchmarkResults).length * 100).toFixed(2),
    },
    regressions,
    results,
  };
}

/**
 * Loads baseline data from file
 * @param {string} [baselinePath] - Path to baseline JSON
 * @returns {Object} Baseline data or empty object
 */
export function loadBaseline(baselinePath) {
  const defaultPath = path.join(__dirname, 'baselines', 'baseline.json');
  const filePath = baselinePath || defaultPath;

  try {
    if (fs.existsSync(filePath)) {
      const content = fs.readFileSync(filePath, 'utf-8');
      return JSON.parse(content);
    }
  } catch (error) {
    console.warn(`Failed to load baseline from ${filePath}:`, error.message);
  }

  return {};
}

/**
 * Saves baseline data to file
 * @param {Object} data - Baseline data to save
 * @param {string} [baselinePath] - Path to save baseline
 * @returns {boolean} Success status
 */
export function saveBaseline(data, baselinePath) {
  const defaultPath = path.join(__dirname, 'baselines', 'baseline.json');
  const filePath = baselinePath || defaultPath;

  try {
    const dir = path.dirname(filePath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }

    fs.writeFileSync(filePath, JSON.stringify(data, null, 2), 'utf-8');
    return true;
  } catch (error) {
    console.error(`Failed to save baseline to ${filePath}:`, error.message);
    return false;
  }
}

/**
 * Saves report to file
 * @param {Object} report - Report data
 * @param {string} [reportPath] - Path to save report
 * @returns {boolean} Success status
 */
export function saveReport(report, reportPath) {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);
  const defaultPath = path.join(__dirname, `benchmarks-${timestamp}.json`);
  const filePath = reportPath || defaultPath;

  try {
    const dir = path.dirname(filePath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }

    fs.writeFileSync(filePath, JSON.stringify(report, null, 2), 'utf-8');
    return true;
  } catch (error) {
    console.error(`Failed to save report to ${filePath}:`, error.message);
    return false;
  }
}

/**
 * Formats benchmark results for console output
 * @param {Object} report - Report data
 * @returns {string} Formatted output
 */
export function formatReportForConsole(report) {
  const lines = [];

  lines.push('\n═══════════════════════════════════════════════════════════════');
  lines.push('         DAEMON PERFORMANCE BENCHMARK REPORT');
  lines.push('═══════════════════════════════════════════════════════════════\n');

  lines.push(`Timestamp: ${report.timestamp}`);
  lines.push(`Total Benchmarks: ${report.summary.totalBenchmarks}`);
  lines.push(`Pass Rate: ${report.summary.passRate}%`);
  lines.push(`Regressions: ${report.summary.totalRegressions}`);
  lines.push(`  - Critical: ${report.summary.criticalRegressions}`);
  lines.push(`  - Warnings: ${report.summary.warningRegressions}\n`);

  if (report.regressions.length > 0) {
    lines.push('REGRESSIONS DETECTED:');
    lines.push('───────────────────────────────────────────────────────────────');
    for (const regression of report.regressions) {
      lines.push(`  ${regression.name} (${regression.severity.toUpperCase()})`);
      lines.push(`    Change: ${regression.percentChange}`);
      lines.push(`    Baseline: ${regression.baselineValue}`);
      lines.push(`    Current: ${regression.currentValue}\n`);
    }
  } else {
    lines.push('✓ No regressions detected\n');
  }

  lines.push('═══════════════════════════════════════════════════════════════\n');

  return lines.join('\n');
}

/**
 * Combines multiple benchmark runs for statistical analysis
 * @param {Array<Array<number>>} runs - Array of benchmark runs
 * @returns {Object} Combined statistics
 */
export function combineBenchmarkRuns(runs) {
  if (runs.length === 0) {
    return { values: [], stats: {} };
  }

  const flattened = runs.flat();
  const stats = analyzeVariance(flattened);

  return {
    values: flattened,
    stats,
    runCount: runs.length,
    samplesPerRun: runs[0].length,
  };
}
