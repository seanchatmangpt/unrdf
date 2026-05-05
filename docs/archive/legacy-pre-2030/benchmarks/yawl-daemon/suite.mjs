/**
 * @file YAWL Daemon Benchmark Suite Utilities
 * @module benchmarks/yawl-daemon/suite
 * @description Utilities for benchmark execution, reporting, and regression detection
 */

import { writeFileSync, readFileSync, existsSync, mkdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const BASELINE_PATH = join(__dirname, 'baselines', 'baseline.json');
const RESULTS_DIR = __dirname;

/**
 * Calculate variance statistics
 * @param {number[]} values - Array of numeric values
 * @returns {Object} Variance statistics
 */
export function analyzeVariance(values) {
  if (values.length === 0) {
    return { mean: 0, min: 0, max: 0, stdDev: 0, coefficientOfVariation: 0 };
  }

  const mean = values.reduce((sum, v) => sum + v, 0) / values.length;
  const min = Math.min(...values);
  const max = Math.max(...values);

  const squaredDiffs = values.map(v => Math.pow(v - mean, 2));
  const variance = squaredDiffs.reduce((sum, v) => sum + v, 0) / values.length;
  const stdDev = Math.sqrt(variance);
  const coefficientOfVariation = mean !== 0 ? (stdDev / mean) * 100 : 0;

  return {
    mean,
    min,
    max,
    stdDev,
    coefficientOfVariation,
  };
}

/**
 * Store benchmark result
 * @param {Object} result - Benchmark result
 * @returns {Object} Stored result
 */
export function storeBenchmarkResult(result) {
  return {
    timestamp: new Date().toISOString(),
    ...result,
  };
}

/**
 * Load baseline results
 * @returns {Object} Baseline results
 */
export function loadBaseline() {
  try {
    if (existsSync(BASELINE_PATH)) {
      const data = readFileSync(BASELINE_PATH, 'utf8');
      return JSON.parse(data);
    }
  } catch (error) {
    console.warn('No baseline found or error loading:', error.message);
  }
  return {};
}

/**
 * Save baseline results
 * @param {Object} baseline - Baseline results to save
 * @returns {boolean} Success status
 */
export function saveBaseline(baseline) {
  try {
    const baselinesDir = join(__dirname, 'baselines');
    if (!existsSync(baselinesDir)) {
      mkdirSync(baselinesDir, { recursive: true });
    }

    writeFileSync(BASELINE_PATH, JSON.stringify(baseline, null, 2));
    return true;
  } catch (error) {
    console.error('Failed to save baseline:', error.message);
    return false;
  }
}

/**
 * Save benchmark report
 * @param {Object} report - Benchmark report
 * @returns {boolean} Success status
 */
export function saveReport(report) {
  try {
    const timestamp = new Date().toISOString().replace(/:/g, '-').split('.')[0];
    const filename = `yawl-daemon-benchmarks-${timestamp}.json`;
    const filepath = join(RESULTS_DIR, filename);

    writeFileSync(filepath, JSON.stringify(report, null, 2));
    console.log(`Report saved: ${filename}`);
    return true;
  } catch (error) {
    console.error('Failed to save report:', error.message);
    return false;
  }
}

/**
 * Compare result with baseline
 * @param {Object} current - Current result
 * @param {Object} baseline - Baseline result
 * @param {number} threshold - Regression threshold (percentage)
 * @returns {Object} Comparison result
 */
export function compareWithBaseline(current, baseline, threshold = 20) {
  if (!baseline || !current) {
    return { regression: false, improvement: false, change: 0 };
  }

  const currentValue = typeof current === 'object' ? (current.mean || current.value || current.p95) : current;
  const baselineValue = typeof baseline === 'object' ? (baseline.mean || baseline.value || baseline.p95) : baseline;

  if (!currentValue || !baselineValue) {
    return { regression: false, improvement: false, change: 0 };
  }

  const change = ((currentValue - baselineValue) / baselineValue) * 100;
  const regression = change > threshold;
  const improvement = change < -threshold;

  return {
    regression,
    improvement,
    change,
    current: currentValue,
    baseline: baselineValue,
  };
}

/**
 * Generate comprehensive benchmark report
 * @param {Object} results - All benchmark results
 * @param {Object} baseline - Baseline results
 * @returns {Object} Formatted report
 */
export function generateReport(results, baseline = {}) {
  const report = {
    timestamp: new Date().toISOString(),
    summary: {
      totalBenchmarks: 0,
      passed: 0,
      failed: 0,
      totalRegressions: 0,
      totalImprovements: 0,
    },
    benchmarks: {},
    regressions: [],
    improvements: [],
  };

  for (const [suiteName, suiteResults] of Object.entries(results)) {
    if (!suiteResults || !suiteResults.results) continue;

    for (const [benchName, benchResult] of Object.entries(suiteResults.results)) {
      const fullName = `${suiteName}.${benchName}`;
      report.summary.totalBenchmarks++;

      if (benchResult.passed) {
        report.summary.passed++;
      } else {
        report.summary.failed++;
      }

      // Compare with baseline
      const baselineBench = baseline[fullName];
      let comparison = null;

      if (baselineBench && benchResult.latency) {
        comparison = compareWithBaseline(
          benchResult.latency,
          baselineBench.latency
        );

        if (comparison.regression) {
          report.summary.totalRegressions++;
          report.regressions.push({
            name: fullName,
            change: comparison.change,
            current: comparison.current,
            baseline: comparison.baseline,
          });
        }

        if (comparison.improvement) {
          report.summary.totalImprovements++;
          report.improvements.push({
            name: fullName,
            change: comparison.change,
            current: comparison.current,
            baseline: comparison.baseline,
          });
        }
      }

      report.benchmarks[fullName] = {
        ...benchResult,
        comparison,
      };
    }
  }

  return report;
}

/**
 * Format report for console output
 * @param {Object} report - Benchmark report
 * @returns {string} Formatted output
 */
export function formatReportForConsole(report) {
  let output = '\n';
  output += '═══════════════════════════════════════════════════════════════\n';
  output += '              YAWL DAEMON PERFORMANCE REPORT\n';
  output += '═══════════════════════════════════════════════════════════════\n\n';

  output += `Timestamp: ${report.timestamp}\n\n`;

  // Summary
  output += '── Summary ──────────────────────────────────────────────────\n';
  output += `Total Benchmarks:  ${report.summary.totalBenchmarks}\n`;
  output += `Passed:            ${report.summary.passed} ✓\n`;
  output += `Failed:            ${report.summary.failed} ${report.summary.failed > 0 ? '✗' : ''}\n`;
  output += `Regressions:       ${report.summary.totalRegressions} ${report.summary.totalRegressions > 0 ? '⚠' : ''}\n`;
  output += `Improvements:      ${report.summary.totalImprovements} ${report.summary.totalImprovements > 0 ? '↑' : ''}\n\n`;

  // Benchmark details
  output += '── Benchmark Results ────────────────────────────────────────\n\n';

  for (const [name, result] of Object.entries(report.benchmarks)) {
    const status = result.passed ? '✓' : '✗';
    output += `${status} ${name}\n`;

    if (result.latency) {
      const lat = result.latency;
      output += `  Latency: P50=${lat.p50?.toFixed(2)}ms, P95=${lat.p95?.toFixed(2)}ms, P99=${lat.p99?.toFixed(2)}ms\n`;
    }

    if (result.throughput) {
      const tp = result.throughput;
      output += `  Throughput: ${tp.mean?.toFixed(2)} ${result.unit || 'ops/s'}\n`;
    }

    if (result.memory) {
      const mem = result.memory;
      if (mem.avgFootprintMB) {
        output += `  Memory: ${mem.avgFootprintMB.toFixed(2)}MB baseline\n`;
      }
      if (mem.growthRateMBPerWorkflow) {
        output += `  Growth: ${(mem.growthRateMBPerWorkflow * 1000).toFixed(3)}KB/workflow\n`;
      }
    }

    if (result.comparison && result.comparison.regression) {
      output += `  ⚠ REGRESSION: +${result.comparison.change.toFixed(1)}% vs baseline\n`;
    } else if (result.comparison && result.comparison.improvement) {
      output += `  ↑ IMPROVEMENT: ${result.comparison.change.toFixed(1)}% vs baseline\n`;
    }

    output += `  Target: ${result.target}\n\n`;
  }

  // Regressions
  if (report.regressions.length > 0) {
    output += '── Regressions ──────────────────────────────────────────────\n';
    for (const reg of report.regressions) {
      output += `⚠ ${reg.name}: +${reg.change.toFixed(1)}% (${reg.current.toFixed(2)} vs ${reg.baseline.toFixed(2)})\n`;
    }
    output += '\n';
  }

  // Improvements
  if (report.improvements.length > 0) {
    output += '── Improvements ─────────────────────────────────────────────\n';
    for (const imp of report.improvements) {
      output += `↑ ${imp.name}: ${imp.change.toFixed(1)}% (${imp.current.toFixed(2)} vs ${imp.baseline.toFixed(2)})\n`;
    }
    output += '\n';
  }

  output += '═══════════════════════════════════════════════════════════════\n';

  return output;
}
