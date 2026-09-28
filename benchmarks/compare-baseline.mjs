#!/usr/bin/env node
/**
 * Performance Regression Detector
 * Compares current benchmark results against v6.0.0 baseline
 *
 * Usage:
 *   node benchmarks/compare-baseline.mjs v6.0.0 [results.json]
 *
 * Exit codes:
 *   0: All metrics within SLA
 *   1: Regression detected (>10% threshold)
 *   2: Invalid baseline or results
 *
 * @module benchmarks/compare-baseline
 */

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// ANSI colors
const c = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

const log = (msg, color = 'reset') => console.log(`${c[color]}${msg}${c.reset}`);
const fmt = (num) => typeof num === 'number' ? num.toFixed(2) : num;

/**
 * Load baseline configuration
 * @param {string} version - Baseline version (e.g., 'v6.0.0')
 * @returns {Object} Baseline configuration
 */
function loadBaseline(version) {
  const baselinePath = path.join(__dirname, `${version}-baseline.json`);

  if (!fs.existsSync(baselinePath)) {
    throw new Error(`Baseline not found: ${baselinePath}`);
  }

  const content = fs.readFileSync(baselinePath, 'utf-8');
  return JSON.parse(content);
}

/**
 * Load current benchmark results
 * @param {string} resultsPath - Path to results JSON
 * @returns {Object} Current benchmark results
 */
function loadResults(resultsPath) {
  if (!fs.existsSync(resultsPath)) {
    throw new Error(`Results file not found: ${resultsPath}`);
  }

  const content = fs.readFileSync(resultsPath, 'utf-8');
  return JSON.parse(content);
}

/**
 * Compare metric against baseline
 * @param {string} metricName - Metric name
 * @param {Object} baseline - Baseline metric definition
 * @param {number} current - Current metric value
 * @param {Object} threshold - Regression threshold
 * @returns {Object} Comparison result
 */
function compareMetric(metricName, baseline, current, threshold) {
  const baselineValue = baseline.value;
  const tolerancePercent = baseline.sla_tolerance || threshold.default || 10;

  // Calculate acceptable range
  let acceptable = false;
  let percentChange = 0;
  let status = 'PASS';

  // For metrics where LOWER is better (latency, time, memory, creation time)
  if (metricName.includes('latency') ||
      metricName.includes('time') ||
      metricName.includes('memory') ||
      metricName.includes('error') ||
      metricName.includes('cold_start') ||
      metricName.includes('creation')) {
    percentChange = Math.abs(((current - baselineValue) / baselineValue) * 100);
    acceptable = percentChange <= tolerancePercent;
    status = acceptable ? 'PASS' : 'FAIL';
  }
  // For metrics where HIGHER is better (throughput, ops/sec)
  else if (metricName.includes('throughput') ||
           metricName.includes('ops') ||
           metricName.includes('rate') ||
           metricName.includes('morphism')) {
    // For morphism metrics: lower p95 is better (time metric)
    percentChange = Math.abs(((current - baselineValue) / baselineValue) * 100);
    acceptable = percentChange <= tolerancePercent;
    status = acceptable ? 'PASS' : 'FAIL';
  }

  return {
    metric: metricName,
    baseline: baselineValue,
    current: current,
    unit: baseline.unit,
    percentChange: parseFloat(percentChange.toFixed(2)),
    tolerance: tolerancePercent,
    status: status,
    acceptable: acceptable,
    description: baseline.description
  };
}

/**
 * Format comparison result for display
 * @param {Object} result - Comparison result
 * @returns {string} Formatted display string
 */
function formatResult(result) {
  const icon = result.acceptable ? '✅' : '❌';
  const changePercent = parseFloat(result.percentChange);
  const change = changePercent >= 0 ? '+' : '';
  const color = result.acceptable ? 'green' : 'red';

  return {
    icon,
    color,
    text: `${icon} ${result.metric}: ${fmt(result.current)}${result.unit} ` +
          `(baseline: ${fmt(result.baseline)}${result.unit}, ` +
          `change: ${change}${changePercent.toFixed(2)}%, ` +
          `tolerance: ±${fmt(result.tolerance)}%)`
  };
}

/**
 * Detect regressions in results
 * @param {Object} baseline - Baseline configuration
 * @param {Object} results - Current benchmark results
 * @returns {Object} Regression analysis
 */
function detectRegressions(baseline, results) {
  const analysis = {
    timestamp: new Date().toISOString(),
    baseline_version: baseline.version,
    baseline_timestamp: baseline.timestamp,
    comparison_timestamp: new Date().toISOString(),
    metrics_compared: 0,
    passed: 0,
    failed: 0,
    regressions: [],
    details: []
  };

  // Map current results to baseline metrics
  const currentMetrics = {};

  // Extract metrics from various result formats
  if (results.metrics) {
    Object.assign(currentMetrics, results.metrics);
  }
  if (results.benchmarks) {
    Object.assign(currentMetrics, results.benchmarks);
  }

  // Compare each baseline metric
  for (const [metricName, metricDef] of Object.entries(baseline.metrics)) {
    const currentValue = currentMetrics[metricName]?.value ||
                        results[metricName]?.value ||
                        null;

    if (currentValue === null) {
      analysis.details.push({
        metric: metricName,
        status: 'SKIPPED',
        reason: 'No current measurement available'
      });
      continue;
    }

    analysis.metrics_compared++;

    const thresholds = baseline.regression_thresholds;
    let tolerancePercent = 10; // default

    if (metricName.includes('universe_creation')) {
      tolerancePercent = thresholds.performance_regression_percent;
    } else if (metricName.includes('memory')) {
      tolerancePercent = thresholds.memory_regression_percent;
    } else if (metricName.includes('throughput')) {
      tolerancePercent = thresholds.throughput_regression_percent;
    } else if (metricName.includes('error')) {
      tolerancePercent = thresholds.error_rate_increase_percent;
    } else if (metricName.includes('latency')) {
      tolerancePercent = thresholds.latency_p95_regression_percent;
    }

    // Add tolerance to baseline for comparison
    const metricWithTolerance = {
      ...metricDef,
      sla_tolerance: tolerancePercent
    };

    const comparison = compareMetric(metricName, metricWithTolerance, currentValue, {
      default: tolerancePercent
    });

    analysis.details.push(comparison);

    if (comparison.acceptable) {
      analysis.passed++;
    } else {
      analysis.failed++;
      analysis.regressions.push({
        metric: metricName,
        baseline: comparison.baseline,
        current: comparison.current,
        regression_percent: parseFloat(comparison.percentChange.toFixed(2)),
        tolerance_percent: comparison.tolerance,
        unit: comparison.unit,
        severity: Math.abs(comparison.percentChange) > comparison.tolerance * 2 ? 'CRITICAL' : 'HIGH'
      });
    }
  }

  analysis.pass_rate = analysis.metrics_compared > 0 ?
    (analysis.passed / analysis.metrics_compared * 100).toFixed(1) + '%' : 'N/A';

  analysis.overall_status = analysis.failed === 0 ? 'PASS' : 'FAIL';

  return analysis;
}

/**
 * Generate regression report
 * @param {Object} analysis - Regression analysis results
 */
function generateReport(analysis) {
  log('\n' + '='.repeat(80), 'bold');
  log('📊 PERFORMANCE REGRESSION ANALYSIS', 'bold');
  log('='.repeat(80), 'bold');

  log(`\nBaseline Version: ${analysis.baseline_version}`, 'cyan');
  log(`Baseline Timestamp: ${analysis.baseline_timestamp}`, 'cyan');
  log(`Comparison Time: ${analysis.comparison_timestamp}`, 'cyan');

  log(`\nMetrics Compared: ${analysis.metrics_compared}`, 'cyan');
  log(`Passed: ${analysis.passed}`, 'green');
  log(`Failed: ${analysis.failed}`, analysis.failed > 0 ? 'red' : 'green');
  log(`Pass Rate: ${analysis.pass_rate}`, analysis.pass_rate === '100.0%' ? 'green' : 'yellow');

  // Overall status
  const statusIcon = analysis.overall_status === 'PASS' ? '✅' : '❌';
  const statusColor = analysis.overall_status === 'PASS' ? 'green' : 'red';
  log(`\n${statusIcon} Overall Status: ${analysis.overall_status}`, statusColor);

  // Detailed metrics
  if (analysis.details.length > 0) {
    log('\n📈 METRIC DETAILS:', 'cyan');
    analysis.details.forEach(detail => {
      if (detail.status === 'SKIPPED') {
        log(`  ⊘  ${detail.metric}: ${detail.reason}`, 'yellow');
      } else {
        const fmt_result = formatResult(detail);
        log(`  ${fmt_result.text}`, fmt_result.color);
      }
    });
  }

  // Regressions
  if (analysis.regressions.length > 0) {
    log('\n🚨 REGRESSIONS DETECTED:', 'red');
    analysis.regressions.forEach(reg => {
      const severity_icon = reg.severity === 'CRITICAL' ? '🔴' : '🟠';
      log(`  ${severity_icon} ${reg.metric}`, 'red');
      log(`     Current: ${fmt(reg.current)}${reg.unit} (baseline: ${fmt(reg.baseline)}${reg.unit})`, 'red');
      log(`     Regression: +${fmt(Math.abs(reg.regression_percent))}% (tolerance: ${fmt(reg.tolerance_percent)}%)`, 'red');
      log(`     Severity: ${reg.severity}`, 'red');
    });
  }

  log('\n' + '='.repeat(80), 'bold');
}

/**
 * Save regression analysis to JSON
 * @param {Object} analysis - Analysis results
 * @param {string} outputPath - Output file path
 */
function saveAnalysis(analysis, outputPath) {
  fs.writeFileSync(outputPath, JSON.stringify(analysis, null, 2));
  log(`\n💾 Analysis saved: ${outputPath}`, 'blue');
}

/**
 * Main execution
 */
async function main() {
  try {
    // Parse arguments
    const args = process.argv.slice(2);
    const baselineVersion = args[0] || 'v6.0.0';
    const resultsPath = args[1] || '/home/user/unrdf/benchmarks/results/benchmark-results.json';

    log('🔍 Performance Regression Detector', 'bold');
    log(`Baseline: ${baselineVersion}`, 'cyan');
    log(`Results: ${resultsPath}`, 'cyan');

    // Load files
    log('\nLoading baseline and results...', 'cyan');
    const baseline = loadBaseline(baselineVersion);
    const results = loadResults(resultsPath);

    // Detect regressions
    log('Analyzing performance metrics...', 'cyan');
    const analysis = detectRegressions(baseline, results);

    // Generate report
    generateReport(analysis);

    // Save analysis
    const analysisPath = resultsPath.replace(/\.json$/, '-regression-analysis.json');
    saveAnalysis(analysis, analysisPath);

    // Exit with appropriate code
    process.exit(analysis.overall_status === 'PASS' ? 0 : 1);

  } catch (error) {
    log(`\n❌ Error: ${error.message}`, 'red');
    console.error(error);
    process.exit(2);
  }
}

main().catch(error => {
  log(`Fatal error: ${error.message}`, 'red');
  process.exit(2);
});
