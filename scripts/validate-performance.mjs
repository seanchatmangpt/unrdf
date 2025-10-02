#!/usr/bin/env node

/**
 * @fileoverview Performance Validation Script for CI/CD
 *
 * @description
 * Validates benchmark results against performance targets.
 * Fails CI if any target is not met or if regression > 10%.
 *
 * Usage:
 *   node scripts/validate-performance.mjs bench-results.json
 *   node scripts/validate-performance.mjs bench-results.json --baseline=benchmarks/v2.1.1.json
 */

import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Performance targets from research-findings.md
const TARGETS = {
  'cli-startup': {
    metric: 'p99',
    target: 100,
    unit: 'ms',
    description: 'CLI cold start time'
  },
  'hook-eval-ask': {
    metric: 'p99',
    target: 2,
    unit: 'ms',
    description: 'ASK hook evaluation'
  },
  'hook-eval-select': {
    metric: 'p99',
    target: 5,
    unit: 'ms',
    description: 'SELECT hook evaluation'
  },
  'transaction-commit': {
    metric: 'p99',
    target: 5,
    unit: 'ms',
    description: 'Transaction commit'
  },
  'sidecar-health': {
    metric: 'p99',
    target: 10,
    unit: 'ms',
    description: 'Sidecar health check'
  },
  'sparql-complex': {
    metric: 'p99',
    target: 50,
    unit: 'ms',
    description: 'Complex SPARQL query'
  },
  'hook-throughput': {
    metric: 'value',
    target: 10000,
    unit: 'exec/min',
    description: 'Hook evaluation throughput'
  },
  'transaction-throughput': {
    metric: 'value',
    target: 1000,
    unit: 'tx/sec',
    description: 'Transaction throughput'
  },
  'sidecar-throughput': {
    metric: 'value',
    target: 1000,
    unit: 'RPS',
    description: 'Sidecar RPC throughput'
  }
};

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  const resultsFile = args[0];
  const baselineFile = args.find(arg => arg.startsWith('--baseline='))?.split('=')[1];
  const verbose = args.includes('--verbose') || args.includes('-v');
  const failOnRegression = !args.includes('--no-fail-regression');
  const regressionThreshold = parseFloat(args.find(arg => arg.startsWith('--regression-threshold='))?.split('=')[1] || '0.1');

  if (!resultsFile) {
    console.error('Usage: node validate-performance.mjs <results-file> [--baseline=<baseline-file>] [--verbose] [--no-fail-regression] [--regression-threshold=0.1]');
    process.exit(1);
  }

  return {
    resultsFile,
    baselineFile,
    verbose,
    failOnRegression,
    regressionThreshold
  };
}

/**
 * Load benchmark results from JSON file
 * @param {string} filePath - Path to results file
 * @returns {Promise<Object>} Benchmark results
 */
async function loadResults(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    return JSON.parse(content);
  } catch (error) {
    throw new Error(`Failed to load results from ${filePath}: ${error.message}`);
  }
}

/**
 * Extract metric value from benchmark result
 * @param {Object} result - Benchmark result
 * @param {string} metricName - Metric name (e.g., 'p99', 'mean', 'value')
 * @returns {number} Metric value
 */
function extractMetric(result, metricName) {
  // Vitest bench format: { name, hz, mean, p50, p95, p99, ... }
  if (result[metricName] !== undefined) {
    return result[metricName];
  }

  // Custom format: { value, p50, p95, p99, ... }
  if (result.stats && result.stats[metricName] !== undefined) {
    return result.stats[metricName];
  }

  // Fallback to mean if metric not found
  return result.mean || result.hz || 0;
}

/**
 * Find matching benchmark result
 * @param {Array} results - Array of benchmark results
 * @param {string} benchmarkKey - Benchmark key (e.g., 'cli-startup')
 * @returns {Object|null} Matching result or null
 */
function findBenchmark(results, benchmarkKey) {
  // Try exact match first
  let match = results.find(r => r.name && r.name.toLowerCase().includes(benchmarkKey.toLowerCase()));

  if (match) {
    return match;
  }

  // Try partial match
  const keyParts = benchmarkKey.split('-');
  match = results.find(r => {
    const name = (r.name || '').toLowerCase();
    return keyParts.every(part => name.includes(part.toLowerCase()));
  });

  return match || null;
}

/**
 * Validate benchmark results against targets
 * @param {Object} results - Benchmark results
 * @param {Object} options - Validation options
 * @returns {Object} Validation results
 */
function validateResults(results, options = {}) {
  const { verbose } = options;
  const failures = [];
  const warnings = [];
  const passes = [];

  console.log('\n📊 Performance Validation Report\n');
  console.log('═'.repeat(80));

  for (const [benchmarkKey, target] of Object.entries(TARGETS)) {
    const result = findBenchmark(results.benchmarks || results, benchmarkKey);

    if (!result) {
      warnings.push(`⚠️  Missing benchmark: ${benchmarkKey} (${target.description})`);
      continue;
    }

    const actualValue = extractMetric(result, target.metric);
    const isPass = actualValue <= target.target;

    const status = isPass ? '✅' : '❌';
    const comparison = isPass ? 'PASS' : 'FAIL';

    const line = `${status} ${benchmarkKey.padEnd(25)} | ${target.description.padEnd(30)} | ${actualValue.toFixed(2)} ${target.unit} ${comparison} (target: ${target.target} ${target.unit})`;

    if (verbose || !isPass) {
      console.log(line);
    }

    if (isPass) {
      passes.push({
        benchmark: benchmarkKey,
        description: target.description,
        actual: actualValue,
        target: target.target,
        unit: target.unit
      });
    } else {
      failures.push({
        benchmark: benchmarkKey,
        description: target.description,
        actual: actualValue,
        target: target.target,
        unit: target.unit,
        message: `${benchmarkKey}: ${target.metric} ${actualValue.toFixed(2)}${target.unit} > target ${target.target}${target.unit}`
      });
    }
  }

  console.log('═'.repeat(80));
  console.log(`\nResults: ${passes.length} passed, ${failures.length} failed, ${warnings.length} warnings\n`);

  return { passes, failures, warnings };
}

/**
 * Compare results against baseline to detect regressions
 * @param {Object} currentResults - Current benchmark results
 * @param {Object} baselineResults - Baseline benchmark results
 * @param {number} threshold - Regression threshold (e.g., 0.1 for 10%)
 * @returns {Array} Regressions detected
 */
function detectRegressions(currentResults, baselineResults, threshold = 0.1) {
  const regressions = [];

  console.log('\n📈 Regression Analysis\n');
  console.log('═'.repeat(80));

  for (const [benchmarkKey, target] of Object.entries(TARGETS)) {
    const current = findBenchmark(currentResults.benchmarks || currentResults, benchmarkKey);
    const baseline = findBenchmark(baselineResults.benchmarks || baselineResults, benchmarkKey);

    if (!current || !baseline) {
      continue;
    }

    const currentValue = extractMetric(current, target.metric);
    const baselineValue = extractMetric(baseline, target.metric);

    const percentChange = ((currentValue - baselineValue) / baselineValue) * 100;
    const isRegression = percentChange > (threshold * 100);

    const status = isRegression ? '❌' : percentChange > 0 ? '⚠️' : '✅';
    const direction = percentChange > 0 ? '↑' : '↓';

    const line = `${status} ${benchmarkKey.padEnd(25)} | ${direction} ${Math.abs(percentChange).toFixed(1)}% | Current: ${currentValue.toFixed(2)}${target.unit} | Baseline: ${baselineValue.toFixed(2)}${target.unit}`;

    console.log(line);

    if (isRegression) {
      regressions.push({
        benchmark: benchmarkKey,
        description: target.description,
        current: currentValue,
        baseline: baselineValue,
        percentChange: percentChange,
        threshold: threshold * 100,
        message: `Regression detected in ${benchmarkKey}: ${percentChange.toFixed(1)}% slower than baseline (threshold: ${(threshold * 100).toFixed(0)}%)`
      });
    }
  }

  console.log('═'.repeat(80));
  console.log(`\nRegressions: ${regressions.length} detected (threshold: ${(threshold * 100).toFixed(0)}%)\n`);

  return regressions;
}

/**
 * Generate summary report
 * @param {Object} validation - Validation results
 * @param {Array} regressions - Regression results
 */
function generateSummary(validation, regressions = []) {
  const { passes, failures, warnings } = validation;
  const totalTests = passes.length + failures.length;

  console.log('\n📋 Summary\n');
  console.log('═'.repeat(80));
  console.log(`Total Benchmarks: ${totalTests}`);
  console.log(`Passed:           ${passes.length} (${((passes.length / totalTests) * 100).toFixed(1)}%)`);
  console.log(`Failed:           ${failures.length} (${((failures.length / totalTests) * 100).toFixed(1)}%)`);
  console.log(`Warnings:         ${warnings.length}`);

  if (regressions.length > 0) {
    console.log(`Regressions:      ${regressions.length}`);
  }

  console.log('═'.repeat(80));

  if (failures.length > 0) {
    console.log('\n❌ FAILED BENCHMARKS:\n');
    failures.forEach(f => {
      console.log(`   • ${f.message}`);
    });
  }

  if (regressions.length > 0) {
    console.log('\n⚠️  PERFORMANCE REGRESSIONS:\n');
    regressions.forEach(r => {
      console.log(`   • ${r.message}`);
    });
  }

  if (warnings.length > 0) {
    console.log('\n⚠️  WARNINGS:\n');
    warnings.forEach(w => {
      console.log(`   • ${w}`);
    });
  }

  console.log();
}

/**
 * Main validation function
 */
async function main() {
  const args = parseArgs();

  try {
    // Load current results
    console.log(`Loading results from ${args.resultsFile}...`);
    const currentResults = await loadResults(args.resultsFile);

    // Validate against targets
    const validation = validateResults(currentResults, { verbose: args.verbose });

    // Load baseline and check regressions if provided
    let regressions = [];
    if (args.baselineFile) {
      console.log(`\nLoading baseline from ${args.baselineFile}...`);
      const baselineResults = await loadResults(args.baselineFile);
      regressions = detectRegressions(currentResults, baselineResults, args.regressionThreshold);
    }

    // Generate summary
    generateSummary(validation, regressions);

    // Determine exit code
    const hasFailures = validation.failures.length > 0;
    const hasRegressions = args.failOnRegression && regressions.length > 0;

    if (hasFailures || hasRegressions) {
      console.error('❌ Performance validation FAILED\n');
      process.exit(1);
    } else {
      console.log('✅ Performance validation PASSED\n');
      process.exit(0);
    }
  } catch (error) {
    console.error(`\n❌ Validation error: ${error.message}\n`);
    process.exit(1);
  }
}

// Run main function
main().catch(error => {
  console.error(error);
  process.exit(1);
});
