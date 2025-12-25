#!/usr/bin/env node
/**
 * Compare current benchmarks against baseline
 * Detect performance regressions
 */

import { readFileSync, writeFileSync, existsSync } from 'fs';

const REGRESSION_THRESHOLD = 10; // percent

/**
 * Load benchmark results
 * @param {string} filepath - Path to results file
 * @returns {Object|null} Benchmark results
 */
function loadBenchmarks(filepath) {
  try {
    if (!existsSync(filepath)) {
      return null;
    }
    return JSON.parse(readFileSync(filepath, 'utf8'));
  } catch (error) {
    console.warn(`Failed to load benchmarks from ${filepath}:`, error.message);
    return null;
  }
}

/**
 * Compare benchmark results
 * @param {Object} baseline - Baseline results
 * @param {Object} current - Current results
 * @returns {Object} Comparison results
 */
function compareBenchmarks(baseline, current) {
  const comparison = {
    regressions: [],
    improvements: [],
    unchanged: []
  };

  // Flatten benchmark arrays for comparison
  const baselineFlat = flattenBenchmarks(baseline);
  const currentFlat = flattenBenchmarks(current);

  // Compare each benchmark
  for (const [name, currentDuration] of Object.entries(currentFlat)) {
    const baselineDuration = baselineFlat[name];

    if (!baselineDuration) {
      // New benchmark
      continue;
    }

    const change = ((currentDuration - baselineDuration) / baselineDuration) * 100;

    if (change > REGRESSION_THRESHOLD) {
      comparison.regressions.push({
        name,
        baseline: baselineDuration,
        current: currentDuration,
        change: change.toFixed(2)
      });
    } else if (change < -REGRESSION_THRESHOLD) {
      comparison.improvements.push({
        name,
        baseline: baselineDuration,
        current: currentDuration,
        change: change.toFixed(2)
      });
    } else {
      comparison.unchanged.push({
        name,
        baseline: baselineDuration,
        current: currentDuration,
        change: change.toFixed(2)
      });
    }
  }

  return comparison;
}

/**
 * Flatten nested benchmark structure
 * @param {Object} results - Benchmark results
 * @returns {Object} Flattened benchmarks
 */
function flattenBenchmarks(results) {
  const flat = {};

  if (results && results.benchmarks) {
    for (const [pkg, benchmarks] of Object.entries(results.benchmarks)) {
      for (const benchmark of benchmarks) {
        const key = `${pkg}:${benchmark.name}`;
        flat[key] = benchmark.duration;
      }
    }
  }

  return flat;
}

/**
 * Main execution
 */
function main() {
  console.log('Comparing benchmarks against baseline...');

  // Load baseline and current results
  const baseline = loadBenchmarks('./baseline/benchmark-results.json');
  const current = loadBenchmarks('./benchmark-results.json');

  if (!current) {
    console.error('❌ Current benchmark results not found');
    process.exit(1);
  }

  if (!baseline) {
    console.warn('⚠️  No baseline found - first run, establishing baseline');
    process.exit(0);
  }

  // Compare benchmarks
  const comparison = compareBenchmarks(baseline, current);

  // Write comparison results
  writeFileSync('benchmark-regression.json', JSON.stringify(comparison, null, 2));

  // Report results
  console.log('\nBenchmark Comparison Results:');
  console.log(`  Regressions: ${comparison.regressions.length}`);
  console.log(`  Improvements: ${comparison.improvements.length}`);
  console.log(`  Unchanged: ${comparison.unchanged.length}`);

  if (comparison.regressions.length > 0) {
    console.log('\n⚠️  Performance Regressions Detected:');
    comparison.regressions.forEach(r => {
      console.log(`  - ${r.name}: ${r.baseline}ms → ${r.current}ms (+${r.change}%)`);
    });
  }

  if (comparison.improvements.length > 0) {
    console.log('\n✅ Performance Improvements:');
    comparison.improvements.forEach(i => {
      console.log(`  - ${i.name}: ${i.baseline}ms → ${i.current}ms (${i.change}%)`);
    });
  }

  console.log('\n✅ Benchmark comparison completed');
  console.log('Results saved to: benchmark-regression.json');
}

main();
