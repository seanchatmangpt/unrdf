#!/usr/bin/env node
/**
 * Performance Regression Example
 * Demonstrates how regression detection works
 *
 * This benchmark intentionally introduces a performance regression
 * to show how the benchmark suite detects it.
 *
 * Usage:
 *   node benchmarks/regression-example.mjs
 */

import { performance } from 'perf_hooks';

const config = {
  iterations: 10000,
  warmupIterations: 1000,
};

/**
 * Calculate statistics
 */
function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    median: sorted[Math.floor(sorted.length / 2)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

/**
 * Fast operation (baseline)
 */
function fastOperation(n) {
  return n * 2;
}

/**
 * Slow operation (simulated regression)
 * Intentionally adds 50% overhead to simulate performance regression
 */
function slowOperation(n) {
  // Simulate 50% slower operation
  let result = n;
  for (let i = 0; i < 10; i++) {
    result = result * 1.01;
  }
  return result;
}

/**
 * Benchmark fast operation
 */
function benchmarkFast() {
  const latencies = [];

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    fastOperation(i);
  }

  // Benchmark
  for (let i = 0; i < config.iterations; i++) {
    const start = performance.now();
    fastOperation(i);
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark slow operation (with regression)
 */
function benchmarkSlow() {
  const latencies = [];

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    slowOperation(i);
  }

  // Benchmark
  for (let i = 0; i < config.iterations; i++) {
    const start = performance.now();
    slowOperation(i);
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Main execution
 */
async function main() {
  console.log('='.repeat(70));
  console.log('PERFORMANCE REGRESSION EXAMPLE');
  console.log('='.repeat(70));
  console.log('');
  console.log('This demonstrates how the benchmark suite detects regressions.');
  console.log('We compare a "fast" baseline with a "slow" regression.');
  console.log('');

  // Benchmark fast operation (baseline)
  console.log('Running FAST operation (baseline)...');
  const fastLatencies = benchmarkFast();
  const fastStats = calculateStats(fastLatencies);

  console.log(`  Mean: ${(fastStats.mean * 1000).toFixed(3)} us`);
  console.log(`  P95:  ${(fastStats.p95 * 1000).toFixed(3)} us`);
  console.log(`  P99:  ${(fastStats.p99 * 1000).toFixed(3)} us`);

  // Benchmark slow operation (regression)
  console.log('\nRunning SLOW operation (regression)...');
  const slowLatencies = benchmarkSlow();
  const slowStats = calculateStats(slowLatencies);

  console.log(`  Mean: ${(slowStats.mean * 1000).toFixed(3)} us`);
  console.log(`  P95:  ${(slowStats.p95 * 1000).toFixed(3)} us`);
  console.log(`  P99:  ${(slowStats.p99 * 1000).toFixed(3)} us`);

  // Calculate regression
  const regression = ((slowStats.p95 - fastStats.p95) / fastStats.p95) * 100;

  console.log('\n' + '='.repeat(70));
  console.log('REGRESSION ANALYSIS');
  console.log('='.repeat(70));
  console.log(`Baseline P95: ${(fastStats.p95 * 1000).toFixed(3)} us`);
  console.log(`Current P95:  ${(slowStats.p95 * 1000).toFixed(3)} us`);
  console.log(`Change:       +${regression.toFixed(1)}%`);

  if (regression > 10) {
    console.log(`Status:       FAILED ⚠️  (regression exceeds 10% threshold)`);
  } else {
    console.log(`Status:       PASSED ✅`);
  }

  console.log('\n' + '='.repeat(70));
  console.log('HOW IT WORKS');
  console.log('='.repeat(70));
  console.log('');
  console.log('1. Baseline is stored in benchmarks/baseline.json');
  console.log('2. Each PR runs benchmarks and compares to baseline');
  console.log('3. If any metric is >10% slower, CI fails');
  console.log('4. Developers must fix regression or justify change');
  console.log('5. After approval, baseline is updated on main branch');
  console.log('');

  console.log('To see this in action:');
  console.log('  1. Run: npm run bench:baseline  # Create baseline');
  console.log('  2. Make code slower (simulate regression)');
  console.log('  3. Run: npm run bench            # Detect regression');
  console.log('  4. Fix the regression');
  console.log('  5. Run: npm run bench:baseline  # Update baseline');
  console.log('');
}

main().catch(console.error);
