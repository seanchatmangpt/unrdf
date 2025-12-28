#!/usr/bin/env node
/**
 * V6 Receipt Overhead Benchmark
 *
 * Measures: withReceipt(fn) time vs bare fn()
 * Target: <1% overhead (receipt generation + hash)
 * Benchmark: 10k operations, report median time
 * Profile: Which parts (hashing, merkle proof, json) take time?
 *
 * @module benchmarks/v6/1-receipt-overhead
 */

import { performance } from 'node:perf_hooks';
import { createReceipt } from '../../packages/v6-core/src/receipts/index.mjs';

// =============================================================================
// Benchmark Configuration
// =============================================================================

const CONFIG = {
  iterations: 10_000,
  warmupIterations: 1_000,
};

// =============================================================================
// Test Functions
// =============================================================================

/**
 * Simple computation function (baseline)
 * @param {number} x - Input value
 * @returns {number} Result
 */
function simpleComputation(x) {
  return x * 2 + Math.sqrt(x);
}

/**
 * Wrapped computation with receipt
 * @param {number} x - Input value
 * @returns {Promise<{result: number, receipt: Object}>}
 */
async function computationWithReceipt(x) {
  const result = simpleComputation(x);

  const receipt = await createReceipt('execution', {
    eventType: 'TASK_COMPLETED',
    caseId: `case-${x}`,
    taskId: `task-${x}`,
    payload: { input: x, output: result },
  });

  return { result, receipt };
}

// =============================================================================
// Timing Utilities
// =============================================================================

/**
 * Calculate statistics from array of values
 * @param {number[]} values - Array of numbers
 * @returns {Object} Statistics
 */
function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  const variance =
    values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
  const stddev = Math.sqrt(variance);

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    stddev,
    median: sorted[Math.floor(sorted.length / 2)],
    p50: sorted[Math.floor(sorted.length * 0.5)],
    p90: sorted[Math.floor(sorted.length * 0.9)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

// =============================================================================
// Benchmark Runner
// =============================================================================

/**
 * Run baseline benchmark (bare function)
 * @returns {Promise<{timings: number[], totalTime: number}>}
 */
async function runBaselineBenchmark() {
  console.log(`\n[Baseline] Running ${CONFIG.iterations} iterations...`);

  // Warmup
  for (let i = 0; i < CONFIG.warmupIterations; i++) {
    simpleComputation(i);
  }

  // Actual benchmark
  const timings = [];
  const startTotal = performance.now();

  for (let i = 0; i < CONFIG.iterations; i++) {
    const start = performance.now();
    simpleComputation(i);
    const elapsed = performance.now() - start;
    timings.push(elapsed);
  }

  const totalTime = performance.now() - startTotal;

  return { timings, totalTime };
}

/**
 * Run receipt benchmark (function + receipt generation)
 * @returns {Promise<{timings: number[], totalTime: number, phaseTimings: Object}>}
 */
async function runReceiptBenchmark() {
  console.log(`\n[Receipt] Running ${CONFIG.iterations} iterations...`);

  // Warmup
  for (let i = 0; i < CONFIG.warmupIterations; i++) {
    await computationWithReceipt(i);
  }

  // Actual benchmark
  const timings = [];
  const phaseTimings = {
    computation: [],
    receiptGeneration: [],
    total: [],
  };
  const startTotal = performance.now();

  for (let i = 0; i < CONFIG.iterations; i++) {
    const startComputation = performance.now();
    const result = simpleComputation(i);
    const computationTime = performance.now() - startComputation;

    const startReceipt = performance.now();
    const receipt = await createReceipt('execution', {
      eventType: 'TASK_COMPLETED',
      caseId: `case-${i}`,
      taskId: `task-${i}`,
      payload: { input: i, output: result },
    });
    const receiptTime = performance.now() - startReceipt;

    const elapsed = performance.now() - startComputation;

    timings.push(elapsed);
    phaseTimings.computation.push(computationTime);
    phaseTimings.receiptGeneration.push(receiptTime);
    phaseTimings.total.push(elapsed);
  }

  const totalTime = performance.now() - startTotal;

  return { timings, totalTime, phaseTimings };
}

// =============================================================================
// Main Benchmark
// =============================================================================

async function main() {
  console.log('='.repeat(80));
  console.log('V6 Receipt Overhead Benchmark');
  console.log('='.repeat(80));
  console.log(`Target: <1% overhead`);
  console.log(`Iterations: ${CONFIG.iterations.toLocaleString()}`);
  console.log(`Warmup: ${CONFIG.warmupIterations.toLocaleString()}`);

  // Run baseline
  const baseline = await runBaselineBenchmark();
  const baselineStats = calculateStats(baseline.timings);

  // Run receipt benchmark
  const receipt = await runReceiptBenchmark();
  const receiptStats = calculateStats(receipt.timings);

  // Calculate phase statistics
  const computationStats = calculateStats(receipt.phaseTimings.computation);
  const receiptGenStats = calculateStats(receipt.phaseTimings.receiptGeneration);

  // Calculate overhead
  const overheadMs = receiptStats.median - baselineStats.median;
  const overheadPercent = (overheadMs / baselineStats.median) * 100;

  // Print results
  console.log('\n' + '='.repeat(80));
  console.log('BASELINE (Bare Function)');
  console.log('='.repeat(80));
  console.log(`Min:       ${baselineStats.min.toFixed(6)} ms`);
  console.log(`Mean:      ${baselineStats.mean.toFixed(6)} ms`);
  console.log(`Median:    ${baselineStats.median.toFixed(6)} ms`);
  console.log(`Stddev:    ${baselineStats.stddev.toFixed(6)} ms`);
  console.log(`P90:       ${baselineStats.p90.toFixed(6)} ms`);
  console.log(`P95:       ${baselineStats.p95.toFixed(6)} ms`);
  console.log(`P99:       ${baselineStats.p99.toFixed(6)} ms`);
  console.log(`Max:       ${baselineStats.max.toFixed(6)} ms`);
  console.log(`Total:     ${baseline.totalTime.toFixed(2)} ms`);

  console.log('\n' + '='.repeat(80));
  console.log('WITH RECEIPT (Function + Receipt Generation)');
  console.log('='.repeat(80));
  console.log(`Min:       ${receiptStats.min.toFixed(6)} ms`);
  console.log(`Mean:      ${receiptStats.mean.toFixed(6)} ms`);
  console.log(`Median:    ${receiptStats.median.toFixed(6)} ms`);
  console.log(`Stddev:    ${receiptStats.stddev.toFixed(6)} ms`);
  console.log(`P90:       ${receiptStats.p90.toFixed(6)} ms`);
  console.log(`P95:       ${receiptStats.p95.toFixed(6)} ms`);
  console.log(`P99:       ${receiptStats.p99.toFixed(6)} ms`);
  console.log(`Max:       ${receiptStats.max.toFixed(6)} ms`);
  console.log(`Total:     ${receipt.totalTime.toFixed(2)} ms`);

  console.log('\n' + '='.repeat(80));
  console.log('PHASE BREAKDOWN');
  console.log('='.repeat(80));
  console.log(`Computation (median):        ${computationStats.median.toFixed(6)} ms`);
  console.log(`Receipt Generation (median): ${receiptGenStats.median.toFixed(6)} ms`);
  console.log(
    `Receipt % of total:          ${((receiptGenStats.median / receiptStats.median) * 100).toFixed(2)}%`
  );

  console.log('\n' + '='.repeat(80));
  console.log('OVERHEAD ANALYSIS');
  console.log('='.repeat(80));
  console.log(`Baseline median:     ${baselineStats.median.toFixed(6)} ms`);
  console.log(`Receipt median:      ${receiptStats.median.toFixed(6)} ms`);
  console.log(`Overhead (absolute): ${overheadMs.toFixed(6)} ms`);
  console.log(`Overhead (relative): ${overheadPercent.toFixed(2)}%`);
  console.log(`Target:              <1.00%`);
  console.log(`Status:              ${overheadPercent < 1.0 ? '✅ PASS' : '❌ FAIL'}`);

  // JSON output for aggregation
  console.log('\n__JSON_RESULTS__');
  const results = {
    benchmark: 'receipt-overhead',
    timestamp: new Date().toISOString(),
    config: CONFIG,
    baseline: {
      median: baselineStats.median,
      mean: baselineStats.mean,
      p95: baselineStats.p95,
      p99: baselineStats.p99,
      total: baseline.totalTime,
    },
    withReceipt: {
      median: receiptStats.median,
      mean: receiptStats.mean,
      p95: receiptStats.p95,
      p99: receiptStats.p99,
      total: receipt.totalTime,
    },
    phases: {
      computation: {
        median: computationStats.median,
      },
      receiptGeneration: {
        median: receiptGenStats.median,
      },
    },
    overhead: {
      absoluteMs: overheadMs,
      percentRelative: overheadPercent,
    },
    target: {
      maxOverheadPercent: 1.0,
      pass: overheadPercent < 1.0,
    },
  };
  console.log(JSON.stringify(results, null, 2));

  // Exit with appropriate code
  process.exit(overheadPercent < 1.0 ? 0 : 1);
}

main().catch((error) => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
