#!/usr/bin/env node
/**
 * @file Benchmark Runner
 * @description Executes all benchmarks sequentially and generates summary report
 */

import { writeFile, mkdir } from 'fs/promises';
import { join } from 'path';
import { runHookRegistrationBenchmark } from './core/01-hook-registration.bench.mjs';
import { runHookExecutionLatencyBenchmark } from './core/02-hook-execution-latency.bench.mjs';
import { runConcurrentExecutionBenchmark } from './core/03-concurrent-execution.bench.mjs';
import { runMemoryFootprintBenchmark } from './core/04-memory-footprint.bench.mjs';
import { runConditionEvaluationBenchmark } from './core/05-condition-evaluation.bench.mjs';

/**
 * Format duration in human-readable format
 * @param {number} ms - Duration in milliseconds
 * @returns {string} Formatted duration
 */
function formatDuration(ms) {
  if (ms < 1000) return `${ms.toFixed(2)}ms`;
  return `${(ms / 1000).toFixed(2)}s`;
}

/**
 * Format number with thousands separator
 * @param {number} num - Number to format
 * @returns {string} Formatted number
 */
function formatNumber(num) {
  return num.toFixed(2).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
}

/**
 * Print colored status
 * @param {boolean} passed - Test passed
 * @returns {string} Colored status string
 */
function getStatus(passed) {
  return passed ? '✅ PASS' : '❌ FAIL';
}

/**
 * Print benchmark results
 * @param {Object} result - Benchmark result
 */
function printBenchmarkResult(result) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`📊 ${result.name.toUpperCase()} ${getStatus(result.passed)}`);
  console.log(`${'='.repeat(80)}`);

  for (const test of result.tests) {
    console.log(`\n  ${test.scale || test.complexity || test.concurrencyLevel || test.testCase || 'test'}:`);

    if (test.latency) {
      console.log(`    Latency: p50=${formatDuration(test.latency.p50)}, p95=${formatDuration(test.latency.p95)}, p99=${formatDuration(test.latency.p99)}`);
    }

    if (test.throughputHooksPerSec !== undefined) {
      console.log(`    Throughput: ${formatNumber(test.throughputHooksPerSec)} hooks/sec`);
    }

    if (test.throughputOpsPerSec !== undefined) {
      console.log(`    Throughput: ${formatNumber(test.throughputOpsPerSec)} ops/sec`);
    }

    if (test.memoryUsedMB !== undefined) {
      console.log(`    Memory: ${formatNumber(test.memoryUsedMB)}MB total, ${formatNumber(test.memoryPerHookMB)}MB per hook`);
    }

    if (test.cacheHitRate !== undefined) {
      console.log(`    Cache: ${(test.cacheHitRate * 100).toFixed(1)}% hit rate`);
    }

    console.log(`    Status: ${getStatus(test.passed)}`);
  }

  if (result.summary) {
    console.log(`\n  Summary: ${result.summary.passed}/${result.summary.totalTests} tests passed`);

    if (result.summary.gcWarning) {
      console.log(`  ⚠️  ${result.summary.gcWarning}`);
    }
  }
}

/**
 * Run all benchmarks
 */
async function runAllBenchmarks() {
  console.log('🚀 Starting Knowledge Hooks Performance Benchmarks');
  console.log(`📅 ${new Date().toISOString()}\n`);

  const startTime = performance.now();
  const benchmarkResults = [];

  try {
    // Benchmark 1: Hook Registration
    console.log('Running benchmark 1/5: Hook Registration...');
    const result1 = await runHookRegistrationBenchmark();
    benchmarkResults.push(result1);
    printBenchmarkResult(result1);

    // Benchmark 2: Hook Execution Latency
    console.log('\nRunning benchmark 2/5: Hook Execution Latency...');
    const result2 = await runHookExecutionLatencyBenchmark();
    benchmarkResults.push(result2);
    printBenchmarkResult(result2);

    // Benchmark 3: Concurrent Execution
    console.log('\nRunning benchmark 3/5: Concurrent Execution...');
    const result3 = await runConcurrentExecutionBenchmark();
    benchmarkResults.push(result3);
    printBenchmarkResult(result3);

    // Benchmark 4: Memory Footprint
    console.log('\nRunning benchmark 4/5: Memory Footprint...');
    const result4 = await runMemoryFootprintBenchmark();
    benchmarkResults.push(result4);
    printBenchmarkResult(result4);

    // Benchmark 5: Condition Evaluation
    console.log('\nRunning benchmark 5/5: Condition Evaluation...');
    const result5 = await runConditionEvaluationBenchmark();
    benchmarkResults.push(result5);
    printBenchmarkResult(result5);

  } catch (error) {
    console.error('\n❌ Benchmark execution failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }

  const totalTime = performance.now() - startTime;

  // Generate summary
  const allPassed = benchmarkResults.every(r => r.passed);
  const totalTests = benchmarkResults.reduce((sum, r) => sum + r.summary.totalTests, 0);
  const passedTests = benchmarkResults.reduce((sum, r) => sum + r.summary.passed, 0);
  const failedTests = benchmarkResults.reduce((sum, r) => sum + r.summary.failed, 0);

  console.log(`\n${'='.repeat(80)}`);
  console.log(`📈 BENCHMARK SUMMARY ${getStatus(allPassed)}`);
  console.log(`${'='.repeat(80)}`);
  console.log(`  Total Benchmarks: ${benchmarkResults.length}`);
  console.log(`  Total Tests: ${totalTests}`);
  console.log(`  Passed: ${passedTests}`);
  console.log(`  Failed: ${failedTests}`);
  console.log(`  Duration: ${formatDuration(totalTime)}`);
  console.log(`${'='.repeat(80)}\n`);

  // Export results to JSON
  const outputDir = join(process.cwd(), 'benchmarks', 'results');
  await mkdir(outputDir, { recursive: true });

  const outputPath = join(outputDir, 'benchmark-results.json');
  const jsonOutput = {
    timestamp: new Date().toISOString(),
    totalDurationMs: totalTime,
    summary: {
      totalBenchmarks: benchmarkResults.length,
      totalTests,
      passed: passedTests,
      failed: failedTests,
      allPassed,
    },
    benchmarks: benchmarkResults,
  };

  await writeFile(outputPath, JSON.stringify(jsonOutput, null, 2));
  console.log(`📁 Results exported to: ${outputPath}\n`);

  // Exit with appropriate code
  process.exit(allPassed ? 0 : 1);
}

// Run benchmarks
runAllBenchmarks().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
