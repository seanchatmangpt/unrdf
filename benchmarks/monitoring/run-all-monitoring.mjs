#!/usr/bin/env node
/**
 * @fileoverview Run All Monitoring Benchmarks
 * @module benchmarks/monitoring/run-all-monitoring
 *
 * @description
 * Runs complete monitoring benchmark suite:
 * - Throughput benchmarks
 * - Latency benchmarks
 * - Scalability benchmarks
 * - Generates comprehensive report
 */

import { writeFile } from 'node:fs/promises';
import { runThroughputBenchmarks } from './throughput-bench.mjs';
import { runLatencyBenchmarks } from './latency-bench.mjs';
import { runScalabilityBenchmarks } from './scalability-bench.mjs';

/**
 * Run all monitoring benchmarks
 */
async function runAllMonitoringBenchmarks() {
  console.log('\n');
  console.log('═'.repeat(80));
  console.log('  UNRDF Monitoring Benchmark Suite');
  console.log('  Comprehensive performance analysis');
  console.log('═'.repeat(80));
  console.log('\n');

  const startTime = Date.now();
  const results = {
    timestamp: new Date().toISOString(),
    platform: process.platform,
    arch: process.arch,
    nodeVersion: process.version,
    benchmarks: {},
  };

  try {
    // Run throughput benchmarks
    console.log('\n[1/3] Running throughput benchmarks...');
    results.benchmarks.throughput = await runThroughputBenchmarks();

    // Run latency benchmarks
    console.log('\n[2/3] Running latency benchmarks...');
    results.benchmarks.latency = await runLatencyBenchmarks();

    // Run scalability benchmarks
    console.log('\n[3/3] Running scalability benchmarks...');
    results.benchmarks.scalability = await runScalabilityBenchmarks();

    const duration = Date.now() - startTime;

    // Summary
    console.log('\n');
    console.log('═'.repeat(80));
    console.log('  Benchmark Summary');
    console.log('═'.repeat(80));
    console.log(`\nTotal Duration: ${(duration / 1000).toFixed(2)}s`);

    // Throughput summary
    const throughputMetrics = results.benchmarks.throughput.metrics.summary;
    console.log(`\nThroughput:`);
    console.log(`  Total Operations:    ${throughputMetrics.totalOperations.toLocaleString()}`);
    console.log(`  Average Throughput:  ${throughputMetrics.avgThroughput.toFixed(2)} ops/sec`);

    // Latency summary
    const latencyMetrics = results.benchmarks.latency.metrics.summary.currentSnapshot;
    if (latencyMetrics) {
      console.log(`\nLatency Distribution:`);
      console.log(`  P50:  ${latencyMetrics.latency.p50.toFixed(3)} ms`);
      console.log(`  P95:  ${latencyMetrics.latency.p95.toFixed(3)} ms`);
      console.log(`  P99:  ${latencyMetrics.latency.p99.toFixed(3)} ms`);
    }

    // Scalability summary
    const scalability = results.benchmarks.scalability.scaling;
    console.log(`\nScalability:`);
    console.log(`  Max Agents:          ${scalability.maxAgents}`);
    console.log(`  Max Speedup:         ${scalability.maxSpeedup.toFixed(2)}x`);
    console.log(`  Scaling Efficiency:  ${scalability.scalingEfficiency.toFixed(1)}%`);

    // Save results
    const outputPath = './benchmarks/results/monitoring-results.json';
    await writeFile(outputPath, JSON.stringify(results, null, 2));
    console.log(`\nResults saved to: ${outputPath}`);

    // Performance assessment
    console.log('\n');
    console.log('═'.repeat(80));
    console.log('  Performance Assessment');
    console.log('═'.repeat(80));

    const assessments = [];

    // Throughput assessment
    if (throughputMetrics.avgThroughput > 100) {
      assessments.push('✓ Excellent throughput (> 100 ops/sec)');
    } else if (throughputMetrics.avgThroughput > 50) {
      assessments.push('⚠ Good throughput (> 50 ops/sec)');
    } else {
      assessments.push('✗ Low throughput (< 50 ops/sec)');
    }

    // Latency assessment
    if (latencyMetrics && latencyMetrics.latency.p95 < 20) {
      assessments.push('✓ Excellent latency (P95 < 20ms)');
    } else if (latencyMetrics && latencyMetrics.latency.p95 < 50) {
      assessments.push('⚠ Good latency (P95 < 50ms)');
    } else {
      assessments.push('✗ High latency (P95 > 50ms)');
    }

    // Scalability assessment
    if (scalability.scalingEfficiency > 80) {
      assessments.push('✓ Excellent scalability (> 80% efficiency)');
    } else if (scalability.scalingEfficiency > 60) {
      assessments.push('⚠ Good scalability (> 60% efficiency)');
    } else {
      assessments.push('✗ Poor scalability (< 60% efficiency)');
    }

    console.log('\n' + assessments.join('\n'));
    console.log('\n');

    return results;

  } catch (error) {
    console.error('\n✗ Benchmark suite failed:', error);
    throw error;
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllMonitoringBenchmarks()
    .then(() => {
      console.log('\n✓ All benchmarks completed successfully\n');
      process.exit(0);
    })
    .catch(error => {
      console.error('\n✗ Benchmark suite failed:', error);
      process.exit(1);
    });
}

export { runAllMonitoringBenchmarks };
