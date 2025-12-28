#!/usr/bin/env node
/**
 * @fileoverview Latency Benchmark - Measure operation latency distribution
 * @module benchmarks/monitoring/latency-bench
 *
 * @description
 * Benchmarks latency characteristics:
 * - P50, P75, P90, P95, P99, P99.9 latencies
 * - Tail latency analysis
 * - Latency variance under load
 * - Outlier detection
 */

import { performance } from 'node:perf_hooks';
import { suite, runBenchmark, formatMarkdownTable, calculateStats } from '../framework.mjs';
import { MetricsCollector } from '../../src/monitoring/metrics-collector.mjs';

/**
 * Simulated operation with variable latency
 * @param {string} profile - Latency profile (fast, normal, slow)
 * @returns {Promise<void>}
 */
async function simulateLatency(profile = 'normal') {
  const profiles = {
    fast: { base: 1, variance: 0.5 },
    normal: { base: 5, variance: 2 },
    slow: { base: 20, variance: 10 },
    variable: { base: 5, variance: 50 }, // High variance
  };

  const p = profiles[profile] || profiles.normal;
  const delay = Math.max(0, p.base + (Math.random() - 0.5) * p.variance * 2);

  await new Promise(resolve => setTimeout(resolve, delay));
}

/**
 * Run latency benchmarks
 */
export async function runLatencyBenchmarks() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║         Latency Benchmark Suite                          ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const collector = new MetricsCollector({
    sampleInterval: 100,
    bufferSize: 10000,
  });

  collector.start();

  const benchmarks = {
    'Fast Operations (P99 < 5ms target)': {
      fn: async function() {
        const start = performance.now();
        await simulateLatency('fast');
        const latency = performance.now() - start;
        collector.recordOperation(latency);
      },
      iterations: 5000,
      warmup: 500,
    },

    'Normal Operations (P99 < 15ms target)': {
      fn: async function() {
        const start = performance.now();
        await simulateLatency('normal');
        const latency = performance.now() - start;
        collector.recordOperation(latency);
      },
      iterations: 5000,
      warmup: 500,
    },

    'Slow Operations (P99 < 50ms target)': {
      fn: async function() {
        const start = performance.now();
        await simulateLatency('slow');
        const latency = performance.now() - start;
        collector.recordOperation(latency);
      },
      iterations: 2000,
      warmup: 200,
    },

    'Variable Latency (outlier detection)': {
      fn: async function() {
        const start = performance.now();
        await simulateLatency('variable');
        const latency = performance.now() - start;
        collector.recordOperation(latency);
      },
      iterations: 5000,
      warmup: 500,
    },

    'Under Load (with contention)': {
      fn: async function() {
        const start = performance.now();
        await Promise.all([
          simulateLatency('normal'),
          simulateLatency('normal'),
          simulateLatency('normal'),
        ]);
        const latency = (performance.now() - start) / 3;
        collector.recordOperation(latency);
      },
      iterations: 2000,
      warmup: 200,
    },
  };

  const suiteRunner = suite('Latency Benchmarks', benchmarks);
  const results = await suiteRunner();

  collector.stop();

  // Detailed latency analysis
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║         Latency Distribution Analysis                    ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const snapshot = collector.getCurrentSnapshot();
  if (snapshot) {
    const stats = snapshot.latency;

    console.log('Percentile Analysis:');
    console.log(`  P50  (median):  ${stats.p50.toFixed(3)} ms`);
    console.log(`  P75  (3rd Q):   ${stats.p75.toFixed(3)} ms`);
    console.log(`  P90:            ${stats.p90.toFixed(3)} ms`);
    console.log(`  P95:            ${stats.p95.toFixed(3)} ms`);
    console.log(`  P99:            ${stats.p99.toFixed(3)} ms`);
    console.log(`  P99.9:          ${stats.p999.toFixed(3)} ms`);

    console.log(`\nTail Latency:`);
    console.log(`  P99/P50 ratio:  ${(stats.p99 / stats.p50).toFixed(2)}x`);
    console.log(`  P99.9/P50 ratio: ${(stats.p999 / stats.p50).toFixed(2)}x`);

    console.log(`\nRange:`);
    console.log(`  Min:            ${stats.min.toFixed(3)} ms`);
    console.log(`  Max:            ${stats.max.toFixed(3)} ms`);
    console.log(`  Mean:           ${stats.mean.toFixed(3)} ms`);

    // Outlier detection
    const threshold = stats.p99 + (stats.p99 - stats.p50) * 3;
    console.log(`\nOutliers (> ${threshold.toFixed(2)} ms):`);
    const outliers = results.results
      .filter(r => r.latency && r.latency.max > threshold)
      .length;
    console.log(`  Count: ${outliers} benchmarks`);
  }

  // Print markdown table
  console.log('\n' + formatMarkdownTable(results));

  return {
    results,
    metrics: collector.toJSON(),
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runLatencyBenchmarks()
    .then(() => process.exit(0))
    .catch(error => {
      console.error('Benchmark failed:', error);
      process.exit(1);
    });
}
