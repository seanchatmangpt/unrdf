#!/usr/bin/env node
/**
 * @fileoverview Throughput Benchmark - Measure operations per second
 * @module benchmarks/monitoring/throughput-bench
 *
 * @description
 * Benchmarks throughput under various conditions:
 * - Baseline throughput (single agent)
 * - Parallel throughput (multiple agents)
 * - Sustained throughput (long duration)
 * - Peak throughput (maximum achievable)
 */

import { performance } from 'node:perf_hooks';
import { suite, runBenchmark, formatMarkdownTable } from '../framework.mjs';
import { MetricsCollector } from '../../src/monitoring/metrics-collector.mjs';

/**
 * Simulated operation (receipt generation)
 * @returns {Promise<void>}
 */
async function simulateOperation() {
  // Simulate CPU-bound operation
  const iterations = 100;
  let sum = 0;
  for (let i = 0; i < iterations; i++) {
    sum += Math.sqrt(i * Math.random());
  }

  // Simulate I/O delay
  await new Promise(resolve => setTimeout(resolve, Math.random() * 2));

  return sum;
}

/**
 * Run throughput benchmarks
 */
export async function runThroughputBenchmarks() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║         Throughput Benchmark Suite                       ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const collector = new MetricsCollector({
    sampleInterval: 100,
    bufferSize: 1000,
  });

  collector.start();

  const benchmarks = {
    'Baseline Throughput (Sequential)': {
      fn: async function() {
        const start = performance.now();
        await simulateOperation();
        const latency = performance.now() - start;
        collector.recordOperation(latency);
      },
      iterations: 1000,
      warmup: 100,
    },

    'Parallel Throughput (10 concurrent)': {
      fn: async function() {
        const start = performance.now();
        await Promise.all(
          Array(10).fill(0).map(() => simulateOperation())
        );
        const latency = (performance.now() - start) / 10; // Average per op
        collector.recordOperation(latency);
      },
      iterations: 500,
      warmup: 50,
    },

    'High Load (100 concurrent)': {
      fn: async function() {
        const start = performance.now();
        await Promise.all(
          Array(100).fill(0).map(() => simulateOperation())
        );
        const latency = (performance.now() - start) / 100;
        collector.recordOperation(latency);
      },
      iterations: 100,
      warmup: 10,
    },

    'Sustained Load (1000 ops)': {
      fn: async function() {
        const ops = [];
        for (let i = 0; i < 1000; i++) {
          const start = performance.now();
          ops.push(simulateOperation().then(() => {
            const latency = performance.now() - start;
            collector.recordOperation(latency);
          }));

          // Small delay to avoid overwhelming
          if (i % 100 === 0) {
            await new Promise(resolve => setTimeout(resolve, 10));
          }
        }
        await Promise.all(ops);
      },
      iterations: 1,
      warmup: 0,
    },
  };

  const suiteRunner = suite('Throughput Benchmarks', benchmarks);
  const results = await suiteRunner();

  collector.stop();

  // Add throughput analysis
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║         Throughput Analysis                               ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const snapshot = collector.getCurrentSnapshot();
  if (snapshot) {
    console.log(`Total Operations:     ${snapshot.throughput.total.toLocaleString()}`);
    console.log(`Average Throughput:   ${collector.getAverageThroughput(60000).toFixed(2)} ops/sec`);
    console.log(`Peak Throughput:      ${snapshot.throughput.opsPerSec.toFixed(2)} ops/sec`);
    console.log(`\nLatency Distribution:`);
    console.log(`  P50:  ${snapshot.latency.p50.toFixed(3)} ms`);
    console.log(`  P95:  ${snapshot.latency.p95.toFixed(3)} ms`);
    console.log(`  P99:  ${snapshot.latency.p99.toFixed(3)} ms`);
    console.log(`  P999: ${snapshot.latency.p999.toFixed(3)} ms`);
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
  runThroughputBenchmarks()
    .then(() => process.exit(0))
    .catch(error => {
      console.error('Benchmark failed:', error);
      process.exit(1);
    });
}
