/**
 * @file Concurrent Operation Throughput Benchmark
 * @module @unrdf/daemon/benchmarks/concurrent-throughput
 * @description Measures throughput of concurrent operation execution
 * Tests how many operations can be executed per second under concurrent load
 */

import { Daemon } from '../src/daemon.mjs';
import { analyzeVariance, storeBenchmarkResult } from './suite.mjs';
import { randomUUID } from 'crypto';

/**
 * Generate a valid UUID for daemon ID
 * @returns {string} Valid UUID
 */
function generateDaemonId() {
  return randomUUID();
}

/**
 * Simple async operation for benchmarking
 * @param {number} duration - Operation duration in milliseconds
 * @returns {Promise<Object>} Operation result
 */
async function simpleAsyncOp(duration = 1) {
  return new Promise(resolve => {
    setTimeout(() => resolve({ success: true }), duration);
  });
}

/**
 * Benchmark: Basic concurrent execution throughput
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=100] - Operations to execute
 * @param {number} [options.runs=5] - Number of runs
 * @param {number} [options.opDuration=1] - Operation duration in ms
 * @returns {Object} Benchmark result with throughput metrics
 */
export async function benchmarkConcurrentThroughput(options = {}) {
  const { operationCount = 100, runs = 5, opDuration = 1 } = options;
  const throughputs = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `bench-concurrent-${runIdx}`,
    });
    await daemon.start();

    const operationIds = [];
    for (let i = 0; i < operationCount; i++) {
      const opId = `op-${runIdx}-${i}`;
      operationIds.push(opId);
      daemon.schedule({
        id: opId,
        handler: () => simpleAsyncOp(opDuration),
      });
    }

    const startTime = performance.now();

    const executePromises = operationIds.map(opId =>
      daemon.execute(opId).catch(() => {
        // Ignore execution errors for benchmark
      })
    );

    await Promise.all(executePromises);

    const endTime = performance.now();
    const durationSecs = (endTime - startTime) / 1000;
    const opsPerSecond = operationCount / durationSecs;

    throughputs.push(opsPerSecond);
    await daemon.stop();
  }

  const variance = analyzeVariance(throughputs);

  return storeBenchmarkResult({
    name: 'concurrent-execution-throughput',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: throughputs.length,
    operationCount,
    opDuration,
    runs,
  });
}

/**
 * Benchmark: Execution latency percentiles
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=50] - Operations to execute
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with latency percentiles
 */
export async function benchmarkExecutionLatencyPercentiles(options = {}) {
  const { operationCount = 50, runs = 5 } = options;
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `bench-latency-${runIdx}`,
    });
    await daemon.start();

    const operationIds = [];
    for (let i = 0; i < operationCount; i++) {
      const opId = `op-${runIdx}-${i}`;
      operationIds.push(opId);
      daemon.schedule({
        id: opId,
        handler: () => simpleAsyncOp(1),
      });
    }

    for (const opId of operationIds) {
      const startTime = performance.now();
      await daemon.execute(opId).catch(() => {});
      const endTime = performance.now();
      allLatencies.push(endTime - startTime);
    }

    await daemon.stop();
  }

  const sorted = allLatencies.sort((a, b) => a - b);
  const p50 = sorted[Math.floor(sorted.length * 0.5)];
  const p95 = sorted[Math.floor(sorted.length * 0.95)];
  const p99 = sorted[Math.floor(sorted.length * 0.99)];
  const variance = analyzeVariance(allLatencies);

  return storeBenchmarkResult({
    name: 'execution-latency-percentiles',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    p50: p50.toFixed(4),
    p95: p95.toFixed(4),
    p99: p99.toFixed(4),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: allLatencies.length,
    runs,
  });
}

/**
 * Benchmark: Concurrency level impact
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with concurrency impact
 */
export async function benchmarkConcurrencyImpact(options = {}) {
  const { runs = 3 } = options;
  const results = [];

  const concurrencyLevels = [1, 5, 10, 50, 100];

  for (const concurrency of concurrencyLevels) {
    const throughputs = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemon = new Daemon({
        daemonId: generateDaemonId(),
        name: `bench-conc-${concurrency}`,
      });
      await daemon.start();

      const operationIds = [];
      const totalOps = concurrency * 10;

      for (let i = 0; i < totalOps; i++) {
        const opId = `op-${runIdx}-${i}`;
        operationIds.push(opId);
        daemon.schedule({
          id: opId,
          handler: () => simpleAsyncOp(5),
        });
      }

      const startTime = performance.now();

      // Execute with concurrency limit
      for (let i = 0; i < operationIds.length; i += concurrency) {
        const batch = operationIds.slice(i, i + concurrency);
        await Promise.all(
          batch.map(opId => daemon.execute(opId).catch(() => {}))
        );
      }

      const endTime = performance.now();
      const durationSecs = (endTime - startTime) / 1000;
      const opsPerSecond = totalOps / durationSecs;

      throughputs.push(opsPerSecond);
      await daemon.stop();
    }

    const variance = analyzeVariance(throughputs);
    results.push({
      concurrency,
      mean: parseFloat(variance.mean),
      variance: parseFloat(variance.coefficientOfVariation),
    });
  }

  return storeBenchmarkResult({
    name: 'concurrency-impact',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(analyzeVariance(results.map(r => r.mean)).mean),
    concurrencyImpact: results,
    runs,
  });
}
