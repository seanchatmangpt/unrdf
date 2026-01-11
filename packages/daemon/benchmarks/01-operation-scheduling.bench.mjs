/**
 * @file Operation Scheduling Latency Benchmark
 * @module @unrdf/daemon/benchmarks/operation-scheduling
 * @description Measures latency of scheduling operations into the daemon
 * Tracks time from schedule() call to operation enqueued event
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
 * Benchmark: Operation scheduling latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=10] - Number of benchmark runs
 * @param {number} [options.operationsPerRun=100] - Operations per run
 * @returns {Object} Benchmark result with latency metrics
 */
export async function benchmarkOperationScheduling(options = {}) {
  const { runs = 10, operationsPerRun = 100 } = options;
  const latencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({ daemonId: generateDaemonId(), name: `bench-daemon-${runIdx}` });
    await daemon.start();

    const runLatencies = [];

    for (let opIdx = 0; opIdx < operationsPerRun; opIdx++) {
      const startTime = performance.now();

      daemon.schedule({
        id: `op-${runIdx}-${opIdx}`,
        name: `Operation ${opIdx}`,
        handler: async () => {
          return { result: 'success' };
        },
      });

      const endTime = performance.now();
      runLatencies.push(endTime - startTime);
    }

    latencies.push(...runLatencies);
    await daemon.stop();
  }

  const variance = analyzeVariance(latencies);

  return storeBenchmarkResult({
    name: 'operation-scheduling-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: latencies.length,
    runs,
    operationsPerRun,
  });
}

/**
 * Benchmark: Concurrent scheduling throughput
 * @param {Object} options - Benchmark options
 * @param {number} [options.concurrentOps=1000] - Concurrent operations
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with throughput metrics
 */
export async function benchmarkSchedulingThroughput(options = {}) {
  const { concurrentOps = 1000, runs = 5 } = options;
  const throughputs = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({ daemonId: generateDaemonId(), name: `bench-tput-${runIdx}` });
    await daemon.start();

    const startTime = performance.now();

    for (let opIdx = 0; opIdx < concurrentOps; opIdx++) {
      daemon.schedule({
        id: `op-${runIdx}-${opIdx}`,
        handler: async () => ({ result: 'success' }),
      });
    }

    const endTime = performance.now();
    const durationMs = endTime - startTime;
    const opsPerSecond = (concurrentOps / (durationMs / 1000));

    throughputs.push(opsPerSecond);
    await daemon.stop();
  }

  const variance = analyzeVariance(throughputs);

  return storeBenchmarkResult({
    name: 'operation-scheduling-throughput',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: throughputs.length,
    runs,
    concurrentOps,
  });
}

/**
 * Benchmark: Operation queue size impact
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with queue impact metrics
 */
export async function benchmarkQueueImpact(options = {}) {
  const { runs = 5 } = options;
  const results = [];

  const queueSizes = [10, 100, 1000, 5000];

  for (const queueSize of queueSizes) {
    const latencies = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemon = new Daemon({ daemonId: generateDaemonId(), name: `bench-q-${queueSize}` });
      await daemon.start();

      // Pre-fill queue
      for (let i = 0; i < queueSize; i++) {
        daemon.schedule({
          id: `prefill-${i}`,
          handler: async () => ({ result: 'success' }),
        });
      }

      // Measure scheduling latency with full queue
      const startTime = performance.now();
      for (let i = 0; i < 100; i++) {
        daemon.schedule({
          id: `measure-${i}`,
          handler: async () => ({ result: 'success' }),
        });
      }
      const endTime = performance.now();

      const avgLatency = (endTime - startTime) / 100;
      latencies.push(avgLatency);

      await daemon.stop();
    }

    const variance = analyzeVariance(latencies);
    results.push({
      queueSize,
      mean: parseFloat(variance.mean),
      variance: parseFloat(variance.coefficientOfVariation),
    });
  }

  return storeBenchmarkResult({
    name: 'operation-queue-impact',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(analyzeVariance(results.map(r => r.mean)).mean),
    queueSizeImpact: results,
    runs,
  });
}
