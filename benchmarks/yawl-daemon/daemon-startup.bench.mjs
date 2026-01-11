/**
 * @file Daemon Startup Latency Benchmark
 * @module benchmarks/yawl-daemon/daemon-startup
 * @description Measures daemon startup time from initialization to ready state
 *
 * Performance Target: <500ms startup latency (P95)
 *
 * Metrics:
 * - P50, P95, P99 startup latency
 * - Initialization phases (config, listeners, ready)
 * - Memory footprint at startup
 */

import { Daemon } from '../../packages/daemon/src/daemon.mjs';
import { randomUUID } from 'crypto';

/**
 * Calculate percentile from sorted array
 * @param {number[]} values - Sorted array of values
 * @param {number} percentile - Percentile (0-100)
 * @returns {number} Percentile value
 */
function getPercentile(values, percentile) {
  if (values.length === 0) return 0;
  const index = Math.ceil((percentile / 100) * values.length) - 1;
  return values[Math.max(0, index)];
}

/**
 * Get current memory usage in MB
 * @returns {number} Memory usage in MB
 */
function getMemoryUsageMB() {
  const usage = process.memoryUsage();
  return usage.heapUsed / 1024 / 1024;
}

/**
 * Benchmark: Cold start daemon initialization
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=50] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkColdStartup(options = {}) {
  const { runs = 50 } = options;
  const startupTimes = [];
  const memoryFootprints = [];

  for (let i = 0; i < runs; i++) {
    const memBefore = getMemoryUsageMB();
    const startTime = performance.now();

    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `startup-bench-${i}`,
      maxConcurrent: 5,
    });

    await daemon.start();
    const startupLatency = performance.now() - startTime;
    const memAfter = getMemoryUsageMB();

    startupTimes.push(startupLatency);
    memoryFootprints.push(memAfter - memBefore);

    await daemon.stop();

    // Allow GC between runs
    if (i % 10 === 0 && global.gc) {
      global.gc();
      await new Promise(resolve => setTimeout(resolve, 50));
    }
  }

  const sortedTimes = startupTimes.sort((a, b) => a - b);
  const mean = startupTimes.reduce((sum, v) => sum + v, 0) / startupTimes.length;
  const p50 = getPercentile(sortedTimes, 50);
  const p95 = getPercentile(sortedTimes, 95);
  const p99 = getPercentile(sortedTimes, 99);
  const avgMemory = memoryFootprints.reduce((sum, v) => sum + v, 0) / memoryFootprints.length;

  return {
    name: 'daemon-cold-startup',
    runs,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...startupTimes),
      max: Math.max(...startupTimes),
    },
    memory: {
      avgMB: avgMemory,
      maxMB: Math.max(...memoryFootprints),
    },
    passed: p95 < 500, // Target: <500ms P95
    target: '500ms',
    unit: 'ms',
  };
}

/**
 * Benchmark: Warm start with existing configuration
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=30] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkWarmStartup(options = {}) {
  const { runs = 30 } = options;
  const startupTimes = [];

  // Pre-warm with one daemon instance
  const warmupDaemon = new Daemon({ daemonId: randomUUID(), name: 'warmup' });
  await warmupDaemon.start();
  await warmupDaemon.stop();

  for (let i = 0; i < runs; i++) {
    const startTime = performance.now();

    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `warm-bench-${i}`,
      maxConcurrent: 5,
    });

    await daemon.start();
    const startupLatency = performance.now() - startTime;
    startupTimes.push(startupLatency);

    await daemon.stop();
  }

  const sortedTimes = startupTimes.sort((a, b) => a - b);
  const mean = startupTimes.reduce((sum, v) => sum + v, 0) / startupTimes.length;
  const p50 = getPercentile(sortedTimes, 50);
  const p95 = getPercentile(sortedTimes, 95);
  const p99 = getPercentile(sortedTimes, 99);

  return {
    name: 'daemon-warm-startup',
    runs,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...startupTimes),
      max: Math.max(...startupTimes),
    },
    passed: p95 < 300, // Warm starts should be faster
    target: '300ms',
    unit: 'ms',
  };
}

/**
 * Benchmark: Startup with pre-scheduled operations
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=100] - Operations to pre-schedule
 * @param {number} [options.runs=10] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkStartupWithLoad(options = {}) {
  const { operationCount = 100, runs = 10 } = options;
  const startupTimes = [];

  for (let i = 0; i < runs; i++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `load-bench-${i}`,
      maxConcurrent: 10,
    });

    // Schedule operations before start
    for (let j = 0; j < operationCount; j++) {
      daemon.schedule({
        id: `op-${j}`,
        name: `Operation ${j}`,
        handler: async () => ({ result: j }),
      });
    }

    const startTime = performance.now();
    await daemon.start();
    const startupLatency = performance.now() - startTime;
    startupTimes.push(startupLatency);

    await daemon.stop();
  }

  const sortedTimes = startupTimes.sort((a, b) => a - b);
  const mean = startupTimes.reduce((sum, v) => sum + v, 0) / startupTimes.length;
  const p50 = getPercentile(sortedTimes, 50);
  const p95 = getPercentile(sortedTimes, 95);
  const p99 = getPercentile(sortedTimes, 99);

  return {
    name: 'daemon-startup-with-load',
    runs,
    operationCount,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...startupTimes),
      max: Math.max(...startupTimes),
    },
    passed: p95 < 1000, // Higher tolerance with load
    target: '1000ms',
    unit: 'ms',
  };
}

/**
 * Run all startup benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runStartupBenchmarks() {
  console.log('Running Daemon Startup Benchmarks...\n');

  const results = {
    coldStartup: await benchmarkColdStartup(),
    warmStartup: await benchmarkWarmStartup(),
    startupWithLoad: await benchmarkStartupWithLoad(),
  };

  const allPassed = Object.values(results).every(r => r.passed);

  return {
    name: 'daemon-startup-suite',
    timestamp: new Date().toISOString(),
    results,
    summary: {
      total: Object.keys(results).length,
      passed: Object.values(results).filter(r => r.passed).length,
      failed: Object.values(results).filter(r => !r.passed).length,
    },
    passed: allPassed,
  };
}
