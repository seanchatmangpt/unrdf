/**
 * @file Maximum Throughput Load Benchmark
 * @module @unrdf/daemon/benchmarks/load/throughput
 * @description Measures maximum operations per second for the daemon under sustained load
 * Tests throughput degradation curves and identifies optimal operating points
 */

import { randomUUID } from 'crypto';
import { OptimizedDaemon } from '../../src/daemon-optimized.mjs';
import { Daemon } from '../../src/daemon.mjs';
import { analyzeVariance, storeBenchmarkResult, detectRegression } from '../suite.mjs';

/**
 * Performance targets for throughput testing
 * @constant
 */
const THROUGHPUT_TARGETS = {
  SIMPLE_OPS_PER_SEC: 10000,
  P99_LATENCY_MS: 100,
  DEGRADATION_THRESHOLD: 0.2,
};

/**
 * Create a no-op handler for maximum throughput measurement
 * @returns {Function} Handler that returns immediately
 */
function createNoOpHandler() {
  return () => Promise.resolve({ status: 'ok' });
}

/**
 * Create a CPU-bound handler for realistic load
 * @param {number} iterations - Number of iterations for CPU work
 * @returns {Function} Handler that performs CPU-bound work
 */
function createCpuBoundHandler(iterations = 1000) {
  return () => {
    let sum = 0;
    for (let i = 0; i < iterations; i++) {
      sum += Math.sqrt(i);
    }
    return Promise.resolve({ sum });
  };
}

/**
 * Latency histogram for detailed distribution analysis
 */
class LatencyHistogram {
  constructor() {
    this.buckets = new Map();
    this.values = [];
    this.bucketBoundaries = [0.1, 0.5, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000];
  }

  record(latencyMs) {
    this.values.push(latencyMs);
    const bucket = this.bucketBoundaries.find(b => latencyMs <= b) || 'overflow';
    this.buckets.set(bucket, (this.buckets.get(bucket) || 0) + 1);
  }

  getPercentile(p) {
    if (this.values.length === 0) return 0;
    const sorted = [...this.values].sort((a, b) => a - b);
    const idx = Math.floor(sorted.length * (p / 100));
    return sorted[Math.min(idx, sorted.length - 1)];
  }

  getHistogram() {
    const total = this.values.length;
    const result = {};
    for (const boundary of this.bucketBoundaries) {
      const count = this.buckets.get(boundary) || 0;
      result[`<=${boundary}ms`] = {
        count,
        percentage: total > 0 ? ((count / total) * 100).toFixed(2) : '0.00',
      };
    }
    const overflow = this.buckets.get('overflow') || 0;
    result['overflow'] = {
      count: overflow,
      percentage: total > 0 ? ((overflow / total) * 100).toFixed(2) : '0.00',
    };
    return result;
  }

  getStats() {
    if (this.values.length === 0) {
      return { count: 0, mean: 0, p50: 0, p95: 0, p99: 0, max: 0 };
    }
    const sorted = [...this.values].sort((a, b) => a - b);
    const len = sorted.length;
    return {
      count: len,
      mean: parseFloat((sorted.reduce((a, b) => a + b, 0) / len).toFixed(4)),
      p50: parseFloat(sorted[Math.floor(len * 0.5)].toFixed(4)),
      p95: parseFloat(sorted[Math.floor(len * 0.95)].toFixed(4)),
      p99: parseFloat(sorted[Math.floor(len * 0.99)].toFixed(4)),
      max: parseFloat(sorted[len - 1].toFixed(4)),
    };
  }
}

/**
 * Benchmark: Maximum throughput with no-op operations
 * Measures the theoretical maximum ops/sec the daemon can handle
 * @param {Object} options - Benchmark options
 * @param {number} [options.durationSec=5] - Test duration in seconds
 * @param {number} [options.runs=3] - Number of runs
 * @param {boolean} [options.useOptimized=true] - Use OptimizedDaemon
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkMaxThroughput(options = {}) {
  const { durationSec = 5, runs = 3, useOptimized = true } = options;
  const throughputs = [];
  const histogram = new LatencyHistogram();

  for (let run = 0; run < runs; run++) {
    const DaemonClass = useOptimized ? OptimizedDaemon : Daemon;
    const daemon = new DaemonClass({
      daemonId: randomUUID(),
      name: `throughput-bench-${run}`,
      cacheSize: 10000,
      batchSize: 500,
      logger: { info: () => {}, debug: () => {}, warn: () => {} },
    });

    await daemon.start();

    const startTime = performance.now();
    const endTime = startTime + (durationSec * 1000);
    let opsCompleted = 0;
    let opId = 0;

    while (performance.now() < endTime) {
      const currentOpId = `op-${run}-${opId++}`;
      daemon.schedule({
        id: currentOpId,
        handler: createNoOpHandler(),
      });

      const opStart = performance.now();
      await daemon.execute(currentOpId);
      const opEnd = performance.now();

      histogram.record(opEnd - opStart);
      opsCompleted++;
    }

    const actualDuration = (performance.now() - startTime) / 1000;
    const opsPerSecond = opsCompleted / actualDuration;
    throughputs.push(opsPerSecond);

    await daemon.stop();
  }

  const variance = analyzeVariance(throughputs);
  const latencyStats = histogram.getStats();
  const meetsTarget = parseFloat(variance.mean) >= THROUGHPUT_TARGETS.SIMPLE_OPS_PER_SEC;
  const p99MeetsTarget = latencyStats.p99 <= THROUGHPUT_TARGETS.P99_LATENCY_MS;

  return storeBenchmarkResult({
    name: 'max-throughput-noop',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    latency: latencyStats,
    histogram: histogram.getHistogram(),
    target: THROUGHPUT_TARGETS.SIMPLE_OPS_PER_SEC,
    meetsTarget,
    p99MeetsTarget,
    durationSec,
    runs,
    daemonType: useOptimized ? 'optimized' : 'standard',
  });
}

/**
 * Benchmark: Throughput degradation under increasing load
 * Tests how throughput degrades as operation complexity increases
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Number of runs per configuration
 * @returns {Promise<Object>} Benchmark result with degradation curve
 */
export async function benchmarkThroughputDegradation(options = {}) {
  const { runs = 3 } = options;
  const cpuWorkLevels = [0, 100, 500, 1000, 5000, 10000];
  const degradationCurve = [];

  let baselineThroughput = null;

  for (const cpuWork of cpuWorkLevels) {
    const throughputs = [];
    const histogram = new LatencyHistogram();

    for (let run = 0; run < runs; run++) {
      const daemon = new OptimizedDaemon({
        daemonId: randomUUID(),
        name: `degradation-bench-${cpuWork}`,
        cacheSize: 5000,
        logger: { info: () => {}, debug: () => {}, warn: () => {} },
      });

      await daemon.start();

      const opsToRun = 1000;
      const startTime = performance.now();

      for (let i = 0; i < opsToRun; i++) {
        const opId = `op-${run}-${i}`;
        daemon.schedule({
          id: opId,
          handler: cpuWork === 0 ? createNoOpHandler() : createCpuBoundHandler(cpuWork),
        });

        const opStart = performance.now();
        await daemon.execute(opId);
        histogram.record(performance.now() - opStart);
      }

      const duration = (performance.now() - startTime) / 1000;
      throughputs.push(opsToRun / duration);

      await daemon.stop();
    }

    const avgThroughput = throughputs.reduce((a, b) => a + b, 0) / throughputs.length;
    if (cpuWork === 0) {
      baselineThroughput = avgThroughput;
    }

    const degradationRatio = baselineThroughput
      ? avgThroughput / baselineThroughput
      : 1;

    degradationCurve.push({
      cpuWorkIterations: cpuWork,
      throughput: parseFloat(avgThroughput.toFixed(2)),
      degradationRatio: parseFloat(degradationRatio.toFixed(4)),
      latency: histogram.getStats(),
    });
  }

  return storeBenchmarkResult({
    name: 'throughput-degradation-curve',
    type: 'throughput',
    unit: 'ops/sec',
    value: baselineThroughput,
    degradationCurve,
    threshold: THROUGHPUT_TARGETS.DEGRADATION_THRESHOLD,
    runs,
  });
}

/**
 * Benchmark: Sustained throughput over time
 * Tests throughput stability over extended periods
 * @param {Object} options - Benchmark options
 * @param {number} [options.durationSec=30] - Total test duration
 * @param {number} [options.samplingIntervalMs=1000] - How often to sample throughput
 * @returns {Promise<Object>} Benchmark result with time series data
 */
export async function benchmarkSustainedThroughput(options = {}) {
  const { durationSec = 30, samplingIntervalMs = 1000 } = options;
  const timeSeries = [];
  const histogram = new LatencyHistogram();

  const daemon = new OptimizedDaemon({
    daemonId: randomUUID(),
    name: 'sustained-throughput-bench',
    cacheSize: 20000,
    batchSize: 500,
    logger: { info: () => {}, debug: () => {}, warn: () => {} },
  });

  await daemon.start();

  const startTime = performance.now();
  const endTime = startTime + (durationSec * 1000);
  let intervalStart = startTime;
  let intervalOps = 0;
  let totalOps = 0;
  let opId = 0;

  while (performance.now() < endTime) {
    const currentOpId = `op-${opId++}`;
    daemon.schedule({
      id: currentOpId,
      handler: createNoOpHandler(),
    });

    const opStart = performance.now();
    await daemon.execute(currentOpId);
    histogram.record(performance.now() - opStart);

    intervalOps++;
    totalOps++;

    const now = performance.now();
    if (now - intervalStart >= samplingIntervalMs) {
      const intervalDuration = (now - intervalStart) / 1000;
      const throughput = intervalOps / intervalDuration;
      const elapsedSec = (now - startTime) / 1000;

      timeSeries.push({
        elapsedSec: parseFloat(elapsedSec.toFixed(2)),
        throughput: parseFloat(throughput.toFixed(2)),
        cumulativeOps: totalOps,
      });

      intervalStart = now;
      intervalOps = 0;
    }
  }

  await daemon.stop();

  const throughputs = timeSeries.map(t => t.throughput);
  const variance = analyzeVariance(throughputs);
  const stability = 100 - parseFloat(variance.coefficientOfVariation);

  return storeBenchmarkResult({
    name: 'sustained-throughput',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    stability: parseFloat(stability.toFixed(2)),
    totalOperations: totalOps,
    latency: histogram.getStats(),
    timeSeries,
    durationSec,
    samplingIntervalMs,
  });
}

/**
 * Benchmark: Compare standard vs optimized daemon throughput
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=5000] - Operations to execute
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Promise<Object>} Comparison result
 */
export async function benchmarkDaemonComparison(options = {}) {
  const { operationCount = 5000, runs = 3 } = options;
  const results = { standard: [], optimized: [] };

  for (const daemonType of ['standard', 'optimized']) {
    for (let run = 0; run < runs; run++) {
      const DaemonClass = daemonType === 'optimized' ? OptimizedDaemon : Daemon;
      const daemon = new DaemonClass({
        daemonId: randomUUID(),
        name: `comparison-${daemonType}-${run}`,
        cacheSize: 10000,
        logger: { info: () => {}, debug: () => {}, warn: () => {} },
      });

      await daemon.start();

      for (let i = 0; i < operationCount; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: createNoOpHandler(),
        });
      }

      const startTime = performance.now();
      const promises = [];
      for (let i = 0; i < operationCount; i++) {
        promises.push(daemon.execute(`op-${i}`).catch(() => {}));
      }
      await Promise.all(promises);
      const duration = (performance.now() - startTime) / 1000;

      results[daemonType].push(operationCount / duration);
      await daemon.stop();
    }
  }

  const standardVariance = analyzeVariance(results.standard);
  const optimizedVariance = analyzeVariance(results.optimized);
  const speedup = parseFloat(optimizedVariance.mean) / parseFloat(standardVariance.mean);

  return storeBenchmarkResult({
    name: 'daemon-throughput-comparison',
    type: 'throughput',
    unit: 'ops/sec',
    value: parseFloat(optimizedVariance.mean),
    standard: {
      mean: parseFloat(standardVariance.mean),
      stdDev: parseFloat(standardVariance.stdDev),
    },
    optimized: {
      mean: parseFloat(optimizedVariance.mean),
      stdDev: parseFloat(optimizedVariance.stdDev),
    },
    speedup: parseFloat(speedup.toFixed(2)),
    operationCount,
    runs,
  });
}

/**
 * Run all throughput benchmarks
 * @param {Object} options - Global benchmark options
 * @returns {Promise<Object>} Combined benchmark results
 */
export async function runAllThroughputBenchmarks(options = {}) {
  console.log('Starting throughput benchmarks...\n');

  const results = {};

  console.log('[1/4] Running max throughput benchmark...');
  results.maxThroughput = await benchmarkMaxThroughput(options);
  console.log(`  Result: ${results.maxThroughput.value.toFixed(0)} ops/sec`);

  console.log('[2/4] Running throughput degradation benchmark...');
  results.degradation = await benchmarkThroughputDegradation(options);
  console.log(`  Baseline: ${results.degradation.value.toFixed(0)} ops/sec`);

  console.log('[3/4] Running sustained throughput benchmark...');
  results.sustained = await benchmarkSustainedThroughput({
    ...options,
    durationSec: options.durationSec || 10,
  });
  console.log(`  Mean: ${results.sustained.value.toFixed(0)} ops/sec, Stability: ${results.sustained.stability}%`);

  console.log('[4/4] Running daemon comparison benchmark...');
  results.comparison = await benchmarkDaemonComparison(options);
  console.log(`  Speedup: ${results.comparison.speedup}x`);

  console.log('\nThroughput benchmarks complete.');

  return {
    name: 'throughput-suite',
    timestamp: new Date().toISOString(),
    results,
    summary: {
      maxThroughput: results.maxThroughput.value,
      meetsTarget: results.maxThroughput.meetsTarget,
      p99Latency: results.maxThroughput.latency.p99,
      stability: results.sustained.stability,
      optimizedSpeedup: results.comparison.speedup,
    },
  };
}

// CLI execution
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllThroughputBenchmarks()
    .then(results => {
      console.log('\n=== THROUGHPUT BENCHMARK SUMMARY ===');
      console.log(JSON.stringify(results.summary, null, 2));
      process.exit(results.summary.meetsTarget ? 0 : 1);
    })
    .catch(err => {
      console.error('Benchmark failed:', err);
      process.exit(1);
    });
}
