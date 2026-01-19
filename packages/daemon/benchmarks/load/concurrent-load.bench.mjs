/**
 * @file Concurrent Load Benchmark
 * @module @unrdf/daemon/benchmarks/load/concurrent-load
 * @description Measures daemon behavior under concurrent load with 100, 1000, 10000 operations
 * Tests backpressure handling, queue depth impact, and latency distributions
 */

import { randomUUID } from 'crypto';
import { OptimizedDaemon } from '../../src/daemon-optimized.mjs';
import { analyzeVariance, storeBenchmarkResult } from '../suite.mjs';

/**
 * Performance targets for concurrent load testing
 * @constant
 */
const CONCURRENCY_TARGETS = {
  P99_LATENCY_MS: 100,
  MIN_THROUGHPUT_10K: 5000,
  MAX_QUEUE_DEPTH: 50000,
  BACKPRESSURE_THRESHOLD: 0.8,
};

/**
 * Semaphore for concurrency control
 */
class Semaphore {
  constructor(limit) {
    this.limit = limit;
    this.active = 0;
    this.waiting = [];
  }

  async acquire() {
    if (this.active < this.limit) {
      this.active++;
      return;
    }
    await new Promise(resolve => this.waiting.push(resolve));
    this.active++;
  }

  release() {
    this.active--;
    if (this.waiting.length > 0) {
      const next = this.waiting.shift();
      next();
    }
  }
}

/**
 * Latency histogram with percentile calculation
 */
class LatencyHistogram {
  constructor() {
    this.values = [];
    this.buckets = new Map();
    this.boundaries = [0.1, 0.5, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 5000];
  }

  record(latencyMs) {
    this.values.push(latencyMs);
    const bucket = this.boundaries.find(b => latencyMs <= b) || 'overflow';
    this.buckets.set(bucket, (this.buckets.get(bucket) || 0) + 1);
  }

  getPercentiles() {
    if (this.values.length === 0) {
      return { p50: 0, p75: 0, p90: 0, p95: 0, p99: 0, p999: 0 };
    }
    const sorted = [...this.values].sort((a, b) => a - b);
    const len = sorted.length;
    return {
      p50: sorted[Math.floor(len * 0.5)],
      p75: sorted[Math.floor(len * 0.75)],
      p90: sorted[Math.floor(len * 0.9)],
      p95: sorted[Math.floor(len * 0.95)],
      p99: sorted[Math.floor(len * 0.99)],
      p999: sorted[Math.floor(len * 0.999)],
    };
  }

  getHistogram() {
    const total = this.values.length;
    const histogram = {};
    for (const b of this.boundaries) {
      const count = this.buckets.get(b) || 0;
      histogram[`<=${b}ms`] = {
        count,
        percentage: total > 0 ? parseFloat(((count / total) * 100).toFixed(2)) : 0,
        cumulative: 0,
      };
    }
    const overflow = this.buckets.get('overflow') || 0;
    histogram['overflow'] = { count: overflow, percentage: parseFloat(((overflow / total) * 100).toFixed(2)) };

    let cumulative = 0;
    for (const key of Object.keys(histogram)) {
      cumulative += histogram[key].count;
      histogram[key].cumulative = total > 0 ? parseFloat(((cumulative / total) * 100).toFixed(2)) : 0;
    }
    return histogram;
  }

  getStats() {
    if (this.values.length === 0) {
      return { count: 0, mean: 0, min: 0, max: 0, stdDev: 0 };
    }
    const len = this.values.length;
    const mean = this.values.reduce((a, b) => a + b, 0) / len;
    const variance = this.values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / len;
    const sorted = [...this.values].sort((a, b) => a - b);
    return {
      count: len,
      mean: parseFloat(mean.toFixed(4)),
      min: parseFloat(sorted[0].toFixed(4)),
      max: parseFloat(sorted[len - 1].toFixed(4)),
      stdDev: parseFloat(Math.sqrt(variance).toFixed(4)),
    };
  }
}

/**
 * Create async operation with configurable delay
 * @param {number} delayMs - Simulated operation delay
 * @returns {Function} Handler function
 */
function createAsyncHandler(delayMs = 1) {
  return () => new Promise(resolve => {
    if (delayMs <= 0) {
      resolve({ status: 'ok', timestamp: Date.now() });
    } else {
      setTimeout(() => resolve({ status: 'ok', timestamp: Date.now() }), delayMs);
    }
  });
}

/**
 * Benchmark: Fixed concurrency levels (100, 1000, 10000)
 * Tests behavior at specific concurrency points
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationDelayMs=1] - Per-operation delay
 * @param {number} [options.runs=3] - Runs per concurrency level
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkConcurrencyLevels(options = {}) {
  const { operationDelayMs = 1, runs = 3 } = options;
  const concurrencyLevels = [100, 1000, 10000];
  const results = [];

  for (const concurrency of concurrencyLevels) {
    const runResults = [];
    const aggregateHistogram = new LatencyHistogram();

    for (let run = 0; run < runs; run++) {
      const daemon = new OptimizedDaemon({
        daemonId: randomUUID(),
        name: `concurrency-${concurrency}-run-${run}`,
        cacheSize: Math.max(concurrency * 2, 5000),
        batchSize: Math.min(concurrency / 10, 500),
        logger: { info: () => {}, debug: () => {}, warn: () => {} },
      });

      await daemon.start();

      for (let i = 0; i < concurrency; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: createAsyncHandler(operationDelayMs),
        });
      }

      const latencies = [];
      const startTime = performance.now();

      const executePromises = [];
      for (let i = 0; i < concurrency; i++) {
        const opStart = performance.now();
        executePromises.push(
          daemon.execute(`op-${i}`)
            .then(() => {
              const latency = performance.now() - opStart;
              latencies.push(latency);
              aggregateHistogram.record(latency);
            })
            .catch(() => {})
        );
      }

      await Promise.all(executePromises);
      const totalDuration = (performance.now() - startTime) / 1000;
      const throughput = concurrency / totalDuration;

      const sorted = [...latencies].sort((a, b) => a - b);
      const len = sorted.length;

      runResults.push({
        throughput,
        p50: sorted[Math.floor(len * 0.5)],
        p99: sorted[Math.floor(len * 0.99)],
        maxLatency: sorted[len - 1],
      });

      await daemon.stop();
    }

    const avgThroughput = runResults.reduce((s, r) => s + r.throughput, 0) / runs;
    const avgP99 = runResults.reduce((s, r) => s + r.p99, 0) / runs;
    const meetsP99Target = avgP99 <= CONCURRENCY_TARGETS.P99_LATENCY_MS;

    results.push({
      concurrency,
      throughput: parseFloat(avgThroughput.toFixed(2)),
      latencyPercentiles: aggregateHistogram.getPercentiles(),
      histogram: aggregateHistogram.getHistogram(),
      stats: aggregateHistogram.getStats(),
      meetsP99Target,
      runs,
    });
  }

  const meets10kTarget = results.find(r => r.concurrency === 10000)?.throughput >= CONCURRENCY_TARGETS.MIN_THROUGHPUT_10K;

  return storeBenchmarkResult({
    name: 'concurrency-levels',
    type: 'concurrency',
    unit: 'ops/sec',
    value: results.find(r => r.concurrency === 10000)?.throughput || 0,
    results,
    target: CONCURRENCY_TARGETS.P99_LATENCY_MS,
    meets10kTarget,
    operationDelayMs,
  });
}

/**
 * Benchmark: Queue depth impact on latency
 * Tests how queue depth affects operation latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkQueueDepthImpact(options = {}) {
  const { runs = 3 } = options;
  const queueDepths = [10, 100, 500, 1000, 5000, 10000];
  const results = [];

  for (const queueDepth of queueDepths) {
    const runResults = [];

    for (let run = 0; run < runs; run++) {
      const daemon = new OptimizedDaemon({
        daemonId: randomUUID(),
        name: `queue-depth-${queueDepth}`,
        cacheSize: queueDepth * 2,
        batchSize: Math.min(queueDepth / 5, 500),
        logger: { info: () => {}, debug: () => {}, warn: () => {} },
      });

      await daemon.start();

      for (let i = 0; i < queueDepth; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: createAsyncHandler(0),
        });
      }

      const health = daemon.getHealth();
      const queuedBefore = health.queuedOperations;

      const latencies = [];
      const startTime = performance.now();

      const semaphore = new Semaphore(100);
      const executePromises = [];

      for (let i = 0; i < queueDepth; i++) {
        const opStart = performance.now();
        executePromises.push(
          semaphore.acquire().then(async () => {
            try {
              await daemon.execute(`op-${i}`);
              latencies.push(performance.now() - opStart);
            } finally {
              semaphore.release();
            }
          }).catch(() => {})
        );
      }

      await Promise.all(executePromises);
      const totalDuration = (performance.now() - startTime) / 1000;

      const sorted = [...latencies].sort((a, b) => a - b);
      const len = sorted.length;

      runResults.push({
        throughput: queueDepth / totalDuration,
        avgLatency: latencies.reduce((a, b) => a + b, 0) / len,
        p50: sorted[Math.floor(len * 0.5)],
        p99: sorted[Math.floor(len * 0.99)],
        queuedBefore,
      });

      await daemon.stop();
    }

    const avgThroughput = runResults.reduce((s, r) => s + r.throughput, 0) / runs;
    const avgP99 = runResults.reduce((s, r) => s + r.p99, 0) / runs;

    results.push({
      queueDepth,
      throughput: parseFloat(avgThroughput.toFixed(2)),
      avgLatency: parseFloat((runResults.reduce((s, r) => s + r.avgLatency, 0) / runs).toFixed(4)),
      p99Latency: parseFloat(avgP99.toFixed(4)),
      meetsTarget: avgP99 <= CONCURRENCY_TARGETS.P99_LATENCY_MS,
    });
  }

  return storeBenchmarkResult({
    name: 'queue-depth-impact',
    type: 'latency',
    unit: 'ms',
    value: results.find(r => r.queueDepth === 10000)?.p99Latency || 0,
    results,
    maxQueueDepth: CONCURRENCY_TARGETS.MAX_QUEUE_DEPTH,
    runs,
  });
}

/**
 * Benchmark: Backpressure handling
 * Tests daemon behavior when load exceeds capacity
 * @param {Object} options - Benchmark options
 * @param {number} [options.sustainedLoadDurationMs=5000] - Duration of sustained load
 * @param {number} [options.targetOpsPerSec=20000] - Target operations per second
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkBackpressure(options = {}) {
  const { sustainedLoadDurationMs = 5000, targetOpsPerSec = 20000 } = options;
  const histogram = new LatencyHistogram();
  const timeSeries = [];

  const daemon = new OptimizedDaemon({
    daemonId: randomUUID(),
    name: 'backpressure-test',
    cacheSize: 50000,
    batchSize: 1000,
    logger: { info: () => {}, debug: () => {}, warn: () => {} },
  });

  await daemon.start();

  const startTime = performance.now();
  const endTime = startTime + sustainedLoadDurationMs;
  const targetIntervalMs = 1000 / targetOpsPerSec;

  let opId = 0;
  let completedOps = 0;
  let droppedOps = 0;
  let lastSample = startTime;
  let intervalOps = 0;

  const activePromises = new Set();
  const maxConcurrent = 5000;

  while (performance.now() < endTime) {
    if (activePromises.size >= maxConcurrent) {
      droppedOps++;
      await new Promise(resolve => setTimeout(resolve, 1));
      continue;
    }

    const currentOpId = `op-${opId++}`;
    daemon.schedule({
      id: currentOpId,
      handler: createAsyncHandler(1),
    });

    const opStart = performance.now();
    const promise = daemon.execute(currentOpId)
      .then(() => {
        const latency = performance.now() - opStart;
        histogram.record(latency);
        completedOps++;
        intervalOps++;
      })
      .catch(() => {})
      .finally(() => activePromises.delete(promise));

    activePromises.add(promise);

    const now = performance.now();
    if (now - lastSample >= 500) {
      const intervalDuration = (now - lastSample) / 1000;
      timeSeries.push({
        elapsedMs: now - startTime,
        throughput: parseFloat((intervalOps / intervalDuration).toFixed(2)),
        activeOps: activePromises.size,
        queueDepth: daemon.getHealth().queuedOperations,
      });
      lastSample = now;
      intervalOps = 0;
    }

    if (targetIntervalMs > 0.5) {
      await new Promise(resolve => setTimeout(resolve, targetIntervalMs));
    }
  }

  await Promise.all(activePromises);
  await daemon.stop();

  const totalDuration = (performance.now() - startTime) / 1000;
  const achievedThroughput = completedOps / totalDuration;
  const backpressureRatio = droppedOps / (completedOps + droppedOps);
  const latencyStats = histogram.getStats();

  return storeBenchmarkResult({
    name: 'backpressure-handling',
    type: 'backpressure',
    unit: 'ops/sec',
    value: parseFloat(achievedThroughput.toFixed(2)),
    targetOpsPerSec,
    achievedOpsPerSec: parseFloat(achievedThroughput.toFixed(2)),
    completedOperations: completedOps,
    droppedOperations: droppedOps,
    backpressureRatio: parseFloat(backpressureRatio.toFixed(4)),
    backpressureTriggered: backpressureRatio > CONCURRENCY_TARGETS.BACKPRESSURE_THRESHOLD,
    latency: latencyStats,
    latencyPercentiles: histogram.getPercentiles(),
    histogram: histogram.getHistogram(),
    timeSeries,
    durationMs: sustainedLoadDurationMs,
  });
}

/**
 * Benchmark: Concurrent operation scaling
 * Tests how performance scales with increasing concurrency
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Number of runs per level
 * @returns {Promise<Object>} Benchmark result with scaling analysis
 */
export async function benchmarkConcurrencyScaling(options = {}) {
  const { runs = 3 } = options;
  const concurrencyLimits = [1, 5, 10, 25, 50, 100, 250, 500];
  const operationsPerTest = 1000;
  const results = [];

  for (const limit of concurrencyLimits) {
    const throughputs = [];

    for (let run = 0; run < runs; run++) {
      const daemon = new OptimizedDaemon({
        daemonId: randomUUID(),
        name: `scaling-${limit}`,
        cacheSize: 5000,
        logger: { info: () => {}, debug: () => {}, warn: () => {} },
      });

      await daemon.start();

      for (let i = 0; i < operationsPerTest; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: createAsyncHandler(5),
        });
      }

      const semaphore = new Semaphore(limit);
      const startTime = performance.now();

      const promises = [];
      for (let i = 0; i < operationsPerTest; i++) {
        promises.push(
          semaphore.acquire().then(async () => {
            try {
              await daemon.execute(`op-${i}`);
            } finally {
              semaphore.release();
            }
          }).catch(() => {})
        );
      }

      await Promise.all(promises);
      const duration = (performance.now() - startTime) / 1000;
      throughputs.push(operationsPerTest / duration);

      await daemon.stop();
    }

    const avgThroughput = throughputs.reduce((a, b) => a + b, 0) / runs;
    results.push({
      concurrencyLimit: limit,
      throughput: parseFloat(avgThroughput.toFixed(2)),
    });
  }

  const baselineThroughput = results[0].throughput;
  const scalingFactors = results.map(r => ({
    ...r,
    scalingFactor: parseFloat((r.throughput / baselineThroughput).toFixed(2)),
    efficiency: parseFloat(((r.throughput / baselineThroughput) / r.concurrencyLimit * 100).toFixed(2)),
  }));

  const optimalConcurrency = scalingFactors.reduce((best, curr) =>
    curr.efficiency > best.efficiency ? curr : best
  );

  return storeBenchmarkResult({
    name: 'concurrency-scaling',
    type: 'scaling',
    unit: 'ops/sec',
    value: scalingFactors.find(s => s.concurrencyLimit === 100)?.throughput || 0,
    scalingCurve: scalingFactors,
    optimalConcurrency: optimalConcurrency.concurrencyLimit,
    maxThroughput: Math.max(...scalingFactors.map(s => s.throughput)),
    operationsPerTest,
    runs,
  });
}

/**
 * Run all concurrent load benchmarks
 * @param {Object} options - Global benchmark options
 * @returns {Promise<Object>} Combined benchmark results
 */
export async function runAllConcurrentLoadBenchmarks(options = {}) {
  console.log('Starting concurrent load benchmarks...\n');

  const results = {};

  console.log('[1/4] Running concurrency levels benchmark (100, 1000, 10000)...');
  results.concurrencyLevels = await benchmarkConcurrencyLevels(options);
  console.log(`  10K ops throughput: ${results.concurrencyLevels.value.toFixed(0)} ops/sec`);

  console.log('[2/4] Running queue depth impact benchmark...');
  results.queueDepth = await benchmarkQueueDepthImpact(options);
  console.log(`  10K queue P99: ${results.queueDepth.value.toFixed(2)}ms`);

  console.log('[3/4] Running backpressure benchmark...');
  results.backpressure = await benchmarkBackpressure(options);
  console.log(`  Achieved: ${results.backpressure.achievedOpsPerSec.toFixed(0)} ops/sec`);

  console.log('[4/4] Running concurrency scaling benchmark...');
  results.scaling = await benchmarkConcurrencyScaling(options);
  console.log(`  Optimal concurrency: ${results.scaling.optimalConcurrency}`);

  console.log('\nConcurrent load benchmarks complete.');

  const p99At10k = results.concurrencyLevels.results.find(r => r.concurrency === 10000)?.latencyPercentiles?.p99 || 0;

  return {
    name: 'concurrent-load-suite',
    timestamp: new Date().toISOString(),
    results,
    summary: {
      throughputAt10k: results.concurrencyLevels.value,
      p99LatencyAt10k: p99At10k,
      meetsP99Target: p99At10k <= CONCURRENCY_TARGETS.P99_LATENCY_MS,
      backpressureRatio: results.backpressure.backpressureRatio,
      optimalConcurrency: results.scaling.optimalConcurrency,
    },
  };
}

// CLI execution
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllConcurrentLoadBenchmarks()
    .then(results => {
      console.log('\n=== CONCURRENT LOAD BENCHMARK SUMMARY ===');
      console.log(JSON.stringify(results.summary, null, 2));
      process.exit(results.summary.meetsP99Target ? 0 : 1);
    })
    .catch(err => {
      console.error('Benchmark failed:', err);
      process.exit(1);
    });
}
