/**
 * @file Burst Traffic Load Benchmark
 * @module @unrdf/daemon/benchmarks/load/burst-traffic
 * @description Tests daemon response to traffic bursts and sudden load spikes
 * Measures recovery time, graceful degradation, and latency under burst conditions
 */

import { randomUUID } from 'crypto';
import { OptimizedDaemon } from '../../src/daemon-optimized.mjs';
import { analyzeVariance, storeBenchmarkResult } from '../suite.mjs';

/**
 * Burst traffic targets
 * @constant
 */
const BURST_TARGETS = {
  RECOVERY_TIME_MS: 1000,
  P99_LATENCY_MS: 100,
  GRACEFUL_DEGRADATION_THRESHOLD: 0.5,
  MAX_LATENCY_SPIKE_RATIO: 5,
};

/**
 * Latency histogram for burst analysis
 */
class BurstLatencyHistogram {
  constructor() {
    this.values = [];
    this.timestamps = [];
    this.buckets = new Map();
    this.boundaries = [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000];
  }

  record(latencyMs, timestamp) {
    this.values.push(latencyMs);
    this.timestamps.push(timestamp);
    const bucket = this.boundaries.find(b => latencyMs <= b) || 'overflow';
    this.buckets.set(bucket, (this.buckets.get(bucket) || 0) + 1);
  }

  getPercentiles() {
    if (this.values.length === 0) {
      return { p50: 0, p75: 0, p90: 0, p95: 0, p99: 0, p999: 0, max: 0 };
    }
    const sorted = [...this.values].sort((a, b) => a - b);
    const len = sorted.length;
    return {
      p50: parseFloat(sorted[Math.floor(len * 0.5)].toFixed(4)),
      p75: parseFloat(sorted[Math.floor(len * 0.75)].toFixed(4)),
      p90: parseFloat(sorted[Math.floor(len * 0.9)].toFixed(4)),
      p95: parseFloat(sorted[Math.floor(len * 0.95)].toFixed(4)),
      p99: parseFloat(sorted[Math.floor(len * 0.99)].toFixed(4)),
      p999: parseFloat(sorted[Math.floor(len * 0.999)].toFixed(4)),
      max: parseFloat(sorted[len - 1].toFixed(4)),
    };
  }

  getHistogram() {
    const total = this.values.length;
    const histogram = {};
    let cumulative = 0;
    for (const b of this.boundaries) {
      const count = this.buckets.get(b) || 0;
      cumulative += count;
      histogram[`<=${b}ms`] = {
        count,
        percentage: total > 0 ? parseFloat(((count / total) * 100).toFixed(2)) : 0,
        cumulative: total > 0 ? parseFloat(((cumulative / total) * 100).toFixed(2)) : 0,
      };
    }
    const overflow = this.buckets.get('overflow') || 0;
    histogram['overflow'] = {
      count: overflow,
      percentage: total > 0 ? parseFloat(((overflow / total) * 100).toFixed(2)) : 0,
    };
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

  getTimeSeriesStats(windowMs = 100) {
    if (this.values.length === 0) return [];
    const startTime = this.timestamps[0];
    const windows = [];
    let windowStart = startTime;
    let windowValues = [];

    for (let i = 0; i < this.values.length; i++) {
      if (this.timestamps[i] - windowStart >= windowMs) {
        if (windowValues.length > 0) {
          const sorted = [...windowValues].sort((a, b) => a - b);
          windows.push({
            windowStart: windowStart - startTime,
            count: windowValues.length,
            mean: windowValues.reduce((a, b) => a + b, 0) / windowValues.length,
            p99: sorted[Math.floor(sorted.length * 0.99)],
          });
        }
        windowStart = this.timestamps[i];
        windowValues = [];
      }
      windowValues.push(this.values[i]);
    }

    if (windowValues.length > 0) {
      const sorted = [...windowValues].sort((a, b) => a - b);
      windows.push({
        windowStart: windowStart - startTime,
        count: windowValues.length,
        mean: windowValues.reduce((a, b) => a + b, 0) / windowValues.length,
        p99: sorted[Math.floor(sorted.length * 0.99)],
      });
    }

    return windows;
  }
}

/**
 * Create no-op handler
 * @returns {Function} Handler function
 */
function createNoOpHandler() {
  return () => Promise.resolve({ status: 'ok', timestamp: Date.now() });
}

/**
 * Create async handler with delay
 * @param {number} delayMs - Delay in milliseconds
 * @returns {Function} Handler function
 */
function createAsyncHandler(delayMs) {
  return () => new Promise(resolve =>
    setTimeout(() => resolve({ status: 'ok' }), delayMs)
  );
}

/**
 * Benchmark: Single burst response
 * Tests daemon response to a single traffic burst
 * @param {Object} options - Benchmark options
 * @param {number} [options.burstSize=1000] - Number of operations in burst
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkSingleBurst(options = {}) {
  const { burstSize = 1000, runs = 5 } = options;
  const results = [];

  for (let run = 0; run < runs; run++) {
    const histogram = new BurstLatencyHistogram();
    const daemon = new OptimizedDaemon({
      daemonId: randomUUID(),
      name: `single-burst-${run}`,
      cacheSize: burstSize * 2,
      batchSize: Math.min(burstSize / 5, 500),
      logger: { info: () => {}, debug: () => {}, warn: () => {} },
    });

    await daemon.start();

    for (let i = 0; i < burstSize; i++) {
      daemon.schedule({
        id: `op-${i}`,
        handler: createNoOpHandler(),
      });
    }

    const burstStartTime = performance.now();

    const promises = [];
    for (let i = 0; i < burstSize; i++) {
      const opStart = performance.now();
      promises.push(
        daemon.execute(`op-${i}`)
          .then(() => {
            const latency = performance.now() - opStart;
            histogram.record(latency, performance.now());
          })
          .catch(() => {})
      );
    }

    await Promise.all(promises);
    const burstEndTime = performance.now();
    const burstDuration = burstEndTime - burstStartTime;

    const percentiles = histogram.getPercentiles();
    const timeSeries = histogram.getTimeSeriesStats(50);

    let recoveryTime = 0;
    if (timeSeries.length > 2) {
      const baselineP99 = timeSeries[0].p99;
      for (let i = timeSeries.length - 1; i >= 0; i--) {
        if (timeSeries[i].p99 > baselineP99 * 2) {
          recoveryTime = timeSeries[i].windowStart;
          break;
        }
      }
    }

    results.push({
      burstDurationMs: burstDuration,
      throughput: burstSize / (burstDuration / 1000),
      latency: percentiles,
      stats: histogram.getStats(),
      recoveryTimeMs: recoveryTime,
      timeSeries,
    });

    await daemon.stop();
  }

  const avgThroughput = results.reduce((s, r) => s + r.throughput, 0) / runs;
  const avgP99 = results.reduce((s, r) => s + r.latency.p99, 0) / runs;
  const avgRecovery = results.reduce((s, r) => s + r.recoveryTimeMs, 0) / runs;

  return storeBenchmarkResult({
    name: 'single-burst-response',
    type: 'burst',
    unit: 'ops/sec',
    value: parseFloat(avgThroughput.toFixed(2)),
    burstSize,
    avgThroughput: parseFloat(avgThroughput.toFixed(2)),
    avgP99Latency: parseFloat(avgP99.toFixed(4)),
    avgRecoveryTimeMs: parseFloat(avgRecovery.toFixed(2)),
    meetsP99Target: avgP99 <= BURST_TARGETS.P99_LATENCY_MS,
    meetsRecoveryTarget: avgRecovery <= BURST_TARGETS.RECOVERY_TIME_MS,
    runs,
    details: results,
  });
}

/**
 * Benchmark: Repeated burst pattern
 * Tests daemon response to repeated traffic bursts with recovery periods
 * @param {Object} options - Benchmark options
 * @param {number} [options.burstSize=500] - Operations per burst
 * @param {number} [options.burstCount=5] - Number of bursts
 * @param {number} [options.cooldownMs=500] - Time between bursts
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkRepeatedBursts(options = {}) {
  const { burstSize = 500, burstCount = 5, cooldownMs = 500 } = options;
  const burstResults = [];
  const overallHistogram = new BurstLatencyHistogram();

  const daemon = new OptimizedDaemon({
    daemonId: randomUUID(),
    name: 'repeated-bursts',
    cacheSize: burstSize * burstCount * 2,
    batchSize: Math.min(burstSize / 5, 200),
    logger: { info: () => {}, debug: () => {}, warn: () => {} },
  });

  await daemon.start();

  const testStartTime = performance.now();

  for (let burst = 0; burst < burstCount; burst++) {
    const burstHistogram = new BurstLatencyHistogram();

    for (let i = 0; i < burstSize; i++) {
      daemon.schedule({
        id: `burst-${burst}-op-${i}`,
        handler: createNoOpHandler(),
      });
    }

    const burstStart = performance.now();

    const promises = [];
    for (let i = 0; i < burstSize; i++) {
      const opStart = performance.now();
      promises.push(
        daemon.execute(`burst-${burst}-op-${i}`)
          .then(() => {
            const latency = performance.now() - opStart;
            burstHistogram.record(latency, performance.now());
            overallHistogram.record(latency, performance.now());
          })
          .catch(() => {})
      );
    }

    await Promise.all(promises);
    const burstEnd = performance.now();

    const percentiles = burstHistogram.getPercentiles();
    burstResults.push({
      burstIndex: burst,
      durationMs: burstEnd - burstStart,
      throughput: burstSize / ((burstEnd - burstStart) / 1000),
      p99Latency: percentiles.p99,
      maxLatency: percentiles.max,
    });

    if (burst < burstCount - 1) {
      await new Promise(resolve => setTimeout(resolve, cooldownMs));
    }
  }

  const testEndTime = performance.now();
  await daemon.stop();

  const throughputs = burstResults.map(b => b.throughput);
  const throughputVariance = analyzeVariance(throughputs);
  const p99Latencies = burstResults.map(b => b.p99Latency);

  const firstBurstThroughput = burstResults[0].throughput;
  const lastBurstThroughput = burstResults[burstResults.length - 1].throughput;
  const degradationRatio = lastBurstThroughput / firstBurstThroughput;

  return storeBenchmarkResult({
    name: 'repeated-bursts',
    type: 'burst',
    unit: 'ops/sec',
    value: parseFloat(throughputVariance.mean),
    burstSize,
    burstCount,
    cooldownMs,
    totalDurationMs: testEndTime - testStartTime,
    throughputStats: {
      mean: parseFloat(throughputVariance.mean),
      min: parseFloat(throughputVariance.min),
      max: parseFloat(throughputVariance.max),
      stdDev: parseFloat(throughputVariance.stdDev),
    },
    p99LatencyStats: {
      mean: parseFloat((p99Latencies.reduce((a, b) => a + b, 0) / p99Latencies.length).toFixed(4)),
      max: parseFloat(Math.max(...p99Latencies).toFixed(4)),
    },
    degradationRatio: parseFloat(degradationRatio.toFixed(4)),
    gracefulDegradation: degradationRatio >= BURST_TARGETS.GRACEFUL_DEGRADATION_THRESHOLD,
    overallLatency: overallHistogram.getPercentiles(),
    histogram: overallHistogram.getHistogram(),
    burstDetails: burstResults,
  });
}

/**
 * Benchmark: Traffic spike pattern
 * Tests sudden spike from baseline to peak and recovery
 * @param {Object} options - Benchmark options
 * @param {number} [options.baselineOpsPerSec=100] - Baseline operations per second
 * @param {number} [options.spikeOpsPerSec=5000] - Spike operations per second
 * @param {number} [options.spikeDurationMs=1000] - Duration of spike
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkTrafficSpike(options = {}) {
  const {
    baselineOpsPerSec = 100,
    spikeOpsPerSec = 5000,
    spikeDurationMs = 1000,
  } = options;

  const phases = [
    { name: 'pre-spike', durationMs: 2000, opsPerSec: baselineOpsPerSec },
    { name: 'spike', durationMs: spikeDurationMs, opsPerSec: spikeOpsPerSec },
    { name: 'post-spike', durationMs: 2000, opsPerSec: baselineOpsPerSec },
  ];

  const phaseResults = [];
  const overallHistogram = new BurstLatencyHistogram();

  const daemon = new OptimizedDaemon({
    daemonId: randomUUID(),
    name: 'traffic-spike',
    cacheSize: 20000,
    batchSize: 500,
    logger: { info: () => {}, debug: () => {}, warn: () => {} },
  });

  await daemon.start();

  let opId = 0;
  const testStartTime = performance.now();

  for (const phase of phases) {
    const phaseHistogram = new BurstLatencyHistogram();
    const phaseStart = performance.now();
    const phaseEnd = phaseStart + phase.durationMs;
    const targetIntervalMs = 1000 / phase.opsPerSec;

    let phaseOpsCompleted = 0;
    const activePromises = new Set();

    while (performance.now() < phaseEnd) {
      const currentOpId = `op-${opId++}`;
      daemon.schedule({
        id: currentOpId,
        handler: createNoOpHandler(),
      });

      const opStart = performance.now();
      const promise = daemon.execute(currentOpId)
        .then(() => {
          const latency = performance.now() - opStart;
          phaseHistogram.record(latency, performance.now());
          overallHistogram.record(latency, performance.now());
          phaseOpsCompleted++;
        })
        .catch(() => {})
        .finally(() => activePromises.delete(promise));

      activePromises.add(promise);

      if (targetIntervalMs > 1) {
        await new Promise(resolve => setTimeout(resolve, targetIntervalMs));
      } else if (activePromises.size > 1000) {
        await Promise.race([...activePromises]);
      }
    }

    await Promise.all(activePromises);

    const actualPhaseDuration = (performance.now() - phaseStart) / 1000;
    const percentiles = phaseHistogram.getPercentiles();

    phaseResults.push({
      phase: phase.name,
      targetOpsPerSec: phase.opsPerSec,
      actualOpsPerSec: parseFloat((phaseOpsCompleted / actualPhaseDuration).toFixed(2)),
      operationsCompleted: phaseOpsCompleted,
      durationMs: actualPhaseDuration * 1000,
      latency: percentiles,
      stats: phaseHistogram.getStats(),
    });
  }

  const testEndTime = performance.now();
  await daemon.stop();

  const preSpikePhase = phaseResults.find(p => p.phase === 'pre-spike');
  const spikePhase = phaseResults.find(p => p.phase === 'spike');
  const postSpikePhase = phaseResults.find(p => p.phase === 'post-spike');

  const latencySpikeRatio = spikePhase.latency.p99 / preSpikePhase.latency.p99;
  const recoveryRatio = postSpikePhase.latency.p99 / preSpikePhase.latency.p99;

  return storeBenchmarkResult({
    name: 'traffic-spike',
    type: 'burst',
    unit: 'ops/sec',
    value: spikePhase.actualOpsPerSec,
    baselineOpsPerSec,
    spikeOpsPerSec,
    spikeDurationMs,
    totalDurationMs: testEndTime - testStartTime,
    phases: phaseResults,
    latencySpikeRatio: parseFloat(latencySpikeRatio.toFixed(2)),
    recoveryRatio: parseFloat(recoveryRatio.toFixed(2)),
    gracefulSpike: latencySpikeRatio <= BURST_TARGETS.MAX_LATENCY_SPIKE_RATIO,
    fullRecovery: recoveryRatio <= 1.5,
    overallLatency: overallHistogram.getPercentiles(),
    histogram: overallHistogram.getHistogram(),
  });
}

/**
 * Benchmark: Burst size scaling
 * Tests how burst handling scales with different burst sizes
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Runs per burst size
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkBurstSizeScaling(options = {}) {
  const { runs = 3 } = options;
  const burstSizes = [100, 500, 1000, 2500, 5000, 10000];
  const results = [];

  for (const burstSize of burstSizes) {
    const runResults = [];

    for (let run = 0; run < runs; run++) {
      const histogram = new BurstLatencyHistogram();
      const daemon = new OptimizedDaemon({
        daemonId: randomUUID(),
        name: `burst-scale-${burstSize}`,
        cacheSize: burstSize * 2,
        batchSize: Math.min(burstSize / 5, 500),
        logger: { info: () => {}, debug: () => {}, warn: () => {} },
      });

      await daemon.start();

      for (let i = 0; i < burstSize; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: createNoOpHandler(),
        });
      }

      const startTime = performance.now();
      const promises = [];

      for (let i = 0; i < burstSize; i++) {
        const opStart = performance.now();
        promises.push(
          daemon.execute(`op-${i}`)
            .then(() => histogram.record(performance.now() - opStart, performance.now()))
            .catch(() => {})
        );
      }

      await Promise.all(promises);
      const duration = (performance.now() - startTime) / 1000;

      runResults.push({
        throughput: burstSize / duration,
        p99: histogram.getPercentiles().p99,
      });

      await daemon.stop();
    }

    const avgThroughput = runResults.reduce((s, r) => s + r.throughput, 0) / runs;
    const avgP99 = runResults.reduce((s, r) => s + r.p99, 0) / runs;

    results.push({
      burstSize,
      throughput: parseFloat(avgThroughput.toFixed(2)),
      p99Latency: parseFloat(avgP99.toFixed(4)),
      meetsTarget: avgP99 <= BURST_TARGETS.P99_LATENCY_MS,
    });
  }

  const baselineThroughput = results[0].throughput;
  const scalingAnalysis = results.map(r => ({
    ...r,
    throughputRatio: parseFloat((r.throughput / baselineThroughput).toFixed(2)),
    efficiency: parseFloat(((r.throughput / r.burstSize) * 100).toFixed(2)),
  }));

  return storeBenchmarkResult({
    name: 'burst-size-scaling',
    type: 'burst',
    unit: 'ops/sec',
    value: results.find(r => r.burstSize === 10000)?.throughput || 0,
    scalingCurve: scalingAnalysis,
    maxThroughput: Math.max(...results.map(r => r.throughput)),
    maxBurstWithP99Target: results.filter(r => r.meetsTarget).pop()?.burstSize || 0,
    runs,
  });
}

/**
 * Run all burst traffic benchmarks
 * @param {Object} options - Global benchmark options
 * @returns {Promise<Object>} Combined benchmark results
 */
export async function runAllBurstTrafficBenchmarks(options = {}) {
  console.log('Starting burst traffic benchmarks...\n');

  const results = {};

  console.log('[1/4] Running single burst benchmark...');
  results.singleBurst = await benchmarkSingleBurst(options);
  console.log(`  Throughput: ${results.singleBurst.avgThroughput.toFixed(0)} ops/sec`);

  console.log('[2/4] Running repeated bursts benchmark...');
  results.repeatedBursts = await benchmarkRepeatedBursts(options);
  console.log(`  Degradation ratio: ${results.repeatedBursts.degradationRatio}`);

  console.log('[3/4] Running traffic spike benchmark...');
  results.trafficSpike = await benchmarkTrafficSpike(options);
  console.log(`  Spike throughput: ${results.trafficSpike.value.toFixed(0)} ops/sec`);

  console.log('[4/4] Running burst size scaling benchmark...');
  results.burstScaling = await benchmarkBurstSizeScaling(options);
  console.log(`  Max burst with P99 target: ${results.burstScaling.maxBurstWithP99Target}`);

  console.log('\nBurst traffic benchmarks complete.');

  return {
    name: 'burst-traffic-suite',
    timestamp: new Date().toISOString(),
    results,
    summary: {
      singleBurstThroughput: results.singleBurst.avgThroughput,
      singleBurstP99: results.singleBurst.avgP99Latency,
      meetsP99Target: results.singleBurst.meetsP99Target,
      gracefulDegradation: results.repeatedBursts.gracefulDegradation,
      gracefulSpike: results.trafficSpike.gracefulSpike,
      fullRecovery: results.trafficSpike.fullRecovery,
      maxBurstWithP99Target: results.burstScaling.maxBurstWithP99Target,
    },
  };
}

// CLI execution
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllBurstTrafficBenchmarks()
    .then(results => {
      console.log('\n=== BURST TRAFFIC BENCHMARK SUMMARY ===');
      console.log(JSON.stringify(results.summary, null, 2));
      const passed = results.summary.meetsP99Target && results.summary.gracefulDegradation;
      process.exit(passed ? 0 : 1);
    })
    .catch(err => {
      console.error('Benchmark failed:', err);
      process.exit(1);
    });
}
