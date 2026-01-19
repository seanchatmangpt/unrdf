/**
 * @file Uptime Simulation Benchmark
 * @module @unrdf/daemon/benchmarks/uptime-simulation
 * @description Measures uptime check performance, health polling, and availability calculations
 * Target: Uptime check latency <1ms
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
 * Calculate percentile from sorted array
 * @param {Array<number>} values - Sorted array of values
 * @param {number} percentile - Percentile (0-100)
 * @returns {number} Percentile value
 */
function getPercentile(values, percentile) {
  if (values.length === 0) return 0;
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil((percentile / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

/**
 * Get current memory usage in bytes
 * @returns {Object} Memory usage information
 */
function getMemorySnapshot() {
  const usage = process.memoryUsage();
  return {
    heapUsed: usage.heapUsed,
    heapTotal: usage.heapTotal,
    external: usage.external,
    rss: usage.rss,
  };
}

/**
 * Simulates an uptime monitor that tracks daemon health
 */
class UptimeMonitor {
  /**
   * @param {Daemon} daemon - Daemon instance to monitor
   */
  constructor(daemon) {
    this.daemon = daemon;
    this.checks = [];
    this.uptimeStart = Date.now();
    this.totalDowntime = 0;
    this.lastCheckTime = Date.now();
    this.lastStatus = true;
  }

  /**
   * Perform uptime check
   * @returns {Object} Check result with latency
   */
  check() {
    const startTime = performance.now();
    const health = this.daemon.getHealth();
    const latency = performance.now() - startTime;

    const currentStatus = health.isRunning;
    const now = Date.now();

    if (!currentStatus && this.lastStatus) {
      this.lastCheckTime = now;
    } else if (currentStatus && !this.lastStatus) {
      this.totalDowntime += now - this.lastCheckTime;
    }

    this.lastStatus = currentStatus;

    const checkResult = {
      timestamp: now,
      latencyMs: latency,
      isRunning: health.isRunning,
      activeOperations: health.activeOperations,
      queuedOperations: health.queuedOperations,
      nodeId: health.nodeId,
    };

    this.checks.push(checkResult);
    return checkResult;
  }

  /**
   * Get availability percentage
   * @returns {number} Availability percentage (0-100)
   */
  getAvailability() {
    const totalTime = Date.now() - this.uptimeStart;
    if (totalTime === 0) return 100;
    const uptime = totalTime - this.totalDowntime;
    return (uptime / totalTime) * 100;
  }

  /**
   * Get statistics for all checks
   * @returns {Object} Check statistics
   */
  getStats() {
    const latencies = this.checks.map(c => c.latencyMs);
    return {
      checkCount: this.checks.length,
      availability: this.getAvailability(),
      latencies,
    };
  }
}

/**
 * Benchmark: Basic uptime check latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.checkCount=1000] - Number of uptime checks
 * @param {number} [options.runs=5] - Number of benchmark runs
 * @returns {Object} Benchmark result with latency metrics
 */
export async function benchmarkUptimeCheckLatency(options = {}) {
  const { checkCount = 1000, runs = 5 } = options;
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `uptime-bench-${runIdx}`,
    });
    await daemon.start();

    const monitor = new UptimeMonitor(daemon);
    const runLatencies = [];

    // Warm-up phase (10% of checks)
    const warmupCount = Math.ceil(checkCount * 0.1);
    for (let i = 0; i < warmupCount; i++) {
      monitor.check();
    }

    // Actual measurement phase
    for (let i = 0; i < checkCount; i++) {
      const result = monitor.check();
      runLatencies.push(result.latencyMs);
    }

    allLatencies.push(...runLatencies);
    await daemon.stop();
  }

  const variance = analyzeVariance(allLatencies);
  const p50 = getPercentile(allLatencies, 50);
  const p95 = getPercentile(allLatencies, 95);
  const p99 = getPercentile(allLatencies, 99);

  return storeBenchmarkResult({
    name: 'uptime-check-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    percentiles: {
      p50: p50.toFixed(4),
      p95: p95.toFixed(4),
      p99: p99.toFixed(4),
    },
    targetMet: p95 < 1.0,
    target: '<1ms (P95)',
    sampleCount: allLatencies.length,
    checkCount,
    runs,
  });
}

/**
 * Benchmark: Uptime check under load
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=500] - Concurrent operations
 * @param {number} [options.checkCount=500] - Number of checks
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with load metrics
 */
export async function benchmarkUptimeUnderLoad(options = {}) {
  const { operationCount = 500, checkCount = 500, runs = 3 } = options;
  const results = [];

  const loadScales = [
    { ops: 10, name: 'light' },
    { ops: 100, name: 'medium' },
    { ops: operationCount, name: 'heavy' },
  ];

  for (const scale of loadScales) {
    const scaleLatencies = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemon = new Daemon({
        daemonId: generateDaemonId(),
        name: `uptime-load-${scale.name}-${runIdx}`,
      });
      await daemon.start();

      // Schedule operations to create load
      for (let i = 0; i < scale.ops; i++) {
        daemon.schedule({
          id: `load-op-${i}`,
          handler: async () => {
            await new Promise(resolve => setTimeout(resolve, 1));
            return { index: i };
          },
        });
      }

      const monitor = new UptimeMonitor(daemon);

      // Warm-up
      for (let i = 0; i < 50; i++) {
        monitor.check();
      }

      // Measure with load
      for (let i = 0; i < checkCount; i++) {
        const result = monitor.check();
        scaleLatencies.push(result.latencyMs);
      }

      await daemon.stop();
    }

    const variance = analyzeVariance(scaleLatencies);
    const p95 = getPercentile(scaleLatencies, 95);

    results.push({
      scale: scale.name,
      operationCount: scale.ops,
      mean: parseFloat(variance.mean),
      p95: p95.toFixed(4),
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 1.0,
    });
  }

  const allMeans = results.map(r => r.mean);
  const overallVariance = analyzeVariance(allMeans);

  return storeBenchmarkResult({
    name: 'uptime-check-under-load',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    scaleResults: results,
    checkCount,
    runs,
  });
}

/**
 * Benchmark: Availability calculation performance
 * @param {Object} options - Benchmark options
 * @param {number} [options.eventCount=10000] - Simulated events
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with availability metrics
 */
export async function benchmarkAvailabilityCalculation(options = {}) {
  const { eventCount = 10000, runs = 5 } = options;
  const calculationTimes = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `avail-bench-${runIdx}`,
    });
    await daemon.start();

    const monitor = new UptimeMonitor(daemon);

    // Simulate check history
    for (let i = 0; i < eventCount; i++) {
      monitor.check();
    }

    // Measure availability calculation
    const calcStart = performance.now();
    for (let i = 0; i < 1000; i++) {
      monitor.getAvailability();
    }
    const calcEnd = performance.now();

    const avgCalcTime = (calcEnd - calcStart) / 1000;
    calculationTimes.push(avgCalcTime);

    await daemon.stop();
  }

  const variance = analyzeVariance(calculationTimes);
  const p95 = getPercentile(calculationTimes, 95);

  return storeBenchmarkResult({
    name: 'availability-calculation',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    percentiles: {
      p95: p95.toFixed(4),
    },
    targetMet: p95 < 0.1,
    target: '<0.1ms (P95)',
    eventCount,
    runs,
  });
}

/**
 * Benchmark: Health polling throughput
 * @param {Object} options - Benchmark options
 * @param {number} [options.duration=3000] - Polling duration in ms
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with throughput metrics
 */
export async function benchmarkHealthPollingThroughput(options = {}) {
  const { duration = 3000, runs = 3 } = options;
  const throughputs = [];
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `polling-bench-${runIdx}`,
    });
    await daemon.start();

    const monitor = new UptimeMonitor(daemon);
    const startTime = Date.now();
    let checkCount = 0;
    const runLatencies = [];

    // Continuous polling for duration
    while (Date.now() - startTime < duration) {
      const result = monitor.check();
      runLatencies.push(result.latencyMs);
      checkCount++;
    }

    const actualDuration = Date.now() - startTime;
    const checksPerSecond = (checkCount / actualDuration) * 1000;
    throughputs.push(checksPerSecond);
    allLatencies.push(...runLatencies);

    await daemon.stop();
  }

  const throughputVariance = analyzeVariance(throughputs);
  const latencyVariance = analyzeVariance(allLatencies);
  const p95 = getPercentile(allLatencies, 95);

  return storeBenchmarkResult({
    name: 'health-polling-throughput',
    type: 'throughput',
    unit: 'checks/sec',
    value: parseFloat(throughputVariance.mean),
    min: parseFloat(throughputVariance.min),
    max: parseFloat(throughputVariance.max),
    stdDev: parseFloat(throughputVariance.stdDev),
    variance: parseFloat(throughputVariance.coefficientOfVariation),
    latency: {
      mean: parseFloat(latencyVariance.mean),
      p95: p95.toFixed(4),
    },
    duration,
    runs,
  });
}

/**
 * Benchmark: Memory footprint of uptime monitoring
 * @param {Object} options - Benchmark options
 * @param {number} [options.historySize=50000] - Check history size
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with memory metrics
 */
export async function benchmarkUptimeMemoryFootprint(options = {}) {
  const { historySize = 50000, runs = 3 } = options;
  const memoryMeasurements = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    if (global.gc) global.gc();

    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `mem-bench-${runIdx}`,
    });
    await daemon.start();

    const beforeMemory = getMemorySnapshot();
    const monitor = new UptimeMonitor(daemon);

    // Build up history
    for (let i = 0; i < historySize; i++) {
      monitor.check();
    }

    const afterMemory = getMemorySnapshot();
    const heapDelta = afterMemory.heapUsed - beforeMemory.heapUsed;
    const bytesPerCheck = heapDelta / historySize;

    memoryMeasurements.push({
      historySize,
      heapDelta,
      bytesPerCheck,
      totalMB: heapDelta / (1024 * 1024),
    });

    await daemon.stop();
  }

  const bytesPerCheckValues = memoryMeasurements.map(m => m.bytesPerCheck);
  const variance = analyzeVariance(bytesPerCheckValues);

  return storeBenchmarkResult({
    name: 'uptime-memory-footprint',
    type: 'memory',
    unit: 'bytes/check',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    measurements: memoryMeasurements,
    historySize,
    runs,
  });
}

/**
 * Benchmark: Multi-scale uptime simulation
 * Tests small, medium, and large scale scenarios
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Number of runs per scale
 * @returns {Object} Benchmark result with multi-scale metrics
 */
export async function benchmarkMultiScaleUptime(options = {}) {
  const { runs = 3 } = options;

  const scales = [
    { name: 'small', daemons: 1, checksPerDaemon: 1000 },
    { name: 'medium', daemons: 5, checksPerDaemon: 500 },
    { name: 'large', daemons: 10, checksPerDaemon: 200 },
  ];

  const scaleResults = [];

  for (const scale of scales) {
    const scaleLatencies = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemons = [];
      const monitors = [];

      // Create daemons and monitors
      for (let d = 0; d < scale.daemons; d++) {
        const daemon = new Daemon({
          daemonId: generateDaemonId(),
          name: `multi-scale-${scale.name}-${d}`,
        });
        await daemon.start();
        daemons.push(daemon);
        monitors.push(new UptimeMonitor(daemon));
      }

      // Perform checks across all monitors
      for (let c = 0; c < scale.checksPerDaemon; c++) {
        for (const monitor of monitors) {
          const result = monitor.check();
          scaleLatencies.push(result.latencyMs);
        }
      }

      // Cleanup
      for (const daemon of daemons) {
        await daemon.stop();
      }
    }

    const variance = analyzeVariance(scaleLatencies);
    const p50 = getPercentile(scaleLatencies, 50);
    const p95 = getPercentile(scaleLatencies, 95);
    const p99 = getPercentile(scaleLatencies, 99);

    scaleResults.push({
      scale: scale.name,
      daemons: scale.daemons,
      checksPerDaemon: scale.checksPerDaemon,
      totalChecks: scale.daemons * scale.checksPerDaemon * runs,
      mean: parseFloat(variance.mean),
      percentiles: {
        p50: p50.toFixed(4),
        p95: p95.toFixed(4),
        p99: p99.toFixed(4),
      },
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 1.0,
    });
  }

  const allMeans = scaleResults.map(r => r.mean);
  const overallVariance = analyzeVariance(allMeans);

  return storeBenchmarkResult({
    name: 'multi-scale-uptime-simulation',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    scaleResults,
    target: '<1ms (P95)',
    runs,
  });
}
