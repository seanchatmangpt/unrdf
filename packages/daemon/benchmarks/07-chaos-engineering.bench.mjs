/**
 * @file Chaos Engineering Benchmark
 * @module @unrdf/daemon/benchmarks/chaos-engineering
 * @description Measures chaos injection overhead, fault simulation, and failure mode performance
 * Target: Chaos injection latency <5ms
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
    rss: usage.rss,
  };
}

/**
 * Chaos types for fault injection
 * @readonly
 * @enum {string}
 */
const ChaosType = {
  LATENCY: 'latency',
  ERROR: 'error',
  TIMEOUT: 'timeout',
  CPU_PRESSURE: 'cpu_pressure',
  MEMORY_PRESSURE: 'memory_pressure',
  NETWORK_PARTITION: 'network_partition',
  OPERATION_DROP: 'operation_drop',
};

/**
 * Chaos Engineering Controller for simulating faults
 */
class ChaosController {
  /**
   * @param {Daemon} daemon - Daemon instance to inject chaos into
   */
  constructor(daemon) {
    this.daemon = daemon;
    this.activeChaos = new Map();
    this.injectionHistory = [];
    this.isEnabled = false;
  }

  /**
   * Enable chaos engineering
   */
  enable() {
    this.isEnabled = true;
  }

  /**
   * Disable chaos engineering
   */
  disable() {
    this.isEnabled = false;
    this.activeChaos.clear();
  }

  /**
   * Inject a chaos fault
   * @param {Object} config - Chaos configuration
   * @param {string} config.type - Chaos type from ChaosType enum
   * @param {number} [config.probability=1.0] - Injection probability (0-1)
   * @param {number} [config.duration=1000] - Duration in ms
   * @param {Object} [config.params] - Type-specific parameters
   * @returns {Object} Injection result with timing
   */
  inject(config) {
    const startTime = performance.now();

    const chaos = {
      id: randomUUID(),
      type: config.type,
      probability: config.probability || 1.0,
      duration: config.duration || 1000,
      params: config.params || {},
      startedAt: Date.now(),
      expiresAt: Date.now() + (config.duration || 1000),
    };

    this.activeChaos.set(chaos.id, chaos);

    // Schedule removal
    setTimeout(() => {
      this.activeChaos.delete(chaos.id);
    }, chaos.duration);

    const injectionTime = performance.now() - startTime;

    const record = {
      chaosId: chaos.id,
      type: chaos.type,
      injectionTimeMs: injectionTime,
      timestamp: Date.now(),
    };

    this.injectionHistory.push(record);

    return {
      chaosId: chaos.id,
      injectionTimeMs: injectionTime,
      chaos,
    };
  }

  /**
   * Inject latency chaos
   * @param {number} delayMs - Delay to inject in milliseconds
   * @param {number} [duration=5000] - How long to keep chaos active
   * @returns {Object} Injection result
   */
  injectLatency(delayMs, duration = 5000) {
    return this.inject({
      type: ChaosType.LATENCY,
      duration,
      params: { delayMs },
    });
  }

  /**
   * Inject error chaos
   * @param {string} errorMessage - Error message to inject
   * @param {number} [probability=0.5] - Probability of error (0-1)
   * @param {number} [duration=5000] - How long to keep chaos active
   * @returns {Object} Injection result
   */
  injectError(errorMessage, probability = 0.5, duration = 5000) {
    return this.inject({
      type: ChaosType.ERROR,
      probability,
      duration,
      params: { errorMessage },
    });
  }

  /**
   * Inject timeout chaos
   * @param {number} timeoutMs - Timeout duration to inject
   * @param {number} [duration=5000] - How long to keep chaos active
   * @returns {Object} Injection result
   */
  injectTimeout(timeoutMs, duration = 5000) {
    return this.inject({
      type: ChaosType.TIMEOUT,
      duration,
      params: { timeoutMs },
    });
  }

  /**
   * Simulate CPU pressure
   * @param {number} intensity - CPU pressure intensity (0-1)
   * @param {number} [duration=1000] - Duration in ms
   * @returns {Object} Injection result
   */
  injectCpuPressure(intensity, duration = 1000) {
    return this.inject({
      type: ChaosType.CPU_PRESSURE,
      duration,
      params: { intensity },
    });
  }

  /**
   * Simulate memory pressure
   * @param {number} sizeMB - Memory to allocate in MB
   * @param {number} [duration=1000] - Duration in ms
   * @returns {Object} Injection result
   */
  injectMemoryPressure(sizeMB, duration = 1000) {
    return this.inject({
      type: ChaosType.MEMORY_PRESSURE,
      duration,
      params: { sizeMB },
    });
  }

  /**
   * Simulate network partition
   * @param {Array<string>} partitionedNodes - Nodes to partition
   * @param {number} [duration=5000] - Duration in ms
   * @returns {Object} Injection result
   */
  injectNetworkPartition(partitionedNodes, duration = 5000) {
    return this.inject({
      type: ChaosType.NETWORK_PARTITION,
      duration,
      params: { partitionedNodes },
    });
  }

  /**
   * Get active chaos count
   * @returns {number} Number of active chaos injections
   */
  getActiveChaosCount() {
    return this.activeChaos.size;
  }

  /**
   * Get injection statistics
   * @returns {Object} Injection statistics
   */
  getStats() {
    const latencies = this.injectionHistory.map(h => h.injectionTimeMs);
    return {
      totalInjections: this.injectionHistory.length,
      activeChaos: this.activeChaos.size,
      latencies,
    };
  }
}

/**
 * Benchmark: Basic chaos injection latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.injectionCount=1000] - Number of injections
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with latency metrics
 */
export async function benchmarkChaosInjectionLatency(options = {}) {
  const { injectionCount = 1000, runs = 5 } = options;
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `chaos-bench-${runIdx}`,
    });
    await daemon.start();

    const chaos = new ChaosController(daemon);
    chaos.enable();

    const runLatencies = [];

    // Warm-up phase
    const warmupCount = Math.ceil(injectionCount * 0.1);
    for (let i = 0; i < warmupCount; i++) {
      chaos.inject({ type: ChaosType.LATENCY, duration: 10, params: { delayMs: 1 } });
    }

    // Actual measurement
    for (let i = 0; i < injectionCount; i++) {
      const result = chaos.inject({
        type: ChaosType.LATENCY,
        duration: 10,
        params: { delayMs: 1 },
      });
      runLatencies.push(result.injectionTimeMs);
    }

    allLatencies.push(...runLatencies);
    chaos.disable();
    await daemon.stop();
  }

  const variance = analyzeVariance(allLatencies);
  const p50 = getPercentile(allLatencies, 50);
  const p95 = getPercentile(allLatencies, 95);
  const p99 = getPercentile(allLatencies, 99);

  return storeBenchmarkResult({
    name: 'chaos-injection-latency',
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
    targetMet: p95 < 5.0,
    target: '<5ms (P95)',
    sampleCount: allLatencies.length,
    injectionCount,
    runs,
  });
}

/**
 * Benchmark: Multiple chaos type injection
 * @param {Object} options - Benchmark options
 * @param {number} [options.injectionsPerType=200] - Injections per chaos type
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with per-type metrics
 */
export async function benchmarkChaosTypeOverhead(options = {}) {
  const { injectionsPerType = 200, runs = 3 } = options;

  const chaosTypes = [
    { type: ChaosType.LATENCY, params: { delayMs: 100 } },
    { type: ChaosType.ERROR, params: { errorMessage: 'Chaos error' } },
    { type: ChaosType.TIMEOUT, params: { timeoutMs: 5000 } },
    { type: ChaosType.CPU_PRESSURE, params: { intensity: 0.5 } },
    { type: ChaosType.MEMORY_PRESSURE, params: { sizeMB: 10 } },
    { type: ChaosType.NETWORK_PARTITION, params: { partitionedNodes: ['node-1'] } },
    { type: ChaosType.OPERATION_DROP, params: { dropRate: 0.1 } },
  ];

  const typeResults = [];

  for (const chaosConfig of chaosTypes) {
    const typeLatencies = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemon = new Daemon({
        daemonId: generateDaemonId(),
        name: `chaos-type-${chaosConfig.type}-${runIdx}`,
      });
      await daemon.start();

      const chaos = new ChaosController(daemon);
      chaos.enable();

      // Warm-up
      for (let i = 0; i < 20; i++) {
        chaos.inject({ type: chaosConfig.type, duration: 5, params: chaosConfig.params });
      }

      // Measure
      for (let i = 0; i < injectionsPerType; i++) {
        const result = chaos.inject({
          type: chaosConfig.type,
          duration: 5,
          params: chaosConfig.params,
        });
        typeLatencies.push(result.injectionTimeMs);
      }

      chaos.disable();
      await daemon.stop();
    }

    const variance = analyzeVariance(typeLatencies);
    const p95 = getPercentile(typeLatencies, 95);

    typeResults.push({
      type: chaosConfig.type,
      mean: parseFloat(variance.mean),
      p95: p95.toFixed(4),
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 5.0,
    });
  }

  const allMeans = typeResults.map(r => r.mean);
  const overallVariance = analyzeVariance(allMeans);

  return storeBenchmarkResult({
    name: 'chaos-type-overhead',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    typeResults,
    target: '<5ms (P95)',
    injectionsPerType,
    runs,
  });
}

/**
 * Benchmark: Concurrent chaos injection
 * @param {Object} options - Benchmark options
 * @param {number} [options.concurrentInjections=100] - Concurrent injections
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with concurrency metrics
 */
export async function benchmarkConcurrentChaosInjection(options = {}) {
  const { concurrentInjections = 100, runs = 3 } = options;
  const throughputs = [];
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `concurrent-chaos-${runIdx}`,
    });
    await daemon.start();

    const chaos = new ChaosController(daemon);
    chaos.enable();

    const startTime = performance.now();
    const injectionPromises = [];

    for (let i = 0; i < concurrentInjections; i++) {
      injectionPromises.push(
        new Promise(resolve => {
          const result = chaos.inject({
            type: ChaosType.LATENCY,
            duration: 100,
            params: { delayMs: 10 },
          });
          resolve(result);
        })
      );
    }

    const results = await Promise.all(injectionPromises);
    const endTime = performance.now();

    const duration = endTime - startTime;
    const throughput = (concurrentInjections / duration) * 1000;
    throughputs.push(throughput);

    const latencies = results.map(r => r.injectionTimeMs);
    allLatencies.push(...latencies);

    chaos.disable();
    await daemon.stop();
  }

  const throughputVariance = analyzeVariance(throughputs);
  const latencyVariance = analyzeVariance(allLatencies);
  const p95 = getPercentile(allLatencies, 95);

  return storeBenchmarkResult({
    name: 'concurrent-chaos-injection',
    type: 'throughput',
    unit: 'injections/sec',
    value: parseFloat(throughputVariance.mean),
    min: parseFloat(throughputVariance.min),
    max: parseFloat(throughputVariance.max),
    stdDev: parseFloat(throughputVariance.stdDev),
    variance: parseFloat(throughputVariance.coefficientOfVariation),
    latency: {
      mean: parseFloat(latencyVariance.mean),
      p95: p95.toFixed(4),
    },
    targetMet: p95 < 5.0,
    target: '<5ms (P95)',
    concurrentInjections,
    runs,
  });
}

/**
 * Benchmark: Chaos injection under daemon load
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=500] - Background operations
 * @param {number} [options.injectionCount=500] - Number of injections
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with load metrics
 */
export async function benchmarkChaosUnderLoad(options = {}) {
  const { operationCount = 500, injectionCount = 500, runs = 3 } = options;

  const loadScales = [
    { ops: 50, name: 'light' },
    { ops: 200, name: 'medium' },
    { ops: operationCount, name: 'heavy' },
  ];

  const scaleResults = [];

  for (const scale of loadScales) {
    const scaleLatencies = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemon = new Daemon({
        daemonId: generateDaemonId(),
        name: `chaos-load-${scale.name}-${runIdx}`,
      });
      await daemon.start();

      // Create background load
      for (let i = 0; i < scale.ops; i++) {
        daemon.schedule({
          id: `load-op-${i}`,
          handler: async () => {
            await new Promise(resolve => setTimeout(resolve, 1));
            return { index: i };
          },
        });
      }

      const chaos = new ChaosController(daemon);
      chaos.enable();

      // Warm-up
      for (let i = 0; i < 50; i++) {
        chaos.inject({ type: ChaosType.LATENCY, duration: 5, params: { delayMs: 1 } });
      }

      // Measure injections under load
      for (let i = 0; i < injectionCount; i++) {
        const result = chaos.inject({
          type: ChaosType.LATENCY,
          duration: 10,
          params: { delayMs: 1 },
        });
        scaleLatencies.push(result.injectionTimeMs);
      }

      chaos.disable();
      await daemon.stop();
    }

    const variance = analyzeVariance(scaleLatencies);
    const p95 = getPercentile(scaleLatencies, 95);

    scaleResults.push({
      scale: scale.name,
      operationCount: scale.ops,
      mean: parseFloat(variance.mean),
      p95: p95.toFixed(4),
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 5.0,
    });
  }

  const allMeans = scaleResults.map(r => r.mean);
  const overallVariance = analyzeVariance(allMeans);

  return storeBenchmarkResult({
    name: 'chaos-injection-under-load',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    scaleResults,
    target: '<5ms (P95)',
    injectionCount,
    runs,
  });
}

/**
 * Benchmark: Chaos memory overhead
 * @param {Object} options - Benchmark options
 * @param {number} [options.activeChaosCount=1000] - Number of active chaos instances
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with memory metrics
 */
export async function benchmarkChaosMemoryOverhead(options = {}) {
  const { activeChaosCount = 1000, runs = 3 } = options;
  const memoryMeasurements = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    if (global.gc) global.gc();

    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `chaos-mem-${runIdx}`,
    });
    await daemon.start();

    const beforeMemory = getMemorySnapshot();
    const chaos = new ChaosController(daemon);
    chaos.enable();

    // Create many active chaos instances
    for (let i = 0; i < activeChaosCount; i++) {
      chaos.inject({
        type: ChaosType.LATENCY,
        duration: 60000, // Long duration to keep active
        params: { delayMs: 10 },
      });
    }

    const afterMemory = getMemorySnapshot();
    const heapDelta = afterMemory.heapUsed - beforeMemory.heapUsed;
    const bytesPerChaos = heapDelta / activeChaosCount;

    memoryMeasurements.push({
      activeChaosCount,
      heapDelta,
      bytesPerChaos,
      totalMB: heapDelta / (1024 * 1024),
    });

    chaos.disable();
    await daemon.stop();
  }

  const bytesPerChaosValues = memoryMeasurements.map(m => m.bytesPerChaos);
  const variance = analyzeVariance(bytesPerChaosValues);

  return storeBenchmarkResult({
    name: 'chaos-memory-overhead',
    type: 'memory',
    unit: 'bytes/chaos',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    measurements: memoryMeasurements,
    activeChaosCount,
    runs,
  });
}

/**
 * Benchmark: Multi-scale chaos simulation
 * Tests small, medium, and large scale chaos scenarios
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=3] - Number of runs per scale
 * @returns {Object} Benchmark result with multi-scale metrics
 */
export async function benchmarkMultiScaleChaos(options = {}) {
  const { runs = 3 } = options;

  const scales = [
    { name: 'small', daemons: 1, injectionsPerDaemon: 500 },
    { name: 'medium', daemons: 3, injectionsPerDaemon: 300 },
    { name: 'large', daemons: 5, injectionsPerDaemon: 200 },
  ];

  const scaleResults = [];

  for (const scale of scales) {
    const scaleLatencies = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      const daemons = [];
      const controllers = [];

      // Create daemons and chaos controllers
      for (let d = 0; d < scale.daemons; d++) {
        const daemon = new Daemon({
          daemonId: generateDaemonId(),
          name: `multi-chaos-${scale.name}-${d}`,
        });
        await daemon.start();
        daemons.push(daemon);

        const chaos = new ChaosController(daemon);
        chaos.enable();
        controllers.push(chaos);
      }

      // Perform injections across all controllers
      for (let i = 0; i < scale.injectionsPerDaemon; i++) {
        for (const chaos of controllers) {
          const result = chaos.inject({
            type: ChaosType.LATENCY,
            duration: 10,
            params: { delayMs: 1 },
          });
          scaleLatencies.push(result.injectionTimeMs);
        }
      }

      // Cleanup
      for (const chaos of controllers) {
        chaos.disable();
      }
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
      injectionsPerDaemon: scale.injectionsPerDaemon,
      totalInjections: scale.daemons * scale.injectionsPerDaemon * runs,
      mean: parseFloat(variance.mean),
      percentiles: {
        p50: p50.toFixed(4),
        p95: p95.toFixed(4),
        p99: p99.toFixed(4),
      },
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 5.0,
    });
  }

  const allMeans = scaleResults.map(r => r.mean);
  const overallVariance = analyzeVariance(allMeans);

  return storeBenchmarkResult({
    name: 'multi-scale-chaos-simulation',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    scaleResults,
    target: '<5ms (P95)',
    runs,
  });
}
