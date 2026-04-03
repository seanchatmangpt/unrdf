/**
 * @file Memory Usage Under Load Benchmark
 * @module @unrdf/daemon/benchmarks/memory-load
 * @description Measures memory consumption with varying operation queue sizes
 * Tests heap usage, garbage collection, and memory stability
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
    arrayBuffers: usage.arrayBuffers || 0,
  };
}

/**
 * Force garbage collection (if available)
 * @returns {void}
 */
function forceGC() {
  if (global.gc) {
    global.gc();
  }
}

/**
 * Benchmark: Memory usage with growing operation queue
 * @param {Object} options - Benchmark options
 * @param {number} [options.maxOperations=10000] - Maximum operations to queue
 * @param {number} [options.steps=5] - Number of measurement steps
 * @returns {Object} Benchmark result with memory metrics
 */
export async function benchmarkMemoryWithLoad(options = {}) {
  const { maxOperations = 10000, steps = 5 } = options;
  const measurements = [];

  for (let step = 0; step < steps; step++) {
    forceGC();

    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `bench-mem-${step}`,
    });
    await daemon.start();

    const operationCount = (maxOperations / steps) * (step + 1);
    const startMemory = getMemorySnapshot();

    for (let i = 0; i < operationCount; i++) {
      daemon.schedule({
        id: `op-${i}`,
        name: `Operation ${i}`,
        handler: async () => ({ index: i, success: true }),
        metadata: {
          timestamp: Date.now(),
          data: `metadata-${i}`,
        },
      });
    }

    const endMemory = getMemorySnapshot();
    const heapDelta = endMemory.heapUsed - startMemory.heapUsed;
    const bytesPerOperation = heapDelta / operationCount;

    measurements.push({
      operationCount,
      heapUsedStart: startMemory.heapUsed,
      heapUsedEnd: endMemory.heapUsed,
      heapDelta,
      bytesPerOperation: bytesPerOperation.toFixed(2),
      rssStart: startMemory.rss,
      rssEnd: endMemory.rss,
      rssDelta: endMemory.rss - startMemory.rss,
    });

    await daemon.stop();
  }

  const bytesPerOpList = measurements.map(m => parseFloat(m.bytesPerOperation));
  const variance = analyzeVariance(bytesPerOpList);

  return storeBenchmarkResult({
    name: 'memory-usage-with-load',
    type: 'memory',
    unit: 'bytes/op',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    measurements,
    maxOperations,
    steps,
  });
}

/**
 * Benchmark: Memory consumption during execution
 * @param {Object} options - Benchmark options
 * @param {number} [options.operationCount=1000] - Operations to execute
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with execution memory metrics
 */
export async function benchmarkExecutionMemory(options = {}) {
  const { operationCount = 1000, runs = 3 } = options;
  const memorySnapshots = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    forceGC();

    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `bench-exec-mem-${runIdx}`,
    });
    await daemon.start();

    const operationIds = [];
    const beforeMemory = getMemorySnapshot();

    for (let i = 0; i < operationCount; i++) {
      const opId = `op-${i}`;
      operationIds.push(opId);
      daemon.schedule({
        id: opId,
        handler: async () => ({ success: true, data: `result-${i}` }),
      });
    }

    const scheduledMemory = getMemorySnapshot();

    // Execute all operations
    const beforeExecution = getMemorySnapshot();
    await Promise.all(
      operationIds.map(opId => daemon.execute(opId).catch(() => {}))
    );
    const afterExecution = getMemorySnapshot();

    memorySnapshots.push({
      operationCount,
      beforeScheduling: beforeMemory.heapUsed,
      afterScheduling: scheduledMemory.heapUsed,
      beforeExecution: beforeExecution.heapUsed,
      afterExecution: afterExecution.heapUsed,
      schedulingDelta: scheduledMemory.heapUsed - beforeMemory.heapUsed,
      executionDelta: afterExecution.heapUsed - beforeExecution.heapUsed,
    });

    await daemon.stop();
    forceGC();
  }

  const peakMemory = Math.max(...memorySnapshots.map(m => m.afterExecution));
  const avgMemory = memorySnapshots.reduce((sum, m) => sum + m.afterExecution, 0) / runs;

  return storeBenchmarkResult({
    name: 'execution-memory-consumption',
    type: 'memory',
    unit: 'bytes',
    value: avgMemory,
    peakMemory,
    snapshots: memorySnapshots,
    operationCount,
    runs,
  });
}

/**
 * Benchmark: Memory stability under sustained load
 * @param {Object} options - Benchmark options
 * @param {number} [options.duration=5000] - Test duration in milliseconds
 * @param {number} [options.opsPerSecond=100] - Operations per second
 * @returns {Object} Benchmark result with stability metrics
 */
export async function benchmarkMemoryStability(options = {}) {
  const { duration = 5000, opsPerSecond = 100 } = options;
  const memoryReadings = [];

  const daemon = new Daemon({
    daemonId: generateDaemonId(),
    name: 'bench-stability',
  });
  await daemon.start();

  const startTime = Date.now();
  const interval = 1000 / opsPerSecond;
  let operationCounter = 0;

  const memoryMonitor = setInterval(() => {
    const reading = getMemorySnapshot();
    memoryReadings.push({
      timestamp: Date.now() - startTime,
      heapUsed: reading.heapUsed,
      rss: reading.rss,
      operationCount: operationCounter,
    });
  }, 100);

  const operationScheduler = setInterval(() => {
    if (Date.now() - startTime < duration) {
      daemon.schedule({
        id: `op-${operationCounter}`,
        handler: async () => ({ count: operationCounter }),
      });
      operationCounter += 1;
    }
  }, interval);

  await new Promise(resolve => {
    setTimeout(() => {
      clearInterval(memoryMonitor);
      clearInterval(operationScheduler);
      resolve();
    }, duration + 1000);
  });

  const heapValues = memoryReadings.map(r => r.heapUsed);
  const heapVariance = analyzeVariance(heapValues);

  const rssValues = memoryReadings.map(r => r.rss);
  const rssVariance = analyzeVariance(rssValues);

  await daemon.stop();

  return storeBenchmarkResult({
    name: 'memory-stability-under-load',
    type: 'memory',
    unit: 'bytes',
    value: parseFloat(heapVariance.mean),
    heap: {
      mean: parseFloat(heapVariance.mean),
      stdDev: parseFloat(heapVariance.stdDev),
      variance: parseFloat(heapVariance.coefficientOfVariation),
      min: parseFloat(heapVariance.min),
      max: parseFloat(heapVariance.max),
    },
    rss: {
      mean: parseFloat(rssVariance.mean),
      stdDev: parseFloat(rssVariance.stdDev),
      variance: parseFloat(rssVariance.coefficientOfVariation),
      min: parseFloat(rssVariance.min),
      max: parseFloat(rssVariance.max),
    },
    readingCount: memoryReadings.length,
    totalOperations: operationCounter,
    duration,
  });
}
