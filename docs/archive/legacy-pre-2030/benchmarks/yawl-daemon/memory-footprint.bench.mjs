/**
 * @file Memory Footprint Benchmark
 * @module benchmarks/yawl-daemon/memory-footprint
 * @description Measures memory usage of daemon and workflows over time
 *
 * Performance Target: <50MB baseline memory footprint
 *
 * Metrics:
 * - Baseline memory usage
 * - Memory growth under load
 * - Memory leaks detection
 * - GC pressure and impact
 */

import { Daemon } from '../../packages/daemon/src/daemon.mjs';
import { randomUUID } from 'crypto';

/**
 * Get current memory usage in MB
 * @returns {Object} Memory usage statistics
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    heapUsedMB: usage.heapUsed / 1024 / 1024,
    heapTotalMB: usage.heapTotal / 1024 / 1024,
    rssMB: usage.rss / 1024 / 1024,
    externalMB: usage.external / 1024 / 1024,
  };
}

/**
 * Force garbage collection if available
 */
function forceGC() {
  if (global.gc) {
    global.gc();
  }
}

/**
 * Wait for specified duration
 * @param {number} ms - Milliseconds to wait
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Benchmark: Baseline memory footprint
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkBaselineMemory(options = {}) {
  const { runs = 5 } = options;
  const memoryMeasurements = [];

  for (let i = 0; i < runs; i++) {
    // Force GC before measurement
    forceGC();
    await sleep(100);

    const beforeMemory = getMemoryUsage();

    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `baseline-${i}`,
      maxConcurrent: 5,
    });

    await daemon.start();
    await sleep(100);

    // Force GC to get stable measurement
    forceGC();
    await sleep(100);

    const afterMemory = getMemoryUsage();
    const daemonMemory = afterMemory.heapUsedMB - beforeMemory.heapUsedMB;

    memoryMeasurements.push({
      baseline: beforeMemory.heapUsedMB,
      withDaemon: afterMemory.heapUsedMB,
      daemonFootprint: daemonMemory,
      rss: afterMemory.rssMB,
    });

    await daemon.stop();
    forceGC();
    await sleep(100);
  }

  const avgFootprint = memoryMeasurements.reduce((sum, m) => sum + m.daemonFootprint, 0) / runs;
  const maxFootprint = Math.max(...memoryMeasurements.map(m => m.daemonFootprint));
  const avgRSS = memoryMeasurements.reduce((sum, m) => sum + m.rss, 0) / runs;

  return {
    name: 'baseline-memory-footprint',
    runs,
    memory: {
      avgFootprintMB: avgFootprint,
      maxFootprintMB: maxFootprint,
      avgRssMB: avgRSS,
    },
    measurements: memoryMeasurements,
    passed: avgFootprint < 50, // Target: <50MB
    target: '50MB',
    unit: 'MB',
  };
}

/**
 * Benchmark: Memory growth under workflow load
 * @param {Object} options - Benchmark options
 * @param {number} [options.workflowCount=1000] - Workflows to execute
 * @param {number} [options.samplingInterval=100] - Sample every N workflows
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkMemoryGrowth(options = {}) {
  const { workflowCount = 1000, samplingInterval = 100 } = options;
  const memorySnapshots = [];

  forceGC();
  await sleep(100);

  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'memory-growth',
    maxConcurrent: 10,
  });

  await daemon.start();

  const baselineMemory = getMemoryUsage();
  memorySnapshots.push({
    workflowsExecuted: 0,
    ...baselineMemory,
  });

  for (let i = 0; i < workflowCount; i++) {
    const workflowId = `growth-wf-${i}`;

    daemon.schedule({
      id: workflowId,
      handler: async () => {
        // Simulate some data processing
        const data = { id: workflowId, index: i, payload: new Array(100).fill(i) };
        await new Promise(resolve => setTimeout(resolve, 5));
        return data;
      },
    });

    await daemon.execute(workflowId).catch(() => {});

    // Sample memory periodically
    if (i % samplingInterval === 0 && i > 0) {
      forceGC();
      await sleep(10);

      const currentMemory = getMemoryUsage();
      memorySnapshots.push({
        workflowsExecuted: i,
        ...currentMemory,
      });
    }
  }

  await daemon.stop();

  // Final memory check
  forceGC();
  await sleep(100);
  const finalMemory = getMemoryUsage();

  // Calculate memory growth rate
  const initialHeap = memorySnapshots[0].heapUsedMB;
  const finalHeap = memorySnapshots[memorySnapshots.length - 1].heapUsedMB;
  const memoryGrowth = finalHeap - initialHeap;
  const growthRate = memoryGrowth / workflowCount; // MB per workflow

  // Check for memory leaks (final should be close to initial after GC)
  const afterStopMemory = finalMemory.heapUsedMB;
  const memoryLeaked = afterStopMemory - baselineMemory.heapUsedMB;

  return {
    name: 'memory-growth-under-load',
    workflowCount,
    samplingInterval,
    memory: {
      initialMB: initialHeap,
      finalMB: finalHeap,
      growthMB: memoryGrowth,
      growthRateMBPerWorkflow: growthRate,
      afterStopMB: afterStopMemory,
      leakedMB: memoryLeaked,
    },
    snapshots: memorySnapshots,
    passed: growthRate < 0.01 && memoryLeaked < 20, // <0.01MB/workflow, <20MB leak
    target: '<0.01MB/wf growth, <20MB leak',
    unit: 'MB',
  };
}

/**
 * Benchmark: Memory stability over time
 * @param {Object} options - Benchmark options
 * @param {number} [options.duration=10000] - Duration in milliseconds
 * @param {number} [options.workflowRate=50] - Workflows per second
 * @param {number} [options.samplingInterval=1000] - Sample every N ms
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkMemoryStability(options = {}) {
  const { duration = 10000, workflowRate = 50, samplingInterval = 1000 } = options;
  const memoryTimeSeries = [];

  forceGC();
  await sleep(100);

  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'memory-stability',
    maxConcurrent: 20,
  });

  await daemon.start();

  const startTime = Date.now();
  const endTime = startTime + duration;
  let workflowCount = 0;

  // Start memory sampling
  const samplingTimer = setInterval(() => {
    const currentMemory = getMemoryUsage();
    memoryTimeSeries.push({
      timestamp: Date.now() - startTime,
      workflowCount,
      ...currentMemory,
    });
  }, samplingInterval);

  // Generate continuous workflow load
  while (Date.now() < endTime) {
    const workflowId = `stability-wf-${workflowCount}`;

    daemon.schedule({
      id: workflowId,
      handler: async () => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return { id: workflowId };
      },
    });

    daemon.execute(workflowId).catch(() => {});
    workflowCount++;

    // Rate limiting
    await sleep(1000 / workflowRate);
  }

  clearInterval(samplingTimer);
  await daemon.stop();

  // Analyze memory stability
  const heapValues = memoryTimeSeries.map(s => s.heapUsedMB);
  const avgHeap = heapValues.reduce((sum, v) => sum + v, 0) / heapValues.length;
  const minHeap = Math.min(...heapValues);
  const maxHeap = Math.max(...heapValues);
  const heapVariance = ((maxHeap - minHeap) / avgHeap) * 100;

  // Check for monotonic growth (potential leak)
  let monotonicGrowth = true;
  for (let i = 1; i < heapValues.length; i++) {
    if (heapValues[i] < heapValues[i - 1]) {
      monotonicGrowth = false;
      break;
    }
  }

  return {
    name: 'memory-stability-over-time',
    duration,
    workflowRate,
    workflowCount,
    samplingInterval,
    memory: {
      avgHeapMB: avgHeap,
      minHeapMB: minHeap,
      maxHeapMB: maxHeap,
      heapVariancePercent: heapVariance,
      monotonicGrowth,
    },
    timeSeries: memoryTimeSeries,
    passed: !monotonicGrowth && heapVariance < 50, // No monotonic growth, <50% variance
    target: 'stable memory, <50% variance',
    unit: 'percent',
  };
}

/**
 * Benchmark: GC pressure and impact
 * @param {Object} options - Benchmark options
 * @param {number} [options.workflowCount=500] - Workflows to execute
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkGCPressure(options = {}) {
  const { workflowCount = 500 } = options;

  if (!global.gc) {
    return {
      name: 'gc-pressure',
      error: 'GC not available (run with --expose-gc)',
      passed: false,
    };
  }

  forceGC();
  await sleep(100);

  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'gc-pressure',
    maxConcurrent: 20,
  });

  await daemon.start();

  const gcCountsBefore = process.memoryUsage();
  const startTime = performance.now();

  for (let i = 0; i < workflowCount; i++) {
    const workflowId = `gc-wf-${i}`;

    daemon.schedule({
      id: workflowId,
      handler: async () => {
        // Create garbage
        const largeArray = new Array(1000).fill({ data: i });
        await new Promise(resolve => setTimeout(resolve, 5));
        return { processed: largeArray.length };
      },
    });

    await daemon.execute(workflowId).catch(() => {});

    // Force GC every 100 workflows
    if (i % 100 === 0) {
      forceGC();
    }
  }

  const executionTime = performance.now() - startTime;
  const gcCountsAfter = process.memoryUsage();

  await daemon.stop();

  const throughput = (workflowCount / executionTime) * 1000;

  return {
    name: 'gc-pressure',
    workflowCount,
    executionTime,
    throughput,
    memory: {
      beforeMB: gcCountsBefore.heapUsed / 1024 / 1024,
      afterMB: gcCountsAfter.heapUsed / 1024 / 1024,
    },
    passed: throughput > 50, // Should maintain >50 wf/s with GC
    target: '>50 wf/s with GC',
    unit: 'workflows/sec',
  };
}

/**
 * Run all memory footprint benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runMemoryBenchmarks() {
  console.log('Running Memory Footprint Benchmarks...\n');

  const results = {
    baselineMemory: await benchmarkBaselineMemory(),
    memoryGrowth: await benchmarkMemoryGrowth(),
    memoryStability: await benchmarkMemoryStability(),
    gcPressure: await benchmarkGCPressure(),
  };

  const allPassed = Object.values(results).every(r => r.passed);

  return {
    name: 'memory-footprint-suite',
    timestamp: new Date().toISOString(),
    gcAvailable: !!global.gc,
    gcWarning: !global.gc ? 'Run with --expose-gc for accurate memory profiling' : null,
    results,
    summary: {
      total: Object.keys(results).length,
      passed: Object.values(results).filter(r => r.passed).length,
      failed: Object.values(results).filter(r => !r.passed).length,
    },
    passed: allPassed,
  };
}
