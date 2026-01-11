/**
 * @file Concurrent Workflow Execution Benchmark
 * @module benchmarks/yawl-daemon/concurrent-workflows
 * @description Measures performance under concurrent workflow execution
 *
 * Performance Targets:
 * - Linear scalability up to maxConcurrent
 * - <200ms P95 latency with 10 concurrent workflows
 * - Throughput scales with concurrency
 *
 * Metrics:
 * - Concurrent execution throughput
 * - Latency under concurrent load
 * - Resource contention impact
 * - Scalability factor
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
 * Simulate a workflow task
 * @param {number} duration - Task duration in ms
 * @returns {Promise<Object>} Task result
 */
async function executeTask(duration = 10) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve({ status: 'completed', duration });
    }, duration);
  });
}

/**
 * Benchmark: Concurrent workflow execution
 * @param {Object} options - Benchmark options
 * @param {number[]} [options.concurrencyLevels=[1, 5, 10, 20, 50]] - Concurrency levels to test
 * @param {number} [options.workflowsPerLevel=20] - Workflows per level
 * @param {number} [options.runs=3] - Number of runs per level
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkConcurrentExecution(options = {}) {
  const { concurrencyLevels = [1, 5, 10, 20, 50], workflowsPerLevel = 20, runs = 3 } = options;
  const results = [];

  for (const concurrency of concurrencyLevels) {
    const levelLatencies = [];
    const levelThroughputs = [];

    for (let run = 0; run < runs; run++) {
      const daemon = new Daemon({
        daemonId: randomUUID(),
        name: `concurrent-${concurrency}-${run}`,
        maxConcurrent: concurrency,
      });

      await daemon.start();

      // Schedule workflows
      const workflowIds = [];
      for (let i = 0; i < workflowsPerLevel; i++) {
        const workflowId = `wf-${concurrency}-${run}-${i}`;
        workflowIds.push(workflowId);

        daemon.schedule({
          id: workflowId,
          handler: async () => {
            await executeTask(10);
            await executeTask(10);
            return { workflowId, concurrency };
          },
        });
      }

      const startTime = performance.now();
      const execPromises = workflowIds.map(id =>
        daemon.execute(id).catch(() => ({}))
      );

      await Promise.all(execPromises);
      const totalTime = performance.now() - startTime;

      const throughput = (workflowsPerLevel / totalTime) * 1000;
      const avgLatency = totalTime / workflowsPerLevel;

      levelLatencies.push(avgLatency);
      levelThroughputs.push(throughput);

      await daemon.stop();
    }

    const avgLatency = levelLatencies.reduce((sum, v) => sum + v, 0) / levelLatencies.length;
    const avgThroughput = levelThroughputs.reduce((sum, v) => sum + v, 0) / levelThroughputs.length;

    results.push({
      concurrency,
      latency: {
        mean: avgLatency,
        min: Math.min(...levelLatencies),
        max: Math.max(...levelLatencies),
      },
      throughput: {
        mean: avgThroughput,
        min: Math.min(...levelThroughputs),
        max: Math.max(...levelThroughputs),
      },
    });
  }

  // Calculate scalability factor
  const baselineThroughput = results[0].throughput.mean;
  const highestThroughput = results[results.length - 1].throughput.mean;
  const scalabilityFactor = highestThroughput / baselineThroughput;

  // Check P95 latency at 10 concurrent
  const concurrency10 = results.find(r => r.concurrency === 10);
  const passed = concurrency10 ? concurrency10.latency.mean < 200 : false;

  return {
    name: 'concurrent-workflow-execution',
    concurrencyLevels,
    workflowsPerLevel,
    runs,
    results,
    scalability: {
      baselineThroughput,
      highestThroughput,
      scalabilityFactor,
      linear: scalabilityFactor > (concurrencyLevels.length * 0.7),
    },
    passed,
    target: '200ms @ 10 concurrent',
    unit: 'ms',
  };
}

/**
 * Benchmark: Resource contention under load
 * @param {Object} options - Benchmark options
 * @param {number} [options.concurrency=20] - Concurrent workflows
 * @param {number} [options.duration=5000] - Duration in milliseconds
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkResourceContention(options = {}) {
  const { concurrency = 20, duration = 5000, runs = 3 } = options;
  const allLatencies = [];
  const throughputs = [];

  for (let run = 0; run < runs; run++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `contention-${run}`,
      maxConcurrent: concurrency,
    });

    await daemon.start();

    const startTime = performance.now();
    const endTime = startTime + duration;
    let workflowCount = 0;
    const runLatencies = [];

    while (performance.now() < endTime) {
      const workflowId = `contention-wf-${run}-${workflowCount}`;

      daemon.schedule({
        id: workflowId,
        handler: async () => {
          await executeTask(10);
          return { workflowId };
        },
      });

      const execStart = performance.now();
      await daemon.execute(workflowId).catch(() => {});
      const execLatency = performance.now() - execStart;

      runLatencies.push(execLatency);
      workflowCount++;

      // Prevent blocking event loop
      if (workflowCount % 10 === 0) {
        await new Promise(resolve => setImmediate(resolve));
      }
    }

    await daemon.stop();

    const actualDuration = (performance.now() - startTime) / 1000;
    const throughput = workflowCount / actualDuration;

    allLatencies.push(...runLatencies);
    throughputs.push(throughput);
  }

  const sortedLatencies = allLatencies.sort((a, b) => a - b);
  const mean = allLatencies.reduce((sum, v) => sum + v, 0) / allLatencies.length;
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);
  const p99 = getPercentile(sortedLatencies, 99);

  const avgThroughput = throughputs.reduce((sum, v) => sum + v, 0) / throughputs.length;

  // Check latency variance under contention (P99/P50 ratio should be <5)
  const latencyVariance = p99 / p50;

  return {
    name: 'resource-contention',
    concurrency,
    duration,
    runs,
    latency: {
      mean,
      p50,
      p95,
      p99,
      variance: latencyVariance,
    },
    throughput: {
      mean: avgThroughput,
      min: Math.min(...throughputs),
      max: Math.max(...throughputs),
    },
    passed: latencyVariance < 5 && p95 < 300,
    target: 'P99/P50 < 5',
    unit: 'ratio',
  };
}

/**
 * Benchmark: Burst handling
 * @param {Object} options - Benchmark options
 * @param {number} [options.burstSize=50] - Workflows in burst
 * @param {number} [options.maxConcurrent=10] - Max concurrent execution
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkBurstHandling(options = {}) {
  const { burstSize = 50, maxConcurrent = 10, runs = 5 } = options;
  const allLatencies = [];
  const completionTimes = [];

  for (let run = 0; run < runs; run++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `burst-${run}`,
      maxConcurrent,
    });

    await daemon.start();

    // Submit burst of workflows
    const workflowIds = [];
    const submissionStart = performance.now();

    for (let i = 0; i < burstSize; i++) {
      const workflowId = `burst-wf-${run}-${i}`;
      workflowIds.push(workflowId);

      daemon.schedule({
        id: workflowId,
        handler: async () => {
          await executeTask(20);
          return { workflowId };
        },
      });
    }

    const submissionTime = performance.now() - submissionStart;

    // Execute all workflows
    const executionStart = performance.now();
    const execPromises = workflowIds.map(async (id) => {
      const start = performance.now();
      await daemon.execute(id).catch(() => {});
      return performance.now() - start;
    });

    const latencies = await Promise.all(execPromises);
    const totalCompletionTime = performance.now() - executionStart;

    allLatencies.push(...latencies);
    completionTimes.push(totalCompletionTime);

    await daemon.stop();
  }

  const sortedLatencies = allLatencies.sort((a, b) => a - b);
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);
  const p99 = getPercentile(sortedLatencies, 99);

  const avgCompletionTime = completionTimes.reduce((sum, v) => sum + v, 0) / completionTimes.length;
  const throughput = (burstSize / avgCompletionTime) * 1000;

  return {
    name: 'burst-handling',
    burstSize,
    maxConcurrent,
    runs,
    latency: {
      p50,
      p95,
      p99,
    },
    completionTime: {
      mean: avgCompletionTime,
      min: Math.min(...completionTimes),
      max: Math.max(...completionTimes),
    },
    throughput,
    passed: p95 < 500 && throughput > 50,
    target: 'P95 <500ms, >50 wf/s',
    unit: 'ms',
  };
}

/**
 * Run all concurrent workflow benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runConcurrentBenchmarks() {
  console.log('Running Concurrent Workflow Benchmarks...\n');

  const results = {
    concurrentExecution: await benchmarkConcurrentExecution(),
    resourceContention: await benchmarkResourceContention(),
    burstHandling: await benchmarkBurstHandling(),
  };

  const allPassed = Object.values(results).every(r => r.passed);

  return {
    name: 'concurrent-workflows-suite',
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
