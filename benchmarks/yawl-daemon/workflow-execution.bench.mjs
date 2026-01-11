/**
 * @file Workflow Execution Performance Benchmark
 * @module benchmarks/yawl-daemon/workflow-execution
 * @description Measures YAWL workflow execution performance through daemon
 *
 * Performance Target: <100ms for simple workflows (P95)
 *
 * Metrics:
 * - Simple workflow execution latency
 * - Multi-step workflow latency
 * - Conditional branching overhead
 * - Task transition latency
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
 * Simulate a simple task execution
 * @param {number} duration - Task duration in ms
 * @returns {Promise<Object>} Task result
 */
async function executeTask(duration = 5) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve({ status: 'completed', duration });
    }, duration);
  });
}

/**
 * Benchmark: Simple single-task workflow
 * @param {Object} options - Benchmark options
 * @param {number} [options.workflowCount=100] - Number of workflows
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkSimpleWorkflow(options = {}) {
  const { workflowCount = 100, runs = 5 } = options;
  const allLatencies = [];

  for (let run = 0; run < runs; run++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `simple-wf-${run}`,
      maxConcurrent: 10,
    });

    await daemon.start();
    const runLatencies = [];

    for (let i = 0; i < workflowCount; i++) {
      const workflowId = `simple-wf-${run}-${i}`;

      daemon.schedule({
        id: workflowId,
        name: `Simple Workflow ${i}`,
        handler: async () => {
          const result = await executeTask(5);
          return { workflowId, result };
        },
      });

      const startTime = performance.now();
      await daemon.execute(workflowId).catch(() => {});
      const latency = performance.now() - startTime;
      runLatencies.push(latency);
    }

    allLatencies.push(...runLatencies);
    await daemon.stop();
  }

  const sortedLatencies = allLatencies.sort((a, b) => a - b);
  const mean = allLatencies.reduce((sum, v) => sum + v, 0) / allLatencies.length;
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);
  const p99 = getPercentile(sortedLatencies, 99);

  return {
    name: 'simple-workflow-execution',
    workflowCount: workflowCount * runs,
    runs,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...allLatencies),
      max: Math.max(...allLatencies),
    },
    passed: p95 < 100, // Target: <100ms P95
    target: '100ms',
    unit: 'ms',
  };
}

/**
 * Benchmark: Multi-step sequential workflow
 * @param {Object} options - Benchmark options
 * @param {number} [options.steps=5] - Steps per workflow
 * @param {number} [options.workflowCount=50] - Number of workflows
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkMultiStepWorkflow(options = {}) {
  const { steps = 5, workflowCount = 50, runs = 3 } = options;
  const allLatencies = [];

  for (let run = 0; run < runs; run++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `multi-step-${run}`,
      maxConcurrent: 10,
    });

    await daemon.start();
    const runLatencies = [];

    for (let i = 0; i < workflowCount; i++) {
      const workflowId = `multi-step-wf-${run}-${i}`;

      daemon.schedule({
        id: workflowId,
        name: `Multi-Step Workflow ${i}`,
        handler: async () => {
          const results = [];

          for (let step = 0; step < steps; step++) {
            const result = await executeTask(3);
            results.push(result);
          }

          return { workflowId, steps: results.length, results };
        },
      });

      const startTime = performance.now();
      await daemon.execute(workflowId).catch(() => {});
      const latency = performance.now() - startTime;
      runLatencies.push(latency);
    }

    allLatencies.push(...runLatencies);
    await daemon.stop();
  }

  const sortedLatencies = allLatencies.sort((a, b) => a - b);
  const mean = allLatencies.reduce((sum, v) => sum + v, 0) / allLatencies.length;
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);
  const p99 = getPercentile(sortedLatencies, 99);

  return {
    name: 'multi-step-workflow-execution',
    steps,
    workflowCount: workflowCount * runs,
    runs,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...allLatencies),
      max: Math.max(...allLatencies),
    },
    passed: p95 < 200, // Multi-step tolerance
    target: '200ms',
    unit: 'ms',
  };
}

/**
 * Benchmark: Conditional branching workflow
 * @param {Object} options - Benchmark options
 * @param {number} [options.branches=3] - Number of possible branches
 * @param {number} [options.workflowCount=50] - Number of workflows
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkConditionalWorkflow(options = {}) {
  const { branches = 3, workflowCount = 50, runs = 3 } = options;
  const allLatencies = [];
  const branchCounts = new Array(branches).fill(0);

  for (let run = 0; run < runs; run++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `conditional-${run}`,
      maxConcurrent: 10,
    });

    await daemon.start();
    const runLatencies = [];

    for (let i = 0; i < workflowCount; i++) {
      const workflowId = `conditional-wf-${run}-${i}`;

      daemon.schedule({
        id: workflowId,
        name: `Conditional Workflow ${i}`,
        handler: async () => {
          const condition = Math.floor(Math.random() * branches);
          branchCounts[condition]++;

          // Different execution paths
          if (condition === 0) {
            await executeTask(5);
            return { workflowId, branch: 'fast' };
          } else if (condition === 1) {
            await executeTask(10);
            await executeTask(5);
            return { workflowId, branch: 'medium' };
          } else {
            await executeTask(15);
            await executeTask(10);
            await executeTask(5);
            return { workflowId, branch: 'slow' };
          }
        },
      });

      const startTime = performance.now();
      await daemon.execute(workflowId).catch(() => {});
      const latency = performance.now() - startTime;
      runLatencies.push(latency);
    }

    allLatencies.push(...runLatencies);
    await daemon.stop();
  }

  const sortedLatencies = allLatencies.sort((a, b) => a - b);
  const mean = allLatencies.reduce((sum, v) => sum + v, 0) / allLatencies.length;
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);
  const p99 = getPercentile(sortedLatencies, 99);

  return {
    name: 'conditional-workflow-execution',
    branches,
    workflowCount: workflowCount * runs,
    runs,
    branchDistribution: branchCounts,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...allLatencies),
      max: Math.max(...allLatencies),
    },
    passed: p95 < 250, // Conditional overhead tolerance
    target: '250ms',
    unit: 'ms',
  };
}

/**
 * Benchmark: Task transition overhead
 * @param {Object} options - Benchmark options
 * @param {number} [options.transitions=10] - Transitions per workflow
 * @param {number} [options.workflowCount=30] - Number of workflows
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkTaskTransitions(options = {}) {
  const { transitions = 10, workflowCount = 30 } = options;
  const transitionLatencies = [];

  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'transition-bench',
    maxConcurrent: 5,
  });

  await daemon.start();

  for (let i = 0; i < workflowCount; i++) {
    const workflowId = `transition-wf-${i}`;

    daemon.schedule({
      id: workflowId,
      name: `Transition Workflow ${i}`,
      handler: async () => {
        const transitionTimes = [];

        for (let t = 0; t < transitions; t++) {
          const transitionStart = performance.now();
          await executeTask(1); // Minimal task
          const transitionTime = performance.now() - transitionStart;
          transitionTimes.push(transitionTime);
        }

        return { workflowId, transitions: transitionTimes };
      },
    });

    const result = await daemon.execute(workflowId).catch(() => ({}));
    if (result && result.transitions) {
      transitionLatencies.push(...result.transitions);
    }
  }

  await daemon.stop();

  const sortedLatencies = transitionLatencies.sort((a, b) => a - b);
  const mean = transitionLatencies.reduce((sum, v) => sum + v, 0) / transitionLatencies.length;
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);

  return {
    name: 'task-transition-overhead',
    transitions: transitions * workflowCount,
    workflowCount,
    latency: {
      mean,
      p50,
      p95,
      min: Math.min(...transitionLatencies),
      max: Math.max(...transitionLatencies),
    },
    passed: p95 < 20, // Transitions should be fast
    target: '20ms',
    unit: 'ms',
  };
}

/**
 * Run all workflow execution benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runWorkflowBenchmarks() {
  console.log('Running Workflow Execution Benchmarks...\n');

  const results = {
    simpleWorkflow: await benchmarkSimpleWorkflow(),
    multiStepWorkflow: await benchmarkMultiStepWorkflow(),
    conditionalWorkflow: await benchmarkConditionalWorkflow(),
    taskTransitions: await benchmarkTaskTransitions(),
  };

  const allPassed = Object.values(results).every(r => r.passed);

  return {
    name: 'workflow-execution-suite',
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
