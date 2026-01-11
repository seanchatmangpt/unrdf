/**
 * @file YAWL Workflow Execution Benchmark
 * @module @unrdf/daemon/benchmarks/yawl-execution
 * @description Measures YAWL workflow execution performance in daemon
 * Tests sequential, parallel, and conditional workflow patterns
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
 * Simulate workflow task execution
 * @param {number} duration - Task duration in milliseconds
 * @returns {Promise<Object>} Task result
 */
async function executeWorkflowTask(duration = 10) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve({ status: 'completed', duration });
    }, duration);
  });
}

/**
 * Benchmark: Sequential workflow execution
 * @param {Object} options - Benchmark options
 * @param {number} [options.stepsPerWorkflow=5] - Tasks per workflow
 * @param {number} [options.workflowCount=20] - Number of workflows
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with sequential workflow metrics
 */
export async function benchmarkSequentialWorkflow(options = {}) {
  const { stepsPerWorkflow = 5, workflowCount = 20, runs = 5 } = options;
  const executionTimes = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `yawl-seq-${runIdx}`,
    });
    await daemon.start();

    const workflowIds = [];

    for (let wIdx = 0; wIdx < workflowCount; wIdx++) {
      const workflowId = `seq-workflow-${wIdx}`;
      workflowIds.push(workflowId);

      daemon.schedule({
        id: workflowId,
        name: `Sequential Workflow ${wIdx}`,
        handler: async () => {
          const results = [];

          for (let stepIdx = 0; stepIdx < stepsPerWorkflow; stepIdx++) {
            const stepResult = await executeWorkflowTask(5);
            results.push(stepResult);
          }

          return {
            workflowId,
            steps: stepsPerWorkflow,
            results,
          };
        },
      });
    }

    const startTime = performance.now();

    await Promise.all(
      workflowIds.map(wId => daemon.execute(wId).catch(() => {}))
    );

    const endTime = performance.now();
    executionTimes.push(endTime - startTime);

    await daemon.stop();
  }

  const variance = analyzeVariance(executionTimes);

  return storeBenchmarkResult({
    name: 'yawl-sequential-workflow',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: executionTimes.length,
    stepsPerWorkflow,
    workflowCount,
    runs,
  });
}

/**
 * Benchmark: Parallel workflow execution
 * @param {Object} options - Benchmark options
 * @param {number} [options.parallelTasks=5] - Tasks to run in parallel
 * @param {number} [options.workflowCount=20] - Number of workflows
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with parallel workflow metrics
 */
export async function benchmarkParallelWorkflow(options = {}) {
  const { parallelTasks = 5, workflowCount = 20, runs = 5 } = options;
  const executionTimes = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `yawl-par-${runIdx}`,
    });
    await daemon.start();

    const workflowIds = [];

    for (let wIdx = 0; wIdx < workflowCount; wIdx++) {
      const workflowId = `par-workflow-${wIdx}`;
      workflowIds.push(workflowId);

      daemon.schedule({
        id: workflowId,
        name: `Parallel Workflow ${wIdx}`,
        handler: async () => {
          const taskPromises = [];

          for (let taskIdx = 0; taskIdx < parallelTasks; taskIdx++) {
            taskPromises.push(executeWorkflowTask(10));
          }

          const results = await Promise.all(taskPromises);

          return {
            workflowId,
            parallelTasks,
            results,
          };
        },
      });
    }

    const startTime = performance.now();

    await Promise.all(
      workflowIds.map(wId => daemon.execute(wId).catch(() => {}))
    );

    const endTime = performance.now();
    executionTimes.push(endTime - startTime);

    await daemon.stop();
  }

  const variance = analyzeVariance(executionTimes);

  return storeBenchmarkResult({
    name: 'yawl-parallel-workflow',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: executionTimes.length,
    parallelTasks,
    workflowCount,
    runs,
  });
}

/**
 * Benchmark: Conditional workflow branching
 * @param {Object} options - Benchmark options
 * @param {number} [options.branchingFactor=3] - Number of possible branches
 * @param {number} [options.workflowCount=20] - Number of workflows
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with conditional workflow metrics
 */
export async function benchmarkConditionalWorkflow(options = {}) {
  const { branchingFactor = 3, workflowCount = 20, runs = 5 } = options;
  const executionTimes = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `yawl-cond-${runIdx}`,
    });
    await daemon.start();

    const workflowIds = [];

    for (let wIdx = 0; wIdx < workflowCount; wIdx++) {
      const workflowId = `cond-workflow-${wIdx}`;
      workflowIds.push(workflowId);

      daemon.schedule({
        id: workflowId,
        name: `Conditional Workflow ${wIdx}`,
        handler: async () => {
          const condition = Math.floor(Math.random() * branchingFactor);
          const tasks = [];

          // Common initial task
          tasks.push(executeWorkflowTask(5));

          // Branching based on condition
          for (let i = 0; i < condition + 1; i++) {
            tasks.push(executeWorkflowTask(5));
          }

          const results = await Promise.all(tasks);

          return {
            workflowId,
            condition,
            branch: condition,
            results,
          };
        },
      });
    }

    const startTime = performance.now();

    await Promise.all(
      workflowIds.map(wId => daemon.execute(wId).catch(() => {}))
    );

    const endTime = performance.now();
    executionTimes.push(endTime - startTime);

    await daemon.stop();
  }

  const variance = analyzeVariance(executionTimes);

  return storeBenchmarkResult({
    name: 'yawl-conditional-workflow',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: executionTimes.length,
    branchingFactor,
    workflowCount,
    runs,
  });
}

/**
 * Benchmark: Mixed workflow complexity
 * @param {Object} options - Benchmark options
 * @param {number} [options.workflowCount=30] - Number of workflows
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with mixed workflow metrics
 */
export async function benchmarkMixedWorkflow(options = {}) {
  const { workflowCount = 30, runs = 3 } = options;
  const throughputs = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `yawl-mixed-${runIdx}`,
    });
    await daemon.start();

    const workflowIds = [];

    for (let wIdx = 0; wIdx < workflowCount; wIdx++) {
      const workflowId = `mixed-workflow-${wIdx}`;
      const workflowType = wIdx % 3;
      workflowIds.push(workflowId);

      if (workflowType === 0) {
        // Sequential workflow
        daemon.schedule({
          id: workflowId,
          handler: async () => {
            for (let i = 0; i < 3; i++) {
              await executeWorkflowTask(5);
            }
            return { type: 'sequential' };
          },
        });
      } else if (workflowType === 1) {
        // Parallel workflow
        daemon.schedule({
          id: workflowId,
          handler: async () => {
            await Promise.all([
              executeWorkflowTask(5),
              executeWorkflowTask(5),
              executeWorkflowTask(5),
            ]);
            return { type: 'parallel' };
          },
        });
      } else {
        // Conditional workflow
        daemon.schedule({
          id: workflowId,
          handler: async () => {
            const condition = Math.random() > 0.5;
            if (condition) {
              await executeWorkflowTask(10);
            } else {
              await executeWorkflowTask(5);
            }
            return { type: 'conditional', condition };
          },
        });
      }
    }

    const startTime = performance.now();

    await Promise.all(
      workflowIds.map(wId => daemon.execute(wId).catch(() => {}))
    );

    const endTime = performance.now();
    const durationSecs = (endTime - startTime) / 1000;
    const throughput = workflowCount / durationSecs;

    throughputs.push(throughput);
    await daemon.stop();
  }

  const variance = analyzeVariance(throughputs);

  return storeBenchmarkResult({
    name: 'yawl-mixed-workflow-throughput',
    type: 'throughput',
    unit: 'workflows/sec',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: throughputs.length,
    workflowCount,
    runs,
  });
}
