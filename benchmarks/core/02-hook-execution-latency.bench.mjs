/**
 * @file Benchmark 2: Hook Execution Latency
 * @description Measures execution latency for hook.run() phase
 *
 * Targets:
 * - p95: < 50ms
 * - p99: < 100ms
 * - Throughput: > 100 hooks/sec
 * - Test 3 complexity levels: Simple, Medium, Complex
 */

import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { createHookExecutor } from '../../src/knowledge-engine/hook-executor.mjs';
import { createStore } from '@unrdf/oxigraph';

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
 * Create hooks with different complexity levels
 */
function createHooks() {
  const simpleHook = defineHook({
    meta: { name: 'simple-hook', description: 'Simple computation' },
    when: {
      kind: 'sparql-ask',
      ref: {
        uri: 'file://bench/simple.rq',
        sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
        mediaType: 'application/sparql-query',
      },
    },
    run: async ({ payload }) => {
      return { result: { count: 1 } };
    },
  });

  const mediumHook = defineHook({
    meta: { name: 'medium-hook', description: 'Medium computation' },
    when: {
      kind: 'sparql-ask',
      ref: {
        uri: 'file://bench/medium.rq',
        sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
        mediaType: 'application/sparql-query',
      },
    },
    run: async ({ payload }) => {
      let sum = 0;
      for (let i = 0; i < 1000; i++) {
        sum += Math.sqrt(i);
      }
      return { result: { sum } };
    },
  });

  const complexHook = defineHook({
    meta: { name: 'complex-hook', description: 'Complex computation' },
    when: {
      kind: 'sparql-ask',
      ref: {
        uri: 'file://bench/complex.rq',
        sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
        mediaType: 'application/sparql-query',
      },
    },
    run: async ({ payload }) => {
      let sum = 0;
      for (let i = 0; i < 10000; i++) {
        sum += Math.sqrt(i) * Math.log(i + 1);
      }
      return { result: { sum } };
    },
  });

  return { simpleHook, mediumHook, complexHook };
}

/**
 * Benchmark hook execution latency
 * @returns {Promise<Object>} Benchmark results
 */
export async function runHookExecutionLatencyBenchmark() {
  const results = {
    name: 'hook-execution-latency',
    tests: [],
    summary: {},
    passed: false,
  };

  const hooks = createHooks();
  const executor = createHookExecutor({
    basePath: process.cwd(),
    strictMode: false,
    enableConditionEvaluation: false,
    enableMetrics: true,
  });

  const store = createStore();
  const complexityLevels = [
    { hook: hooks.simpleHook, name: 'simple', iterations: 1000 },
    { hook: hooks.mediumHook, name: 'medium', iterations: 500 },
    { hook: hooks.complexHook, name: 'complex', iterations: 100 },
  ];

  for (const level of complexityLevels) {
    const latencies = [];
    const startTime = performance.now();

    for (let i = 0; i < level.iterations; i++) {
      const event = {
        name: level.hook.meta.name,
        payload: { iteration: i },
        context: { graph: store, env: {} },
      };

      const iterStartTime = performance.now();
      await executor.execute(level.hook, event);
      const latency = performance.now() - iterStartTime;
      latencies.push(latency);
    }

    const totalTime = performance.now() - startTime;
    const sortedLatencies = latencies.sort((a, b) => a - b);

    const p50 = getPercentile(sortedLatencies, 50);
    const p95 = getPercentile(sortedLatencies, 95);
    const p99 = getPercentile(sortedLatencies, 99);
    const mean = latencies.reduce((sum, v) => sum + v, 0) / latencies.length;
    const throughput = (level.iterations / totalTime) * 1000;

    const testResult = {
      complexity: level.name,
      iterations: level.iterations,
      totalTimeMs: totalTime,
      latency: {
        p50,
        p95,
        p99,
        mean,
      },
      throughputOpsPerSec: throughput,
      passed: p95 < 50 && p99 < 100 && throughput > 100,
    };

    results.tests.push(testResult);
  }

  // Summary
  const allPassed = results.tests.every(t => t.passed);
  results.summary = {
    totalTests: results.tests.length,
    passed: results.tests.filter(t => t.passed).length,
    failed: results.tests.filter(t => !t.passed).length,
  };
  results.passed = allPassed;

  return results;
}
