/**
 * @file Benchmark 3: Concurrent Execution Throughput
 * @description Measures throughput with concurrent hook execution
 *
 * Targets:
 * - Throughput: > 1000 ops/sec at 10 workers
 * - Test with 10, 100, 1000 concurrent workers
 * - Measure parallelization efficiency
 */

import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { createHookExecutor } from '../../src/knowledge-engine/hook-executor.mjs';
import { createStore } from '@unrdf/oxigraph';

/**
 * Benchmark concurrent hook execution
 * @returns {Promise<Object>} Benchmark results
 */
export async function runConcurrentExecutionBenchmark() {
  const results = {
    name: 'concurrent-execution',
    tests: [],
    summary: {},
    passed: false,
  };

  const hook = defineHook({
    meta: { name: 'concurrent-hook', description: 'Concurrent execution test' },
    when: {
      kind: 'sparql-ask',
      ref: {
        uri: 'file://bench/concurrent.rq',
        sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
        mediaType: 'application/sparql-query',
      },
    },
    run: async ({ payload }) => {
      // Simulate some work
      let sum = 0;
      for (let i = 0; i < 100; i++) {
        sum += Math.sqrt(i);
      }
      return { result: { sum, id: payload.id } };
    },
  });

  const executor = createHookExecutor({
    basePath: process.cwd(),
    strictMode: false,
    enableConditionEvaluation: false,
    enableMetrics: true,
  });

  const store = createStore();
  const concurrencyLevels = [
    { workers: 10, name: 'low' },
    { workers: 100, name: 'medium' },
    { workers: 1000, name: 'high' },
  ];

  for (const level of concurrencyLevels) {
    const startTime = performance.now();
    let successCount = 0;
    let errorCount = 0;

    // Execute hooks concurrently
    const promises = [];
    for (let i = 0; i < level.workers; i++) {
      const event = {
        name: hook.meta.name,
        payload: { id: i },
        context: { graph: store, env: {} },
      };

      const promise = executor.execute(hook, event)
        .then(result => {
          if (result.success) successCount++;
          else errorCount++;
        })
        .catch(() => {
          errorCount++;
        });

      promises.push(promise);
    }

    await Promise.all(promises);
    const totalTime = performance.now() - startTime;
    const throughput = (level.workers / totalTime) * 1000;
    const errorRate = errorCount / level.workers;
    const parallelizationEfficiency = throughput / level.workers;

    const testResult = {
      concurrencyLevel: level.name,
      workers: level.workers,
      totalTimeMs: totalTime,
      successCount,
      errorCount,
      throughputOpsPerSec: throughput,
      errorRate,
      parallelizationEfficiency,
      passed: throughput > 1000 && errorRate < 0.01,
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
