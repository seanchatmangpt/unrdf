/**
 * @file Benchmark 1: Hook Registration Performance
 * @description Measures time to register N hooks and throughput
 *
 * Targets:
 * - Latency: < 1ms per hook average
 * - Throughput: > 1000 hooks/sec
 * - Memory: < 50MB per 1000 hooks
 * - Metrics: p50, p95, p99 latency + throughput
 */

import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
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
 * Benchmark hook registration at different scales
 * @returns {Promise<Object>} Benchmark results
 */
export async function runHookRegistrationBenchmark() {
  const results = {
    name: 'hook-registration',
    tests: [],
    summary: {},
    passed: false,
  };

  const testScales = [
    { count: 10, name: 'small' },
    { count: 50, name: 'medium' },
    { count: 100, name: 'large' },
  ];

  for (const scale of testScales) {
    const latencies = [];
    const startTime = performance.now();
    const manager = new KnowledgeHookManager({
      basePath: process.cwd(),
      enableKnowledgeHooks: true,
      strictMode: false,
    });

    // Register N hooks
    for (let i = 0; i < scale.count; i++) {
      const hookStartTime = performance.now();

      const hook = defineHook({
        meta: {
          name: `bench-hook-${i}`,
          description: `Benchmark hook ${i}`,
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://bench/${i}.rq`,
            sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
            mediaType: 'application/sparql-query',
          },
        },
        run: async ({ payload }) => {
          return { result: { processed: true, data: payload } };
        },
      });

      manager.addKnowledgeHook(hook);
      const hookLatency = performance.now() - hookStartTime;
      latencies.push(hookLatency);
    }

    const totalTime = performance.now() - startTime;
    const sortedLatencies = latencies.sort((a, b) => a - b);

    const p50 = getPercentile(sortedLatencies, 50);
    const p95 = getPercentile(sortedLatencies, 95);
    const p99 = getPercentile(sortedLatencies, 99);
    const mean = latencies.reduce((sum, v) => sum + v, 0) / latencies.length;
    const throughput = (scale.count / totalTime) * 1000;

    const testResult = {
      scale: scale.name,
      hookCount: scale.count,
      totalTimeMs: totalTime,
      latency: {
        p50,
        p95,
        p99,
        mean,
      },
      throughputHooksPerSec: throughput,
      passed: p95 < 1.0 && throughput > 1000,
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
