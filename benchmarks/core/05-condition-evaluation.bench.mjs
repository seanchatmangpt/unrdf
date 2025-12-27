/**
 * @file Benchmark 5: Condition Evaluation Performance
 * @description Measures SPARQL condition evaluation time
 *
 * Targets:
 * - Throughput: > 10000 ops/sec
 * - Cache hit rate: 80%
 * - Test SPARQL ASK evaluation
 */

import { createConditionEvaluator } from '../../src/knowledge-engine/condition-evaluator.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

/**
 * Create a test graph with sample data
 * @param {number} size - Number of quads to add
 * @returns {Store} Populated store
 */
function createTestGraph(size) {
  const store = createStore();

  for (let i = 0; i < size; i++) {
    const subject = namedNode(`http://example.org/subject${i}`);
    const predicate = namedNode('http://example.org/predicate');
    const object = literal(`value${i}`);
    store.add(quad(subject, predicate, object));
  }

  return store;
}

/**
 * Benchmark condition evaluation
 * @returns {Promise<Object>} Benchmark results
 */
export async function runConditionEvaluationBenchmark() {
  const results = {
    name: 'condition-evaluation',
    tests: [],
    summary: {},
    passed: false,
  };

  const evaluator = createConditionEvaluator({
    basePath: process.cwd(),
    enableCache: true,
    cacheMaxAge: 60000,
    strictMode: false,
  });

  const testCases = [
    { graphSize: 100, iterations: 1000, name: 'small-graph' },
    { graphSize: 1000, iterations: 500, name: 'medium-graph' },
    { graphSize: 10000, iterations: 100, name: 'large-graph' },
  ];

  for (const testCase of testCases) {
    const store = createTestGraph(testCase.graphSize);

    const condition = {
      kind: 'sparql-ask',
      query: 'ASK { ?s ?p ?o }',
    };

    let cacheHits = 0;
    let cacheMisses = 0;
    const latencies = [];
    const startTime = performance.now();

    // First iteration (cache miss)
    for (let i = 0; i < testCase.iterations; i++) {
      const iterStartTime = performance.now();
      const result = await evaluator.evaluate(condition, store, {});
      const latency = performance.now() - iterStartTime;
      latencies.push(latency);

      // Approximate cache detection (very fast = cache hit)
      if (latency < 0.1) cacheHits++;
      else cacheMisses++;
    }

    const totalTime = performance.now() - startTime;
    const throughput = (testCase.iterations / totalTime) * 1000;
    const cacheHitRate = cacheHits / testCase.iterations;
    const avgLatency = latencies.reduce((sum, v) => sum + v, 0) / latencies.length;

    // Clear cache for next test
    evaluator.clearCache();

    const testResult = {
      testCase: testCase.name,
      graphSize: testCase.graphSize,
      iterations: testCase.iterations,
      totalTimeMs: totalTime,
      avgLatencyMs: avgLatency,
      throughputOpsPerSec: throughput,
      cacheHits,
      cacheMisses,
      cacheHitRate,
      passed: throughput > 10000 || cacheHitRate > 0.8,
    };

    results.tests.push(testResult);
  }

  // Summary
  const allPassed = results.tests.every(t => t.passed);
  const avgThroughput = results.tests.reduce((sum, t) => sum + t.throughputOpsPerSec, 0) / results.tests.length;
  const avgCacheHitRate = results.tests.reduce((sum, t) => sum + t.cacheHitRate, 0) / results.tests.length;

  results.summary = {
    totalTests: results.tests.length,
    passed: results.tests.filter(t => t.passed).length,
    failed: results.tests.filter(t => !t.passed).length,
    avgThroughputOpsPerSec: avgThroughput,
    avgCacheHitRate,
  };
  results.passed = allPassed;

  return results;
}
