/**
 * @file Graph Operations Performance Benchmarks
 * @module benchmarks/rdf-core/04-graph-operations
 * @description Benchmarks for add, remove, find, and count operations
 */

import { performance } from 'perf_hooks';
import { createStore, addQuad, removeQuad, getQuads, countQuads, namedNode, literal } from '@unrdf/core';
import { analyzeVariance } from './suite.mjs';

/**
 * Benchmark add operations
 * @param {number} quadCount - Number of quads to add
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkAdd(quadCount, iterations = 50) {
  const latencies = [];

  for (let i = 0; i < iterations; i++) {
    const store = createStore();
    const start = performance.now();

    for (let j = 0; j < quadCount; j++) {
      addQuad(store, {
        subject: namedNode(`http://example.org/s${j}`),
        predicate: namedNode(`http://example.org/p${j % 10}`),
        object: literal(`Object ${j}`),
      });
    }

    const duration = performance.now() - start;
    latencies.push(duration);
  }

  const stats = analyzeVariance(latencies);
  const throughput = (quadCount * iterations) / (latencies.reduce((a, b) => a + b, 0) / 1000);

  return {
    latency: stats,
    throughput: { mean: throughput },
    operations: quadCount,
    iterations,
  };
}

/**
 * Benchmark remove operations
 * @param {number} quadCount - Number of quads in store
 * @param {number} removeCount - Number of quads to remove
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkRemove(quadCount, removeCount, iterations = 50) {
  const latencies = [];

  for (let i = 0; i < iterations; i++) {
    // Setup store
    const store = createStore();
    for (let j = 0; j < quadCount; j++) {
      addQuad(store, {
        subject: namedNode(`http://example.org/s${j}`),
        predicate: namedNode(`http://example.org/p${j % 10}`),
        object: literal(`Object ${j}`),
      });
    }

    // Measure remove
    const start = performance.now();
    for (let j = 0; j < removeCount; j++) {
      removeQuad(store, {
        subject: namedNode(`http://example.org/s${j}`),
        predicate: namedNode(`http://example.org/p${j % 10}`),
        object: literal(`Object ${j}`),
      });
    }
    const duration = performance.now() - start;
    latencies.push(duration);
  }

  const stats = analyzeVariance(latencies);
  const throughput = (removeCount * iterations) / (latencies.reduce((a, b) => a + b, 0) / 1000);

  return {
    latency: stats,
    throughput: { mean: throughput },
    operations: removeCount,
    iterations,
  };
}

/**
 * Benchmark find operations
 * @param {number} quadCount - Number of quads in store
 * @param {string} queryType - Type of query (all, subject, predicate, pattern)
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkFind(quadCount, queryType, iterations = 100) {
  const store = createStore();

  // Setup store
  for (let j = 0; j < quadCount; j++) {
    addQuad(store, {
      subject: namedNode(`http://example.org/s${j}`),
      predicate: namedNode(`http://example.org/p${j % 10}`),
      object: literal(`Object ${j}`),
    });
  }

  const latencies = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();

    let results;
    switch (queryType) {
      case 'all':
        results = getQuads(store);
        break;
      case 'subject':
        results = getQuads(store, namedNode('http://example.org/s0'));
        break;
      case 'predicate':
        results = getQuads(store, null, namedNode('http://example.org/p0'));
        break;
      case 'pattern':
        results = getQuads(
          store,
          namedNode('http://example.org/s0'),
          namedNode('http://example.org/p0')
        );
        break;
    }

    const duration = performance.now() - start;
    latencies.push(duration);
  }

  const stats = analyzeVariance(latencies);
  const throughput = iterations / (latencies.reduce((a, b) => a + b, 0) / 1000);

  return {
    latency: stats,
    throughput: { mean: throughput },
    iterations,
  };
}

/**
 * Benchmark count operations
 * @param {number} quadCount - Number of quads in store
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkCount(quadCount, iterations = 1000) {
  const store = createStore();

  // Setup store
  for (let j = 0; j < quadCount; j++) {
    addQuad(store, {
      subject: namedNode(`http://example.org/s${j}`),
      predicate: namedNode(`http://example.org/p${j % 10}`),
      object: literal(`Object ${j}`),
    });
  }

  const latencies = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    const count = countQuads(store);
    const duration = performance.now() - start;
    latencies.push(duration);
  }

  const stats = analyzeVariance(latencies);
  const throughput = iterations / (latencies.reduce((a, b) => a + b, 0) / 1000);

  return {
    latency: stats,
    throughput: { mean: throughput },
    iterations,
  };
}

/**
 * Run all graph operation benchmarks
 * @returns {Promise<Object>} Benchmark results
 */
export async function runGraphOperationsBenchmarks() {
  console.log('\n▶ Running Graph Operations Benchmarks...');

  const results = {};

  // Add operations
  const addSmallResult = benchmarkAdd(100, 50);
  results['add-small-100'] = {
    ...addSmallResult,
    passed: addSmallResult.latency.p95 < 10,
    target: 'P95 < 10ms',
    unit: 'ops/s',
  };

  const addMediumResult = benchmarkAdd(1000, 30);
  results['add-medium-1k'] = {
    ...addMediumResult,
    passed: addMediumResult.latency.p95 < 50,
    target: 'P95 < 50ms',
    unit: 'ops/s',
  };

  const addLargeResult = benchmarkAdd(10000, 10);
  results['add-large-10k'] = {
    ...addLargeResult,
    passed: addLargeResult.latency.p95 < 500,
    target: 'P95 < 500ms',
    unit: 'ops/s',
  };

  // Remove operations
  const removeSmallResult = benchmarkRemove(1000, 100, 50);
  results['remove-small-100'] = {
    ...removeSmallResult,
    passed: removeSmallResult.latency.p95 < 10,
    target: 'P95 < 10ms',
    unit: 'ops/s',
  };

  const removeMediumResult = benchmarkRemove(10000, 1000, 20);
  results['remove-medium-1k'] = {
    ...removeMediumResult,
    passed: removeMediumResult.latency.p95 < 50,
    target: 'P95 < 50ms',
    unit: 'ops/s',
  };

  // Find operations
  const findAllResult = benchmarkFind(1000, 'all', 100);
  results['find-all-1k'] = {
    ...findAllResult,
    passed: findAllResult.latency.p95 < 5,
    target: 'P95 < 5ms',
    unit: 'queries/s',
  };

  const findSubjectResult = benchmarkFind(1000, 'subject', 100);
  results['find-subject-1k'] = {
    ...findSubjectResult,
    passed: findSubjectResult.latency.p95 < 2,
    target: 'P95 < 2ms',
    unit: 'queries/s',
  };

  const findPredicateResult = benchmarkFind(1000, 'predicate', 100);
  results['find-predicate-1k'] = {
    ...findPredicateResult,
    passed: findPredicateResult.latency.p95 < 3,
    target: 'P95 < 3ms',
    unit: 'queries/s',
  };

  const findPatternResult = benchmarkFind(1000, 'pattern', 100);
  results['find-pattern-1k'] = {
    ...findPatternResult,
    passed: findPatternResult.latency.p95 < 1,
    target: 'P95 < 1ms',
    unit: 'queries/s',
  };

  // Count operations
  const countSmallResult = benchmarkCount(100, 1000);
  results['count-small-100'] = {
    ...countSmallResult,
    passed: countSmallResult.latency.p95 < 0.5,
    target: 'P95 < 0.5ms',
    unit: 'counts/s',
  };

  const countMediumResult = benchmarkCount(1000, 1000);
  results['count-medium-1k'] = {
    ...countMediumResult,
    passed: countMediumResult.latency.p95 < 1,
    target: 'P95 < 1ms',
    unit: 'counts/s',
  };

  const summary = {
    total: Object.keys(results).length,
    passed: Object.values(results).filter(r => r.passed).length,
    failed: Object.values(results).filter(r => !r.passed).length,
  };

  console.log(`✓ Completed: ${summary.passed}/${summary.total} passed`);

  return { results, summary };
}
