/**
 * @file RDF Validation Performance Benchmarks
 * @module benchmarks/rdf-core/05-validation
 * @description Benchmarks for SHACL validation and quad validation
 */

import { performance } from 'perf_hooks';
import { createStore, addQuad, namedNode, literal } from '@unrdf/core';
import { validateQuad } from '@unrdf/core';
import { analyzeVariance } from './suite.mjs';

/**
 * Benchmark quad validation
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkQuadValidation(iterations = 1000) {
  const latencies = [];

  const validQuad = {
    subject: namedNode('http://example.org/subject'),
    predicate: namedNode('http://example.org/predicate'),
    object: literal('Object value'),
  };

  // Warmup
  for (let i = 0; i < 100; i++) {
    validateQuad(validQuad);
  }

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    validateQuad(validQuad);
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
 * Benchmark store validation
 * @param {number} quadCount - Number of quads in store
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkStoreValidation(quadCount, iterations = 50) {
  const latencies = [];

  for (let i = 0; i < iterations; i++) {
    // Create store with quads
    const store = createStore();
    for (let j = 0; j < quadCount; j++) {
      addQuad(store, {
        subject: namedNode(`http://example.org/s${j}`),
        predicate: namedNode(`http://example.org/p${j % 10}`),
        object: literal(`Object ${j}`),
      });
    }

    // Measure validation (iterate and count)
    const start = performance.now();
    let count = 0;
    for (const quad of store.match()) {
      validateQuad({
        subject: quad.subject,
        predicate: quad.predicate,
        object: quad.object,
      });
      count++;
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
 * Benchmark term type validation
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
function benchmarkTermValidation(iterations = 1000) {
  const latencies = [];

  const terms = [
    namedNode('http://example.org/term'),
    literal('String value'),
    literal('42', namedNode('http://www.w3.org/2001/XMLSchema#integer')),
  ];

  // Warmup
  for (let i = 0; i < 100; i++) {
    terms.forEach(term => {
      const isNamed = term.termType === 'NamedNode';
      const isLiteral = term.termType === 'Literal';
    });
  }

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    terms.forEach(term => {
      const isNamed = term.termType === 'NamedNode';
      const isLiteral = term.termType === 'Literal';
      const value = term.value;
    });
    const duration = performance.now() - start;
    latencies.push(duration);
  }

  const stats = analyzeVariance(latencies);
  const throughput = (iterations * terms.length) / (latencies.reduce((a, b) => a + b, 0) / 1000);

  return {
    latency: stats,
    throughput: { mean: throughput },
    iterations,
  };
}

/**
 * Run all validation benchmarks
 * @returns {Promise<Object>} Benchmark results
 */
export async function runValidationBenchmarks() {
  console.log('\n▶ Running Validation Benchmarks...');

  const results = {};

  // Quad validation
  const quadValidationResult = benchmarkQuadValidation(1000);
  results['quad-validation'] = {
    ...quadValidationResult,
    passed: quadValidationResult.latency.p95 < 0.1,
    target: 'P95 < 0.1ms',
    unit: 'validations/s',
  };

  // Store validation - Small
  const storeSmallResult = benchmarkStoreValidation(100, 50);
  results['store-validation-small-100'] = {
    ...storeSmallResult,
    passed: storeSmallResult.latency.p95 < 5,
    target: 'P95 < 5ms',
    unit: 'validations/s',
  };

  // Store validation - Medium
  const storeMediumResult = benchmarkStoreValidation(1000, 30);
  results['store-validation-medium-1k'] = {
    ...storeMediumResult,
    passed: storeMediumResult.latency.p95 < 30,
    target: 'P95 < 30ms',
    unit: 'validations/s',
  };

  // Store validation - Large
  const storeLargeResult = benchmarkStoreValidation(10000, 10);
  results['store-validation-large-10k'] = {
    ...storeLargeResult,
    passed: storeLargeResult.latency.p95 < 300,
    target: 'P95 < 300ms',
    unit: 'validations/s',
  };

  // Term validation
  const termValidationResult = benchmarkTermValidation(1000);
  results['term-validation'] = {
    ...termValidationResult,
    passed: termValidationResult.latency.p95 < 0.05,
    target: 'P95 < 0.05ms',
    unit: 'validations/s',
  };

  const summary = {
    total: Object.keys(results).length,
    passed: Object.values(results).filter(r => r.passed).length,
    failed: Object.values(results).filter(r => !r.passed).length,
  };

  console.log(`✓ Completed: ${summary.passed}/${summary.total} passed`);

  return { results, summary };
}
