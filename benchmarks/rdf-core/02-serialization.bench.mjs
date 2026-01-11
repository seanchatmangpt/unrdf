/**
 * @file RDF Serialization Performance Benchmarks
 * @module benchmarks/rdf-core/02-serialization
 * @description Benchmarks for Turtle and N-Triples serialization
 */

import { performance } from 'perf_hooks';
import { createStore, addQuad, namedNode, literal } from '@unrdf/core';
import { Writer } from '@unrdf/core/rdf/n3-justified-only';
import { analyzeVariance } from './suite.mjs';

/**
 * Generate test quads in store
 * @param {number} quadCount - Number of quads to generate
 * @returns {Object} RDF store
 */
function generateTestStore(quadCount) {
  const store = createStore();

  for (let i = 0; i < quadCount; i++) {
    addQuad(store, {
      subject: namedNode(`http://example.org/person${i}`),
      predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      object: literal(`Person ${i}`),
    });

    addQuad(store, {
      subject: namedNode(`http://example.org/person${i}`),
      predicate: namedNode('http://xmlns.com/foaf/0.1/age'),
      object: literal(String(20 + (i % 50))),
    });
  }

  return store;
}

/**
 * Benchmark serialization performance
 * @param {Object} store - RDF store
 * @param {string} format - Serialization format
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
async function benchmarkSerialization(store, format, iterations = 50) {
  const latencies = [];
  const sizes = [];

  // Warmup
  for (let i = 0; i < 5; i++) {
    await serializeStore(store, format);
  }

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    const output = await serializeStore(store, format);
    const duration = performance.now() - start;
    latencies.push(duration);
    sizes.push(output.length);
  }

  const stats = analyzeVariance(latencies);
  const throughput = (iterations / (latencies.reduce((a, b) => a + b, 0) / 1000));

  return {
    latency: stats,
    throughput: { mean: throughput },
    outputSize: { mean: sizes[0] },
    iterations,
  };
}

/**
 * Serialize store to string
 * @param {Object} store - RDF store
 * @param {string} format - Serialization format
 * @returns {Promise<string>} Serialized output
 */
async function serializeStore(store, format) {
  return new Promise((resolve, reject) => {
    const writer = new Writer({ format });
    let output = '';

    // Get all quads from store
    const quads = store.match();

    writer.addQuads([...quads]);
    writer.end((error, result) => {
      if (error) {
        reject(error);
      } else {
        output = result;
        resolve(output);
      }
    });
  });
}

/**
 * Run all serialization benchmarks
 * @returns {Promise<Object>} Benchmark results
 */
export async function runSerializationBenchmarks() {
  console.log('\n▶ Running Serialization Benchmarks...');

  const results = {};

  // Turtle - Small (100 quads)
  const storeSmall = generateTestStore(50); // 50 * 2 = 100 quads
  const turtleSmallResult = await benchmarkSerialization(storeSmall, 'text/turtle', 50);
  results['turtle-small-100'] = {
    ...turtleSmallResult,
    passed: turtleSmallResult.latency.p95 < 50,
    target: 'P95 < 50ms',
    unit: 'serializations/s',
  };

  // Turtle - Medium (1000 quads)
  const storeMedium = generateTestStore(500); // 500 * 2 = 1000 quads
  const turtleMediumResult = await benchmarkSerialization(storeMedium, 'text/turtle', 30);
  results['turtle-medium-1k'] = {
    ...turtleMediumResult,
    passed: turtleMediumResult.latency.p95 < 200,
    target: 'P95 < 200ms',
    unit: 'serializations/s',
  };

  // Turtle - Large (10K quads)
  const storeLarge = generateTestStore(5000); // 5000 * 2 = 10000 quads
  const turtleLargeResult = await benchmarkSerialization(storeLarge, 'text/turtle', 10);
  results['turtle-large-10k'] = {
    ...turtleLargeResult,
    passed: turtleLargeResult.latency.p95 < 1000,
    target: 'P95 < 1000ms',
    unit: 'serializations/s',
  };

  // N-Triples - Small (100 quads)
  const ntriplesSmallResult = await benchmarkSerialization(storeSmall, 'application/n-triples', 50);
  results['ntriples-small-100'] = {
    ...ntriplesSmallResult,
    passed: ntriplesSmallResult.latency.p95 < 30,
    target: 'P95 < 30ms',
    unit: 'serializations/s',
  };

  // N-Triples - Medium (1000 quads)
  const ntriplesMediumResult = await benchmarkSerialization(storeMedium, 'application/n-triples', 30);
  results['ntriples-medium-1k'] = {
    ...ntriplesMediumResult,
    passed: ntriplesMediumResult.latency.p95 < 100,
    target: 'P95 < 100ms',
    unit: 'serializations/s',
  };

  // N-Triples - Large (10K quads)
  const ntriplesLargeResult = await benchmarkSerialization(storeLarge, 'application/n-triples', 10);
  results['ntriples-large-10k'] = {
    ...ntriplesLargeResult,
    passed: ntriplesLargeResult.latency.p95 < 500,
    target: 'P95 < 500ms',
    unit: 'serializations/s',
  };

  const summary = {
    total: Object.keys(results).length,
    passed: Object.values(results).filter(r => r.passed).length,
    failed: Object.values(results).filter(r => !r.passed).length,
  };

  console.log(`✓ Completed: ${summary.passed}/${summary.total} passed`);

  return { results, summary };
}
