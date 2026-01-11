/**
 * @file RDF Parsing Performance Benchmarks
 * @module benchmarks/rdf-core/01-parsing
 * @description Benchmarks for Turtle, N-Triples, and JSON-LD parsing
 */

import { performance } from 'perf_hooks';
import { Parser } from '@unrdf/core/rdf/n3-justified-only';
import { analyzeVariance } from './suite.mjs';

/**
 * Generate Turtle test data
 * @param {number} tripleCount - Number of triples to generate
 * @returns {string} Turtle document
 */
function generateTurtleData(tripleCount) {
  const prefixes = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

`;

  const triples = [];
  for (let i = 0; i < tripleCount; i++) {
    triples.push(`ex:person${i} a foaf:Person ;`);
    triples.push(`  foaf:name "Person ${i}" ;`);
    triples.push(`  foaf:age ${20 + (i % 50)} ;`);
    triples.push(`  ex:created "${new Date().toISOString()}"^^xsd:dateTime .`);
  }

  return prefixes + triples.join('\n');
}

/**
 * Generate N-Triples test data
 * @param {number} tripleCount - Number of triples to generate
 * @returns {string} N-Triples document
 */
function generateNTriplesData(tripleCount) {
  const triples = [];
  for (let i = 0; i < tripleCount; i++) {
    triples.push(`<http://example.org/person${i}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .`);
    triples.push(`<http://example.org/person${i}> <http://xmlns.com/foaf/0.1/name> "Person ${i}" .`);
    triples.push(`<http://example.org/person${i}> <http://xmlns.com/foaf/0.1/age> "${20 + (i % 50)}"^^<http://www.w3.org/2001/XMLSchema#integer> .`);
  }
  return triples.join('\n');
}

/**
 * Benchmark parsing performance
 * @param {string} data - RDF data to parse
 * @param {string} format - RDF format (turtle, n-triples, etc.)
 * @param {number} iterations - Number of iterations
 * @returns {Object} Benchmark results
 */
async function benchmarkParsing(data, format, iterations = 100) {
  const latencies = [];

  // Warmup
  for (let i = 0; i < 10; i++) {
    await parseData(data, format);
  }

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await parseData(data, format);
    const duration = performance.now() - start;
    latencies.push(duration);
  }

  const stats = analyzeVariance(latencies);
  const throughput = (iterations / (latencies.reduce((a, b) => a + b, 0) / 1000));

  return {
    latency: stats,
    throughput: { mean: throughput },
    iterations,
  };
}

/**
 * Parse RDF data
 * @param {string} data - RDF data
 * @param {string} format - RDF format
 * @returns {Promise<Array>} Parsed quads
 */
async function parseData(data, format) {
  return new Promise((resolve, reject) => {
    const parser = new Parser({ format });
    const quads = [];

    parser.parse(data, (error, quad) => {
      if (error) {
        reject(error);
      } else if (quad) {
        quads.push(quad);
      } else {
        resolve(quads);
      }
    });
  });
}

/**
 * Run all parsing benchmarks
 * @returns {Promise<Object>} Benchmark results
 */
export async function runParsingBenchmarks() {
  console.log('\n▶ Running Parsing Benchmarks...');

  const results = {};

  // Turtle - Small (100 triples)
  const turtleSmall = generateTurtleData(25); // 25 * 4 = 100 triples
  const turtleSmallResult = await benchmarkParsing(turtleSmall, 'text/turtle', 100);
  results['turtle-small-100'] = {
    ...turtleSmallResult,
    passed: turtleSmallResult.latency.p95 < 50,
    target: 'P95 < 50ms',
    unit: 'parses/s',
  };

  // Turtle - Medium (1000 triples)
  const turtleMedium = generateTurtleData(250); // 250 * 4 = 1000 triples
  const turtleMediumResult = await benchmarkParsing(turtleMedium, 'text/turtle', 50);
  results['turtle-medium-1k'] = {
    ...turtleMediumResult,
    passed: turtleMediumResult.latency.p95 < 200,
    target: 'P95 < 200ms',
    unit: 'parses/s',
  };

  // Turtle - Large (10K triples)
  const turtleLarge = generateTurtleData(2500); // 2500 * 4 = 10000 triples
  const turtleLargeResult = await benchmarkParsing(turtleLarge, 'text/turtle', 20);
  results['turtle-large-10k'] = {
    ...turtleLargeResult,
    passed: turtleLargeResult.latency.p95 < 1000,
    target: 'P95 < 1000ms',
    unit: 'parses/s',
  };

  // N-Triples - Small (100 triples)
  const ntriplesSmall = generateNTriplesData(100);
  const ntriplesSmallResult = await benchmarkParsing(ntriplesSmall, 'application/n-triples', 100);
  results['ntriples-small-100'] = {
    ...ntriplesSmallResult,
    passed: ntriplesSmallResult.latency.p95 < 30,
    target: 'P95 < 30ms',
    unit: 'parses/s',
  };

  // N-Triples - Medium (1000 triples)
  const ntriplesMedium = generateNTriplesData(1000);
  const ntriplesMediumResult = await benchmarkParsing(ntriplesMedium, 'application/n-triples', 50);
  results['ntriples-medium-1k'] = {
    ...ntriplesMediumResult,
    passed: ntriplesMediumResult.latency.p95 < 100,
    target: 'P95 < 100ms',
    unit: 'parses/s',
  };

  // N-Triples - Large (10K triples)
  const ntriplesLarge = generateNTriplesData(10000);
  const ntriplesLargeResult = await benchmarkParsing(ntriplesLarge, 'application/n-triples', 20);
  results['ntriples-large-10k'] = {
    ...ntriplesLargeResult,
    passed: ntriplesLargeResult.latency.p95 < 500,
    target: 'P95 < 500ms',
    unit: 'parses/s',
  };

  const summary = {
    total: Object.keys(results).length,
    passed: Object.values(results).filter(r => r.passed).length,
    failed: Object.values(results).filter(r => !r.passed).length,
  };

  console.log(`✓ Completed: ${summary.passed}/${summary.total} passed`);

  return { results, summary };
}
