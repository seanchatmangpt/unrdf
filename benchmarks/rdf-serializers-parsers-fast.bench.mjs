/**
 * @file RDF Serializers and Parsers Fast Benchmarks
 * @module benchmarks/rdf-serializers-parsers-fast
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  serializeNTriples,
  serializeTurtle,
} from '../packages/core/src/rdf/serializers.mjs';
import {
  parseRdf,
} from '../packages/core/src/rdf/parsers.mjs';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

/**
 * Create test dataset with N quads
 */
function createTestDataset(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push(
      quad(
        namedNode(`http://example.org/subject${i}`),
        namedNode(`http://example.org/predicate${i % 10}`),
        literal(`value${i}`),
        defaultGraph()
      )
    );
  }
  return quads;
}

/**
 * Benchmark a function
 */
async function benchmark(name, fn, iterations = 5) {
  const times = [];

  // Warmup
  await fn();

  // Actual benchmark
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    const end = performance.now();
    times.push(end - start);
  }

  times.sort((a, b) => a - b);

  const mean = times.reduce((a, b) => a + b, 0) / times.length;
  const p50 = times[Math.floor(times.length * 0.5)];
  const p95 = times[Math.floor(times.length * 0.95)];

  return {
    name,
    mean: mean.toFixed(3),
    p50: p50.toFixed(3),
    p95: p95.toFixed(3),
  };
}

/**
 * Run all benchmarks
 */
async function runBenchmarks() {
  console.log('RDF Serializers and Parsers Fast Benchmarks');
  console.log('='.repeat(80));

  const results = [];

  // Test datasets
  const smallDataset = createTestDataset(100);
  const mediumDataset = createTestDataset(1000);

  // Serialization
  results.push(
    await benchmark(
      'Serialize N-Triples (100 quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeNTriples(smallDataset)) {
          chunks.push(chunk);
        }
      },
      5
    )
  );

  results.push(
    await benchmark(
      'Serialize N-Triples (1K quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeNTriples(mediumDataset)) {
          chunks.push(chunk);
        }
      },
      5
    )
  );

  results.push(
    await benchmark(
      'Serialize Turtle (100 quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeTurtle(smallDataset)) {
          chunks.push(chunk);
        }
      },
      5
    )
  );

  // Parsing
  const smallNTriples = Array.from({ length: 100 }, (_, i) =>
    `<http://example.org/s${i}> <http://example.org/p> "value${i}" .`
  ).join('\n');

  const mediumNTriples = Array.from({ length: 1000 }, (_, i) =>
    `<http://example.org/s${i}> <http://example.org/p> "value${i}" .`
  ).join('\n');

  results.push(
    await benchmark(
      'Parse N-Triples (100 quads)',
      async () => {
        const quads = [];
        for await (const quad of parseRdf(smallNTriples, { format: 'ntriples' })) {
          quads.push(quad);
        }
      },
      5
    )
  );

  results.push(
    await benchmark(
      'Parse N-Triples (1K quads)',
      async () => {
        const quads = [];
        for await (const quad of parseRdf(mediumNTriples, { format: 'ntriples' })) {
          quads.push(quad);
        }
      },
      5
    )
  );

  // Print results
  console.log('');
  console.log('Results:');
  console.log('='.repeat(80));
  console.log(
    'Name'.padEnd(40) +
    'Mean'.padStart(12) +
    'P50'.padStart(12) +
    'P95'.padStart(12)
  );
  console.log('-'.repeat(80));

  for (const result of results) {
    console.log(
      result.name.padEnd(40) +
      (result.mean + ' ms').padStart(12) +
      (result.p50 + ' ms').padStart(12) +
      (result.p95 + ' ms').padStart(12)
    );
  }

  console.log('='.repeat(80));

  // Calculate throughput
  const serializeResult = results.find(r => r.name === 'Serialize N-Triples (1K quads)');
  const parseResult = results.find(r => r.name === 'Parse N-Triples (1K quads)');

  if (serializeResult) {
    const quadsPerSecond = (1000 / (parseFloat(serializeResult.mean) / 1000)).toFixed(0);
    console.log(`\nSerialization Throughput: ${quadsPerSecond} quads/second (1K dataset)`);
  }

  if (parseResult) {
    const quadsPerSecond = (1000 / (parseFloat(parseResult.mean) / 1000)).toFixed(0);
    console.log(`Parsing Throughput: ${quadsPerSecond} quads/second (1K dataset)`);
  }

  console.log('');
  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runBenchmarks()
    .then(() => {
      console.log('Benchmarks completed successfully!');
      process.exit(0);
    })
    .catch(error => {
      console.error('Benchmark failed:', error);
      process.exit(1);
    });
}

export { runBenchmarks };
