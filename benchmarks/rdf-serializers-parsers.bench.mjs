/**
 * @file RDF Serializers and Parsers Performance Benchmarks
 * @module benchmarks/rdf-serializers-parsers
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  serializeNTriples,
  serializeTurtle,
  serializeJsonLd,
} from '../packages/core/src/rdf/serializers.mjs';
import {
  parseRdf,
  parseToStore,
} from '../packages/core/src/rdf/parsers.mjs';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

/**
 * Create test dataset with N quads
 * @param {number} count - Number of quads to create
 * @returns {Array<Quad>} Test quads
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
 * @param {string} name - Benchmark name
 * @param {Function} fn - Function to benchmark
 * @param {number} iterations - Number of iterations
 * @returns {Promise<Object>} Benchmark results
 */
async function benchmark(name, fn, iterations = 10) {
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
  const p99 = times[Math.floor(times.length * 0.99)];
  const min = times[0];
  const max = times[times.length - 1];

  return {
    name,
    iterations,
    mean: mean.toFixed(3),
    p50: p50.toFixed(3),
    p95: p95.toFixed(3),
    p99: p99.toFixed(3),
    min: min.toFixed(3),
    max: max.toFixed(3),
  };
}

/**
 * Run all benchmarks
 */
async function runBenchmarks() {
  console.log('RDF Serializers and Parsers Performance Benchmarks');
  console.log('='.repeat(80));
  console.log('');

  const results = [];

  // Serialization Benchmarks
  console.log('Serialization Benchmarks:');
  console.log('-'.repeat(80));

  // N-Triples serialization - Small dataset (100 quads)
  const smallDataset = createTestDataset(100);
  results.push(
    await benchmark(
      'Serialize N-Triples (100 quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeNTriples(smallDataset)) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      10
    )
  );

  // N-Triples serialization - Medium dataset (10K quads)
  const mediumDataset = createTestDataset(10000);
  results.push(
    await benchmark(
      'Serialize N-Triples (10K quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeNTriples(mediumDataset)) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      5
    )
  );

  // N-Triples serialization - Large dataset (100K quads)
  const largeDataset = createTestDataset(100000);
  results.push(
    await benchmark(
      'Serialize N-Triples (100K quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeNTriples(largeDataset, { chunkSize: 5000 })) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      3
    )
  );

  // Turtle serialization - Small dataset
  results.push(
    await benchmark(
      'Serialize Turtle (100 quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeTurtle(smallDataset)) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      10
    )
  );

  // Turtle serialization - Medium dataset
  results.push(
    await benchmark(
      'Serialize Turtle (10K quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeTurtle(mediumDataset)) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      5
    )
  );

  // JSON-LD serialization - Small dataset
  results.push(
    await benchmark(
      'Serialize JSON-LD (100 quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeJsonLd(smallDataset)) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      10
    )
  );

  // JSON-LD serialization - Medium dataset
  results.push(
    await benchmark(
      'Serialize JSON-LD (10K quads)',
      async () => {
        const chunks = [];
        for await (const chunk of serializeJsonLd(mediumDataset)) {
          chunks.push(chunk);
        }
        return chunks.join('');
      },
      5
    )
  );

  // Parsing Benchmarks
  console.log('');
  console.log('Parsing Benchmarks:');
  console.log('-'.repeat(80));

  // Generate test N-Triples content
  const smallNTriples = Array.from({ length: 100 }, (_, i) =>
    `<http://example.org/s${i}> <http://example.org/p> "value${i}" .`
  ).join('\n');

  const mediumNTriples = Array.from({ length: 10000 }, (_, i) =>
    `<http://example.org/s${i}> <http://example.org/p> "value${i}" .`
  ).join('\n');

  const largeNTriples = Array.from({ length: 100000 }, (_, i) =>
    `<http://example.org/s${i}> <http://example.org/p> "value${i}" .`
  ).join('\n');

  // N-Triples parsing - Small dataset
  results.push(
    await benchmark(
      'Parse N-Triples (100 quads)',
      async () => {
        const quads = [];
        for await (const quad of parseRdf(smallNTriples, { format: 'ntriples' })) {
          quads.push(quad);
        }
        return quads;
      },
      10
    )
  );

  // N-Triples parsing - Medium dataset
  results.push(
    await benchmark(
      'Parse N-Triples (10K quads)',
      async () => {
        const quads = [];
        for await (const quad of parseRdf(mediumNTriples, { format: 'ntriples' })) {
          quads.push(quad);
        }
        return quads;
      },
      5
    )
  );

  // N-Triples parsing - Large dataset
  results.push(
    await benchmark(
      'Parse N-Triples (100K quads)',
      async () => {
        const quads = [];
        for await (const quad of parseRdf(largeNTriples, { format: 'ntriples' })) {
          quads.push(quad);
        }
        return quads;
      },
      3
    )
  );

  // Parse to Store - Medium dataset
  results.push(
    await benchmark(
      'Parse to Store (10K quads)',
      async () => {
        return await parseToStore(mediumNTriples, { format: 'ntriples' });
      },
      5
    )
  );

  // Parse to Store - Large dataset
  results.push(
    await benchmark(
      'Parse to Store (100K quads)',
      async () => {
        return await parseToStore(largeNTriples, { format: 'ntriples' });
      },
      3
    )
  );

  // Print results
  console.log('');
  console.log('Results:');
  console.log('='.repeat(80));
  console.log(
    'Name'.padEnd(40) +
    'Mean'.padStart(10) +
    'P50'.padStart(10) +
    'P95'.padStart(10) +
    'P99'.padStart(10)
  );
  console.log('-'.repeat(80));

  for (const result of results) {
    console.log(
      result.name.padEnd(40) +
      (result.mean + ' ms').padStart(10) +
      (result.p50 + ' ms').padStart(10) +
      (result.p95 + ' ms').padStart(10) +
      (result.p99 + ' ms').padStart(10)
    );
  }

  console.log('='.repeat(80));
  console.log('');

  // Calculate throughput for large dataset tests
  console.log('Throughput Analysis (100K quads):');
  console.log('-'.repeat(80));

  const serializeLargeResult = results.find(r => r.name === 'Serialize N-Triples (100K quads)');
  const parseLargeResult = results.find(r => r.name === 'Parse N-Triples (100K quads)');

  if (serializeLargeResult) {
    const quadsPerSecond = (100000 / (parseFloat(serializeLargeResult.mean) / 1000)).toFixed(0);
    console.log(`Serialization: ${quadsPerSecond} quads/second`);
  }

  if (parseLargeResult) {
    const quadsPerSecond = (100000 / (parseFloat(parseLargeResult.mean) / 1000)).toFixed(0);
    console.log(`Parsing: ${quadsPerSecond} quads/second`);
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

export { runBenchmarks, benchmark };
