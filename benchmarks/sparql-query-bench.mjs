/**
 * SPARQL Query Performance Benchmark
 * Measures query execution time across different query patterns
 */
import { performance } from 'perf_hooks';
import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

/**
 * Benchmark configuration
 */
const config = {
  datasetSizes: [100, 1000, 10000],
  queriesPerSize: 50,
  warmupQueries: 10,
};

/**
 * Generate test dataset
 */
function generateDataset(size) {
  const quads = [];
  for (let i = 0; i < size; i++) {
    quads.push(
      quad(
        namedNode(`http://example.org/person${i}`),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal(`Person ${i}`)
      ),
      quad(
        namedNode(`http://example.org/person${i}`),
        namedNode('http://xmlns.com/foaf/0.1/age'),
        literal(String(20 + (i % 60)))
      ),
      quad(
        namedNode(`http://example.org/person${i}`),
        namedNode('http://example.org/dept'),
        namedNode(`http://example.org/dept${i % 10}`)
      )
    );
  }
  return quads;
}

/**
 * SPARQL queries to benchmark
 */
const queries = {
  simple: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person foaf:name ?name .
    } LIMIT 10
  `,

  filtered: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?age WHERE {
      ?person foaf:age ?age .
      FILTER (?age > "30")
    } LIMIT 10
  `,

  join: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>
    SELECT ?person ?name ?dept WHERE {
      ?person foaf:name ?name .
      ?person ex:dept ?dept .
    } LIMIT 10
  `,

  aggregate: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>
    SELECT ?dept (COUNT(?person) as ?count) WHERE {
      ?person ex:dept ?dept .
    } GROUP BY ?dept
  `,
};

/**
 * Benchmark query performance
 */
async function benchmarkQuery(store, queryName, queryString, iterations) {
  const latencies = [];

  // Warmup
  for (let i = 0; i < config.warmupQueries; i++) {
    await store.query(queryString);
  }

  // Actual benchmark
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await store.query(queryString);
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Calculate statistics
 */
function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    median: sorted[Math.floor(sorted.length / 2)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

/**
 * Main benchmark runner
 */
async function main() {
  console.log('='.repeat(60));
  console.log('SPARQL Query Performance Benchmark');
  console.log('='.repeat(60));
  console.log('');

  for (const size of config.datasetSizes) {
    console.log(`\nDataset size: ${size} entities (${size * 3} triples)`);
    console.log('-'.repeat(60));

    // Create and populate store
    const store = createStore();
    const dataset = generateDataset(size);
    dataset.forEach(q => store.add(q));

    // Benchmark each query type
    for (const [queryName, queryString] of Object.entries(queries)) {
      const latencies = await benchmarkQuery(
        store,
        queryName,
        queryString,
        config.queriesPerSize
      );

      const stats = calculateStats(latencies);

      console.log(`\n${queryName.toUpperCase()} query:`);
      console.log(`  Mean:   ${stats.mean.toFixed(2)} ms`);
      console.log(`  Median: ${stats.median.toFixed(2)} ms`);
      console.log(`  P95:    ${stats.p95.toFixed(2)} ms`);
      console.log(`  P99:    ${stats.p99.toFixed(2)} ms`);
      console.log(`  Min/Max: ${stats.min.toFixed(2)} / ${stats.max.toFixed(2)} ms`);
    }

    console.log('');
  }

  console.log('='.repeat(60));
  console.log('Benchmark complete');
  console.log('='.repeat(60));
}

main().catch(console.error);
