/**
 * Pattern Match Benchmark
 *
 * Measures RDF triple pattern matching performance with actual numbers.
 *
 * Method:
 * - Creates 1000 triples in-memory
 * - Runs pattern matching 100 times
 * - Measures: ops/sec, P50 latency, P99 latency
 */

import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import { OxigraphBridge } from '../src/oxigraph-bridge.mjs';

const { namedNode, literal } = dataFactory;

/**
 * Calculate percentile from sorted array
 * @param {number[]} arr - Sorted array of numbers
 * @param {number} p - Percentile (0-100)
 * @returns {number} Percentile value
 */
function percentile(arr, p) {
  if (arr.length === 0) return 0;
  const sorted = [...arr].sort((a, b) => a - b);
  const index = Math.ceil((p / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

/**
 * Format number with thousands separator
 * @param {number} num - Number to format
 * @returns {string} Formatted number
 */
function formatNumber(num) {
  return num.toLocaleString('en-US', { maximumFractionDigits: 3 });
}

/**
 * Main benchmark function
 */
async function runBenchmark() {
  console.log('\n=== Pattern Match Benchmark ===\n');

  // Setup: Create store and bridge
  const store = createStore();
  const bridge = new OxigraphBridge(store);

  // Create 1000 test triples
  console.log('Setting up: Creating 1000 triples...');
  const setupStart = performance.now();

  const triples = [];
  for (let i = 0; i < 1000; i++) {
    const subject = namedNode(`http://example.org/subject${i % 100}`);
    const predicate = namedNode(`http://example.org/predicate${i % 10}`);
    const object = literal(`value_${i}`);
    triples.push({ subject, predicate, object });
  }

  await bridge.addTriples(triples);
  const setupTime = performance.now() - setupStart;
  console.log(`Setup completed in ${setupTime.toFixed(2)}ms\n`);

  // Warmup: 10 queries (not measured)
  console.log('Warming up: 10 queries...');
  for (let i = 0; i < 10; i++) {
    await bridge.queryPattern(null, null, null);
  }
  console.log('Warmup completed\n');

  // Benchmark 1: Full pattern match (wildcard query)
  console.log('Benchmark 1: Full pattern match (null, null, null)');
  const wildcardLatencies = [];
  const wildcardStart = performance.now();

  for (let i = 0; i < 100; i++) {
    const queryStart = performance.now();
    const results = await bridge.queryPattern(null, null, null);
    const queryEnd = performance.now();
    wildcardLatencies.push(queryEnd - queryStart);

    if (i === 0) {
      console.log(`  First query returned ${results.length} triples`);
    }
  }

  const wildcardEnd = performance.now();
  const wildcardTotal = wildcardEnd - wildcardStart;
  const wildcardOpsPerSec = (100 / wildcardTotal) * 1000;

  // Benchmark 2: Specific subject queries
  console.log('\nBenchmark 2: Specific subject queries');
  const specificLatencies = [];
  const specificStart = performance.now();

  for (let i = 0; i < 100; i++) {
    const subject = namedNode(`http://example.org/subject${i % 100}`);
    const queryStart = performance.now();
    const results = await bridge.queryPattern(subject, null, null);
    const queryEnd = performance.now();
    specificLatencies.push(queryEnd - queryStart);

    if (i === 0) {
      console.log(`  First query returned ${results.length} triples`);
    }
  }

  const specificEnd = performance.now();
  const specificTotal = specificEnd - specificStart;
  const specificOpsPerSec = (100 / specificTotal) * 1000;

  // Benchmark 3: Predicate-filtered queries
  console.log('\nBenchmark 3: Predicate-filtered queries');
  const predicateLatencies = [];
  const predicateStart = performance.now();

  for (let i = 0; i < 100; i++) {
    const predicate = namedNode(`http://example.org/predicate${i % 10}`);
    const queryStart = performance.now();
    const results = await bridge.queryPattern(null, predicate, null);
    const queryEnd = performance.now();
    predicateLatencies.push(queryEnd - queryStart);

    if (i === 0) {
      console.log(`  First query returned ${results.length} triples`);
    }
  }

  const predicateEnd = performance.now();
  const predicateTotal = predicateEnd - predicateStart;
  const predicateOpsPerSec = (100 / predicateTotal) * 1000;

  // Benchmark 4: Exact triple match
  console.log('\nBenchmark 4: Exact triple match');
  const exactLatencies = [];
  const exactStart = performance.now();

  for (let i = 0; i < 100; i++) {
    const subject = namedNode(`http://example.org/subject${i % 100}`);
    const predicate = namedNode(`http://example.org/predicate${i % 10}`);
    const object = literal(`value_${i}`);
    const queryStart = performance.now();
    const results = await bridge.queryPattern(subject, predicate, object);
    const queryEnd = performance.now();
    exactLatencies.push(queryEnd - queryStart);
  }

  const exactEnd = performance.now();
  const exactTotal = exactEnd - exactStart;
  const exactOpsPerSec = (100 / exactTotal) * 1000;

  // Calculate overall statistics
  const allLatencies = [
    ...wildcardLatencies,
    ...specificLatencies,
    ...predicateLatencies,
    ...exactLatencies
  ];

  const totalOps = 400;
  const totalTime = wildcardTotal + specificTotal + predicateTotal + exactTotal;
  const overallOpsPerSec = (totalOps / totalTime) * 1000;

  // Print results
  console.log('\n=== RESULTS ===\n');

  console.log('Overall Performance:');
  console.log(`- Total Operations: ${totalOps}`);
  console.log(`- Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`- Overall Throughput: ${formatNumber(overallOpsPerSec)} ops/sec`);
  console.log(`- Overall P50 Latency: ${percentile(allLatencies, 50).toFixed(3)}ms`);
  console.log(`- Overall P99 Latency: ${percentile(allLatencies, 99).toFixed(3)}ms`);
  console.log(`- Overall P99.9 Latency: ${percentile(allLatencies, 99.9).toFixed(3)}ms`);
  console.log(`- Mean Latency: ${(allLatencies.reduce((a, b) => a + b, 0) / allLatencies.length).toFixed(3)}ms`);

  console.log('\nWildcard Queries (null, null, null):');
  console.log(`- Operations: 100`);
  console.log(`- Time: ${wildcardTotal.toFixed(2)}ms`);
  console.log(`- Throughput: ${formatNumber(wildcardOpsPerSec)} ops/sec`);
  console.log(`- P50 Latency: ${percentile(wildcardLatencies, 50).toFixed(3)}ms`);
  console.log(`- P99 Latency: ${percentile(wildcardLatencies, 99).toFixed(3)}ms`);

  console.log('\nSpecific Subject Queries:');
  console.log(`- Operations: 100`);
  console.log(`- Time: ${specificTotal.toFixed(2)}ms`);
  console.log(`- Throughput: ${formatNumber(specificOpsPerSec)} ops/sec`);
  console.log(`- P50 Latency: ${percentile(specificLatencies, 50).toFixed(3)}ms`);
  console.log(`- P99 Latency: ${percentile(specificLatencies, 99).toFixed(3)}ms`);

  console.log('\nPredicate-Filtered Queries:');
  console.log(`- Operations: 100`);
  console.log(`- Time: ${predicateTotal.toFixed(2)}ms`);
  console.log(`- Throughput: ${formatNumber(predicateOpsPerSec)} ops/sec`);
  console.log(`- P50 Latency: ${percentile(predicateLatencies, 50).toFixed(3)}ms`);
  console.log(`- P99 Latency: ${percentile(predicateLatencies, 99).toFixed(3)}ms`);

  console.log('\nExact Triple Match:');
  console.log(`- Operations: 100`);
  console.log(`- Time: ${exactTotal.toFixed(2)}ms`);
  console.log(`- Throughput: ${formatNumber(exactOpsPerSec)} ops/sec`);
  console.log(`- P50 Latency: ${percentile(exactLatencies, 50).toFixed(3)}ms`);
  console.log(`- P99 Latency: ${percentile(exactLatencies, 99).toFixed(3)}ms`);

  console.log('\n=== Benchmark Statistics ===');
  console.log(`- Store size: ${triples.length} triples`);
  console.log(`- Bridge stats:`, bridge.stats);

  console.log('\n=== Performance Target Validation ===');
  const targetOps = 10000;
  const meetsTarget = overallOpsPerSec >= targetOps;
  console.log(`- Target: >= ${formatNumber(targetOps)} ops/sec`);
  console.log(`- Actual: ${formatNumber(overallOpsPerSec)} ops/sec`);
  console.log(`- Status: ${meetsTarget ? '✅ PASS' : '❌ FAIL'}`);

  console.log('\n===============================\n');

  // Cleanup
  bridge.destroy();
}

// Run benchmark
runBenchmark().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
