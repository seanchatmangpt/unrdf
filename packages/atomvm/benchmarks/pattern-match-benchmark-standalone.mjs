/**
 * Pattern Match Benchmark (Standalone Version)
 *
 * This is a standalone demo showing benchmark methodology.
 * For full benchmarks with actual Oxigraph integration, run:
 *   cd /home/user/unrdf && pnpm install
 *   node packages/atomvm/benchmarks/pattern-match-benchmark.mjs
 *
 * This demo simulates pattern matching to show measurement approach.
 */

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
 * Simulated in-memory triple store
 */
class MockTripleStore {
  constructor() {
    this.triples = [];
  }

  addTriples(triples) {
    this.triples.push(...triples);
  }

  queryPattern(s, p, o) {
    // Simulate pattern matching with realistic timing
    const matchStart = performance.now();

    const results = this.triples.filter(triple => {
      const matchS = s === null || triple.subject === s;
      const matchP = p === null || triple.predicate === p;
      const matchO = o === null || triple.object === o;
      return matchS && matchP && matchO;
    });

    // Add small delay to simulate database query overhead (0.01-0.1ms)
    const delay = 0.01 + Math.random() * 0.09;
    const elapsed = performance.now() - matchStart;
    if (elapsed < delay) {
      // Busy wait to simulate realistic query time
      const target = matchStart + delay;
      while (performance.now() < target) {
        // Spin
      }
    }

    return results;
  }
}

/**
 * Main benchmark function
 */
async function runBenchmark() {
  console.log('\n=== Pattern Match Benchmark (Standalone Demo) ===\n');
  console.log('NOTE: This is a simplified demo. For full benchmarks, install dependencies:\n');
  console.log('  cd /home/user/unrdf && pnpm install');
  console.log('  node packages/atomvm/benchmarks/pattern-match-benchmark.mjs\n');

  // Setup: Create store and add 1000 triples
  console.log('Setting up: Creating 1000 triples...');
  const setupStart = performance.now();

  const store = new MockTripleStore();
  const triples = [];

  for (let i = 0; i < 1000; i++) {
    const subject = `http://example.org/subject${i % 100}`;
    const predicate = `http://example.org/predicate${i % 10}`;
    const object = `value_${i}`;
    triples.push({ subject, predicate, object });
  }

  store.addTriples(triples);
  const setupTime = performance.now() - setupStart;
  console.log(`Setup completed in ${setupTime.toFixed(2)}ms\n`);

  // Warmup: 10 queries (not measured)
  console.log('Warming up: 10 queries...');
  for (let i = 0; i < 10; i++) {
    store.queryPattern(null, null, null);
  }
  console.log('Warmup completed\n');

  // Benchmark 1: Full pattern match (wildcard query)
  console.log('Benchmark 1: Full pattern match (null, null, null)');
  const wildcardLatencies = [];
  const wildcardStart = performance.now();

  for (let i = 0; i < 100; i++) {
    const queryStart = performance.now();
    const results = store.queryPattern(null, null, null);
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
    const subject = `http://example.org/subject${i % 100}`;
    const queryStart = performance.now();
    const results = store.queryPattern(subject, null, null);
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
    const predicate = `http://example.org/predicate${i % 10}`;
    const queryStart = performance.now();
    const results = store.queryPattern(null, predicate, null);
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
    const subject = `http://example.org/subject${i % 100}`;
    const predicate = `http://example.org/predicate${i % 10}`;
    const object = `value_${i}`;
    const queryStart = performance.now();
    const results = store.queryPattern(subject, predicate, object);
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

  console.log('\n=== Benchmark Methodology Demonstrated ===');
  console.log('✅ Using performance.now() for high-precision timing');
  console.log('✅ Multiple iterations (100 per benchmark)');
  console.log('✅ Warmup phase before measurement');
  console.log('✅ Percentile calculations (P50, P99, P99.9)');
  console.log('✅ Throughput in ops/sec');
  console.log('✅ Actual measured numbers (not estimates)');

  console.log('\n===============================\n');
}

// Run benchmark
runBenchmark().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
