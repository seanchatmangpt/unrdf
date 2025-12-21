/**
 * Baseline Performance Benchmark for UNRDF
 *
 * Establishes baseline metrics for:
 * - Query latency (p50, p95, p99)
 * - Throughput (ops/sec)
 * - Memory per operation
 * - Scalability characteristics
 */

// Use direct path in monorepo
import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

// Configuration
const WARMUP_OPS = 1000;
const BENCHMARK_OPS = 10000;
const DATA_SIZES = [100, 1000, 10000, 50000];

/**
 * Generate test quad
 */
function generateQuad(id) {
  return quad(
    namedNode(`http://example.org/entity/${id}`),
    namedNode('http://schema.org/name'),
    literal(`Entity ${id}`)
  );
}

/**
 * Benchmark query performance
 */
async function benchmarkQueries(store, dataSize) {
  const latencies = [];

  // Warmup
  for (let i = 0; i < WARMUP_OPS; i++) {
    for (const _ of store.match()) { break; }
  }

  // Measure
  for (let i = 0; i < BENCHMARK_OPS; i++) {
    const start = performance.now();

    let count = 0;
    for (const _ of store.match()) {
      count++;
      if (count >= 100) break;
    }

    const latency = performance.now() - start;
    latencies.push(latency);
  }

  return calculateStats(latencies);
}

/**
 * Benchmark insert performance
 */
async function benchmarkInserts(store) {
  const latencies = [];

  // Warmup
  for (let i = 0; i < WARMUP_OPS; i++) {
    store.add(generateQuad(i + 1000000));
  }

  // Measure
  for (let i = 0; i < BENCHMARK_OPS; i++) {
    const start = performance.now();
    store.add(generateQuad(i + 2000000));
    const latency = performance.now() - start;
    latencies.push(latency);
  }

  return calculateStats(latencies);
}

/**
 * Benchmark delete performance
 */
async function benchmarkDeletes(store) {
  const latencies = [];

  // Prepare data
  const quadsToDelete = [];
  for (let i = 0; i < BENCHMARK_OPS; i++) {
    const q = generateQuad(i + 3000000);
    store.add(q);
    quadsToDelete.push(q);
  }

  // Warmup
  for (let i = 0; i < WARMUP_OPS; i++) {
    const q = generateQuad(i + 4000000);
    store.add(q);
    store.delete(q);
  }

  // Measure
  for (let i = 0; i < quadsToDelete.length; i++) {
    const start = performance.now();
    store.delete(quadsToDelete[i]);
    const latency = performance.now() - start;
    latencies.push(latency);
  }

  return calculateStats(latencies);
}

/**
 * Calculate statistics
 */
function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);

  return {
    count: values.length,
    mean: sum / values.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    p50: sorted[Math.floor(sorted.length * 0.5)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
    p999: sorted[Math.floor(sorted.length * 0.999)],
  };
}

/**
 * Measure memory per operation
 */
async function measureMemoryPerOp(store) {
  global.gc && global.gc();
  const startMem = process.memoryUsage().heapUsed;

  const numOps = 10000;
  for (let i = 0; i < numOps; i++) {
    store.add(generateQuad(i + 5000000));
  }

  global.gc && global.gc();
  const endMem = process.memoryUsage().heapUsed;

  const memoryPerOp = (endMem - startMem) / numOps;
  return memoryPerOp;
}

/**
 * Benchmark throughput
 */
async function benchmarkThroughput(store) {
  const duration = 10000; // 10 seconds
  let operations = 0;
  const startTime = Date.now();

  while (Date.now() - startTime < duration) {
    // Mixed workload
    const op = Math.random();
    if (op < 0.6) {
      // Query
      for (const _ of store.match()) { break; }
    } else if (op < 0.9) {
      // Insert
      store.add(generateQuad(operations + 6000000));
    } else {
      // Delete (skip if no data)
      const quads = [];
      for (const q of store.match()) {
        quads.push(q);
        break;
      }
      if (quads.length > 0) {
        store.delete(quads[0]);
      }
    }
    operations++;
  }

  const actualDuration = Date.now() - startTime;
  const opsPerSec = (operations / actualDuration) * 1000;

  return {
    operations,
    durationMs: actualDuration,
    opsPerSec,
  };
}

/**
 * Run complete benchmark suite
 */
async function runBenchmarks() {
  console.log('🎯 UNRDF Baseline Performance Benchmark\n');

  const results = {
    timestamp: new Date().toISOString(),
    node: process.version,
    platform: process.platform,
    arch: process.arch,
    benchmarks: {},
  };

  // Test across different data sizes
  for (const dataSize of DATA_SIZES) {
    console.log(`\n📊 Benchmarking with ${dataSize} quads...`);

    const store = createStore();

    // Seed data
    console.log(`  Seeding ${dataSize} quads...`);
    for (let i = 0; i < dataSize; i++) {
      store.add(generateQuad(i));
    }

    // Query benchmark
    console.log('  Benchmarking queries...');
    const queryStats = await benchmarkQueries(store, dataSize);

    // Insert benchmark
    console.log('  Benchmarking inserts...');
    const insertStats = await benchmarkInserts(store);

    // Delete benchmark
    console.log('  Benchmarking deletes...');
    const deleteStats = await benchmarkDeletes(store);

    results.benchmarks[`${dataSize}_quads`] = {
      dataSize,
      queries: queryStats,
      inserts: insertStats,
      deletes: deleteStats,
    };

    console.log(`  ✅ Query p50: ${queryStats.p50.toFixed(3)}ms, p99: ${queryStats.p99.toFixed(3)}ms`);
    console.log(`  ✅ Insert p50: ${insertStats.p50.toFixed(3)}ms, p99: ${insertStats.p99.toFixed(3)}ms`);
    console.log(`  ✅ Delete p50: ${deleteStats.p50.toFixed(3)}ms, p99: ${deleteStats.p99.toFixed(3)}ms`);
  }

  // Throughput benchmark
  console.log('\n📈 Benchmarking throughput (mixed workload)...');
  const store = createStore();
  for (let i = 0; i < 10000; i++) {
    store.add(generateQuad(i));
  }
  const throughput = await benchmarkThroughput(store);
  results.throughput = throughput;
  console.log(`  ✅ ${throughput.opsPerSec.toFixed(0)} ops/sec (${throughput.operations} ops in ${throughput.durationMs}ms)`);

  // Memory per operation (requires --expose-gc)
  if (global.gc) {
    console.log('\n💾 Measuring memory per operation...');
    const freshStore = createStore();
    const memPerOp = await measureMemoryPerOp(freshStore);
    results.memoryPerOp = memPerOp;
    console.log(`  ✅ ${(memPerOp / 1024).toFixed(2)} KB per operation`);
  } else {
    console.log('\n⚠️ Skipping memory measurement (run with --expose-gc)');
  }

  // Save results
  const reportPath = `tests/load/baseline-benchmark-${Date.now()}.json`;
  const fs = await import('fs');
  fs.writeFileSync(reportPath, JSON.stringify(results, null, 2));

  console.log(`\n📄 Baseline report saved: ${reportPath}`);
  console.log('\n✅ Baseline benchmark complete');
}

// Run benchmarks
runBenchmarks().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
