/**
 * Quick Baseline Benchmark (30 seconds)
 * Streamlined version for rapid validation
 */

// Use direct path in monorepo
import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

// Quick configuration
const WARMUP_OPS = 100;
const BENCHMARK_OPS = 1000;
const DATA_SIZE = 10000; // Single test size

function generateQuad(id) {
  return quad(
    namedNode(`http://example.org/entity/${id}`),
    namedNode('http://schema.org/name'),
    literal(`Entity ${id}`)
  );
}

function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);

  return {
    count: values.length,
    mean: sum / values.length,
    p50: sorted[Math.floor(sorted.length * 0.5)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

async function benchmarkQueries(store) {
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
    latencies.push(performance.now() - start);
  }

  return calculateStats(latencies);
}

async function benchmarkThroughput(store) {
  const duration = 5000; // 5 seconds
  let operations = 0;
  const startTime = Date.now();

  while (Date.now() - startTime < duration) {
    const op = Math.random();
    if (op < 0.6) {
      for (const _ of store.match()) { break; }
    } else {
      store.add(generateQuad(operations + 100000));
    }
    operations++;
  }

  const actualDuration = Date.now() - startTime;
  return {
    operations,
    durationMs: actualDuration,
    opsPerSec: (operations / actualDuration) * 1000,
  };
}

async function run() {
  console.log('🎯 Quick Baseline Benchmark (30s)\n');

  const store = createStore();

  // Seed
  console.log(`Seeding ${DATA_SIZE} quads...`);
  for (let i = 0; i < DATA_SIZE; i++) {
    store.add(generateQuad(i));
  }

  // Query benchmark
  console.log('Benchmarking queries...');
  const queryStats = await benchmarkQueries(store);
  console.log(`  ✅ Query p50: ${queryStats.p50.toFixed(3)}ms, p99: ${queryStats.p99.toFixed(3)}ms`);

  // Throughput
  console.log('Benchmarking throughput...');
  const throughput = await benchmarkThroughput(store);
  console.log(`  ✅ ${throughput.opsPerSec.toFixed(0)} ops/sec`);

  // Save
  const report = {
    timestamp: new Date().toISOString(),
    dataSize: DATA_SIZE,
    queries: queryStats,
    throughput,
  };

  const fs = await import('fs');
  const reportPath = `tests/load/quick-baseline-${Date.now()}.json`;
  fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));

  console.log(`\n📄 Report: ${reportPath}`);
  console.log('✅ Quick baseline complete');
}

run().catch(console.error);
