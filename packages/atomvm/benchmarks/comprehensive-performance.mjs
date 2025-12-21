#!/usr/bin/env node
/**
 * @fileoverview Comprehensive Performance Analysis for UNRDF Packages
 *
 * Fully compatible with OxigraphStore and actual package APIs
 *
 * @version 1.0.0
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';

const { namedNode, literal, quad } = dataFactory;

// ============================================================================
// Performance Measurement Utilities
// ============================================================================

function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: usage.rss,
    heapTotal: usage.heapTotal,
    heapUsed: usage.heapUsed,
    external: usage.external,
  };
}

function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${(bytes / k ** i).toFixed(2)} ${sizes[i]}`;
}

function calculatePercentile(values, percentile) {
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil((percentile / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

async function measurePerformance(fn, iterations = 1000) {
  const memoryBefore = getMemoryUsage();
  const times = [];

  // Warmup
  for (let i = 0; i < 10; i++) {
    await fn();
  }

  if (global.gc) global.gc();

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    times.push(performance.now() - start);
  }

  const memoryAfter = getMemoryUsage();
  const totalTime = times.reduce((sum, t) => sum + t, 0);

  return {
    iterations,
    totalTime: totalTime.toFixed(2),
    avgTime: (totalTime / iterations).toFixed(4),
    minTime: Math.min(...times).toFixed(4),
    maxTime: Math.max(...times).toFixed(4),
    p50: calculatePercentile(times, 50).toFixed(4),
    p95: calculatePercentile(times, 95).toFixed(4),
    p99: calculatePercentile(times, 99).toFixed(4),
    throughput: (iterations / (totalTime / 1000)).toFixed(2),
    memoryGrowth: formatBytes(memoryAfter.heapUsed - memoryBefore.heapUsed),
    memoryGrowthBytes: memoryAfter.heapUsed - memoryBefore.heapUsed,
  };
}

async function measureConcurrency(fn, concurrency) {
  const start = performance.now();
  await Promise.all(Array.from({ length: concurrency }, () => fn()));
  const totalTime = performance.now() - start;

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    avgTimePerOp: (totalTime / concurrency).toFixed(4),
    throughput: (concurrency / (totalTime / 1000)).toFixed(2),
  };
}

// ============================================================================
// Test Data Generators
// ============================================================================

function createTestStore(size = 1000) {
  const store = createStore();
  for (let i = 0; i < size; i++) {
    store.add(
      quad(
        namedNode(`http://example.org/subject${i}`),
        namedNode(`http://example.org/predicate`),
        literal(`Object ${i}`)
      )
    );
  }
  return store;
}

// ============================================================================
// Store Operations Benchmarks
// ============================================================================

async function benchmarkStoreOperations() {
  console.log('\n=== STORE OPERATIONS PERFORMANCE ===\n');

  const results = {};

  // Add operation
  console.log('Benchmarking add...');
  let counter = 0;
  results.add = await measurePerformance(() => {
    const store = createStore();
    store.add(
      quad(
        namedNode(`http://example.org/subject${counter++}`),
        namedNode('http://example.org/predicate'),
        literal('test')
      )
    );
  }, 1000);

  // Match operation
  console.log('Benchmarking match...');
  const testStore = createTestStore(1000);
  results.match_specific = await measurePerformance(() => {
    testStore.match(
      namedNode('http://example.org/subject0'),
      namedNode('http://example.org/predicate'),
      null,
      null
    );
  }, 1000);

  results.match_all = await measurePerformance(() => {
    testStore.match(null, null, null, null);
  }, 100);

  // Has operation
  console.log('Benchmarking has...');
  const testQuad = quad(
    namedNode('http://example.org/subject0'),
    namedNode('http://example.org/predicate'),
    literal('Object 0')
  );
  results.has = await measurePerformance(() => {
    testStore.has(testQuad);
  }, 1000);

  // Delete operation
  console.log('Benchmarking delete...');
  results.delete = await measurePerformance(() => {
    const store = createTestStore(10);
    const quads = store.match(null, null, null, null);
    if (quads.length > 0) {
      store.delete(quads[0]);
    }
  }, 1000);

  // Size operation
  console.log('Benchmarking size...');
  results.size = await measurePerformance(() => {
    const _size = testStore.size;
  }, 1000);

  // Concurrent operations
  console.log('Benchmarking concurrent match...');
  results.match_concurrent_10 = await measureConcurrency(
    () => testStore.match(namedNode('http://example.org/subject0'), null, null, null),
    10
  );
  results.match_concurrent_100 = await measureConcurrency(
    () => testStore.match(namedNode('http://example.org/subject0'), null, null, null),
    100
  );
  results.match_concurrent_1000 = await measureConcurrency(
    () => testStore.match(namedNode('http://example.org/subject0'), null, null, null),
    1000
  );

  return results;
}

// ============================================================================
// Scalability Benchmarks
// ============================================================================

async function benchmarkScalability() {
  console.log('\n=== SCALABILITY ANALYSIS ===\n');

  const results = {};
  const sizes = [100, 500, 1000, 5000];

  console.log('Benchmarking store creation at different sizes...');
  results.creation = {};
  for (const size of sizes) {
    console.log(`  Testing size ${size}...`);
    // Reduce iterations for larger sizes
    const iterations = size <= 1000 ? 10 : 2;
    const metric = await measurePerformance(() => {
      createTestStore(size);
    }, iterations);
    results.creation[`size_${size}`] = metric;
  }

  console.log('Benchmarking match performance at different sizes...');
  results.match = {};
  for (const size of sizes) {
    console.log(`  Testing size ${size}...`);
    const store = createTestStore(size);
    const iterations = size <= 1000 ? 10 : 2;
    const metric = await measurePerformance(() => {
      store.match(null, null, null, null);
    }, iterations);
    results.match[`size_${size}`] = metric;
  }

  console.log('Benchmarking size calculation at different sizes...');
  results.size = {};
  for (const size of sizes) {
    console.log(`  Testing size ${size}...`);
    const store = createTestStore(size);
    const iterations = size <= 1000 ? 100 : 10;
    const metric = await measurePerformance(() => {
      const _s = store.size;
    }, iterations);
    results.size[`size_${size}`] = metric;
  }

  return results;
}

// ============================================================================
// Resource Cleanup Tests
// ============================================================================

async function testResourceCleanup() {
  console.log('\n=== RESOURCE CLEANUP TEST ===\n');

  const initialMemory = getMemoryUsage();
  console.log('Creating and destroying 100 stores with 1000 quads each...');

  for (let i = 0; i < 100; i++) {
    const store = createTestStore(1000);
    void store; // Intentionally unused
  }

  if (global.gc) {
    global.gc();
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  const finalMemory = getMemoryUsage();
  const memoryLeak = finalMemory.heapUsed - initialMemory.heapUsed;

  return {
    initialMemory: formatBytes(initialMemory.heapUsed),
    finalMemory: formatBytes(finalMemory.heapUsed),
    memoryDelta: formatBytes(memoryLeak),
    memoryLeakBytes: memoryLeak,
    leakDetected: memoryLeak > 10 * 1024 * 1024,
  };
}

// ============================================================================
// Bottleneck Analysis
// ============================================================================

async function analyzeBottlenecks() {
  console.log('\n=== BOTTLENECK ANALYSIS ===\n');

  const bottlenecks = [];

  // Test 1: Large store iteration
  console.log('Testing large store match operation...');
  const largeStore = createTestStore(5000);
  const matchStart = performance.now();
  const quads = largeStore.match(null, null, null, null);
  const matchDuration = performance.now() - matchStart;

  if (matchDuration > 100) {
    bottlenecks.push({
      operation: 'match(null, null, null, null) on 5k store',
      duration: matchDuration.toFixed(2),
      quadCount: quads.length,
      severity: 'HIGH',
      recommendation: 'Use specific predicates to reduce result set size',
    });
  }

  // Test 2: Size calculation performance
  console.log('Testing size calculation...');
  const sizeStart = performance.now();
  const size = largeStore.size;
  const sizeDuration = performance.now() - sizeStart;

  if (sizeDuration > 50) {
    bottlenecks.push({
      operation: 'size getter on 5k store',
      duration: sizeDuration.toFixed(2),
      size: size,
      severity: 'MEDIUM',
      recommendation: 'Size calculation requires match(). Cache if used frequently.',
    });
  }

  // Test 3: Sequential vs parallel inserts
  console.log('Testing sequential inserts...');
  const seqStore = createStore();
  const seqStart = performance.now();
  for (let i = 0; i < 1000; i++) {
    seqStore.add(
      quad(namedNode(`http://example.org/s${i}`), namedNode('http://p'), literal('o'))
    );
  }
  const seqDuration = performance.now() - seqStart;

  if (seqDuration > 100) {
    bottlenecks.push({
      operation: '1000 sequential inserts',
      duration: seqDuration.toFixed(2),
      severity: 'LOW',
      recommendation: 'Sequential inserts are inherently linear. Consider batch loading.',
    });
  }

  // Test 4: Memory allocation during creation
  console.log('Testing memory allocation...');
  const memBefore = getMemoryUsage();
  const allocStore = createTestStore(5000);
  const memAfter = getMemoryUsage();
  const memoryUsed = memAfter.heapUsed - memBefore.heapUsed;

  if (memoryUsed > 50 * 1024 * 1024) {
    bottlenecks.push({
      operation: 'Store creation (5k quads)',
      memoryUsed: formatBytes(memoryUsed),
      severity: 'MEDIUM',
      recommendation: 'Each quad allocates memory. Use dump/load for large datasets.',
    });
  }

  return { bottlenecks, count: bottlenecks.length };
}

// ============================================================================
// I/O and Blocking Benchmarks
// ============================================================================

async function benchmarkIO() {
  console.log('\n=== I/O OPERATIONS PERFORMANCE ===\n');

  const results = {};
  const testStore = createTestStore(1000);

  // Test dump operation (serialization)
  console.log('Benchmarking dump (N-Triples)...');
  try {
    results.dump_ntriples = await measurePerformance(() => {
      testStore.dump({ format: 'application/n-triples' });
    }, 10);
  } catch (error) {
    console.log(`  Dump benchmark skipped: ${error.message}`);
    results.dump_ntriples = { error: error.message };
  }

  // Test load operation (deserialization)
  console.log('Benchmarking load (N-Triples)...');
  try {
    const data = `<http://s> <http://p> <http://o> .`;
    results.load_ntriples = await measurePerformance(() => {
      const store = createStore();
      store.load(data, { format: 'application/n-triples' });
    }, 10);
  } catch (error) {
    console.log(`  Load benchmark skipped: ${error.message}`);
    results.load_ntriples = { error: error.message };
  }

  return results;
}

// ============================================================================
// Main Runner
// ============================================================================

async function runAllBenchmarks() {
  console.log('╔═══════════════════════════════════════════════════════════╗');
  console.log('║   UNRDF COMPREHENSIVE PERFORMANCE ANALYSIS               ║');
  console.log('╚═══════════════════════════════════════════════════════════╝');

  const results = {
    timestamp: new Date().toISOString(),
    nodeVersion: process.version,
    platform: process.platform,
    arch: process.arch,
  };

  try {
    results.storeOperations = await benchmarkStoreOperations();
    results.scalability = await benchmarkScalability();
    results.io = await benchmarkIO();
    results.resourceCleanup = await testResourceCleanup();
    results.bottlenecks = await analyzeBottlenecks();

    // Summary
    console.log('\n╔═══════════════════════════════════════════════════════════╗');
    console.log('║   PERFORMANCE SUMMARY                                     ║');
    console.log('╚═══════════════════════════════════════════════════════════╝\n');

    console.log('STORE OPERATIONS:');
    console.log(`  add:               ${results.storeOperations.add.throughput} ops/sec`);
    console.log(
      `  match (specific):  ${results.storeOperations.match_specific.throughput} ops/sec`
    );
    console.log(`  match (all):       ${results.storeOperations.match_all.throughput} ops/sec`);
    console.log(`  has:               ${results.storeOperations.has.throughput} ops/sec`);
    console.log(`  delete:            ${results.storeOperations.delete.throughput} ops/sec`);
    console.log(`  size:              ${results.storeOperations.size.throughput} ops/sec`);

    console.log('\nCONCURRENT OPERATIONS:');
    console.log(
      `  10 parallel:       ${results.storeOperations.match_concurrent_10.throughput} ops/sec`
    );
    console.log(
      `  100 parallel:      ${results.storeOperations.match_concurrent_100.throughput} ops/sec`
    );
    console.log(
      `  1000 parallel:     ${results.storeOperations.match_concurrent_1000.throughput} ops/sec`
    );

    console.log('\nSCALABILITY (Match All Quads):');
    console.log(
      `  100 quads:         ${results.scalability.match.size_100.avgTime}ms avg`
    );
    console.log(
      `  1000 quads:        ${results.scalability.match.size_1000.avgTime}ms avg`
    );
    console.log(
      `  5000 quads:        ${results.scalability.match.size_5000.avgTime}ms avg`
    );

    console.log('\nRESOURCE CLEANUP:');
    console.log(`  Initial Memory:    ${results.resourceCleanup.initialMemory}`);
    console.log(`  Final Memory:      ${results.resourceCleanup.finalMemory}`);
    console.log(`  Memory Delta:      ${results.resourceCleanup.memoryDelta}`);
    console.log(
      `  Leak Detected:     ${results.resourceCleanup.leakDetected ? 'YES ⚠️' : 'NO ✅'}`
    );

    console.log('\nBOTTLENECKS:');
    if (results.bottlenecks.count === 0) {
      console.log('  None detected ✅');
    } else {
      results.bottlenecks.bottlenecks.forEach(b => {
        console.log(`  - ${b.operation} (${b.severity})`);
        console.log(`    Duration: ${b.duration}ms`);
        console.log(`    Recommendation: ${b.recommendation}`);
      });
    }

    // Save results
    const fs = await import('node:fs/promises');
    const outputPath = '/Users/sac/unrdf/packages/atomvm/benchmarks/performance-results.json';
    await fs.writeFile(outputPath, JSON.stringify(results, null, 2));

    console.log('\n✅ Performance analysis complete!');
    console.log(`   Results saved to: ${outputPath}\n`);

    return results;
  } catch (error) {
    console.error('❌ Benchmark failed:', error);
    throw error;
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  runAllBenchmarks()
    .then(() => process.exit(0))
    .catch(error => {
      console.error(error);
      process.exit(1);
    });
}

export { runAllBenchmarks };
