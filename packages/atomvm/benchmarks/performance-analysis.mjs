#!/usr/bin/env node
/**
 * @fileoverview Comprehensive Performance Analysis for UNRDF Packages
 *
 * MEASURES:
 * 1. Execution time (baseline + 1000 iterations)
 * 2. Memory usage (baseline, peak, growth rate)
 * 3. CPU usage and hotspots
 * 4. I/O operations and blocking calls
 * 5. Concurrency performance (10, 100, 1000 parallel ops)
 * 6. Throughput (operations per second)
 * 7. Latency percentiles (p50, p95, p99)
 * 8. Resource cleanup
 * 9. Scalability
 * 10. Bottlenecks
 *
 * @version 1.0.0
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import * as validationUtils from '../../core/src/utils/validation-utils.mjs';
import * as graphUtils from '../../core/src/utils/graph-utils.mjs';
import * as transformUtils from '../../core/src/utils/transform-utils.mjs';

const { namedNode, literal, quad } = dataFactory;

// ============================================================================
// Performance Measurement Utilities
// ============================================================================

/**
 * Get current memory usage
 * @returns {object} Memory usage stats
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: usage.rss,
    heapTotal: usage.heapTotal,
    heapUsed: usage.heapUsed,
    external: usage.external,
    arrayBuffers: usage.arrayBuffers,
  };
}

/**
 * Format bytes to human-readable string
 * @param {number} bytes - Bytes
 * @returns {string} Formatted string
 */
function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${(bytes / k ** i).toFixed(2)} ${sizes[i]}`;
}

/**
 * Calculate percentile
 * @param {number[]} values - Sorted values
 * @param {number} percentile - Percentile (0-100)
 * @returns {number} Percentile value
 */
function calculatePercentile(values, percentile) {
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil((percentile / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

/**
 * Measure execution time and memory
 * @param {Function} fn - Function to measure
 * @param {number} iterations - Number of iterations
 * @returns {Promise<object>} Performance metrics
 */
async function measurePerformance(fn, iterations = 1000) {
  const memoryBefore = getMemoryUsage();
  const times = [];
  const memorySnapshots = [];

  // Warmup
  for (let i = 0; i < 10; i++) {
    await fn();
  }

  // Force GC if available
  if (global.gc) {
    global.gc();
  }

  // Measure iterations
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    const end = performance.now();
    times.push(end - start);

    // Sample memory every 100 iterations
    if (i % 100 === 0) {
      memorySnapshots.push(getMemoryUsage());
    }
  }

  const memoryAfter = getMemoryUsage();

  // Calculate statistics
  const totalTime = times.reduce((sum, t) => sum + t, 0);
  const avgTime = totalTime / iterations;
  const minTime = Math.min(...times);
  const maxTime = Math.max(...times);
  const p50 = calculatePercentile(times, 50);
  const p95 = calculatePercentile(times, 95);
  const p99 = calculatePercentile(times, 99);

  // Calculate memory growth
  const memoryGrowth = memoryAfter.heapUsed - memoryBefore.heapUsed;
  const peakMemory = Math.max(...memorySnapshots.map(m => m.heapUsed));

  return {
    iterations,
    totalTime: totalTime.toFixed(2),
    avgTime: avgTime.toFixed(4),
    minTime: minTime.toFixed(4),
    maxTime: maxTime.toFixed(4),
    p50: p50.toFixed(4),
    p95: p95.toFixed(4),
    p99: p99.toFixed(4),
    throughput: (iterations / (totalTime / 1000)).toFixed(2),
    memoryGrowth: formatBytes(memoryGrowth),
    memoryGrowthBytes: memoryGrowth,
    peakMemory: formatBytes(peakMemory),
    peakMemoryBytes: peakMemory,
    baselineMemory: formatBytes(memoryBefore.heapUsed),
  };
}

/**
 * Measure concurrent performance
 * @param {Function} fn - Function to measure
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<object>} Concurrency metrics
 */
async function measureConcurrency(fn, concurrency) {
  const start = performance.now();
  const promises = Array.from({ length: concurrency }, () => fn());
  await Promise.all(promises);
  const end = performance.now();

  const totalTime = end - start;
  const throughput = concurrency / (totalTime / 1000);

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    avgTimePerOp: (totalTime / concurrency).toFixed(4),
    throughput: throughput.toFixed(2),
  };
}

// ============================================================================
// Test Data Generators
// ============================================================================

/**
 * Create test store with N triples
 * @param {number} size - Number of triples
 * @returns {object} Test store
 */
function createTestStore(size = 1000) {
  const store = createStore();
  for (let i = 0; i < size; i++) {
    store.add(
      quad(
        namedNode(`http://example.org/subject${i}`),
        namedNode('http://example.org/predicate'),
        literal(`Object ${i}`)
      )
    );
  }
  return store;
}

/**
 * Create test quad
 * @returns {object} Test quad
 */
function createTestQuad() {
  return quad(
    namedNode('http://example.org/subject'),
    namedNode('http://example.org/predicate'),
    literal('test value')
  );
}

// ============================================================================
// Validation Utils Benchmarks
// ============================================================================

async function benchmarkValidationUtils() {
  console.log('\n=== VALIDATION UTILS PERFORMANCE ===\n');

  const results = {};

  // Test IRI validation
  console.log('Benchmarking validateIRI...');
  results.validateIRI = await measurePerformance(() => {
    validationUtils.validateIRI('http://example.org/test');
  }, 1000);

  // Test quad validation
  console.log('Benchmarking validateQuad...');
  const testQuad = createTestQuad();
  results.validateQuad = await measurePerformance(() => {
    validationUtils.validateQuad(testQuad);
  }, 1000);

  // Test store validation (expensive operation)
  console.log('Benchmarking validateStore...');
  const smallStore = createTestStore(100);
  results.validateStore_100 = await measurePerformance(() => {
    validationUtils.validateStore(smallStore);
  }, 100);

  const medStore = createTestStore(1000);
  results.validateStore_1000 = await measurePerformance(() => {
    validationUtils.validateStore(medStore);
  }, 10);

  // Test RDF constraints validation
  console.log('Benchmarking validateRDFConstraints...');
  results.validateRDFConstraints = await measurePerformance(() => {
    validationUtils.validateRDFConstraints(smallStore);
  }, 100);

  // Test concurrent validation
  console.log('Benchmarking concurrent validation...');
  results.validateIRI_concurrent_10 = await measureConcurrency(
    () => validationUtils.validateIRI('http://example.org/test'),
    10
  );
  results.validateIRI_concurrent_100 = await measureConcurrency(
    () => validationUtils.validateIRI('http://example.org/test'),
    100
  );
  results.validateIRI_concurrent_1000 = await measureConcurrency(
    () => validationUtils.validateIRI('http://example.org/test'),
    1000
  );

  return results;
}

// ============================================================================
// Graph Utils Benchmarks
// ============================================================================

async function benchmarkGraphUtils() {
  console.log('\n=== GRAPH UTILS PERFORMANCE ===\n');

  const results = {};
  const testStore = createTestStore(1000);

  // Test getObjects
  console.log('Benchmarking getObjects...');
  results.getObjects = await measurePerformance(() => {
    graphUtils.getObjects(
      testStore,
      'http://example.org/subject0',
      'http://example.org/predicate'
    );
  }, 1000);

  // Test getAllSubjects
  console.log('Benchmarking getAllSubjects...');
  results.getAllSubjects = await measurePerformance(() => {
    graphUtils.getAllSubjects(testStore);
  }, 100);

  // Test getProperties
  console.log('Benchmarking getProperties...');
  results.getProperties = await measurePerformance(() => {
    graphUtils.getProperties(testStore, 'http://example.org/subject0');
  }, 1000);

  // Test indexByPredicate
  console.log('Benchmarking indexByPredicate...');
  results.indexByPredicate = await measurePerformance(() => {
    graphUtils.indexByPredicate(testStore, 'http://example.org/predicate');
  }, 100);

  // Test concurrent graph operations
  console.log('Benchmarking concurrent graph queries...');
  results.getObjects_concurrent_10 = await measureConcurrency(
    () =>
      graphUtils.getObjects(
        testStore,
        'http://example.org/subject0',
        'http://example.org/predicate'
      ),
    10
  );
  results.getObjects_concurrent_100 = await measureConcurrency(
    () =>
      graphUtils.getObjects(
        testStore,
        'http://example.org/subject0',
        'http://example.org/predicate'
      ),
    100
  );

  // Scalability test - different store sizes
  console.log('Benchmarking scalability (store sizes)...');
  const sizes = [100, 500, 1000, 5000];
  results.scalability = {};
  for (const size of sizes) {
    const store = createTestStore(size);
    const metric = await measurePerformance(() => {
      graphUtils.getAllSubjects(store);
    }, 10);
    results.scalability[`size_${size}`] = metric;
  }

  return results;
}

// ============================================================================
// Transform Utils Benchmarks
// ============================================================================

async function benchmarkTransformUtils() {
  console.log('\n=== TRANSFORM UTILS PERFORMANCE ===\n');

  const results = {};
  const testStore = createTestStore(1000);

  // Test storeToJSONLD
  console.log('Benchmarking storeToJSONLD...');
  results.storeToJSONLD = await measurePerformance(() => {
    transformUtils.storeToJSONLD(testStore);
  }, 100);

  // Test storeToNTriples
  console.log('Benchmarking storeToNTriples...');
  results.storeToNTriples = await measurePerformance(() => {
    transformUtils.storeToNTriples(testStore);
  }, 100);

  // Test storeToCSV
  console.log('Benchmarking storeToCSV...');
  results.storeToCSV = await measurePerformance(() => {
    transformUtils.storeToCSV(testStore);
  }, 100);

  // Test transformStore
  console.log('Benchmarking transformStore...');
  const transformer = quad => quad; // Identity transformer
  results.transformStore = await measurePerformance(() => {
    transformUtils.transformStore(testStore, transformer);
  }, 10);

  // Test denormalizeStore
  console.log('Benchmarking denormalizeStore...');
  results.denormalizeStore = await measurePerformance(() => {
    transformUtils.denormalizeStore(testStore);
  }, 100);

  // Test concurrent transforms
  console.log('Benchmarking concurrent transforms...');
  results.storeToNTriples_concurrent_10 = await measureConcurrency(
    () => transformUtils.storeToNTriples(testStore),
    10
  );

  // Scalability test
  console.log('Benchmarking transform scalability...');
  const sizes = [100, 500, 1000, 5000];
  results.scalability = {};
  for (const size of sizes) {
    const store = createTestStore(size);
    const metric = await measurePerformance(() => {
      transformUtils.storeToNTriples(store);
    }, 10);
    results.scalability[`size_${size}`] = metric;
  }

  return results;
}

// ============================================================================
// Resource Cleanup Test
// ============================================================================

async function testResourceCleanup() {
  console.log('\n=== RESOURCE CLEANUP TEST ===\n');

  const initialMemory = getMemoryUsage();

  // Create and destroy stores
  for (let i = 0; i < 100; i++) {
    const store = createTestStore(1000);
    // Intentionally not using store to test cleanup
    void store;
  }

  // Force GC if available
  if (global.gc) {
    global.gc();
    // Wait for GC to complete
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  const finalMemory = getMemoryUsage();
  const memoryLeak = finalMemory.heapUsed - initialMemory.heapUsed;

  return {
    initialMemory: formatBytes(initialMemory.heapUsed),
    finalMemory: formatBytes(finalMemory.heapUsed),
    memoryDelta: formatBytes(memoryLeak),
    memoryLeakBytes: memoryLeak,
    leakDetected: memoryLeak > 10 * 1024 * 1024, // > 10MB indicates leak
  };
}

// ============================================================================
// Bottleneck Analysis
// ============================================================================

async function analyzeBottlenecks() {
  console.log('\n=== BOTTLENECK ANALYSIS ===\n');

  const bottlenecks = [];
  const testStore = createTestStore(1000);

  // Test 1: Store iteration bottleneck
  const iterationTime = performance.now();
  let count = 0;
  for (const _quad of testStore) {
    count++;
  }
  const iterationDuration = performance.now() - iterationTime;

  if (iterationDuration > 10) {
    bottlenecks.push({
      operation: 'Store iteration',
      duration: iterationDuration.toFixed(2),
      severity: 'MEDIUM',
      recommendation: 'Consider using indexed queries instead of full iteration',
    });
  }

  // Test 2: Validation pipeline bottleneck
  const validationStart = performance.now();
  validationUtils.validateStore(testStore);
  const validationDuration = performance.now() - validationStart;

  if (validationDuration > 50) {
    bottlenecks.push({
      operation: 'Store validation',
      duration: validationDuration.toFixed(2),
      severity: 'HIGH',
      recommendation: 'Validation scales O(n) with store size. Use incremental validation.',
    });
  }

  // Test 3: Transform bottleneck
  const transformStart = performance.now();
  transformUtils.storeToJSONLD(testStore);
  const transformDuration = performance.now() - transformStart;

  if (transformDuration > 100) {
    bottlenecks.push({
      operation: 'storeToJSONLD transform',
      duration: transformDuration.toFixed(2),
      severity: 'HIGH',
      recommendation: 'JSON-LD transformation creates intermediate objects. Use streaming.',
    });
  }

  // Test 4: Memory allocation bottleneck
  const allocStart = performance.now();
  const memBefore = getMemoryUsage();
  const largeStore = createTestStore(10000);
  const memAfter = getMemoryUsage();
  const allocDuration = performance.now() - allocStart;
  const memoryUsed = memAfter.heapUsed - memBefore.heapUsed;

  if (memoryUsed > 50 * 1024 * 1024) {
    // > 50MB
    bottlenecks.push({
      operation: 'Large store creation (10k triples)',
      duration: allocDuration.toFixed(2),
      memoryUsed: formatBytes(memoryUsed),
      severity: 'MEDIUM',
      recommendation: 'Consider using streaming insertion for large datasets',
    });
  }

  return { bottlenecks, count: bottlenecks.length };
}

// ============================================================================
// Main Benchmark Runner
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
    // Run benchmarks
    results.validationUtils = await benchmarkValidationUtils();
    results.graphUtils = await benchmarkGraphUtils();
    results.transformUtils = await benchmarkTransformUtils();
    results.resourceCleanup = await testResourceCleanup();
    results.bottlenecks = await analyzeBottlenecks();

    // Generate summary
    console.log('\n╔═══════════════════════════════════════════════════════════╗');
    console.log('║   PERFORMANCE SUMMARY                                     ║');
    console.log('╚═══════════════════════════════════════════════════════════╝\n');

    // Validation Utils Summary
    console.log('VALIDATION UTILS:');
    console.log(
      `  validateIRI:        ${results.validationUtils.validateIRI.throughput} ops/sec`
    );
    console.log(
      `  validateQuad:       ${results.validationUtils.validateQuad.throughput} ops/sec`
    );
    console.log(
      `  validateStore(100): ${results.validationUtils.validateStore_100.throughput} ops/sec`
    );
    console.log(
      `  Concurrent (1000):  ${results.validationUtils.validateIRI_concurrent_1000.throughput} ops/sec`
    );

    // Graph Utils Summary
    console.log('\nGRAPH UTILS:');
    console.log(`  getObjects:         ${results.graphUtils.getObjects.throughput} ops/sec`);
    console.log(
      `  getAllSubjects:     ${results.graphUtils.getAllSubjects.throughput} ops/sec`
    );
    console.log(`  getProperties:      ${results.graphUtils.getProperties.throughput} ops/sec`);
    console.log(
      `  indexByPredicate:   ${results.graphUtils.indexByPredicate.throughput} ops/sec`
    );

    // Transform Utils Summary
    console.log('\nTRANSFORM UTILS:');
    console.log(
      `  storeToJSONLD:      ${results.transformUtils.storeToJSONLD.throughput} ops/sec`
    );
    console.log(
      `  storeToNTriples:    ${results.transformUtils.storeToNTriples.throughput} ops/sec`
    );
    console.log(`  storeToCSV:         ${results.transformUtils.storeToCSV.throughput} ops/sec`);
    console.log(
      `  denormalizeStore:   ${results.transformUtils.denormalizeStore.throughput} ops/sec`
    );

    // Resource Cleanup
    console.log('\nRESOURCE CLEANUP:');
    console.log(`  Initial Memory:     ${results.resourceCleanup.initialMemory}`);
    console.log(`  Final Memory:       ${results.resourceCleanup.finalMemory}`);
    console.log(`  Memory Delta:       ${results.resourceCleanup.memoryDelta}`);
    console.log(
      `  Leak Detected:      ${results.resourceCleanup.leakDetected ? 'YES ⚠️' : 'NO ✅'}`
    );

    // Bottlenecks
    console.log('\nBOTTLENECKS IDENTIFIED:');
    if (results.bottlenecks.count === 0) {
      console.log('  No significant bottlenecks detected ✅');
    } else {
      results.bottlenecks.bottlenecks.forEach(b => {
        console.log(`  - ${b.operation}`);
        console.log(`    Duration: ${b.duration}ms | Severity: ${b.severity}`);
        console.log(`    Recommendation: ${b.recommendation}`);
      });
    }

    // Write results to file
    const fs = await import('node:fs/promises');
    await fs.writeFile(
      '/Users/sac/unrdf/packages/atomvm/benchmarks/performance-results.json',
      JSON.stringify(results, null, 2)
    );

    console.log('\n✅ Performance analysis complete!');
    console.log(
      '   Results saved to: packages/atomvm/benchmarks/performance-results.json\n'
    );

    return results;
  } catch (error) {
    console.error('❌ Benchmark failed:', error);
    throw error;
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllBenchmarks()
    .then(() => process.exit(0))
    .catch(error => {
      console.error(error);
      process.exit(1);
    });
}

export { runAllBenchmarks, measurePerformance, measureConcurrency };
