/**
 * SLA Enforcement Overhead Benchmark Suite
 *
 * Measures the performance cost of SLA tracking and enforcement.
 * Compares operations with and without SLA tracking.
 *
 * @module benchmarks/sla-overhead-benchmark
 */

import { performance } from 'perf_hooks';
import {
  startRoundtrip,
  endRoundtrip,
  getSLAStats,
  resetSLAStats,
  OPERATION_TYPES,
} from '../src/roundtrip-sla.mjs';

/**
 * Simulate operation without SLA tracking
 * @returns {Promise<number>} Operation latency
 */
async function operationWithoutSLA() {
  const startTime = performance.now();

  // Simulate work
  await new Promise(resolve => setTimeout(resolve, Math.random() * 2));

  return performance.now() - startTime;
}

/**
 * Simulate operation with SLA tracking
 * @returns {Promise<number>} Operation latency
 */
async function operationWithSLA() {
  const startTime = performance.now();

  // Start SLA tracking
  const operationId = startRoundtrip(OPERATION_TYPES.EMIT_EVENT);

  // Simulate work
  await new Promise(resolve => setTimeout(resolve, Math.random() * 2));

  // End SLA tracking
  endRoundtrip(operationId, true);

  return performance.now() - startTime;
}

/**
 * Measure SLA tracking overhead
 * @param {number} iterations - Number of operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureSLAOverhead(iterations = 1000) {
  console.log(`\n=== SLA Overhead Benchmark (${iterations} iterations) ===`);

  resetSLAStats();

  // Warmup
  for (let i = 0; i < 100; i++) {
    await operationWithoutSLA();
    await operationWithSLA();
  }

  // Measure WITHOUT SLA
  console.log('\nMeasuring WITHOUT SLA tracking...');
  const withoutSLATimes = [];
  const withoutSLAStart = performance.now();

  for (let i = 0; i < iterations; i++) {
    const latency = await operationWithoutSLA();
    withoutSLATimes.push(latency);
  }

  const withoutSLATotal = performance.now() - withoutSLAStart;

  // Measure WITH SLA
  console.log('Measuring WITH SLA tracking...');
  const withSLATimes = [];
  const withSLAStart = performance.now();

  for (let i = 0; i < iterations; i++) {
    const latency = await operationWithSLA();
    withSLATimes.push(latency);
  }

  const withSLATotal = performance.now() - withSLAStart;

  // Calculate statistics
  const withoutSLAMean = withoutSLATimes.reduce((a, b) => a + b, 0) / withoutSLATimes.length;
  const withSLAMean = withSLATimes.reduce((a, b) => a + b, 0) / withSLATimes.length;

  const overhead = withSLAMean - withoutSLAMean;
  const overheadPercent = (overhead / withoutSLAMean * 100).toFixed(2);

  const slaStats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);

  console.log(`\nResults:`);
  console.log(`\nWithout SLA Tracking:`);
  console.log(`  Total Time: ${withoutSLATotal.toFixed(2)}ms`);
  console.log(`  Mean: ${withoutSLAMean.toFixed(3)}ms`);
  console.log(`  Throughput: ${(iterations / (withoutSLATotal / 1000)).toFixed(0)} ops/sec`);
  console.log(`\nWith SLA Tracking:`);
  console.log(`  Total Time: ${withSLATotal.toFixed(2)}ms`);
  console.log(`  Mean: ${withSLAMean.toFixed(3)}ms`);
  console.log(`  Throughput: ${(iterations / (withSLATotal / 1000)).toFixed(0)} ops/sec`);
  console.log(`\nOverhead:`);
  console.log(`  Absolute: ${overhead.toFixed(3)}ms`);
  console.log(`  Percentage: ${overheadPercent}%`);
  console.log(`  Acceptable: ${parseFloat(overheadPercent) < 5 ? '✓ YES' : '⚠ NO'} (<5% threshold)`);
  console.log(`\nSLA Stats:`);
  console.log(`  Operations: ${slaStats.count}`);
  console.log(`  Avg Latency: ${slaStats.averageLatency.toFixed(3)}ms`);
  console.log(`  SLA Compliant: ${slaStats.slaCompliant ? '✓ YES' : '✗ NO'}`);

  return {
    withoutSLA: {
      totalTime: withoutSLATotal.toFixed(2),
      mean: withoutSLAMean.toFixed(3),
      throughput: (iterations / (withoutSLATotal / 1000)).toFixed(0),
    },
    withSLA: {
      totalTime: withSLATotal.toFixed(2),
      mean: withSLAMean.toFixed(3),
      throughput: (iterations / (withSLATotal / 1000)).toFixed(0),
    },
    overhead: {
      absolute: overhead.toFixed(3),
      percentage: overheadPercent,
      acceptable: parseFloat(overheadPercent) < 5,
    },
    slaStats,
  };
}

/**
 * Measure SLA validation performance
 * @param {number} iterations - Number of validations
 * @returns {Object} Benchmark results
 */
export async function measureSLAValidation(iterations = 10000) {
  console.log(`\n=== SLA Validation Benchmark (${iterations} iterations) ===`);

  resetSLAStats();

  // Populate with some data
  for (let i = 0; i < 100; i++) {
    const id = startRoundtrip(OPERATION_TYPES.EMIT_EVENT);
    endRoundtrip(id, true);
  }

  const timings = [];
  const startTime = performance.now();

  for (let i = 0; i < iterations; i++) {
    const validateStart = performance.now();
    const stats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
    const validateEnd = performance.now();

    timings.push(validateEnd - validateStart);
  }

  const totalTime = performance.now() - startTime;
  const mean = timings.reduce((a, b) => a + b, 0) / timings.length;

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Mean: ${(mean * 1000).toFixed(3)}µs`);
  console.log(`  Throughput: ${(iterations / (totalTime / 1000)).toFixed(0)} validations/sec`);

  return {
    iterations,
    totalTime: totalTime.toFixed(2),
    mean: (mean * 1000).toFixed(3),
    throughput: (iterations / (totalTime / 1000)).toFixed(0),
  };
}

/**
 * Measure concurrent SLA tracking performance
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureConcurrentSLATracking(concurrency = 100) {
  console.log(`\n=== Concurrent SLA Tracking (${concurrency} concurrent ops) ===`);

  resetSLAStats();

  const startTime = performance.now();

  // Create concurrent operations with SLA tracking
  const operations = [];
  for (let i = 0; i < concurrency; i++) {
    operations.push(
      (async () => {
        const id = startRoundtrip(OPERATION_TYPES.EMIT_EVENT);
        await new Promise(resolve => setTimeout(resolve, Math.random() * 10));
        endRoundtrip(id, Math.random() > 0.01); // 1% error rate
      })()
    );
  }

  await Promise.all(operations);

  const totalTime = performance.now() - startTime;
  const stats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Operations: ${stats.count}`);
  console.log(`  Avg Latency: ${stats.averageLatency.toFixed(3)}ms`);
  console.log(`  Error Rate: ${(stats.errorRate * 100).toFixed(2)}%`);
  console.log(`  SLA Compliant: ${stats.slaCompliant ? '✓ YES' : '✗ NO'}`);
  console.log(`  Throughput: ${(concurrency / (totalTime / 1000)).toFixed(0)} ops/sec`);

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    operations: stats.count,
    avgLatency: stats.averageLatency.toFixed(3),
    errorRate: (stats.errorRate * 100).toFixed(2),
    slaCompliant: stats.slaCompliant,
    throughput: (concurrency / (totalTime / 1000)).toFixed(0),
  };
}

/**
 * Run comprehensive SLA overhead benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runComprehensiveSLABenchmarks() {
  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  SLA ENFORCEMENT OVERHEAD BENCHMARK SUITE     ║');
  console.log('╚════════════════════════════════════════════════╝');

  const results = {
    overhead: await measureSLAOverhead(1000),
    validation: await measureSLAValidation(10000),
    concurrent: await measureConcurrentSLATracking(100),
  };

  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  SLA OVERHEAD BENCHMARK SUMMARY               ║');
  console.log('╚════════════════════════════════════════════════╝');
  console.log('\nSLA Tracking Overhead:');
  console.log(`  Without SLA: ${results.overhead.withoutSLA.mean}ms avg`);
  console.log(`  With SLA: ${results.overhead.withSLA.mean}ms avg`);
  console.log(`  Overhead: ${results.overhead.overhead.percentage}%`);
  console.log(`  Acceptable: ${results.overhead.overhead.acceptable ? '✓ YES' : '⚠ NO'}`);
  console.log('\nSLA Validation (10,000 iterations):');
  console.log(`  Mean: ${results.validation.mean}µs`);
  console.log(`  Throughput: ${results.validation.throughput} validations/sec`);
  console.log('\nConcurrent SLA Tracking (100 ops):');
  console.log(`  Avg Latency: ${results.concurrent.avgLatency}ms`);
  console.log(`  SLA Compliant: ${results.concurrent.slaCompliant ? '✓ YES' : '✗ NO'}`);

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runComprehensiveSLABenchmarks().catch(console.error);
}
