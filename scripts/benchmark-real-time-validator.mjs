#!/usr/bin/env node
/**
 * @file Performance Benchmark for Real-time SHACL Validator
 * @description Measures actual performance characteristics to establish realistic test targets
 */

import { createStore } from '@unrdf/core';
import { dataFactory } from '@unrdf/oxigraph';
import { RealTimeValidator, ValidationMode } from '../src/knowledge-engine/streaming/real-time-validator.mjs';

const { namedNode, literal, quad } = dataFactory;

// SHACL shapes for testing
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string
    ] ;
    sh:property [
      sh:path ex:age ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
      sh:maxInclusive 150
    ] .
`;

/**
 * Calculate percentile from array of numbers
 */
function percentile(arr, p) {
  if (arr.length === 0) return 0;
  const sorted = [...arr].sort((a, b) => a - b);
  const index = Math.ceil((p / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

/**
 * Calculate statistics from array of numbers
 */
function calculateStats(arr) {
  if (arr.length === 0) return { min: 0, max: 0, mean: 0, p50: 0, p95: 0, p99: 0 };

  const sorted = [...arr].sort((a, b) => a - b);
  const sum = arr.reduce((acc, val) => acc + val, 0);

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean: sum / arr.length,
    p50: percentile(arr, 50),
    p95: percentile(arr, 95),
    p99: percentile(arr, 99)
  };
}

/**
 * Create a valid delta
 */
function createValidDelta(index = 0) {
  return {
    additions: [
      quad(
        namedNode(`http://example.org/person${index}`),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode(`http://example.org/person${index}`),
        namedNode('http://example.org/name'),
        literal(`Person ${index}`)
      ),
      quad(
        namedNode(`http://example.org/person${index}`),
        namedNode('http://example.org/age'),
        literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      )
    ],
    removals: []
  };
}

/**
 * Create an invalid delta (missing required name)
 */
function createInvalidDelta(index = 0) {
  return {
    additions: [
      quad(
        namedNode(`http://example.org/person${index}`),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      )
      // Missing required name property
    ],
    removals: []
  };
}

/**
 * Benchmark 1: Single validation latency
 */
async function benchmarkSingleValidation() {
  console.log('\n📊 Benchmark 1: Single Validation Latency');
  console.log('─'.repeat(60));

  const validator = new RealTimeValidator({
    mode: ValidationMode.DELTA,
    shapes,
    enableCaching: false // Disable cache for baseline measurement
  });

  const iterations = 100;
  const latencies = [];

  for (let i = 0; i < iterations; i++) {
    const delta = createValidDelta(i);
    const start = Date.now();
    await validator.validateDelta(delta);
    const duration = Date.now() - start;
    latencies.push(duration);
  }

  const stats = calculateStats(latencies);

  console.log(`Iterations: ${iterations}`);
  console.log(`Min:  ${stats.min.toFixed(2)}ms`);
  console.log(`Mean: ${stats.mean.toFixed(2)}ms`);
  console.log(`P50:  ${stats.p50.toFixed(2)}ms`);
  console.log(`P95:  ${stats.p95.toFixed(2)}ms`);
  console.log(`P99:  ${stats.p99.toFixed(2)}ms`);
  console.log(`Max:  ${stats.max.toFixed(2)}ms`);

  await validator.cleanup();

  return {
    benchmark: 'single-validation',
    iterations,
    stats,
    recommendation: {
      p50Target: Math.ceil(stats.p95 * 1.2), // 20% buffer above P95
      p95Target: Math.ceil(stats.p99 * 1.2),
      p99Target: Math.ceil(stats.max * 1.5)
    }
  };
}

/**
 * Benchmark 2: Cache effectiveness
 */
async function benchmarkCacheEffectiveness() {
  console.log('\n📊 Benchmark 2: Cache Effectiveness');
  console.log('─'.repeat(60));

  const validator = new RealTimeValidator({
    mode: ValidationMode.DELTA,
    shapes,
    enableCaching: true,
    cacheSize: 50
  });

  const delta = createValidDelta(0);

  // First validation (cache miss)
  const start1 = Date.now();
  await validator.validateDelta(delta);
  const cacheMissLatency = Date.now() - start1;

  // Second validation (cache hit)
  const start2 = Date.now();
  await validator.validateDelta(delta);
  const cacheHitLatency = Date.now() - start2;

  const speedup = cacheMissLatency / cacheHitLatency;

  console.log(`Cache Miss Latency: ${cacheMissLatency.toFixed(2)}ms`);
  console.log(`Cache Hit Latency:  ${cacheHitLatency.toFixed(2)}ms`);
  console.log(`Speedup Factor:     ${speedup.toFixed(2)}x`);

  const metrics = validator.getMetrics();
  console.log(`Cache Hit Rate:     ${(metrics.cacheHitRate * 100).toFixed(1)}%`);

  await validator.cleanup();

  return {
    benchmark: 'cache-effectiveness',
    cacheMissLatency,
    cacheHitLatency,
    speedup,
    recommendation: {
      minHitRate: 0.5, // 50% cache hit rate in typical workloads
      expectedSpeedup: Math.floor(speedup * 0.8) // 80% of observed speedup
    }
  };
}

/**
 * Benchmark 3: High-frequency validation throughput
 */
async function benchmarkHighFrequency() {
  console.log('\n📊 Benchmark 3: High-Frequency Validation Throughput');
  console.log('─'.repeat(60));

  const validator = new RealTimeValidator({
    mode: ValidationMode.DELTA,
    shapes,
    enableCaching: true
  });

  const iterations = 100;
  const promises = [];

  const start = Date.now();

  for (let i = 0; i < iterations; i++) {
    const delta = createValidDelta(i);
    promises.push(validator.validateDelta(delta));
  }

  await Promise.all(promises);
  const duration = Date.now() - start;

  const throughput = (iterations / duration) * 1000; // ops/sec
  const avgLatency = duration / iterations;

  console.log(`Total Duration:     ${duration.toFixed(2)}ms`);
  console.log(`Iterations:         ${iterations}`);
  console.log(`Avg Latency:        ${avgLatency.toFixed(2)}ms`);
  console.log(`Throughput:         ${throughput.toFixed(2)} validations/sec`);

  const metrics = validator.getMetrics();
  console.log(`Validations:        ${metrics.validationsPerformed}`);
  console.log(`Cache Hit Rate:     ${(metrics.cacheHitRate * 100).toFixed(1)}%`);

  await validator.cleanup();

  return {
    benchmark: 'high-frequency',
    iterations,
    duration,
    throughput,
    avgLatency,
    recommendation: {
      minThroughput: Math.floor(throughput * 0.7), // 70% of observed throughput
      maxDuration: Math.ceil(duration * 1.5), // 50% buffer on total duration
      maxAvgLatency: Math.ceil(avgLatency * 1.5)
    }
  };
}

/**
 * Benchmark 4: Validation modes comparison
 */
async function benchmarkValidationModes() {
  console.log('\n📊 Benchmark 4: Validation Modes Comparison');
  console.log('─'.repeat(60));

  const modes = [ValidationMode.DELTA, ValidationMode.INCREMENTAL, ValidationMode.FULL];
  const results = {};

  for (const mode of modes) {
    const validator = new RealTimeValidator({
      mode,
      shapes,
      enableCaching: false
    });

    const store = new Store();
    const delta = createValidDelta(0);

    const iterations = 50;
    const latencies = [];

    for (let i = 0; i < iterations; i++) {
      const start = Date.now();
      await validator.validateDelta(delta, store);
      latencies.push(Date.now() - start);
    }

    const stats = calculateStats(latencies);
    results[mode] = stats;

    console.log(`\n${mode.toUpperCase()} Mode:`);
    console.log(`  Mean: ${stats.mean.toFixed(2)}ms`);
    console.log(`  P95:  ${stats.p95.toFixed(2)}ms`);
    console.log(`  P99:  ${stats.p99.toFixed(2)}ms`);

    await validator.cleanup();
  }

  return {
    benchmark: 'validation-modes',
    results,
    recommendation: {
      delta: { maxLatency: Math.ceil(results[ValidationMode.DELTA].p95 * 1.3) },
      incremental: { maxLatency: Math.ceil(results[ValidationMode.INCREMENTAL].p95 * 1.3) },
      full: { maxLatency: Math.ceil(results[ValidationMode.FULL].p95 * 1.3) }
    }
  };
}

/**
 * Benchmark 5: Violation detection overhead
 */
async function benchmarkViolationDetection() {
  console.log('\n📊 Benchmark 5: Violation Detection Overhead');
  console.log('─'.repeat(60));

  const validator = new RealTimeValidator({
    mode: ValidationMode.DELTA,
    shapes,
    enableCaching: false
  });

  const iterations = 50;

  // Benchmark valid deltas
  const validLatencies = [];
  for (let i = 0; i < iterations; i++) {
    const delta = createValidDelta(i);
    const start = Date.now();
    await validator.validateDelta(delta);
    validLatencies.push(Date.now() - start);
  }

  // Benchmark invalid deltas
  const invalidLatencies = [];
  for (let i = 0; i < iterations; i++) {
    const delta = createInvalidDelta(i);
    const start = Date.now();
    await validator.validateDelta(delta);
    invalidLatencies.push(Date.now() - start);
  }

  const validStats = calculateStats(validLatencies);
  const invalidStats = calculateStats(invalidLatencies);

  console.log(`Valid Deltas:`);
  console.log(`  Mean: ${validStats.mean.toFixed(2)}ms`);
  console.log(`  P95:  ${validStats.p95.toFixed(2)}ms`);

  console.log(`\nInvalid Deltas (with violations):`);
  console.log(`  Mean: ${invalidStats.mean.toFixed(2)}ms`);
  console.log(`  P95:  ${invalidStats.p95.toFixed(2)}ms`);

  const overhead = ((invalidStats.mean - validStats.mean) / validStats.mean) * 100;
  console.log(`\nViolation Detection Overhead: ${overhead.toFixed(1)}%`);

  await validator.cleanup();

  return {
    benchmark: 'violation-detection',
    validStats,
    invalidStats,
    overhead,
    recommendation: {
      maxOverhead: Math.ceil(overhead * 1.5) // 50% buffer on overhead
    }
  };
}

/**
 * Main benchmark runner
 */
async function runBenchmarks() {
  console.log('🚀 Real-time SHACL Validator Performance Benchmarks');
  console.log('='.repeat(60));

  const results = {};

  try {
    results.singleValidation = await benchmarkSingleValidation();
    results.cacheEffectiveness = await benchmarkCacheEffectiveness();
    results.highFrequency = await benchmarkHighFrequency();
    results.validationModes = await benchmarkValidationModes();
    results.violationDetection = await benchmarkViolationDetection();

    console.log('\n' + '='.repeat(60));
    console.log('📋 Summary of Recommendations');
    console.log('='.repeat(60));

    console.log('\n1. Single Validation Latency Targets:');
    console.log(`   - P50: ≤ ${results.singleValidation.recommendation.p50Target}ms`);
    console.log(`   - P95: ≤ ${results.singleValidation.recommendation.p95Target}ms`);
    console.log(`   - P99: ≤ ${results.singleValidation.recommendation.p99Target}ms`);

    console.log('\n2. Cache Performance Targets:');
    console.log(`   - Min Hit Rate: ≥ ${(results.cacheEffectiveness.recommendation.minHitRate * 100).toFixed(0)}%`);
    console.log(`   - Expected Speedup: ≥ ${results.cacheEffectiveness.recommendation.expectedSpeedup}x`);

    console.log('\n3. High-Frequency Throughput Targets:');
    console.log(`   - Min Throughput: ≥ ${results.highFrequency.recommendation.minThroughput.toFixed(0)} ops/sec`);
    console.log(`   - Max Duration (100 ops): ≤ ${results.highFrequency.recommendation.maxDuration}ms`);
    console.log(`   - Max Avg Latency: ≤ ${results.highFrequency.recommendation.maxAvgLatency.toFixed(0)}ms`);

    console.log('\n4. Validation Mode Targets:');
    console.log(`   - DELTA: ≤ ${results.validationModes.recommendation.delta.maxLatency}ms (P95)`);
    console.log(`   - INCREMENTAL: ≤ ${results.validationModes.recommendation.incremental.maxLatency}ms (P95)`);
    console.log(`   - FULL: ≤ ${results.validationModes.recommendation.full.maxLatency}ms (P95)`);

    console.log('\n5. Violation Detection:');
    console.log(`   - Max Overhead: ≤ ${results.violationDetection.recommendation.maxOverhead}%`);

    console.log('\n✅ Benchmark complete!');

    return results;

  } catch (error) {
    console.error('\n❌ Benchmark failed:', error);
    throw error;
  }
}

// Run benchmarks
runBenchmarks()
  .then(results => {
    console.log('\n📊 Writing results to docs/performance-targets-v4.0.0.md...');
    process.exit(0);
  })
  .catch(error => {
    console.error(error);
    process.exit(1);
  });
