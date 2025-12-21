/**
 * Circuit Breaker Performance Benchmark Suite
 *
 * Measures overhead and performance of circuit breaker pattern implementation.
 *
 * @module benchmarks/circuit-breaker-benchmark
 */

import { performance } from 'perf_hooks';
import { CircuitBreaker } from '../src/circuit-breaker.mjs';

/**
 * Measure circuit breaker overhead
 * @param {number} iterations - Number of operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureCircuitBreakerOverhead(iterations = 1000) {
  console.log(`\n=== Circuit Breaker Overhead (${iterations} iterations) ===`);

  const breaker = new CircuitBreaker({
    failureThreshold: 5,
    resetTimeout: 5000,
  });

  // Measure WITHOUT circuit breaker
  console.log('Measuring WITHOUT circuit breaker...');
  const withoutCBTimes = [];
  const withoutCBStart = performance.now();

  for (let i = 0; i < iterations; i++) {
    const opStart = performance.now();
    await Promise.resolve('success');
    withoutCBTimes.push(performance.now() - opStart);
  }

  const withoutCBTotal = performance.now() - withoutCBStart;

  // Measure WITH circuit breaker
  console.log('Measuring WITH circuit breaker...');
  const withCBTimes = [];
  const withCBStart = performance.now();

  for (let i = 0; i < iterations; i++) {
    const opStart = performance.now();
    await breaker.call(async () => 'success');
    withCBTimes.push(performance.now() - opStart);
  }

  const withCBTotal = performance.now() - withCBStart;

  // Calculate statistics
  const withoutCBMean = withoutCBTimes.reduce((a, b) => a + b, 0) / withoutCBTimes.length;
  const withCBMean = withCBTimes.reduce((a, b) => a + b, 0) / withCBTimes.length;

  const overhead = withCBMean - withoutCBMean;
  const overheadPercent = (overhead / withoutCBMean * 100).toFixed(2);

  console.log(`\nResults:`);
  console.log(`\nWithout Circuit Breaker:`);
  console.log(`  Total Time: ${withoutCBTotal.toFixed(2)}ms`);
  console.log(`  Mean: ${(withoutCBMean * 1000).toFixed(3)}µs`);
  console.log(`\nWith Circuit Breaker:`);
  console.log(`  Total Time: ${withCBTotal.toFixed(2)}ms`);
  console.log(`  Mean: ${(withCBMean * 1000).toFixed(3)}µs`);
  console.log(`\nOverhead:`);
  console.log(`  Absolute: ${(overhead * 1000).toFixed(3)}µs`);
  console.log(`  Percentage: ${overheadPercent}%`);
  console.log(`  Acceptable: ${parseFloat(overheadPercent) < 10 ? '✓ YES' : '⚠ NO'} (<10% threshold)`);

  return {
    withoutCB: {
      totalTime: withoutCBTotal.toFixed(2),
      mean: (withoutCBMean * 1000).toFixed(3),
    },
    withCB: {
      totalTime: withCBTotal.toFixed(2),
      mean: (withCBMean * 1000).toFixed(3),
    },
    overhead: {
      absolute: (overhead * 1000).toFixed(3),
      percentage: overheadPercent,
      acceptable: parseFloat(overheadPercent) < 10,
    },
  };
}

/**
 * Measure state transition performance
 * @param {number} cycles - Number of transition cycles
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureStateTransitions(cycles = 1000) {
  console.log(`\n=== Circuit Breaker State Transitions (${cycles} cycles) ===`);

  const breaker = new CircuitBreaker({
    failureThreshold: 3,
    resetTimeout: 100,
  });

  const timings = [];
  let openCount = 0;
  let closeCount = 0;

  console.log('Measuring state transitions...');

  for (let i = 0; i < cycles; i++) {
    const cycleStart = performance.now();

    // Cause failures to open circuit
    for (let j = 0; j < 3; j++) {
      try {
        await breaker.call(async () => {
          throw new Error('Simulated failure');
        });
      } catch (error) {
        // Expected
      }
    }

    if (breaker.getState() === 'open') {
      openCount++;
    }

    // Wait for reset timeout
    await new Promise(resolve => setTimeout(resolve, 110));

    // Close circuit
    if (breaker.canClose()) {
      breaker.close();
      closeCount++;
    }

    timings.push(performance.now() - cycleStart);

    if ((i + 1) % Math.floor(cycles / 10) === 0) {
      console.log(`  Progress: ${((i + 1) / cycles * 100).toFixed(0)}%`);
    }
  }

  const totalTime = timings.reduce((a, b) => a + b, 0);
  const mean = totalTime / cycles;
  const sortedTimings = [...timings].sort((a, b) => a - b);
  const median = sortedTimings[Math.floor(sortedTimings.length / 2)];

  console.log(`\nResults:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Mean Per Cycle: ${mean.toFixed(3)}ms`);
  console.log(`  Median: ${median.toFixed(3)}ms`);
  console.log(`  Open Count: ${openCount}/${cycles}`);
  console.log(`  Close Count: ${closeCount}/${cycles}`);
  console.log(`  Success Rate: ${(closeCount / cycles * 100).toFixed(1)}%`);

  return {
    cycles,
    totalTime: totalTime.toFixed(2),
    mean: mean.toFixed(3),
    median: median.toFixed(3),
    openCount,
    closeCount,
    successRate: (closeCount / cycles * 100).toFixed(1),
  };
}

/**
 * Measure failure detection latency
 * @param {number} iterations - Number of tests
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureFailureDetection(iterations = 100) {
  console.log(`\n=== Failure Detection Latency (${iterations} iterations) ===`);

  const detectionTimes = [];

  for (let i = 0; i < iterations; i++) {
    const breaker = new CircuitBreaker({
      failureThreshold: 5,
      resetTimeout: 1000,
    });

    const detectionStart = performance.now();

    // Cause failures until circuit opens
    let failures = 0;
    while (breaker.getState() !== 'open') {
      try {
        await breaker.call(async () => {
          throw new Error('Failure');
        });
      } catch (error) {
        failures++;
      }
    }

    const detectionTime = performance.now() - detectionStart;
    detectionTimes.push(detectionTime);
  }

  const sortedTimes = [...detectionTimes].sort((a, b) => a - b);
  const mean = detectionTimes.reduce((a, b) => a + b, 0) / detectionTimes.length;
  const median = sortedTimes[Math.floor(sortedTimes.length / 2)];
  const min = sortedTimes[0];
  const max = sortedTimes[sortedTimes.length - 1];

  console.log(`Results:`);
  console.log(`  Mean Detection Time: ${mean.toFixed(3)}ms`);
  console.log(`  Median: ${median.toFixed(3)}ms`);
  console.log(`  Min: ${min.toFixed(3)}ms`);
  console.log(`  Max: ${max.toFixed(3)}ms`);

  return {
    iterations,
    mean: mean.toFixed(3),
    median: median.toFixed(3),
    min: min.toFixed(3),
    max: max.toFixed(3),
  };
}

/**
 * Measure timeout accuracy
 * @param {number} iterations - Number of tests
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureTimeoutAccuracy(iterations = 100) {
  console.log(`\n=== Timeout Accuracy (${iterations} iterations) ===`);

  const targetTimeout = 1000; // 1 second
  const accuracies = [];

  for (let i = 0; i < iterations; i++) {
    const breaker = new CircuitBreaker({
      failureThreshold: 1,
      resetTimeout: targetTimeout,
    });

    // Open the circuit
    try {
      await breaker.call(async () => {
        throw new Error('Failure');
      });
    } catch (error) {
      // Expected
    }

    const openTime = Date.now();

    // Wait for circuit to be closeable
    while (!breaker.canClose()) {
      await new Promise(resolve => setTimeout(resolve, 10));
    }

    const actualTimeout = Date.now() - openTime;
    const accuracy = actualTimeout - targetTimeout;
    accuracies.push(accuracy);
  }

  const sortedAccuracies = [...accuracies].sort((a, b) => a - b);
  const mean = accuracies.reduce((a, b) => a + b, 0) / accuracies.length;
  const median = sortedAccuracies[Math.floor(sortedAccuracies.length / 2)];
  const min = sortedAccuracies[0];
  const max = sortedAccuracies[sortedAccuracies.length - 1];

  console.log(`Results:`);
  console.log(`  Target Timeout: ${targetTimeout}ms`);
  console.log(`  Mean Deviation: ${mean.toFixed(2)}ms`);
  console.log(`  Median Deviation: ${median.toFixed(2)}ms`);
  console.log(`  Min Deviation: ${min.toFixed(2)}ms`);
  console.log(`  Max Deviation: ${max.toFixed(2)}ms`);
  console.log(`  Accuracy: ${((1 - Math.abs(mean) / targetTimeout) * 100).toFixed(2)}%`);

  return {
    targetTimeout,
    meanDeviation: mean.toFixed(2),
    medianDeviation: median.toFixed(2),
    minDeviation: min.toFixed(2),
    maxDeviation: max.toFixed(2),
    accuracy: ((1 - Math.abs(mean) / targetTimeout) * 100).toFixed(2),
  };
}

/**
 * Run comprehensive circuit breaker benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runComprehensiveCircuitBreakerBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  CIRCUIT BREAKER PERFORMANCE BENCHMARK SUITE  ║');
  console.log('╚════════════════════════════════════════════════╝');

  const results = {
    overhead: await measureCircuitBreakerOverhead(1000),
    stateTransitions: await measureStateTransitions(100),
    failureDetection: await measureFailureDetection(100),
    timeoutAccuracy: await measureTimeoutAccuracy(100),
  };

  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  CIRCUIT BREAKER BENCHMARK SUMMARY            ║');
  console.log('╚════════════════════════════════════════════════╝');
  console.log('\nOverhead:');
  console.log(`  Without CB: ${results.overhead.withoutCB.mean}µs`);
  console.log(`  With CB: ${results.overhead.withCB.mean}µs`);
  console.log(`  Overhead: ${results.overhead.overhead.percentage}%`);
  console.log(`  Acceptable: ${results.overhead.overhead.acceptable ? '✓ YES' : '⚠ NO'}`);
  console.log('\nState Transitions (100 cycles):');
  console.log(`  Mean Per Cycle: ${results.stateTransitions.mean}ms`);
  console.log(`  Success Rate: ${results.stateTransitions.successRate}%`);
  console.log('\nFailure Detection (100 tests):');
  console.log(`  Mean Detection: ${results.failureDetection.mean}ms`);
  console.log(`  Median: ${results.failureDetection.median}ms`);
  console.log('\nTimeout Accuracy (100 tests):');
  console.log(`  Mean Deviation: ${results.timeoutAccuracy.meanDeviation}ms`);
  console.log(`  Accuracy: ${results.timeoutAccuracy.accuracy}%`);

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runComprehensiveCircuitBreakerBenchmarks().catch(console.error);
}
