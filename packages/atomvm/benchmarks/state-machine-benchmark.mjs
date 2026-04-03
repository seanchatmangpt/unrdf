/**
 * State Machine Performance Benchmark Suite
 *
 * Measures performance overhead of state machine operations including
 * state checks, transitions, and validation.
 *
 * @module benchmarks/state-machine-benchmark
 */

import { performance } from 'perf_hooks';

/**
 * Simulated state machine (based on atomvm-runtime pattern)
 */
class TestStateMachine {
  constructor() {
    this.state = 'Uninitialized';
    this.transitionCount = 0;
  }

  isReady() {
    return this.state === 'Ready';
  }

  isLoaded() {
    return this.state === 'Ready' || this.state === 'Executing';
  }

  transitionTo(newState) {
    const validTransitions = {
      'Uninitialized': ['Loading', 'Error'],
      'Loading': ['Ready', 'Error'],
      'Ready': ['Executing', 'Destroyed'],
      'Executing': ['Ready', 'Error'],
      'Error': ['Destroyed'],
    };

    const allowed = validTransitions[this.state];
    if (!allowed || !allowed.includes(newState)) {
      throw new Error(`Invalid transition from ${this.state} to ${newState}`);
    }

    this.state = newState;
    this.transitionCount++;
  }
}

/**
 * Measure state check performance
 * @param {number} iterations - Number of checks to perform
 * @returns {Object} Benchmark results
 */
export async function benchmarkStateChecks(iterations = 10000) {
  console.log(`\n=== State Check Benchmark (${iterations} iterations) ===`);

  const machine = new TestStateMachine();
  machine.transitionTo('Loading');
  machine.transitionTo('Ready');

  const timings = [];
  const startTime = performance.now();

  for (let i = 0; i < iterations; i++) {
    const checkStart = performance.now();
    const ready = machine.isReady();
    const loaded = machine.isLoaded();
    const checkEnd = performance.now();

    timings.push(checkEnd - checkStart);
  }

  const totalTime = performance.now() - startTime;

  // Calculate statistics
  const sortedTimings = [...timings].sort((a, b) => a - b);
  const mean = timings.reduce((a, b) => a + b, 0) / timings.length;
  const median = sortedTimings[Math.floor(sortedTimings.length / 2)];
  const min = sortedTimings[0];
  const max = sortedTimings[sortedTimings.length - 1];

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Mean: ${(mean * 1000).toFixed(3)}µs`);
  console.log(`  Median: ${(median * 1000).toFixed(3)}µs`);
  console.log(`  Min: ${(min * 1000).toFixed(3)}µs`);
  console.log(`  Max: ${(max * 1000).toFixed(3)}µs`);
  console.log(`  Throughput: ${(iterations / (totalTime / 1000)).toFixed(0)} checks/sec`);

  return {
    iterations,
    totalTime: totalTime.toFixed(2),
    mean: (mean * 1000).toFixed(3),
    median: (median * 1000).toFixed(3),
    min: (min * 1000).toFixed(3),
    max: (max * 1000).toFixed(3),
    throughput: (iterations / (totalTime / 1000)).toFixed(0),
  };
}

/**
 * Measure state transition performance
 * @param {number} iterations - Number of transitions to perform
 * @returns {Object} Benchmark results
 */
export async function benchmarkStateTransitions(iterations = 1000) {
  console.log(`\n=== State Transition Benchmark (${iterations} transitions) ===`);

  const timings = [];
  const startTime = performance.now();

  for (let i = 0; i < iterations; i++) {
    const machine = new TestStateMachine();

    const transitionStart = performance.now();

    // Perform valid transition sequence
    machine.transitionTo('Loading');
    machine.transitionTo('Ready');
    machine.transitionTo('Executing');
    machine.transitionTo('Ready');

    const transitionEnd = performance.now();
    timings.push(transitionEnd - transitionStart);
  }

  const totalTime = performance.now() - startTime;

  // Calculate statistics
  const sortedTimings = [...timings].sort((a, b) => a - b);
  const mean = timings.reduce((a, b) => a + b, 0) / timings.length;
  const median = sortedTimings[Math.floor(sortedTimings.length / 2)];
  const min = sortedTimings[0];
  const max = sortedTimings[sortedTimings.length - 1];

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Mean (4 transitions): ${(mean * 1000).toFixed(3)}µs`);
  console.log(`  Mean (per transition): ${(mean * 1000 / 4).toFixed(3)}µs`);
  console.log(`  Median: ${(median * 1000).toFixed(3)}µs`);
  console.log(`  Min: ${(min * 1000).toFixed(3)}µs`);
  console.log(`  Max: ${(max * 1000).toFixed(3)}µs`);
  console.log(`  Throughput: ${((iterations * 4) / (totalTime / 1000)).toFixed(0)} transitions/sec`);

  return {
    iterations,
    totalTime: totalTime.toFixed(2),
    mean: (mean * 1000).toFixed(3),
    meanPerTransition: (mean * 1000 / 4).toFixed(3),
    median: (median * 1000).toFixed(3),
    min: (min * 1000).toFixed(3),
    max: (max * 1000).toFixed(3),
    throughput: ((iterations * 4) / (totalTime / 1000)).toFixed(0),
  };
}

/**
 * Test for performance degradation with many transitions
 * @param {number} iterations - Number of transition cycles
 * @returns {Object} Benchmark results
 */
export async function benchmarkStateDegradation(iterations = 10000) {
  console.log(`\n=== State Degradation Test (${iterations} cycles) ===`);

  const machine = new TestStateMachine();
  const samples = [];
  const sampleInterval = Math.floor(iterations / 100);

  const startTime = performance.now();

  for (let i = 0; i < iterations; i++) {
    const cycleStart = performance.now();

    // Reset to initial state
    machine.state = 'Uninitialized';

    // Perform transition sequence
    machine.transitionTo('Loading');
    machine.transitionTo('Ready');
    machine.transitionTo('Executing');
    machine.transitionTo('Ready');

    const cycleEnd = performance.now();

    // Sample at intervals
    if (i % sampleInterval === 0) {
      samples.push({
        iteration: i,
        time: cycleEnd - cycleStart,
      });
    }
  }

  const totalTime = performance.now() - startTime;

  // Analyze for degradation (compare first 10% vs last 10%)
  const firstSamples = samples.slice(0, Math.floor(samples.length * 0.1));
  const lastSamples = samples.slice(-Math.floor(samples.length * 0.1));

  const firstAvg = firstSamples.reduce((sum, s) => sum + s.time, 0) / firstSamples.length;
  const lastAvg = lastSamples.reduce((sum, s) => sum + s.time, 0) / lastSamples.length;
  const degradation = ((lastAvg - firstAvg) / firstAvg * 100).toFixed(2);

  console.log(`Results:`);
  console.log(`  Total Transitions: ${machine.transitionCount}`);
  console.log(`  First 10% Avg: ${(firstAvg * 1000).toFixed(3)}µs`);
  console.log(`  Last 10% Avg: ${(lastAvg * 1000).toFixed(3)}µs`);
  console.log(`  Performance Change: ${degradation}%`);
  console.log(`  Degradation Detected: ${Math.abs(parseFloat(degradation)) > 5 ? '⚠ YES' : '✓ NO'}`);

  return {
    iterations,
    totalTime: totalTime.toFixed(2),
    totalTransitions: machine.transitionCount,
    firstAvg: (firstAvg * 1000).toFixed(3),
    lastAvg: (lastAvg * 1000).toFixed(3),
    degradation,
    degradationDetected: Math.abs(parseFloat(degradation)) > 5,
  };
}

/**
 * Run comprehensive state machine benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runComprehensiveStateMachineBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  STATE MACHINE PERFORMANCE BENCHMARK SUITE    ║');
  console.log('╚════════════════════════════════════════════════╝');

  const results = {
    stateChecks: await benchmarkStateChecks(10000),
    transitions: await benchmarkStateTransitions(1000),
    degradation: await benchmarkStateDegradation(10000),
  };

  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  STATE MACHINE BENCHMARK SUMMARY              ║');
  console.log('╚════════════════════════════════════════════════╝');
  console.log('\nState Checks (10,000 iterations):');
  console.log(`  Mean: ${results.stateChecks.mean}µs`);
  console.log(`  Throughput: ${results.stateChecks.throughput} checks/sec`);
  console.log('\nState Transitions (1,000 iterations):');
  console.log(`  Mean Per Transition: ${results.transitions.meanPerTransition}µs`);
  console.log(`  Throughput: ${results.transitions.throughput} transitions/sec`);
  console.log('\nDegradation Test (10,000 cycles):');
  console.log(`  Performance Change: ${results.degradation.degradation}%`);
  console.log(`  Degradation: ${results.degradation.degradationDetected ? '⚠ YES' : '✓ NO'}`);

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runComprehensiveStateMachineBenchmarks().catch(console.error);
}
