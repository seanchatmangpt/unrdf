/**
 * @file Spike test - 0 to 5000 users in 10 seconds
 * @module test/load-testing/04-spike-test
 * @description Tests system behavior under sudden traffic spike
 */

import autocannon from 'autocannon';

/**
 * Run spike test
 * @param {string} url - Target URL
 * @returns {Promise<Object>} Test results
 */
export async function runSpikeTest(url = 'http://localhost:3000') {
  console.log('\n=== SPIKE TEST ===');
  console.log('Target: 0 → 5000 users in 10 seconds');
  console.log('Purpose: Test auto-scaling and resilience\n');

  // Phase 1: Warm-up (5 seconds, 10 connections)
  console.log('Phase 1: Warm-up (5s, 10 connections)');
  const warmup = await runPhase(url, 10, 5);

  // Phase 2: Spike (10 seconds, 5000 connections)
  console.log('\nPhase 2: Spike (10s, 5000 connections)');
  const spike = await runPhase(url, 5000, 10);

  // Phase 3: Recovery (15 seconds, 100 connections)
  console.log('\nPhase 3: Recovery (15s, 100 connections)');
  const recovery = await runPhase(url, 100, 15);

  return analyzeSpikeResults({ warmup, spike, recovery });
}

/**
 * Run a single phase of the spike test
 * @param {string} url - Target URL
 * @param {number} connections - Number of connections
 * @param {number} duration - Duration in seconds
 * @returns {Promise<Object>} Phase results
 */
async function runPhase(url, connections, duration) {
  const instance = autocannon({
    url: `${url}/api/receipt`,
    connections,
    duration,
    pipelining: 1,
    headers: {
      'Content-Type': 'application/json'
    }
  });

  return new Promise((resolve, reject) => {
    instance.on('done', (results) => {
      resolve({
        connections,
        duration,
        latency: results.latency,
        throughput: results.requests,
        errors: results.errors
      });
    });

    instance.on('error', (err) => {
      reject(err);
    });
  });
}

/**
 * Analyze spike test results
 * @param {Object} phases - Results from all phases
 * @returns {Object} Analyzed results
 */
function analyzeSpikeResults(phases) {
  const { warmup, spike, recovery } = phases;

  // Calculate degradation during spike
  const spikeLatencyIncrease = (spike.latency.p95 - warmup.latency.p95) / warmup.latency.p95;
  const spikeErrorIncrease = spike.errors - warmup.errors;

  // Calculate recovery quality
  const recoveryLatency = recovery.latency.p95;
  const recoveredToBaseline = recoveryLatency < warmup.latency.p95 * 1.2; // Within 20% of baseline

  return {
    testType: 'spike-test',
    timestamp: new Date().toISOString(),

    phases: {
      warmup: {
        connections: warmup.connections,
        duration: warmup.duration,
        latencyP95: warmup.latency.p95,
        latencyP99: warmup.latency.p99,
        throughput: warmup.throughput.average,
        errors: warmup.errors
      },
      spike: {
        connections: spike.connections,
        duration: spike.duration,
        latencyP95: spike.latency.p95,
        latencyP99: spike.latency.p99,
        throughput: spike.throughput.average,
        errors: spike.errors
      },
      recovery: {
        connections: recovery.connections,
        duration: recovery.duration,
        latencyP95: recovery.latency.p95,
        latencyP99: recovery.latency.p99,
        throughput: recovery.throughput.average,
        errors: recovery.errors
      }
    },

    analysis: {
      spikeLatencyIncrease: `${(spikeLatencyIncrease * 100).toFixed(2)}%`,
      spikeErrorIncrease,
      recoveredToBaseline,
      maxConcurrentHandled: spike.connections
    },

    // Pass/fail criteria
    passed: {
      spikeHandled: spike.errors / spike.throughput.total < 0.01, // <1% errors during spike
      latencyDegradation: spikeLatencyIncrease < 5, // <500% degradation
      recovery: recoveredToBaseline,
      errorRate: spike.errors / spike.throughput.total < 0.01
    }
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const url = process.argv[2] || 'http://localhost:3000';

  runSpikeTest(url)
    .then((results) => {
      console.log('\n=== RESULTS ===');
      console.log(JSON.stringify(results, null, 2));

      const allPassed = Object.values(results.passed).every(p => p);
      process.exit(allPassed ? 0 : 1);
    })
    .catch((err) => {
      console.error('Test failed:', err);
      process.exit(1);
    });
}
