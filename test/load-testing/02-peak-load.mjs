/**
 * @file Peak load test - 1000 concurrent users, 10000 req/min
 * @module test/load-testing/02-peak-load
 * @description Tests system behavior under peak load conditions
 */

import autocannon from 'autocannon';

/**
 * Run peak load test
 * @param {string} url - Target URL
 * @returns {Promise<Object>} Test results
 */
export async function runPeakLoad(url = 'http://localhost:3000') {
  console.log('\n=== PEAK LOAD TEST ===');
  console.log('Target: 1000 concurrent users, ~10000 req/min');
  console.log('Duration: 60 seconds\n');

  const instance = autocannon({
    url: `${url}/api/query`,
    connections: 1000,     // 1000 concurrent connections
    duration: 60,          // 60 seconds
    pipelining: 1,         // 1 request per connection at a time

    // Headers
    headers: {
      'Content-Type': 'application/json'
    },

    // 10000 req/min = ~166.67 req/sec
    // With 1000 connections, each sends ~0.167 req/sec
    requests: [
      {
        method: 'GET',
        path: '/api/query?q=SELECT%20*%20WHERE%20%7B%20%3Fs%20%3Fp%20%3Fo%20%7D'
      }
    ]
  });

  // Track progress
  autocannon.track(instance, {
    renderProgressBar: true,
    renderResultsTable: true
  });

  return new Promise((resolve, reject) => {
    instance.on('done', (results) => {
      const analysis = analyzeResults(results, 'peak-load');
      resolve(analysis);
    });

    instance.on('error', (err) => {
      reject(err);
    });
  });
}

/**
 * Analyze test results
 * @param {Object} results - Autocannon results
 * @param {string} testType - Test type name
 * @returns {Object} Analyzed results
 */
function analyzeResults(results, testType) {
  const latency = results.latency;
  const throughput = results.requests;
  const errors = results.errors;

  return {
    testType,
    timestamp: new Date().toISOString(),
    duration: results.duration,

    // Latency metrics (in ms)
    latency: {
      mean: latency.mean,
      p50: latency.p50,
      p75: latency.p75,
      p90: latency.p90,
      p95: latency.p95,
      p99: latency.p99,
      p999: latency.p999,
      max: latency.max,
      min: latency.min,
      stddev: latency.stddev
    },

    // Throughput metrics
    throughput: {
      totalRequests: throughput.total,
      requestsPerSecond: throughput.average,
      min: throughput.min,
      max: throughput.max,
      mean: throughput.mean,
      stddev: throughput.stddev
    },

    // Error metrics
    errors: {
      total: errors,
      errorRate: (errors / throughput.total) * 100
    },

    // Connection metrics
    connections: results.connections,

    // Status code distribution
    statusCodes: results['2xx'] || results.statusCodeStats,

    // Pass/fail criteria (more relaxed for peak load)
    passed: {
      latencyP95: latency.p95 < 200, // Target: <200ms under peak
      latencyP99: latency.p99 < 1000, // Target: <1s under peak
      errorRate: (errors / throughput.total) < 0.001, // Target: <0.1%
      throughput: throughput.average > 100 // Target: >100 req/s
    }
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const url = process.argv[2] || 'http://localhost:3000';

  runPeakLoad(url)
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
