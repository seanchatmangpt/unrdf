/**
 * @file Baseline load test - 100 concurrent users, 1000 req/min
 * @module test/load-testing/01-baseline-load
 * @description Tests normal operating conditions with moderate load
 */

import autocannon from 'autocannon';

/**
 * Run baseline load test
 * @param {string} url - Target URL
 * @returns {Promise<Object>} Test results
 */
export async function runBaselineLoad(url = 'http://localhost:3000') {
  console.log('\n=== BASELINE LOAD TEST ===');
  console.log('Target: 100 concurrent users, ~1000 req/min');
  console.log('Duration: 60 seconds\n');

  const instance = autocannon({
    url: `${url}/api/receipt`,
    connections: 100,      // 100 concurrent connections
    duration: 60,          // 60 seconds
    pipelining: 1,         // 1 request per connection at a time

    // Headers
    headers: {
      'Content-Type': 'application/json'
    },

    // Expected ~1000 req/min = ~16.67 req/sec
    // With 100 connections, each should send ~0.167 req/sec
    // This is controlled by the rate limiter
    requests: [
      {
        method: 'GET',
        path: '/api/receipt'
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
      const analysis = analyzeResults(results, 'baseline');
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

    // Pass/fail criteria
    passed: {
      latencyP95: latency.p95 < 100, // Target: <100ms
      latencyP99: latency.p99 < 500, // Target: <500ms
      errorRate: (errors / throughput.total) < 0.001, // Target: <0.1%
      throughput: throughput.average > 10 // Target: >10 req/s
    }
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const url = process.argv[2] || 'http://localhost:3000';

  runBaselineLoad(url)
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
