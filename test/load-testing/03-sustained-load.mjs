/**
 * @file Sustained load test - 500 concurrent users for 30 minutes
 * @module test/load-testing/03-sustained-load
 * @description Tests system stability under sustained moderate load
 */

import autocannon from 'autocannon';

/**
 * Run sustained load test
 * @param {string} url - Target URL
 * @param {number} [duration=1800] - Test duration in seconds (default: 30 min)
 * @returns {Promise<Object>} Test results
 */
export async function runSustainedLoad(url = 'http://localhost:3000', duration = 1800) {
  console.log('\n=== SUSTAINED LOAD TEST ===');
  console.log(`Target: 500 concurrent users for ${duration / 60} minutes`);
  console.log('Purpose: Detect memory leaks and performance degradation\n');

  const instance = autocannon({
    url: `${url}/api/triples`,
    connections: 500,      // 500 concurrent connections
    duration,              // 30 minutes (1800 seconds) by default
    pipelining: 1,         // 1 request per connection at a time

    // Headers
    headers: {
      'Content-Type': 'application/json'
    },

    requests: [
      {
        method: 'GET',
        path: '/api/triples'
      },
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

  // Sample memory usage during test
  const memorySnapshots = [];
  const memoryInterval = setInterval(() => {
    const usage = process.memoryUsage();
    memorySnapshots.push({
      timestamp: Date.now(),
      rss: usage.rss,
      heapUsed: usage.heapUsed,
      heapTotal: usage.heapTotal,
      external: usage.external
    });
  }, 60000); // Every minute

  return new Promise((resolve, reject) => {
    instance.on('done', (results) => {
      clearInterval(memoryInterval);
      const analysis = analyzeResults(results, 'sustained-load', memorySnapshots);
      resolve(analysis);
    });

    instance.on('error', (err) => {
      clearInterval(memoryInterval);
      reject(err);
    });
  });
}

/**
 * Analyze test results
 * @param {Object} results - Autocannon results
 * @param {string} testType - Test type name
 * @param {Array} memorySnapshots - Memory usage snapshots
 * @returns {Object} Analyzed results
 */
function analyzeResults(results, testType, memorySnapshots) {
  const latency = results.latency;
  const throughput = results.requests;
  const errors = results.errors;

  // Analyze memory trend
  const memoryTrend = analyzeMemoryTrend(memorySnapshots);

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

    // Memory metrics
    memory: memoryTrend,

    // Connection metrics
    connections: results.connections,

    // Status code distribution
    statusCodes: results['2xx'] || results.statusCodeStats,

    // Pass/fail criteria
    passed: {
      latencyP95: latency.p95 < 150, // Target: <150ms
      latencyP99: latency.p99 < 750, // Target: <750ms
      errorRate: (errors / throughput.total) < 0.001, // Target: <0.1%
      throughput: throughput.average > 50, // Target: >50 req/s
      memoryGrowth: memoryTrend.growthRate < 1024 * 1024 // <1MB/min growth
    }
  };
}

/**
 * Analyze memory usage trend
 * @param {Array} snapshots - Memory snapshots
 * @returns {Object} Memory trend analysis
 */
function analyzeMemoryTrend(snapshots) {
  if (snapshots.length < 2) {
    return {
      samples: snapshots.length,
      growthRate: 0,
      memoryLeak: false
    };
  }

  const first = snapshots[0];
  const last = snapshots[snapshots.length - 1];
  const duration = (last.timestamp - first.timestamp) / 1000; // seconds

  const heapGrowth = last.heapUsed - first.heapUsed;
  const growthRate = heapGrowth / (duration / 60); // bytes per minute

  return {
    samples: snapshots.length,
    initialHeap: first.heapUsed,
    finalHeap: last.heapUsed,
    heapGrowth,
    growthRate,
    growthRateMB: growthRate / (1024 * 1024),
    memoryLeak: growthRate > 1024 * 1024, // >1MB/min is suspicious
    snapshots
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const url = process.argv[2] || 'http://localhost:3000';
  const duration = parseInt(process.argv[3] || '1800', 10); // Default 30 min

  runSustainedLoad(url, duration)
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
