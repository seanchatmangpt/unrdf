/**
 * @file Soak test - 100 users for 2 hours
 * @module test/load-testing/05-soak-test
 * @description Long-running test to detect memory leaks and degradation
 */

import autocannon from 'autocannon';
import { exec } from 'node:child_process';
import { promisify } from 'node:util';

const execAsync = promisify(exec);

/**
 * Run soak test
 * @param {string} url - Target URL
 * @param {number} [duration=7200] - Test duration in seconds (default: 2 hours)
 * @returns {Promise<Object>} Test results
 */
export async function runSoakTest(url = 'http://localhost:3000', duration = 7200) {
  console.log('\n=== SOAK TEST ===');
  console.log(`Target: 100 concurrent users for ${duration / 3600} hours`);
  console.log('Purpose: Detect memory leaks, resource exhaustion, degradation\n');

  const instance = autocannon({
    url: `${url}/api/receipt`,
    connections: 100,      // 100 concurrent connections
    duration,              // 2 hours (7200 seconds) by default
    pipelining: 1,

    headers: {
      'Content-Type': 'application/json'
    },

    // Mix of different endpoints
    requests: [
      { method: 'GET', path: '/api/receipt' },
      { method: 'GET', path: '/api/triples' },
      { method: 'GET', path: '/api/query?q=SELECT%20*%20WHERE%20%7B%20%3Fs%20%3Fp%20%3Fo%20%7D' },
      { method: 'GET', path: '/health' }
    ]
  });

  // Track progress
  autocannon.track(instance, {
    renderProgressBar: true,
    renderResultsTable: false // Too verbose for long tests
  });

  // Collect detailed metrics during test
  const metrics = {
    memory: [],
    cpu: [],
    latency: [],
    errors: []
  };

  const startTime = Date.now();

  // Sample system metrics every minute
  const metricsInterval = setInterval(async () => {
    const elapsed = (Date.now() - startTime) / 1000;

    // Memory usage
    const memUsage = process.memoryUsage();
    metrics.memory.push({
      timestamp: Date.now(),
      elapsed,
      rss: memUsage.rss,
      heapUsed: memUsage.heapUsed,
      heapTotal: memUsage.heapTotal,
      external: memUsage.external
    });

    // CPU usage (if available)
    try {
      const { stdout } = await execAsync('ps -p $$ -o %cpu | tail -1');
      const cpuPercent = parseFloat(stdout.trim());
      metrics.cpu.push({
        timestamp: Date.now(),
        elapsed,
        percent: cpuPercent
      });
    } catch (err) {
      // CPU monitoring not available
    }

    // Progress update
    const progress = (elapsed / duration) * 100;
    console.log(`Progress: ${progress.toFixed(1)}% | Memory: ${(memUsage.heapUsed / 1024 / 1024).toFixed(2)} MB`);
  }, 60000); // Every minute

  return new Promise((resolve, reject) => {
    instance.on('done', (results) => {
      clearInterval(metricsInterval);
      const analysis = analyzeSoakResults(results, metrics);
      resolve(analysis);
    });

    instance.on('error', (err) => {
      clearInterval(metricsInterval);
      reject(err);
    });
  });
}

/**
 * Analyze soak test results
 * @param {Object} results - Autocannon results
 * @param {Object} metrics - Collected metrics
 * @returns {Object} Analyzed results
 */
function analyzeSoakResults(results, metrics) {
  const latency = results.latency;
  const throughput = results.requests;
  const errors = results.errors;

  // Analyze memory trend
  const memoryAnalysis = analyzeMemoryTrend(metrics.memory);

  // Analyze CPU trend
  const cpuAnalysis = analyzeCPUTrend(metrics.cpu);

  // Check for performance degradation over time
  const degradation = analyzePerformanceDegradation(metrics);

  return {
    testType: 'soak-test',
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

    // Resource usage
    resources: {
      memory: memoryAnalysis,
      cpu: cpuAnalysis
    },

    // Performance degradation
    degradation,

    // Connection metrics
    connections: results.connections,

    // Pass/fail criteria
    passed: {
      latencyP95: latency.p95 < 100, // Target: <100ms
      latencyP99: latency.p99 < 500, // Target: <500ms
      errorRate: (errors / throughput.total) < 0.001, // Target: <0.1%
      memoryStable: memoryAnalysis.leakDetected === false,
      cpuStable: cpuAnalysis.average < 80, // <80% CPU
      noDegradation: degradation.detected === false
    }
  };
}

/**
 * Analyze memory usage trend
 * @param {Array} memorySnapshots - Memory snapshots
 * @returns {Object} Memory analysis
 */
function analyzeMemoryTrend(memorySnapshots) {
  if (memorySnapshots.length < 2) {
    return {
      samples: memorySnapshots.length,
      leakDetected: false
    };
  }

  const first = memorySnapshots[0];
  const last = memorySnapshots[memorySnapshots.length - 1];
  const duration = (last.timestamp - first.timestamp) / 1000 / 60; // minutes

  const heapGrowth = last.heapUsed - first.heapUsed;
  const growthRate = heapGrowth / duration; // bytes per minute

  // Linear regression to detect trend
  const heapTrend = calculateTrend(memorySnapshots.map(s => s.heapUsed));

  return {
    samples: memorySnapshots.length,
    initialHeap: first.heapUsed,
    finalHeap: last.heapUsed,
    heapGrowth,
    growthRate,
    growthRateMB: growthRate / (1024 * 1024),
    trend: heapTrend,
    leakDetected: growthRate > 1024 * 1024 || heapTrend > 0.1, // >1MB/min or positive trend
    peakMemory: Math.max(...memorySnapshots.map(s => s.heapUsed)),
    averageMemory: memorySnapshots.reduce((sum, s) => sum + s.heapUsed, 0) / memorySnapshots.length
  };
}

/**
 * Analyze CPU usage trend
 * @param {Array} cpuSnapshots - CPU snapshots
 * @returns {Object} CPU analysis
 */
function analyzeCPUTrend(cpuSnapshots) {
  if (cpuSnapshots.length < 2) {
    return {
      samples: cpuSnapshots.length,
      average: 0
    };
  }

  const cpuValues = cpuSnapshots.map(s => s.percent);
  const average = cpuValues.reduce((sum, v) => sum + v, 0) / cpuValues.length;
  const peak = Math.max(...cpuValues);
  const trend = calculateTrend(cpuValues);

  return {
    samples: cpuSnapshots.length,
    average,
    peak,
    trend,
    stable: trend < 0.1 // CPU not increasing over time
  };
}

/**
 * Analyze performance degradation
 * @param {Object} metrics - All collected metrics
 * @returns {Object} Degradation analysis
 */
function analyzePerformanceDegradation(metrics) {
  if (metrics.memory.length < 10) {
    return { detected: false, reason: 'Insufficient samples' };
  }

  // Compare first 10% vs last 10% of samples
  const sampleCount = metrics.memory.length;
  const firstTenth = Math.floor(sampleCount * 0.1);
  const lastTenth = Math.floor(sampleCount * 0.9);

  const firstSamples = metrics.memory.slice(0, firstTenth);
  const lastSamples = metrics.memory.slice(lastTenth);

  const firstAvgMemory = firstSamples.reduce((sum, s) => sum + s.heapUsed, 0) / firstSamples.length;
  const lastAvgMemory = lastSamples.reduce((sum, s) => sum + s.heapUsed, 0) / lastSamples.length;

  const memoryIncrease = ((lastAvgMemory - firstAvgMemory) / firstAvgMemory) * 100;

  return {
    detected: memoryIncrease > 20, // >20% increase is degradation
    memoryIncrease: `${memoryIncrease.toFixed(2)}%`,
    firstPeriodAvg: firstAvgMemory,
    lastPeriodAvg: lastAvgMemory
  };
}

/**
 * Calculate linear regression trend
 * @param {Array<number>} values - Data points
 * @returns {number} Slope (trend)
 */
function calculateTrend(values) {
  const n = values.length;
  const x = Array.from({ length: n }, (_, i) => i);
  const y = values;

  const sumX = x.reduce((a, b) => a + b, 0);
  const sumY = y.reduce((a, b) => a + b, 0);
  const sumXY = x.reduce((sum, xi, i) => sum + xi * y[i], 0);
  const sumXX = x.reduce((sum, xi) => sum + xi * xi, 0);

  const slope = (n * sumXY - sumX * sumY) / (n * sumXX - sumX * sumX);
  return slope;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const url = process.argv[2] || 'http://localhost:3000';
  const duration = parseInt(process.argv[3] || '7200', 10); // Default 2 hours

  runSoakTest(url, duration)
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
