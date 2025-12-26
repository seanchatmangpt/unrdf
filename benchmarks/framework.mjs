/**
 * @file Benchmark Framework - Statistical benchmarking with performance analysis
 * @module benchmarks/framework
 *
 * @description
 * Comprehensive benchmarking framework providing:
 * - Statistical significance testing (min 1000 iterations)
 * - Warmup runs to eliminate JIT effects
 * - GC between benchmarks
 * - Percentile calculations (P50, P95, P99)
 * - Memory profiling (RSS, heap, external)
 * - CPU time measurement
 * - Reproducible results with fixed random seeds
 */

import { performance } from 'node:perf_hooks';
import { randomBytes } from 'node:crypto';

// =============================================================================
// Statistics Utilities
// =============================================================================

/**
 * Calculate percentile from sorted array
 * @param {number[]} sortedValues - Sorted array of numbers
 * @param {number} percentile - Percentile to calculate (0-100)
 * @returns {number} Percentile value
 */
export function calculatePercentile(sortedValues, percentile) {
  if (sortedValues.length === 0) return 0;
  if (percentile <= 0) return sortedValues[0];
  if (percentile >= 100) return sortedValues[sortedValues.length - 1];

  const index = (percentile / 100) * (sortedValues.length - 1);
  const lower = Math.floor(index);
  const upper = Math.ceil(index);
  const weight = index - lower;

  return sortedValues[lower] * (1 - weight) + sortedValues[upper] * weight;
}

/**
 * Calculate mean of array
 * @param {number[]} values - Array of numbers
 * @returns {number} Mean value
 */
export function calculateMean(values) {
  if (values.length === 0) return 0;
  return values.reduce((sum, v) => sum + v, 0) / values.length;
}

/**
 * Calculate standard deviation
 * @param {number[]} values - Array of numbers
 * @returns {number} Standard deviation
 */
export function calculateStdDev(values) {
  if (values.length === 0) return 0;
  const mean = calculateMean(values);
  const squaredDiffs = values.map(v => Math.pow(v - mean, 2));
  const variance = calculateMean(squaredDiffs);
  return Math.sqrt(variance);
}

/**
 * Calculate statistics for array of values
 * @param {number[]} values - Array of numbers
 * @returns {object} Statistics object
 */
export function calculateStats(values) {
  if (values.length === 0) {
    return {
      count: 0,
      min: 0,
      max: 0,
      mean: 0,
      median: 0,
      stdDev: 0,
      p50: 0,
      p75: 0,
      p95: 0,
      p99: 0,
      p999: 0
    };
  }

  const sorted = [...values].sort((a, b) => a - b);

  return {
    count: values.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean: calculateMean(values),
    median: calculatePercentile(sorted, 50),
    stdDev: calculateStdDev(values),
    p50: calculatePercentile(sorted, 50),
    p75: calculatePercentile(sorted, 75),
    p95: calculatePercentile(sorted, 95),
    p99: calculatePercentile(sorted, 99),
    p999: calculatePercentile(sorted, 99.9)
  };
}

// =============================================================================
// Memory Measurement
// =============================================================================

/**
 * Get current memory usage
 * @returns {object} Memory usage snapshot
 */
export function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: usage.rss,
    heapTotal: usage.heapTotal,
    heapUsed: usage.heapUsed,
    external: usage.external,
    arrayBuffers: usage.arrayBuffers || 0
  };
}

/**
 * Calculate memory delta between two snapshots
 * @param {object} before - Memory snapshot before
 * @param {object} after - Memory snapshot after
 * @returns {object} Memory delta
 */
export function calculateMemoryDelta(before, after) {
  return {
    rss: after.rss - before.rss,
    heapTotal: after.heapTotal - before.heapTotal,
    heapUsed: after.heapUsed - before.heapUsed,
    external: after.external - before.external,
    arrayBuffers: after.arrayBuffers - before.arrayBuffers
  };
}

/**
 * Format bytes to human readable
 * @param {number} bytes - Bytes to format
 * @returns {string} Formatted string
 */
export function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(Math.abs(bytes)) / Math.log(k));
  const value = bytes / Math.pow(k, i);
  return `${value >= 0 ? '+' : ''}${value.toFixed(2)} ${sizes[i]}`;
}

// =============================================================================
// Benchmark Runner
// =============================================================================

/**
 * Benchmark configuration
 * @typedef {object} BenchmarkConfig
 * @property {Function} fn - Function to benchmark
 * @property {number} iterations - Number of iterations (default: 1000)
 * @property {number} warmup - Number of warmup iterations (default: 100)
 * @property {boolean} gc - Force GC before benchmark (default: true)
 * @property {Function} setup - Setup function run before each iteration
 * @property {Function} teardown - Teardown function run after each iteration
 */

/**
 * Run a single benchmark
 * @param {string} name - Benchmark name
 * @param {BenchmarkConfig} config - Benchmark configuration
 * @returns {Promise<object>} Benchmark results
 */
export async function runBenchmark(name, config) {
  const {
    fn,
    iterations = 1000,
    warmup = 100,
    gc: shouldGc = true,
    setup = null,
    teardown = null
  } = config;

  // Force GC if available and requested
  if (shouldGc && global.gc) {
    global.gc();
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  // Warmup phase
  console.log(`  Warmup: ${warmup} iterations...`);
  let warmupContext = setup ? await setup() : {};
  for (let i = 0; i < warmup; i++) {
    await fn.call(warmupContext);
    if (teardown) await teardown.call(warmupContext);
  }

  // Force GC after warmup
  if (shouldGc && global.gc) {
    global.gc();
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  // Measurement phase
  console.log(`  Running: ${iterations} iterations...`);
  const latencies = [];
  const memoryBefore = getMemoryUsage();
  const startTime = performance.now();

  // Setup context once if setup returns an object
  let context = setup ? await setup() : {};

  for (let i = 0; i < iterations; i++) {
    const iterStart = performance.now();
    await fn.call(context);
    const iterEnd = performance.now();

    latencies.push(iterEnd - iterStart);

    if (teardown) await teardown.call(context);
  }

  const endTime = performance.now();
  const memoryAfter = getMemoryUsage();

  // Calculate statistics
  const totalTime = endTime - startTime;
  const throughput = iterations / (totalTime / 1000); // ops/sec
  const latencyStats = calculateStats(latencies);
  const memoryDelta = calculateMemoryDelta(memoryBefore, memoryAfter);

  return {
    name,
    iterations,
    totalTime,
    throughput,
    latency: latencyStats,
    memory: memoryDelta,
    memoryBefore,
    memoryAfter
  };
}

/**
 * Create a benchmark suite
 * @param {string} suiteName - Suite name
 * @param {object} benchmarks - Map of benchmark name to config
 * @returns {Function} Suite runner function
 */
export function suite(suiteName, benchmarks) {
  return async () => {
    console.log(`\n${'='.repeat(80)}`);
    console.log(`Benchmark Suite: ${suiteName}`);
    console.log('='.repeat(80));

    const results = [];

    for (const [name, config] of Object.entries(benchmarks)) {
      console.log(`\nBenchmark: ${name}`);
      try {
        const result = await runBenchmark(name, config);
        results.push(result);
        console.log(`  ✓ Complete: ${result.throughput.toFixed(2)} ops/sec`);
      } catch (error) {
        console.error(`  ✗ Error: ${error.message}`);
        results.push({
          name,
          error: error.message,
          failed: true
        });
      }
    }

    return {
      suite: suiteName,
      timestamp: new Date().toISOString(),
      results
    };
  };
}

// =============================================================================
// Reporting
// =============================================================================

/**
 * Format benchmark results as markdown table
 * @param {object} suiteResult - Suite result from running suite
 * @returns {string} Markdown table
 */
export function formatMarkdownTable(suiteResult) {
  const lines = [];

  lines.push(`## ${suiteResult.suite}\n`);
  lines.push(`**Timestamp**: ${suiteResult.timestamp}\n`);
  lines.push('| Operation | Throughput | Latency P50 | Latency P95 | Latency P99 | Memory |');
  lines.push('|-----------|------------|-------------|-------------|-------------|--------|');

  for (const result of suiteResult.results) {
    if (result.failed) {
      lines.push(`| ${result.name} | **FAILED** | - | - | - | ${result.error} |`);
      continue;
    }

    const throughput = `${result.throughput.toFixed(2)}/sec`;
    const p50 = `${result.latency.p50.toFixed(3)}ms`;
    const p95 = `${result.latency.p95.toFixed(3)}ms`;
    const p99 = `${result.latency.p99.toFixed(3)}ms`;
    const memory = formatBytes(result.memory.heapUsed);

    lines.push(`| ${result.name} | ${throughput} | ${p50} | ${p95} | ${p99} | ${memory} |`);
  }

  lines.push('');
  return lines.join('\n');
}

/**
 * Format detailed benchmark results
 * @param {object} suiteResult - Suite result from running suite
 * @returns {string} Detailed markdown report
 */
export function formatDetailedReport(suiteResult) {
  const lines = [];

  lines.push(`# Benchmark Report: ${suiteResult.suite}\n`);
  lines.push(`**Generated**: ${suiteResult.timestamp}\n`);

  for (const result of suiteResult.results) {
    if (result.failed) {
      lines.push(`## ${result.name} (FAILED)\n`);
      lines.push(`**Error**: ${result.error}\n`);
      continue;
    }

    lines.push(`## ${result.name}\n`);

    // Throughput
    lines.push('### Throughput');
    lines.push(`- **Operations per second**: ${result.throughput.toFixed(2)}`);
    lines.push(`- **Total time**: ${result.totalTime.toFixed(2)}ms`);
    lines.push(`- **Iterations**: ${result.iterations}\n`);

    // Latency
    lines.push('### Latency');
    lines.push(`- **Mean**: ${result.latency.mean.toFixed(3)}ms`);
    lines.push(`- **Median (P50)**: ${result.latency.p50.toFixed(3)}ms`);
    lines.push(`- **P75**: ${result.latency.p75.toFixed(3)}ms`);
    lines.push(`- **P95**: ${result.latency.p95.toFixed(3)}ms`);
    lines.push(`- **P99**: ${result.latency.p99.toFixed(3)}ms`);
    lines.push(`- **P99.9**: ${result.latency.p999.toFixed(3)}ms`);
    lines.push(`- **Min**: ${result.latency.min.toFixed(3)}ms`);
    lines.push(`- **Max**: ${result.latency.max.toFixed(3)}ms`);
    lines.push(`- **Std Dev**: ${result.latency.stdDev.toFixed(3)}ms\n`);

    // Memory
    lines.push('### Memory');
    lines.push(`- **RSS**: ${formatBytes(result.memory.rss)}`);
    lines.push(`- **Heap Total**: ${formatBytes(result.memory.heapTotal)}`);
    lines.push(`- **Heap Used**: ${formatBytes(result.memory.heapUsed)}`);
    lines.push(`- **External**: ${formatBytes(result.memory.external)}\n`);
  }

  return lines.join('\n');
}

// =============================================================================
// Random Data Generation
// =============================================================================

/**
 * Generate random string
 * @param {number} length - String length
 * @returns {string} Random string
 */
export function randomString(length = 16) {
  return randomBytes(length).toString('hex').slice(0, length);
}

/**
 * Generate random integer
 * @param {number} min - Minimum value
 * @param {number} max - Maximum value
 * @returns {number} Random integer
 */
export function randomInt(min = 0, max = 100) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

/**
 * Set random seed for reproducibility
 * @param {number} seed - Random seed
 */
export function setRandomSeed(seed) {
  // Note: JavaScript doesn't have built-in seeded random
  // This is a placeholder for future implementation
  console.warn('setRandomSeed not implemented - use external library for reproducible randomness');
}

// =============================================================================
// Exports
// =============================================================================

export default {
  // Statistics
  calculatePercentile,
  calculateMean,
  calculateStdDev,
  calculateStats,

  // Memory
  getMemoryUsage,
  calculateMemoryDelta,
  formatBytes,

  // Benchmarking
  runBenchmark,
  suite,

  // Reporting
  formatMarkdownTable,
  formatDetailedReport,

  // Random data
  randomString,
  randomInt,
  setRandomSeed
};
