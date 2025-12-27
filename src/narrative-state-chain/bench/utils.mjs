/**
 * @file Benchmark Utilities - Timing, percentile calculations, and reporting
 * @module narrative-state-chain/bench/utils
 */

/**
 * High-precision timer using process.hrtime.bigint()
 * @class HRTimer
 */
export class HRTimer {
  constructor() {
    this.start = 0n;
  }

  /**
   * Start timing
   */
  begin() {
    this.start = process.hrtime.bigint();
  }

  /**
   * Get elapsed time in nanoseconds
   * @returns {bigint} Elapsed time in nanoseconds
   */
  elapsedNs() {
    return process.hrtime.bigint() - this.start;
  }

  /**
   * Get elapsed time in microseconds
   * @returns {number} Elapsed time in microseconds
   */
  elapsedUs() {
    return Number(this.elapsedNs()) / 1000;
  }

  /**
   * Get elapsed time in milliseconds
   * @returns {number} Elapsed time in milliseconds
   */
  elapsedMs() {
    return Number(this.elapsedNs()) / 1_000_000;
  }
}

/**
 * Calculate percentiles from sorted array
 * @param {number[]} sortedValues - Sorted array of values
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

  if (lower === upper) return sortedValues[lower];
  return sortedValues[lower] * (1 - weight) + sortedValues[upper] * weight;
}

/**
 * Calculate statistics for measurement array
 * @param {number[]} measurements - Array of measurements in milliseconds
 * @returns {object} Statistics object
 */
export function analyzeStats(measurements) {
  if (measurements.length === 0) {
    return {
      count: 0,
      min: 0,
      max: 0,
      mean: 0,
      median: 0,
      p50: 0,
      p95: 0,
      p99: 0,
      stdDev: 0
    };
  }

  const sorted = [...measurements].sort((a, b) => a - b);
  const mean = sorted.reduce((sum, v) => sum + v, 0) / sorted.length;
  const variance = sorted.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / sorted.length;
  const stdDev = Math.sqrt(variance);

  return {
    count: sorted.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    median: calculatePercentile(sorted, 50),
    p50: calculatePercentile(sorted, 50),
    p95: calculatePercentile(sorted, 95),
    p99: calculatePercentile(sorted, 99),
    stdDev
  };
}

/**
 * Format milliseconds as human-readable string
 * @param {number} ms - Milliseconds
 * @returns {string} Formatted string
 */
export function formatMs(ms) {
  if (ms < 0.001) return `${(ms * 1000000).toFixed(2)}ns`;
  if (ms < 1) return `${(ms * 1000).toFixed(2)}µs`;
  return `${ms.toFixed(2)}ms`;
}

/**
 * Run benchmark with warmup and measurements
 * @param {Function} fn - Function to benchmark
 * @param {object} options - Benchmark options
 * @param {number} [options.warmup=10] - Warmup iterations
 * @param {number} [options.iterations=100] - Measurement iterations
 * @returns {Promise<object>} Benchmark results
 */
export async function runBenchmark(fn, options = {}) {
  const { warmup = 10, iterations = 100 } = options;
  const measurements = [];

  // Warmup phase
  for (let i = 0; i < warmup; i++) {
    await fn();
  }

  // Force garbage collection if available
  if (global.gc) {
    global.gc();
  }

  // Measurement phase
  for (let i = 0; i < iterations; i++) {
    const timer = new HRTimer();
    timer.begin();
    await fn();
    measurements.push(timer.elapsedMs());
  }

  return analyzeStats(measurements);
}

/**
 * Report benchmark results
 * @param {string} name - Benchmark name
 * @param {object} results - Statistics from analyzeStats
 * @param {string} [sla] - SLA to compare against
 * @returns {string} Formatted report
 */
export function reportResults(name, results, sla = null) {
  const slaStatus = sla ?
    (results.p99 <= parseFloat(sla) ? '✅' : '❌') : '';

  let report = `\n${name}:\n`;
  report += `  p50:${formatMs(results.p50).padStart(10)} `;
  report += `p95:${formatMs(results.p95).padStart(10)} `;
  report += `p99:${formatMs(results.p99).padStart(10)} `;
  if (sla) {
    report += `SLA:${sla}ms ${slaStatus}`;
  }
  report += `\n`;
  report += `  count:${results.count} mean:${formatMs(results.mean)} `;
  report += `stdDev:${formatMs(results.stdDev)}\n`;

  return report;
}

/**
 * Create test RDF data with specified number of quads
 * @param {number} quadCount - Number of quads to generate
 * @returns {Array} Array of quad-like objects
 */
export function generateTestQuads(quadCount) {
  const quads = [];
  for (let i = 0; i < quadCount; i++) {
    quads.push({
      subject: { termType: 'NamedNode', value: `http://example.org/subject${i}` },
      predicate: { termType: 'NamedNode', value: `http://example.org/predicate${i % 10}` },
      object: { termType: 'Literal', value: `value${i}`, language: 'en' },
      graph: { termType: 'DefaultGraph' }
    });
  }
  return quads;
}
