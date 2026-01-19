/**
 * @file Uptime Command Helpers
 * @module cli/commands/daemon/uptime/helpers
 * @description Shared helpers for uptime simulation, reporting, and benchmarking
 */

import { randomUUID } from 'crypto';

/**
 * Generate a unique ID with prefix
 * @param {string} prefix - ID prefix
 * @returns {string} Unique identifier
 */
export function generateId(prefix = 'uptime') {
  return `${prefix}-${randomUUID().slice(0, 8)}`;
}

/**
 * Seeded random number generator for reproducible simulations
 * @param {number} seed - Random seed
 * @returns {Function} Random function returning 0-1
 */
export function createSeededRandom(seed) {
  let state = seed;
  return function () {
    state = (state * 1103515245 + 12345) & 0x7fffffff;
    return state / 0x7fffffff;
  };
}

/**
 * Format duration in human-readable form
 * @param {number} ms - Duration in milliseconds
 * @returns {string} Formatted duration
 */
export function formatDuration(ms) {
  if (ms < 1000) return `${ms.toFixed(2)}ms`;
  if (ms < 60000) return `${(ms / 1000).toFixed(2)}s`;
  if (ms < 3600000) return `${(ms / 60000).toFixed(2)}m`;
  return `${(ms / 3600000).toFixed(2)}h`;
}

/**
 * Format percentage with appropriate precision
 * @param {number} value - Percentage value (0-100)
 * @returns {string} Formatted percentage
 */
export function formatPercentage(value) {
  if (value >= 99.99) return value.toFixed(4);
  if (value >= 99) return value.toFixed(3);
  return value.toFixed(2);
}

/**
 * Chaos injection parameters by level
 */
export const CHAOS_PARAMS = {
  none: { failureMultiplier: 0, latencyMultiplier: 1, recoveryDelay: 0 },
  low: { failureMultiplier: 1.5, latencyMultiplier: 1.2, recoveryDelay: 100 },
  medium: { failureMultiplier: 3, latencyMultiplier: 1.5, recoveryDelay: 250 },
  high: { failureMultiplier: 5, latencyMultiplier: 2, recoveryDelay: 500 },
  extreme: { failureMultiplier: 10, latencyMultiplier: 3, recoveryDelay: 1000 },
};

/**
 * Simulate a single heartbeat with optional chaos
 * @param {Object} options - Simulation options
 * @param {Function} options.random - Random function
 * @param {number} options.baseFailureRate - Base failure rate
 * @param {string} options.chaosLevel - Chaos level
 * @returns {Object} Heartbeat result
 */
export function simulateHeartbeat({ random, baseFailureRate, chaosLevel }) {
  const chaos = CHAOS_PARAMS[chaosLevel] || CHAOS_PARAMS.none;
  const effectiveFailureRate = baseFailureRate * chaos.failureMultiplier;
  const baseLatency = 10 + random() * 40;
  const latency = baseLatency * chaos.latencyMultiplier;
  const failed = random() < effectiveFailureRate;

  return {
    success: !failed,
    latency: Math.round(latency * 100) / 100,
    timestamp: new Date().toISOString(),
    chaosApplied: chaosLevel !== 'none',
  };
}

/**
 * Calculate percentile from sorted array
 * @param {number[]} sortedValues - Sorted array of values
 * @param {number} percentile - Percentile (0-100)
 * @returns {number} Percentile value
 */
export function calculatePercentile(sortedValues, percentile) {
  if (sortedValues.length === 0) return 0;
  const index = Math.ceil((percentile / 100) * sortedValues.length) - 1;
  return sortedValues[Math.max(0, index)];
}

/**
 * Calculate statistics from latency samples
 * @param {number[]} latencies - Array of latency values
 * @returns {Object} Statistics object
 */
export function calculateStats(latencies) {
  if (latencies.length === 0) {
    return { avg: 0, min: 0, max: 0, p50: 0, p90: 0, p95: 0, p99: 0 };
  }

  const sorted = [...latencies].sort((a, b) => a - b);
  const sum = sorted.reduce((acc, val) => acc + val, 0);

  return {
    avg: sum / sorted.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    p50: calculatePercentile(sorted, 50),
    p90: calculatePercentile(sorted, 90),
    p95: calculatePercentile(sorted, 95),
    p99: calculatePercentile(sorted, 99),
  };
}

/**
 * Calculate MTBF (Mean Time Between Failures)
 * @param {number} totalUptime - Total uptime in ms
 * @param {number} failureCount - Number of failures
 * @returns {number} MTBF in ms
 */
export function calculateMTBF(totalUptime, failureCount) {
  if (failureCount === 0) return totalUptime;
  return totalUptime / failureCount;
}

/**
 * Calculate MTTR (Mean Time To Recovery)
 * @param {number} totalDowntime - Total downtime in ms
 * @param {number} recoveryCount - Number of recoveries
 * @returns {number} MTTR in ms
 */
export function calculateMTTR(totalDowntime, recoveryCount) {
  if (recoveryCount === 0) return 0;
  return totalDowntime / recoveryCount;
}

/**
 * Format report data as CSV
 * @param {Object} report - Report data
 * @returns {string} CSV formatted string
 */
export function formatAsCSV(report) {
  const headers = [
    'report_id',
    'period',
    'generated_at',
    'sla_target',
    'sla_met',
    'uptime_pct',
    'downtime_ms',
    'incident_count',
    'mtbf_ms',
    'mttr_ms',
  ];

  const values = [
    report.reportId,
    report.period,
    report.generatedAt,
    report.slaTarget,
    report.slaMet,
    report.metrics.uptimePercentage,
    report.metrics.totalDowntime,
    report.metrics.incidentCount,
    report.metrics.mtbf,
    report.metrics.mttr,
  ];

  return `${headers.join(',')}\n${values.join(',')}`;
}

/**
 * Format benchmark data as CSV
 * @param {Object} benchmark - Benchmark data
 * @returns {string} CSV formatted string
 */
export function formatBenchmarkAsCSV(benchmark) {
  const headers = [
    'benchmark_id',
    'iterations',
    'concurrency',
    'completed_at',
    'avg_latency_ms',
    'min_latency_ms',
    'max_latency_ms',
    'p50_ms',
    'p90_ms',
    'p95_ms',
    'p99_ms',
    'throughput_ops',
    'error_rate',
  ];

  const m = benchmark.metrics;
  const values = [
    benchmark.benchmarkId,
    benchmark.iterations,
    benchmark.concurrency,
    benchmark.completedAt,
    m.avgLatency.toFixed(3),
    m.minLatency.toFixed(3),
    m.maxLatency.toFixed(3),
    m.p50.toFixed(3),
    m.p90.toFixed(3),
    m.p95.toFixed(3),
    m.p99.toFixed(3),
    m.throughput.toFixed(2),
    m.errorRate.toFixed(4),
  ];

  return `${headers.join(',')}\n${values.join(',')}`;
}

/**
 * Write output to file or stdout
 * @param {string} content - Content to write
 * @param {string|undefined} outputPath - Output file path (undefined for stdout)
 */
export async function writeOutput(content, outputPath) {
  if (outputPath) {
    const { writeFile } = await import('fs/promises');
    await writeFile(outputPath, content, 'utf-8');
    console.log(`Output written to: ${outputPath}`);
  } else {
    console.log(content);
  }
}

/**
 * Print separator line
 * @param {string} char - Character to use
 * @param {number} length - Line length
 */
export function printSeparator(char = '=', length = 60) {
  console.log(char.repeat(length));
}

/**
 * Print section header
 * @param {string} title - Section title
 */
export function printHeader(title) {
  console.log(`\n${title}`);
  printSeparator();
}
