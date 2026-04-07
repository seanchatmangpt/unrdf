/**
 * @file Performance timing utilities for benchmark tests.
 *
 * Replaces repeated performance.now() boilerplate across 8+ test files.
 *
 * @module @unrdf/test-utils/timing
 */

// ============================================================================
// Benchmark helpers
// ============================================================================

/**
 * Time a synchronous function once.
 *
 * @param {Function} fn - Function to time
 * @returns {number} Duration in milliseconds
 */
export function timeSync(fn) {
  const start = performance.now();
  fn();
  return performance.now() - start;
}

/**
 * Run a function N times and return timing statistics.
 *
 * @param {Function} fn - Function to benchmark
 * @param {number} [iterations=100] - Number of iterations
 * @returns {{ avg: number, min: number, max: number, p95: number, total: number, iterations: number }}
 *
 * @example
 * const stats = benchmarkSync(() => store.add(testQuad), 1000);
 * expect(stats.avg).toBeLessThan(1); // < 1ms average
 * console.log(`Avg: ${(stats.avg * 1000).toFixed(1)}μs`);
 */
export function benchmarkSync(fn, iterations = 100) {
  const durations = [];
  for (let i = 0; i < iterations; i++) {
    durations.push(timeSync(fn));
  }
  durations.sort((a, b) => a - b);
  const total = durations.reduce((sum, d) => sum + d, 0);
  return {
    avg: total / iterations,
    min: durations[0],
    max: durations[durations.length - 1],
    p95: durations[Math.floor(iterations * 0.95)],
    total,
    iterations,
    /** μs per operation */
    perOpUs: (total / iterations) * 1000,
  };
}

/**
 * Measure throughput: run fn for `durationMs` milliseconds and count iterations.
 *
 * @param {Function} fn
 * @param {number} [durationMs=100]
 * @returns {{ opsPerSec: number, iterations: number, durationMs: number }}
 */
export function measureThroughput(fn, durationMs = 100) {
  const deadline = performance.now() + durationMs;
  let count = 0;
  while (performance.now() < deadline) {
    fn();
    count++;
  }
  return {
    opsPerSec: Math.round((count / durationMs) * 1000),
    iterations: count,
    durationMs,
  };
}

// ============================================================================
// Assertion helpers for performance tests
// ============================================================================

/**
 * Assert that a function completes N iterations within a time budget.
 * Throws with a helpful message if it exceeds the limit.
 *
 * @param {Function} fn
 * @param {number} count - Number of iterations
 * @param {number} budgetMs - Maximum allowed milliseconds
 * @returns {number} Actual duration in ms
 *
 * @example
 * assertPerf(() => executeHook(hook, testQuad), 1000, 200);
 * // "1000 iterations of [fn] took Xms, budget was 200ms"
 */
export function assertPerf(fn, count, budgetMs) {
  const start = performance.now();
  for (let i = 0; i < count; i++) fn();
  const duration = performance.now() - start;
  if (duration > budgetMs) {
    throw new Error(
      `Performance budget exceeded: ${count} iterations took ${duration.toFixed(1)}ms, budget was ${budgetMs}ms`
    );
  }
  return duration;
}
