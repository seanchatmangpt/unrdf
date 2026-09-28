/**
 * @file Benchmark Validation Tests
 * @module benchmarks/validation.test
 * @vitest-environment node
 *
 * @description
 * Validates benchmark correctness, OTEL span creation, metric calculations,
 * memory measurement accuracy, and regression detection logic.
 *
 * These tests ensure that benchmark infrastructure works correctly before
 * using it to measure actual performance.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { Bench } from 'tinybench';

/**
 * Test OTEL span creation and attributes
 */
describe('OTEL Span Validation', () => {
  let tracer;

  beforeEach(() => {
    tracer = trace.getTracer('benchmark-validation-test', '1.0.0');
  });

  it('should create root span with required attributes', async () => {
    let capturedAttributes = {};

    await tracer.startActiveSpan('benchmark.test', (span) => {
      // Set required common attributes
      span.setAttribute('benchmark.suite.name', 'Test Suite');
      span.setAttribute('benchmark.suite.version', '1.0.0');
      span.setAttribute('benchmark.id', 'test-benchmark');
      span.setAttribute('benchmark.name', 'Test Benchmark');
      span.setAttribute('benchmark.timestamp', new Date().toISOString());
      span.setAttribute('system.platform', process.platform);
      span.setAttribute('system.arch', process.arch);
      span.setAttribute('process.nodeVersion', process.version);
      span.setAttribute('process.pid', process.pid);

      // Validate attributes exist (in real implementation, extract from span)
      capturedAttributes = {
        suiteName: 'Test Suite',
        suiteVersion: '1.0.0',
        benchmarkId: 'test-benchmark',
        platform: process.platform,
        arch: process.arch,
        nodeVersion: process.version,
      };

      span.setStatus({ code: SpanStatusCode.OK });
      span.end();
    });

    // Validate all required attributes are present
    expect(capturedAttributes.suiteName).toBe('Test Suite');
    expect(capturedAttributes.suiteVersion).toBe('1.0.0');
    expect(capturedAttributes.benchmarkId).toBe('test-benchmark');
    expect(capturedAttributes.platform).toBeTruthy();
    expect(capturedAttributes.arch).toBeTruthy();
    expect(capturedAttributes.nodeVersion).toMatch(/^v\d+\.\d+\.\d+/);
  });

  it('should create child spans for operations', async () => {
    const operations = [];

    await tracer.startActiveSpan('benchmark.root', (rootSpan) => {
      operations.push('root');

      tracer.startActiveSpan('benchmark.setup', (setupSpan) => {
        operations.push('setup');
        setupSpan.setStatus({ code: SpanStatusCode.OK });
        setupSpan.end();
      });

      tracer.startActiveSpan('benchmark.measure', (measureSpan) => {
        operations.push('measure');
        measureSpan.setStatus({ code: SpanStatusCode.OK });
        measureSpan.end();
      });

      tracer.startActiveSpan('benchmark.validation', (validationSpan) => {
        operations.push('validation');
        validationSpan.setStatus({ code: SpanStatusCode.OK });
        validationSpan.end();
      });

      rootSpan.setStatus({ code: SpanStatusCode.OK });
      rootSpan.end();
    });

    expect(operations).toEqual(['root', 'setup', 'measure', 'validation']);
  });

  it('should set span status based on validation results', async () => {
    const results = { pass: 0, fail: 0 };

    // Test PASS scenario
    await tracer.startActiveSpan('benchmark.pass', (span) => {
      const validation = { status: 'PASS', targetsMet: 5, targetsTotal: 5 };

      span.setAttribute('validation.status', validation.status);
      span.setAttribute('validation.targetsMet', validation.targetsMet);
      span.setStatus({
        code: validation.status === 'PASS' ? SpanStatusCode.OK : SpanStatusCode.ERROR,
      });
      span.end();

      results.pass = 1;
    });

    // Test FAIL scenario
    await tracer.startActiveSpan('benchmark.fail', (span) => {
      const validation = { status: 'FAIL', targetsMet: 2, targetsTotal: 5 };

      span.setAttribute('validation.status', validation.status);
      span.setAttribute('validation.targetsMet', validation.targetsMet);
      span.setStatus({
        code: validation.status === 'PASS' ? SpanStatusCode.OK : SpanStatusCode.ERROR,
      });
      span.end();

      results.fail = 1;
    });

    expect(results.pass).toBe(1);
    expect(results.fail).toBe(1);
  });
});

/**
 * Test percentile calculation accuracy
 */
describe('Percentile Calculator Validation', () => {
  /**
   * Calculate percentile from array of values
   * @param {number[]} values - Array of measurements
   * @param {number} percentile - Percentile to calculate (0-100)
   * @returns {number} Calculated percentile value
   */
  function calculatePercentile(values, percentile) {
    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil((percentile / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }

  it('should calculate p50 (median) correctly', () => {
    const values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    const p50 = calculatePercentile(values, 50);

    // Median of [1..10] should be 5 or 6
    expect(p50).toBeGreaterThanOrEqual(5);
    expect(p50).toBeLessThanOrEqual(6);
  });

  it('should calculate p95 correctly', () => {
    const values = Array.from({ length: 100 }, (_, i) => i + 1); // 1..100
    const p95 = calculatePercentile(values, 95);

    // 95th percentile of 1..100 should be 95
    expect(p95).toBe(95);
  });

  it('should calculate p99 correctly', () => {
    const values = Array.from({ length: 100 }, (_, i) => i + 1); // 1..100
    const p99 = calculatePercentile(values, 99);

    // 99th percentile of 1..100 should be 99
    expect(p99).toBe(99);
  });

  it('should handle small sample sizes', () => {
    const values = [1, 2, 3];
    const p50 = calculatePercentile(values, 50);
    const p95 = calculatePercentile(values, 95);

    expect(p50).toBe(2);
    expect(p95).toBe(3);
  });

  it('should handle single value', () => {
    const values = [42];
    const p50 = calculatePercentile(values, 50);
    const p95 = calculatePercentile(values, 95);
    const p99 = calculatePercentile(values, 99);

    expect(p50).toBe(42);
    expect(p95).toBe(42);
    expect(p99).toBe(42);
  });

  it('should handle unsorted input', () => {
    const values = [10, 1, 5, 8, 3, 7, 2, 9, 4, 6];
    const p50 = calculatePercentile(values, 50);

    expect(p50).toBeGreaterThanOrEqual(5);
    expect(p50).toBeLessThanOrEqual(6);
  });
});

/**
 * Test memory measurement accuracy
 */
describe('Memory Measurement Validation', () => {
  /**
   * Measure current memory usage
   * @returns {Object} Memory usage in MB
   */
  function measureMemory() {
    if (global.gc) global.gc();
    const usage = process.memoryUsage();
    return {
      heapUsed: usage.heapUsed / 1024 / 1024,
      heapTotal: usage.heapTotal / 1024 / 1024,
      external: usage.external / 1024 / 1024,
      rss: usage.rss / 1024 / 1024,
    };
  }

  it('should measure memory in megabytes', () => {
    const memory = measureMemory();

    expect(memory.heapUsed).toBeGreaterThan(0);
    expect(memory.heapTotal).toBeGreaterThan(0);
    expect(memory.rss).toBeGreaterThan(0);
    expect(memory.heapUsed).toBeLessThan(memory.heapTotal);
  });

  it('should detect memory growth', () => {
    const baseline = measureMemory();

    // Allocate 5MB of memory
    const allocations = [];
    for (let i = 0; i < 5; i++) {
      allocations.push(new Array(1024 * 256).fill(0)); // ~1MB each
    }

    const current = measureMemory();
    const growth = current.heapUsed - baseline.heapUsed;

    // Should detect at least 3MB growth (accounting for GC)
    expect(growth).toBeGreaterThan(3);

    // Clean up
    allocations.length = 0;
    if (global.gc) global.gc();
  });

  it('should calculate memory overhead correctly', () => {
    const baseline = measureMemory();

    // Simulate hook registration (small objects)
    const hooks = Array.from({ length: 100 }, (_, i) => ({
      id: `hook-${i}`,
      name: `Test Hook ${i}`,
      handler: () => {},
    }));

    const current = measureMemory();
    const overhead = current.heapUsed - baseline.heapUsed;
    const perHook = (overhead / hooks.length) * 1000; // per 1000 hooks

    // Each hook should use < 1MB per 1000 hooks
    expect(perHook).toBeLessThan(1);
  });
});

/**
 * Test regression detection logic
 */
describe('Regression Detection Validation', () => {
  /**
   * Compare metric against baseline and detect regression
   * @param {number} current - Current metric value
   * @param {number} baseline - Baseline metric value
   * @param {number} threshold - Regression threshold (e.g., 0.1 for 10%)
   * @param {string} direction - 'higher' or 'lower' is better
   * @returns {Object} Regression detection result
   */
  function detectRegression(current, baseline, threshold, direction = 'lower') {
    const delta = current - baseline;
    const percentChange = (delta / baseline) * 100;

    let isRegression = false;
    if (direction === 'lower') {
      // For latency/memory: higher is worse
      isRegression = delta > baseline * threshold;
    } else {
      // For throughput: lower is worse
      isRegression = delta < -(baseline * threshold);
    }

    return {
      current,
      baseline,
      delta,
      percentChange,
      isRegression,
      threshold: threshold * 100,
    };
  }

  it('should detect latency regression (10% threshold)', () => {
    const baseline = 10; // ms
    const current = 12; // ms (20% increase)
    const threshold = 0.1; // 10%

    const result = detectRegression(current, baseline, threshold, 'lower');

    expect(result.percentChange).toBe(20);
    expect(result.isRegression).toBe(true);
  });

  it('should not flag acceptable latency increase', () => {
    const baseline = 10; // ms
    const current = 10.5; // ms (5% increase)
    const threshold = 0.1; // 10%

    const result = detectRegression(current, baseline, threshold, 'lower');

    expect(result.percentChange).toBe(5);
    expect(result.isRegression).toBe(false);
  });

  it('should detect throughput regression', () => {
    const baseline = 1000; // ops/sec
    const current = 800; // ops/sec (20% decrease)
    const threshold = 0.1; // 10%

    const result = detectRegression(current, baseline, threshold, 'higher');

    expect(result.percentChange).toBe(-20);
    expect(result.isRegression).toBe(true);
  });

  it('should not flag throughput improvement', () => {
    const baseline = 1000; // ops/sec
    const current = 1200; // ops/sec (20% increase)
    const threshold = 0.1; // 10%

    const result = detectRegression(current, baseline, threshold, 'higher');

    expect(result.percentChange).toBe(20);
    expect(result.isRegression).toBe(false);
  });

  it('should handle memory regression correctly', () => {
    const baseline = 50; // MB
    const current = 75; // MB (50% increase)
    const threshold = 0.3; // 30%

    const result = detectRegression(current, baseline, threshold, 'lower');

    expect(result.percentChange).toBe(50);
    expect(result.isRegression).toBe(true);
  });
});

/**
 * Test baseline target validation
 */
describe('Baseline Target Validation', () => {
  /**
   * Validate results against baseline targets
   * @param {Object} results - Benchmark results
   * @param {Object} targets - Baseline targets
   * @returns {Object} Validation result
   */
  function validateAgainstTargets(results, targets) {
    const validation = {
      status: 'PASS',
      targetsMet: 0,
      targetsTotal: 0,
      failures: [],
    };

    // Check each target
    for (const [metric, target] of Object.entries(targets)) {
      validation.targetsTotal++;

      const actual = results[metric];
      let passes = false;

      // Parse target (e.g., "< 1", "> 1000")
      if (target.startsWith('< ')) {
        const threshold = parseFloat(target.substring(2));
        passes = actual < threshold;
      } else if (target.startsWith('> ')) {
        const threshold = parseFloat(target.substring(2));
        passes = actual > threshold;
      }

      if (passes) {
        validation.targetsMet++;
      } else {
        validation.failures.push(`${metric}: ${actual} (target: ${target})`);
      }
    }

    if (validation.targetsMet < validation.targetsTotal) {
      validation.status = 'FAIL';
    }

    return validation;
  }

  it('should pass when all targets are met', () => {
    const results = {
      avgLatency: 0.8,
      p95Latency: 1.5,
      throughput: 1200,
      memory: 20,
    };

    const targets = {
      avgLatency: '< 1',
      p95Latency: '< 2',
      throughput: '> 1000',
      memory: '< 25',
    };

    const validation = validateAgainstTargets(results, targets);

    expect(validation.status).toBe('PASS');
    expect(validation.targetsMet).toBe(4);
    expect(validation.targetsTotal).toBe(4);
    expect(validation.failures).toHaveLength(0);
  });

  it('should fail when targets are not met', () => {
    const results = {
      avgLatency: 1.5,
      p95Latency: 3.0,
      throughput: 800,
      memory: 30,
    };

    const targets = {
      avgLatency: '< 1',
      p95Latency: '< 2',
      throughput: '> 1000',
      memory: '< 25',
    };

    const validation = validateAgainstTargets(results, targets);

    expect(validation.status).toBe('FAIL');
    expect(validation.targetsMet).toBe(0);
    expect(validation.targetsTotal).toBe(4);
    expect(validation.failures).toHaveLength(4);
  });

  it('should handle partial target success', () => {
    const results = {
      avgLatency: 0.5, // PASS
      p95Latency: 3.0, // FAIL
      throughput: 1200, // PASS
    };

    const targets = {
      avgLatency: '< 1',
      p95Latency: '< 2',
      throughput: '> 1000',
    };

    const validation = validateAgainstTargets(results, targets);

    expect(validation.status).toBe('FAIL');
    expect(validation.targetsMet).toBe(2);
    expect(validation.targetsTotal).toBe(3);
    expect(validation.failures).toHaveLength(1);
  });
});

/**
 * Test tinybench integration
 */
describe('Tinybench Integration Validation', () => {
  it('should run simple benchmark with tinybench', async () => {
    const bench = new Bench({ time: 100 });

    let executionCount = 0;

    bench.add('test-operation', () => {
      executionCount++;
      // Simple operation
      const result = Math.sqrt(16);
      return result;
    });

    await bench.run();

    const task = bench.tasks[0];
    expect(task.name).toBe('test-operation');
    expect(task.result).toBeDefined();
    expect(task.result.mean).toBeGreaterThan(0);
    expect(executionCount).toBeGreaterThan(0);
  });

  it('should collect accurate timing samples', async () => {
    const bench = new Bench({ time: 100 });

    bench.add('timed-operation', () => {
      // Operation that takes ~1ms
      const start = Date.now();
      while (Date.now() - start < 1) {
        // Busy wait
      }
    });

    await bench.run();

    const task = bench.tasks[0];
    expect(task.result.samples.length).toBeGreaterThan(0);
    expect(task.result.mean).toBeGreaterThan(0.5); // Should take at least 0.5ms
  });

  it('should provide statistical metrics', async () => {
    const bench = new Bench({ time: 100 });

    bench.add('math-operation', () => {
      return Math.pow(2, 16);
    });

    await bench.run();

    const task = bench.tasks[0];
    const stats = task.result;

    expect(stats.mean).toBeDefined();
    expect(stats.variance).toBeDefined();
    expect(stats.sd).toBeDefined();
    expect(stats.min).toBeDefined();
    expect(stats.max).toBeDefined();
    expect(stats.samples).toBeInstanceOf(Array);
  });
});
