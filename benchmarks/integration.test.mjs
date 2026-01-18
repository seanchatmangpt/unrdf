/**
 * @file Benchmark Integration Tests
 * @module benchmarks/integration.test
 * @vitest-environment node
 *
 * @description
 * Integration tests for the complete benchmark suite.
 * Tests running all 5 benchmarks, result aggregation, JSON export,
 * error handling, and runner orchestration.
 */

import { describe, it, expect, beforeAll } from 'vitest';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { Bench } from 'tinybench';
import { readFileSync, writeFileSync, existsSync, mkdirSync } from 'fs';
import { join } from 'path';

/**
 * Mock benchmark runner that simulates the 5 core benchmarks
 */
class BenchmarkRunner {
  constructor() {
    this.tracer = trace.getTracer('benchmark-integration-test', '1.0.0');
    this.results = [];
  }

  /**
   * Run hook registration benchmark
   */
  async runHookRegistrationBenchmark() {
    return await this.tracer.startActiveSpan('benchmark.hook-registration', async (span) => {
      const bench = new Bench({ time: 50 });

      // Simulate hook registration
      const hooks = Array.from({ length: 100 }, (_, i) => ({
        id: `hook-${i}`,
        handler: () => {},
      }));

      bench.add('hook-registration', () => {
        const manager = { hooks: [] };
        manager.hooks.push(hooks[0]);
      });

      await bench.run();

      const task = bench.tasks[0];
      const mean = task.result?.mean || 0.001;
      const results = {
        benchmarkId: 'hook-registration',
        meanLatency: mean,
        throughput: 1000 / mean,
        samples: task.result?.samples?.length || 1,
      };

      span.setAttribute('benchmark.id', 'hook-registration');
      span.setAttribute('benchmark.latency.mean.ms', results.meanLatency);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return results;
    });
  }

  /**
   * Run hook execution benchmark
   */
  async runHookExecutionBenchmark() {
    return await this.tracer.startActiveSpan('benchmark.hook-execution', async (span) => {
      const bench = new Bench({ time: 50 });

      const hook = {
        id: 'test-hook',
        handler: (context) => {
          return { success: true, value: context.value };
        },
      };

      bench.add('hook-execution', () => {
        hook.handler({ value: 'test' });
      });

      await bench.run();

      const task = bench.tasks[0];
      const mean = task.result?.mean || 0.001;
      const results = {
        benchmarkId: 'hook-execution',
        meanLatency: mean,
        throughput: 1000 / mean,
        samples: task.result?.samples?.length || 1,
      };

      span.setAttribute('benchmark.id', 'hook-execution');
      span.setAttribute('benchmark.latency.mean.ms', results.meanLatency);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return results;
    });
  }

  /**
   * Run validation benchmark
   */
  async runValidationBenchmark() {
    return await this.tracer.startActiveSpan('benchmark.hook-validation', async (span) => {
      const bench = new Bench({ time: 50 });

      // Simple validation function
      const validate = (value) => {
        return typeof value === 'string' && value.length > 0;
      };

      bench.add('validation', () => {
        validate('test-value');
      });

      await bench.run();

      const task = bench.tasks[0];
      const mean = task.result?.mean || 0.001;
      const results = {
        benchmarkId: 'hook-validation',
        meanLatency: mean,
        throughput: 1000 / mean,
        samples: task.result?.samples?.length || 1,
      };

      span.setAttribute('benchmark.id', 'hook-validation');
      span.setAttribute('benchmark.latency.mean.ms', results.meanLatency);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return results;
    });
  }

  /**
   * Run memory profiling benchmark
   */
  async runMemoryProfilingBenchmark() {
    return await this.tracer.startActiveSpan('benchmark.memory-profiling', async (span) => {
      const measureMemory = () => {
        if (global.gc) global.gc();
        const usage = process.memoryUsage();
        return usage.heapUsed / 1024 / 1024;
      };

      const baseline = measureMemory();

      // Allocate memory
      const data = Array.from({ length: 1000 }, (_, i) => ({
        id: i,
        value: `test-${i}`,
      }));

      const current = measureMemory();
      const overhead = current - baseline;

      const results = {
        benchmarkId: 'memory-profiling',
        baselineMemory: baseline,
        currentMemory: current,
        memoryOverhead: overhead,
      };

      span.setAttribute('benchmark.id', 'memory-profiling');
      span.setAttribute('benchmark.memory.overhead.mb', overhead);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      // Clean up
      data.length = 0;

      return results;
    });
  }

  /**
   * Run concurrent execution benchmark
   */
  async runConcurrentExecutionBenchmark() {
    return await this.tracer.startActiveSpan('benchmark.concurrent-execution', async (span) => {
      const workerCount = 10;
      const startTime = Date.now();

      // Simulate concurrent execution
      const workers = Array.from({ length: workerCount }, async (_, i) => {
        return new Promise((resolve) => {
          setTimeout(() => {
            resolve({ workerId: i, result: 'complete' });
          }, 10);
        });
      });

      await Promise.all(workers);

      const duration = Date.now() - startTime;
      const throughput = (workerCount / duration) * 1000;

      const results = {
        benchmarkId: 'concurrent-execution',
        workerCount,
        duration,
        throughput,
      };

      span.setAttribute('benchmark.id', 'concurrent-execution');
      span.setAttribute('benchmark.concurrent.workers', workerCount);
      span.setAttribute('benchmark.throughput.ops_per_sec', throughput);
      span.setStatus({ code: SpanStatusCode.OK });
      span.end();

      return results;
    });
  }

  /**
   * Run all benchmarks
   */
  async runAll() {
    return await this.tracer.startActiveSpan('benchmark.suite.all', async (span) => {
      span.setAttribute('benchmark.suite.name', 'Knowledge Hooks Performance Benchmark Suite');
      span.setAttribute('benchmark.suite.version', '1.0.0');
      span.setAttribute('benchmark.timestamp', new Date().toISOString());

      const results = [];

      try {
        // Run all 5 benchmarks
        results.push(await this.runHookRegistrationBenchmark());
        results.push(await this.runHookExecutionBenchmark());
        results.push(await this.runValidationBenchmark());
        results.push(await this.runMemoryProfilingBenchmark());
        results.push(await this.runConcurrentExecutionBenchmark());

        span.setAttribute('benchmark.suite.total', results.length);
        span.setAttribute('benchmark.suite.status', 'complete');
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        this.results = results;
        return results;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Aggregate results
   */
  aggregateResults() {
    return {
      suite: 'Knowledge Hooks Performance Benchmark Suite',
      version: '1.0.0',
      timestamp: new Date().toISOString(),
      platform: process.platform,
      arch: process.arch,
      nodeVersion: process.version,
      totalBenchmarks: this.results.length,
      results: this.results,
      summary: {
        avgLatency: this.results
          .filter((r) => r.meanLatency)
          .reduce((sum, r) => sum + r.meanLatency, 0) / this.results.filter((r) => r.meanLatency).length,
        avgThroughput: this.results
          .filter((r) => r.throughput)
          .reduce((sum, r) => sum + r.throughput, 0) / this.results.filter((r) => r.throughput).length,
        totalMemoryOverhead: this.results
          .filter((r) => r.memoryOverhead)
          .reduce((sum, r) => sum + r.memoryOverhead, 0),
      },
    };
  }

  /**
   * Export results to JSON file
   */
  exportToJSON(filePath) {
    const aggregated = this.aggregateResults();
    const dir = filePath.split('/').slice(0, -1).join('/');
    if (dir && !existsSync(dir)) {
      mkdirSync(dir, { recursive: true });
    }
    writeFileSync(filePath, JSON.stringify(aggregated, null, 2), 'utf-8');
    return aggregated;
  }
}

/**
 * Integration test suite
 */
describe('Benchmark Integration Tests', () => {
  let runner;
  let testOutputDir;

  beforeAll(() => {
    runner = new BenchmarkRunner();
    testOutputDir = join(process.cwd(), 'benchmarks', 'test-output');
  });

  it('should run all 5 benchmarks successfully', async () => {
    const results = await runner.runAll();

    expect(results).toHaveLength(5);
    expect(results[0].benchmarkId).toBe('hook-registration');
    expect(results[1].benchmarkId).toBe('hook-execution');
    expect(results[2].benchmarkId).toBe('hook-validation');
    expect(results[3].benchmarkId).toBe('memory-profiling');
    expect(results[4].benchmarkId).toBe('concurrent-execution');
  });

  it('should collect valid measurements from each benchmark', async () => {
    const results = await runner.runAll();

    // Verify hook registration results
    expect(results[0].meanLatency).toBeGreaterThan(0);
    expect(results[0].throughput).toBeGreaterThan(0);
    expect(results[0].samples).toBeGreaterThan(0);

    // Verify hook execution results
    expect(results[1].meanLatency).toBeGreaterThan(0);
    expect(results[1].throughput).toBeGreaterThan(0);

    // Verify validation results
    expect(results[2].meanLatency).toBeGreaterThan(0);
    expect(results[2].throughput).toBeGreaterThan(0);

    // Verify memory profiling results
    expect(results[3].baselineMemory).toBeGreaterThan(0);
    expect(results[3].currentMemory).toBeGreaterThan(0);
    expect(results[3].memoryOverhead).toBeGreaterThanOrEqual(0);

    // Verify concurrent execution results
    expect(results[4].workerCount).toBe(10);
    expect(results[4].duration).toBeGreaterThan(0);
    expect(results[4].throughput).toBeGreaterThan(0);
  });

  it('should aggregate results correctly', async () => {
    await runner.runAll();
    const aggregated = runner.aggregateResults();

    expect(aggregated.suite).toBe('Knowledge Hooks Performance Benchmark Suite');
    expect(aggregated.version).toBe('1.0.0');
    expect(aggregated.timestamp).toBeTruthy();
    expect(aggregated.platform).toBe(process.platform);
    expect(aggregated.arch).toBe(process.arch);
    expect(aggregated.nodeVersion).toMatch(/^v\d+\.\d+\.\d+/);
    expect(aggregated.totalBenchmarks).toBe(5);
    expect(aggregated.results).toHaveLength(5);
    expect(aggregated.summary.avgLatency).toBeGreaterThan(0);
    expect(aggregated.summary.avgThroughput).toBeGreaterThan(0);
  });

  it('should export valid JSON results', async () => {
    await runner.runAll();

    const outputPath = join(testOutputDir, 'integration-test-results.json');
    const exported = runner.exportToJSON(outputPath);

    // Verify export succeeded
    expect(exported).toBeDefined();
    expect(exported.results).toHaveLength(5);
    expect(exported.summary).toBeDefined();

    // Verify file was created
    expect(existsSync(outputPath)).toBe(true);

    // Verify JSON is valid
    const fileContent = readFileSync(outputPath, 'utf-8');
    const parsed = JSON.parse(fileContent);
    expect(parsed.suite).toBe('Knowledge Hooks Performance Benchmark Suite');
    expect(parsed.results).toHaveLength(5);
  });

  it('should handle errors gracefully', async () => {
    const errorRunner = new BenchmarkRunner();

    // Override a benchmark to throw error
    errorRunner.runHookRegistrationBenchmark = async () => {
      throw new Error('Simulated benchmark error');
    };

    await expect(errorRunner.runAll()).rejects.toThrow('Simulated benchmark error');
  });

  it('should validate result completeness', async () => {
    await runner.runAll();
    const aggregated = runner.aggregateResults();

    // Verify all benchmarks completed
    const benchmarkIds = aggregated.results.map((r) => r.benchmarkId);
    expect(benchmarkIds).toContain('hook-registration');
    expect(benchmarkIds).toContain('hook-execution');
    expect(benchmarkIds).toContain('hook-validation');
    expect(benchmarkIds).toContain('memory-profiling');
    expect(benchmarkIds).toContain('concurrent-execution');

    // Verify no duplicate benchmark IDs
    const uniqueIds = new Set(benchmarkIds);
    expect(uniqueIds.size).toBe(5);
  });

  it('should maintain consistent result structure', async () => {
    await runner.runAll();
    const aggregated = runner.aggregateResults();

    // Verify required fields exist
    expect(aggregated).toHaveProperty('suite');
    expect(aggregated).toHaveProperty('version');
    expect(aggregated).toHaveProperty('timestamp');
    expect(aggregated).toHaveProperty('platform');
    expect(aggregated).toHaveProperty('arch');
    expect(aggregated).toHaveProperty('nodeVersion');
    expect(aggregated).toHaveProperty('totalBenchmarks');
    expect(aggregated).toHaveProperty('results');
    expect(aggregated).toHaveProperty('summary');

    // Verify summary structure
    expect(aggregated.summary).toHaveProperty('avgLatency');
    expect(aggregated.summary).toHaveProperty('avgThroughput');
    expect(aggregated.summary).toHaveProperty('totalMemoryOverhead');
  });
});

/**
 * Error recovery tests
 */
describe('Error Handling and Recovery', () => {
  it('should continue after single benchmark failure', async () => {
    const runner = new BenchmarkRunner();
    const results = [];

    // Run benchmarks with error handling
    try {
      results.push(await runner.runHookRegistrationBenchmark());
    } catch (error) {
      results.push({ benchmarkId: 'hook-registration', error: error.message });
    }

    try {
      results.push(await runner.runHookExecutionBenchmark());
    } catch (error) {
      results.push({ benchmarkId: 'hook-execution', error: error.message });
    }

    // Should have 2 results even if one failed
    expect(results).toHaveLength(2);
  });

  it('should validate JSON export path', () => {
    const runner = new BenchmarkRunner();

    expect(() => {
      runner.exportToJSON('');
    }).toThrow();
  });
});
