/**
 * Example Benchmark Implementation: Hook Registration
 *
 * This is a reference implementation showing how to implement benchmarks
 * according to the Knowledge Hooks Benchmark Specification.
 *
 * @see /home/user/unrdf/benchmark/specs/BENCHMARK-SPECIFICATION.md
 */

import { Bench } from 'tinybench';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';

/**
 * Hook Registration Benchmark
 *
 * Measures the speed of registering N hooks to the knowledge engine.
 * Implements scenario: "medium-batch" (1000 hooks)
 *
 * Baseline Targets:
 * - avgLatency: < 1ms per hook
 * - p95Latency: < 2ms per hook
 * - throughput: > 1000 hooks/sec
 * - memoryOverhead: < 25MB
 * - errorRate: < 0.1%
 */

// Initialize OTEL tracer
const tracer = trace.getTracer('knowledge-hooks-benchmark', '1.0.0');

// Benchmark configuration from spec
const BENCHMARK_CONFIG = {
  id: 'hook-registration',
  scenario: 'medium-batch',
  parameters: {
    hookCount: 1000,
    complexity: 'medium',
    concurrency: 1,
    warmupRuns: 5,
    measurementRuns: 50,
    timeout: 10000
  },
  expectedTargets: {
    avgLatency: 1, // ms
    p95Latency: 2, // ms
    throughput: 1000, // hooks/sec
    memoryOverhead: 25 // MB
  }
};

/**
 * Generate test hooks of varying complexity
 */
function generateTestHooks(count, complexity = 'medium') {
  const hooks = [];

  for (let i = 0; i < count; i++) {
    const hook = defineHook({
      id: `test-hook-${i}`,
      name: `Test Hook ${i}`,
      description: `Generated test hook for benchmarking`,
      version: '1.0.0',

      // Hook complexity varies based on parameter
      triggers: complexity === 'simple'
        ? ['onLoad']
        : ['onLoad', 'onChange', 'beforeCommit'],

      schema: {
        type: 'object',
        properties: {
          value: { type: complexity === 'complex' ? 'array' : 'string' },
          timestamp: { type: 'number' }
        },
        required: ['value']
      },

      handler: async (context) => {
        // Simple validation
        if (!context.value) throw new Error('Missing value');

        if (complexity === 'medium' || complexity === 'complex') {
          // Store operation simulation
          context.store?.addQuad({
            subject: `test:${i}`,
            predicate: 'test:value',
            object: String(context.value),
            graph: 'test:graph'
          });
        }

        if (complexity === 'complex') {
          // Async operation simulation
          await new Promise(resolve => setImmediate(resolve));
        }

        return { success: true, hookId: `test-hook-${i}` };
      }
    });

    hooks.push(hook);
  }

  return hooks;
}

/**
 * Measure memory usage
 */
function measureMemory() {
  if (global.gc) global.gc();
  const usage = process.memoryUsage();
  return {
    heapUsed: usage.heapUsed / 1024 / 1024, // MB
    heapTotal: usage.heapTotal / 1024 / 1024,
    external: usage.external / 1024 / 1024,
    rss: usage.rss / 1024 / 1024
  };
}

/**
 * Calculate percentiles from array of values
 */
function calculatePercentile(values, percentile) {
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil((percentile / 100) * sorted.length) - 1;
  return sorted[index];
}

/**
 * Main benchmark execution
 */
export async function runHookRegistrationBenchmark() {
  return await tracer.startActiveSpan(
    'benchmark.hook-registration',
    async (rootSpan) => {
      try {
        // Set root span attributes
        rootSpan.setAttribute('benchmark.suite.name', 'Knowledge Hooks Performance Benchmark Suite');
        rootSpan.setAttribute('benchmark.suite.version', '1.0.0');
        rootSpan.setAttribute('benchmark.id', BENCHMARK_CONFIG.id);
        rootSpan.setAttribute('benchmark.name', 'Hook Registration Benchmark');
        rootSpan.setAttribute('benchmark.scenario', BENCHMARK_CONFIG.scenario);
        rootSpan.setAttribute('benchmark.timestamp', new Date().toISOString());
        rootSpan.setAttribute('system.platform', process.platform);
        rootSpan.setAttribute('system.arch', process.arch);
        rootSpan.setAttribute('process.nodeVersion', process.version);
        rootSpan.setAttribute('process.pid', process.pid);

        console.log('🚀 Starting Hook Registration Benchmark');
        console.log(`   Scenario: ${BENCHMARK_CONFIG.scenario}`);
        console.log(`   Hooks: ${BENCHMARK_CONFIG.parameters.hookCount}`);
        console.log(`   Complexity: ${BENCHMARK_CONFIG.parameters.complexity}`);
        console.log('');

        // ====================
        // SETUP PHASE
        // ====================
        const setupSpan = tracer.startActiveSpan('benchmark.setup', (span) => {
          console.log('📋 Setup Phase');

          // Generate test hooks
          console.log(`   Generating ${BENCHMARK_CONFIG.parameters.hookCount} test hooks...`);
          const testHooks = generateTestHooks(
            BENCHMARK_CONFIG.parameters.hookCount,
            BENCHMARK_CONFIG.parameters.complexity
          );

          // Initialize hook manager
          const hookManager = new KnowledgeHookManager();

          // Force GC and record baseline memory
          console.log('   Measuring baseline memory...');
          const baselineMemory = measureMemory();
          console.log(`   Baseline memory: ${baselineMemory.heapUsed.toFixed(2)} MB`);

          span.setAttribute('setup.hookCount', testHooks.length);
          span.setAttribute('setup.baselineMemory.mb', baselineMemory.heapUsed);
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();

          return { testHooks, hookManager, baselineMemory };
        });

        const { testHooks, hookManager, baselineMemory } = setupSpan;

        // ====================
        // WARMUP PHASE
        // ====================
        console.log('');
        console.log('🔥 Warmup Phase');
        console.log(`   Warmup runs: ${BENCHMARK_CONFIG.parameters.warmupRuns}`);

        const warmupSpan = tracer.startActiveSpan('benchmark.warmup', (span) => {
          for (let i = 0; i < BENCHMARK_CONFIG.parameters.warmupRuns; i++) {
            const warmupManager = new KnowledgeHookManager();
            warmupManager.registerHook(testHooks[i]);
          }

          span.setAttribute('warmup.runs', BENCHMARK_CONFIG.parameters.warmupRuns);
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
        });

        // ====================
        // MEASUREMENT PHASE
        // ====================
        console.log('');
        console.log('📊 Measurement Phase');
        console.log(`   Measurement runs: ${BENCHMARK_CONFIG.parameters.measurementRuns}`);

        const measurementResults = await tracer.startActiveSpan(
          'benchmark.measure',
          async (span) => {
            const bench = new Bench({
              time: BENCHMARK_CONFIG.parameters.timeout,
              iterations: BENCHMARK_CONFIG.parameters.measurementRuns
            });

            let hookIndex = 0;

            bench.add('hook-registration', () => {
              const manager = new KnowledgeHookManager();
              const hook = testHooks[hookIndex % testHooks.length];
              manager.registerHook(hook);
              hookIndex++;
            });

            // Run benchmark
            await bench.run();

            // Extract results
            const task = bench.tasks[0];
            const results = {
              totalOps: task.result.samples.length,
              totalDuration: task.result.totalTime,
              samples: task.result.samples, // Individual timings in ms
              meanLatency: task.result.mean,
              minLatency: task.result.min,
              maxLatency: task.result.max,
              variance: task.result.variance,
              sd: task.result.sd
            };

            // Calculate percentiles
            const p50 = calculatePercentile(results.samples, 50);
            const p95 = calculatePercentile(results.samples, 95);
            const p99 = calculatePercentile(results.samples, 99);

            results.p50Latency = p50;
            results.p95Latency = p95;
            results.p99Latency = p99;

            // Calculate throughput
            results.throughput = (results.totalOps / results.totalDuration) * 1000; // ops/sec

            // Record in span
            span.setAttribute('benchmark.totalOps', results.totalOps);
            span.setAttribute('benchmark.duration.ms', results.totalDuration);
            span.setAttribute('benchmark.latency.mean.ms', results.meanLatency);
            span.setAttribute('benchmark.latency.p50.ms', results.p50Latency);
            span.setAttribute('benchmark.latency.p95.ms', results.p95Latency);
            span.setAttribute('benchmark.latency.p99.ms', results.p99Latency);
            span.setAttribute('benchmark.latency.max.ms', results.maxLatency);
            span.setAttribute('benchmark.throughput.ops_per_sec', results.throughput);

            span.setStatus({ code: SpanStatusCode.OK });
            span.end();

            return results;
          }
        );

        // ====================
        // MEMORY MEASUREMENT
        // ====================
        console.log('');
        console.log('💾 Memory Measurement');

        const memoryResults = tracer.startActiveSpan('memory.measure', (span) => {
          const currentMemory = measureMemory();
          const memoryOverhead = currentMemory.heapUsed - baselineMemory.heapUsed;
          const memoryPer1kHooks = (memoryOverhead / BENCHMARK_CONFIG.parameters.hookCount) * 1000;

          console.log(`   Current memory: ${currentMemory.heapUsed.toFixed(2)} MB`);
          console.log(`   Memory overhead: ${memoryOverhead.toFixed(2)} MB`);
          console.log(`   Per 1000 hooks: ${memoryPer1kHooks.toFixed(2)} MB`);

          span.setAttribute('memory.baseline.mb', baselineMemory.heapUsed);
          span.setAttribute('memory.current.mb', currentMemory.heapUsed);
          span.setAttribute('memory.overhead.mb', memoryOverhead);
          span.setAttribute('memory.per1kHooks.mb', memoryPer1kHooks);
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();

          return {
            baseline: baselineMemory,
            current: currentMemory,
            overhead: memoryOverhead,
            per1kHooks: memoryPer1kHooks
          };
        });

        // ====================
        // VALIDATION PHASE
        // ====================
        console.log('');
        console.log('✅ Validation Phase');

        const validation = tracer.startActiveSpan('benchmark.validation', (span) => {
          const results = {
            status: 'PASS',
            targetsMet: 0,
            targetsTotal: 0,
            failures: []
          };

          // Validate against targets
          const targets = BENCHMARK_CONFIG.expectedTargets;

          // Check mean latency
          results.targetsTotal++;
          if (measurementResults.meanLatency < targets.avgLatency) {
            results.targetsMet++;
            console.log(`   ✅ Mean latency: ${measurementResults.meanLatency.toFixed(3)}ms (target: < ${targets.avgLatency}ms)`);
          } else {
            results.failures.push(`Mean latency ${measurementResults.meanLatency.toFixed(3)}ms exceeds target ${targets.avgLatency}ms`);
            console.log(`   ❌ Mean latency: ${measurementResults.meanLatency.toFixed(3)}ms (target: < ${targets.avgLatency}ms)`);
          }

          // Check p95 latency
          results.targetsTotal++;
          if (measurementResults.p95Latency < targets.p95Latency) {
            results.targetsMet++;
            console.log(`   ✅ P95 latency: ${measurementResults.p95Latency.toFixed(3)}ms (target: < ${targets.p95Latency}ms)`);
          } else {
            results.failures.push(`P95 latency ${measurementResults.p95Latency.toFixed(3)}ms exceeds target ${targets.p95Latency}ms`);
            console.log(`   ❌ P95 latency: ${measurementResults.p95Latency.toFixed(3)}ms (target: < ${targets.p95Latency}ms)`);
          }

          // Check throughput
          results.targetsTotal++;
          if (measurementResults.throughput > targets.throughput) {
            results.targetsMet++;
            console.log(`   ✅ Throughput: ${measurementResults.throughput.toFixed(0)} ops/sec (target: > ${targets.throughput} ops/sec)`);
          } else {
            results.failures.push(`Throughput ${measurementResults.throughput.toFixed(0)} below target ${targets.throughput}`);
            console.log(`   ❌ Throughput: ${measurementResults.throughput.toFixed(0)} ops/sec (target: > ${targets.throughput} ops/sec)`);
          }

          // Check memory overhead
          results.targetsTotal++;
          if (memoryResults.per1kHooks < targets.memoryOverhead) {
            results.targetsMet++;
            console.log(`   ✅ Memory per 1k hooks: ${memoryResults.per1kHooks.toFixed(2)} MB (target: < ${targets.memoryOverhead} MB)`);
          } else {
            results.failures.push(`Memory per 1k hooks ${memoryResults.per1kHooks.toFixed(2)}MB exceeds target ${targets.memoryOverhead}MB`);
            console.log(`   ❌ Memory per 1k hooks: ${memoryResults.per1kHooks.toFixed(2)} MB (target: < ${targets.memoryOverhead} MB)`);
          }

          // Overall status
          if (results.targetsMet < results.targetsTotal) {
            results.status = 'FAIL';
          }

          span.setAttribute('validation.status', results.status);
          span.setAttribute('validation.targetsMet', results.targetsMet);
          span.setAttribute('validation.targetsTotal', results.targetsTotal);
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();

          return results;
        });

        // ====================
        // FINAL RESULTS
        // ====================
        console.log('');
        console.log('=' .repeat(60));
        console.log(`📊 BENCHMARK RESULTS: ${validation.status === 'PASS' ? '✅ PASS' : '❌ FAIL'}`);
        console.log('=' .repeat(60));
        console.log(`Targets Met: ${validation.targetsMet}/${validation.targetsTotal}`);
        console.log('');

        if (validation.failures.length > 0) {
          console.log('Failures:');
          validation.failures.forEach(failure => console.log(`  - ${failure}`));
          console.log('');
        }

        const finalResults = {
          benchmarkId: BENCHMARK_CONFIG.id,
          scenario: BENCHMARK_CONFIG.scenario,
          timestamp: new Date().toISOString(),
          results: {
            ...measurementResults,
            memory: memoryResults
          },
          validation,
          targets: BENCHMARK_CONFIG.expectedTargets
        };

        // Set final root span attributes
        rootSpan.setAttribute('benchmark.status', validation.status);
        rootSpan.setAttribute('benchmark.validation.targetsMet', validation.targetsMet);
        rootSpan.setAttribute('benchmark.validation.targetsTotal', validation.targetsTotal);
        rootSpan.setStatus({
          code: validation.status === 'PASS' ? SpanStatusCode.OK : SpanStatusCode.ERROR
        });
        rootSpan.end();

        return finalResults;

      } catch (error) {
        rootSpan.recordException(error);
        rootSpan.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });
        rootSpan.end();
        throw error;
      }
    }
  );
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runHookRegistrationBenchmark()
    .then(results => {
      console.log('');
      console.log('Full results:', JSON.stringify(results, null, 2));
      process.exit(results.validation.status === 'PASS' ? 0 : 1);
    })
    .catch(error => {
      console.error('Benchmark failed:', error);
      process.exit(1);
    });
}
