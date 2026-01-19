/**
 * @file Benchmark Command - Run Uptime Benchmarks
 * @module cli/commands/daemon/uptime/benchmark
 * @description CLI command for running uptime benchmarks with performance metrics
 *
 * @example
 * # Run 100 iterations with 10 concurrent workers
 * unrdf daemon uptime benchmark --iterations 100 --concurrency 10
 *
 * # Run benchmark with JSON output
 * unrdf daemon uptime benchmark --iterations 500 --json --output bench.json
 *
 * # Run benchmark with warmup iterations
 * unrdf daemon uptime benchmark --iterations 200 --warmup 10
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { BenchmarkArgsSchema } from './schemas.mjs';
import {
  generateId,
  formatDuration,
  calculateStats,
  formatBenchmarkAsCSV,
  writeOutput,
  printHeader,
  printSeparator,
} from './helpers.mjs';

/**
 * Simulated operation for benchmarking
 * Represents a typical uptime check operation
 * @returns {Promise<Object>} Operation result
 */
async function simulatedOperation() {
  const startTime = performance.now();

  // Simulate variable latency (5-50ms typical, occasional spikes)
  const baseLatency = 5 + Math.random() * 20;
  const spike = Math.random() > 0.95 ? Math.random() * 100 : 0;
  const latency = baseLatency + spike;

  await new Promise(resolve => setTimeout(resolve, Math.min(latency, 100)));

  const endTime = performance.now();
  const success = Math.random() > 0.001; // 99.9% success rate

  return {
    latency: endTime - startTime,
    success,
  };
}

/**
 * Run benchmark iterations with concurrency
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Benchmark results
 */
async function runBenchmark(options) {
  const { iterations, concurrency, warmup, onProgress } = options;

  const benchmarkId = generateId('bench');
  const startTime = Date.now();

  // Warmup phase
  if (warmup > 0) {
    if (onProgress) onProgress('warmup', { count: warmup });
    for (let i = 0; i < warmup; i++) {
      await simulatedOperation();
    }
  }

  // Main benchmark
  const latencies = [];
  let successCount = 0;
  let errorCount = 0;
  let completed = 0;

  // Process in batches based on concurrency
  const batches = Math.ceil(iterations / concurrency);

  for (let batch = 0; batch < batches; batch++) {
    const batchSize = Math.min(concurrency, iterations - completed);
    const promises = [];

    for (let i = 0; i < batchSize; i++) {
      promises.push(simulatedOperation());
    }

    const results = await Promise.all(promises);

    for (const result of results) {
      latencies.push(result.latency);
      if (result.success) {
        successCount++;
      } else {
        errorCount++;
      }
      completed++;
    }

    // Progress callback
    if (onProgress && batch % Math.max(1, Math.floor(batches / 10)) === 0) {
      onProgress('progress', {
        completed,
        total: iterations,
        percentage: Math.round((completed / iterations) * 100),
      });
    }
  }

  const endTime = Date.now();
  const totalDuration = endTime - startTime;

  // Calculate statistics
  const stats = calculateStats(latencies);
  const throughput = (iterations / totalDuration) * 1000; // ops/sec
  const errorRate = errorCount / iterations;

  return {
    benchmarkId,
    iterations,
    concurrency,
    warmup,
    completedAt: new Date().toISOString(),
    metrics: {
      totalDuration,
      avgLatency: stats.avg,
      minLatency: stats.min,
      maxLatency: stats.max,
      p50: stats.p50,
      p90: stats.p90,
      p95: stats.p95,
      p99: stats.p99,
      throughput,
      successCount,
      errorCount,
      errorRate,
    },
  };
}

/**
 * Format benchmark result as text
 * @param {Object} result - Benchmark result
 * @param {boolean} includePercentiles - Include percentile breakdown
 * @returns {string} Formatted text
 */
function formatTextBenchmark(result, includePercentiles = true) {
  const lines = [];
  const m = result.metrics;

  lines.push('\nUptime Benchmark Results');
  lines.push('='.repeat(60));
  lines.push(`Benchmark ID: ${result.benchmarkId}`);
  lines.push(`Completed: ${result.completedAt}`);
  lines.push('');

  lines.push('Configuration');
  lines.push('-'.repeat(60));
  lines.push(`Iterations: ${result.iterations}`);
  lines.push(`Concurrency: ${result.concurrency}`);
  lines.push(`Warmup: ${result.warmup}`);
  lines.push('');

  lines.push('Performance Metrics');
  lines.push('-'.repeat(60));
  lines.push(`Total Duration: ${formatDuration(m.totalDuration)}`);
  lines.push(`Throughput: ${m.throughput.toFixed(2)} ops/sec`);
  lines.push(`Average Latency: ${m.avgLatency.toFixed(3)}ms`);
  lines.push(`Min Latency: ${m.minLatency.toFixed(3)}ms`);
  lines.push(`Max Latency: ${m.maxLatency.toFixed(3)}ms`);
  lines.push('');

  if (includePercentiles) {
    lines.push('Latency Percentiles');
    lines.push('-'.repeat(60));
    lines.push(`P50 (Median): ${m.p50.toFixed(3)}ms`);
    lines.push(`P90: ${m.p90.toFixed(3)}ms`);
    lines.push(`P95: ${m.p95.toFixed(3)}ms`);
    lines.push(`P99: ${m.p99.toFixed(3)}ms`);
    lines.push('');
  }

  lines.push('Reliability');
  lines.push('-'.repeat(60));
  lines.push(`Success Count: ${m.successCount}`);
  lines.push(`Error Count: ${m.errorCount}`);
  lines.push(`Error Rate: ${(m.errorRate * 100).toFixed(4)}%`);
  lines.push('='.repeat(60));

  // Performance assessment
  lines.push('\nAssessment');
  lines.push('-'.repeat(60));

  const assessments = [];
  if (m.p99 < 100) assessments.push('P99 latency within SLA (<100ms)');
  else assessments.push('P99 latency exceeds SLA (>=100ms)');

  if (m.errorRate < 0.001) assessments.push('Error rate excellent (<0.1%)');
  else if (m.errorRate < 0.01) assessments.push('Error rate acceptable (<1%)');
  else assessments.push('Error rate needs improvement (>=1%)');

  if (m.throughput > 100) assessments.push('High throughput (>100 ops/sec)');
  else if (m.throughput > 50) assessments.push('Moderate throughput (50-100 ops/sec)');
  else assessments.push('Low throughput (<50 ops/sec)');

  assessments.forEach(a => lines.push(`  - ${a}`));

  return lines.join('\n');
}

export const benchmarkCommand = defineCommand({
  meta: {
    name: 'benchmark',
    description: 'Run uptime benchmarks with performance metrics',
  },
  args: {
    iterations: {
      type: 'string',
      description: 'Number of iterations (1-10000)',
      default: '100',
    },
    concurrency: {
      type: 'string',
      description: 'Concurrent operations (1-100)',
      default: '10',
    },
    output: {
      type: 'string',
      description: 'Output file path (default: stdout)',
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON',
      default: false,
    },
    warmup: {
      type: 'string',
      description: 'Warmup iterations before benchmark',
      default: '5',
    },
    'include-percentiles': {
      type: 'boolean',
      description: 'Include percentile breakdown in output',
      default: true,
    },
    csv: {
      type: 'boolean',
      description: 'Output as CSV',
      default: false,
    },
  },
  async run({ args }) {
    try {
      // Parse numeric args from strings
      const parsedArgs = {
        iterations: parseInt(args.iterations, 10),
        concurrency: parseInt(args.concurrency, 10),
        output: args.output,
        json: args.json,
        warmup: parseInt(args.warmup, 10),
        'include-percentiles': args['include-percentiles'],
      };

      const validated = BenchmarkArgsSchema.parse(parsedArgs);

      if (!validated.json && !args.csv) {
        printHeader('Uptime Benchmark');
        console.log(`Iterations: ${validated.iterations}`);
        console.log(`Concurrency: ${validated.concurrency}`);
        console.log(`Warmup: ${validated.warmup}`);
        printSeparator('-');
        console.log('Running benchmark...\n');
      }

      const result = await runBenchmark({
        iterations: validated.iterations,
        concurrency: validated.concurrency,
        warmup: validated.warmup,
        onProgress: !validated.json && !args.csv ? (phase, data) => {
          if (phase === 'warmup') {
            console.log(`Warming up with ${data.count} iterations...`);
          } else if (phase === 'progress') {
            process.stdout.write(`\rProgress: ${data.percentage}% (${data.completed}/${data.total})`);
          }
        } : null,
      });

      if (!validated.json && !args.csv) {
        console.log('\n'); // Clear progress line
      }

      let output;
      if (validated.json) {
        output = JSON.stringify(result, null, 2);
      } else if (args.csv) {
        output = formatBenchmarkAsCSV(result);
      } else {
        output = formatTextBenchmark(result, validated['include-percentiles']);
      }

      await writeOutput(output, validated.output);
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error(`Validation error: ${error.errors.map(e => e.message).join(', ')}`);
        console.error('\nUsage examples:');
        console.error('  unrdf daemon uptime benchmark --iterations 100 --concurrency 10');
        console.error('  unrdf daemon uptime benchmark --iterations 500 --json');
        console.error('  unrdf daemon uptime benchmark --iterations 200 --warmup 10 --output results.json');
      } else {
        console.error(`Error: ${error.message}`);
      }
      process.exit(1);
    }
  },
});
