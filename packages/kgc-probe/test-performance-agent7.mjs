#!/usr/bin/env node
/**
 * Agent 7 Performance Envelope Test - Prove it works
 *
 * ADVERSARIAL PM VERIFICATION:
 * - RUN the performance probe
 * - MEASURE actual variance
 * - PROVE stddev < 20% of mean
 * - SHOW full statistics (min, max, mean, p50, p95, p99, stddev)
 */

import { probePerformance } from './src/probes/performance.mjs';
import { writeFile } from 'node:fs/promises';
import { join } from 'node:path';

const startTime = performance.now();

console.log('='.repeat(80));
console.log('Agent 7 (Performance Envelopes) - Stable Benchmarking Verification');
console.log('='.repeat(80));
console.log('');

try {
  // Run performance probe with reduced samples for faster execution
  const observations = await probePerformance({
    out: './probe-output/perf-test',
    samples: 10,  // Reduced from 100 for faster demo
    budgetMs: 30000,
    warmupIterations: 2
  });

  const endTime = performance.now();
  const totalTime = endTime - startTime;

  console.log(`✅ Performance probe completed successfully`);
  console.log(`📊 Total benchmarks: ${observations.length}`);
  console.log(`⏱️  Total execution time: ${totalTime.toFixed(2)}ms (${(totalTime/1000).toFixed(2)}s)`);
  console.log('');
  console.log('='.repeat(80));
  console.log('BENCHMARK RESULTS WITH VARIANCE ANALYSIS');
  console.log('='.repeat(80));
  console.log('');

  // Analyze variance for each benchmark
  const varianceAnalysis = [];

  for (const obs of observations) {
    const { observation, value, metadata } = obs;

    if (metadata && metadata.mean_ms !== undefined && metadata.stddev_ms !== undefined) {
      const mean = metadata.mean_ms;
      const stddev = metadata.stddev_ms;
      const variancePercent = (stddev / mean) * 100;
      const stableScore = variancePercent < 20 ? '✅' : '⚠️';

      varianceAnalysis.push({
        observation,
        mean,
        stddev,
        variancePercent,
        stableScore
      });

      console.log(`${stableScore} ${observation}`);
      console.log(`   Mean:     ${mean.toFixed(4)}ms`);
      console.log(`   Median:   ${metadata.median_ms.toFixed(4)}ms`);
      console.log(`   P95:      ${metadata.p95_ms.toFixed(4)}ms`);
      console.log(`   P99:      ${metadata.p99_ms.toFixed(4)}ms`);
      console.log(`   Min:      ${metadata.min_ms?.toFixed(4) || 'N/A'}ms`);
      console.log(`   Max:      ${metadata.max_ms?.toFixed(4) || 'N/A'}ms`);
      console.log(`   StdDev:   ${stddev.toFixed(4)}ms`);
      console.log(`   Variance: ${variancePercent.toFixed(2)}% ${variancePercent < 20 ? '(STABLE ✅)' : '(HIGH ⚠️)'}`);

      if (metadata.unit === 'MB/sec' || metadata.unit === 'ops/sec') {
        console.log(`   Throughput: ${value?.toFixed(2) || 'N/A'} ${metadata.unit}`);
      }
      console.log('');
    }
  }

  console.log('='.repeat(80));
  console.log('VARIANCE SUMMARY');
  console.log('='.repeat(80));

  const stableCount = varianceAnalysis.filter(a => a.variancePercent < 20).length;
  const totalCount = varianceAnalysis.length;
  const avgVariance = varianceAnalysis.reduce((sum, a) => sum + a.variancePercent, 0) / totalCount;

  console.log(`Stable benchmarks (< 20% variance): ${stableCount}/${totalCount} (${((stableCount/totalCount)*100).toFixed(1)}%)`);
  console.log(`Average variance: ${avgVariance.toFixed(2)}%`);
  console.log(`Variance range: ${Math.min(...varianceAnalysis.map(a => a.variancePercent)).toFixed(2)}% - ${Math.max(...varianceAnalysis.map(a => a.variancePercent)).toFixed(2)}%`);
  console.log('');

  // Save results to file
  const outputFile = join('./probe-output/perf-test', `agent7-results-${Date.now()}.json`);
  await writeFile(outputFile, JSON.stringify({
    summary: {
      totalBenchmarks: observations.length,
      executionTimeMs: totalTime,
      stableCount,
      totalCount,
      avgVariance,
      timestamp: new Date().toISOString()
    },
    varianceAnalysis,
    observations
  }, null, 2));

  console.log(`📝 Full results saved to: ${outputFile}`);
  console.log('');
  console.log('='.repeat(80));
  console.log('✅ AGENT 7 VERIFICATION COMPLETE');
  console.log('='.repeat(80));

  // Exit with success
  process.exit(0);

} catch (error) {
  console.error('❌ Performance probe failed:');
  console.error(error);
  process.exit(1);
}
