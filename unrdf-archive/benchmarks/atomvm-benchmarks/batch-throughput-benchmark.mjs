/**
 * Batch Throughput Benchmark
 *
 * Measures batching performance using TripleStreamBatcher.
 *
 * Method:
 * - Uses TripleStreamBatcher
 * - Measures triples/second at different batch sizes
 * - Finds optimal batch size
 * - Outputs recommendations
 */

import { dataFactory } from '../../oxigraph/src/index.mjs';
import { TripleStreamBatcher } from '../src/triple-stream-batcher.mjs';

const { namedNode, literal } = dataFactory;

/**
 * Format number with thousands separator
 * @param {number} num - Number to format
 * @returns {string} Formatted number
 */
function formatNumber(num) {
  return num.toLocaleString('en-US', { maximumFractionDigits: 2 });
}

/**
 * Benchmark a specific batch size
 * @param {number} batchSize - Batch size to test
 * @param {number} totalTriples - Total triples to process
 * @returns {Promise<object>} Benchmark results
 */
async function benchmarkBatchSize(batchSize, totalTriples) {
  return new Promise((resolve, reject) => {
    const batcher = new TripleStreamBatcher({
      batchSize: batchSize,
      timeout: 50,
    });

    const batches = [];
    let processedCount = 0;

    // Register batch handler
    batcher.onBatch(async (batch) => {
      const batchStart = performance.now();

      // Simulate processing (in real scenario, this would be store insertion)
      await new Promise(resolve => setImmediate(resolve));

      const batchEnd = performance.now();
      const batchLatency = batchEnd - batchStart;

      batches.push({
        size: batch.length,
        latency: batchLatency
      });

      processedCount += batch.length;

      return { success: true };
    });

    // Create test triples
    const triples = [];
    for (let i = 0; i < totalTriples; i++) {
      const subject = namedNode(`http://example.org/subject${i}`);
      const predicate = namedNode(`http://example.org/predicate${i % 10}`);
      const object = literal(`value_${i}`);
      triples.push({ subject, predicate, object });
    }

    // Start benchmark
    const benchmarkStart = performance.now();

    // Add all triples
    try {
      for (const triple of triples) {
        batcher.addTriple(triple);
      }

      // Flush and wait for completion
      batcher.flush().then(() => {
        const benchmarkEnd = performance.now();
        const totalTime = benchmarkEnd - benchmarkStart;
        const throughput = (totalTriples / totalTime) * 1000;

        const metrics = batcher.getMetrics();

        // Calculate batch latency statistics
        const batchLatencies = batches.map(b => b.latency);
        const avgBatchLatency = batchLatencies.length > 0
          ? batchLatencies.reduce((a, b) => a + b, 0) / batchLatencies.length
          : 0;

        // Calculate percentiles
        const sorted = [...batchLatencies].sort((a, b) => a - b);
        const p50 = sorted[Math.floor(sorted.length * 0.5)] || 0;
        const p99 = sorted[Math.floor(sorted.length * 0.99)] || 0;

        batcher.destroy();

        resolve({
          batchSize: batchSize,
          totalTriples: processedCount,
          totalTime: totalTime,
          throughput: throughput,
          batchCount: batches.length,
          avgBatchLatency: avgBatchLatency,
          p50BatchLatency: p50,
          p99BatchLatency: p99,
          backpressureEvents: metrics.backpressureEvents,
          metrics: metrics
        });
      }).catch(reject);
    } catch (error) {
      batcher.destroy();
      reject(error);
    }
  });
}

/**
 * Main benchmark function
 */
async function runBenchmark() {
  console.log('\n=== Batch Throughput Benchmark ===\n');

  const totalTriples = 10000;
  const batchSizes = [10, 50, 100, 200, 500, 1000];

  console.log(`Testing ${totalTriples} triples with different batch sizes...\n`);

  const results = [];

  for (const batchSize of batchSizes) {
    console.log(`Benchmarking batch size ${batchSize}...`);

    const result = await benchmarkBatchSize(batchSize, totalTriples);

    results.push(result);

    console.log(`  Throughput: ${formatNumber(result.throughput)} triples/sec`);
    console.log(`  Batches processed: ${result.batchCount}`);
    console.log(`  Average batch latency: ${result.avgBatchLatency.toFixed(3)}ms`);
    console.log(`  Backpressure events: ${result.backpressureEvents}`);
    console.log('');
  }

  // Find optimal batch size
  const optimal = results.reduce((best, current) => {
    return current.throughput > best.throughput ? current : best;
  }, results[0]);

  // Print detailed results
  console.log('=== RESULTS ===\n');

  console.log('Batch Size Performance:');
  console.log('┌─────────────┬──────────────────┬───────────┬──────────────────┬─────────────────┐');
  console.log('│ Batch Size  │ Throughput       │ Batches   │ Avg Batch Lat.   │ P99 Batch Lat.  │');
  console.log('├─────────────┼──────────────────┼───────────┼──────────────────┼─────────────────┤');

  for (const result of results) {
    const isOptimal = result.batchSize === optimal.batchSize;
    const marker = isOptimal ? ' ★' : '  ';
    const bs = String(result.batchSize).padStart(9);
    const tp = formatNumber(result.throughput).padStart(14);
    const bc = String(result.batchCount).padStart(7);
    const al = result.avgBatchLatency.toFixed(3).padStart(14);
    const p99 = result.p99BatchLatency.toFixed(3).padStart(13);

    console.log(`│${marker}${bs} │ ${tp} t/s │ ${bc}   │ ${al} ms │ ${p99} ms │`);
  }

  console.log('└─────────────┴──────────────────┴───────────┴──────────────────┴─────────────────┘');
  console.log('(★ = optimal batch size)\n');

  console.log('Optimal Configuration:');
  console.log(`- Batch Size: ${optimal.batchSize}`);
  console.log(`- Throughput: ${formatNumber(optimal.throughput)} triples/sec`);
  console.log(`- Average Batch Latency: ${optimal.avgBatchLatency.toFixed(3)}ms`);
  console.log(`- P50 Batch Latency: ${optimal.p50BatchLatency.toFixed(3)}ms`);
  console.log(`- P99 Batch Latency: ${optimal.p99BatchLatency.toFixed(3)}ms`);
  console.log(`- Batches Processed: ${optimal.batchCount}`);
  console.log(`- Total Time: ${optimal.totalTime.toFixed(2)}ms`);

  // Calculate efficiency vs baseline
  const baseline = results.find(r => r.batchSize === 100); // Default batch size
  if (baseline && optimal.batchSize !== 100) {
    const improvement = ((optimal.throughput - baseline.throughput) / baseline.throughput) * 100;
    console.log(`\nImprovement over default (batch size 100):`);
    console.log(`- Throughput gain: ${improvement > 0 ? '+' : ''}${improvement.toFixed(1)}%`);
  }

  // Analyze scaling behavior
  console.log('\nScaling Analysis:');
  const small = results.find(r => r.batchSize === 10);
  const large = results.find(r => r.batchSize === 1000);

  if (small && large) {
    const scalingFactor = large.throughput / small.throughput;
    console.log(`- Throughput scaling (10 → 1000): ${scalingFactor.toFixed(2)}x`);

    const latencyScaling = large.avgBatchLatency / small.avgBatchLatency;
    console.log(`- Batch latency scaling (10 → 1000): ${latencyScaling.toFixed(2)}x`);
  }

  // Performance target validation
  console.log('\n=== Performance Target Validation ===');
  const targetThroughput = 10000;
  const meetsTarget = optimal.throughput >= targetThroughput;
  console.log(`- Target: >= ${formatNumber(targetThroughput)} triples/sec`);
  console.log(`- Actual: ${formatNumber(optimal.throughput)} triples/sec`);
  console.log(`- Status: ${meetsTarget ? '✅ PASS' : '❌ FAIL'}`);

  // Recommendations
  console.log('\n=== Recommendations ===');
  console.log(`1. Use batch size: ${optimal.batchSize} for maximum throughput`);

  if (optimal.batchSize > 500) {
    console.log('2. Large batch size may increase memory usage');
    console.log('3. Consider batch size 200-500 for balanced performance/memory');
  } else if (optimal.batchSize < 50) {
    console.log('2. Small batch size may increase overhead');
    console.log('3. Consider batch size 100-200 unless latency is critical');
  } else {
    console.log('2. Current optimal batch size is well-balanced');
  }

  const hasBackpressure = results.some(r => r.backpressureEvents > 0);
  if (hasBackpressure) {
    console.log('4. Backpressure detected - consumer may be slower than producer');
    console.log('5. Consider increasing batch processing parallelism');
  }

  console.log('\n===============================\n');
}

// Run benchmark
runBenchmark().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
