/**
 * Batch Throughput Benchmark (Standalone Version)
 *
 * Demonstrates batching performance measurement methodology.
 * Uses simplified batching logic instead of actual TripleStreamBatcher.
 */

/**
 * Format number with thousands separator
 */
function formatNumber(num) {
  return num.toLocaleString('en-US', { maximumFractionDigits: 2 });
}

/**
 * Simple mock batcher
 */
class MockBatcher {
  constructor(batchSize) {
    this.batchSize = batchSize;
    this.queue = [];
    this.batches = [];
    this.callback = null;
  }

  onBatch(callback) {
    this.callback = callback;
  }

  async addTriple(triple) {
    this.queue.push(triple);

    if (this.queue.length >= this.batchSize) {
      await this.flush();
    }
  }

  async flush() {
    if (this.queue.length === 0 || !this.callback) return;

    const batch = this.queue.splice(0, this.batchSize);
    const batchStart = performance.now();

    await this.callback(batch);

    const batchEnd = performance.now();
    this.batches.push({
      size: batch.length,
      latency: batchEnd - batchStart
    });
  }
}

/**
 * Benchmark a specific batch size
 */
async function benchmarkBatchSize(batchSize, totalTriples) {
  const batcher = new MockBatcher(batchSize);

  // Register batch handler
  batcher.onBatch(async (batch) => {
    // Simulate processing overhead (0.1-0.5ms per batch)
    const overhead = 0.1 + Math.random() * 0.4;
    const start = performance.now();
    while (performance.now() - start < overhead) {
      // Busy wait
    }
    return { success: true };
  });

  // Create test triples
  const triples = [];
  for (let i = 0; i < totalTriples; i++) {
    triples.push({
      subject: `http://example.org/subject${i}`,
      predicate: `http://example.org/predicate${i % 10}`,
      object: `value_${i}`
    });
  }

  // Benchmark
  const benchmarkStart = performance.now();

  for (const triple of triples) {
    await batcher.addTriple(triple);
  }

  await batcher.flush();

  const benchmarkEnd = performance.now();
  const totalTime = benchmarkEnd - benchmarkStart;
  const throughput = (totalTriples / totalTime) * 1000;

  // Calculate batch statistics
  const batchLatencies = batcher.batches.map(b => b.latency);
  const avgBatchLatency = batchLatencies.length > 0
    ? batchLatencies.reduce((a, b) => a + b, 0) / batchLatencies.length
    : 0;

  const sorted = [...batchLatencies].sort((a, b) => a - b);
  const p50 = sorted[Math.floor(sorted.length * 0.5)] || 0;
  const p99 = sorted[Math.floor(sorted.length * 0.99)] || 0;

  return {
    batchSize,
    totalTriples,
    totalTime,
    throughput,
    batchCount: batcher.batches.length,
    avgBatchLatency,
    p50BatchLatency: p50,
    p99BatchLatency: p99
  };
}

/**
 * Main benchmark
 */
async function runBenchmark() {
  console.log('\n=== Batch Throughput Benchmark (Standalone Demo) ===\n');

  const totalTriples = 10000;
  const batchSizes = [10, 50, 100, 200, 500, 1000];

  console.log(`Testing ${totalTriples} triples with different batch sizes...\n`);

  const results = [];

  for (const batchSize of batchSizes) {
    console.log(`Benchmarking batch size ${batchSize}...`);

    const result = await benchmarkBatchSize(batchSize, totalTriples);
    results.push(result);

    console.log(`  Throughput: ${formatNumber(result.throughput)} triples/sec`);
    console.log(`  Batches: ${result.batchCount}`);
    console.log(`  Avg batch latency: ${result.avgBatchLatency.toFixed(3)}ms`);
    console.log('');
  }

  // Find optimal
  const optimal = results.reduce((best, current) =>
    current.throughput > best.throughput ? current : best,
    results[0]
  );

  // Print results table
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
  console.log(`- Total Time: ${optimal.totalTime.toFixed(2)}ms`);

  // Scaling analysis
  console.log('\nScaling Analysis:');
  const small = results.find(r => r.batchSize === 10);
  const large = results.find(r => r.batchSize === 1000);

  if (small && large) {
    const scalingFactor = large.throughput / small.throughput;
    console.log(`- Throughput scaling (10 → 1000): ${scalingFactor.toFixed(2)}x`);

    const latencyScaling = large.avgBatchLatency / small.avgBatchLatency;
    console.log(`- Batch latency scaling (10 → 1000): ${latencyScaling.toFixed(2)}x`);
  }

  console.log('\n=== Benchmark Methodology Demonstrated ===');
  console.log('✅ Testing multiple batch sizes');
  console.log('✅ Measuring throughput at each size');
  console.log('✅ Calculating batch latency statistics');
  console.log('✅ Finding optimal configuration');
  console.log('✅ Analyzing scaling behavior');

  console.log('\n===============================\n');
}

runBenchmark().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
