/**
 * Receipt Generation Performance Benchmark
 * Measures cryptographic receipt generation and verification
 */
import { performance } from 'perf_hooks';
import { KGCStore } from '../packages/kgc-4d/src/index.mjs';
import { dataFactory } from '../packages/oxigraph/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

/**
 * Benchmark configuration
 */
const config = {
  iterations: 1000,
  warmupIterations: 100,
};

/**
 * Benchmark receipt generation
 */
async function benchmarkReceiptGeneration() {
  const results = {
    generationLatencies: [],
    totalReceipts: 0,
  };

  // Warmup
  console.log(`Warming up (${config.warmupIterations} iterations)...`);
  const warmupStore = new KGCStore();
  for (let i = 0; i < config.warmupIterations; i++) {
    await warmupStore.appendEvent({ type: 'WARMUP', payload: { iteration: i } });
  }

  // Actual benchmark
  console.log(`Running receipt generation benchmark (${config.iterations} iterations)...`);
  const store = new KGCStore();

  for (let i = 0; i < config.iterations; i++) {
    const delta = {
      type: 'add',
      subject: namedNode(`http://example.org/entity${i}`),
      predicate: namedNode('http://example.org/prop'),
      object: literal(`value_${i}`)
    };

    const start = performance.now();
    const receipt = await store.appendEvent({ type: 'UPDATE', payload: { iteration: i } }, [delta]);
    const elapsed = performance.now() - start;

    results.generationLatencies.push(elapsed);
    results.totalReceipts++;

    // Verify receipt has required fields
    if (!receipt || !receipt.receipt || !receipt.receipt.id) {
      throw new Error(`Invalid receipt at iteration ${i}`);
    }
  }

  return results;
}

/**
 * Calculate statistics
 */
function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  // Calculate standard deviation
  const variance = values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
  const stddev = Math.sqrt(variance);

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    stddev,
    median: sorted[Math.floor(sorted.length / 2)],
    p50: sorted[Math.floor(sorted.length * 0.50)],
    p90: sorted[Math.floor(sorted.length * 0.90)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
    p999: sorted[Math.floor(sorted.length * 0.999)],
  };
}

/**
 * Main benchmark runner
 */
async function main() {
  console.log('='.repeat(60));
  console.log('Receipt Generation Performance Benchmark');
  console.log('='.repeat(60));
  console.log(`Iterations: ${config.iterations}`);
  console.log('');

  const startTime = performance.now();
  const results = await benchmarkReceiptGeneration();
  const totalTime = performance.now() - startTime;

  const stats = calculateStats(results.generationLatencies);

  console.log('\n' + '='.repeat(60));
  console.log('RECEIPT GENERATION LATENCY');
  console.log('='.repeat(60));
  console.log(`Min:       ${stats.min.toFixed(3)} ms`);
  console.log(`Mean:      ${stats.mean.toFixed(3)} ms`);
  console.log(`Median:    ${stats.median.toFixed(3)} ms`);
  console.log(`Stddev:    ${stats.stddev.toFixed(3)} ms`);
  console.log(`P90:       ${stats.p90.toFixed(3)} ms`);
  console.log(`P95:       ${stats.p95.toFixed(3)} ms`);
  console.log(`P99:       ${stats.p99.toFixed(3)} ms`);
  console.log(`P99.9:     ${stats.p999.toFixed(3)} ms`);
  console.log(`Max:       ${stats.max.toFixed(3)} ms`);

  console.log('\n' + '='.repeat(60));
  console.log('SUMMARY');
  console.log('='.repeat(60));
  console.log(`Total receipts generated: ${results.totalReceipts}`);
  console.log(`Total time: ${(totalTime / 1000).toFixed(2)}s`);
  console.log(`Throughput: ${(results.totalReceipts / (totalTime / 1000)).toFixed(0)} receipts/sec`);

  // Performance claims validation
  console.log('\n' + '='.repeat(60));
  console.log('PERFORMANCE CLAIMS VALIDATION');
  console.log('='.repeat(60));
  console.log(`Target: <10ms per receipt`);
  console.log(`Actual P95: ${stats.p95.toFixed(3)} ms`);
  console.log(`Status: ${stats.p95 < 10 ? 'PASS' : 'FAIL'}`);
  console.log('');
}

main().catch(console.error);
