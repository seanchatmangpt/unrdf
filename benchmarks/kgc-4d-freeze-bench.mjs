/**
 * KGC-4D Freeze Operation Benchmark
 * Measures performance of universe snapshot/freeze operations
 */
import { performance } from 'perf_hooks';
import { KGCStore, GitBackbone, freezeUniverse } from '../packages/kgc-4d/src/index.mjs';
import { dataFactory } from '../packages/oxigraph/src/index.mjs';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

const { namedNode, literal } = dataFactory;

/**
 * Benchmark configuration
 */
const config = {
  iterations: 100,
  eventsPerIteration: 10,
  warmupIterations: 10,
};

/**
 * Measure freeze operation latency
 */
async function benchmarkFreeze() {
  const results = {
    freezeLatencies: [],
    eventAppendLatencies: [],
    totalEvents: 0,
    totalFreezes: 0,
  };

  // Warmup - skip freeze (requires git setup)
  console.log(`Warming up (${config.warmupIterations} iterations)...`);
  const warmupStore = new KGCStore();
  for (let i = 0; i < config.warmupIterations; i++) {
    await warmupStore.appendEvent({ type: 'INIT', payload: { warmup: true } });
  }

  // Actual benchmark
  console.log(`Running freeze benchmark (${config.iterations} iterations)...`);

  // Create temporary directory for git repos
  const tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'kgc-freeze-bench-'));

  try {
    for (let i = 0; i < config.iterations; i++) {
      const store = new KGCStore();
      const repoPath = path.join(tmpDir, `repo-${i}`);
      const gitBackbone = new GitBackbone(repoPath);

      // Append events
      const appendStart = performance.now();
      for (let e = 0; e < config.eventsPerIteration; e++) {
        const delta = {
          type: 'add',
          subject: namedNode(`http://example.org/entity${i}_${e}`),
          predicate: namedNode('http://example.org/prop'),
          object: literal(`value_${i}_${e}`)
        };
        await store.appendEvent({ type: 'UPDATE', payload: { iteration: i, event: e } }, [delta]);
        results.totalEvents++;
      }
      const appendElapsed = performance.now() - appendStart;
      results.eventAppendLatencies.push(appendElapsed / config.eventsPerIteration);

      // Freeze operation
      const freezeStart = performance.now();
      await freezeUniverse(store, gitBackbone);
      const freezeElapsed = performance.now() - freezeStart;

      results.freezeLatencies.push(freezeElapsed);
      results.totalFreezes++;
    }
  } finally {
    // Cleanup temporary directory
    await fs.rm(tmpDir, { recursive: true, force: true });
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

  return {
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    median: sorted[Math.floor(sorted.length / 2)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

/**
 * Main benchmark runner
 */
async function main() {
  console.log('='.repeat(60));
  console.log('KGC-4D Freeze Performance Benchmark');
  console.log('='.repeat(60));
  console.log(`Iterations: ${config.iterations}`);
  console.log(`Events per freeze: ${config.eventsPerIteration}`);
  console.log('');

  const startTime = performance.now();
  const results = await benchmarkFreeze();
  const totalTime = performance.now() - startTime;

  const freezeStats = calculateStats(results.freezeLatencies);
  const appendStats = calculateStats(results.eventAppendLatencies);

  console.log('\n' + '='.repeat(60));
  console.log('FREEZE OPERATION PERFORMANCE');
  console.log('='.repeat(60));
  console.log(`Min:    ${freezeStats.min.toFixed(2)} ms`);
  console.log(`Mean:   ${freezeStats.mean.toFixed(2)} ms`);
  console.log(`Median: ${freezeStats.median.toFixed(2)} ms`);
  console.log(`P95:    ${freezeStats.p95.toFixed(2)} ms`);
  console.log(`P99:    ${freezeStats.p99.toFixed(2)} ms`);
  console.log(`Max:    ${freezeStats.max.toFixed(2)} ms`);

  console.log('\n' + '='.repeat(60));
  console.log('EVENT APPEND PERFORMANCE (per event)');
  console.log('='.repeat(60));
  console.log(`Min:    ${appendStats.min.toFixed(2)} ms`);
  console.log(`Mean:   ${appendStats.mean.toFixed(2)} ms`);
  console.log(`Median: ${appendStats.median.toFixed(2)} ms`);
  console.log(`P95:    ${appendStats.p95.toFixed(2)} ms`);
  console.log(`P99:    ${appendStats.p99.toFixed(2)} ms`);
  console.log(`Max:    ${appendStats.max.toFixed(2)} ms`);

  console.log('\n' + '='.repeat(60));
  console.log('SUMMARY');
  console.log('='.repeat(60));
  console.log(`Total events appended: ${results.totalEvents}`);
  console.log(`Total freezes: ${results.totalFreezes}`);
  console.log(`Total time: ${(totalTime / 1000).toFixed(2)}s`);
  console.log(`Throughput: ${(results.totalEvents / (totalTime / 1000)).toFixed(0)} events/sec`);
  console.log('');
}

main().catch(console.error);
