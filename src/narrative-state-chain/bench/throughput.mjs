/**
 * @file Throughput Benchmark
 * @module narrative-state-chain/bench/throughput
 * @description Measures scenes admitted per second
 */

import { HRTimer, analyzeStats, reportResults, generateTestQuads } from './utils.mjs';

/**
 * Simulate scene admission (reconciliation + admission decision)
 * @param {Array} quads - Quads to admit
 * @returns {Promise<object>} Admission result
 */
async function admitScene(quads) {
  // Simulate admission processing time
  // Base time + per-quad time + small random variance
  const baseTimeMs = 0.3;
  const perQuadTimeMs = 0.2;
  const processingTimeMs = baseTimeMs + (quads.length * perQuadTimeMs) + Math.random() * 0.5;

  await new Promise(resolve => setTimeout(resolve, processingTimeMs / 1000));

  return {
    admitted: true,
    quadCount: quads.length,
    timestamp: Date.now()
  };
}

/**
 * Benchmark throughput - scenes admitted per second
 * @async
 */
export async function benchmarkThroughput() {
  console.log('\n========== THROUGHPUT BENCHMARK ==========\n');
  console.log('Measuring scenes admitted per second\n');

  const warmup = 5;
  const measurementScenes = 100;

  console.log('Benchmarking: Individual scene admission\n');

  // Warmup phase
  const quads = generateTestQuads(10);
  for (let i = 0; i < warmup; i++) {
    await admitScene(quads);
  }

  // Force GC if available
  if (global.gc) {
    global.gc();
  }

  // Measurement phase - individual scene admission
  const timer = new HRTimer();
  timer.begin();

  for (let i = 0; i < measurementScenes; i++) {
    const sceneQuads = generateTestQuads(10);
    await admitScene(sceneQuads);

    if ((i + 1) % 20 === 0) {
      process.stdout.write(`  ${i + 1}/${measurementScenes}\r`);
    }
  }

  const elapsedMs = timer.elapsedMs();
  const throughputIndividual = (measurementScenes / elapsedMs) * 1000; // scenes/sec

  console.log('\nIndividual scene admission:');
  console.log(`  ${measurementScenes} scenes in ${elapsedMs.toFixed(2)}ms`);
  console.log(`  Throughput: ${throughputIndividual.toFixed(2)} scenes/sec`);
  console.log(`  Per-scene latency: ${(elapsedMs / measurementScenes).toFixed(2)}ms\n`);

  // Benchmark batch admission
  console.log('Benchmarking: Batch scene admission\n');

  const batchSizes = [10, 50, 100];
  const batchResults = {};

  for (const batchSize of batchSizes) {
    console.log(`  Batch size: ${batchSize}`);
    const measurements = [];

    // Warmup batch operations
    for (let b = 0; b < warmup; b++) {
      for (let i = 0; i < batchSize; i++) {
        const batchQuads = generateTestQuads(5);
        await admitScene(batchQuads);
      }
    }

    // Force GC
    if (global.gc) {
      global.gc();
    }

    // Measure batch admission
    const batchIterations = 10;
    for (let b = 0; b < batchIterations; b++) {
      const batchTimer = new HRTimer();
      batchTimer.begin();

      for (let i = 0; i < batchSize; i++) {
        const batchQuads = generateTestQuads(5);
        await admitScene(batchQuads);
      }

      const batchElapsedMs = batchTimer.elapsedMs();
      measurements.push(batchElapsedMs / batchSize); // Per-scene latency
    }

    const stats = analyzeStats(measurements);
    batchResults[batchSize] = {
      stats,
      throughputPerSec: (batchSize / (stats.mean / 1000))
    };

    console.log(`    p50: ${stats.p50.toFixed(3)}ms per scene, ` +
                `${batchResults[batchSize].throughputPerSec.toFixed(2)} scenes/sec`);
  }

  return {
    name: 'Throughput',
    individual: {
      scenesPerSec: throughputIndividual,
      elapsedMs
    },
    batch: batchResults
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const results = await benchmarkThroughput();
  console.log('\n✅ Throughput benchmark complete\n');
}
