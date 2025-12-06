/**
 * Vector Engine Worker Performance Demo
 *
 * Compares WASM-style Worker execution vs main thread for vector operations
 * Demonstrates non-blocking performance for HDIT coordinate operations
 *
 * Key metrics:
 * - Operation latency (main thread vs Worker)
 * - Throughput for batch operations
 * - Memory transfer overhead
 */

import {
  coordsForEvent,
  batchCoordsForEvents,
  createUniverseContext,
  cosineSimilarity as mainThreadCosine,
  calculateCentroid as mainThreadCentroid,
  findKNearest as mainThreadKNearest,
  D_HEAVY,
} from '../src/hdit/index.mjs';
import { VectorEngineClient } from '../src/hdit/vector-engine-client.mjs';
import { EVENT_TYPES } from '../src/constants.mjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Generate synthetic events for benchmarking
 */
function generateSyntheticEvents(count) {
  const events = [];
  const baseTime = Date.now() * 1e6;

  for (let i = 0; i < count; i++) {
    events.push({
      type: EVENT_TYPES.CREATE,
      timestamp: baseTime + i * 1000000,
      vectorClock: {
        nodeId: `node-${i % 10}`,
        counters: { [`node-${i % 10}`]: String(i + 1) },
      },
      payload: {
        description: `Event ${i}`,
        domain: ['ai', 'backend', 'frontend', 'data'][i % 4],
      },
      mutations: [],
    });
  }

  return events;
}

/**
 * Measure execution time
 */
async function measure(name, fn) {
  const start = performance.now();
  const result = await fn();
  const elapsed = performance.now() - start;
  return { name, elapsed, result };
}

// ============================================================================
// Setup
// ============================================================================

console.log('🚀 Vector Engine Worker Performance Demo\n');
console.log('Comparing WASM-style Worker vs main thread execution\n');

// Generate test data
console.log('📊 Generating test data...\n');
const numEvents = 1000;
const dimension = D_HEAVY; // 512D

const events = generateSyntheticEvents(numEvents);
const universe = createUniverseContext(events);

console.log(`   Events: ${numEvents}`);
console.log(`   Dimension: ${dimension}D`);
console.log(`   Memory per vector: ${(dimension * 4 / 1024).toFixed(2)}KB`);
console.log(`   Total memory: ${(numEvents * dimension * 4 / 1024 / 1024).toFixed(2)}MB\n`);

// Generate coordinates (this happens on main thread in both cases)
console.log('🔢 Generating 512D coordinates for all events...\n');
const coordsStart = performance.now();
const coords = batchCoordsForEvents(events, universe, dimension);
const coordsElapsed = performance.now() - coordsStart;
console.log(`   Coordinate generation: ${coordsElapsed.toFixed(2)}ms\n`);

// ============================================================================
// Benchmark 1: Cosine Similarity (pairwise)
// ============================================================================

console.log('📏 Benchmark 1: Cosine Similarity (100 pairs)\n');

const pairs = 100;
const pairIndices = Array.from({ length: pairs }, (_, i) => [
  Math.floor(Math.random() * coords.length),
  Math.floor(Math.random() * coords.length),
]);

// Main thread
const mainCosineMeasure = await measure('Main Thread Cosine', async () => {
  const results = [];
  for (const [i, j] of pairIndices) {
    results.push(mainThreadCosine(coords[i], coords[j]));
  }
  return results;
});

console.log(`   Main Thread: ${mainCosineMeasure.elapsed.toFixed(2)}ms`);
console.log(`   Per operation: ${(mainCosineMeasure.elapsed / pairs).toFixed(3)}ms\n`);

// Worker (initialize)
const workerURL = new URL('../src/hdit/vector-engine.worker.mjs', import.meta.url);
const workerClient = new VectorEngineClient(workerURL);
await workerClient.waitReady();

const workerCosineMeasure = await measure('Worker Cosine', async () => {
  const results = [];
  for (const [i, j] of pairIndices) {
    results.push(await workerClient.cosineSimilarity(coords[i], coords[j]));
  }
  return results;
});

console.log(`   Worker: ${workerCosineMeasure.elapsed.toFixed(2)}ms`);
console.log(`   Per operation: ${(workerCosineMeasure.elapsed / pairs).toFixed(3)}ms`);

const cosineSpeedup = mainCosineMeasure.elapsed / workerCosineMeasure.elapsed;
console.log(`   Speedup: ${cosineSpeedup.toFixed(2)}x ${cosineSpeedup > 1 ? '(Worker faster)' : '(Main thread faster)'}\n`);

// ============================================================================
// Benchmark 2: Centroid Calculation
// ============================================================================

console.log('📐 Benchmark 2: Centroid Calculation (1000 vectors)\n');

// Main thread
const mainCentroidMeasure = await measure('Main Thread Centroid', async () => {
  return mainThreadCentroid(coords);
});

console.log(`   Main Thread: ${mainCentroidMeasure.elapsed.toFixed(2)}ms\n`);

// Worker
const workerCentroidMeasure = await measure('Worker Centroid', async () => {
  return await workerClient.centroid(coords);
});

console.log(`   Worker: ${workerCentroidMeasure.elapsed.toFixed(2)}ms`);

const centroidSpeedup = mainCentroidMeasure.elapsed / workerCentroidMeasure.elapsed;
console.log(`   Speedup: ${centroidSpeedup.toFixed(2)}x ${centroidSpeedup > 1 ? '(Worker faster)' : '(Main thread faster)'}\n`);

// Verify results match
const centroidDiff = mainCentroidMeasure.result.reduce((sum, val, i) =>
  sum + Math.abs(val - workerCentroidMeasure.result[i]), 0
);
console.log(`   Result difference: ${centroidDiff.toExponential(2)} (should be ~0)\n`);

// ============================================================================
// Benchmark 3: K-Nearest Neighbors
// ============================================================================

console.log('🔍 Benchmark 3: K-Nearest Neighbors (k=10)\n');

const query = coords[0];
const candidates = coords.slice(1, 101); // 100 candidates
const k = 10;

// Main thread
const mainKNNMeasure = await measure('Main Thread KNN', async () => {
  return mainThreadKNearest(query, candidates, k, 'cosine');
});

console.log(`   Main Thread: ${mainKNNMeasure.elapsed.toFixed(2)}ms\n`);

// Worker
const workerKNNMeasure = await measure('Worker KNN', async () => {
  return await workerClient.kNearest(query, candidates, k);
});

console.log(`   Worker: ${workerKNNMeasure.elapsed.toFixed(2)}ms`);

const knnSpeedup = mainKNNMeasure.elapsed / workerKNNMeasure.elapsed;
console.log(`   Speedup: ${knnSpeedup.toFixed(2)}x ${knnSpeedup > 1 ? '(Worker faster)' : '(Main thread faster)'}\n`);

// Verify results match
const knnMatch = mainKNNMeasure.result.every((item, i) =>
  item.index === workerKNNMeasure.result[i].index
);
console.log(`   Results match: ${knnMatch ? '✅' : '❌'}\n`);

// ============================================================================
// Summary
// ============================================================================

console.log('\n🌟 Performance Summary:\n');

console.log('1. WORKER OVERHEAD:');
console.log('   - Message passing adds latency for small operations');
console.log('   - Best for batch operations or CPU-intensive work');
console.log('   - Non-blocking: main thread free during computation\n');

console.log('2. WHEN TO USE WORKER:');
console.log('   ✅ Batch centroid calculation (>100 vectors)');
console.log('   ✅ Large k-NN searches (>1000 candidates)');
console.log('   ✅ Keeping UI responsive during computation');
console.log('   ❌ Single pairwise operations (too much overhead)\n');

console.log('3. WASM-STYLE PATTERNS:');
console.log('   - Loop unrolling (4x) for JIT optimization');
console.log('   - Contiguous Float32Array (cache-friendly)');
console.log('   - Fused operations (dot product, squared distance)');
console.log('   - Avoid sqrt (use squared distance for comparisons)\n');

console.log('4. MEMORY TRANSFER:');
const transferSize = (coords.length * dimension * 4) / 1024 / 1024;
console.log(`   - ${transferSize.toFixed(2)}MB transferred to Worker`);
console.log('   - Use SharedArrayBuffer for zero-copy (advanced)\n');

console.log('5. INTEGRATION WITH HDIT:');
console.log('   - coordsForEvent runs on main thread (fast)');
console.log('   - Vector operations offload to Worker (optional)');
console.log('   - Same API, different execution context\n');

// Cleanup
workerClient.terminate();

console.log('✅ Demo complete - Worker terminated\n');
