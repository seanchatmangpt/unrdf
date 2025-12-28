#!/usr/bin/env node
/**
 * V6 Delta Compression Benchmark
 *
 * Measures: Delta proposal size vs full state
 * Target: <10% of original state size
 * Benchmark: 10 operations on 1MB RDF graph, report delta sizes
 * Analyze: Is Merkle tree efficient for RDF quads?
 *
 * @module benchmarks/v6/2-delta-compression
 */

import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import { createDelta, DeltaGate } from '../../packages/v6-core/src/delta/index.mjs';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Benchmark Configuration
// =============================================================================

const CONFIG = {
  initialQuads: 10_000, // ~1MB of RDF data
  operations: 10,
  predicateTypes: 5,
};

// =============================================================================
// Data Generation
// =============================================================================

/**
 * Generate initial RDF graph
 * @param {number} count - Number of quads to generate
 * @returns {Array<Object>} Array of quads
 */
function generateInitialGraph(count) {
  const quads = [];
  const predicates = [
    'http://example.org/name',
    'http://example.org/description',
    'http://example.org/value',
    'http://example.org/type',
    'http://example.org/status',
  ];

  for (let i = 0; i < count; i++) {
    const subject = namedNode(`http://example.org/entity${i}`);
    const predicate = namedNode(predicates[i % predicates.length]);
    const object = literal(`Value for entity ${i} - ${Math.random().toString(36)}`);

    quads.push(quad(subject, predicate, object));
  }

  return quads;
}

/**
 * Serialize quads to estimate size
 * @param {Array<Object>} quads - Quads to serialize
 * @returns {number} Size in bytes
 */
function estimateQuadSize(quads) {
  const serialized = JSON.stringify(
    quads.map((q) => ({
      subject: q.subject.value,
      predicate: q.predicate.value,
      object: q.object.value,
    }))
  );
  return Buffer.byteLength(serialized, 'utf8');
}

/**
 * Serialize delta to estimate size
 * @param {Object} delta - Delta to serialize
 * @returns {number} Size in bytes
 */
function estimateDeltaSize(delta) {
  const serialized = JSON.stringify(delta);
  return Buffer.byteLength(serialized, 'utf8');
}

// =============================================================================
// Benchmark Runner
// =============================================================================

/**
 * Run delta compression benchmark
 * @returns {Promise<Object>} Benchmark results
 */
async function runDeltaCompressionBenchmark() {
  console.log('\n[Delta Compression] Initializing...');

  // Generate initial state
  const initialQuads = generateInitialGraph(CONFIG.initialQuads);
  const initialSize = estimateQuadSize(initialQuads);

  console.log(`Initial graph: ${CONFIG.initialQuads.toLocaleString()} quads`);
  console.log(`Initial size: ${(initialSize / 1024).toFixed(2)} KB`);

  // Create store and populate
  const store = createStore();
  initialQuads.forEach((q) => store.add(q));

  console.log(`\n[Delta Compression] Generating ${CONFIG.operations} deltas...`);

  const deltas = [];
  const deltaSizes = [];

  // Generate deltas
  for (let i = 0; i < CONFIG.operations; i++) {
    const entityId = Math.floor(Math.random() * CONFIG.initialQuads);
    const subject = `http://example.org/entity${entityId}`;
    const predicate = 'http://example.org/status';
    const newValue = `Updated-${i}-${Date.now()}`;

    const delta = createDelta('update', subject, predicate, newValue, {
      oldObject: `OldValue-${entityId}`,
      package: '@unrdf/v6-benchmark',
      actor: 'benchmark-runner',
    });

    deltas.push(delta);

    const deltaSize = estimateDeltaSize(delta);
    deltaSizes.push(deltaSize);

    console.log(`Delta ${i + 1}: ${deltaSize} bytes (${(deltaSize / 1024).toFixed(3)} KB)`);
  }

  // Calculate statistics
  const totalDeltaSize = deltaSizes.reduce((sum, size) => sum + size, 0);
  const avgDeltaSize = totalDeltaSize / deltaSizes.length;
  const maxDeltaSize = Math.max(...deltaSizes);
  const minDeltaSize = Math.min(...deltaSizes);

  // Calculate compression ratio
  const compressionRatio = (totalDeltaSize / initialSize) * 100;
  const compressionRatioPerOperation = (avgDeltaSize / initialSize) * 100;

  return {
    initialSize,
    initialQuads: CONFIG.initialQuads,
    operations: CONFIG.operations,
    deltaSizes,
    totalDeltaSize,
    avgDeltaSize,
    maxDeltaSize,
    minDeltaSize,
    compressionRatio,
    compressionRatioPerOperation,
  };
}

// =============================================================================
// Main Benchmark
// =============================================================================

async function main() {
  console.log('='.repeat(80));
  console.log('V6 Delta Compression Benchmark');
  console.log('='.repeat(80));
  console.log(`Target: <10% of original state size`);
  console.log(`Initial graph size: ${CONFIG.initialQuads.toLocaleString()} quads`);
  console.log(`Operations: ${CONFIG.operations}`);

  const results = await runDeltaCompressionBenchmark();

  // Print results
  console.log('\n' + '='.repeat(80));
  console.log('COMPRESSION RESULTS');
  console.log('='.repeat(80));
  console.log(`Initial state size:     ${(results.initialSize / 1024).toFixed(2)} KB`);
  console.log(`Total delta size:       ${(results.totalDeltaSize / 1024).toFixed(2)} KB`);
  console.log(`Average delta size:     ${(results.avgDeltaSize / 1024).toFixed(3)} KB`);
  console.log(`Min delta size:         ${results.minDeltaSize} bytes`);
  console.log(`Max delta size:         ${results.maxDeltaSize} bytes`);

  console.log('\n' + '='.repeat(80));
  console.log('COMPRESSION RATIO');
  console.log('='.repeat(80));
  console.log(
    `Total deltas vs state:       ${results.compressionRatio.toFixed(2)}%`
  );
  console.log(
    `Avg delta vs state:          ${results.compressionRatioPerOperation.toFixed(4)}%`
  );
  console.log(
    `Size reduction:              ${(100 - results.compressionRatio).toFixed(2)}%`
  );
  console.log(`Target:                      <10.00%`);
  console.log(
    `Status (per operation):      ${results.compressionRatioPerOperation < 10.0 ? '✅ PASS' : '❌ FAIL'}`
  );

  console.log('\n' + '='.repeat(80));
  console.log('EFFICIENCY ANALYSIS');
  console.log('='.repeat(80));
  console.log(
    `Bytes per quad (initial):    ${(results.initialSize / results.initialQuads).toFixed(2)}`
  );
  console.log(
    `Bytes per operation (delta): ${results.avgDeltaSize.toFixed(2)}`
  );
  console.log(
    `Efficiency gain:             ${((results.initialSize / results.initialQuads / results.avgDeltaSize) * 100).toFixed(2)}x`
  );

  // JSON output for aggregation
  console.log('\n__JSON_RESULTS__');
  const jsonResults = {
    benchmark: 'delta-compression',
    timestamp: new Date().toISOString(),
    config: CONFIG,
    results: {
      initialSizeBytes: results.initialSize,
      initialSizeKB: results.initialSize / 1024,
      totalDeltaSizeBytes: results.totalDeltaSize,
      totalDeltaSizeKB: results.totalDeltaSize / 1024,
      avgDeltaSizeBytes: results.avgDeltaSize,
      avgDeltaSizeKB: results.avgDeltaSize / 1024,
      minDeltaSizeBytes: results.minDeltaSize,
      maxDeltaSizeBytes: results.maxDeltaSize,
      compressionRatioPercent: results.compressionRatio,
      compressionRatioPerOperationPercent: results.compressionRatioPerOperation,
    },
    target: {
      maxCompressionPercent: 10.0,
      pass: results.compressionRatioPerOperation < 10.0,
    },
  };
  console.log(JSON.stringify(jsonResults, null, 2));

  // Exit with appropriate code
  process.exit(results.compressionRatioPerOperation < 10.0 ? 0 : 1);
}

main().catch((error) => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
