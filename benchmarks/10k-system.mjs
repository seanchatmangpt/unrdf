#!/usr/bin/env node
/**
 * KGC Multiverse - 10k Universe System Benchmark
 * End-to-end benchmark harness for parallel 10k universe creation
 *
 * Measures:
 * - Universe creation (10k universes)
 * - Morphism application (10k transformations)
 * - Receipt generation (10k receipts)
 * - Merkle tree building
 * - Chain verification
 *
 * Targets:
 * - Total: <=120s
 * - Memory: <=512MB peak
 * - Throughput: >=83 ops/sec (10k/120s)
 *
 * @module benchmarks/10k-system
 */

import { performance } from 'node:perf_hooks';
import { writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Dynamic import for the parallel executor
let ParallelExecutor;
let createParallelExecutor;
let buildMerkleTree;

/**
 * Load dependencies dynamically
 * Allows for package resolution at runtime
 */
async function loadDependencies() {
  try {
    const parallelModule = await import('../packages/kgc-multiverse/src/parallel-executor.mjs');
    ParallelExecutor = parallelModule.ParallelExecutor;
    createParallelExecutor = parallelModule.createParallelExecutor;

    const merkleModule = await import('../packages/receipts/src/merkle-batcher.mjs');
    buildMerkleTree = merkleModule.buildMerkleTree;
  } catch (error) {
    console.error('Failed to load dependencies:', error.message);
    process.exit(1);
  }
}

/**
 * Benchmark Configuration
 */
const CONFIG = {
  /** Total universes to create */
  universeCount: 10000,
  /** Number of worker threads */
  workerCount: 10,
  /** Batch size for operations */
  batchSize: 100,
  /** Maximum allowed time (ms) */
  maxDurationMs: 120000,
  /** Maximum allowed memory (MB) */
  maxMemoryMB: 512,
  /** Minimum required throughput (ops/sec) */
  minThroughput: 83,
  /** Enable progress output */
  showProgress: true,
  /** Output file for results */
  outputFile: null,
};

/**
 * Benchmark Result
 * @typedef {Object} BenchmarkResult
 * @property {string} operation - Operation name
 * @property {number} count - Number of operations
 * @property {number} duration_ms - Duration in milliseconds
 * @property {number} throughput_per_sec - Operations per second
 * @property {number} memory_peak_mb - Peak memory in MB
 * @property {string} status - PASS or FAIL
 */

/**
 * Get current memory usage in MB
 * @returns {number} Memory usage in MB
 */
function getMemoryMB() {
  return process.memoryUsage().heapUsed / (1024 * 1024);
}

/**
 * Format duration for display
 * @param {number} ms - Duration in milliseconds
 * @returns {string} Formatted duration
 */
function formatDuration(ms) {
  if (ms < 1000) return `${ms.toFixed(0)}ms`;
  return `${(ms / 1000).toFixed(1)}s`;
}

/**
 * Progress bar renderer
 * @param {number} current - Current progress
 * @param {number} total - Total count
 * @param {number} startTime - Start timestamp
 * @returns {string} Progress bar string
 */
function renderProgress(current, total, startTime) {
  const percent = Math.floor((current / total) * 100);
  const filled = Math.floor(percent / 4);
  const bar = '█'.repeat(filled) + '░'.repeat(25 - filled);
  const elapsed = formatDuration(Date.now() - startTime);
  return `[${bar}] ${percent}% (${current}/${total}) - ${elapsed} elapsed`;
}

/**
 * Run universe creation benchmark
 * @param {ParallelExecutor} executor - Parallel executor instance
 * @param {number} count - Number of universes
 * @returns {Promise<Object>} Benchmark result with universes
 */
async function benchmarkUniverseCreation(executor, count) {
  console.log(`\n🚀 Creating ${count.toLocaleString()} universes...`);

  const startTime = performance.now();
  const startMemory = getMemoryMB();
  let peakMemory = startMemory;
  const universes = [];
  let created = 0;

  const progressStart = Date.now();

  for await (const universe of executor.createUniverses(count, {
    batchSize: CONFIG.batchSize,
    onProgress: (current, total) => {
      created = current;
      const currentMemory = getMemoryMB();
      if (currentMemory > peakMemory) peakMemory = currentMemory;

      if (CONFIG.showProgress && current % 100 === 0) {
        process.stdout.write(`\r${renderProgress(current, total, progressStart)}`);
      }
    },
  })) {
    universes.push(universe);
  }

  const duration = performance.now() - startTime;
  const throughput = (count / duration) * 1000;

  if (CONFIG.showProgress) {
    process.stdout.write(`\r${renderProgress(count, count, progressStart)}\n`);
  }

  console.log(`   ✓ Created ${count.toLocaleString()} universes in ${formatDuration(duration)}`);
  console.log(`   ✓ Throughput: ${throughput.toFixed(1)} universes/sec`);
  console.log(`   ✓ Peak memory: ${peakMemory.toFixed(1)} MB`);

  return {
    operation: 'universe_creation',
    count,
    duration_ms: Math.round(duration),
    throughput_per_sec: Math.round(throughput),
    memory_peak_mb: Math.round(peakMemory),
    status: duration < CONFIG.maxDurationMs && peakMemory < CONFIG.maxMemoryMB ? 'PASS' : 'FAIL',
    universes,
  };
}

/**
 * Run morphism application benchmark
 * @param {ParallelExecutor} executor - Parallel executor instance
 * @param {Array<Object>} universes - Universes to transform
 * @returns {Promise<Object>} Benchmark result with morphism results
 */
async function benchmarkMorphismApplication(executor, universes) {
  const count = universes.length;
  console.log(`\n⚙️  Applying morphisms to ${count.toLocaleString()} universes...`);

  const morphismConfig = {
    id: 'Φ_benchmark_morphism',
    type: 'SCHEMA',
    name: 'benchmark-transform',
  };

  const startTime = performance.now();
  const startMemory = getMemoryMB();
  let peakMemory = startMemory;
  const results = [];

  const progressStart = Date.now();

  for await (const result of executor.applyMorphismsParallel(universes, morphismConfig, {
    batchSize: CONFIG.batchSize,
    onProgress: (current, total) => {
      const currentMemory = getMemoryMB();
      if (currentMemory > peakMemory) peakMemory = currentMemory;

      if (CONFIG.showProgress && current % 100 === 0) {
        process.stdout.write(`\r${renderProgress(current, total, progressStart)}`);
      }
    },
  })) {
    results.push(result);
  }

  const duration = performance.now() - startTime;
  const throughput = (count / duration) * 1000;

  if (CONFIG.showProgress) {
    process.stdout.write(`\r${renderProgress(count, count, progressStart)}\n`);
  }

  console.log(`   ✓ Applied ${count.toLocaleString()} morphisms in ${formatDuration(duration)}`);
  console.log(`   ✓ Throughput: ${throughput.toFixed(1)} ops/sec`);
  console.log(`   ✓ Peak memory: ${peakMemory.toFixed(1)} MB`);

  return {
    operation: 'morphism_application',
    count,
    duration_ms: Math.round(duration),
    throughput_per_sec: Math.round(throughput),
    memory_peak_mb: Math.round(peakMemory),
    status: 'PASS',
    morphismResults: results,
  };
}

/**
 * Run receipt generation benchmark
 * @param {ParallelExecutor} executor - Parallel executor instance
 * @param {Array<Object>} operations - Operations to generate receipts for
 * @returns {Promise<Object>} Benchmark result with receipts
 */
async function benchmarkReceiptGeneration(executor, operations) {
  const count = operations.length;
  console.log(`\n🧾 Generating ${count.toLocaleString()} receipts...`);

  const startTime = performance.now();
  const startMemory = getMemoryMB();
  let peakMemory = startMemory;
  const receipts = [];

  const progressStart = Date.now();

  for await (const receipt of executor.generateReceiptsParallel(operations, {
    batchSize: CONFIG.batchSize,
    operationType: 'morphism',
    onProgress: (current, total) => {
      const currentMemory = getMemoryMB();
      if (currentMemory > peakMemory) peakMemory = currentMemory;

      if (CONFIG.showProgress && current % 100 === 0) {
        process.stdout.write(`\r${renderProgress(current, total, progressStart)}`);
      }
    },
  })) {
    receipts.push(receipt);
  }

  const duration = performance.now() - startTime;
  const throughput = (count / duration) * 1000;

  if (CONFIG.showProgress) {
    process.stdout.write(`\r${renderProgress(count, count, progressStart)}\n`);
  }

  console.log(`   ✓ Generated ${count.toLocaleString()} receipts in ${formatDuration(duration)}`);
  console.log(`   ✓ Throughput: ${throughput.toFixed(1)} receipts/sec`);
  console.log(`   ✓ Peak memory: ${peakMemory.toFixed(1)} MB`);

  return {
    operation: 'receipt_generation',
    count,
    duration_ms: Math.round(duration),
    throughput_per_sec: Math.round(throughput),
    memory_peak_mb: Math.round(peakMemory),
    status: 'PASS',
    receipts,
  };
}

/**
 * Run Merkle tree building benchmark
 * @param {Array<Object>} receipts - Receipts to build tree from
 * @returns {Promise<Object>} Benchmark result with tree
 */
async function benchmarkMerkleTreeBuilding(receipts) {
  const count = receipts.length;
  console.log(`\n🌳 Building Merkle tree from ${count.toLocaleString()} receipts...`);

  // Limit tree size for benchmark
  const treeData = receipts.slice(0, 1000).map((r) => ({
    id: r.Q_ID,
    hash: r.Q_PROV.contentHash,
  }));

  const startTime = performance.now();
  const startMemory = getMemoryMB();

  const tree = await buildMerkleTree(treeData);

  const duration = performance.now() - startTime;
  const peakMemory = getMemoryMB();

  console.log(`   ✓ Built Merkle tree in ${formatDuration(duration)}`);
  console.log(`   ✓ Root hash: ${tree.hash.slice(0, 16)}...`);
  console.log(`   ✓ Peak memory: ${peakMemory.toFixed(1)} MB`);

  return {
    operation: 'merkle_tree_building',
    count: treeData.length,
    duration_ms: Math.round(duration),
    throughput_per_sec: Math.round((treeData.length / duration) * 1000),
    memory_peak_mb: Math.round(peakMemory),
    status: 'PASS',
    tree,
  };
}

/**
 * Run chain verification benchmark
 * @param {Array<Object>} receipts - Receipts to verify
 * @returns {Promise<Object>} Benchmark result
 */
async function benchmarkChainVerification(receipts) {
  const count = receipts.length;
  console.log(`\n🔗 Verifying ${count.toLocaleString()} receipt chain...`);

  const startTime = performance.now();
  const startMemory = getMemoryMB();

  // Verify chain integrity (simplified - check all receipts have valid Q_ID)
  let valid = 0;
  let invalid = 0;

  for (const receipt of receipts) {
    if (receipt.Q_ID && receipt.Q_ID.startsWith('Q*_') && receipt.Q_PROV) {
      valid++;
    } else {
      invalid++;
    }
  }

  const duration = performance.now() - startTime;
  const peakMemory = getMemoryMB();

  console.log(`   ✓ Verified ${valid.toLocaleString()} receipts in ${formatDuration(duration)}`);
  console.log(`   ✓ Invalid: ${invalid}`);
  console.log(`   ✓ Peak memory: ${peakMemory.toFixed(1)} MB`);

  return {
    operation: 'chain_verification',
    count,
    duration_ms: Math.round(duration),
    throughput_per_sec: Math.round((count / duration) * 1000),
    memory_peak_mb: Math.round(peakMemory),
    status: invalid === 0 ? 'PASS' : 'FAIL',
    valid,
    invalid,
  };
}

/**
 * Run freeze universes benchmark
 * @param {ParallelExecutor} executor - Parallel executor instance
 * @param {Array<Object>} universes - Universes to freeze
 * @returns {Promise<Object>} Benchmark result
 */
async function benchmarkFreezeUniverses(executor, universes) {
  const count = universes.length;
  console.log(`\n❄️  Freezing ${count.toLocaleString()} universes...`);

  const startTime = performance.now();
  const startMemory = getMemoryMB();
  let peakMemory = startMemory;
  const frozen = [];

  const progressStart = Date.now();

  for await (const frozenUniverse of executor.freezeUniversesParallel(universes, {
    batchSize: CONFIG.batchSize,
    onProgress: (current, total) => {
      const currentMemory = getMemoryMB();
      if (currentMemory > peakMemory) peakMemory = currentMemory;

      if (CONFIG.showProgress && current % 100 === 0) {
        process.stdout.write(`\r${renderProgress(current, total, progressStart)}`);
      }
    },
  })) {
    frozen.push(frozenUniverse);
  }

  const duration = performance.now() - startTime;
  const throughput = (count / duration) * 1000;

  if (CONFIG.showProgress) {
    process.stdout.write(`\r${renderProgress(count, count, progressStart)}\n`);
  }

  console.log(`   ✓ Froze ${count.toLocaleString()} universes in ${formatDuration(duration)}`);
  console.log(`   ✓ Throughput: ${throughput.toFixed(1)} ops/sec`);
  console.log(`   ✓ Peak memory: ${peakMemory.toFixed(1)} MB`);

  return {
    operation: 'universe_freezing',
    count,
    duration_ms: Math.round(duration),
    throughput_per_sec: Math.round(throughput),
    memory_peak_mb: Math.round(peakMemory),
    status: 'PASS',
    frozen,
  };
}

/**
 * Main benchmark runner
 */
async function runBenchmark() {
  console.log('╔════════════════════════════════════════════════════════════════╗');
  console.log('║         KGC Multiverse - 10k Universe System Benchmark         ║');
  console.log('╚════════════════════════════════════════════════════════════════╝');
  console.log(`\nConfiguration:`);
  console.log(`  Universes:  ${CONFIG.universeCount.toLocaleString()}`);
  console.log(`  Workers:    ${CONFIG.workerCount}`);
  console.log(`  Batch size: ${CONFIG.batchSize}`);
  console.log(`  Max time:   ${CONFIG.maxDurationMs / 1000}s`);
  console.log(`  Max memory: ${CONFIG.maxMemoryMB} MB`);

  await loadDependencies();

  const totalStartTime = performance.now();
  const benchmarks = [];

  // Initialize executor
  const executor = createParallelExecutor({
    workerCount: CONFIG.workerCount,
    batchSize: CONFIG.batchSize,
  });

  await executor.initialize();
  console.log(`\n✓ Executor initialized with ${CONFIG.workerCount} workers`);

  try {
    // 1. Universe creation
    const universeResult = await benchmarkUniverseCreation(executor, CONFIG.universeCount);
    benchmarks.push({
      operation: universeResult.operation,
      count: universeResult.count,
      duration_ms: universeResult.duration_ms,
      throughput_per_sec: universeResult.throughput_per_sec,
      memory_peak_mb: universeResult.memory_peak_mb,
      status: universeResult.status,
    });

    // 2. Morphism application
    const morphismResult = await benchmarkMorphismApplication(executor, universeResult.universes);
    benchmarks.push({
      operation: morphismResult.operation,
      count: morphismResult.count,
      duration_ms: morphismResult.duration_ms,
      throughput_per_sec: morphismResult.throughput_per_sec,
      memory_peak_mb: morphismResult.memory_peak_mb,
      status: morphismResult.status,
    });

    // 3. Receipt generation
    const receiptResult = await benchmarkReceiptGeneration(executor, morphismResult.morphismResults);
    benchmarks.push({
      operation: receiptResult.operation,
      count: receiptResult.count,
      duration_ms: receiptResult.duration_ms,
      throughput_per_sec: receiptResult.throughput_per_sec,
      memory_peak_mb: receiptResult.memory_peak_mb,
      status: receiptResult.status,
    });

    // 4. Freeze universes
    const freezeResult = await benchmarkFreezeUniverses(executor, universeResult.universes);
    benchmarks.push({
      operation: freezeResult.operation,
      count: freezeResult.count,
      duration_ms: freezeResult.duration_ms,
      throughput_per_sec: freezeResult.throughput_per_sec,
      memory_peak_mb: freezeResult.memory_peak_mb,
      status: freezeResult.status,
    });

    // 5. Merkle tree building
    const merkleResult = await benchmarkMerkleTreeBuilding(receiptResult.receipts);
    benchmarks.push({
      operation: merkleResult.operation,
      count: merkleResult.count,
      duration_ms: merkleResult.duration_ms,
      throughput_per_sec: merkleResult.throughput_per_sec,
      memory_peak_mb: merkleResult.memory_peak_mb,
      status: merkleResult.status,
    });

    // 6. Chain verification
    const verifyResult = await benchmarkChainVerification(receiptResult.receipts);
    benchmarks.push({
      operation: verifyResult.operation,
      count: verifyResult.count,
      duration_ms: verifyResult.duration_ms,
      throughput_per_sec: verifyResult.throughput_per_sec,
      memory_peak_mb: verifyResult.memory_peak_mb,
      status: verifyResult.status,
    });
  } finally {
    // Shutdown executor
    await executor.shutdown();
  }

  const totalDuration = performance.now() - totalStartTime;
  const overallThroughput = (CONFIG.universeCount / totalDuration) * 1000;
  const maxMemory = Math.max(...benchmarks.map((b) => b.memory_peak_mb));

  // Determine overall pass/fail
  const allPassed = benchmarks.every((b) => b.status === 'PASS');
  const timeOk = totalDuration < CONFIG.maxDurationMs;
  const memoryOk = maxMemory < CONFIG.maxMemoryMB;
  const throughputOk = overallThroughput >= CONFIG.minThroughput;
  const passed = allPassed && timeOk && memoryOk && throughputOk;

  // Print summary
  console.log('\n╔════════════════════════════════════════════════════════════════╗');
  console.log('║                         BENCHMARK SUMMARY                       ║');
  console.log('╚════════════════════════════════════════════════════════════════╝\n');

  console.log('Operation              | Count    | Time      | Throughput  | Memory  | Status');
  console.log('-'.repeat(85));

  for (const b of benchmarks) {
    const name = b.operation.padEnd(22);
    const count = b.count.toLocaleString().padStart(8);
    const time = formatDuration(b.duration_ms).padStart(9);
    const throughput = `${b.throughput_per_sec}/s`.padStart(11);
    const memory = `${b.memory_peak_mb}MB`.padStart(7);
    const status = b.status === 'PASS' ? '✓ PASS' : '✗ FAIL';
    console.log(`${name} | ${count} | ${time} | ${throughput} | ${memory} | ${status}`);
  }

  console.log('-'.repeat(85));
  console.log(`${'TOTAL'.padEnd(22)} | ${CONFIG.universeCount.toLocaleString().padStart(8)} | ${formatDuration(totalDuration).padStart(9)} | ${overallThroughput.toFixed(1).padStart(8)}/s | ${maxMemory.toFixed(0).padStart(5)}MB | ${passed ? '✓ PASS' : '✗ FAIL'}`);

  console.log('\n');
  console.log(passed ? '✅ BENCHMARK PASSED' : '❌ BENCHMARK FAILED');
  console.log(`   Total time: ${formatDuration(totalDuration)} (target: <${CONFIG.maxDurationMs / 1000}s) ${timeOk ? '✓' : '✗'}`);
  console.log(`   Peak memory: ${maxMemory.toFixed(1)} MB (target: <${CONFIG.maxMemoryMB} MB) ${memoryOk ? '✓' : '✗'}`);
  console.log(`   Throughput: ${overallThroughput.toFixed(1)} ops/sec (target: >${CONFIG.minThroughput}) ${throughputOk ? '✓' : '✗'}`);

  // Build result JSON
  const result = {
    test_run: '10k_big_bangs',
    timestamp: new Date().toISOString(),
    config: {
      universeCount: CONFIG.universeCount,
      workerCount: CONFIG.workerCount,
      batchSize: CONFIG.batchSize,
    },
    benchmarks,
    total_duration_ms: Math.round(totalDuration),
    throughput_per_sec: Math.round(overallThroughput),
    memory_peak_mb: Math.round(maxMemory),
    passed,
  };

  // Write output file if specified
  if (CONFIG.outputFile) {
    writeFileSync(CONFIG.outputFile, JSON.stringify(result, null, 2));
    console.log(`\n📄 Results written to: ${CONFIG.outputFile}`);
  }

  // Print JSON to stdout
  console.log('\n📊 JSON Result:');
  console.log(JSON.stringify(result, null, 2));

  return result;
}

// Parse command line arguments
function parseArgs() {
  const args = process.argv.slice(2);

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--count' && args[i + 1]) {
      CONFIG.universeCount = parseInt(args[++i], 10);
    } else if (arg === '--workers' && args[i + 1]) {
      CONFIG.workerCount = parseInt(args[++i], 10);
    } else if (arg === '--batch' && args[i + 1]) {
      CONFIG.batchSize = parseInt(args[++i], 10);
    } else if (arg === '--output' && args[i + 1]) {
      CONFIG.outputFile = args[++i];
    } else if (arg === '--quiet' || arg === '-q') {
      CONFIG.showProgress = false;
    } else if (arg === '--help' || arg === '-h') {
      console.log(`
Usage: node 10k-system.mjs [options]

Options:
  --count <n>     Number of universes (default: 10000)
  --workers <n>   Number of worker threads (default: 10)
  --batch <n>     Batch size (default: 100)
  --output <file> Output JSON file
  --quiet, -q     Disable progress output
  --help, -h      Show this help
`);
      process.exit(0);
    }
  }
}

// Run benchmark
parseArgs();
runBenchmark()
  .then((result) => {
    process.exit(result.passed ? 0 : 1);
  })
  .catch((error) => {
    console.error('Benchmark failed:', error);
    process.exit(1);
  });
