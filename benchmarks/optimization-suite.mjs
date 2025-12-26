#!/usr/bin/env node
/**
 * Performance Optimization Benchmark Suite
 *
 * Measures before/after performance for:
 * 1. Receipt generation (baseline vs batch)
 * 2. SPARQL queries (baseline vs cached)
 * 3. Hook execution (baseline vs compiled)
 * 4. Time-travel (baseline vs cached)
 *
 * Usage: node benchmarks/optimization-suite.mjs [--quick]
 */

import { performance } from 'perf_hooks';

// =============================================================================
// Configuration
// =============================================================================

const QUICK_MODE = process.argv.includes('--quick');

const config = {
  // Receipt benchmarks
  receipt: {
    iterations: QUICK_MODE ? 100 : 1000,
    warmup: QUICK_MODE ? 10 : 100,
    batchSize: QUICK_MODE ? 100 : 500,
  },
  // Query benchmarks
  query: {
    datasetSizes: QUICK_MODE ? [100, 500] : [100, 1000, 5000],
    queriesPerSize: QUICK_MODE ? 20 : 50,
    warmup: QUICK_MODE ? 5 : 10,
  },
  // Hook benchmarks
  hook: {
    iterations: QUICK_MODE ? 1000 : 10000,
    warmup: QUICK_MODE ? 100 : 500,
    batchSize: QUICK_MODE ? 100 : 1000,
  },
  // Snapshot benchmarks
  snapshot: {
    cacheSize: 10,
    accessPattern: QUICK_MODE ? 20 : 100,
  },
};

// =============================================================================
// Statistics Helpers
// =============================================================================

function calculateStats(values) {
  if (values.length === 0) return null;

  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  const variance = values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
  const stddev = Math.sqrt(variance);

  return {
    count: values.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    stddev,
    median: sorted[Math.floor(sorted.length / 2)],
    p50: sorted[Math.floor(sorted.length * 0.50)],
    p90: sorted[Math.floor(sorted.length * 0.90)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

function formatMs(ms) {
  if (ms < 0.001) return `${(ms * 1000000).toFixed(2)} ns`;
  if (ms < 1) return `${(ms * 1000).toFixed(2)} us`;
  if (ms < 1000) return `${ms.toFixed(2)} ms`;
  return `${(ms / 1000).toFixed(2)} s`;
}

function printStats(name, stats, throughput = null) {
  console.log(`\n  ${name}:`);
  console.log(`    Mean:    ${formatMs(stats.mean)}`);
  console.log(`    Median:  ${formatMs(stats.median)}`);
  console.log(`    P95:     ${formatMs(stats.p95)}`);
  console.log(`    P99:     ${formatMs(stats.p99)}`);
  console.log(`    Min/Max: ${formatMs(stats.min)} / ${formatMs(stats.max)}`);
  if (throughput !== null) {
    console.log(`    Throughput: ${throughput.toFixed(0)} ops/sec`);
  }
}

// =============================================================================
// Mock Implementations for Benchmarking
// =============================================================================

// Mock quad factory
const mockQuad = (i) => ({
  subject: { termType: 'NamedNode', value: `http://example.org/entity${i}` },
  predicate: { termType: 'NamedNode', value: 'http://example.org/prop' },
  object: { termType: 'Literal', value: `value_${i}` },
  graph: { termType: 'NamedNode', value: 'http://example.org/default' },
});

// Mock store with SPARQL-like query
class MockStore {
  constructor() {
    this.quads = [];
    this.queryCount = 0;
  }

  add(quad) {
    this.quads.push(quad);
  }

  match(s, p, o) {
    return this.quads.filter(q => {
      if (s && q.subject.value !== s.value) return false;
      if (p && q.predicate.value !== p.value) return false;
      if (o && q.object.value !== o.value) return false;
      return true;
    });
  }

  query(sparql) {
    this.queryCount++;
    // Simulate query processing
    const results = [];
    for (let i = 0; i < Math.min(10, this.quads.length); i++) {
      results.push({ person: this.quads[i]?.subject, name: this.quads[i]?.object });
    }
    return results;
  }

  get size() {
    return this.quads.length;
  }
}

// =============================================================================
// Benchmark 1: Receipt Generation
// =============================================================================

async function benchmarkReceiptGeneration() {
  console.log('\n' + '='.repeat(60));
  console.log('BENCHMARK 1: Receipt Generation');
  console.log('='.repeat(60));

  // Import hash-wasm
  const { blake3 } = await import('hash-wasm');

  // Baseline: Sequential hashing
  async function generateReceiptBaseline(event, previousHash) {
    const payload = JSON.stringify({
      eventType: event.eventType,
      caseId: event.caseId,
      taskId: event.taskId,
      payload: event.payload,
      t_ns: String(Date.now() * 1000000),
    });

    const payloadHash = await blake3(payload);
    const chainInput = `${previousHash || 'GENESIS'}:${payloadHash}`;
    const receiptHash = await blake3(chainInput);

    return {
      payloadHash,
      receiptHash,
      eventType: event.eventType,
    };
  }

  // Optimized: Batch with parallel hashing
  async function generateReceiptBatch(events) {
    // Pre-serialize all
    const payloads = events.map(event => JSON.stringify({
      eventType: event.eventType,
      caseId: event.caseId,
      taskId: event.taskId,
      payload: event.payload,
      t_ns: String(Date.now() * 1000000),
    }));

    // Hash all payloads in parallel
    const payloadHashes = await Promise.all(payloads.map(p => blake3(p)));

    // Compute chain hashes
    const receipts = [];
    let prevHash = null;
    for (let i = 0; i < payloadHashes.length; i++) {
      const chainInput = `${prevHash || 'GENESIS'}:${payloadHashes[i]}`;
      const receiptHash = await blake3(chainInput);
      receipts.push({
        payloadHash: payloadHashes[i],
        receiptHash,
        eventType: events[i].eventType,
      });
      prevHash = receiptHash;
    }

    return receipts;
  }

  // Generate test events
  const events = Array.from({ length: config.receipt.iterations }, (_, i) => ({
    eventType: 'TASK_ENABLED',
    caseId: `case-${i}`,
    taskId: `task-${i}`,
    payload: { decision: 'ENABLE', value: i },
  }));

  // Warmup
  console.log(`\nWarming up (${config.receipt.warmup} iterations)...`);
  for (let i = 0; i < config.receipt.warmup; i++) {
    await generateReceiptBaseline(events[i], null);
  }

  // Baseline benchmark
  console.log(`\nRunning baseline (sequential) - ${config.receipt.iterations} receipts...`);
  const baselineLatencies = [];
  let prevHash = null;

  const baselineStart = performance.now();
  for (const event of events) {
    const start = performance.now();
    const receipt = await generateReceiptBaseline(event, prevHash);
    baselineLatencies.push(performance.now() - start);
    prevHash = receipt.receiptHash;
  }
  const baselineTotalTime = performance.now() - baselineStart;
  const baselineThroughput = (events.length / baselineTotalTime) * 1000;

  // Optimized benchmark (batched)
  console.log(`\nRunning optimized (batch) - ${config.receipt.iterations} receipts in batches of ${config.receipt.batchSize}...`);
  const batchLatencies = [];

  const batchStart = performance.now();
  for (let i = 0; i < events.length; i += config.receipt.batchSize) {
    const batch = events.slice(i, i + config.receipt.batchSize);
    const start = performance.now();
    await generateReceiptBatch(batch);
    const elapsed = performance.now() - start;
    // Per-receipt latency
    for (let j = 0; j < batch.length; j++) {
      batchLatencies.push(elapsed / batch.length);
    }
  }
  const batchTotalTime = performance.now() - batchStart;
  const batchThroughput = (events.length / batchTotalTime) * 1000;

  // Results
  const baselineStats = calculateStats(baselineLatencies);
  const batchStats = calculateStats(batchLatencies);

  console.log('\n--- RESULTS ---');
  printStats('Baseline (Sequential)', baselineStats, baselineThroughput);
  printStats('Optimized (Batch)', batchStats, batchThroughput);

  const improvement = ((batchThroughput - baselineThroughput) / baselineThroughput) * 100;
  console.log(`\n  IMPROVEMENT: ${improvement.toFixed(1)}% throughput increase`);
  console.log(`  Baseline: ${baselineThroughput.toFixed(0)} receipts/sec`);
  console.log(`  Optimized: ${batchThroughput.toFixed(0)} receipts/sec`);

  return {
    baseline: { stats: baselineStats, throughput: baselineThroughput },
    optimized: { stats: batchStats, throughput: batchThroughput },
    improvement,
  };
}

// =============================================================================
// Benchmark 2: SPARQL Query Caching
// =============================================================================

async function benchmarkQueryCaching() {
  console.log('\n' + '='.repeat(60));
  console.log('BENCHMARK 2: SPARQL Query Caching');
  console.log('='.repeat(60));

  const results = {};

  for (const size of config.query.datasetSizes) {
    console.log(`\n--- Dataset size: ${size} entities ---`);

    // Create and populate store
    const store = new MockStore();
    for (let i = 0; i < size; i++) {
      store.add(mockQuad(i));
    }

    const queries = [
      'SELECT ?person ?name WHERE { ?person foaf:name ?name } LIMIT 10',
      'SELECT ?person WHERE { ?person foaf:age ?age } LIMIT 10',
      'SELECT ?p ?o WHERE { <http://example.org/entity1> ?p ?o }',
    ];

    // Warmup
    for (let i = 0; i < config.query.warmup; i++) {
      store.query(queries[i % queries.length]);
    }

    // Baseline: No caching (simulated by always querying)
    const baselineLatencies = [];
    for (let i = 0; i < config.query.queriesPerSize; i++) {
      const query = queries[i % queries.length];
      const start = performance.now();
      store.query(query);
      baselineLatencies.push(performance.now() - start);
    }

    // Optimized: With caching (simulated cache hit)
    const queryCache = new Map();
    const cachedLatencies = [];

    for (let i = 0; i < config.query.queriesPerSize; i++) {
      const query = queries[i % queries.length];
      const start = performance.now();

      if (queryCache.has(query)) {
        // Cache hit - just access map
        queryCache.get(query);
      } else {
        // Cache miss - query and store
        const result = store.query(query);
        queryCache.set(query, result);
      }

      cachedLatencies.push(performance.now() - start);
    }

    const baselineStats = calculateStats(baselineLatencies);
    const cachedStats = calculateStats(cachedLatencies);

    printStats('Baseline (No Cache)', baselineStats);
    printStats('Optimized (Cached)', cachedStats);

    const improvement = ((baselineStats.mean - cachedStats.mean) / baselineStats.mean) * 100;
    console.log(`  IMPROVEMENT: ${improvement.toFixed(1)}% latency reduction`);

    results[size] = {
      baseline: baselineStats,
      optimized: cachedStats,
      improvement,
    };
  }

  return results;
}

// =============================================================================
// Benchmark 3: Hook Policy Compilation
// =============================================================================

async function benchmarkPolicyCompilation() {
  console.log('\n' + '='.repeat(60));
  console.log('BENCHMARK 3: Hook Policy Compilation');
  console.log('='.repeat(60));

  // Mock hooks
  const hooks = [
    {
      name: 'validate-iri',
      validate: (quad) => quad.subject?.value?.startsWith('http'),
    },
    {
      name: 'validate-predicate',
      validate: (quad) => !quad.predicate?.value?.includes('forbidden'),
    },
    {
      name: 'validate-literal',
      validate: (quad) => typeof quad.object?.value === 'string' && quad.object.value.length < 1000,
    },
  ];

  // Generate test quads
  const quads = Array.from({ length: config.hook.iterations }, (_, i) => mockQuad(i));

  // Warmup
  console.log(`\nWarming up (${config.hook.warmup} iterations)...`);
  for (let i = 0; i < config.hook.warmup; i++) {
    for (const hook of hooks) {
      hook.validate(quads[i]);
    }
  }

  // Baseline: Direct function calls
  console.log(`\nRunning baseline - ${config.hook.iterations} quads...`);
  const baselineLatencies = [];

  for (const quad of quads) {
    const start = performance.now();
    for (const hook of hooks) {
      hook.validate(quad);
    }
    baselineLatencies.push(performance.now() - start);
  }

  // Optimized: Pre-compiled/cached hooks
  // In real impl, this compiles to optimized form
  // Here we simulate by pre-binding
  const compiledHooks = hooks.map(hook => ({
    ...hook,
    _compiled: true,
    validate: hook.validate.bind(null), // Pre-bound for slight perf boost
  }));

  console.log(`\nRunning optimized (compiled) - ${config.hook.iterations} quads...`);
  const compiledLatencies = [];

  for (const quad of quads) {
    const start = performance.now();
    for (const hook of compiledHooks) {
      hook.validate(quad);
    }
    compiledLatencies.push(performance.now() - start);
  }

  // Batch validation
  console.log(`\nRunning batch validation - ${config.hook.iterations} quads in batches of ${config.hook.batchSize}...`);
  const batchLatencies = [];

  for (let i = 0; i < quads.length; i += config.hook.batchSize) {
    const batch = quads.slice(i, i + config.hook.batchSize);
    const start = performance.now();

    // Batch validate
    const results = new Uint8Array(batch.length);
    for (let j = 0; j < batch.length; j++) {
      let valid = true;
      for (const hook of compiledHooks) {
        if (!hook.validate(batch[j])) {
          valid = false;
          break;
        }
      }
      results[j] = valid ? 1 : 0;
    }

    const elapsed = performance.now() - start;
    for (let j = 0; j < batch.length; j++) {
      batchLatencies.push(elapsed / batch.length);
    }
  }

  const baselineStats = calculateStats(baselineLatencies);
  const compiledStats = calculateStats(compiledLatencies);
  const batchStats = calculateStats(batchLatencies);

  console.log('\n--- RESULTS ---');
  printStats('Baseline (Direct)', baselineStats);
  printStats('Optimized (Compiled)', compiledStats);
  printStats('Optimized (Batch)', batchStats);

  const compiledImprovement = ((baselineStats.mean - compiledStats.mean) / baselineStats.mean) * 100;
  const batchImprovement = ((baselineStats.mean - batchStats.mean) / baselineStats.mean) * 100;

  console.log(`\n  Compiled: ${compiledImprovement.toFixed(1)}% improvement`);
  console.log(`  Batch: ${batchImprovement.toFixed(1)}% improvement`);
  console.log(`  Baseline P95: ${formatMs(baselineStats.p95)}`);
  console.log(`  Compiled P95: ${formatMs(compiledStats.p95)}`);

  return {
    baseline: baselineStats,
    compiled: compiledStats,
    batch: batchStats,
    compiledImprovement,
    batchImprovement,
  };
}

// =============================================================================
// Benchmark 4: Snapshot Caching
// =============================================================================

async function benchmarkSnapshotCaching() {
  console.log('\n' + '='.repeat(60));
  console.log('BENCHMARK 4: Snapshot Caching (Simulated)');
  console.log('='.repeat(60));

  // Simulate snapshot data (N-Quads strings)
  const snapshotSize = 1000; // quads per snapshot
  const snapshots = {};

  for (let i = 0; i < config.snapshot.cacheSize; i++) {
    const quads = Array.from({ length: snapshotSize }, (_, j) =>
      `<http://example.org/entity${j}> <http://example.org/prop> "value_${j}" .`
    ).join('\n');
    snapshots[`snapshot-${i}`] = quads;
  }

  // Simulate git read (slow - 50-100ms simulated)
  async function readFromGit(ref) {
    await new Promise(resolve => setTimeout(resolve, 50 + Math.random() * 50));
    return snapshots[ref] || '';
  }

  // LRU Cache
  const cache = new Map();
  const maxCacheSize = 5;

  async function readWithCache(ref) {
    if (cache.has(ref)) {
      // Move to end (LRU)
      const value = cache.get(ref);
      cache.delete(ref);
      cache.set(ref, value);
      return value;
    }

    // Cache miss - load from git
    const data = await readFromGit(ref);

    // Evict if needed
    if (cache.size >= maxCacheSize) {
      const oldestKey = cache.keys().next().value;
      cache.delete(oldestKey);
    }

    cache.set(ref, data);
    return data;
  }

  // Generate access pattern (mix of cache hits and misses)
  const accessPattern = [];
  const refs = Object.keys(snapshots);

  for (let i = 0; i < config.snapshot.accessPattern; i++) {
    // 70% recent (likely cache hit), 30% random
    if (Math.random() < 0.7 && accessPattern.length > 0) {
      const recentIdx = Math.max(0, accessPattern.length - 5);
      accessPattern.push(accessPattern[recentIdx + Math.floor(Math.random() * Math.min(5, accessPattern.length - recentIdx))]);
    } else {
      accessPattern.push(refs[Math.floor(Math.random() * refs.length)]);
    }
  }

  // Baseline: Always read from git (simulated)
  console.log(`\nRunning baseline (no cache) - ${config.snapshot.accessPattern} accesses...`);
  const baselineLatencies = [];

  for (const ref of accessPattern.slice(0, 10)) { // Only 10 for baseline (slow)
    const start = performance.now();
    await readFromGit(ref);
    baselineLatencies.push(performance.now() - start);
  }

  // Optimized: With LRU cache
  console.log(`\nRunning optimized (cached) - ${config.snapshot.accessPattern} accesses...`);
  const cachedLatencies = [];
  let cacheHits = 0;

  cache.clear();

  for (const ref of accessPattern) {
    const hadInCache = cache.has(ref);
    const start = performance.now();
    await readWithCache(ref);
    cachedLatencies.push(performance.now() - start);

    if (hadInCache) cacheHits++;
  }

  const baselineStats = calculateStats(baselineLatencies);
  const cachedStats = calculateStats(cachedLatencies);

  console.log('\n--- RESULTS ---');
  printStats('Baseline (No Cache)', baselineStats);
  printStats('Optimized (Cached)', cachedStats);

  const hitRate = cacheHits / accessPattern.length;
  console.log(`\n  Cache Hit Rate: ${(hitRate * 100).toFixed(1)}%`);
  console.log(`  Baseline Mean: ${formatMs(baselineStats.mean)}`);
  console.log(`  Cached Mean: ${formatMs(cachedStats.mean)}`);

  const improvement = ((baselineStats.mean - cachedStats.mean) / baselineStats.mean) * 100;
  console.log(`  IMPROVEMENT: ${improvement.toFixed(1)}% latency reduction`);

  return {
    baseline: baselineStats,
    optimized: cachedStats,
    cacheHitRate: hitRate,
    improvement,
  };
}

// =============================================================================
// Main Runner
// =============================================================================

async function main() {
  console.log('='.repeat(60));
  console.log('PERFORMANCE OPTIMIZATION BENCHMARK SUITE');
  console.log('='.repeat(60));
  console.log(`Mode: ${QUICK_MODE ? 'QUICK' : 'FULL'}`);
  console.log(`Started: ${new Date().toISOString()}`);

  const results = {};

  try {
    // Run all benchmarks
    results.receipt = await benchmarkReceiptGeneration();
    results.query = await benchmarkQueryCaching();
    results.hooks = await benchmarkPolicyCompilation();
    results.snapshot = await benchmarkSnapshotCaching();

    // Summary
    console.log('\n' + '='.repeat(60));
    console.log('OPTIMIZATION SUMMARY');
    console.log('='.repeat(60));

    console.log('\n1. Receipt Generation:');
    console.log(`   Baseline: ${results.receipt.baseline.throughput.toFixed(0)} receipts/sec`);
    console.log(`   Optimized: ${results.receipt.optimized.throughput.toFixed(0)} receipts/sec`);
    console.log(`   Improvement: ${results.receipt.improvement.toFixed(1)}%`);

    console.log('\n2. SPARQL Query Caching:');
    for (const [size, data] of Object.entries(results.query)) {
      console.log(`   Dataset ${size}: ${data.improvement.toFixed(1)}% latency reduction`);
    }

    console.log('\n3. Hook Policy Compilation:');
    console.log(`   Compiled: ${results.hooks.compiledImprovement.toFixed(1)}% improvement`);
    console.log(`   Batch: ${results.hooks.batchImprovement.toFixed(1)}% improvement`);
    console.log(`   P95 Latency: ${formatMs(results.hooks.compiled.p95)}`);

    console.log('\n4. Snapshot Caching:');
    console.log(`   Cache Hit Rate: ${(results.snapshot.cacheHitRate * 100).toFixed(1)}%`);
    console.log(`   Latency Reduction: ${results.snapshot.improvement.toFixed(1)}%`);

    console.log('\n' + '='.repeat(60));
    console.log('SUCCESS CRITERIA CHECK');
    console.log('='.repeat(60));

    const criteria = [
      {
        name: 'Receipt generation >= 80K/sec',
        target: 80000,
        actual: results.receipt.optimized.throughput,
        pass: results.receipt.optimized.throughput >= 80000,
      },
      {
        name: 'Hook P95 < 500us',
        target: 0.5,
        actual: results.hooks.compiled.p95,
        pass: results.hooks.compiled.p95 < 0.5,
      },
      {
        name: 'Snapshot cache hit rate > 50%',
        target: 0.5,
        actual: results.snapshot.cacheHitRate,
        pass: results.snapshot.cacheHitRate > 0.5,
      },
    ];

    for (const c of criteria) {
      const status = c.pass ? 'PASS' : 'FAIL';
      console.log(`   [${status}] ${c.name}`);
    }

    console.log('\n' + '='.repeat(60));
    console.log(`Completed: ${new Date().toISOString()}`);
    console.log('='.repeat(60));

    // Return results for programmatic use
    return results;

  } catch (error) {
    console.error('\nBenchmark failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
main().catch(console.error);
