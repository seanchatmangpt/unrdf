/**
 * Optimization Benchmarks Suite
 * Comprehensive benchmarks for all 4 optimization modules
 */
import { performance } from 'perf_hooks';

// =============================================================================
// Utilities
// =============================================================================

function calculateStats(values) {
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;
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

function printStats(name, stats, unit = 'ms') {
  console.log(`\n${name}:`);
  console.log(`  Min:       ${stats.min.toFixed(3)} ${unit}`);
  console.log(`  Mean:      ${stats.mean.toFixed(3)} ${unit}`);
  console.log(`  Median:    ${stats.median.toFixed(3)} ${unit}`);
  console.log(`  P90:       ${stats.p90.toFixed(3)} ${unit}`);
  console.log(`  P95:       ${stats.p95.toFixed(3)} ${unit}`);
  console.log(`  P99:       ${stats.p99.toFixed(3)} ${unit}`);
  console.log(`  Max:       ${stats.max.toFixed(3)} ${unit}`);
}

// =============================================================================
// 1. Snapshot Cache Benchmark
// =============================================================================

async function benchmarkSnapshotCache() {
  console.log('\n' + '='.repeat(60));
  console.log('1. SNAPSHOT CACHE BENCHMARK');
  console.log('='.repeat(60));

  const { SnapshotLRUCache } = await import('../packages/kgc-4d/src/snapshot-cache.mjs');

  const cache = new SnapshotLRUCache({
    maxSize: 1000,
    maxMemoryMB: 100,
    ttlMs: 60000,
  });

  // Benchmark: Cache writes
  const writeLatencies = [];
  for (let i = 0; i < 1000; i++) {
    const start = performance.now();
    cache.set(`key${i}`, { data: `value${i}`.repeat(100) });
    writeLatencies.push(performance.now() - start);
  }

  // Benchmark: Cache reads (hits)
  const readLatencies = [];
  for (let i = 0; i < 1000; i++) {
    const start = performance.now();
    cache.get(`key${i}`);
    readLatencies.push(performance.now() - start);
  }

  // Benchmark: Cache misses
  const missLatencies = [];
  for (let i = 0; i < 100; i++) {
    const start = performance.now();
    cache.get(`nonexistent${i}`);
    missLatencies.push(performance.now() - start);
  }

  const stats = cache.getStats();

  console.log('\nCache Write Performance:');
  printStats('Write Latency', calculateStats(writeLatencies));

  console.log('\nCache Read Performance (Hits):');
  printStats('Read Latency', calculateStats(readLatencies));

  console.log('\nCache Read Performance (Misses):');
  printStats('Miss Latency', calculateStats(missLatencies));

  console.log('\nCache Statistics:');
  console.log(`  Size:           ${stats.size}`);
  console.log(`  Hit Rate:       ${(stats.hitRate * 100).toFixed(1)}%`);
  console.log(`  Memory Used:    ${stats.memoryUsedMB.toFixed(2)} MB`);

  return {
    writeStats: calculateStats(writeLatencies),
    readStats: calculateStats(readLatencies),
    missStats: calculateStats(missLatencies),
    cacheStats: stats,
  };
}

// =============================================================================
// 2. Query Cache Benchmark
// =============================================================================

async function benchmarkQueryCache() {
  console.log('\n' + '='.repeat(60));
  console.log('2. QUERY CACHE BENCHMARK');
  console.log('='.repeat(60));

  const { normalizeQuery, analyzeQueryPattern } = await import('../packages/oxigraph/src/query-cache.mjs');

  const queries = [
    'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
    'SELECT ?s WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?type FILTER(?s > 10) }',
    'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o OPTIONAL { ?s ?p2 ?o2 } }',
    'ASK { ?s ?p ?o }',
  ];

  // Benchmark: Query normalization
  const normLatencies = [];
  for (let i = 0; i < 1000; i++) {
    const query = queries[i % queries.length];
    const start = performance.now();
    normalizeQuery(query);
    normLatencies.push(performance.now() - start);
  }

  // Benchmark: Pattern analysis
  const analysisLatencies = [];
  for (let i = 0; i < 1000; i++) {
    const query = queries[i % queries.length];
    const start = performance.now();
    analyzeQueryPattern(query);
    analysisLatencies.push(performance.now() - start);
  }

  console.log('\nQuery Normalization Performance:');
  printStats('Normalization Latency', calculateStats(normLatencies));

  console.log('\nPattern Analysis Performance:');
  printStats('Analysis Latency', calculateStats(analysisLatencies));

  return {
    normStats: calculateStats(normLatencies),
    analysisStats: calculateStats(analysisLatencies),
  };
}

// =============================================================================
// 3. Receipt Batch Benchmark
// =============================================================================

async function benchmarkReceiptBatch() {
  console.log('\n' + '='.repeat(60));
  console.log('3. RECEIPT BATCH BENCHMARK');
  console.log('='.repeat(60));

  const { generateReceiptBatch, verifyReceiptBatch, resetPool } = await import('../packages/yawl/src/receipt-batch.mjs');
  const { RECEIPT_EVENT_TYPES } = await import('../packages/yawl/src/receipt.mjs');

  resetPool(1000);

  const batchSizes = [10, 100, 1000, 10000];
  const results = [];

  for (const size of batchSizes) {
    const events = Array(size).fill({
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: { data: 'test' },
    });

    const start = performance.now();
    const result = await generateReceiptBatch(events, { workers: 4, usePool: true });
    const elapsed = performance.now() - start;

    console.log(`\nBatch Size: ${size}`);
    console.log(`  Duration:     ${result.duration.toFixed(2)} ms`);
    console.log(`  Throughput:   ${result.throughput.toFixed(0)} receipts/sec`);
    console.log(`  Pool Reuse:   ${(result.stats.poolStats?.reuseRate * 100 || 0).toFixed(1)}%`);

    results.push({
      size,
      duration: result.duration,
      throughput: result.throughput,
      poolStats: result.stats.poolStats,
    });

    // Benchmark verification
    const verifyStart = performance.now();
    const verification = await verifyReceiptBatch(result.receipts, 4);
    const verifyElapsed = performance.now() - verifyStart;

    console.log(`  Verify Time:  ${verifyElapsed.toFixed(2)} ms`);
    console.log(`  Verify Valid: ${verification.valid}`);
  }

  return results;
}

// =============================================================================
// 4. Policy Compiler Benchmark
// =============================================================================

async function benchmarkPolicyCompiler() {
  console.log('\n' + '='.repeat(60));
  console.log('4. POLICY COMPILER BENCHMARK');
  console.log('='.repeat(60));

  const {
    PolicyPatterns,
    compilePolicy,
    compileHook,
    executeCompiledHook,
    batchValidateCompiled,
    resetCompilerStats,
    getCompilerStats,
  } = await import('../packages/hooks/src/policy-compiler.mjs');

  resetCompilerStats();

  // Benchmark: Policy compilation
  const policies = [
    { type: PolicyPatterns.ALLOW_ALL },
    { type: PolicyPatterns.SUBJECT_PATTERN, config: { pattern: /^http:\/\/example\.org/ } },
    { type: PolicyPatterns.PREDICATE_PATTERN, config: { pattern: /rdf:type/ } },
    { type: PolicyPatterns.NAMESPACE, config: { namespace: 'http://example.org/' } },
  ];

  const compileLatencies = [];
  for (let i = 0; i < 1000; i++) {
    const policy = policies[i % policies.length];
    const start = performance.now();
    compilePolicy(policy);
    compileLatencies.push(performance.now() - start);
  }

  console.log('\nPolicy Compilation Performance:');
  printStats('Compilation Latency', calculateStats(compileLatencies));

  // Benchmark: Hook execution
  const hook = {
    name: 'test-hook',
    validate: (quad) => quad.subject.value.length > 0,
    transform: (quad) => ({ ...quad, processed: true }),
  };

  const compiledHook = compileHook(hook);
  const quad = { subject: { value: 'http://example.org/test' } };

  const execLatencies = [];
  for (let i = 0; i < 10000; i++) {
    const start = performance.now();
    executeCompiledHook(compiledHook, quad);
    execLatencies.push(performance.now() - start);
  }

  console.log('\nHook Execution Performance:');
  printStats('Execution Latency', calculateStats(execLatencies), 'ms');

  // Benchmark: Batch validation
  const hooks = [compiledHook];
  const quads = Array(1000).fill(quad);

  const batchLatencies = [];
  for (let i = 0; i < 100; i++) {
    const start = performance.now();
    batchValidateCompiled(hooks, quads);
    batchLatencies.push(performance.now() - start);
  }

  console.log('\nBatch Validation Performance (1000 quads):');
  printStats('Batch Latency', calculateStats(batchLatencies));

  const stats = getCompilerStats();
  console.log('\nCompiler Statistics:');
  console.log(`  Cache Hit Rate:     ${(stats.cacheHitRate * 100).toFixed(1)}%`);
  console.log(`  Avg Compile Time:   ${stats.avgCompileTimeMs.toFixed(3)} ms`);
  console.log(`  Avg Eval Time:      ${stats.avgEvalTimeUs.toFixed(3)} µs`);

  return {
    compileStats: calculateStats(compileLatencies),
    execStats: calculateStats(execLatencies),
    batchStats: calculateStats(batchLatencies),
    compilerStats: stats,
  };
}

// =============================================================================
// Main Runner
// =============================================================================

async function main() {
  console.log('='.repeat(60));
  console.log('OPTIMIZATION BENCHMARKS SUITE');
  console.log('='.repeat(60));

  const results = {
    timestamp: new Date().toISOString(),
    benchmarks: {},
  };

  try {
    results.benchmarks.snapshotCache = await benchmarkSnapshotCache();
  } catch (error) {
    console.error('\n❌ Snapshot Cache benchmark failed:', error.message);
    results.benchmarks.snapshotCache = { error: error.message };
  }

  try {
    results.benchmarks.queryCache = await benchmarkQueryCache();
  } catch (error) {
    console.error('\n❌ Query Cache benchmark failed:', error.message);
    results.benchmarks.queryCache = { error: error.message };
  }

  try {
    results.benchmarks.receiptBatch = await benchmarkReceiptBatch();
  } catch (error) {
    console.error('\n❌ Receipt Batch benchmark failed:', error.message);
    results.benchmarks.receiptBatch = { error: error.message };
  }

  try {
    results.benchmarks.policyCompiler = await benchmarkPolicyCompiler();
  } catch (error) {
    console.error('\n❌ Policy Compiler benchmark failed:', error.message);
    results.benchmarks.policyCompiler = { error: error.message };
  }

  // Summary
  console.log('\n' + '='.repeat(60));
  console.log('SUMMARY');
  console.log('='.repeat(60));

  if (results.benchmarks.snapshotCache?.readStats) {
    console.log(`\n✅ Snapshot Cache: ${results.benchmarks.snapshotCache.readStats.p95.toFixed(3)} ms P95 read latency`);
    console.log(`   Target: <10ms, Hit Rate: ${(results.benchmarks.snapshotCache.cacheStats.hitRate * 100).toFixed(1)}%`);
  }

  if (results.benchmarks.queryCache?.normStats) {
    console.log(`\n✅ Query Cache: ${results.benchmarks.queryCache.normStats.p95.toFixed(3)} ms P95 normalization`);
    console.log(`   Target: <5ms for indexed queries`);
  }

  if (results.benchmarks.receiptBatch && Array.isArray(results.benchmarks.receiptBatch)) {
    const largest = results.benchmarks.receiptBatch[results.benchmarks.receiptBatch.length - 1];
    console.log(`\n✅ Receipt Batch: ${largest?.throughput?.toFixed(0) || 'N/A'} receipts/sec`);
    console.log(`   Target: 100K receipts/sec`);
  }

  if (results.benchmarks.policyCompiler?.compilerStats) {
    console.log(`\n✅ Policy Compiler: ${results.benchmarks.policyCompiler.compilerStats.avgEvalTimeUs.toFixed(3)} µs avg execution`);
    console.log(`   Target: <500µs P95 for hook execution`);
  }

  // JSON output for reports
  console.log('\n__JSON_RESULTS__');
  console.log(JSON.stringify(results, null, 2));
}

main().catch(console.error);
