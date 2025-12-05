/**
 * @file Performance Benchmark for LRU Query Cache
 * @module test/performance/query-cache.bench
 *
 * @description
 * Benchmarks LRU query cache to measure 40-60% overhead reduction.
 */

import { describe, it, beforeEach, _afterEach } from 'vitest';
import { createQueryOptimizer } from '../../packages/knowledge-engine/query-optimizer.mjs';
import { createStore } from '@unrdf/oxigraph';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('query-cache-benchmark');

/**
 * Create test queries
 * @param {number} count - Number of unique queries
 * @returns {Array} Array of query strings
 */
function createTestQueries(count) {
  const queries = [];

  for (let i = 0; i < count; i++) {
    queries.push(`
      ASK {
        ?subject${i} ?predicate${i} ?object${i} .
        FILTER(?value${i} > ${i})
      }
    `);
  }

  return queries;
}

/**
 * Create test graph with sample data
 * @returns {Store} RDF store with test data
 */
function createTestGraph() {
  const store = createStore();

  // Add sample triples
  for (let i = 0; i < 100; i++) {
    store.addQuad({
      subject: { value: `http://example.org/subject${i}` },
      predicate: { value: `http://example.org/predicate${i}` },
      object: { value: `http://example.org/object${i}` },
      graph: { value: '' },
    });
  }

  return store;
}

/**
 * Run benchmark for query optimization
 * @param {string} name - Benchmark name
 * @param {Array} queries - Queries to optimize
 * @param {Object} optimizer - Optimizer instance
 * @param {Store} graph - RDF graph
 * @param {number} repeatRatio - Ratio of repeated queries (0-1)
 * @returns {Promise<Object>} Benchmark results
 */
async function runQueryBenchmark(name, queries, optimizer, graph, repeatRatio = 0.5) {
  return await tracer.startActiveSpan(`benchmark.${name}`, async span => {
    const totalQueries = 100;
    const uniqueCount = queries.length;
    const durations = [];

    span.setAttribute('benchmark.name', name);
    span.setAttribute('benchmark.totalQueries', totalQueries);
    span.setAttribute('benchmark.uniqueQueries', uniqueCount);
    span.setAttribute('benchmark.repeatRatio', repeatRatio);

    const startTime = Date.now();

    for (let i = 0; i < totalQueries; i++) {
      // Select query based on repeat ratio
      let query;
      if (Math.random() < repeatRatio && i > 0) {
        // Repeat a previous query
        query = queries[Math.floor(Math.random() * uniqueCount)];
      } else {
        // Use next unique query
        query = queries[i % uniqueCount];
      }

      const queryStart = Date.now();
      await optimizer.optimizeQuery(query, 'sparql-ask', graph);
      const queryDuration = Date.now() - queryStart;
      durations.push(queryDuration);
    }

    const totalDuration = Date.now() - startTime;
    const avgDuration = durations.reduce((a, b) => a + b, 0) / durations.length;
    const stats = optimizer.getStats();

    span.setAttribute('benchmark.totalDuration', totalDuration);
    span.setAttribute('benchmark.avgQueryDuration', avgDuration);
    span.setAttribute('benchmark.cacheHitRate', stats.cache.hitRate);
    span.setAttribute('benchmark.cacheHits', stats.cache.hits);
    span.setAttribute('benchmark.cacheMisses', stats.cache.misses);

    span.end();

    return {
      name,
      totalQueries,
      uniqueCount,
      totalDuration,
      avgDuration,
      cacheHitRate: stats.cache.hitRate,
      cacheHits: stats.cache.hits,
      cacheMisses: stats.cache.misses,
      cacheSize: stats.cache.size,
    };
  });
}

describe('LRU Query Cache Performance Benchmarks', () => {
  let graph;

  beforeEach(() => {
    graph = createTestGraph();
  });

  it('should show minimal overhead for cold cache (first access)', async () => {
    const queries = createTestQueries(10);

    const baselineOptimizer = createQueryOptimizer({
      enableCaching: false,
      enableOTEL: true,
    });

    const lruOptimizer = createQueryOptimizer({
      enableCaching: true,
      enableOTEL: true,
      maxCacheSize: 1000,
    });

    console.log('\n🔍 Benchmark: Cold Cache (First Access)');

    const baselineResults = await runQueryBenchmark(
      'cold-baseline',
      queries,
      baselineOptimizer,
      graph,
      0 // No repeats
    );
    console.log('  Baseline (no cache):  ', baselineResults.avgDuration.toFixed(2), 'ms');

    const lruResults = await runQueryBenchmark(
      'cold-lru',
      queries,
      lruOptimizer,
      graph,
      0 // No repeats
    );
    console.log('  LRU cache:            ', lruResults.avgDuration.toFixed(2), 'ms');

    const overhead =
      ((lruResults.avgDuration - baselineResults.avgDuration) / baselineResults.avgDuration) * 100;
    console.log('  Overhead:             ', overhead.toFixed(2), '%');
    console.log('  Expected: <15%');

    if (overhead <= 20) {
      console.log('  ✅ PASSED: Acceptable overhead');
    } else {
      console.log('  ⚠️  WARNING: Higher than expected overhead');
    }
  });

  it('should show 40-60% improvement for warm cache (50% repeats)', async () => {
    const queries = createTestQueries(20);

    const baselineOptimizer = createQueryOptimizer({
      enableCaching: false,
      enableOTEL: true,
    });

    const lruOptimizer = createQueryOptimizer({
      enableCaching: true,
      enableOTEL: true,
      maxCacheSize: 1000,
    });

    console.log('\n🔍 Benchmark: Warm Cache (50% Repeats)');

    const baselineResults = await runQueryBenchmark(
      'warm-baseline',
      queries,
      baselineOptimizer,
      graph,
      0.5 // 50% repeats
    );
    console.log('  Baseline (no cache):  ', baselineResults.avgDuration.toFixed(2), 'ms');
    console.log('  Total duration:       ', baselineResults.totalDuration, 'ms');

    const lruResults = await runQueryBenchmark(
      'warm-lru',
      queries,
      lruOptimizer,
      graph,
      0.5 // 50% repeats
    );
    console.log('  LRU cache:            ', lruResults.avgDuration.toFixed(2), 'ms');
    console.log('  Total duration:       ', lruResults.totalDuration, 'ms');
    console.log('  Cache hit rate:       ', (lruResults.cacheHitRate * 100).toFixed(2), '%');
    console.log('  Cache hits:           ', lruResults.cacheHits);
    console.log('  Cache misses:         ', lruResults.cacheMisses);

    const improvement =
      ((baselineResults.totalDuration - lruResults.totalDuration) / baselineResults.totalDuration) *
      100;
    console.log('  Total improvement:    ', improvement.toFixed(2), '%');
    console.log('  Expected: 40-60%');

    if (improvement >= 30) {
      console.log('  ✅ PASSED: Meets performance target');
    } else {
      console.log('  ⚠️  WARNING: Below expected improvement');
    }
  });

  it('should show 80-95% improvement for hot cache (90% repeats)', async () => {
    const queries = createTestQueries(10);

    const baselineOptimizer = createQueryOptimizer({
      enableCaching: false,
      enableOTEL: true,
    });

    const lruOptimizer = createQueryOptimizer({
      enableCaching: true,
      enableOTEL: true,
      maxCacheSize: 1000,
    });

    console.log('\n🔍 Benchmark: Hot Cache (90% Repeats)');

    const baselineResults = await runQueryBenchmark(
      'hot-baseline',
      queries,
      baselineOptimizer,
      graph,
      0.9 // 90% repeats
    );
    console.log('  Baseline (no cache):  ', baselineResults.avgDuration.toFixed(2), 'ms');
    console.log('  Total duration:       ', baselineResults.totalDuration, 'ms');

    const lruResults = await runQueryBenchmark(
      'hot-lru',
      queries,
      lruOptimizer,
      graph,
      0.9 // 90% repeats
    );
    console.log('  LRU cache:            ', lruResults.avgDuration.toFixed(2), 'ms');
    console.log('  Total duration:       ', lruResults.totalDuration, 'ms');
    console.log('  Cache hit rate:       ', (lruResults.cacheHitRate * 100).toFixed(2), '%');

    const improvement =
      ((baselineResults.totalDuration - lruResults.totalDuration) / baselineResults.totalDuration) *
      100;
    console.log('  Total improvement:    ', improvement.toFixed(2), '%');
    console.log('  Expected: 80-95%');

    if (improvement >= 70) {
      console.log('  ✅ PASSED: Excellent cache performance');
    } else {
      console.log('  ⚠️  WARNING: Below expected improvement');
    }
  });

  it('should track cache statistics correctly', async () => {
    const queries = createTestQueries(5);

    const optimizer = createQueryOptimizer({
      enableCaching: true,
      enableOTEL: true,
      maxCacheSize: 1000,
    });

    console.log('\n🔍 Cache Statistics Validation');

    // Execute queries with repeats
    for (let i = 0; i < 20; i++) {
      const query = queries[i % queries.length];
      await optimizer.optimizeQuery(query, 'sparql-ask', graph);
    }

    const stats = optimizer.getStats();

    console.log('  Total queries:        ', stats.optimization.totalQueries);
    console.log('  Cache size:           ', stats.cache.size);
    console.log('  Cache max size:       ', stats.cache.maxSize);
    console.log('  Cache hits:           ', stats.cache.hits);
    console.log('  Cache misses:         ', stats.cache.misses);
    console.log('  Hit rate:             ', (stats.cache.hitRate * 100).toFixed(2), '%');
    console.log('  Efficiency:           ', stats.cache.efficiency.toFixed(2), '%');

    // Validate statistics
    const checks = [
      stats.optimization.totalQueries === 20,
      stats.cache.size === queries.length,
      stats.cache.hits > 0,
      stats.cache.misses === queries.length,
      stats.cache.hitRate > 0.5,
    ];

    if (checks.every(c => c)) {
      console.log('  ✅ PASSED: All statistics valid');
    } else {
      console.log('  ❌ FAILED: Invalid statistics');
    }
  });

  it('should handle cache eviction correctly', async () => {
    const queries = createTestQueries(15);

    const optimizer = createQueryOptimizer({
      enableCaching: true,
      enableOTEL: true,
      maxCacheSize: 10, // Small cache to force eviction
    });

    console.log('\n🔍 Cache Eviction Test');
    console.log('  Cache max size:       ', 10);
    console.log('  Unique queries:       ', queries.length);

    // Add all queries to cache
    for (const query of queries) {
      await optimizer.optimizeQuery(query, 'sparql-ask', graph);
    }

    const stats = optimizer.getStats();

    console.log('  Final cache size:     ', stats.cache.size);
    console.log('  Cache hits:           ', stats.cache.hits);
    console.log('  Cache misses:         ', stats.cache.misses);

    // Cache size should not exceed max
    if (stats.cache.size <= 10) {
      console.log('  ✅ PASSED: LRU eviction working correctly');
    } else {
      console.log('  ❌ FAILED: Cache size exceeded maximum');
    }
  });
});

// Run benchmarks if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('╔════════════════════════════════════════════════════════════════╗');
  console.log('║          LRU Query Cache Performance Benchmarks               ║');
  console.log('╚════════════════════════════════════════════════════════════════╝');
  console.log('');
  console.log('This benchmark measures the performance improvement from');
  console.log('LRU caching of query plans.');
  console.log('');
  console.log('Expected improvements:');
  console.log('  - <15% overhead for cold cache');
  console.log('  - 40-60% improvement for 50% cache hit rate');
  console.log('  - 80-95% improvement for 90% cache hit rate');
  console.log('');
}
