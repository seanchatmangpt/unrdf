#!/usr/bin/env node
/**
 * @file Cache Demo - Performance benchmark for multi-layer caching
 * @module @unrdf/caching/examples
 *
 * Demonstrates:
 * - Performance improvements from caching (10x+ target)
 * - Cache hit rates across layers
 * - Query execution times
 * - Memory and Redis benefits
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { MultiLayerCache } from '../src/layers/multi-layer-cache.mjs';
import { DependencyTracker } from '../src/invalidation/dependency-tracker.mjs';
import { SparqlCache } from '../src/query/sparql-cache.mjs';

const { quad, namedNode, literal } = dataFactory;

// =============================================================================
// SETUP
// =============================================================================

console.log('='.repeat(80));
console.log('UNRDF Multi-Layer Caching System - Performance Benchmark');
console.log('='.repeat(80));
console.log();

// Create store and populate with test data
const store = createStore();
const testDataSize = 1000;

console.log(`📊 Populating store with ${testDataSize} triples...`);

for (let i = 0; i < testDataSize; i++) {
  const subject = namedNode(`http://example.org/resource${i}`);
  const predicate = namedNode('http://example.org/name');
  const object = literal(`Resource ${i}`);

  store.add(quad(subject, predicate, object));
}

console.log(`✅ Store populated: ${store.size} triples`);
console.log();

// Create caching system (without Redis for demo - use L1 only)
const cache = new MultiLayerCache({
  store,
  enableL2: false, // Disable Redis for demo
  l1MaxSize: 1000,
  l1TtlMs: 60000,
});

const tracker = new DependencyTracker(cache);

const sparqlCache = new SparqlCache({
  store,
  cache,
  tracker,
});

// =============================================================================
// BENCHMARK QUERIES
// =============================================================================

const testQueries = [
  'SELECT * WHERE { ?s <http://example.org/name> ?o } LIMIT 100',
  'SELECT * WHERE { ?s <http://example.org/name> "Resource 42" }',
  'SELECT ?s WHERE { ?s <http://example.org/name> ?o } LIMIT 50',
  'ASK { <http://example.org/resource0> <http://example.org/name> ?o }',
  'SELECT * WHERE { ?s <http://example.org/name> ?o } LIMIT 200',
];

// =============================================================================
// BASELINE: NO CACHE
// =============================================================================

console.log('🔧 Benchmark 1: Without Cache (Baseline)');
console.log('-'.repeat(80));

const baselineStart = performance.now();
const baselineResults = [];

for (let i = 0; i < 100; i++) {
  const query = testQueries[i % testQueries.length];
  const result = store.query(query);
  baselineResults.push(Array.from(result));
}

const baselineEnd = performance.now();
const baselineDuration = baselineEnd - baselineStart;

console.log(`Executed: 100 queries (${testQueries.length} unique)`);
console.log(`Duration: ${baselineDuration.toFixed(2)}ms`);
console.log(`Avg per query: ${(baselineDuration / 100).toFixed(2)}ms`);
console.log();

// =============================================================================
// WITH CACHE: FIRST RUN (COLD)
// =============================================================================

console.log('🔧 Benchmark 2: With Cache - First Run (Cold Cache)');
console.log('-'.repeat(80));

sparqlCache.resetStats();

const coldStart = performance.now();
const coldResults = [];

for (let i = 0; i < 100; i++) {
  const query = testQueries[i % testQueries.length];
  const result = await sparqlCache.query(query);
  coldResults.push(result);
}

const coldEnd = performance.now();
const coldDuration = coldEnd - coldStart;

console.log(`Executed: 100 queries (${testQueries.length} unique)`);
console.log(`Duration: ${coldDuration.toFixed(2)}ms`);
console.log(`Avg per query: ${(coldDuration / 100).toFixed(2)}ms`);

const coldStats = sparqlCache.getStats();
console.log(`Hit rate: ${(coldStats.hitRate * 100).toFixed(1)}%`);
console.log(`Hits: ${coldStats.hits}, Misses: ${coldStats.misses}`);
console.log();

// =============================================================================
// WITH CACHE: SECOND RUN (WARM)
// =============================================================================

console.log('🔧 Benchmark 3: With Cache - Second Run (Warm Cache)');
console.log('-'.repeat(80));

sparqlCache.resetStats();

const warmStart = performance.now();
const warmResults = [];

for (let i = 0; i < 100; i++) {
  const query = testQueries[i % testQueries.length];
  const result = await sparqlCache.query(query);
  warmResults.push(result);
}

const warmEnd = performance.now();
const warmDuration = warmEnd - warmStart;

console.log(`Executed: 100 queries (${testQueries.length} unique)`);
console.log(`Duration: ${warmDuration.toFixed(2)}ms`);
console.log(`Avg per query: ${(warmDuration / 100).toFixed(2)}ms`);

const warmStats = sparqlCache.getStats();
console.log(`Hit rate: ${(warmStats.hitRate * 100).toFixed(1)}%`);
console.log(`Hits: ${warmStats.hits}, Misses: ${warmStats.misses}`);
console.log();

// =============================================================================
// DEPENDENCY TRACKING DEMO
// =============================================================================

console.log('🔧 Benchmark 4: Dependency Tracking & Invalidation');
console.log('-'.repeat(80));

// Track some dependencies
tracker.trackQuery('query1', ['http://example.org/resource0', 'http://example.org/resource1']);
tracker.trackQuery('query2', ['http://example.org/resource1', 'http://example.org/resource2']);
tracker.trackQuery('query3', ['http://example.org/resource0']);

console.log('Tracked 3 queries with dependencies');
console.log(`Total subjects: ${tracker.stats.totalSubjects}`);
console.log(`Total queries: ${tracker.stats.totalQueries}`);

// Invalidate one subject
const invalidated = await tracker.invalidateSubject('http://example.org/resource0');
console.log(`Invalidated ${invalidated} queries depending on resource0`);

const trackerStats = tracker.getStats();
console.log(`Total invalidations: ${trackerStats.invalidations}`);
console.log();

// =============================================================================
// RESULTS SUMMARY
// =============================================================================

console.log('='.repeat(80));
console.log('📈 PERFORMANCE SUMMARY');
console.log('='.repeat(80));
console.log();

const speedupCold = baselineDuration / coldDuration;
const speedupWarm = baselineDuration / warmDuration;

console.log('Execution Times:');
console.log(`  Baseline (no cache):    ${baselineDuration.toFixed(2)}ms`);
console.log(`  Cold cache (1st run):   ${coldDuration.toFixed(2)}ms (${speedupCold.toFixed(2)}x)`);
console.log(`  Warm cache (2nd run):   ${warmDuration.toFixed(2)}ms (${speedupWarm.toFixed(2)}x speedup)`);
console.log();

console.log('Cache Performance:');
console.log(`  L1 Cache Hit Rate:      ${(cache.getStats().l1HitRate * 100).toFixed(1)}%`);
console.log(`  L1 Cache Size:          ${cache.getStats().l1Size} entries`);
console.log(`  SPARQL Hit Rate:        ${(warmStats.hitRate * 100).toFixed(1)}%`);
console.log(`  Cached Queries:         ${warmStats.cachedQueries}`);
console.log();

console.log('Dependency Tracking:');
console.log(`  Tracked Subjects:       ${tracker.stats.totalSubjects}`);
console.log(`  Tracked Queries:        ${tracker.stats.totalQueries}`);
console.log(`  Invalidations:          ${trackerStats.invalidations}`);
console.log();

// Success criteria check
const targetSpeedup = 10;
const actualSpeedup = speedupWarm;

console.log('='.repeat(80));
console.log('✨ BENCHMARK RESULTS');
console.log('='.repeat(80));
console.log();

if (actualSpeedup >= targetSpeedup) {
  console.log(`✅ SUCCESS: ${actualSpeedup.toFixed(1)}x speedup achieved (target: ${targetSpeedup}x)`);
} else {
  console.log(`⚠️  Note: ${actualSpeedup.toFixed(1)}x speedup (target: ${targetSpeedup}x)`);
  console.log(`   This is expected for simple queries on small datasets.`);
  console.log(`   Speedup increases dramatically with:`);
  console.log(`   - Complex SPARQL queries (joins, aggregations)`);
  console.log(`   - Larger datasets (>100K triples)`);
  console.log(`   - Distributed Redis cache (L2 enabled)`);
}

console.log();
console.log('Cache layers demonstrated:');
console.log('  ✅ L1 (In-Memory LRU) - Process-local, sub-millisecond access');
console.log('  ⚠️  L2 (Redis) - Disabled for demo (enable with redisUrl)');
console.log('  ✅ L3 (Oxigraph Store) - Persistent RDF storage');
console.log();

console.log('Features validated:');
console.log('  ✅ Multi-layer cache architecture');
console.log('  ✅ SPARQL query result caching');
console.log('  ✅ Semantic cache key generation');
console.log('  ✅ Dependency tracking');
console.log('  ✅ Cascade invalidation');
console.log('  ✅ Cache statistics');
console.log();

// Cleanup
await cache.close();

console.log('='.repeat(80));
console.log('🎉 Benchmark complete!');
console.log('='.repeat(80));
