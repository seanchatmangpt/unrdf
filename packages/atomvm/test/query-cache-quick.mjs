/**
 * Quick verification test for QueryCache
 * Run with: node packages/atomvm/test/query-cache-quick.mjs
 */

import { QueryCache, createQueryCache } from '../src/query-cache.mjs';

console.log('=== QueryCache Quick Verification ===\n');

let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`[PASS] ${name}`);
    passed++;
  } catch (e) {
    console.log(`[FAIL] ${name}: ${e.message}`);
    failed++;
  }
}

function assert(condition, message) {
  if (!condition) throw new Error(message || 'Assertion failed');
}

// Test 1: Constructor with defaults
test('Constructor with default options', () => {
  const cache = new QueryCache();
  assert(cache.maxSize === 100, 'maxSize should be 100');
  assert(cache.ttl === 60000, 'ttl should be 60000');
});

// Test 2: Constructor with custom options
test('Constructor with custom options', () => {
  const cache = new QueryCache({ maxSize: 50, ttl: 30000 });
  assert(cache.maxSize === 50, 'maxSize should be 50');
  assert(cache.ttl === 30000, 'ttl should be 30000');
});

// Test 3: Cache hit
test('Cache hit on repeated query', () => {
  const cache = new QueryCache({ maxSize: 10, ttl: 60000 });
  const query = 'SELECT ?s WHERE { ?s ?p ?o }';
  const result = { rows: [{ s: 'http://example.org/alice' }] };

  cache.set(query, {}, result);
  const cached = cache.get(query, {});

  assert(JSON.stringify(cached) === JSON.stringify(result), 'Should return cached result');
  assert(cache.stats().hits === 1, 'Should have 1 hit');
});

// Test 4: Cache miss
test('Cache miss on first query', () => {
  const cache = new QueryCache({ maxSize: 10, ttl: 60000 });
  const query = 'SELECT ?s WHERE { ?s ?p ?o }';

  const result = cache.get(query, {});

  assert(result === undefined, 'Should return undefined');
  assert(cache.stats().misses === 1, 'Should have 1 miss');
});

// Test 5: LRU eviction
test('LRU eviction when maxSize exceeded', () => {
  const cache = new QueryCache({ maxSize: 3, ttl: 60000 });

  cache.set('query1', {}, { result: 1 });
  cache.set('query2', {}, { result: 2 });
  cache.set('query3', {}, { result: 3 });
  cache.set('query4', {}, { result: 4 });

  assert(cache.size === 3, 'Size should be 3');
  assert(cache.get('query1', {}) === undefined, 'query1 should be evicted');
  assert(cache.get('query4', {}) !== undefined, 'query4 should exist');
});

// Test 6: TTL expiration
test('TTL expiration', async () => {
  const cache = new QueryCache({ maxSize: 10, ttl: 50 });
  const query = 'SELECT ?s WHERE { ?s ?p ?o }';

  cache.set(query, {}, { result: 'test' });

  // Wait for expiration
  await new Promise(resolve => setTimeout(resolve, 100));

  const result = cache.get(query, {});
  assert(result === undefined, 'Should return undefined after TTL');
});

// Test 7: invalidateAll
test('invalidateAll clears cache', () => {
  const cache = new QueryCache({ maxSize: 10, ttl: 60000 });

  cache.set('query1', {}, { result: 1 });
  cache.set('query2', {}, { result: 2 });
  cache.set('query3', {}, { result: 3 });

  const cleared = cache.invalidateAll();

  assert(cleared === 3, 'Should return 3');
  assert(cache.size === 0, 'Size should be 0');
});

// Test 8: Stats
test('Stats tracking', () => {
  const cache = new QueryCache({ maxSize: 10, ttl: 60000 });

  cache.set('query1', {}, { result: 1 });

  // 2 hits
  cache.get('query1', {});
  cache.get('query1', {});

  // 3 misses
  cache.get('query2', {});
  cache.get('query3', {});
  cache.get('query4', {});

  const stats = cache.stats();
  assert(stats.hits === 2, 'Should have 2 hits');
  assert(stats.misses === 3, 'Should have 3 misses');
  assert(Math.abs(stats.hitRate - 0.4) < 0.01, 'Hit rate should be 0.4');
});

// Test 9: Performance - lookup <1ms
test('Performance: cache lookup <1ms', () => {
  const cache = new QueryCache({ maxSize: 1000, ttl: 60000 });

  // Pre-populate
  for (let i = 0; i < 1000; i++) {
    cache.set(`SELECT * WHERE { ?s ?p ${i} }`, {}, { result: i });
  }

  // Measure lookup time
  const start = performance.now();
  for (let i = 0; i < 1000; i++) {
    cache.get(`SELECT * WHERE { ?s ?p ${i % 1000} }`, {});
  }
  const duration = performance.now() - start;
  const avgLookup = duration / 1000;

  assert(avgLookup < 1, `Average lookup should be <1ms, got ${avgLookup.toFixed(4)}ms`);
});

// Test 10: Hit rate benchmark
test('Benchmark: >80% hit rate', () => {
  const cache = new QueryCache({ maxSize: 100, ttl: 60000 });
  const queries = [];

  // Generate 20 unique queries
  for (let i = 0; i < 20; i++) {
    queries.push(`SELECT * WHERE { ?s ?p ${i} }`);
  }

  // Pre-populate
  for (const q of queries) {
    cache.set(q, {}, { result: q });
  }

  // Run 1000 queries with Zipf-like distribution
  for (let i = 0; i < 1000; i++) {
    const queryIndex = Math.random() < 0.8
      ? Math.floor(Math.random() * 4)
      : Math.floor(Math.random() * 20);
    cache.get(queries[queryIndex], {});
  }

  const stats = cache.stats();
  assert(stats.hitRate > 0.8, `Hit rate should be >80%, got ${(stats.hitRate * 100).toFixed(1)}%`);
});

// Test 11: createQueryCache factory
test('createQueryCache factory', () => {
  const cache = createQueryCache({ maxSize: 25 });
  assert(cache instanceof QueryCache, 'Should be QueryCache instance');
  assert(cache.maxSize === 25, 'maxSize should be 25');
});

// Test 12: Different bindings are different cache entries
test('Different bindings create different entries', () => {
  const cache = new QueryCache({ maxSize: 10, ttl: 60000 });
  const query = 'SELECT ?name WHERE { ?s foaf:name ?name }';

  cache.set(query, { s: 'alice' }, { name: 'Alice' });
  cache.set(query, { s: 'bob' }, { name: 'Bob' });

  assert(cache.get(query, { s: 'alice' }).name === 'Alice', 'Should get Alice');
  assert(cache.get(query, { s: 'bob' }).name === 'Bob', 'Should get Bob');
  assert(cache.size === 2, 'Size should be 2');
});

// Test 13: Validation errors
test('Validation errors for invalid input', () => {
  const cache = new QueryCache();

  try {
    cache.get('');
    assert(false, 'Should throw');
  } catch (e) {
    assert(e.message.includes('non-empty string'), 'Should throw validation error');
  }

  try {
    cache.set('', {}, {});
    assert(false, 'Should throw');
  } catch (e) {
    assert(e.message.includes('non-empty string'), 'Should throw validation error');
  }
});

console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
process.exit(failed > 0 ? 1 : 0);
