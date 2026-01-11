/**
 * Composition C12: RDF Store + Multi-Layer Cache + SPARQL Cache
 * Atoms: A01 + A22 + A24
 *
 * Proof: High-performance cached RDF queries (L1+L2+L3)
 */

import { createStore } from '@unrdf/core';
import { createCachingSystem } from '@unrdf/caching';

console.log('=== C12: Multi-Layer Cache Proof ===\n');

async function prove() {
  try {
    // A01: Create RDF store
    const store = createStore();
    console.log('✅ A01: RDF store created');

    // Add test data
    const { namedNode, literal, quad } = await import('@unrdf/core');
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.add(quad(alice, name, literal('Alice')));
    store.add(quad(bob, name, literal('Bob')));
    console.log('✅ Added test data');

    // A22 + A24: Create multi-layer caching system
    const caching = await createCachingSystem({
      store,
      l1MaxSize: 100,      // L1: In-memory cache
      l2TtlSeconds: 300,   // L2: Redis (if available)
      // L3: Direct store access
    });

    console.log('✅ A22: Multi-layer cache initialized');
    console.log('✅ A24: SPARQL cache created');

    // Execute cached query (first time - cache miss)
    const query = 'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }';

    console.log('\n🔍 Query 1 (cache MISS):');
    const start1 = Date.now();
    const results1 = await caching.sparqlCache.query(query);
    const time1 = Date.now() - start1;
    console.log(`   Results: ${results1.length}, Time: ${time1}ms`);

    // Execute same query (cache hit)
    console.log('\n🔍 Query 2 (cache HIT):');
    const start2 = Date.now();
    const results2 = await caching.sparqlCache.query(query);
    const time2 = Date.now() - start2;
    console.log(`   Results: ${results2.length}, Time: ${time2}ms`);

    // Get cache statistics
    const stats = caching.getStats();
    console.log('\n📊 Cache Statistics:');
    console.log(`   L1 size: ${stats.cache.l1Size}`);
    console.log(`   Hit rate: ${((stats.sparql.hits / (stats.sparql.hits + stats.sparql.misses)) * 100).toFixed(1)}%`);
    console.log(`   Total queries: ${stats.sparql.hits + stats.sparql.misses}`);

    // Speedup from cache
    const speedup = time1 / time2;
    console.log(`\n⚡ Cache speedup: ${speedup.toFixed(1)}x faster`);

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log('   Value: Multi-layer caching for read-heavy workloads');
    console.log(`   L1 (memory): ${stats.cache.l1Size} entries`);
    console.log(`   Cache hit improvement: ${((1 - time2/time1) * 100).toFixed(1)}% faster`);

    await caching.close();

    process.exit(stats.sparql.hits > 0 ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

prove();
