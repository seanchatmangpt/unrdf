/**
 * @file Query Optimizer Example
 * @description Demonstrates createQueryOptimizer — SPARQL query normalization,
 *   predicate index creation, LRU cache, and statistics.
 *
 * API demonstrated: packages/hooks/src/hooks/query-optimizer.mjs
 * Docs: packages/hooks/README.md — "Query Optimizer" section (added by this triple)
 *
 * createQueryOptimizer returns a pure optimizer object that:
 * - normalize()s whitespace in SPARQL strings
 * - createIndexes() builds predicate-keyed index over any quad iterable
 * - cacheResult() / getCachedResult() implement an LRU cache with TTL
 * - getStats() exposes queriesOptimized, cacheHits, cacheMisses, cacheHitRate
 *
 * EXPECTED OUTPUT:
 *   [optimize] whitespace collapses; query count increments
 *   [createIndexes] predicate index created from quad array
 *   [cache] getCachedResult returns stored value; stale entry returns null
 *   [stats] cacheHitRate = hits / (hits + misses)
 */

import { createQueryOptimizer } from '../packages/hooks/src/hooks/query-optimizer.mjs';

// ─── 1. optimize() — SPARQL string normalization ─────────────────────────────

const optimizer = createQueryOptimizer({
  enableIndexing: true,
  enableCaching: true,
  cacheMaxSize: 10,
});

const raw = `  SELECT   *  WHERE {
    ?s   a   ?type .
  }  `;
const optimized = optimizer.optimize(raw);

if (typeof optimized !== 'string') throw new Error('optimize() must return a string');
if (optimized.includes('  ')) throw new Error('optimize() must collapse double spaces');
if (!optimized.includes('SELECT')) throw new Error('optimize() must preserve SELECT keyword');

const stats1 = optimizer.getStats();
if (stats1.queriesOptimized !== 1) throw new Error(`Expected 1 query optimized, got ${stats1.queriesOptimized}`);

console.log(`[optimize] ✅ raw ${raw.length} chars → optimized ${optimized.length} chars`);
console.log(`[optimize] ✅ queriesOptimized=${stats1.queriesOptimized}`);

// ─── 2. createIndexes() — predicate-keyed index over quad array ───────────────

const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const FOAF_KNOWS = 'http://xmlns.com/foaf/0.1/knows';
const ALICE = 'https://example.org/Alice';
const BOB = 'https://example.org/Bob';

function quad(s, p, o) {
  return {
    subject: { value: s },
    predicate: { value: p },
    object: { value: o },
  };
}

const quads = [
  quad(ALICE, RDF_TYPE, 'http://xmlns.com/foaf/0.1/Person'),
  quad(BOB, RDF_TYPE, 'http://xmlns.com/foaf/0.1/Person'),
  quad(ALICE, FOAF_KNOWS, BOB),
];

const createdIndexes = await optimizer.createIndexes(quads);
if (!Array.isArray(createdIndexes)) throw new Error('createIndexes() must return an array');
if (createdIndexes.length === 0) throw new Error('createIndexes() must return at least 1 index name');
if (!createdIndexes.includes('predicate')) throw new Error(`Expected 'predicate' index, got: ${createdIndexes}`);

const stats2 = optimizer.getStats();
if (stats2.indexesCreated !== 1) throw new Error(`Expected 1 index created, got ${stats2.indexesCreated}`);

console.log(`[createIndexes] ✅ created: [${createdIndexes.join(', ')}], indexCount=${stats2.indexCount}`);

// ─── 3. updateIndexes() — add and remove quads from existing index ────────────

const carol = 'https://example.org/Carol';
await optimizer.updateIndexes({
  additions: [quad(BOB, FOAF_KNOWS, carol)],
  deletions: [quad(ALICE, FOAF_KNOWS, BOB)],
});

// No exception = updateIndexes handled gracefully
console.log(`[updateIndexes] ✅ delta applied without error`);

// ─── 4. cacheResult / getCachedResult — LRU cache with TTL ───────────────────

const sparqlQuery = 'SELECT * WHERE { ?s a ?type }';
const mockResult = [{ s: ALICE, type: 'foaf:Person' }];

optimizer.cacheResult(sparqlQuery, mockResult);

// Immediate retrieval: within TTL (default 60s)
const cached = optimizer.getCachedResult(sparqlQuery, 60000);
if (!Array.isArray(cached)) throw new Error('getCachedResult should return the cached array');
if (cached.length !== 1) throw new Error(`Expected 1 result in cache, got ${cached.length}`);
if (cached[0].s !== ALICE) throw new Error(`Expected Alice in cached result, got ${cached[0].s}`);

console.log(`[cache] ✅ getCachedResult returned ${cached.length} result(s)`);

// Stale retrieval: maxAge=0 forces TTL expiry check
const stale = optimizer.getCachedResult(sparqlQuery, 0);
if (stale !== null) throw new Error('getCachedResult with maxAge=0 must return null (stale)');

console.log(`[cache] ✅ maxAge=0 → stale entry returns null`);

// ─── 5. getStats() — hit rate calculation ─────────────────────────────────────

optimizer.cacheResult('query-a', []);
optimizer.getCachedResult('query-a', 60000); // hit
optimizer.getCachedResult('query-b', 60000); // miss

const stats3 = optimizer.getStats();
if (typeof stats3.cacheHitRate !== 'number') throw new Error('cacheHitRate must be a number');
if (stats3.cacheHits < 1) throw new Error(`Expected at least 1 cache hit, got ${stats3.cacheHits}`);
if (stats3.cacheMisses < 1) throw new Error(`Expected at least 1 cache miss, got ${stats3.cacheMisses}`);
if (stats3.cacheHitRate <= 0 || stats3.cacheHitRate >= 1) {
  throw new Error(`Expected cacheHitRate between 0 and 1, got ${stats3.cacheHitRate}`);
}

console.log(`[stats] ✅ hits=${stats3.cacheHits}, misses=${stats3.cacheMisses}, hitRate=${stats3.cacheHitRate.toFixed(2)}`);

// ─── 6. clear() — resets cache and indexes ───────────────────────────────────

optimizer.clear();
const statsAfterClear = optimizer.getStats();
if (statsAfterClear.cacheSize !== 0) throw new Error(`Expected cacheSize=0 after clear(), got ${statsAfterClear.cacheSize}`);
if (statsAfterClear.indexCount !== 0) throw new Error(`Expected indexCount=0 after clear(), got ${statsAfterClear.indexCount}`);

console.log(`[clear] ✅ cache and indexes reset: cacheSize=${statsAfterClear.cacheSize}, indexCount=${statsAfterClear.indexCount}`);

// ─── 7. Forgery witness — cacheMaxSize enforces eviction ─────────────────────

const smallOptimizer = createQueryOptimizer({ cacheMaxSize: 3 });
for (let i = 0; i < 5; i++) {
  smallOptimizer.cacheResult(`query-${i}`, [i]);
}
const s = smallOptimizer.getStats();
if (s.cacheSize > 3) {
  throw new Error(`cacheMaxSize=3 must evict entries — got cacheSize=${s.cacheSize}`);
}

console.log(`[forgery witness] ✅ cacheMaxSize=3 enforced: cacheSize=${s.cacheSize} (≤3)`);

console.log('\n✅ All createQueryOptimizer assertions passed.');
