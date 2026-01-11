import { describe, test, expect } from 'vitest';

import { TemporalSPARQL } from '../../src/temporal-sparql.mjs'
import { KGCStore } from '../../src/store.mjs'
import { GitBackbone } from '../../src/git.mjs'

describe('Doctests: temporal-sparql.mjs', () => {
  test('unknown example 1 (line 1)', async () => {
    const store = new KGCStore();
const git = new GitBackbone('./repo');
const temporal = new TemporalSPARQL(store, git);
const results = await temporal.query(`
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
AT TIMESTAMP '2026-01-01T00:00:00Z'
`);
  });

  test('TemporalSPARQL example 2 (line 39)', async () => {
    const temporal = new TemporalSPARQL(store, git, {
cache: { maxSize: 500, ttl: 600000 },
enableOTEL: true
});
  });

  test('startTime example 3 (line 97)', async () => {
    const results = await temporal.query(`
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
AT TIMESTAMP '2026-01-01T00:00:00Z'
`);
  });

  test('results example 4 (line 143)', async () => {
    const results = await temporal.queryAtTime(
'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
'2026-01-01T00:00:00Z'
);
  });

  test('startTimestamp example 5 (line 300)', async () => {
    const results = await temporal.queryBetween(
'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
'2026-01-01T00:00:00Z',
'2026-01-02T00:00:00Z'
);
  });

  test('avgQueryTime example 6 (line 322)', async () => {
    const stats = temporal.getStats();
console.log(`Cache hit rate: ${stats.cache.hitRate}%`);
  });

  test('unknown example 7 (line 345)', async () => {
    temporal.resetStats();
  });

  test('unknown example 8 (line 358)', async () => {
    temporal.clearCache();
  });

  test('startNs example 9 (line 369)', async () => {
    await temporal.prefetch('2026-01-01T00:00:00Z', '2026-01-02T00:00:00Z', 20);
  });
});
