import { describe, test, expect } from 'vitest';

import { TemporalCache } from '../../src/temporal-cache.mjs'

describe('Doctests: temporal-cache.mjs', () => {
  test('TemporalCache example 1 (line 1)', async () => {
    const cache = new TemporalCache({ maxSize: 100, ttl: 300000 });
cache.set('key', { results: [] });
const cached = cache.get('key');
  });

  test('TemporalCache example 2 (line 19)', async () => {
    const cache = new TemporalCache({ maxSize: 100, ttl: 60000 });
  });

  test('composite example 3 (line 57)', async () => {
    const key = await cache.generateKey('SELECT * WHERE { ?s ?p ?o }', 123456789n);
  });

  test('entry example 4 (line 73)', async () => {
    const result = cache.get('abc123...');
  });

  test('unknown example 5 (line 112)', async () => {
    cache.set('abc123...', { results: [], metadata: {} });
  });

  test('lruKey example 6 (line 148)', async () => {
    cache.clear();
  });

  test('total example 7 (line 185)', async () => {
    const stats = cache.getStats();
console.log(`Hit rate: ${stats.hitRate}%`);
  });

  test('entry example 8 (line 209)', async () => {
    cache.resetStats();
  });

  test('entry example 9 (line 221)', async () => {
    if (cache.has('abc123...')) { }
  });

  test('unknown example 10 (line 252)', async () => {
    const size = cache.size();
  });
});
