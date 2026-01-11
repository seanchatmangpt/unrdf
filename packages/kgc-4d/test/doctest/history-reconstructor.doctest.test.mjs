import { describe, test, expect } from 'vitest';

import { HistoryReconstructor } from '../../src/history-reconstructor.mjs'

describe('Doctests: history-reconstructor.mjs', () => {
  test('HistoryReconstructor example 1 (line 1)', async () => {
    const reconstructor = new HistoryReconstructor(store, git);
const pastStore = await reconstructor.reconstructAtTime(targetTime);
  });

  test('HistoryReconstructor example 2 (line 18)', async () => {
    const reconstructor = new HistoryReconstructor(store, git, { cacheSize: 50 });
  });

  test('cacheKey example 3 (line 63)', async () => {
    const pastStore = await reconstructor.reconstructAtTime(123456789n);
const results = await pastStore.query('SELECT * WHERE { ?s ?p ?o }');
  });

  test('sorted example 4 (line 116)', async () => {
    const stores = await reconstructor.reconstructAtTimes([t1, t2, t3]);
  });

  test('total example 5 (line 153)', async () => {
    const stats = reconstructor.getStats();
console.log(`Cache hit rate: ${stats.cacheHitRate}%`);
  });

  test('unknown example 6 (line 176)', async () => {
    reconstructor.resetStats();
  });

  test('unknown example 7 (line 188)', async () => {
    reconstructor.clearCache();
  });

  test('interval example 8 (line 200)', async () => {
    await reconstructor.prefetch(startTime, endTime, 20);
  });

  test('unknown example 9 (line 235)', async () => {
    const size = reconstructor.getCacheSize();
  });
});
