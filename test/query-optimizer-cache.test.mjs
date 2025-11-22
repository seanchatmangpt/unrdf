import { describe, it, expect } from 'vitest';
import { QueryOptimizer } from '../src/knowledge-engine/query-optimizer.mjs';
import { Store, DataFactory } from 'n3';

const { quad, namedNode } = DataFactory;

describe('Query Optimizer - cache behavior', () => {
  it('caches query plans and reports cache hit rate', async () => {
    const optimizer = new QueryOptimizer({
      enableOTEL: false,
      maxCacheSize: 128,
    });
    const graph = new Store([quad(namedNode('ex:s'), namedNode('ex:p'), namedNode('ex:o'))]);

    const q = 'SELECT * WHERE { ?s ?p ?o }';
    const plan1 = await optimizer.optimizeQuery(q, 'sparql-select', graph);
    expect(plan1).toBeTruthy();

    const plan2 = await optimizer.optimizeQuery(q, 'sparql-select', graph);
    expect(plan2).toBeTruthy();

    const stats = optimizer.getStats();
    expect(stats.cache.hits).toBeGreaterThan(0);
    expect(stats.cache.maxSize).toBe(128);
    expect(stats.cache.hitRate).toBeGreaterThan(0);
    expect(stats.cache.misses).toBeGreaterThanOrEqual(0);
  });
});
