/**
 * @file Behavioural tests for @unrdf/react SemanticAnalyzer (real Oxigraph store, real OTEL API no-op tracer).
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  SemanticAnalyzer,
  createSemanticAnalyzer,
  defaultSemanticAnalyzer,
} from '../src/index.mjs';
import * as aiSemantic from '../src/ai-semantic/index.mjs';

const { namedNode, literal, quad } = dataFactory;
const EX = 'http://example.org/';
const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const RDFS_LABEL = 'http://www.w3.org/2000/01/rdf-schema#label';

const n = name => namedNode(`${EX}${name}`);
const add = (store, s, p, o) => store.add(quad(s, p, o));

/** Small social graph: alice/bob/carol Persons, knows edges, labels. */
function socialStore() {
  const store = createStore();
  const type = namedNode(RDF_TYPE);
  const label = namedNode(RDFS_LABEL);
  for (const who of ['alice', 'bob', 'carol']) {
    add(store, n(who), type, n('Person'));
    add(store, n(who), label, literal(who.toUpperCase()));
  }
  add(store, n('alice'), n('knows'), n('bob'));
  add(store, n('bob'), n('knows'), n('carol'));
  add(store, n('carol'), n('knows'), n('alice'));
  return store;
}

describe('exports', () => {
  it('index and ai-semantic entry points expose the same analyzer API', () => {
    expect(aiSemantic.SemanticAnalyzer).toBe(SemanticAnalyzer);
    expect(createSemanticAnalyzer()).toBeInstanceOf(SemanticAnalyzer);
    expect(defaultSemanticAnalyzer).toBeInstanceOf(SemanticAnalyzer);
  });
});

describe('SemanticAnalyzer construction', () => {
  it('applies defaults', () => {
    const a = new SemanticAnalyzer();
    expect(a.config).toEqual({
      cacheSize: 1000,
      enableCache: true,
      maxConcepts: 100,
      minConceptFrequency: 2,
      similarityThreshold: 0.7,
    });
  });

  it('rejects invalid config', () => {
    expect(() => new SemanticAnalyzer({ similarityThreshold: 2 })).toThrow();
    expect(() => new SemanticAnalyzer({ cacheSize: 'big' })).toThrow();
  });

  it('has no cache when disabled', () => {
    expect(new SemanticAnalyzer({ enableCache: false }).cache).toBeNull();
  });
});

describe('SemanticAnalyzer.analyze', () => {
  it('computes exact statistics for the graph', async () => {
    const store = socialStore();
    const r = await new SemanticAnalyzer().analyze(store);

    expect(r.statistics.totalTriples).toBe(9);
    expect(r.statistics.uniqueSubjects).toBe(3);
    expect(r.statistics.uniquePredicates).toBe(3); // type, label, knows
    // objects: Person, 3 labels, alice, bob, carol
    expect(r.statistics.uniqueObjects).toBe(7);
    // nodes = 3 people + Person + 3 labels = 7
    expect(r.statistics.avgDegree).toBeCloseTo(9 / 7, 10);
    expect(r.statistics.density).toBeCloseTo(9 / (7 * 6), 10);
  });

  it('extracts concepts with labels, types, frequency and PageRank-like centrality', async () => {
    const r = await new SemanticAnalyzer().analyze(await socialStore());
    expect(r.concepts.map(c => c.uri).sort()).toEqual([`${EX}alice`, `${EX}bob`, `${EX}carol`]);
    for (const c of r.concepts) {
      expect(c.frequency).toBe(3); // type + label + knows
      expect(c.type).toBe(`${EX}Person`);
      expect(c.label).toBe(c.uri.split('/').pop().toUpperCase());
      // symmetric 3-cycle -> every node converges to centrality 1
      expect(c.centrality).toBeCloseTo(1, 6);
    }
  });

  it('honours minConceptFrequency and maxConcepts', async () => {
    const store = socialStore();
    const none = await new SemanticAnalyzer({ minConceptFrequency: 4 }).analyze(store);
    expect(none.concepts).toEqual([]);

    const limited = await new SemanticAnalyzer().analyze(store, { maxConcepts: 2, useCache: false });
    expect(limited.concepts).toHaveLength(2);
  });

  it('reports relationships (strength normalised to the max) and common patterns', async () => {
    const r = await new SemanticAnalyzer().analyze(socialStore());
    expect(r.relationships).toHaveLength(9);
    expect(r.relationships.every(rel => rel.strength === 1)).toBe(true);

    const patterns = r.patterns.map(p => p.pattern);
    expect(patterns).toContain(`Common predicate: ${EX}knows`);
    expect(patterns).toContain(`Common predicate: ${RDF_TYPE}`);
    const knows = r.patterns.find(p => p.pattern === `Common predicate: ${EX}knows`);
    expect(knows).toMatchObject({ count: 3 });
    expect(knows.confidence).toBeCloseTo(3 / 9, 10);
  });

  it('suggests subclass axioms for multi-typed entities', async () => {
    const store = socialStore();
    add(store, n('alice'), namedNode(RDF_TYPE), n('Employee'));
    const r = await new SemanticAnalyzer().analyze(store);
    const sub = r.suggestions.filter(s => s.type === 'missing_subclass');
    expect(sub).toHaveLength(1);
    expect(sub[0].description).toContain(`${EX}alice`);
    expect(sub[0].priority).toBe('low');
  });

  it('suggests a missing inverse for a one-directional predicate used more than 5 times', async () => {
    const store = createStore();
    for (let i = 0; i < 6; i++) add(store, n(`a${i}`), n('parentOf'), n(`b${i}`));
    const r = await new SemanticAnalyzer().analyze(store);
    expect(r.suggestions).toContainEqual({
      type: 'missing_inverse',
      description: `Consider adding inverse property for ${EX}parentOf`,
      priority: 'medium',
    });
  });

  it('handles an empty store', async () => {
    const r = await new SemanticAnalyzer().analyze(createStore());
    expect(r.concepts).toEqual([]);
    expect(r.statistics).toMatchObject({ totalTriples: 0, avgDegree: 0, density: 0 });
  });

  it('rejects things that are not stores', async () => {
    await expect(new SemanticAnalyzer().analyze({ size: 1 })).rejects.toThrow(TypeError);
  });

  it('also accepts a plain iterable store', async () => {
    const quads = Array.from(socialStore().match());
    const iterable = { size: quads.length, [Symbol.iterator]: () => quads[Symbol.iterator]() };
    const r = await new SemanticAnalyzer().analyze(iterable);
    expect(r.statistics.totalTriples).toBe(9);
  });
});

describe('SemanticAnalyzer caching', () => {
  it('serves repeated analyses of an identical graph from cache', async () => {
    const a = new SemanticAnalyzer();
    const first = await a.analyze(socialStore());
    const second = await a.analyze(socialStore());
    expect(second).toBe(first);
    expect(a.getStats()).toMatchObject({ analyses: 1, cacheHits: 1, cacheMisses: 1, cacheSize: 1 });
    expect(a.getStats().cacheHitRate).toBe(0.5);
  });

  it('does not mix up different graphs that share size and their first quads', async () => {
    // Same triple count and identical first 10 quads, but the final quad differs.
    const build = lastObject => {
      const store = createStore();
      for (let i = 0; i < 10; i++) add(store, n(`s${i}`), n('p'), n(`o${i}`));
      add(store, n('tail'), n('p'), n(lastObject));
      return store;
    };
    const a = new SemanticAnalyzer({ minConceptFrequency: 1 });
    const r1 = await a.analyze(build('X'));
    const r2 = await a.analyze(build('Y'));
    expect(r2).not.toBe(r1);
    expect(r1.relationships.some(r => r.object === `${EX}X`)).toBe(true);
    expect(r2.relationships.some(r => r.object === `${EX}Y`)).toBe(true);
    expect(r2.relationships.some(r => r.object === `${EX}X`)).toBe(false);
  });

  it('bypasses the cache with useCache:false and after clearCache()', async () => {
    const a = new SemanticAnalyzer();
    const store = socialStore();
    const first = await a.analyze(store);
    expect(await a.analyze(store, { useCache: false })).not.toBe(first);
    a.clearCache();
    expect(a.getStats().cacheSize).toBe(0);
    expect(await a.analyze(store)).not.toBe(first);
  });

  it('cache hit rate never exceeds 1 after many hits', async () => {
    const a = new SemanticAnalyzer();
    const store = socialStore();
    for (let i = 0; i < 5; i++) await a.analyze(store);
    const stats = a.getStats();
    expect(stats.cacheHits).toBe(4);
    expect(stats.cacheHitRate).toBeCloseTo(4 / 5, 10);
    expect(stats.cacheHitRate).toBeLessThanOrEqual(1);
  });

  it('never caches when disabled', async () => {
    const a = new SemanticAnalyzer({ enableCache: false });
    const store = socialStore();
    expect(await a.analyze(store)).not.toBe(await a.analyze(store));
    expect(a.getStats()).toMatchObject({ cacheHits: 0, cacheSize: 0, cacheHitRate: 0 });
  });
});

describe('SemanticAnalyzer.computeSimilarity', () => {
  it('is 1 for concepts with identical properties and neighbours, and reports what they share', async () => {
    const store = createStore();
    add(store, n('x'), n('likes'), n('tea'));
    add(store, n('y'), n('likes'), n('tea'));
    const r = await new SemanticAnalyzer().computeSimilarity(store, `${EX}x`, `${EX}y`);
    expect(r.similarity).toBe(1);
    expect(r.method).toBe('jaccard');
    expect(r.commonProperties).toEqual([`${EX}likes`]);
    expect(r.commonNeighbors).toEqual([`${EX}tea`]);
  });

  it('weights property overlap 0.6 and neighbour overlap 0.4', async () => {
    const store = createStore();
    add(store, n('x'), n('likes'), n('tea'));
    add(store, n('y'), n('likes'), n('coffee')); // same property, different neighbour
    const r = await new SemanticAnalyzer().computeSimilarity(store, `${EX}x`, `${EX}y`);
    expect(r.similarity).toBeCloseTo(0.6, 10);
    expect(r.commonNeighbors).toEqual([]);
  });

  it('is 0 for unrelated concepts and unknown URIs', async () => {
    const store = createStore();
    add(store, n('x'), n('p'), n('a'));
    add(store, n('y'), n('q'), n('b'));
    const a = new SemanticAnalyzer();
    expect((await a.computeSimilarity(store, `${EX}x`, `${EX}y`)).similarity).toBe(0);
    expect((await a.computeSimilarity(store, `${EX}nope`, `${EX}nada`)).similarity).toBe(0);
  });
});
