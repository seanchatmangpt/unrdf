import { describe, it, expect, vi } from 'vitest';
import {
  // Core factories
  df, namedNode, literal, quad, defaultGraph,
  testQuad, createTestQuad, createTestStore, createStoreWith,
  createPopulatedTestStore, generateQuads,
  TEST_SUBJECT, TEST_PREDICATE, TEST_OBJECT_VALUE, TEST_BASE,
  // Vocab
  VOCAB, terms, entities, socialGraphQuads,
  RDF_NS, RDFS_NS, FOAF_NS,
  // SPARQL
  SPARQL, getBindingValues, countResults,
  // Mocks
  createMockSpan, createMockTracer, createMockMeter, createMockFetch,
  createTestCleanup, createMockEmitter,
  // Timing
  timeSync, benchmarkSync, assertPerf,
  // Assertions
  assertThrowsCode, assertRejects, assertStoreSize,
  assertStoreContains, assertBindingContains,
} from '../src/index.mjs';

describe('@unrdf/test-utils', () => {
  describe('Core factories', () => {
    it('exports data factory helpers', () => {
      expect(typeof df.namedNode).toBe('function');
      expect(typeof namedNode).toBe('function');
      expect(typeof literal).toBe('function');
      expect(typeof quad).toBe('function');
    });

    it('exports standard test IRIs', () => {
      expect(TEST_SUBJECT).toBe('http://example.org/subject');
      expect(TEST_PREDICATE).toBe('http://example.org/predicate');
      expect(TEST_OBJECT_VALUE).toBe('test value');
    });

    it('testQuad has correct default values', () => {
      expect(testQuad.subject.value).toBe(TEST_SUBJECT);
      expect(testQuad.predicate.value).toBe(TEST_PREDICATE);
      expect(testQuad.object.value).toBe(TEST_OBJECT_VALUE);
    });

    it('createTestQuad() accepts overrides', () => {
      const q = createTestQuad({ object: literal('override') });
      expect(q.object.value).toBe('override');
      expect(q.subject.value).toBe(TEST_SUBJECT);
    });

    it('generateQuads() produces correct count with indexed IRIs', () => {
      const quads = generateQuads(10);
      expect(quads).toHaveLength(10);
      expect(quads[3].subject.value).toBe(`${TEST_BASE}subject3`);
      expect(quads[3].object.value).toBe('value3');
    });

    it('generateQuads() cycles predicates', () => {
      const quads = generateQuads(6, { predicates: 3 });
      expect(quads[0].predicate.value).toBe(`${TEST_BASE}predicate0`);
      expect(quads[3].predicate.value).toBe(`${TEST_BASE}predicate0`);
    });

    it('createStoreWith() loads quads', () => {
      const store = createStoreWith(generateQuads(5));
      expect([...store.match()]).toHaveLength(5);
    });

    it('createPopulatedTestStore() has one quad', () => {
      expect([...createPopulatedTestStore().match()]).toHaveLength(1);
    });
  });

  describe('Vocabulary', () => {
    it('exports standard vocab namespaces', () => {
      expect(RDF_NS).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
      expect(RDFS_NS).toBe('http://www.w3.org/2000/01/rdf-schema#');
      expect(FOAF_NS).toBe('http://xmlns.com/foaf/0.1/');
    });

    it('VOCAB has rdf:type IRI', () => {
      expect(VOCAB.rdf.type).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    });

    it('terms.rdfType is a named node', () => {
      expect(terms.rdfType.termType).toBe('NamedNode');
      expect(terms.rdfType.value).toBe(VOCAB.rdf.type);
    });

    it('entities.alice is a named node', () => {
      expect(entities.alice.termType).toBe('NamedNode');
      expect(entities.alice.value).toBe('http://example.org/alice');
    });

    it('socialGraphQuads() returns 7 quads', () => {
      const quads = socialGraphQuads();
      expect(quads).toHaveLength(7);
    });

    it('socialGraphQuads() loads into a store correctly', () => {
      const store = createStoreWith(socialGraphQuads());
      expect([...store.match()]).toHaveLength(7);
    });
  });

  describe('SPARQL fixtures', () => {
    it('SPARQL.selectAll is a string', () => {
      expect(typeof SPARQL.selectAll).toBe('string');
      expect(SPARQL.selectAll).toContain('SELECT');
    });

    it('SPARQL.selectByType produces parameterized query', () => {
      const q = SPARQL.selectByType('http://example.org/Person');
      expect(q).toContain('http://example.org/Person');
    });

    it('getBindingValues extracts values from result rows', () => {
      const fakeResults = [
        { s: { value: 'http://a' } },
        { s: { value: 'http://b' } },
      ];
      const vals = getBindingValues(fakeResults, 's');
      expect(vals).toEqual(['http://a', 'http://b']);
    });

    it('countResults handles arrays and booleans', () => {
      expect(countResults([1, 2, 3])).toBe(3);
      expect(countResults(true)).toBe(1);
      expect(countResults(false)).toBe(0);
    });
  });

  describe('Mocks', () => {
    it('createMockSpan has spy methods', () => {
      const span = createMockSpan();
      span.setAttribute('key', 'val');
      expect(span.setAttribute).toHaveBeenCalledWith('key', 'val');
      span.end();
      expect(span.end).toHaveBeenCalledTimes(1);
    });

    it('createMockTracer tracks started spans', () => {
      const tracer = createMockTracer();
      tracer.startSpan('my-op');
      expect(tracer.startSpan).toHaveBeenCalledWith('my-op');
      expect(tracer._spans).toHaveLength(1);
    });

    it('createMockMeter returns instruments with spy methods', () => {
      const meter = createMockMeter();
      const counter = meter.createCounter('requests');
      counter.add(1);
      expect(counter.add).toHaveBeenCalledWith(1);
    });

    it('createMockFetch returns ok response for unknown URL', async () => {
      const fetch = createMockFetch();
      const res = await fetch('http://any.url/endpoint');
      expect(res.ok).toBe(true);
      expect(res.status).toBe(200);
    });

    it('createMockFetch routes to matching response', async () => {
      const fetch = createMockFetch({
        '/api/query': { ok: true, data: { results: ['a', 'b'] } },
        '/api/health': { ok: false, status: 503 },
      });
      const res1 = await fetch('http://host/api/query');
      expect((await res1.json()).results).toHaveLength(2);
      const res2 = await fetch('http://host/api/health');
      expect(res2.ok).toBe(false);
      expect(res2.status).toBe(503);
    });

    it('createTestCleanup runs fns in reverse and clears', async () => {
      const order = [];
      const cleanup = createTestCleanup();
      cleanup.onCleanup(() => order.push(1));
      cleanup.onCleanup(() => order.push(2));
      cleanup.onCleanup(() => order.push(3));
      await cleanup.run();
      expect(order).toEqual([3, 2, 1]);
    });

    it('createMockEmitter tracks listeners and emits', () => {
      const emitter = createMockEmitter();
      const handler = vi.fn();
      emitter.on('change', handler);
      emitter.emit('change', { type: 'add' });
      expect(handler).toHaveBeenCalledWith({ type: 'add' });
    });
  });

  describe('Timing', () => {
    it('timeSync returns a positive number', () => {
      const ms = timeSync(() => 1 + 1);
      expect(ms).toBeGreaterThanOrEqual(0);
    });

    it('benchmarkSync returns stats object', () => {
      const stats = benchmarkSync(() => Math.random(), 50);
      expect(stats.iterations).toBe(50);
      expect(stats.avg).toBeGreaterThanOrEqual(0);
      expect(stats.min).toBeLessThanOrEqual(stats.max);
      expect(typeof stats.perOpUs).toBe('number');
    });

    it('assertPerf passes within budget', () => {
      expect(() => assertPerf(() => {}, 100, 1000)).not.toThrow();
    });

    it('assertPerf throws when budget exceeded', () => {
      expect(() => assertPerf(() => {
        const end = Date.now() + 5;
        while (Date.now() < end) {} // spin 5ms per iteration
      }, 10, 1)).toThrow('Performance budget exceeded');
    });
  });

  describe('Assertions', () => {
    it('assertThrowsCode catches and returns error', () => {
      const err = assertThrowsCode(() => { throw Object.assign(new Error('bad'), { code: 'ERR_X' }); }, 'ERR_X');
      expect(err.code).toBe('ERR_X');
    });

    it('assertThrowsCode throws if nothing thrown', () => {
      expect(() => assertThrowsCode(() => {}, 'ERR_X')).toThrow('nothing was thrown');
    });

    it('assertRejects resolves with caught error', async () => {
      const err = await assertRejects(async () => { throw new Error('oops'); }, 'oops');
      expect(err.message).toBe('oops');
    });

    it('assertStoreSize passes when count matches', () => {
      const store = createStoreWith(generateQuads(3));
      expect(() => assertStoreSize(store, 3)).not.toThrow();
    });

    it('assertStoreSize throws when count mismatches', () => {
      const store = createStoreWith(generateQuads(3));
      expect(() => assertStoreSize(store, 5)).toThrow('Expected store to contain 5 quads, but has 3');
    });

    it('assertStoreContains finds the quad', () => {
      const store = createPopulatedTestStore();
      expect(() => assertStoreContains(store, testQuad)).not.toThrow();
    });

    it('assertBindingContains finds value in results', () => {
      const results = [{ s: { value: 'http://a' } }, { s: { value: 'http://b' } }];
      expect(() => assertBindingContains(results, 's', 'http://a')).not.toThrow();
      expect(() => assertBindingContains(results, 's', 'http://c')).toThrow();
    });
  });
});
